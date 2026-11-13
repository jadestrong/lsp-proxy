use globset::{GlobBuilder, GlobMatcher};
use lsp_types::{FileSystemWatcher, GlobPattern, OneOf, WatchKind};
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Weak,
};
use tokio::sync::mpsc;

use crate::client::Client;

enum Event {
    FileChanged {
        path: PathBuf,
    },
    Register {
        client_id: usize,
        client: Weak<Client>,
        registration_id: String,
        options: lsp_types::DidChangeWatchedFilesRegistrationOptions,
    },
    Unregister {
        client_id: usize,
        registration_id: String,
    },
    RemoveClient {
        client_id: usize,
    },
    #[cfg(test)]
    Barrier(tokio::sync::oneshot::Sender<()>),
}

#[derive(Default)]
struct ClientState {
    client: Weak<Client>,
    registered: HashMap<String, Vec<Watcher>>,
}

struct Watcher {
    glob: GlobMatcher,
    base: Option<PathBuf>,
    kind: WatchKind,
}

fn workspace_paths(client: &Client) -> Vec<PathBuf> {
    client
        .workspace_folders
        .lock()
        .iter()
        .filter_map(|folder| folder.uri.to_file_path().ok())
        .map(crate::utils::path::normalize)
        .collect()
}

impl Watcher {
    fn new(watcher: FileSystemWatcher, roots: &[PathBuf]) -> Option<Self> {
        let (pattern, base) = match watcher.glob_pattern {
            GlobPattern::String(pattern) => (pattern, None),
            GlobPattern::Relative(relative) => {
                let base = match relative.base_uri {
                    OneOf::Right(uri) => crate::utils::path::normalize(uri.to_file_path().ok()?),
                    OneOf::Left(folder) => {
                        let path = crate::utils::path::normalize(folder.uri.to_file_path().ok()?);
                        // Like VS Code's converter, resolve a WorkspaceFolder
                        // base to the workspace containing it; reject unknown roots.
                        roots
                            .iter()
                            .filter(|root| path.starts_with(root))
                            .max_by_key(|root| root.components().count())?
                            .clone()
                    }
                };
                (relative.pattern, Some(base))
            }
        };
        let glob = match GlobBuilder::new(&pattern).literal_separator(true).build() {
            Ok(glob) => glob.compile_matcher(),
            Err(err) => {
                log::warn!("Invalid didChangeWatchedFiles glob {pattern:?}: {err}");
                return None;
            }
        };
        Some(Self {
            glob,
            base,
            kind: watcher.kind.unwrap_or_else(WatchKind::all),
        })
    }

    fn matches(&self, path: &Path, roots: &[PathBuf]) -> bool {
        if !self.kind.contains(WatchKind::Change) {
            return false;
        }
        match &self.base {
            Some(base) => path
                .strip_prefix(base)
                .is_ok_and(|relative| self.glob.is_match(relative)),
            // String patterns, including absolute ones, are confined to this
            // client's current workspace folders. Explicit URI bases may be external.
            None => roots.iter().any(|root| path.starts_with(root)) && self.glob.is_match(path),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Handler {
    tx: mpsc::UnboundedSender<Event>,
}

impl Default for Handler {
    fn default() -> Self {
        Self::new()
    }
}

impl Handler {
    pub fn new() -> Self {
        let (tx, rx) = mpsc::unbounded_channel();
        tokio::spawn(Self::run(rx));
        Self { tx }
    }

    pub fn register(
        &self,
        client_id: usize,
        client: Weak<Client>,
        registration_id: String,
        options: lsp_types::DidChangeWatchedFilesRegistrationOptions,
    ) {
        let _ = self.tx.send(Event::Register {
            client_id,
            client,
            registration_id,
            options,
        });
    }

    pub fn unregister(&self, client_id: usize, registration_id: String) {
        let _ = self.tx.send(Event::Unregister {
            client_id,
            registration_id,
        });
    }

    pub fn file_changed(&self, path: PathBuf) {
        let _ = self.tx.send(Event::FileChanged { path });
    }

    pub fn remove_client(&self, client_id: usize) {
        let _ = self.tx.send(Event::RemoveClient { client_id });
    }

    async fn run(mut rx: mpsc::UnboundedReceiver<Event>) {
        let mut state: HashMap<usize, ClientState> = HashMap::new();
        while let Some(event) = rx.recv().await {
            match event {
                Event::FileChanged { path } => {
                    log::debug!("Received file event for {:?}", &path);
                    let path = crate::utils::path::normalize(path);

                    state.retain(|id, client_state| {
                        let Some(client) = client_state.client.upgrade() else {
                            log::warn!("LSP client was dropped: {id}");
                            return false;
                        };
                        let roots = workspace_paths(&client);
                        if !client_state
                            .registered
                            .values()
                            .flatten()
                            .any(|watcher| watcher.matches(&path, &roots))
                        {
                            return true;
                        }

                        let Ok(uri) = lsp_types::Url::from_file_path(&path) else {
                            return true;
                        };
                        log::debug!(
                            "Sending didChangeWatchedFiles notification to client '{}'",
                            client.name()
                        );
                        if let Err(err) =
                            client.did_change_watched_files(vec![lsp_types::FileEvent {
                                uri,
                                // We currently always send the CHANGED state
                                // since we don't actually have more context at
                                // the moment.
                                typ: lsp_types::FileChangeType::CHANGED,
                            }])
                        {
                            log::warn!("Failed to send didChangeWatchedFiles notification to client: {err}");
                        }

                        true
                    });
                }
                Event::Register {
                    client_id,
                    client,
                    registration_id,
                    options: ops,
                } => {
                    log::debug!(
                        "Registering didChangeWatchedFiles for client '{client_id}' with id '{registration_id}'",
                    );

                    let Some(active_client) = client.upgrade() else {
                        continue;
                    };
                    let roots = workspace_paths(&active_client);
                    let watchers = ops
                        .watchers
                        .into_iter()
                        .filter_map(|watcher| Watcher::new(watcher, &roots))
                        .collect();
                    let entry = state.entry(client_id).or_default();
                    entry.client = client;
                    entry.registered.insert(registration_id, watchers);
                }
                Event::Unregister {
                    client_id,
                    registration_id,
                } => {
                    log::debug!(
                        "Unregistering didChangeWatchedFiles with id '{registration_id}' for client '{client_id}'"
                    );
                    if let Some(client_state) = state.get_mut(&client_id) {
                        client_state.registered.remove(&registration_id);
                        if client_state.registered.is_empty() {
                            state.remove(&client_id);
                        }
                    }
                }
                Event::RemoveClient { client_id } => {
                    log::debug!("Removing LSP client: {client_id}");
                    state.remove(&client_id);
                }
                #[cfg(test)]
                Event::Barrier(sender) => {
                    let _ = sender.send(());
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::{Url, WorkspaceFolder};
    use serde_json::{json, Value};
    use std::{path::Path, sync::Arc};

    fn folder(root: &Path) -> WorkspaceFolder {
        WorkspaceFolder {
            name: "project".into(),
            uri: Url::from_directory_path(root).unwrap(),
        }
    }

    async fn peer(id: usize, root: &Path) -> Arc<Client> {
        let (client, _) =
            crate::test_support::client("jdtls", id, folder(root).uri.as_str(), true).await;
        *client.workspace_folders.lock() = vec![folder(root)];
        client
    }

    fn register(handler: &Handler, client: &Arc<Client>, id: &str, watchers: Value) {
        handler.register(
            client.id(),
            Arc::downgrade(client),
            id.into(),
            serde_json::from_value(json!({"watchers": watchers})).unwrap(),
        );
    }

    async fn events(handler: &Handler, client: &Client) -> Vec<Value> {
        let (sender, receiver) = tokio::sync::oneshot::channel();
        assert!(handler.tx.send(Event::Barrier(sender)).is_ok());
        receiver.await.unwrap();
        let response = client
            .call_method(
                "events".to_string().into(),
                "test/watchedFiles".into(),
                json!({}),
            )
            .await
            .unwrap();
        serde_json::from_value(response).unwrap()
    }

    #[tokio::test]
    async fn test_string_watchers_do_not_cross_checkout_boundaries() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("funding");
        let checkout = temp.path().join("funding-worktree");
        let first = peer(1, &root).await;
        let second = peer(2, &checkout).await;
        let handler = Handler::new();
        for client in [&first, &second] {
            register(
                &handler,
                client,
                "same-id",
                json!([
                    {"globPattern":"**/*.java"}, {"globPattern":"**/src/**"}
                ]),
            );
        }
        let file = root.join("src/main/java/Service.java");
        handler.file_changed(file.clone());
        assert_eq!(
            events(&handler, &first).await,
            vec![json!({
                "uri":Url::from_file_path(file).unwrap(), "type":2
            })]
        );
        assert!(events(&handler, &second).await.is_empty());
        handler.file_changed(checkout.join("src/main/java/Service.java"));
        assert_eq!(events(&handler, &first).await.len(), 1);
        assert_eq!(events(&handler, &second).await.len(), 1);
        first.stop_and_wait().await.unwrap();
        second.stop_and_wait().await.unwrap();
    }

    #[tokio::test]
    async fn test_absolute_strings_stay_in_workspace_and_uri_bases_can_be_external() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("project");
        let external = temp.path().join("generated");
        let client = peer(1, &root).await;
        let handler = Handler::new();
        register(
            &handler,
            &client,
            "absolute",
            json!([
            {"globPattern":format!("{}/**/*.java", root.to_string_lossy().replace('\\', "/"))},
            {"globPattern":format!("{}/**/*.java", external.to_string_lossy().replace('\\', "/"))}
            ]),
        );
        handler.file_changed(root.join("src/App.java"));
        handler.file_changed(external.join("Generated.java"));
        assert_eq!(events(&handler, &client).await.len(), 1);
        register(
            &handler,
            &client,
            "external",
            json!([{
                "globPattern":{"baseUri":folder(&external).uri,"pattern":"*.java"}
            }]),
        );
        handler.file_changed(external.join("Generated.java"));
        handler.file_changed(external.join("nested/Generated.java"));
        handler.file_changed(root.join("other/Generated.java"));
        assert_eq!(events(&handler, &client).await.len(), 3);
        // Explicit external bases remain valid even with no workspace folders.
        client.workspace_folders.lock().clear();
        handler.file_changed(external.join("Another.java"));
        handler.file_changed(root.join("src/App.java"));
        assert_eq!(events(&handler, &client).await.len(), 4);
        client.stop_and_wait().await.unwrap();
    }

    #[tokio::test]
    async fn test_workspace_folder_bases_resolve_only_known_workspaces() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("project");
        let other = temp.path().join("other");
        let client = peer(1, &root).await;
        let handler = Handler::new();
        register(
            &handler,
            &client,
            "relative",
            json!([
                {"globPattern":{"baseUri":folder(&root),"pattern":"src/**/*.java"}},
                {"globPattern":{"baseUri":folder(&other),"pattern":"**/*.java"}}
            ]),
        );
        handler.file_changed(root.join("src/main/java/App.java"));
        handler.file_changed(root.join("resources/App.java"));
        handler.file_changed(other.join("src/App.java"));
        assert_eq!(events(&handler, &client).await.len(), 1);
        client.stop_and_wait().await.unwrap();
    }

    #[tokio::test]
    async fn test_string_watchers_follow_added_and_removed_workspace_folders() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("first");
        let added = temp.path().join("second");
        let client = peer(1, &root).await;
        let handler = Handler::new();
        register(
            &handler,
            &client,
            "java",
            json!([{"globPattern":"**/*.java"}]),
        );
        handler.file_changed(added.join("App.java"));
        assert!(events(&handler, &client).await.is_empty());
        client.workspace_folders.lock().push(folder(&added));
        handler.file_changed(added.join("App.java"));
        handler.file_changed(root.join("App.java"));
        assert_eq!(events(&handler, &client).await.len(), 2);
        client.workspace_folders.lock().remove(0);
        handler.file_changed(root.join("App.java"));
        handler.file_changed(added.join("App.java"));
        assert_eq!(events(&handler, &client).await.len(), 3);
        client.workspace_folders.lock().clear();
        handler.file_changed(added.join("App.java"));
        assert_eq!(events(&handler, &client).await.len(), 3);
        client.stop_and_wait().await.unwrap();
    }

    #[tokio::test]
    async fn test_watch_kind_change_is_respected_and_missing_kind_defaults_to_all() {
        let temp = tempfile::tempdir().unwrap();
        let client = peer(1, temp.path()).await;
        let handler = Handler::new();
        register(
            &handler,
            &client,
            "kinds",
            json!([
                {"globPattern":"**/*.java","kind":5},
                {"globPattern":"**/*.xml","kind":2},
                {"globPattern":"**/*.properties"}
            ]),
        );
        for name in ["App.java", "pom.xml", "application.properties"] {
            handler.file_changed(temp.path().join(name));
        }
        let result = events(&handler, &client).await;
        assert_eq!(result.len(), 2);
        assert!(result.iter().all(|event| event["type"] == 2));
        register(
            &handler,
            &client,
            "kinds",
            json!([{"globPattern":"**/*","kind":0}]),
        );
        handler.file_changed(temp.path().join("pom.xml"));
        assert_eq!(events(&handler, &client).await.len(), 2);
        client.stop_and_wait().await.unwrap();
    }

    #[tokio::test]
    async fn test_registration_replacement_unregistration_and_client_removal() {
        let temp = tempfile::tempdir().unwrap();
        let first = peer(1, temp.path()).await;
        let second = peer(2, temp.path()).await;
        let handler = Handler::new();
        for client in [&first, &second] {
            register(
                &handler,
                client,
                "same-id",
                json!([{"globPattern":"**/*.java"}]),
            );
        }
        register(
            &handler,
            &first,
            "same-id",
            json!([{"globPattern":"**/*.xml"}]),
        );
        handler.file_changed(temp.path().join("App.java"));
        assert!(events(&handler, &first).await.is_empty());
        assert_eq!(events(&handler, &second).await.len(), 1);
        register(
            &handler,
            &first,
            "java",
            json!([{"globPattern":"**/*.java"}]),
        );
        handler.unregister(first.id(), "java".into());
        handler.unregister(first.id(), "unknown".into());
        handler.file_changed(temp.path().join("App.java"));
        handler.file_changed(temp.path().join("pom.xml"));
        assert_eq!(events(&handler, &first).await.len(), 1);
        assert_eq!(events(&handler, &second).await.len(), 2);
        handler.remove_client(second.id());
        handler.file_changed(temp.path().join("App.java"));
        assert_eq!(events(&handler, &second).await.len(), 2);
        first.stop_and_wait().await.unwrap();
        second.stop_and_wait().await.unwrap();
    }

    #[tokio::test]
    async fn test_invalid_patterns_and_non_file_bases_do_not_keep_old_registrations() {
        let temp = tempfile::tempdir().unwrap();
        let client = peer(1, temp.path()).await;
        let handler = Handler::new();
        register(
            &handler,
            &client,
            "java",
            json!([{"globPattern":"**/*.java"}]),
        );
        register(
            &handler,
            &client,
            "java",
            json!([
                {"globPattern":"["},
                {"globPattern":{"baseUri":"https://example.com/project","pattern":"**/*.java"}}
            ]),
        );
        handler.file_changed(temp.path().join("App.java"));
        assert!(events(&handler, &client).await.is_empty());
        register(
            &handler,
            &client,
            "java",
            json!([{"globPattern":"**/*.java"}]),
        );
        handler.file_changed(temp.path().join("src/../App.java"));
        assert_eq!(events(&handler, &client).await.len(), 1);
        register(&handler, &client, "java", json!([]));
        handler.file_changed(temp.path().join("App.java"));
        assert_eq!(events(&handler, &client).await.len(), 1);
        client.stop_and_wait().await.unwrap();
    }
}

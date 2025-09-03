use globset::{GlobBuilder, GlobSetBuilder};
use std::{collections::HashMap, path::PathBuf, sync::Weak};
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
}

#[derive(Default)]
struct ClientState {
    client: Weak<Client>,
    registered: HashMap<String, globset::GlobSet>,
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

                    state.retain(|id, client_state| {
                        if !client_state
                            .registered
                            .values()
                            .any(|glob| glob.is_match(&path))
                        {
                            return true;
                        }

                        let Some(client) = client_state.client.upgrade() else {
                            log::warn!("LSP client was dropped: {id}");
                            return false;
                        };
                        let Ok(uri) = lsp_types::Url::from_file_path(&path) else {
                            return true;
                        };
                        log::debug!(
                            "Sending didChangeWatchedFiles notification to client '{}'",
                            client.name()
                        );
                        if let Err(err) =
                            // block_on()
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
                        "Registering didChangeWatchedFiles for client '{}' with id '{}'",
                        client_id,
                        registration_id,
                    );

                    let entry = state.entry(client_id).or_insert_with(ClientState::default);
                    entry.client = client;

                    let mut builder = GlobSetBuilder::new();
                    for watcher in ops.watchers {
                        if let lsp_types::GlobPattern::String(pattern) = watcher.glob_pattern {
                            if let Ok(glob) = GlobBuilder::new(&pattern).build() {
                                builder.add(glob);
                            }
                        }
                    }
                    match builder.build() {
                        Ok(globset) => {
                            entry.registered.insert(registration_id, globset);
                        }
                        Err(err) => {
                            // Remove any old state for that registration id and
                            // remove the entire client if it's now empty.
                            entry.registered.remove(&registration_id);
                            if entry.registered.is_empty() {
                                state.remove(&client_id);
                            }
                            log::warn!(
                                "Unable to build globset for LSP didChangeWatchedFiles {err}"
                            );
                        }
                    }
                }
                Event::Unregister {
                    client_id,
                    registration_id,
                } => {
                    log::debug!(
                        "Unregistering didChangeWatchedFiles with id '{}' for client '{}'",
                        registration_id,
                        client_id
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
            }
        }
    }
}

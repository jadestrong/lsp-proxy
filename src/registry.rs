use crate::{
    client::Client,
    lsp::{
        file_event,
        jsonrpc::{self, Call},
    },
    msg::RequestId,
    syntax::{self, LanguageConfiguration, LanguageServerConfiguration, LanguageServerFeatures},
    utils::path,
};
use anyhow::anyhow;
use futures_util::stream::SelectAll;
use log::{debug, error};
use lsp::notification::Notification;
use lsp_types as lsp;
use std::{collections::HashMap, path::PathBuf, sync::Arc};
use thiserror::Error;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio_stream::wrappers::UnboundedReceiverStream;

pub type LanguageServerName = String;
struct NewClient(Arc<Client>, UnboundedReceiver<(usize, Call)>);

#[derive(Error, Debug)]
#[allow(dead_code)]
pub enum Error {
    #[error("protocol error: {0}")]
    Rpc(#[from] jsonrpc::Error),
    #[error("failed to parse: {0}")]
    Parse(#[from] serde_json::Error),
    #[error("IO Error: {0}")]
    IO(#[from] std::io::Error),
    #[error("request {0} timed out")]
    Timeout(RequestId),
    #[error("server closed the stream")]
    StreamClosed,
    #[error("Unhandled")]
    Unhandled,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug, PartialEq, Clone)]
pub enum NotificationFromServer {
    Initialized,
    Exit,
    PublishDianostics(lsp::PublishDiagnosticsParams),
    ShowMessage(lsp::ShowMessageParams),
    LogMessage(lsp::LogMessageParams),
    ProgressMessage(lsp::ProgressParams),
}

impl NotificationFromServer {
    pub fn parse(method: &str, params: jsonrpc::Params) -> Result<NotificationFromServer> {
        // let lsp::notification::Notification as _;

        let notification = match method {
            lsp::notification::Initialized::METHOD => Self::Initialized,
            lsp::notification::Exit::METHOD => Self::Exit,
            lsp::notification::PublishDiagnostics::METHOD => {
                let params: lsp::PublishDiagnosticsParams = params.parse()?;
                Self::PublishDianostics(params)
            }
            lsp::notification::ShowMessage::METHOD => {
                let params: lsp::ShowMessageParams = params.parse()?;
                Self::ShowMessage(params)
            }
            lsp::notification::LogMessage::METHOD => {
                let params: lsp::LogMessageParams = params.parse()?;
                Self::LogMessage(params)
            }
            lsp::notification::Progress::METHOD => {
                let params: lsp::ProgressParams = params.parse()?;
                Self::ProgressMessage(params)
            }
            _ => {
                return Err(Error::Unhandled);
            }
        };

        Ok(notification)
    }
}

pub struct Registry {
    inner: HashMap<LanguageServerName, Vec<Arc<Client>>>,
    syn_loader: Arc<syntax::Loader>,
    counter: usize,
    pub incoming: SelectAll<UnboundedReceiverStream<(usize, Call)>>,
    pub file_event_handler: file_event::Handler,
}

impl Registry {
    pub fn new(syn_loader: Arc<syntax::Loader>) -> Self {
        Self {
            inner: HashMap::new(),
            syn_loader,
            counter: 0,
            incoming: SelectAll::new(),
            file_event_handler: file_event::Handler::new(),
        }
    }

    pub fn get<'a>(
        &'a mut self,
        language_config: &'a LanguageConfiguration,
        doc_path: Option<&'a std::path::PathBuf>,
    ) -> impl Iterator<Item = (LanguageServerName, Result<Arc<Client>>)> + 'a {
        // NOTE 从 language_config 中查找是否有满足的 lsp server ，是否可以将所有支持的 language lsp adapter 都注册到 editor 实例上
        // 直接从 LspRegistry 中查找？
        language_config.language_servers.iter().map(
            move |LanguageServerFeatures {
                      name,
                      support_workspace,
                      library_directories,
                      ..
                  }| {
                if let Some(clients) = self.inner.get(name) {
                    if let Some((_, client)) = clients.iter().enumerate().find(|(_, client)| {
                        client.try_add_doc(
                            &language_config.roots,
                            doc_path,
                            support_workspace.to_owned(),
                        )
                    }) {
                        return (name.to_owned(), Ok(client.clone()));
                    }

                    // 如果存在 library_directories 则判断该文件是否属于，若属于则返回最新活跃的 client
                    if let Some(_) = library_directories.iter().find(|dir| {
                        path::path_is_ancestor_of(dir, &doc_path.unwrap().to_string_lossy())
                    }) {
                        if let Some(client) = clients
                            .iter()
                            .max_by_key(|item| *item.activate_time.try_lock().unwrap())
                        {
                            debug!("the latest workspace is {:?}", client.root_path);
                            return (name.to_owned(), Ok(client.clone()));
                        }
                    }
                }

                match self.start_client(name.clone(), language_config, doc_path) {
                    Ok(client) => {
                        self.inner
                            .entry(name.to_owned())
                            .or_default()
                            .push(client.clone());
                        (name.to_owned(), Ok(client))
                    }
                    Err(err) => (name.to_owned(), Err(err)),
                }
            },
        )
    }

    pub fn get_by_id(&self, id: usize) -> Option<Arc<Client>> {
        self.inner
            .values()
            .flatten()
            .find(|client| client.id() == id)
            .map(|client| client.to_owned().clone())
    }

    pub fn remove_by_id(&mut self, id: usize) {
        self.file_event_handler.remove_client(id);
        self.inner.retain(|_, language_servers| {
            language_servers.retain(|ls| id != ls.id());
            !language_servers.is_empty()
        });
    }

    fn start_client(
        &mut self,
        name: String,
        ls_config: &LanguageConfiguration,
        doc_path: Option<&std::path::PathBuf>,
    ) -> Result<Arc<Client>> {
        // 加载 LanguageServerConfiguration
        let config = self
            .syn_loader
            .language_server_configs()
            .get(&name)
            .ok_or_else(|| anyhow!("Language server '{name}' not defined."))?;
        let id = self.counter;
        self.counter += 1;
        let NewClient(client, incoming) = start_client(id, name, ls_config, config, doc_path)?;
        self.incoming.push(UnboundedReceiverStream::new(incoming));
        Ok(client)
    }

    pub fn iter_clients(&self) -> impl Iterator<Item = &Arc<Client>> {
        self.inner.values().flatten()
    }

    pub fn restart_v2(
        &mut self,
        language_config: &LanguageConfiguration,
        old_client_ids: Vec<usize>,
        doc_path: Option<PathBuf>,
    ) -> Result<Vec<Arc<Client>>> {
        let new_clients = language_config
            .language_servers
            .iter()
            .filter_map(|LanguageServerFeatures { name, .. }| {
                if self.inner.contains_key(name) {
                    let client =
                        match self.start_client(name.clone(), language_config, doc_path.as_ref()) {
                            Ok(client) => client,
                            error => return Some(error),
                        };

                    // 这样是将所有同名的 server 都清空了
                    // let old_clients = self
                    //     .inner
                    //     .insert(name.clone(), vec![client.clone()])
                    //     .unwrap();

                    self.inner
                        .entry(name.to_string())
                        .or_default()
                        .push(client.clone());
                    Some(Ok(client))
                } else {
                    None
                }
            })
            .collect();

        let old_clients: Vec<Arc<Client>> = old_client_ids
            .into_iter()
            .filter_map(|old_client_id| self.get_by_id(old_client_id))
            .collect();
        for old_client in old_clients {
            self.file_event_handler.remove_client(old_client.id());
            tokio::spawn(async move {
                let _ = old_client.force_shutdown().await;
            });
        }

        new_clients
    }
}

fn start_client(
    id: usize,
    name: String,
    config: &LanguageConfiguration,
    ls_config: &LanguageServerConfiguration,
    doc_path: Option<&std::path::PathBuf>,
) -> Result<NewClient> {
    let (client, incoming, initialize_notify) = Client::start(
        &ls_config.command,
        &ls_config.args,
        ls_config.config.clone(),
        ls_config.experimental.clone(),
        ls_config.environment.clone(),
        &config.roots,
        id,
        name.clone(),
        ls_config.timeout,
        doc_path,
        config
            .language_servers
            .iter()
            .find(|features| &features.name == &name),
    )?;

    let client = Arc::new(client);
    // Initialize the client asynchoronously
    let _client = client.clone();
    tokio::spawn(async move {
        use futures_util::TryFutureExt;
        let value = _client
            .capabilities
            .get_or_try_init(|| {
                _client
                    .initialize(true)
                    .map_ok(|response| response.capabilities)
            })
            .await;

        if let Err(e) = value {
            error!("failed to initialize language server: {}", e);
            return;
        }
        debug!("server {:?} capabilities {:?}", _client.name(), value.ok());
        // next up, notify<initialized>
        // _client
        //     .notify::<lsp::notification::Initialized>(lsp::InitializedParams {})
        //     .unwrap();

        initialize_notify.notify_one();
    });

    Ok(NewClient(client, incoming))
}

use std::{
    collections::{BTreeMap, HashMap},
    num::NonZeroUsize,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::Result;
use log::error;
use lsp_types::Url;

use crate::{
    client::Client,
    document::{Document, DocumentId},
    lsp_ext,
    registry::Registry,
    syntax,
};

pub struct Editor {
    pub next_document_id: DocumentId,
    pub documents: BTreeMap<DocumentId, Document>,
    pub language_servers: Registry,
    pub syn_loader: Arc<syntax::Loader>,
}

impl Editor {
    pub fn new(syn_loader: Arc<syntax::Loader>) -> Self {
        let language_servers = Registry::new(syn_loader.clone());

        Self {
            next_document_id: DocumentId::default(),
            documents: BTreeMap::new(),
            language_servers,
            syn_loader,
        }
    }

    /// If the document already existed, return it
    /// Else create it, and launch langauge server for this document
    pub fn get(&mut self, uri: &Url) -> Result<DocumentId> {
        let id = self.document_by_uri(&uri).map(|doc| doc.id);

        let id = if let Some(id) = id {
            id
        } else {
            self.new_document(uri, "".to_string())
            // self.launch_langauge_servers(id);
        };

        Ok(id)
    }

    #[inline]
    pub fn documents(&self) -> impl Iterator<Item = &Document> {
        self.documents.values()
    }

    #[inline]
    pub fn documents_mut(&mut self) -> impl Iterator<Item = &mut Document> {
        self.documents.values_mut()
    }

    pub fn document_by_path<P: AsRef<Path>>(&self, path: P) -> Option<&Document> {
        self.documents()
            .find(|doc| doc.path().map(|p| p == path.as_ref()).unwrap_or(false))
    }

    pub fn document_by_path_mut<P: AsRef<Path>>(&mut self, path: P) -> Option<&mut Document> {
        self.documents_mut()
            .find(|doc| doc.path().map(|p| p == path.as_ref()).unwrap_or(false))
    }

    pub fn document_by_uri(&self, uri: &Url) -> Option<&Document> {
        self.documents()
            .find(|doc| doc.uri().map(|u| u == uri).unwrap_or(false))
    }

    pub fn document_by_uri_mut(&mut self, uri: &Url) -> Option<&mut Document> {
        self.documents_mut()
            .find(|doc| doc.uri().map(|u| u == uri).unwrap_or(false))
    }

    pub fn new_document(&mut self, uri: &Url, text: String) -> DocumentId {
        let mut doc = Document::new(uri, text, Some(self.syn_loader.clone()));
        let id = self.next_document_id;
        self.next_document_id =
            DocumentId(unsafe { NonZeroUsize::new_unchecked(self.next_document_id.0.get() + 1) });
        doc.id = id;
        self.documents.insert(id, doc);
        id
    }

    /// FIXME some different need to align
    pub fn launch_langauge_servers(
        &mut self,
        doc_id: DocumentId,
        params: lsp_ext::CustomizeDidOpenTextDocumentParams,
    ) {
        let Some(doc) = self.documents.get_mut(&doc_id) else {
            return;
        };
        let Some(doc_url) = doc.uri() else {
            return;
        };
        let (lang, path) = (doc.language.clone(), doc.path());
        // TODO doc.config and workspace_lsp_roots
        // let config = doc.config
        let root_dirs: Vec<PathBuf> = Vec::new();
        let language_servers = lang.as_ref().map_or_else(HashMap::default, |language| {
            self.language_servers
                .get(language, path.as_ref(), &root_dirs)
                .filter_map(|(lang, client)| match client {
                    Ok(client) => Some((lang, client)),
                    Err(err) => {
                        error!(
                            "Failed to initialize the language servers for `{}` - `{}` {{ {} }}",
                            language.language_id, lang, err,
                        );
                        None
                    }
                })
                .collect::<HashMap<_, _>>()
        });

        // 如果没有合适的 language server 则直接返回
        // 如果存在？
        if language_servers.is_empty() {
            return;
        }

        let language_id = doc.language_id().map(ToOwned::to_owned).unwrap_or_default();

        // only spawn new language servers if the servers aren't the same
        let doc_language_servers_not_in_registry =
            doc.language_servers.iter().filter(|(name, doc_ls)| {
                // 最新的属于该文档的 server ，如果 doc 记录的 servers 中存在多出来的，则需要关闭
                language_servers
                    .get(*name)
                    .map_or(true, |ls| ls.id() != doc_ls.id())
            });

        for (_, language_server) in doc_language_servers_not_in_registry {
            // tokio::spawn();
            language_server
                .text_document_did_close(lsp_types::DidCloseTextDocumentParams {
                    text_document: doc.identifier(),
                })
                .unwrap();
        }

        // 针对还没有记录到 doc 对象里面的，则表示是第一次打开
        let language_servers_not_in_doc = language_servers.iter().filter(|(name, ls)| {
            doc.language_servers
                .get(*name)
                .map_or(true, |doc_ls| ls.id() != doc_ls.id())
        });

        // TODO 这一步不要在这里做，放到接到 did open 请求时转发
        for (_, language_server) in language_servers_not_in_doc {
            // TODO this now races with on_init_code if the init happens too quickly
            language_server
                .text_document_did_open(
                    doc_url.clone(),
                    doc.version(),
                    params.text_document.text.clone(), // TODO
                    language_id.clone(),
                )
                .unwrap();
        }

        doc.language_servers = language_servers;
    }

    #[inline]
    pub fn language_server_by_id(&self, language_server_id: usize) -> Option<&Client> {
        self.language_servers.get_by_id(language_server_id)
    }

    #[inline]
    pub fn language_server_by_id_v2(&self, language_server_id: usize) -> Option<Arc<Client>> {
        self.language_servers.get_by_id_v2(language_server_id)
    }
}

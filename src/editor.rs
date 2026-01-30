use crate::{
    client::Client,
    document::{Document, DocumentId},
    registry::Registry,
    syntax,
    utils::uri_to_path,
};
use log::error;
use lsp_types::Url;
use std::{
    collections::{BTreeMap, HashMap},
    num::NonZeroUsize,
    path::Path,
    sync::Arc,
};

pub struct Editor {
    pub next_document_id: DocumentId,
    pub documents: BTreeMap<DocumentId, Document>,
    pub language_servers: Registry,
    pub syn_loader: Arc<syntax::Loader>,
}

#[allow(dead_code)]
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
    pub fn get(&mut self, uri: &Url) -> &mut Document {
        let doc_id = self
            .document_by_uri(uri)
            .map(|doc| doc.id)
            .unwrap_or_else(|| self.new_document(uri, None).id);

        self.documents.get_mut(&doc_id).unwrap()
    }

    #[inline]
    pub fn document_mut(&mut self, id: DocumentId) -> Option<&mut Document> {
        self.documents.get_mut(&id)
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
        if cfg!(target_os = "windows") {
            self.documents()
                .find(|doc| uri_to_path(doc.uri()) == uri_to_path(uri))
        } else {
            self.documents().find(|doc| doc.uri() == uri)
        }
    }

    pub fn document_by_uri_mut(&mut self, uri: &Url) -> Option<&mut Document> {
        if cfg!(target_os = "windows") {
            self.documents_mut()
                .find(|doc| uri_to_path(doc.uri()) == uri_to_path(uri))
        } else {
            self.documents_mut().find(|doc| doc.uri() == uri)
        }
    }

    pub fn new_document(&mut self, uri: &Url, language: Option<&str>) -> &Document {
        let mut doc = Document::new(uri, Some(self.syn_loader.clone()), language);
        let id = self.next_document_id;
        self.next_document_id =
            DocumentId(unsafe { NonZeroUsize::new_unchecked(self.next_document_id.0.get() + 1) });
        doc.id = id;
        self.documents.insert(id, doc);
        self.documents.get(&id).unwrap()
    }

    pub fn launch_language_servers(&mut self, doc_id: DocumentId) {
        let Some(doc) = self.documents.get_mut(&doc_id) else {
            return;
        };
        let (language_config, doc_path) = (doc.language_config.clone(), doc.path());
        let language_servers =
            language_config
                .as_ref()
                .map_or_else(HashMap::default, |language_config| {
                    self.language_servers
                        .get(language_config, doc_path.as_ref())
                        .filter_map(|(lang, client)| match client {
                            Ok(client) => Some((lang, client)),
                            Err(err) => {
                                error!(
                            "Failed to initialize the language servers for `{}` - `{}` {{ {} }}",
                            language_config.language_id, lang, err,
                        );
                                None
                            }
                        })
                        .collect::<HashMap<_, _>>()
                });

        if language_servers.is_empty() && doc.language_servers.is_empty() {
            return;
        }

        // only spawn new language servers if the servers aren't the same
        let doc_language_servers_not_in_registry =
            doc.language_servers.iter().filter(|(name, doc_ls)| {
                language_servers
                    .get(*name)
                    .is_none_or(|ls| ls.id() != doc_ls.id())
            });

        for (_, language_server) in doc_language_servers_not_in_registry {
            language_server
                .text_document_did_close(lsp_types::DidCloseTextDocumentParams {
                    text_document: doc.identifier(),
                })
                .unwrap();
        }

        doc.language_servers = language_servers;
    }

    pub fn launch_language_servers_for_virtual_document(&mut self, doc_id: DocumentId, language: &str) -> Option<Arc<Client>> {
        let doc = self.documents.get_mut(&doc_id)?;

        // 获取虚拟文档的语言配置
        let virtual_language_config = doc.virtual_doc.as_ref()
            .and_then(|virtual_doc| virtual_doc.language_config.clone());

        // org block 只需要第一个 server，直接取第一个配置
        let doc_path = doc.path();
        let first_server = virtual_language_config
            .as_ref()
            .and_then(|language_config| {
                // 只取第一个语言服务器配置
                let first_ls_feature = language_config.language_servers.first()?;
                
                // 使用 iterator 的 next() 只获取第一个，避免启动多个 server
                let mut iter = self.language_servers.get(language_config, doc_path.as_ref());
                let (name, result) = iter.next()?;
                
                match result {
                    Ok(client) => Some((name, client)),
                    Err(err) => {
                        error!(
                            "Failed to initialize language server for virtual document `{}` - `{}` {{ {} }}",
                            language, first_ls_feature.name, err
                        );
                        None
                    }
                }
            });

        let (_, server) = first_server?;

        // 存储到 language_servers_of_virtual_doc，使用 language 作为 key
        // 需要重新获取 doc 的可变引用
        if let Some(doc) = self.documents.get_mut(&doc_id) {
            doc.language_servers_of_virtual_doc.insert(language.to_owned(), server.clone());
        }
        
        Some(server)
    }

    #[inline]
    pub fn language_server_by_id(&self, language_server_id: usize) -> Option<Arc<Client>> {
        self.language_servers.get_by_id(language_server_id)
    }

    /// Remove document from editor by URI
    /// Returns true if the document was found and removed
    pub fn remove_document(&mut self, uri: &Url) -> bool {
        if let Some(doc) = self.document_by_uri(uri) {
            let doc_id = doc.id;
            self.documents.remove(&doc_id).is_some()
        } else {
            false
        }
    }

    /// Remove document from editor by DocumentId  
    /// Returns the removed document if it existed
    pub fn remove_document_by_id(&mut self, doc_id: DocumentId) -> Option<Document> {
        self.documents.remove(&doc_id)
    }
}

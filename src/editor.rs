use crate::{
    client::Client,
    document::{Document, DocumentId},
    registry::Registry,
    syntax,
};
use anyhow::Result;
use log::error;
use lsp_types::Url;
use percent_encoding::percent_decode;
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
    pub fn get(&mut self, uri: &Url) -> Result<DocumentId> {
        let id = self.document_by_uri(&uri).map(|doc| doc.id);

        let id = if let Some(id) = id {
            id
        } else {
            self.new_document(uri)
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
        if cfg!(target_os = "windows") {
            let decoded_uri_str = percent_decode(uri.as_str().as_bytes())
                .decode_utf8_lossy()
                .to_string();
            let decoded_uri = Url::parse(&decoded_uri_str).unwrap();
            self.documents().find(|doc| doc.uri() == &decoded_uri)
        } else {
            self.documents().find(|doc| doc.uri() == uri)
        }
    }

    pub fn document_by_uri_mut(&mut self, uri: &Url) -> Option<&mut Document> {
        if cfg!(target_os = "windows") {
            let decoded_uri_str = percent_decode(uri.as_str().as_bytes())
                .decode_utf8_lossy()
                .to_string();
            let decoded_uri = Url::parse(&decoded_uri_str).unwrap();
            self.documents_mut().find(|doc| doc.uri() == &decoded_uri)
        } else {
            self.documents_mut().find(|doc| doc.uri() == uri)
        }
    }

    pub fn new_document(&mut self, uri: &Url) -> DocumentId {
        let mut doc = Document::new(uri, Some(self.syn_loader.clone()));
        let id = self.next_document_id;
        self.next_document_id =
            DocumentId(unsafe { NonZeroUsize::new_unchecked(self.next_document_id.0.get() + 1) });
        doc.id = id;
        self.documents.insert(id, doc);
        id
    }

    pub fn launch_langauge_servers(&mut self, doc_id: DocumentId) {
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
                    .map_or(true, |ls| ls.id() != doc_ls.id())
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

    #[inline]
    pub fn language_server_by_id(&self, language_server_id: usize) -> Option<Arc<Client>> {
        self.language_servers.get_by_id(language_server_id)
    }
}

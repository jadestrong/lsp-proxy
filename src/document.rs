use crate::{
    client::Client,
    registry::LanguageServerName,
    syntax::{self, LanguageConfiguration, LanguageServerFeature},
};
use lsp::Diagnostic;
use lsp_types::{self as lsp, Url};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, num::NonZeroUsize, path::PathBuf, sync::Arc};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DocumentId(pub NonZeroUsize);

impl Default for DocumentId {
    fn default() -> Self {
        // Safety: 1 is non-zero
        DocumentId(unsafe { NonZeroUsize::new_unchecked(1) })
    }
}

impl std::fmt::Display for DocumentId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct DiagnosticItem {
    pub item: Diagnostic,
    pub language_server_id: usize,
    pub file_path: String,
}

#[derive(Debug)]
pub struct Document {
    pub(crate) id: DocumentId,
    pub uri: Url,
    /// Corresponding language scope name. Usually `source.<lang>`.
    pub language_config: Option<Arc<LanguageConfiguration>>,
    pub(crate) language_servers: HashMap<LanguageServerName, Arc<Client>>,
    pub(crate) diagnostics: Option<Vec<DiagnosticItem>>,
    pub version: i32,
}

impl Document {
    pub fn new(uri: &Url, config_loader: Option<Arc<syntax::Loader>>) -> Self {
        let mut doc = Document {
            id: DocumentId::default(),
            uri: uri.clone(),
            language_config: None,
            language_servers: HashMap::new(),
            version: 0,
            diagnostics: None,
        };

        if let Some(loader) = config_loader {
            doc.set_language_config(loader);
        }

        doc
    }

    pub fn id(&self) -> DocumentId {
        self.id
    }

    #[inline]
    /// File url
    pub fn uri(&self) -> &Url {
        &self.uri
    }

    /// A Url to file path
    pub fn path(&self) -> Option<PathBuf> {
        Url::to_file_path(self.uri()).ok()
    }

    pub fn get_trigger_characters(&self) -> Vec<String> {
        let mut trigger_characters: Vec<String> = Vec::new();
        self.language_servers().for_each(|language_server| {
            if let Some(lsp_types::CompletionOptions {
                trigger_characters: Some(triggers),
                ..
            }) = &language_server.capabilities().completion_provider
            {
                let mut triggers = triggers.clone();
                trigger_characters.append(&mut triggers);
            }
        });

        trigger_characters
    }

    pub fn get_signature_trigger_characters(&self) -> Vec<String> {
        let mut signature_trigger_characters = vec![];
        self.language_servers().for_each(|ls| {
            if let Some(lsp_types::SignatureHelpOptions {
                trigger_characters: Some(characters),
                ..
            }) = &ls.capabilities().signature_help_provider
            {
                signature_trigger_characters.append(&mut characters.to_owned());
            }
        });
        signature_trigger_characters
    }

    pub fn is_has_inlay_hints_support(&self) -> bool {
        self.language_servers()
            .any(|ls| ls.supports_feature(LanguageServerFeature::InlayHints))
    }

    pub fn is_document_highlight_support(&self) -> bool {
        self.language_servers()
            .any(|ls| ls.supports_feature(LanguageServerFeature::DocumentHighlight))
    }

    pub fn is_document_symbols_support(&self) -> bool {
        self.language_servers()
            .any(|ls| ls.supports_feature(LanguageServerFeature::DocumentSymbols))
    }

    pub(crate) fn is_signature_help_support(&self) -> bool {
        self.language_servers()
            .any(|ls| ls.supports_feature(LanguageServerFeature::SignatureHelp))
    }

    fn set_language_config(&mut self, config_loader: Arc<syntax::Loader>) {
        let language_config =
            config_loader.language_config_for_file_name(self.path().unwrap().as_ref());
        self.language_config = language_config;
    }

    /// Language name for the document. Corresponds to the `name` key in `language.toml` configuration
    pub fn language_name(&self) -> Option<&str> {
        self.language_config
            .as_ref()
            .map(|language| language.language_id.as_str())
    }

    /// Langauge ID for the document. Either the `language-id`,
    /// or the document language name if no `language-id` has been specified.
    pub fn language_id(&self) -> Option<&str> {
        self.language_config()?
            .language_server_language_id
            .as_deref()
            .or_else(|| self.language_name())
    }

    pub fn language_config(&self) -> Option<&LanguageConfiguration> {
        self.language_config.as_deref()
    }

    /// Current document version, incremented at each change.
    pub fn version(&self) -> i32 {
        self.version
    }

    pub fn language_servers(&self) -> impl Iterator<Item = &Client> {
        self.language_config().into_iter().flat_map(move |config| {
            config.language_servers.iter().filter_map(move |features| {
                let ls = &**self.language_servers.get(&features.name)?;
                if ls.is_initialized() {
                    Some(ls)
                } else {
                    None
                }
            })
        })
    }

    pub fn get_all_language_servers(&self) -> Vec<Arc<Client>> {
        self.language_config()
            .into_iter()
            .flat_map(move |config| {
                config.language_servers.iter().filter_map(move |features| {
                    let ls = self.language_servers.get(&features.name).cloned()?;
                    if ls.is_initialized() {
                        Some(ls)
                    } else {
                        None
                    }
                })
            })
            .collect()
    }

    // -- LSP methods

    #[inline]
    pub fn identifier(&self) -> lsp::TextDocumentIdentifier {
        lsp::TextDocumentIdentifier::new(self.uri.clone())
    }

    #[inline]
    pub fn diagnostics(&self) -> &Option<Vec<DiagnosticItem>> {
        &self.diagnostics
    }

    pub fn replace_diagnostics(
        &mut self,
        mut diagnostics: Vec<DiagnosticItem>,
        language_server_id: usize,
    ) {
        self.clear_diagnostics(language_server_id);
        match self.diagnostics.as_mut() {
            Some(diags) => {
                diags.append(&mut diagnostics);
                // self.diagnostics
                //     .sort_unstable_by_key(|diagnostic| diagnostic.item.range.start);
                diags.sort_by(|a, b| {
                    if a.item.range.start.eq(&b.item.range.start) {
                        return a.item.range.end.partial_cmp(&b.item.range.end).unwrap();
                    } else {
                        return a.item.range.start.partial_cmp(&b.item.range.start).unwrap();
                    }
                })
            }
            None => {
                self.diagnostics = Some(diagnostics);
            }
        }
    }

    pub fn clear_diagnostics(&mut self, language_server_id: usize) {
        if let Some(diagnostics) = self.diagnostics.as_mut() {
            diagnostics.retain(|d| d.language_server_id != language_server_id);
        }
    }

    pub fn get_diagnostics_by_language_server_id(
        &self,
        language_server_id: usize,
    ) -> Option<Vec<&Diagnostic>> {
        self.diagnostics.as_ref().and_then(|diags| {
            let items: Vec<&Diagnostic> = diags
                .iter()
                .filter_map(|diagnostic| {
                    if diagnostic.language_server_id == language_server_id {
                        return Some(&diagnostic.item);
                    }
                    None
                })
                .collect();
            Some(items)
        })
    }
}

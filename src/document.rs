use crate::{
    client::Client,
    lsp_ext::CustomServerCapabilitiesParams,
    registry::LanguageServerName,
    syntax::{self, LanguageConfiguration, LanguageServerFeature},
};
use lsp::Diagnostic;
use lsp_types::{self as lsp, Url};
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

/// This type is cheap to clone: all data is either `Copy` or wrapped in an `Arc`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct DiagnosticProvider {
    pub server_id: usize,
    /// `identifier` is a field from the LSP "Pull Diagnostics" feature meant to provide an
    /// optional "namespace" for diagnostics: a language server can respond to a diagnostics
    /// pull request with an identifier and these diagnostics should be treated as separate
    /// from push diagnostics. Rust-analyzer uses this feature for example to provide Cargo
    /// diagnostics with push and internal diagnostics with pull. The push diagnostics should
    /// not clear the pull diagnostics and vice-versa.
    pub identifier: Option<String>,
}

#[derive(Debug, Clone)]
pub struct DiagnosticItem {
    pub item: Diagnostic,
    pub provider: DiagnosticProvider,
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

    pub previous_diagnostic_id: Option<String>,
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
            previous_diagnostic_id: None,
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

    pub fn get_server_capabilities(&self) -> CustomServerCapabilitiesParams {
        let mut server_capabilities = CustomServerCapabilitiesParams {
            uri: self.uri.to_string(),
            trigger_characters: vec![],
            support_inlay_hints: false,
            support_document_highlight: false,
            support_document_symbols: false,
            support_signature_help: false,
            support_pull_diagnostic: false,
            support_inline_completion: false,
        };

        self.language_servers().for_each(|ls| {
            if let Some(lsp_types::CompletionOptions {
                trigger_characters: Some(triggers),
                ..
            }) = &ls.capabilities().completion_provider
            {
                let mut triggers = triggers.clone();
                server_capabilities.trigger_characters.append(&mut triggers);
            }

            if ls.supports_feature(LanguageServerFeature::InlayHints) {
                server_capabilities.support_inlay_hints = true;
            }
            if ls.supports_feature(LanguageServerFeature::DocumentHighlight) {
                server_capabilities.support_document_highlight = true;
            }
            if ls.supports_feature(LanguageServerFeature::DocumentSymbols) {
                server_capabilities.support_document_symbols = true;
            }
            if ls.supports_feature(LanguageServerFeature::SignatureHelp) {
                server_capabilities.support_signature_help = true;
            }

            if ls.supports_feature(LanguageServerFeature::PullDiagnostics) {
                server_capabilities.support_pull_diagnostic = true;
            }

            if ls.supports_feature(LanguageServerFeature::InlineCompletion) {
                server_capabilities.support_inline_completion = true;
            }
        });

        server_capabilities
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

    pub fn language_servers_with_feature(
        &self,
        feature: LanguageServerFeature,
    ) -> Vec<Arc<Client>> {
        self.language_config()
            .into_iter()
            .flat_map(move |config| {
                config.language_servers.iter().filter_map(move |features| {
                    let ls = self.language_servers.get(&features.name).cloned()?;
                    if ls.is_initialized() && ls.with_feature(feature) {
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
        provider: &DiagnosticProvider,
    ) {
        self.clear_diagnostics(provider);
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

    pub fn clear_diagnostics(&mut self, provider: &DiagnosticProvider) {
        if let Some(diagnostics) = self.diagnostics.as_mut() {
            diagnostics.retain(|d| d.provider != *provider);
        }
    }

    pub fn clear_all_diagnostics(&mut self, server_id: usize) {
        if let Some(diagnostics) = self.diagnostics.as_mut() {
            diagnostics.retain(|d| d.provider.server_id != server_id);
        }
    }

    pub fn get_diagnostics_by_provider(
        &self,
        provider: &DiagnosticProvider,
    ) -> Option<Vec<&Diagnostic>> {
        self.diagnostics.as_ref().and_then(|diags| {
            let items: Vec<&Diagnostic> = diags
                .iter()
                .filter_map(|diagnostic| {
                    if diagnostic.provider == *provider {
                        return Some(&diagnostic.item);
                    }
                    None
                })
                .collect();
            Some(items)
        })
    }
}

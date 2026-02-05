use crate::{
    client::Client,
    lsp_ext::CustomServerCapabilitiesParams,
    registry::LanguageServerName,
    syntax::{self, LanguageConfiguration, LanguageServerFeature},
};
use lsp::Diagnostic;
use lsp_types::{self as lsp, Url};
use std::{collections::HashMap, num::NonZeroUsize, path::PathBuf, sync::Arc, time::Instant};

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
    #[allow(dead_code)]
    pub file_path: String,
}

/// Default TTL for virtual document servers (5 minutes)
pub const DEFAULT_VIRTUAL_DOC_SERVER_TTL_SECS: u64 = 300;

/// Entry for a cached virtual document language server with TTL tracking.
#[derive(Debug)]
pub struct VirtualDocServerEntry {
    pub client: Arc<Client>,
    pub last_used: Instant,
}

impl VirtualDocServerEntry {
    pub fn new(client: Arc<Client>) -> Self {
        Self {
            client,
            last_used: Instant::now(),
        }
    }

    /// Update the last used timestamp to now.
    pub fn touch(&mut self) {
        self.last_used = Instant::now();
    }

    /// Check if this entry has expired based on the given TTL.
    pub fn is_expired(&self, ttl_secs: u64) -> bool {
        self.last_used.elapsed().as_secs() > ttl_secs
    }
}

#[derive(Debug)]
pub struct VirtualDocumentInfo {
    #[allow(dead_code)]
    pub(crate) line_bias: u32,
    pub(crate) language: String,
    pub language_config: Option<Arc<LanguageConfiguration>>,
}

impl VirtualDocumentInfo {
    pub fn new(
        line_bias: u32,
        language: String,
        config_loader: Option<Arc<syntax::Loader>>,
    ) -> Self {
        let mut virtual_doc = VirtualDocumentInfo {
            line_bias,
            language: language.clone(),
            language_config: None,
        };

        if let Some(loader) = config_loader {
            virtual_doc.language_config = loader.language_config_for_language_id(&language);
        }

        virtual_doc
    }

    pub fn language_id(&self) -> &str {
        self.language_config
            .as_deref()
            .and_then(|c| c.language_server_language_id.as_deref())
            .unwrap_or(&self.language)
    }
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

    // If the document is a Org file, contains virtual document information
    pub virtual_doc: Option<VirtualDocumentInfo>,
    pub(crate) language_servers_of_virtual_doc: HashMap<LanguageServerName, VirtualDocServerEntry>,

    /// Cached result of is_org_file check
    is_org_file: bool,
}

impl Document {
    pub fn new(
        uri: &Url,
        config_loader: Option<Arc<syntax::Loader>>,
        language: Option<&str>,
    ) -> Self {
        // Pre-compute is_org_file
        let is_org_file = uri
            .to_file_path()
            .ok()
            .and_then(|path| path.extension().map(|ext| ext == "org"))
            .unwrap_or(false);

        let mut doc = Document {
            id: DocumentId::default(),
            uri: uri.clone(),
            language_config: None,
            language_servers: HashMap::new(),
            version: 0,
            diagnostics: None,
            previous_diagnostic_id: None,
            virtual_doc: None,
            language_servers_of_virtual_doc: HashMap::new(),
            is_org_file,
        };

        if let Some(loader) = config_loader {
            doc.set_language_config(loader, language);
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
            support_hover: false,
            text_document_sync_kind: "incremental".to_string(), // Default to incremental
            has_any_servers: false,
        };

        let mut has_any_servers = false;
        let mut all_support_incremental = true;

        self.language_servers().for_each(|ls| {
            has_any_servers = true;

            // Check text document sync capability
            let sync_kind = ls.get_text_document_sync_kind();
            if sync_kind == "full" {
                all_support_incremental = false;
            }

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

            if ls.supports_feature(LanguageServerFeature::Hover) {
                server_capabilities.support_hover = true;
            }
        });

        // Set has_any_servers flag
        server_capabilities.has_any_servers = has_any_servers;

        // If any server only supports full sync, use full sync for all
        // If all servers support incremental (or no servers), use incremental
        if has_any_servers && !all_support_incremental {
            server_capabilities.text_document_sync_kind = "full".to_string();
        }

        server_capabilities
    }

    /// Get server capabilities for virtual document (e.g., org babel code block).
    /// This is similar to get_server_capabilities but uses language_servers_of_virtual_doc.
    pub fn get_virtual_doc_server_capabilities(&self) -> CustomServerCapabilitiesParams {
        let mut server_capabilities = CustomServerCapabilitiesParams {
            uri: self.uri.to_string(),
            trigger_characters: vec![],
            support_inlay_hints: false,
            support_document_highlight: false,
            support_document_symbols: false,
            support_signature_help: false,
            support_pull_diagnostic: false,
            support_inline_completion: false,
            support_hover: false,
            text_document_sync_kind: "incremental".to_string(),
            has_any_servers: false,
        };

        let mut has_any_servers = false;
        let mut all_support_incremental = true;

        for entry in self.language_servers_of_virtual_doc.values() {
            let ls = &entry.client;
            has_any_servers = true;

            let sync_kind = ls.get_text_document_sync_kind();
            if sync_kind == "full" {
                all_support_incremental = false;
            }

            if let Some(lsp_types::CompletionOptions {
                trigger_characters: Some(triggers),
                ..
            }) = &ls.capabilities().completion_provider
            {
                let mut triggers = triggers.clone();
                server_capabilities.trigger_characters.append(&mut triggers);
            }

            if ls.supports_feature(LanguageServerFeature::SignatureHelp) {
                server_capabilities.support_signature_help = true;
            }
            if ls.supports_feature(LanguageServerFeature::Hover) {
                server_capabilities.support_hover = true;
            }
        }

        // Set has_any_servers flag
        server_capabilities.has_any_servers = has_any_servers;

        if has_any_servers && !all_support_incremental {
            server_capabilities.text_document_sync_kind = "full".to_string();
        }

        server_capabilities
    }

    fn set_language_config(&mut self, config_loader: Arc<syntax::Loader>, language: Option<&str>) {
        let language_config =
            config_loader.language_config_for_file_name(self.path().unwrap().as_ref(), language);
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
                        a.item.range.end.partial_cmp(&b.item.range.end).unwrap()
                    } else {
                        a.item.range.start.partial_cmp(&b.item.range.start).unwrap()
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
        self.diagnostics.as_ref().map(|diags| {
            let items: Vec<&Diagnostic> = diags
                .iter()
                .filter_map(|diagnostic| {
                    if diagnostic.provider == *provider {
                        return Some(&diagnostic.item);
                    }
                    None
                })
                .collect();
            items
        })
    }

    /// Reset document state including version, diagnostics, language config and language servers
    pub fn reset(&mut self) {
        self.version = 0;
        self.diagnostics = None;
        self.previous_diagnostic_id = None;
        self.language_servers.clear();
        self.virtual_doc = None;
        self.language_servers_of_virtual_doc.clear();
    }

    /// Check if this document is an Org file (cached)
    #[inline]
    pub fn is_org_file(&self) -> bool {
        self.is_org_file
    }

    /// Get a virtual document server by language, updating its last_used timestamp.
    pub fn get_virtual_doc_server(&mut self, language: &str) -> Option<Arc<Client>> {
        if let Some(entry) = self.language_servers_of_virtual_doc.get_mut(language) {
            entry.touch();
            Some(entry.client.clone())
        } else {
            None
        }
    }

    /// Check if a virtual document server exists for the given language.
    pub fn has_virtual_doc_server(&self, language: &str) -> bool {
        self.language_servers_of_virtual_doc.contains_key(language)
    }

    /// Insert a new virtual document server entry.
    pub fn insert_virtual_doc_server(&mut self, language: String, client: Arc<Client>) {
        self.language_servers_of_virtual_doc
            .insert(language, VirtualDocServerEntry::new(client));
    }

    /// Get expired virtual document server languages based on TTL.
    pub fn get_expired_virtual_doc_servers(&self, ttl_secs: u64) -> Vec<String> {
        self.language_servers_of_virtual_doc
            .iter()
            .filter(|(_, entry)| entry.is_expired(ttl_secs))
            .map(|(lang, _)| lang.clone())
            .collect()
    }

    /// Remove and return a virtual document server entry.
    pub fn remove_virtual_doc_server(&mut self, language: &str) -> Option<VirtualDocServerEntry> {
        self.language_servers_of_virtual_doc.remove(language)
    }
}

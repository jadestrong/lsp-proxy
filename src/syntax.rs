use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Display},
    path::Path,
    sync::Arc,
};

use serde::{ser::SerializeSeq, Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Configuration {
    pub language: Vec<LanguageConfiguration>,
    #[serde(default)]
    pub language_server: HashMap<String, LanguageServerConfiguration>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", deny_unknown_fields)]
pub struct LanguageConfiguration {
    #[serde(rename = "name")]
    pub language_id: String,
    #[serde(rename = "language-id")]
    pub language_server_language_id: Option<String>,
    #[serde(default)]
    pub roots: Vec<String>, // these indicate project roots <.git, Cargo.toml>
    pub file_types: Vec<FileType>,
    #[serde(
        default,
        skip_serializing_if = "Vec::is_empty",
        serialize_with = "serialize_lang_features",
        deserialize_with = "deserialize_lang_features"
    )]
    pub language_servers: Vec<LanguageServerFeatures>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FileType {
    /// The extension of the file, either the `Path::extension` or the full
    /// filename if the file does not have an extensio.
    Extension(String),
    /// A Unix-style path glob. This is compared to the file's absolute path, so
    /// it can be used to detect files based on their directories. If the glob
    /// is not an absolute path and does not already start with a glob pattern,
    /// a glob pattern will be prepended to it.
    Glob(globset::Glob),
}

impl Serialize for FileType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;

        match self {
            FileType::Extension(extension) => serializer.serialize_str(extension),
            FileType::Glob(glob) => {
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry("glob", glob.glob())?;
                map.end()
            }
        }
    }
}

impl<'de> Deserialize<'de> for FileType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct FileTypeVisitor;

        impl<'de> serde::de::Visitor<'de> for FileTypeVisitor {
            type Value = FileType;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("string or table")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(FileType::Extension(v.to_string()))
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                match map.next_entry::<String, String>()? {
                    Some((key, mut glob)) if key == "glob" => {
                        // If the glob isn't an absolute path or already starts
                        // with a glob pattern, add a leading glob so we
                        // properly match relative paths.
                        if !glob.starts_with('/') && !glob.starts_with("*/") {
                            glob.insert_str(0, "*/");
                        }

                        globset::Glob::new(glob.as_str())
                            .map(FileType::Glob)
                            .map_err(|err| {
                                serde::de::Error::custom(format!("invalid `glob` pattern: {}", err))
                            })
                    }
                    Some((key, _value)) => Err(serde::de::Error::custom(format!(
                        "unknown key in `file-types` list: {}",
                        key
                    ))),
                    None => Err(serde::de::Error::custom(
                        "expected a `suffix` key in the `file-types` entry",
                    )),
                }
            }
        }

        deserializer.deserialize_any(FileTypeVisitor)
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "kebab-case")]
pub enum LanguageServerFeature {
    Format,
    GotoDeclaration,
    GotoDefinition,
    GotoTypeDefinition,
    GotoReference,
    GotoImplementation,
    // Goto, use bitflags, combining previous Goto members?
    SignatureHelp,
    Hover,
    DocumentHighlight,
    Completion,
    CompletionResolve,
    CodeAction,
    WorkspaceCommand,
    DocumentSymbols,
    WorkspaceSymbols,
    // Symbols, use bitflags, see above?
    Diagnostics,
    RenameSymbol,
    InlayHints,
}

impl Display for LanguageServerFeature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LanguageServerFeature::*;
        let feature = match self {
            Format => "format",
            GotoDeclaration => "goto-declaration",
            GotoDefinition => "goto-definition",
            GotoTypeDefinition => "goto-type-definition",
            GotoReference => "goto-type-definition",
            GotoImplementation => "goto-implementation",
            SignatureHelp => "signature-help",
            Hover => "hover",
            DocumentHighlight => "document-highlight",
            Completion => "completion",
            CompletionResolve => "completion-resolve",
            CodeAction => "code-action",
            WorkspaceCommand => "workspace-command",
            DocumentSymbols => "document-symbols",
            WorkspaceSymbols => "workspace-symbols",
            Diagnostics => "diagnostics",
            RenameSymbol => "rename-symbol",
            InlayHints => "inlay-hints",
        };
        write!(f, "{feature}",)
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged, rename_all = "kebab-case", deny_unknown_fields)]
enum LanguageServerFeatureConfiguration {
    #[serde(rename_all = "kebab-case")]
    Features {
        #[serde(default, skip_serializing_if = "HashSet::is_empty")]
        only_features: HashSet<LanguageServerFeature>,
        #[serde(default, skip_serializing_if = "HashSet::is_empty")]
        except_features: HashSet<LanguageServerFeature>,
        name: String,
        #[serde(default)]
        support_workspace: bool,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        library_directories: Vec<String>,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        config_files: Vec<String>,
    },
    Simple(String),
}

#[derive(Debug, Default, Clone)]
pub struct LanguageServerFeatures {
    pub name: String,
    pub only: HashSet<LanguageServerFeature>,
    pub excluded: HashSet<LanguageServerFeature>,
    pub support_workspace: bool,
    pub library_directories: Vec<String>,
    pub config_files: Vec<String>,
}

impl LanguageServerFeatures {
    pub fn has_features(&self, feature: LanguageServerFeature) -> bool {
        (self.only.is_empty() || self.only.contains(&feature)) && !self.excluded.contains(&feature)
    }
}

fn deserialize_lang_features<'de, D>(
    deserializer: D,
) -> Result<Vec<LanguageServerFeatures>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    let raw: Vec<LanguageServerFeatureConfiguration> = Deserialize::deserialize(deserializer)?;
    let res = raw
        .into_iter()
        .map(|config| match config {
            LanguageServerFeatureConfiguration::Features {
                only_features,
                except_features,
                name,
                support_workspace,
                library_directories,
                config_files,
            } => LanguageServerFeatures {
                name,
                only: only_features,
                excluded: except_features,
                support_workspace,
                library_directories,
                config_files,
            },
            LanguageServerFeatureConfiguration::Simple(name) => LanguageServerFeatures {
                name,
                ..Default::default()
            },
        })
        .collect();

    Ok(res)
}

fn serialize_lang_features<S>(
    map: &Vec<LanguageServerFeatures>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let mut serializer = serializer.serialize_seq(Some(map.len()))?;
    for features in map {
        let features = if features.only.is_empty() && features.excluded.is_empty() {
            LanguageServerFeatureConfiguration::Simple(features.name.to_owned())
        } else {
            LanguageServerFeatureConfiguration::Features {
                only_features: features.only.clone(),
                except_features: features.excluded.clone(),
                name: features.name.to_owned(),
                support_workspace: features.support_workspace,
                library_directories: features.library_directories.clone(),
                config_files: features.config_files.clone(),
            }
        };
        serializer.serialize_element(&features)?;
    }
    serializer.end()
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct LanguageServerConfiguration {
    pub command: String,
    #[serde(default)]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub args: Vec<String>,
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub environment: HashMap<String, String>,
    #[serde(default, skip_serializing, deserialize_with = "deserialize_lsp_config")]
    pub config: Option<serde_json::Value>,
    #[serde(default = "default_timeout")]
    pub timeout: u64,
    #[serde(default, skip_serializing, deserialize_with = "deserialize_lsp_config")]
    pub experimental: Option<serde_json::Value>,
}

fn default_timeout() -> u64 {
    8
}

fn deserialize_lsp_config<'de, D>(deserializer: D) -> Result<Option<serde_json::Value>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    Option::<toml::Value>::deserialize(deserializer)?
        .map(|toml| toml.try_into().map_err(serde::de::Error::custom))
        .transpose()
}

#[derive(Debug)]
struct FileTypeGlob {
    glob: globset::Glob,
    language_id: usize,
}

impl FileTypeGlob {
    fn new(glob: globset::Glob, language_id: usize) -> Self {
        Self { glob, language_id }
    }
}

#[derive(Debug)]
struct FileTypeGlobMatcher {
    matcher: globset::GlobSet,
    file_types: Vec<FileTypeGlob>,
}

impl FileTypeGlobMatcher {
    fn new(file_types: Vec<FileTypeGlob>) -> Result<Self, globset::Error> {
        let mut builder = globset::GlobSetBuilder::new();
        for file_type in &file_types {
            builder.add(file_type.glob.clone());
        }

        Ok(Self {
            matcher: builder.build()?,
            file_types,
        })
    }

    fn language_id_for_path(&self, path: &Path) -> Option<&usize> {
        self.matcher
            .matches(path)
            .iter()
            .filter_map(|idx| self.file_types.get(*idx))
            .max_by_key(|file_type| file_type.glob.glob().len())
            .map(|file_type| &file_type.language_id)
    }
}

pub struct Loader {
    language_configs: Vec<Arc<LanguageConfiguration>>,
    language_config_ids_by_extension: HashMap<String, usize>,
    language_config_ids_glob_matcher: FileTypeGlobMatcher,
    language_server_configs: HashMap<String, LanguageServerConfiguration>,
}

pub type LoaderError = globset::Error;

#[allow(dead_code)]
impl Loader {
    pub fn new(config: Configuration) -> Result<Self, LoaderError> {
        let mut language_configs = Vec::new();
        let mut language_config_ids_by_extension = HashMap::new();
        let mut file_type_globs = Vec::new();
        for config in config.language {
            let language_id = language_configs.len();
            for file_type in &config.file_types {
                match file_type {
                    FileType::Extension(extension) => {
                        language_config_ids_by_extension.insert(extension.clone(), language_id);
                    }
                    FileType::Glob(glob) => {
                        file_type_globs.push(FileTypeGlob::new(glob.to_owned(), language_id));
                    }
                };
            }

            language_configs.push(Arc::new(config));
        }

        Ok(Self {
            language_configs,
            language_config_ids_by_extension,
            language_config_ids_glob_matcher: FileTypeGlobMatcher::new(file_type_globs)?,
            language_server_configs: config.language_server,
        })
    }

    pub fn language_config_for_file_name(&self, path: &Path) -> Option<Arc<LanguageConfiguration>> {
        // Find all the language configurations that match this file name
        // or a suffix of the file name.
        let configuration_id = self
            .language_config_ids_glob_matcher
            .language_id_for_path(path)
            .or_else(|| {
                path.extension()
                    .and_then(|extension| extension.to_str())
                    .and_then(|extension| self.language_config_ids_by_extension.get(extension))
            });

        configuration_id.and_then(|&id| self.language_configs.get(id).cloned())

        // TODO: content_regex handling conflict resolution
    }

    pub fn language_config_for_language_id(&self, id: &str) -> Option<Arc<LanguageConfiguration>> {
        self.language_configs
            .iter()
            .find(|config| config.language_id == id)
            .cloned()
    }

    pub fn language_configs(&self) -> impl Iterator<Item = &Arc<LanguageConfiguration>> {
        self.language_configs.iter()
    }

    pub fn language_server_configs(&self) -> &HashMap<String, LanguageServerConfiguration> {
        &self.language_server_configs
    }
}

use crate::{
    syntax,
    utils::{ensure_parent_dir, merge_toml_values},
};
use etcetera::{base_strategy::choose_base_strategy, BaseStrategy};
use log::debug;
use std::{fs, path::PathBuf, str::from_utf8};

static CONFIG_FILE: once_cell::sync::OnceCell<PathBuf> = once_cell::sync::OnceCell::new();
static LOG_FILE: once_cell::sync::OnceCell<PathBuf> = once_cell::sync::OnceCell::new();
static LOG_LEVEL: once_cell::sync::OnceCell<u64> = once_cell::sync::OnceCell::new();
static REMOTE_SERVER_MODE: once_cell::sync::OnceCell<bool> = once_cell::sync::OnceCell::new();
pub static MAX_COMPLETION_ITEMS: once_cell::sync::OnceCell<usize> =
    once_cell::sync::OnceCell::new();
pub static MAX_DIAGNOSTICS_PUSH: once_cell::sync::OnceCell<usize> =
    once_cell::sync::OnceCell::new();
pub static ENABLE_BYTECODE: once_cell::sync::OnceCell<bool> = once_cell::sync::OnceCell::new();
pub static ENABLE_SNIPPETS: once_cell::sync::OnceCell<bool> = once_cell::sync::OnceCell::new();

pub static COPILOT_SERVER_NAME: once_cell::sync::OnceCell<String> =
    once_cell::sync::OnceCell::new();

static REMOTE_BINARY_PATH: once_cell::sync::OnceCell<String> =
    once_cell::sync::OnceCell::new();

pub fn set_max_completion_items(max_items: usize) {
    MAX_COMPLETION_ITEMS.set(max_items).ok();
}

// Diagnostic configuration constants
pub const DEFAULT_MAX_DIAGNOSTICS_PUSH: usize = 50;

pub fn set_max_diagnostics_push(max_diagnostics: usize) {
    MAX_DIAGNOSTICS_PUSH.set(max_diagnostics).ok();
}

pub fn set_enable_bytecode(enable: bool) {
    ENABLE_BYTECODE.set(enable).ok();
}

pub fn set_enable_snippets(enable: bool) {
    ENABLE_SNIPPETS.set(enable).ok();
}

pub fn set_copilot_server_name(name: String) {
    COPILOT_SERVER_NAME.set(name).ok();
}

/// Initialise the remote binary path from the `LSP_PROXY_REMOTE_BINARY_PATH`
/// environment variable.  Call once at startup, before the remote manager is
/// used.  If the variable is absent or empty the built-in default is used.
pub fn initialize_remote_binary_path() {
    if let Ok(val) = std::env::var("LSP_PROXY_REMOTE_BINARY_PATH") {
        if !val.is_empty() {
            REMOTE_BINARY_PATH.set(val).ok();
        }
    }
}

pub fn remote_binary_path() -> &'static str {
    REMOTE_BINARY_PATH
        .get()
        .map(|s| s.as_str())
        .unwrap_or(crate::remote::deploy::DEFAULT_REMOTE_BINARY_PATH)
}

pub fn initialize_config_file(specified_file: Option<PathBuf>) {
    // Explicit --config flag takes priority.
    if let Some(file) = specified_file {
        if file.exists() && file.is_file() {
            CONFIG_FILE.set(file).unwrap();
        }
        return;
    }

    // In --remote-server mode, fall back to `languages.toml` beside the binary
    // so operators can drop a config file next to the deployed binary without
    // having to pass --config explicitly.
    if is_remote_server_mode() {
        if let Ok(exe) = std::env::current_exe() {
            if let Some(dir) = exe.parent() {
                let candidate = dir.join("languages.toml");
                if candidate.is_file() {
                    debug!(
                        "remote-server: using languages.toml beside binary: {}",
                        candidate.display()
                    );
                    CONFIG_FILE.set(candidate).ok();
                }
            }
        }
    }
}

pub fn default_log_file() -> PathBuf {
    // In --remote-server mode, put the log next to the binary so whoever
    // deploys it knows where to grep — the user's $HOME cache tree may not
    // even exist yet on a freshly-provisioned remote.
    if is_remote_server_mode() {
        if let Ok(exe) = std::env::current_exe() {
            if let Some(dir) = exe.parent() {
                return dir.join(format!("remote-server-{}.log", timestamp_now()));
            }
        }
    }
    let strategy = choose_base_strategy().expect("Unable to find the cache directory!");
    let mut path = strategy.cache_dir();
    path.push("lsp-proxy");
    path.join("default.log")
}

pub fn initialize_log_file(specified_file: Option<PathBuf>) {
    let log_file = specified_file.unwrap_or_else(default_log_file);
    ensure_parent_dir(&log_file);
    LOG_FILE.set(log_file).ok();
}

pub fn log_file() -> PathBuf {
    LOG_FILE.get().map(|path| path.to_path_buf()).unwrap()
}

pub fn set_log_level(level: u64) {
    LOG_LEVEL.set(level).ok();
}

/// Current log verbosity (0 = warn, 1 = info, 2 = debug, 3 = trace).
/// Used by the remote-dispatch code to forward the same level to the
/// remote-server process so its log isn't empty by default.
pub fn log_level() -> u64 {
    *LOG_LEVEL.get().unwrap_or(&0)
}

pub fn set_remote_server_mode(flag: bool) {
    REMOTE_SERVER_MODE.set(flag).ok();
}

pub fn is_remote_server_mode() -> bool {
    *REMOTE_SERVER_MODE.get().unwrap_or(&false)
}

fn default_lang_config() -> toml::Value {
    let default_config = include_bytes!("../languages.toml");
    toml::from_str(from_utf8(default_config).unwrap())
        .expect("Could not parse built-in language.toml to valid tom")
}

pub fn user_lang_config() -> Result<toml::Value, toml::de::Error> {
    if let Some(user_config_path) = CONFIG_FILE.get().map(|path| path.to_path_buf()) {
        let config = [user_config_path]
            .into_iter()
            .filter_map(|file| {
                fs::read_to_string(file)
                    .map(|config| toml::from_str(&config))
                    .ok()
            })
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .fold(default_lang_config(), |a, b| {
                // Merge user config (a) into default config (b)
                // Depth 5 allows merging:
                // 1. [[language]] arrays
                // 2. [language-server.xxx] tables
                // 3. config/args/environment tables
                // 4. nested config options (e.g., config.typescript.preferences)
                // 5. deep nested options
                merge_toml_values(a, b, 5)
            });
        Ok(config)
    } else {
        Ok(default_lang_config())
    }
}

pub fn default_syntax_loader() -> syntax::Configuration {
    debug!("load default_lang_config...");
    let config = match user_lang_config()
        .expect("Could not load user config")
        .try_into::<syntax::Configuration>()
    {
        Ok(config) => {
            // debug!("serialized config {config:?}");
            config
        }
        Err(err) => {
            panic!("Cound not serialize your custom languages.toml: {err}")
        }
    };
    config
}

/// Format the current local time as `YYYYMMDD-HHMMSS`, falling back to UTC.
fn timestamp_now() -> String {
    use time::macros::format_description;
    let fmt = format_description!("[year][month][day]-[hour][minute][second]");
    let now = time::OffsetDateTime::now_local()
        .unwrap_or_else(|_| time::OffsetDateTime::now_utc());
    now.format(fmt)
        .unwrap_or_else(|_| "00000000-000000".to_string())
}

#[cfg(test)]
mod builtin_config_tests {
    use super::default_lang_config;

    /// The built-in `languages.toml` must deserialize into the real configuration
    /// types, not merely parse as TOML.
    ///
    /// `default_syntax_loader` panics on failure, and it runs before anything else,
    /// so a malformed entry for one language takes down the proxy for every language
    /// — with a message about serialization rather than about the block that is
    /// wrong. `toml::from_str` succeeding is not enough to catch that: a field of the
    /// wrong shape parses fine as TOML and only fails at `try_into`.
    #[test]
    fn the_builtin_config_deserializes() {
        default_lang_config()
            .try_into::<crate::syntax::Configuration>()
            .expect("built-in languages.toml must deserialize");
    }

    /// Both Java backends must be defined, and defined the way they are launched.
    ///
    /// The `command` assertion is the load-bearing one: the official launcher derives
    /// its per-project `-data` workspace from the directory it starts in, while `args`
    /// here is a fixed list that cannot vary per project. Replacing it with a
    /// hand-assembled `java -jar ...equinox.launcher...` would therefore pin every
    /// project to one shared workspace.
    #[test]
    fn both_java_backends_are_configured() {
        let config: crate::syntax::Configuration = default_lang_config().try_into().unwrap();
        let servers = &config.language_server;

        let jdtls = servers
            .get("jdtls")
            .expect("jdtls must be defined as a language server");
        assert_eq!(
            jdtls.command, "jdtls",
            "must invoke the official launcher, not a hand-assembled java command"
        );
        assert!(
            !jdtls.args.iter().any(|a| a.contains("equinox.launcher")),
            "args must not hand-assemble the launcher: {:?}",
            jdtls.args
        );
        assert!(
            servers.contains_key("intellij"),
            "the existing backend must stay defined"
        );
    }

    /// Every declared `extendedClientCapabilities` switch must have a client handler.
    ///
    /// Declaring one without its handler is worse than leaving it off: the server
    /// then sends commands or notifications nothing can act on, and the feature looks
    /// broken rather than absent. This pins the exact set so adding a switch has to be
    /// a deliberate edit here as well as in the client.
    #[test]
    fn jdtls_declares_only_implemented_client_capabilities() {
        let config: crate::syntax::Configuration = default_lang_config().try_into().unwrap();
        let caps = config
            .language_server
            .get("jdtls")
            .and_then(|s| s.config.as_ref())
            .and_then(|c| c.get("extendedClientCapabilities"))
            .and_then(|c| c.as_object())
            .expect("jdtls must declare extendedClientCapabilities");

        let mut declared: Vec<&str> = caps
            .iter()
            .filter(|(_, v)| v.as_bool() == Some(true))
            .map(|(k, _)| k.as_str())
            .collect();
        declared.sort_unstable();

        assert_eq!(
            declared,
            vec![
                // goto-definition into a dependency; handled by the editor's
                // `java/classFileContents` fetch.
                "classFileContentsSupport",
                // handled by `lsp-proxy-java--client-command-handlers`
                "advancedGenerateAccessorsSupport",
                "advancedOrganizeImportsSupport",
                "overrideMethodsPromptSupport",
                // import progress; handled by `progress_report_to_progress` here
                "progressReportProvider",
                // no prompt and no command: the ordinary completion path applies the
                // extra edits already
                "resolveAdditionalTextEditsSupport",
            ]
            .into_iter()
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect::<Vec<_>>(),
            "a switch here without a client handler makes the server send commands \
             nothing can run"
        );
    }

    /// Kotlin stays on the IntelliJ backend: jdtls does not serve Kotlin at all, so
    /// switching it would leave Kotlin files with no server rather than a different
    /// one.
    #[test]
    fn kotlin_is_not_switched_to_jdtls() {
        let config: crate::syntax::Configuration = default_lang_config().try_into().unwrap();
        let kotlin = config
            .language
            .iter()
            .find(|l| l.language_id == "kotlin")
            .expect("kotlin must stay configured");
        let names: Vec<&str> = kotlin
            .language_servers
            .iter()
            .map(|f| f.name.as_str())
            .collect();
        assert!(
            !names.contains(&"jdtls"),
            "kotlin must not be routed to jdtls, got {names:?}"
        );
    }
}

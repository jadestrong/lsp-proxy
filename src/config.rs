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

pub fn set_copilot_server_name(name: String) {
    COPILOT_SERVER_NAME.set(name).ok();
}

pub fn set_remote_binary_path(path: String) {
    REMOTE_BINARY_PATH.set(path).ok();
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

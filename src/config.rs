use crate::{
    syntax,
    utils::{ensure_parent_dir, merge_toml_values},
};
use etcetera::{base_strategy::choose_base_strategy, BaseStrategy};
use log::debug;
use std::{fs, path::PathBuf, str::from_utf8};

static CONFIG_FILE: once_cell::sync::OnceCell<PathBuf> = once_cell::sync::OnceCell::new();
static LOG_FILE: once_cell::sync::OnceCell<PathBuf> = once_cell::sync::OnceCell::new();
pub static MAX_COMPLETION_ITEMS: once_cell::sync::OnceCell<usize> =
    once_cell::sync::OnceCell::new();
pub static MAX_DIAGNOSTICS_PUSH: once_cell::sync::OnceCell<usize> =
    once_cell::sync::OnceCell::new();
pub static ENABLE_BYTECODE: once_cell::sync::OnceCell<bool> =
    once_cell::sync::OnceCell::new();

pub fn set_max_completion_items(max_items: usize) {
    MAX_COMPLETION_ITEMS.set(max_items).ok();
}

pub fn set_max_diagnostics_push(max_diagnostics: usize) {
    MAX_DIAGNOSTICS_PUSH.set(max_diagnostics).ok();
}

pub fn set_enable_bytecode(enable: bool) {
    ENABLE_BYTECODE.set(enable).ok();
}

pub fn initialize_config_file(specified_file: Option<PathBuf>) {
    // check specified file exist and is file, then set CONFIG_FILE
    if let Some(file) = specified_file {
        if file.exists() && file.is_file() {
            CONFIG_FILE.set(file).unwrap();
        }
    }
}

pub fn default_log_file() -> PathBuf {
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
                // combines for example
                // b:
                //   [[language]]
                //   name = "toml"
                //   language-server = { command = "taplo", args = ["lsp", "stdio"] }
                //
                // a:
                //   [[language]]
                //   language-server = { command = "/usr/bin/taplo" }
                //
                // into:
                //   [[language]]
                //   name = "toml"
                //   language-server = { command = "/usr/bin/taplo" }
                //
                // thus it overrides the third depth-level of b with values of a if they exist, but otherwise merges their values
                merge_toml_values(a, b, 3)
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
            debug!("serialized config {config:?}");
            config
        }
        Err(err) => {
            panic!(
                "Cound not serialize your custom languages.toml: {err}"
            )
        }
    };
    config
}

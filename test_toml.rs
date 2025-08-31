// Test TOML parsing
use std::collections::HashMap;
use serde::{Deserialize, Serialize};

// Simulate the structures from the real code
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RemoteConfig {
    pub servers: HashMap<String, ServerConfig>,
    pub default_server: Option<String>,
    pub cache_settings: CacheSettings,
    pub connection_settings: ConnectionSettings,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerConfig {
    pub name: String,
    pub host: String,
    pub user: String,
    pub port: u16,
    pub workspace_root: String,
    pub mode: String, // Simplified for testing
    pub auth: AuthConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthConfig {
    pub method: String,
    pub key_file: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheSettings {
    pub enabled: bool,
    pub max_size_mb: usize,
    pub ttl_seconds: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConnectionSettings {
    pub timeout_seconds: u64,
    pub retry_attempts: u32,
    pub keepalive_interval: Option<u64>,
    pub compression: bool,
}

fn main() {
    let toml_content = r#"
[servers.dev-server]
name = "dev-server"
host = "dev.example.com"
user = "developer"
port = 22
workspace_root = "/home/developer/projects"
mode = "Direct"

[servers.dev-server.auth]
method = "key"
key_file = "~/.ssh/id_rsa"

default_server = "dev-server"

[cache_settings]
enabled = true
max_size_mb = 100
ttl_seconds = 3600

[connection_settings]
timeout_seconds = 30
retry_attempts = 3
keepalive_interval = 60
compression = true
"#;

    match toml::from_str::<RemoteConfig>(toml_content) {
        Ok(config) => println!("TOML parsed successfully: {:#?}", config),
        Err(e) => println!("TOML parse error: {}", e),
    }
}

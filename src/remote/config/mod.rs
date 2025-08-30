//! Configuration management for remote development

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

use crate::remote::{RemoteServerConfig, RemoteMode, RemoteAuth};

/// Remote development configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RemoteConfig {
    pub servers: HashMap<String, RemoteServerConfig>,
    pub default_server: Option<String>,
    pub cache_settings: CacheSettings,
    pub connection_settings: ConnectionSettings,
}

/// Cache configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheSettings {
    pub enabled: bool,
    pub max_size_mb: usize,
    pub ttl_seconds: u64,
    pub cache_dir: Option<PathBuf>,
}

/// Connection configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConnectionSettings {
    pub timeout_seconds: u64,
    pub retry_attempts: u32,
    pub keepalive_interval: Option<u64>,
    pub compression: bool,
}

impl Default for RemoteConfig {
    fn default() -> Self {
        Self {
            servers: HashMap::new(),
            default_server: None,
            cache_settings: CacheSettings::default(),
            connection_settings: ConnectionSettings::default(),
        }
    }
}

impl Default for CacheSettings {
    fn default() -> Self {
        Self {
            enabled: true,
            max_size_mb: 100,
            ttl_seconds: 3600, // 1 hour
            cache_dir: None,
        }
    }
}

impl Default for ConnectionSettings {
    fn default() -> Self {
        Self {
            timeout_seconds: 30,
            retry_attempts: 3,
            keepalive_interval: Some(60), // 1 minute
            compression: true,
        }
    }
}

/// Remote configuration manager
pub struct RemoteConfigManager {
    config_file: PathBuf,
    config: RemoteConfig,
}

impl RemoteConfigManager {
    /// Create new config manager
    pub fn new() -> Result<Self> {
        let config_dir = dirs::config_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join("lsp-proxy");
        
        std::fs::create_dir_all(&config_dir)?;
        
        let config_file = config_dir.join("remote.toml");
        let config = if config_file.exists() {
            let content = std::fs::read_to_string(&config_file)?;
            toml::from_str(&content)?
        } else {
            RemoteConfig::default()
        };
        
        Ok(Self {
            config_file,
            config,
        })
    }
    
    /// Create a minimal fallback config manager for error cases
    pub fn fallback() -> Self {
        Self {
            config_file: std::env::temp_dir().join("lsp-proxy-fallback.toml"),
            config: RemoteConfig::default(),
        }
    }
    
    /// Load configuration from file
    pub fn load(&mut self) -> Result<()> {
        if self.config_file.exists() {
            let content = std::fs::read_to_string(&self.config_file)?;
            self.config = toml::from_str(&content)?;
        }
        Ok(())
    }
    
    /// Save configuration to file
    pub fn save(&self) -> Result<()> {
        let content = toml::to_string_pretty(&self.config)?;
        std::fs::write(&self.config_file, content)?;
        Ok(())
    }
    
    /// Get server configuration by name
    pub fn get_server(&self, name: &str) -> Option<&RemoteServerConfig> {
        self.servers().get(name)
    }
    
    /// Add server configuration
    pub fn add_server(&mut self, name: String, config: RemoteServerConfig) {
        self.config.servers.insert(name, config);
    }
    
    /// Remove server configuration
    pub fn remove_server(&mut self, name: &str) -> Option<RemoteServerConfig> {
        self.config.servers.remove(name)
    }
    
    /// List all server names
    pub fn list_servers(&self) -> Vec<&str> {
        self.config.servers.keys().map(|s| s.as_str()).collect()
    }
    
    /// Get servers map
    pub fn servers(&self) -> &HashMap<String, RemoteServerConfig> {
        &self.config.servers
    }
    
    /// Get cache settings
    pub fn cache_settings(&self) -> &CacheSettings {
        &self.config.cache_settings
    }
    
    /// Get connection settings
    pub fn connection_settings(&self) -> &ConnectionSettings {
        &self.config.connection_settings
    }
    
    /// Set default server
    pub fn set_default_server(&mut self, name: Option<String>) {
        self.config.default_server = name;
    }
    
    /// Get default server
    pub fn get_default_server(&self) -> Option<&str> {
        self.config.default_server.as_deref()
    }
    
    /// Create example configuration
    pub fn create_example_config(&mut self) {
        // Add example server configurations
        let dev_server = RemoteServerConfig {
            name: "dev-server".to_string(),
            host: "dev.example.com".to_string(),
            port: Some(22),
            user: "developer".to_string(),
            auth: RemoteAuth::Key {
                path: dirs::home_dir().unwrap_or_default().join(".ssh/id_rsa"),
                passphrase: None,
            },
            workspace_root: Some(PathBuf::from("/home/developer/projects")),
            connection_timeout: None,
            mode: RemoteMode::Auto,
        };
        
        let prod_server = RemoteServerConfig {
            name: "prod-server".to_string(),
            host: "prod.example.com".to_string(),
            port: Some(22),
            user: "developer".to_string(),
            auth: RemoteAuth::Agent,
            workspace_root: Some(PathBuf::from("/opt/projects")),
            connection_timeout: Some(60),
            mode: RemoteMode::Server { auto_deploy: true, server_path: None },
        };
        
        self.config.servers.insert("dev-server".to_string(), dev_server);
        self.config.servers.insert("prod-server".to_string(), prod_server);
        self.config.default_server = Some("dev-server".to_string());
    }
    
    /// Validate configuration
    pub fn validate(&self) -> Result<Vec<String>> {
        let mut errors = Vec::new();
        
        for (name, server) in &self.config.servers {
            if server.host.is_empty() {
                errors.push(format!("Server '{}': host cannot be empty", name));
            }
            
            if server.user.is_empty() {
                errors.push(format!("Server '{}': user cannot be empty", name));
            }
            
            match &server.auth {
                RemoteAuth::Key { path, .. } => {
                    if !path.exists() {
                        errors.push(format!("Server '{}': SSH key file does not exist: {}", name, path.display()));
                    }
                },
                RemoteAuth::Password(pass) => {
                    if pass.is_empty() {
                        errors.push(format!("Server '{}': password cannot be empty", name));
                    }
                },
                RemoteAuth::Agent => {
                    // Can't validate agent availability without connecting
                }
                RemoteAuth::SshConfig { host } => {
                    // Try to parse SSH config to validate
                    use crate::remote::ssh_config::SshConfigParser;
                    let parser = SshConfigParser::new();
                    let config_host = host.as_ref().unwrap_or(&server.host);
                    
                    match parser.get_host_config(config_host) {
                        Ok(Some(_)) => {
                            // SSH config found, good
                        }
                        Ok(None) => {
                            errors.push(format!("Server '{}': No SSH configuration found for host '{}'", name, config_host));
                        }
                        Err(e) => {
                            errors.push(format!("Server '{}': Error reading SSH config: {}", name, e));
                        }
                    }
                }
            }
        }
        
        if let Some(default) = &self.config.default_server {
            if !self.config.servers.contains_key(default) {
                errors.push(format!("Default server '{}' is not defined", default));
            }
        }
        
        if self.config.cache_settings.max_size_mb == 0 {
            errors.push("Cache max size cannot be zero".to_string());
        }
        
        if self.config.connection_settings.timeout_seconds == 0 {
            errors.push("Connection timeout cannot be zero".to_string());
        }
        
        Ok(errors)
    }
}

/// Load remote configuration from default location
pub fn load_remote_config() -> Result<RemoteConfigManager> {
    RemoteConfigManager::new()
}

/// Create default cache directory
pub fn default_cache_dir() -> PathBuf {
    dirs::cache_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join("lsp-proxy")
        .join("remote")
}
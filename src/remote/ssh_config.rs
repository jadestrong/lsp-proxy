//! SSH config file parsing for automatic configuration
//! 
//! This module provides functionality to parse SSH configuration files
//! and automatically extract connection parameters like host, user, port, and identity files.

use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use anyhow::{anyhow, Result};
use log::{debug, warn};

/// SSH configuration for a specific host
#[derive(Debug, Clone)]
pub struct SshHostConfig {
    pub hostname: Option<String>,
    pub user: Option<String>,
    pub port: Option<u16>,
    pub identity_file: Option<PathBuf>,
    pub identity_files: Vec<PathBuf>,
}

impl Default for SshHostConfig {
    fn default() -> Self {
        Self {
            hostname: None,
            user: None,
            port: Some(22),
            identity_file: None,
            identity_files: Vec::new(),
        }
    }
}

/// SSH config file parser
pub struct SshConfigParser {
    config_path: PathBuf,
}

impl SshConfigParser {
    /// Create a new SSH config parser
    pub fn new() -> Self {
        let config_path = dirs::home_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join(".ssh")
            .join("config");
        
        Self { config_path }
    }
    
    /// Create parser with custom config file path
    pub fn with_config_path<P: AsRef<Path>>(path: P) -> Self {
        Self {
            config_path: path.as_ref().to_path_buf(),
        }
    }
    
    /// Parse SSH config file and return host configurations
    pub fn parse(&self) -> Result<HashMap<String, SshHostConfig>> {
        if !self.config_path.exists() {
            debug!("SSH config file not found: {}", self.config_path.display());
            return Ok(HashMap::new());
        }
        
        let content = fs::read_to_string(&self.config_path)
            .map_err(|e| anyhow!("Failed to read SSH config file: {}", e))?;
        
        self.parse_content(&content)
    }
    
    /// Parse SSH config content
    fn parse_content(&self, content: &str) -> Result<HashMap<String, SshHostConfig>> {
        let mut configs = HashMap::new();
        let mut current_host: Option<String> = None;
        let mut current_config = SshHostConfig::default();
        
        for line in content.lines() {
            let line = line.trim();
            
            // Skip comments and empty lines
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() < 2 {
                continue;
            }
            
            let keyword = parts[0].to_lowercase();
            let value = parts[1..].join(" ");
            
            match keyword.as_str() {
                "host" => {
                    // Save previous host config if exists
                    if let Some(host) = current_host.take() {
                        configs.insert(host, current_config.clone());
                    }
                    
                    // Start new host config
                    current_host = Some(value.clone());
                    current_config = SshHostConfig::default();
                    debug!("Parsing SSH config for host: {}", value);
                }
                "hostname" => {
                    current_config.hostname = Some(value);
                }
                "user" => {
                    current_config.user = Some(value);
                }
                "port" => {
                    if let Ok(port) = value.parse::<u16>() {
                        current_config.port = Some(port);
                    } else {
                        warn!("Invalid port in SSH config: {}", value);
                    }
                }
                "identityfile" => {
                    let path = self.expand_path(&value);
                    if current_config.identity_file.is_none() {
                        current_config.identity_file = Some(path.clone());
                    }
                    current_config.identity_files.push(path);
                }
                _ => {
                    // Ignore other SSH config options
                }
            }
        }
        
        // Save last host config
        if let Some(host) = current_host {
            configs.insert(host, current_config);
        }
        
        debug!("Parsed {} host configurations from SSH config", configs.len());
        Ok(configs)
    }
    
    /// Expand path with tilde and SSH config variables
    fn expand_path(&self, path: &str) -> PathBuf {
        let path = path.trim_matches('"');
        
        if path.starts_with('~') {
            if let Some(home) = dirs::home_dir() {
                let relative_path = path.strip_prefix('~').unwrap_or(path).trim_start_matches('/');
                home.join(relative_path)
            } else {
                PathBuf::from(path)
            }
        } else if path.starts_with("%d") {
            // %d represents ~/.ssh directory
            let ssh_dir = dirs::home_dir()
                .unwrap_or_else(|| PathBuf::from("."))
                .join(".ssh");
            let relative_path = path.strip_prefix("%d/").unwrap_or(path.strip_prefix("%d").unwrap_or(path));
            ssh_dir.join(relative_path)
        } else {
            PathBuf::from(path)
        }
    }
    
    /// Get configuration for a specific host
    pub fn get_host_config(&self, host: &str) -> Result<Option<SshHostConfig>> {
        let configs = self.parse()?;
        
        // Try exact match first
        if let Some(config) = configs.get(host) {
            return Ok(Some(config.clone()));
        }
        
        // Try wildcard matching (simple implementation)
        for (pattern, config) in &configs {
            if self.matches_pattern(pattern, host) {
                debug!("Host '{}' matches pattern '{}'", host, pattern);
                return Ok(Some(config.clone()));
            }
        }
        
        Ok(None)
    }
    
    /// Simple pattern matching for SSH host patterns
    fn matches_pattern(&self, pattern: &str, host: &str) -> bool {
        if pattern == "*" {
            return true;
        }
        
        if pattern.contains('*') {
            // Simple wildcard matching
            if pattern.ends_with('*') {
                let prefix = &pattern[..pattern.len() - 1];
                return host.starts_with(prefix);
            }
            
            if pattern.starts_with('*') {
                let suffix = &pattern[1..];
                return host.ends_with(suffix);
            }
        }
        
        pattern == host
    }
    
    /// Find the best identity file for a host
    pub fn find_identity_file(&self, config: &SshHostConfig) -> Option<PathBuf> {
        // Use explicit identity file if specified
        if let Some(ref identity_file) = config.identity_file {
            if identity_file.exists() {
                return Some(identity_file.clone());
            }
        }
        
        // Try all identity files from config
        for identity_file in &config.identity_files {
            if identity_file.exists() {
                return Some(identity_file.clone());
            }
        }
        
        // Try default identity files
        let ssh_dir = dirs::home_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join(".ssh");
        
        let default_keys = [
            "id_rsa",
            "id_ed25519", 
            "id_ecdsa",
            "id_dsa",
        ];
        
        for key_name in &default_keys {
            let key_path = ssh_dir.join(key_name);
            if key_path.exists() {
                debug!("Using default SSH key: {}", key_path.display());
                return Some(key_path);
            }
        }
        
        None
    }
}

impl Default for SshConfigParser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_ssh_config() {
        let config_content = r#"
# Default settings
Host *
    User defaultuser
    Port 22
    IdentityFile ~/.ssh/id_rsa

# Development server
Host dev
    HostName dev.example.com
    User developer
    Port 2222
    IdentityFile ~/.ssh/dev_key

# Production server with wildcard
Host prod*
    HostName prod.example.com  
    User admin
    Port 22
    IdentityFile ~/.ssh/prod_key
"#;
        
        let parser = SshConfigParser::new();
        let configs = parser.parse_content(config_content).unwrap();
        
        assert_eq!(configs.len(), 3);
        
        // Check wildcard config
        let wildcard_config = configs.get("*").unwrap();
        assert_eq!(wildcard_config.user, Some("defaultuser".to_string()));
        assert_eq!(wildcard_config.port, Some(22));
        
        // Check dev config
        let dev_config = configs.get("dev").unwrap();
        assert_eq!(dev_config.hostname, Some("dev.example.com".to_string()));
        assert_eq!(dev_config.user, Some("developer".to_string()));
        assert_eq!(dev_config.port, Some(2222));
        
        // Check prod config
        let prod_config = configs.get("prod*").unwrap();
        assert_eq!(prod_config.hostname, Some("prod.example.com".to_string()));
        assert_eq!(prod_config.user, Some("admin".to_string()));
    }
    
    #[test]
    fn test_pattern_matching() {
        let parser = SshConfigParser::new();
        
        assert!(parser.matches_pattern("*", "anything"));
        assert!(parser.matches_pattern("dev*", "dev-server"));
        assert!(parser.matches_pattern("*prod", "staging-prod"));
        assert!(parser.matches_pattern("exact", "exact"));
        assert!(!parser.matches_pattern("dev*", "production"));
    }
    
    #[test]
    fn test_path_expansion() {
        let parser = SshConfigParser::new();
        
        let expanded = parser.expand_path("~/.ssh/id_rsa");
        assert!(expanded.to_string_lossy().ends_with(".ssh/id_rsa"));
        
        let expanded = parser.expand_path("%d/config");
        assert!(expanded.to_string_lossy().ends_with(".ssh/config"));
    }
}
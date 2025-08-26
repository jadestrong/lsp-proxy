//! Authentication module for remote connections

use anyhow::Result;
use std::path::PathBuf;
use serde::{Deserialize, Serialize};

/// Authentication configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthConfig {
    pub method: AuthMethod,
    pub ssh_config_file: Option<PathBuf>,
    pub known_hosts_file: Option<PathBuf>,
}

/// Authentication methods
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AuthMethod {
    /// SSH key authentication
    SshKey {
        private_key_path: PathBuf,
        passphrase: Option<String>,
        public_key_path: Option<PathBuf>,
    },
    /// SSH password authentication
    Password(String),
    /// SSH agent authentication
    SshAgent,
    /// Interactive authentication
    Interactive,
}

impl Default for AuthConfig {
    fn default() -> Self {
        Self {
            method: AuthMethod::SshAgent,
            ssh_config_file: None,
            known_hosts_file: None,
        }
    }
}

impl AuthConfig {
    /// Load authentication config from system defaults
    pub fn from_system() -> Self {
        let home_dir = dirs::home_dir().unwrap_or_else(|| PathBuf::from("."));
        let ssh_dir = home_dir.join(".ssh");
        
        Self {
            method: AuthMethod::SshAgent,
            ssh_config_file: Some(ssh_dir.join("config")),
            known_hosts_file: Some(ssh_dir.join("known_hosts")),
        }
    }
    
    /// Load private key for SSH key authentication
    pub async fn load_private_key(&self) -> Result<Option<Vec<u8>>> {
        match &self.method {
            AuthMethod::SshKey { private_key_path, .. } => {
                let key_data = tokio::fs::read(private_key_path).await?;
                Ok(Some(key_data))
            },
            _ => Ok(None),
        }
    }
    
    /// Get password for authentication
    pub fn get_password(&self) -> Option<&str> {
        match &self.method {
            AuthMethod::Password(password) => Some(password),
            _ => None,
        }
    }
}

/// SSH key manager
pub struct SshKeyManager {
    key_dir: PathBuf,
}

impl SshKeyManager {
    /// Create new SSH key manager
    pub fn new() -> Self {
        let home_dir = dirs::home_dir().unwrap_or_else(|| PathBuf::from("."));
        Self {
            key_dir: home_dir.join(".ssh"),
        }
    }
    
    /// List available SSH keys
    pub async fn list_keys(&self) -> Result<Vec<PathBuf>> {
        let mut keys = Vec::new();
        
        let common_key_names = vec![
            "id_rsa", "id_ecdsa", "id_ed25519", "id_dsa"
        ];
        
        for key_name in common_key_names {
            let key_path = self.key_dir.join(key_name);
            if key_path.exists() {
                keys.push(key_path);
            }
        }
        
        Ok(keys)
    }
    
    /// Get default SSH key
    pub async fn default_key(&self) -> Result<Option<PathBuf>> {
        let keys = self.list_keys().await?;
        
        // Prefer ed25519, then ecdsa, then rsa
        for preferred in &["id_ed25519", "id_ecdsa", "id_rsa"] {
            if let Some(key) = keys.iter().find(|k| k.file_name().and_then(|n| n.to_str()) == Some(preferred)) {
                return Ok(Some(key.clone()));
            }
        }
        
        Ok(keys.first().cloned())
    }
}
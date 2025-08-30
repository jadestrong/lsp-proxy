//! SSH connection implementation
//! 
//! Provides SSH-based connectivity for remote development using the russh crate.

use super::{Connection, CommandResult, FileTransfer};
use crate::remote::{RemoteServerConfig, RemoteAuth};
use crate::remote::ssh_config::{SshConfigParser, SshHostConfig};
use anyhow::{anyhow, Result};
use async_trait::async_trait;
use log::{debug, info};
use std::sync::Arc;
use tokio::sync::Mutex;

/// SSH handler for the russh library
#[derive(Clone)]
struct SshHandler;

#[async_trait]
impl russh::client::Handler for SshHandler {
    type Error = anyhow::Error;
    
    async fn check_server_key(
        &mut self,
        _server_public_key: &russh_keys::key::PublicKey,
    ) -> Result<bool, Self::Error> {
        Ok(true) // In production, you should verify the server key
    }
}

/// SSH connection implementation
pub struct SSHConnection {
    config: RemoteServerConfig,
    session: Arc<Mutex<Option<russh::client::Handle<SshHandler>>>>,
    connected: Arc<Mutex<bool>>,
}

impl SSHConnection {
    /// Create a new SSH connection
    pub async fn new(config: RemoteServerConfig) -> Result<Self> {
        Ok(SSHConnection {
            config,
            session: Arc::new(Mutex::new(None)),
            connected: Arc::new(Mutex::new(false)),
        })
    }
    
    /// Apply SSH configuration to override connection settings
    async fn apply_ssh_config(&mut self, ssh_config: &SshHostConfig) -> Result<()> {
        Self::apply_ssh_config_to(&mut self.config, ssh_config)
    }
    
    /// Apply SSH configuration to a RemoteServerConfig
    fn apply_ssh_config_to(config: &mut RemoteServerConfig, ssh_config: &SshHostConfig) -> Result<()> {
        // Override connection settings from SSH config
        if let Some(ref hostname) = ssh_config.hostname {
            config.host = hostname.clone();
        }
        
        if let Some(ref user) = ssh_config.user {
            config.user = user.clone();
        }
        
        if let Some(port) = ssh_config.port {
            config.port = Some(port);
        }
        
        log::debug!("Applied SSH config: host={}, user={}, port={:?}", 
                   config.host, config.user, config.port);
        
        Ok(())
    }
}

#[async_trait]
impl Connection for SSHConnection {
    async fn connect(&self) -> Result<()> {
        debug!("Connecting to SSH server {}@{}", self.config.user, self.config.host);
        
        let config = russh::client::Config::default();
        let ssh_config = Arc::new(config);
        let handler = SshHandler;
        let mut session = russh::client::connect(
            ssh_config, 
            (&self.config.host[..], self.config.port.unwrap_or(22)),
            handler.clone()
        ).await?;
        
        // Authenticate based on config
        let auth_result = match &self.config.auth {
            RemoteAuth::Key { path, passphrase } => {
                // Expand tilde (~) in path
                let expanded_path = if path.to_string_lossy().starts_with('~') {
                    if let Some(home) = dirs::home_dir() {
                        let path_str = path.to_string_lossy();
                        let relative_path = path_str.strip_prefix('~').unwrap_or(&path_str).trim_start_matches('/');
                        home.join(relative_path)
                    } else {
                        path.clone()
                    }
                } else {
                    path.clone()
                };
                
                log::debug!("Attempting to load SSH key from: {}", expanded_path.display());
                let key_pair = russh_keys::load_secret_key(&expanded_path, passphrase.as_deref())?;
                session.authenticate_publickey(&self.config.user, Arc::new(key_pair)).await?
            },
            RemoteAuth::SshConfig { host } => {
                let parser = SshConfigParser::new();
                let config_host = host.as_ref().unwrap_or(&self.config.host);
                
                log::debug!("Attempting to parse SSH config for host: {}", config_host);
                match parser.get_host_config(config_host)? {
                    Some(ssh_config) => {
                        // Apply SSH config settings (need to create new config since self is immutable)
                        let mut updated_config = self.config.clone();
                        Self::apply_ssh_config_to(&mut updated_config, &ssh_config)?;
                        
                        // Use updated config for connection (temporarily override self.config)
                        let host = &updated_config.host;
                        let port = updated_config.port.unwrap_or(22);
                        let user = &updated_config.user;
                        
                        log::debug!("Connecting with SSH config: host={}, user={}, port={}", host, user, port);
                        
                        // Connect with updated config
                        let mut session = russh::client::connect(
                            Arc::new(russh::client::Config::default()),
                            (host.as_str(), port),
                            handler.clone()
                        ).await?;
                        
                        // Find identity file from SSH config
                        if let Some(identity_file) = parser.find_identity_file(&ssh_config) {
                            log::debug!("Using SSH key from config: {}", identity_file.display());
                            let key_pair = russh_keys::load_secret_key(&identity_file, None)?;
                            let auth_result = session.authenticate_publickey(user, Arc::new(key_pair)).await?;
                            
                            if !auth_result {
                                return Err(anyhow!("SSH authentication failed"));
                            }
                            
                            // Store the session
                            let mut session_guard = self.session.lock().await;
                            *session_guard = Some(session);
                            let mut connected_guard = self.connected.lock().await;
                            *connected_guard = true;
                            
                            return Ok(());
                        } else {
                            return Err(anyhow!("No suitable SSH identity file found for host '{}'", config_host));
                        }
                    }
                    None => {
                        return Err(anyhow!("No SSH configuration found for host '{}'", config_host));
                    }
                }
            },
            RemoteAuth::Password(password) => {
                session.authenticate_password(&self.config.user, password).await?
            },
            RemoteAuth::Agent => {
                // For agent authentication, we'll use default key
                return Err(anyhow!("Agent authentication not implemented yet"));
            }
        };

        if !auth_result {
            return Err(anyhow!("SSH authentication failed"));
        }

        *self.session.lock().await = Some(session);
        *self.connected.lock().await = true;
        
        info!("Successfully connected to SSH server {}@{}", self.config.user, self.config.host);
        Ok(())
    }

    async fn disconnect(&self) -> Result<()> {
        debug!("Disconnecting from SSH server");
        
        if let Some(session) = self.session.lock().await.take() {
            session.disconnect(russh::Disconnect::ByApplication, "", "").await?;
        }
        
        *self.connected.lock().await = false;
        info!("Disconnected from SSH server");
        Ok(())
    }

    fn is_connected(&self) -> bool {
        // This is a synchronous check, so we can't use async lock
        // We'll use a try_lock approach for a quick check
        self.connected.try_lock()
            .map(|guard| *guard)
            .unwrap_or(false)
    }

    async fn execute_command(&self, command: &str, args: &[&str]) -> Result<CommandResult> {
        let session_guard = self.session.lock().await;
        let session = session_guard.as_ref()
            .ok_or_else(|| anyhow!("SSH session not connected"))?;
        
        let full_command = if args.is_empty() {
            command.to_string()
        } else {
            format!("{} {}", command, args.join(" "))
        };
        
        debug!("Executing SSH command: {}", full_command);
        
        let mut channel = session.channel_open_session().await?;
        channel.exec(true, full_command.as_bytes()).await?;
        
        let mut stdout = Vec::new();
        let mut stderr = Vec::new();
        let mut exit_code = 0;
        
        // Read output
        while let Some(msg) = channel.wait().await {
            match msg {
                russh::ChannelMsg::Data { data } => {
                    stdout.extend_from_slice(&data);
                },
                russh::ChannelMsg::ExtendedData { data, ext: 1 } => {
                    stderr.extend_from_slice(&data);
                },
                russh::ChannelMsg::ExitStatus { exit_status } => {
                    exit_code = exit_status;
                },
                russh::ChannelMsg::Eof => break,
                _ => {}
            }
        }
        
        Ok(CommandResult {
            exit_code: exit_code as i32,
            stdout: String::from_utf8_lossy(&stdout).to_string(),
            stderr: String::from_utf8_lossy(&stderr).to_string(),
        })
    }

    async fn create_tunnel(&self, local_port: u16, remote_port: u16) -> Result<()> {
        let session_guard = self.session.lock().await;
        let _session = session_guard.as_ref()
            .ok_or_else(|| anyhow!("SSH session not connected"))?;
        
        // TODO: Implement SSH tunnel creation
        // This would involve setting up port forwarding
        debug!("Creating SSH tunnel from local:{} to remote:{}", local_port, remote_port);
        
        Ok(())
    }

    async fn file_transfer(&self) -> Result<Box<dyn FileTransfer>> {
        Ok(Box::new(SSHFileTransfer {
            connection: self.session.clone(),
        }))
    }
}

/// SFTP-based file transfer implementation
pub struct SSHFileTransfer {
    connection: Arc<Mutex<Option<russh::client::Handle<SshHandler>>>>,
}

#[async_trait]
impl FileTransfer for SSHFileTransfer {
    async fn upload(&self, local_path: &str, remote_path: &str) -> Result<()> {
        debug!("Uploading {} to {}", local_path, remote_path);
        
        // Read local file
        let content = tokio::fs::read(local_path).await?;
        self.write_file(remote_path, &content).await
    }

    async fn download(&self, remote_path: &str, local_path: &str) -> Result<()> {
        debug!("Downloading {} to {}", remote_path, local_path);
        
        let content = self.read_file(remote_path).await?;
        tokio::fs::write(local_path, content).await?;
        Ok(())
    }

    async fn read_file(&self, remote_path: &str) -> Result<Vec<u8>> {
        let session_guard = self.connection.lock().await;
        let session = session_guard.as_ref()
            .ok_or_else(|| anyhow!("SSH session not connected"))?;
        
        let mut channel = session.channel_open_session().await?;
        let command = format!("cat '{}'", remote_path);
        channel.exec(true, command.as_bytes()).await?;
        
        let mut content = Vec::new();
        while let Some(msg) = channel.wait().await {
            match msg {
                russh::ChannelMsg::Data { data } => {
                    content.extend_from_slice(&data);
                },
                russh::ChannelMsg::Eof => break,
                _ => {}
            }
        }
        
        Ok(content)
    }

    async fn write_file(&self, remote_path: &str, content: &[u8]) -> Result<()> {
        let session_guard = self.connection.lock().await;
        let session = session_guard.as_ref()
            .ok_or_else(|| anyhow!("SSH session not connected"))?;
        
        let mut channel = session.channel_open_session().await?;
        let command = format!("cat > '{}'", remote_path);
        channel.exec(true, command.as_bytes()).await?;
        
        // Send content
        channel.data(content).await?;
        channel.eof().await?;
        
        // Wait for completion
        while let Some(msg) = channel.wait().await {
            match msg {
                russh::ChannelMsg::Eof => break,
                _ => {}
            }
        }
        
        Ok(())
    }

    async fn exists(&self, remote_path: &str) -> Result<bool> {
        let session_guard = self.connection.lock().await;
        let session = session_guard.as_ref()
            .ok_or_else(|| anyhow!("SSH session not connected"))?;
        
        let mut channel = session.channel_open_session().await?;
        let command = format!("test -e '{}'", remote_path);
        channel.exec(true, command.as_bytes()).await?;
        
        let mut exit_code = 1;
        while let Some(msg) = channel.wait().await {
            match msg {
                russh::ChannelMsg::ExitStatus { exit_status } => {
                    exit_code = exit_status;
                },
                russh::ChannelMsg::Eof => break,
                _ => {}
            }
        }
        
        Ok(exit_code == 0)
    }

    async fn list_dir(&self, remote_path: &str) -> Result<Vec<String>> {
        let session_guard = self.connection.lock().await;
        let session = session_guard.as_ref()
            .ok_or_else(|| anyhow!("SSH session not connected"))?;
        
        let mut channel = session.channel_open_session().await?;
        let command = format!("ls -1 '{}'", remote_path);
        channel.exec(true, command.as_bytes()).await?;
        
        let mut stdout = Vec::new();
        while let Some(msg) = channel.wait().await {
            match msg {
                russh::ChannelMsg::Data { data } => {
                    stdout.extend_from_slice(&data);
                },
                russh::ChannelMsg::Eof => break,
                _ => {}
            }
        }
        
        let output = String::from_utf8_lossy(&stdout);
        Ok(output.lines().map(|s| s.to_string()).collect())
    }
}
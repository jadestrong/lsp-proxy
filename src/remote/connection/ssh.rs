//! SSH connection implementation
//! 
//! Provides SSH-based connectivity for remote development using the russh crate.

use super::{Connection, CommandResult, FileTransfer};
use crate::remote::{RemoteServerConfig, RemoteAuth};
use anyhow::{anyhow, Result};
use async_trait::async_trait;
use log::{debug, info};
use std::sync::Arc;
use tokio::sync::Mutex;

/// SSH handler for the russh library
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
            handler
        ).await?;
        
        // Authenticate based on config
        let auth_result = match &self.config.auth {
            RemoteAuth::Key { path, passphrase } => {
                let key_pair = russh_keys::load_secret_key(path, passphrase.as_deref())?;
                session.authenticate_publickey(&self.config.user, Arc::new(key_pair)).await?
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
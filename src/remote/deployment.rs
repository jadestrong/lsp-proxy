//! Auto-deployment functionality for Server Mode
//!
//! This module handles automatic deployment of lsp-proxy-server to remote machines
//! when Server Mode is configured with auto_deploy enabled.

use crate::remote::{connection::ssh::SSHConnection, connection::Connection, RemoteServerConfig, server_config::ServerConfig};
use anyhow::Result;
use log::{debug, error, info, warn};
use std::path::{Path, PathBuf};
use tokio::fs;
use tokio::process::Command;

/// Auto-deployment manager
pub struct AutoDeployManager {
    ssh_connection: SSHConnection,
    config: RemoteServerConfig,
}

impl AutoDeployManager {
    /// Create a new auto-deploy manager
    pub async fn new(config: RemoteServerConfig) -> Result<Self> {
        let ssh_connection = SSHConnection::new(config.clone()).await?;
        
        Ok(Self {
            ssh_connection,
            config,
        })
    }

    /// Deploy lsp-proxy-server to the remote machine
    pub async fn deploy(&mut self) -> Result<()> {
        info!("Starting auto-deployment for server: {}", self.config.name);

        // Connect to remote server via SSH
        self.ssh_connection.connect().await?;
        
        // Check if server is already deployed and up-to-date
        if self.is_server_up_to_date().await? {
            info!("lsp-proxy-server is already up-to-date on {}", self.config.name);
            return Ok(());
        }

        // Build the server binary locally
        let local_binary_path = self.build_server_binary().await?;
        
        // Determine remote binary path
        let remote_binary_path = self.get_remote_binary_path();
        
        // Upload binary to remote server
        self.upload_binary(&local_binary_path, &remote_binary_path).await?;
        
        // Upload configuration
        self.upload_configuration().await?;
        
        // Set up systemd service (optional)
        if self.should_setup_service() {
            self.setup_systemd_service(&remote_binary_path).await?;
        }
        
        // Verify deployment
        self.verify_deployment(&remote_binary_path).await?;
        
        info!("Auto-deployment completed successfully for {}", self.config.name);
        Ok(())
    }

    /// Check if server is already deployed and up-to-date
    async fn is_server_up_to_date(&self) -> Result<bool> {
        let remote_path = self.get_remote_binary_path();
        
        // Check if binary exists
        let file_transfer = self.ssh_connection.file_transfer().await?;
        if !file_transfer.exists(&remote_path.to_string_lossy()).await? {
            debug!("Server binary not found at {}", remote_path.display());
            return Ok(false);
        }
        
        // Get local version
        let local_version = self.get_local_version().await?;
        
        // Get remote version
        let remote_version = self.get_remote_version(&remote_path).await?;
        
        if local_version != remote_version {
            info!("Version mismatch: local={}, remote={}", local_version, remote_version);
            return Ok(false);
        }
        
        Ok(true)
    }

    /// Build the server binary locally
    async fn build_server_binary(&self) -> Result<PathBuf> {
        info!("Building lsp-proxy-server binary...");
        
        // Find the project root
        let project_root = self.find_project_root().await?;
        
        // Build the binary
        let output = Command::new("cargo")
            .current_dir(&project_root)
            .args(&["build", "--release", "--bin", "lsp-proxy-server"])
            .output()
            .await?;
            
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow::anyhow!("Failed to build server binary: {}", stderr));
        }
        
        let binary_path = project_root.join("target/release/lsp-proxy-server");
        if !binary_path.exists() {
            return Err(anyhow::anyhow!("Built binary not found at {:?}", binary_path));
        }
        
        info!("Server binary built successfully at {:?}", binary_path);
        Ok(binary_path)
    }

    /// Get the remote binary path
    fn get_remote_binary_path(&self) -> PathBuf {
        if let crate::remote::RemoteMode::Server { server_path: Some(path), .. } = &self.config.mode {
            path.clone()
        } else {
            PathBuf::from("/usr/local/bin/lsp-proxy-server")
        }
    }

    /// Upload binary to remote server
    async fn upload_binary(&self, local_path: &Path, remote_path: &Path) -> Result<()> {
        info!("Uploading binary from {:?} to {}:{:?}", 
              local_path, self.config.host, remote_path);
        
        // Read local binary
        let binary_content = fs::read(local_path).await?;
        
        // Upload to remote
        let file_transfer = self.ssh_connection.file_transfer().await?;
        file_transfer.write_file(&remote_path.to_string_lossy(), &binary_content).await?;
        
        // Make executable
        let chmod_cmd = format!("chmod +x {}", remote_path.to_string_lossy());
        self.ssh_connection.execute_command("sh", &["-c", &chmod_cmd]).await?;
        
        info!("Binary uploaded and made executable");
        Ok(())
    }

    /// Upload configuration files
    async fn upload_configuration(&self) -> Result<()> {
        info!("Uploading configuration...");
        
        // Create default server configuration
        let server_config = self.create_default_server_config();
        let config_content = toml::to_string_pretty(&server_config)?;
        
        // Create remote config directory
        let remote_config_dir = format!("/home/{}/.config/lsp-proxy", self.config.user);
        let mkdir_cmd = format!("mkdir -p {}", remote_config_dir);
        self.ssh_connection.execute_command("sh", &["-c", &mkdir_cmd]).await?;
        
        // Upload config
        let remote_config_path = format!("{}/server.toml", remote_config_dir);
        let file_transfer = self.ssh_connection.file_transfer().await?;
        file_transfer.write_file(&remote_config_path, config_content.as_bytes()).await?;
        
        info!("Configuration uploaded to {}", remote_config_path);
        Ok(())
    }

    /// Set up systemd service
    async fn setup_systemd_service(&self, binary_path: &Path) -> Result<()> {
        info!("Setting up systemd service...");
        
        let service_content = self.create_systemd_service(binary_path);
        let service_path = format!("/home/{}/.config/systemd/user/lsp-proxy-server.service", self.config.user);
        
        // Create systemd user directory
        let mkdir_cmd = format!("mkdir -p /home/{}/.config/systemd/user", self.config.user);
        self.ssh_connection.execute_command("sh", &["-c", &mkdir_cmd]).await?;
        
        // Write service file
        let file_transfer = self.ssh_connection.file_transfer().await?;
        file_transfer.write_file(&service_path, service_content.as_bytes()).await?;
        
        // Enable and start service
        let systemctl_commands = [
            "systemctl --user daemon-reload",
            "systemctl --user enable lsp-proxy-server",
            "systemctl --user start lsp-proxy-server",
        ];
        
        for cmd in &systemctl_commands {
            match self.ssh_connection.execute_command("sh", &["-c", cmd]).await {
                Ok(_) => debug!("Executed: {}", cmd),
                Err(e) => warn!("Failed to execute '{}': {}", cmd, e),
            }
        }
        
        info!("Systemd service set up");
        Ok(())
    }

    /// Verify deployment
    async fn verify_deployment(&self, binary_path: &Path) -> Result<()> {
        info!("Verifying deployment...");
        
        // Check binary exists and is executable
        let test_cmd = format!("{} --version", binary_path.to_string_lossy());
        match self.ssh_connection.execute_command("sh", &["-c", &test_cmd]).await {
            Ok(result) => {
                if result.exit_code == 0 {
                    info!("Deployment verified successfully. Version: {}", result.stdout.trim());
                } else {
                    return Err(anyhow::anyhow!("Binary test failed: {}", result.stderr));
                }
            },
            Err(e) => {
                return Err(anyhow::anyhow!("Failed to test binary: {}", e));
            }
        }
        
        // Test connection to server
        if self.should_test_connection() {
            self.test_server_connection().await?;
        }
        
        Ok(())
    }

    /// Test connection to deployed server
    async fn test_server_connection(&self) -> Result<()> {
        info!("Testing server connection...");
        
        // Wait a bit for server to start
        tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;
        
        // Try to connect to server
        let server_config = RemoteServerConfig {
            name: format!("{}-test", self.config.name),
            host: self.config.host.clone(),
            port: Some(7878), // Default server port
            user: self.config.user.clone(),
            auth: self.config.auth.clone(),
            workspace_root: self.config.workspace_root.clone(),
            connection_timeout: Some(10),
            mode: crate::remote::RemoteMode::Server { auto_deploy: false, server_path: None },
        };
        
        let test_connection = crate::remote::connection::server_mode::ServerModeConnection::new(server_config).await?;
        
        match test_connection.connect().await {
            Ok(()) => {
                info!("Server connection test successful");
                test_connection.disconnect().await?;
            },
            Err(e) => {
                warn!("Server connection test failed (this is expected if server is not running): {}", e);
            }
        }
        
        Ok(())
    }

    /// Find project root directory
    async fn find_project_root(&self) -> Result<PathBuf> {
        let mut current = std::env::current_dir()?;
        
        loop {
            if current.join("Cargo.toml").exists() {
                return Ok(current);
            }
            
            match current.parent() {
                Some(parent) => current = parent.to_path_buf(),
                None => return Err(anyhow::anyhow!("Could not find Cargo.toml")),
            }
        }
    }

    /// Get local version
    async fn get_local_version(&self) -> Result<String> {
        // Read from Cargo.toml or use git commit
        let project_root = self.find_project_root().await?;
        
        // Try git first
        if let Ok(output) = Command::new("git")
            .current_dir(&project_root)
            .args(&["rev-parse", "--short", "HEAD"])
            .output()
            .await
        {
            if output.status.success() {
                let commit = String::from_utf8_lossy(&output.stdout).trim().to_string();
                return Ok(format!("git-{}", commit));
            }
        }
        
        // Fallback to package version
        Ok(env!("CARGO_PKG_VERSION").to_string())
    }

    /// Get remote version
    async fn get_remote_version(&self, binary_path: &Path) -> Result<String> {
        let cmd = format!("{} --version", binary_path.to_string_lossy());
        
        match self.ssh_connection.execute_command("sh", &["-c", &cmd]).await {
            Ok(result) if result.exit_code == 0 => {
                // Parse version from output
                let output = result.stdout.trim();
                if let Some(version) = output.split_whitespace().nth(1) {
                    Ok(version.to_string())
                } else {
                    Ok(output.to_string())
                }
            },
            _ => Ok("unknown".to_string()),
        }
    }

    /// Create default server configuration
    fn create_default_server_config(&self) -> ServerConfig {
        ServerConfig::default()
    }

    /// Create systemd service content
    fn create_systemd_service(&self, binary_path: &Path) -> String {
        format!(
            r#"[Unit]
Description=LSP-Proxy Server
After=network.target

[Service]
Type=simple
ExecStart={} --port 7878
Restart=always
RestartSec=5
User={}
WorkingDirectory=/home/{}

[Install]
WantedBy=default.target
"#,
            binary_path.to_string_lossy(),
            self.config.user,
            self.config.user
        )
    }

    /// Check if systemd service should be set up
    fn should_setup_service(&self) -> bool {
        // Only set up service if auto_deploy is true
        matches!(self.config.mode, crate::remote::RemoteMode::Server { auto_deploy: true, .. })
    }

    /// Check if server connection should be tested
    fn should_test_connection(&self) -> bool {
        true // Always test for now
    }
}

/// Deploy server to remote machine
pub async fn deploy_server(config: RemoteServerConfig) -> Result<()> {
    let mut deployer = AutoDeployManager::new(config).await?;
    deployer.deploy().await
}
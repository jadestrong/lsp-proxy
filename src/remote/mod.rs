pub mod deploy;
pub mod detector;
pub mod rpc;
pub mod router;
pub mod server;
pub mod ssh;

pub use detector::{RemoteDetector, RemoteInfo, RemotePathInfo};
pub use router::RemoteConnectionManager;
pub use rpc::{RpcClient, RpcServer};
pub use ssh::{SshConnection, SshConnectionOptions};

use anyhow::Result;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RemoteType {
    Ssh,
    Rpc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RemoteHost {
    pub remote_type: RemoteType,
    pub user: String,
    pub host: String,
    pub port: Option<u16>,
}

impl RemoteHost {
    pub fn connection_key(&self) -> String {
        match self.port {
            Some(port) => format!("{}@{}:{}", self.user, self.host, port),
            None => format!("{}@{}", self.user, self.host),
        }
    }
}
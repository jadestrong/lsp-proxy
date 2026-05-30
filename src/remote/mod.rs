pub mod deploy;
pub mod detector;
pub mod router;
pub mod rpc;
pub mod server;
pub mod ssh;

pub use router::RemoteConnectionManager;


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
        // Empty user means "let ssh resolve via config" — omit the `@` so
        // the key we cache matches how ssh would see the destination.
        let user_part = if self.user.is_empty() {
            String::new()
        } else {
            format!("{}@", self.user)
        };
        // Include remote_type in the key so `/ssh:user@host:` and
        // `/rpc:user@host:` don't collapse to the same cached client —
        // their unsolicited-message bridges rewrite URIs back with
        // different TRAMP prefixes.
        let type_str = match self.remote_type {
            RemoteType::Ssh => "ssh",
            RemoteType::Rpc => "rpc",
        };
        match self.port {
            Some(port) => format!("{}:{}{}:{}", type_str, user_part, self.host, port),
            None => format!("{}:{}{}", type_str, user_part, self.host),
        }
    }
}

use anyhow::Result;
use regex::Regex;

use super::{RemoteHost, RemoteType};

/// 远程文件信息
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RemoteInfo {
    pub host: RemoteHost,
    pub remote_path: String,
}

/// 路径信息，可能是本地或远程
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RemotePathInfo {
    Local(String),
    Remote(RemoteInfo),
}

/// 远程文件检测器
pub struct RemoteDetector {
    ssh_regex: Regex,
    rpc_regex: Regex,
}

impl RemoteDetector {
    pub fn new() -> Result<Self> {
        // SSH TRAMP 格式: /ssh:user@host:/path/to/file 或 /ssh:user@host#port:/path/to/file
        let ssh_regex = Regex::new(r"^/ssh:([^@]+)@([^:#]+)(?:#(\d+))?:(.*)$")?;

        // RPC TRAMP 格式: /rpc:user@host:/path/to/file 或 /rpc:user@host#port:/path/to/file
        let rpc_regex = Regex::new(r"^/rpc:([^@]+)@([^:#]+)(?:#(\d+))?:(.*)$")?;

        Ok(Self {
            ssh_regex,
            rpc_regex,
        })
    }

    /// 解析路径，判断是本地路径还是远程路径
    pub fn parse_path(&self, path: &str) -> RemotePathInfo {
        // 尝试解析 SSH 路径
        if let Some(captures) = self.ssh_regex.captures(path) {
            let user = captures.get(1).unwrap().as_str().to_string();
            let host = captures.get(2).unwrap().as_str().to_string();
            let port = captures.get(3).map(|m| m.as_str().parse().ok()).flatten();
            let remote_path = captures.get(4).unwrap().as_str().to_string();

            return RemotePathInfo::Remote(RemoteInfo {
                host: RemoteHost {
                    remote_type: RemoteType::Ssh,
                    user,
                    host,
                    port,
                },
                remote_path,
            });
        }

        // 尝试解析 RPC 路径
        if let Some(captures) = self.rpc_regex.captures(path) {
            let user = captures.get(1).unwrap().as_str().to_string();
            let host = captures.get(2).unwrap().as_str().to_string();
            let port = captures.get(3).map(|m| m.as_str().parse().ok()).flatten();
            let remote_path = captures.get(4).unwrap().as_str().to_string();

            return RemotePathInfo::Remote(RemoteInfo {
                host: RemoteHost {
                    remote_type: RemoteType::Rpc,
                    user,
                    host,
                    port,
                },
                remote_path,
            });
        }

        // 默认为本地路径
        RemotePathInfo::Local(path.to_string())
    }

    /// 将本地路径转换为远程路径表示
    pub fn local_to_remote_path(&self, local_path: &str, remote_info: &RemoteInfo) -> String {
        // 简化实现：直接将本地路径作为远程路径
        // 实际应用中可能需要更复杂的路径映射逻辑
        local_path.to_string()
    }

    /// 将远程路径转换为本地路径表示
    pub fn remote_to_local_path(&self, remote_info: &RemoteInfo) -> String {
        // 构造 TRAMP 格式的路径
        let remote_type_str = match remote_info.host.remote_type {
            RemoteType::Ssh => "ssh",
            RemoteType::Rpc => "rpc",
        };

        match remote_info.host.port {
            Some(port) => format!(
                "/{}:{}@{}#{}:{}",
                remote_type_str,
                remote_info.host.user,
                remote_info.host.host,
                port,
                remote_info.remote_path
            ),
            None => format!(
                "/{}:{}@{}:{}",
                remote_type_str,
                remote_info.host.user,
                remote_info.host.host,
                remote_info.remote_path
            ),
        }
    }

    /// 检查是否为远程路径
    pub fn is_remote_path(&self, path: &str) -> bool {
        matches!(self.parse_path(path), RemotePathInfo::Remote(_))
    }

    /// 提取远程信息（如果是远程路径）
    pub fn extract_remote_info(&self, path: &str) -> Option<RemoteInfo> {
        match self.parse_path(path) {
            RemotePathInfo::Remote(info) => Some(info),
            RemotePathInfo::Local(_) => None,
        }
    }

    /// 检查路径是否使用指定的远程类型
    pub fn is_remote_type(&self, path: &str, remote_type: RemoteType) -> bool {
        if let Some(info) = self.extract_remote_info(path) {
            info.host.remote_type == remote_type
        } else {
            false
        }
    }
}

impl Default for RemoteDetector {
    fn default() -> Self {
        Self::new().expect("Failed to create RemoteDetector")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ssh_path_parsing() {
        let detector = RemoteDetector::new().unwrap();

        // 测试标准 SSH 路径
        let path = "/ssh:user@example.com:/home/user/file.txt";
        match detector.parse_path(path) {
            RemotePathInfo::Remote(info) => {
                assert_eq!(info.host.remote_type, RemoteType::Ssh);
                assert_eq!(info.host.user, "user");
                assert_eq!(info.host.host, "example.com");
                assert_eq!(info.host.port, None);
                assert_eq!(info.remote_path, "/home/user/file.txt");
            }
            _ => panic!("Expected remote path"),
        }

        // 测试带端口的 SSH 路径
        let path = "/ssh:admin@192.168.1.100#2222:/var/log/test.log";
        match detector.parse_path(path) {
            RemotePathInfo::Remote(info) => {
                assert_eq!(info.host.remote_type, RemoteType::Ssh);
                assert_eq!(info.host.user, "admin");
                assert_eq!(info.host.host, "192.168.1.100");
                assert_eq!(info.host.port, Some(2222));
                assert_eq!(info.remote_path, "/var/log/test.log");
            }
            _ => panic!("Expected remote path"),
        }
    }

    #[test]
    fn test_rpc_path_parsing() {
        let detector = RemoteDetector::new().unwrap();

        let path = "/rpc:dev@server.local:/project/src/main.rs";
        match detector.parse_path(path) {
            RemotePathInfo::Remote(info) => {
                assert_eq!(info.host.remote_type, RemoteType::Rpc);
                assert_eq!(info.host.user, "dev");
                assert_eq!(info.host.host, "server.local");
                assert_eq!(info.host.port, None);
                assert_eq!(info.remote_path, "/project/src/main.rs");
            }
            _ => panic!("Expected remote path"),
        }
    }

    #[test]
    fn test_local_path_parsing() {
        let detector = RemoteDetector::new().unwrap();

        let path = "/home/user/local/file.txt";
        match detector.parse_path(path) {
            RemotePathInfo::Local(local_path) => {
                assert_eq!(local_path, "/home/user/local/file.txt");
            }
            _ => panic!("Expected local path"),
        }
    }

    #[test]
    fn test_path_conversion() {
        let detector = RemoteDetector::new().unwrap();

        let remote_info = RemoteInfo {
            host: RemoteHost {
                remote_type: RemoteType::Ssh,
                user: "test".to_string(),
                host: "example.com".to_string(),
                port: Some(22),
            },
            remote_path: "/home/test/file.rs".to_string(),
        };

        let tramp_path = detector.remote_to_local_path(&remote_info);
        assert_eq!(tramp_path, "/ssh:test@example.com#22:/home/test/file.rs");

        // 测试往返转换
        match detector.parse_path(&tramp_path) {
            RemotePathInfo::Remote(parsed_info) => {
                assert_eq!(parsed_info, remote_info);
            }
            _ => panic!("Expected remote path after round-trip conversion"),
        }
    }
}
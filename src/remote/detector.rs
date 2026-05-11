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
        // 兼容三种情况:
        //   1. 裸 TRAMP 路径: /ssh:user@host:/path  或  /ssh:host:/path  或
        //      /ssh:user@host#port:/path
        //   2. eglot/Emacs 侧经 `file://` 包装的同样路径
        //   3. RPC 同理
        // user@ 是可选的(SSH config 别名没有用户名),file:// 前缀也是可选的。
        let ssh_regex = Regex::new(
            r"^(?:file://)?/ssh:(?:([^@:/#]+)@)?([^:#/]+)(?:#(\d+))?:(.*)$",
        )?;
        let rpc_regex = Regex::new(
            r"^(?:file://)?/rpc:(?:([^@:/#]+)@)?([^:#/]+)(?:#(\d+))?:(.*)$",
        )?;

        Ok(Self {
            ssh_regex,
            rpc_regex,
        })
    }

    /// 解析路径，判断是本地路径还是远程路径
    pub fn parse_path(&self, path: &str) -> RemotePathInfo {
        // 尝试解析 SSH 路径 (user@ 可选, file:// 前缀可选)
        if let Some(captures) = self.ssh_regex.captures(path) {
            let user = captures
                .get(1)
                .map(|m| m.as_str().to_string())
                .unwrap_or_default();
            let host = captures.get(2).unwrap().as_str().to_string();
            let port = captures.get(3).and_then(|m| m.as_str().parse().ok());
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

        // 尝试解析 RPC 路径 (user@ 可选)
        if let Some(captures) = self.rpc_regex.captures(path) {
            let user = captures
                .get(1)
                .map(|m| m.as_str().to_string())
                .unwrap_or_default();
            let host = captures.get(2).unwrap().as_str().to_string();
            let port = captures.get(3).and_then(|m| m.as_str().parse().ok());
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
    #[allow(dead_code)]
    pub fn local_to_remote_path(&self, local_path: &str, _remote_info: &RemoteInfo) -> String {
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
        // `user@` 只在有 user 时出现,否则按 TRAMP 的惯例省略。
        let user_part = if remote_info.host.user.is_empty() {
            String::new()
        } else {
            format!("{}@", remote_info.host.user)
        };
        match remote_info.host.port {
            Some(port) => format!(
                "/{}:{}{}#{}:{}",
                remote_type_str,
                user_part,
                remote_info.host.host,
                port,
                remote_info.remote_path
            ),
            None => format!(
                "/{}:{}{}:{}",
                remote_type_str, user_part, remote_info.host.host, remote_info.remote_path
            ),
        }
    }

    /// 检查是否为远程路径
    #[allow(dead_code)]
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
    #[allow(dead_code)]
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
    fn ssh_path_without_user_uses_empty_user() {
        // TRAMP path against an SSH config alias: `/ssh:home:/path/file`.
        // There's no user@ segment because the alias config has `User …`.
        let detector = RemoteDetector::new().unwrap();
        match detector.parse_path("/ssh:home:/Users/jadestrong/proj/file.ts") {
            RemotePathInfo::Remote(info) => {
                assert_eq!(info.host.remote_type, RemoteType::Ssh);
                assert_eq!(info.host.user, "");
                assert_eq!(info.host.host, "home");
                assert_eq!(info.host.port, None);
                assert_eq!(info.remote_path, "/Users/jadestrong/proj/file.ts");
            }
            _ => panic!("expected remote"),
        }
    }

    #[test]
    fn file_uri_wrapping_is_accepted() {
        // eglot's path-to-uri produces `file://` prefix on a TRAMP path.
        let detector = RemoteDetector::new().unwrap();
        match detector.parse_path("file:///ssh:home:/Users/jadestrong/proj/file.ts") {
            RemotePathInfo::Remote(info) => {
                assert_eq!(info.host.user, "");
                assert_eq!(info.host.host, "home");
                assert_eq!(info.remote_path, "/Users/jadestrong/proj/file.ts");
            }
            _ => panic!("expected remote"),
        }

        // file://-wrapped with user@ form.
        match detector.parse_path("file:///ssh:alice@box:/home/alice/a.py") {
            RemotePathInfo::Remote(info) => {
                assert_eq!(info.host.user, "alice");
                assert_eq!(info.host.host, "box");
            }
            _ => panic!("expected remote"),
        }
    }

    #[test]
    fn rpc_with_ip_user_and_realistic_path() {
        // Exactly the URI emacs-tramp-rpc will hand us in production:
        //   /rpc:<user>@<ip>:<absolute remote path>
        // wrapped in file:// by our own path-to-uri.
        let detector = RemoteDetector::new().unwrap();
        let uri =
            "file:///rpc:jadestrong@100.127.163.35:/Users/jadestrong/Documents/Github/vtsls/packages/server/src/index.ts";
        match detector.parse_path(uri) {
            RemotePathInfo::Remote(info) => {
                assert_eq!(info.host.remote_type, RemoteType::Rpc);
                assert_eq!(info.host.user, "jadestrong");
                assert_eq!(info.host.host, "100.127.163.35");
                assert_eq!(info.host.port, None);
                assert_eq!(
                    info.remote_path,
                    "/Users/jadestrong/Documents/Github/vtsls/packages/server/src/index.ts"
                );
            }
            _ => panic!("expected remote, got {:?}", detector.parse_path(uri)),
        }
    }

    #[test]
    fn remote_to_local_omits_at_when_user_empty() {
        let detector = RemoteDetector::new().unwrap();
        let info = RemoteInfo {
            host: RemoteHost {
                remote_type: RemoteType::Ssh,
                user: "".to_string(),
                host: "home".to_string(),
                port: None,
            },
            remote_path: "/a/b.ts".to_string(),
        };
        assert_eq!(detector.remote_to_local_path(&info), "/ssh:home:/a/b.ts");
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
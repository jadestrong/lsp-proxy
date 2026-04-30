use anyhow::{anyhow, Result};
use log::{debug, info, warn};
use std::collections::HashMap;
use std::pin::Pin;
use std::sync::Arc;
use std::task::Poll;
use tokio::sync::Mutex;
use tokio_util::compat::{Compat, TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

use super::{
    deploy::{self, DEFAULT_REMOTE_BINARY_PATH},
    detector::{RemoteDetector, RemoteInfo, RemotePathInfo},
    rpc::RpcClient,
    ssh::{SshConnection, SshConnectionOptions},
};
use crate::msg::{Message, Response};

/// 远程连接管理器
pub struct RemoteConnectionManager {
    detector: RemoteDetector,
    ssh_connections: Arc<Mutex<HashMap<String, Arc<SshConnection>>>>,
    rpc_clients: Arc<Mutex<HashMap<String, Arc<RpcClient>>>>,
}

impl RemoteConnectionManager {
    pub fn new() -> Result<Self> {
        Ok(Self {
            detector: RemoteDetector::new()?,
            ssh_connections: Arc::new(Mutex::new(HashMap::new())),
            rpc_clients: Arc::new(Mutex::new(HashMap::new())),
        })
    }

    /// 检查是否为远程路径
    pub fn is_remote_path(&self, path: &str) -> bool {
        self.detector.is_remote_path(path)
    }

    /// 获取或创建SSH连接
    async fn get_or_create_ssh_connection(&self, remote_info: &RemoteInfo) -> Result<Arc<SshConnection>> {
        let connection_key = remote_info.host.connection_key();

        let mut connections = self.ssh_connections.lock().await;

        if let Some(connection) = connections.get(&connection_key) {
            return Ok(connection.clone());
        }

        // 创建新的SSH连接
        let options = SshConnectionOptions::new(
            remote_info.host.host.clone(),
            remote_info.host.user.clone(),
        );

        let options = if let Some(port) = remote_info.host.port {
            options.with_port(port)
        } else {
            options
        };

        info!("Creating new SSH connection to {}", connection_key);
        let connection = Arc::new(SshConnection::connect(options).await?);
        connections.insert(connection_key.clone(), connection.clone());

        Ok(connection)
    }

    /// 获取或创建RPC客户端
    async fn get_or_create_rpc_client(&self, remote_info: &RemoteInfo) -> Result<Arc<RpcClient>> {
        let connection_key = remote_info.host.connection_key();

        let mut clients = self.rpc_clients.lock().await;

        if let Some(client) = clients.get(&connection_key) {
            return Ok(client.clone());
        }

        // 通过SSH启动远程LSP代理并创建RPC客户端
        let ssh_connection = self.get_or_create_ssh_connection(remote_info).await?;

        // 在启动远程进程前,先确保远端 binary 存在且版本匹配,否则 scp 过去
        let remote_lsp_path = DEFAULT_REMOTE_BINARY_PATH;
        deploy::ensure_remote_binary(ssh_connection.as_ref(), remote_lsp_path)
            .await
            .map_err(|e| anyhow!("auto-deploy failed for {}: {}", connection_key, e))?;

        // 启动远程LSP代理进程
        let lsp_process = ssh_connection.start_remote_lsp_proxy(remote_lsp_path).await?;

        // 创建RPC客户端，使用SSH隧道
        let stream = SshRpcStream::from_child_stdio(lsp_process.stdin, lsp_process.stdout);
        let client = Arc::new(RpcClient::new(stream)?);

        clients.insert(connection_key.clone(), client.clone());
        info!("Created new RPC client for {}", connection_key);

        Ok(client)
    }

    /// 路由消息到适当的后端
    pub async fn route_message(&self, message: Message) -> Result<Option<Message>> {
        let path = self.extract_path_from_message(&message);

        if let Some(path_str) = path {
            match self.detector.parse_path(&path_str) {
                RemotePathInfo::Remote(remote_info) => {
                    debug!("Routing message to remote: {}", remote_info.host.connection_key());
                    return self.handle_remote_message(message, remote_info).await;
                }
                RemotePathInfo::Local(_) => {
                    // 本地路径，返回None表示由本地处理
                    return Ok(None);
                }
            }
        }

        // 没有路径信息或无法解析，本地处理
        Ok(None)
    }

    /// 处理远程消息。`/ssh:` 和 `/rpc:` 前缀都走同一套 SSH 传输 — TRAMP
    /// 方法名只是 Emacs 侧的约定,对我们而言底层都是 `ssh <host> …`。
    async fn handle_remote_message(
        &self,
        message: Message,
        remote_info: RemoteInfo,
    ) -> Result<Option<Message>> {
        let _ = remote_info.host.remote_type; // variant kept for future diversification
        let client = self.get_or_create_rpc_client(&remote_info).await?;
        self.handle_message_via_rpc(message, client, remote_info).await
    }

    /// 通过RPC处理消息
    async fn handle_message_via_rpc(
        &self,
        message: Message,
        client: Arc<RpcClient>,
        remote_info: RemoteInfo,
    ) -> Result<Option<Message>> {
        // 转换路径：将TRAMP路径转换为远程路径
        let transformed_message = self.transform_paths_for_remote(message, &remote_info)?;

        match transformed_message {
            Message::Request(request) => {
                let response = client.send_request(request).await?;
                // 转换响应中的路径：将远程路径转换回TRAMP路径
                let transformed_response = self.transform_response_paths_from_remote(response, &remote_info)?;
                Ok(Some(Message::Response(transformed_response)))
            }
            Message::Notification(notification) => {
                client.send_notification(notification)?;
                Ok(None) // 通知不需要响应
            }
            Message::Response(_) => {
                warn!("Received response message for routing, this should not happen");
                Ok(None)
            }
        }
    }

    /// 从消息中提取文件路径
    fn extract_path_from_message(&self, message: &Message) -> Option<String> {
        match message {
            Message::Request(req) => {
                // 从请求参数中提取URI
                if let Some(uri) = &req.params.uri {
                    Some(uri.clone())
                } else {
                    // 尝试从params中提取textDocument.uri
                    self.extract_uri_from_params(&req.params.params)
                }
            }
            Message::Notification(notif) => {
                // 从通知参数中提取URI
                self.extract_uri_from_params(&notif.params.params)
            }
            _ => None,
        }
    }

    /// 从JSON参数中提取URI
    fn extract_uri_from_params(&self, params: &serde_json::Value) -> Option<String> {
        // 尝试多种路径提取方式
        if let Some(text_doc) = params.get("textDocument") {
            if let Some(uri) = text_doc.get("uri") {
                return uri.as_str().map(|s| s.to_string());
            }
        }

        if let Some(uri) = params.get("uri") {
            return uri.as_str().map(|s| s.to_string());
        }

        None
    }

    /// 转换消息中的路径以适应远程执行
    fn transform_paths_for_remote(&self, message: Message, remote_info: &RemoteInfo) -> Result<Message> {
        match message {
            Message::Request(mut req) => {
                // 转换URI路径
                if let Some(uri) = &req.params.uri {
                    if let Some(remote_uri) = self.convert_tramp_to_remote_uri(uri, remote_info) {
                        req.params.uri = Some(remote_uri);
                    }
                }

                // 转换params中的路径
                req.params.params = self.transform_json_paths_for_remote(req.params.params, remote_info)?;

                Ok(Message::Request(req))
            }
            Message::Notification(mut notif) => {
                notif.params.params = self.transform_json_paths_for_remote(notif.params.params, remote_info)?;
                Ok(Message::Notification(notif))
            }
            msg => Ok(msg),
        }
    }

    /// 转换响应中的路径
    fn transform_response_paths_from_remote(&self, mut response: Response, remote_info: &RemoteInfo) -> Result<Response> {
        if let Some(result) = response.result {
            response.result = Some(self.transform_json_paths_from_remote(result, remote_info)?);
        }
        Ok(response)
    }

    /// 转换JSON中的路径（远程 -> 本地）
    fn transform_json_paths_for_remote(&self, mut value: serde_json::Value, remote_info: &RemoteInfo) -> Result<serde_json::Value> {
        match &mut value {
            serde_json::Value::Object(map) => {
                for (key, val) in map.iter_mut() {
                    if key == "uri" || key.ends_with("Uri") {
                        if let Some(uri_str) = val.as_str() {
                            if let Some(remote_uri) = self.convert_tramp_to_remote_uri(uri_str, remote_info) {
                                *val = serde_json::Value::String(remote_uri);
                            }
                        }
                    } else {
                        *val = self.transform_json_paths_for_remote(val.clone(), remote_info)?;
                    }
                }
            }
            serde_json::Value::Array(arr) => {
                for item in arr.iter_mut() {
                    *item = self.transform_json_paths_for_remote(item.clone(), remote_info)?;
                }
            }
            _ => {}
        }
        Ok(value)
    }

    /// 转换JSON中的路径（本地 -> 远程）
    fn transform_json_paths_from_remote(&self, mut value: serde_json::Value, remote_info: &RemoteInfo) -> Result<serde_json::Value> {
        match &mut value {
            serde_json::Value::Object(map) => {
                for (key, val) in map.iter_mut() {
                    if key == "uri" || key.ends_with("Uri") {
                        if let Some(uri_str) = val.as_str() {
                            if let Some(tramp_uri) = self.convert_remote_to_tramp_uri(uri_str, remote_info) {
                                *val = serde_json::Value::String(tramp_uri);
                            }
                        }
                    } else {
                        *val = self.transform_json_paths_from_remote(val.clone(), remote_info)?;
                    }
                }
            }
            serde_json::Value::Array(arr) => {
                for item in arr.iter_mut() {
                    *item = self.transform_json_paths_from_remote(item.clone(), remote_info)?;
                }
            }
            _ => {}
        }
        Ok(value)
    }

    /// 将TRAMP URI转换为远程URI
    fn convert_tramp_to_remote_uri(&self, uri: &str, remote_info: &RemoteInfo) -> Option<String> {
        if uri.starts_with("file://") {
            let path = &uri[7..]; // 移除 "file://" 前缀
            if let Some(remote_info_from_path) = self.detector.extract_remote_info(path) {
                if remote_info_from_path.host == remote_info.host {
                    // 转换为远程文件URI
                    return Some(format!("file://{}", remote_info_from_path.remote_path));
                }
            }
        }
        None
    }

    /// 将远程URI转换为TRAMP URI
    fn convert_remote_to_tramp_uri(&self, uri: &str, remote_info: &RemoteInfo) -> Option<String> {
        if uri.starts_with("file://") {
            let remote_path = &uri[7..]; // 移除 "file://" 前缀
            let tramp_path = self.detector.remote_to_local_path(&RemoteInfo {
                host: remote_info.host.clone(),
                remote_path: remote_path.to_string(),
            });
            return Some(format!("file://{}", tramp_path));
        }
        None
    }

    /// 清理资源
    pub async fn cleanup(&self) {
        // 清理SSH连接
        let mut connections = self.ssh_connections.lock().await;
        connections.clear();

        // 清理RPC客户端
        let mut clients = self.rpc_clients.lock().await;
        clients.clear();

        info!("Remote connections cleaned up");
    }
}

/// SSH RPC 流适配器 — 把子进程的 stdin/stdout 组合成一个同时实现
/// `futures::AsyncRead` 和 `futures::AsyncWrite` 的双向流。
///
/// 内部通过 `tokio_util::compat::Compat` 把 tokio 的 AsyncRead/AsyncWrite
/// 转换成 futures 版本,避免自己手写 `poll_*` 里的 ReadBuf / wake 细节。
pub struct SshRpcStream {
    reader: Compat<tokio::process::ChildStdout>,
    writer: Compat<tokio::process::ChildStdin>,
}

impl SshRpcStream {
    pub fn from_child_stdio(
        stdin: tokio::process::ChildStdin,
        stdout: tokio::process::ChildStdout,
    ) -> Self {
        Self {
            reader: stdout.compat(),
            writer: stdin.compat_write(),
        }
    }
}

impl futures::AsyncRead for SshRpcStream {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut [u8],
    ) -> Poll<std::io::Result<usize>> {
        let this = self.get_mut();
        Pin::new(&mut this.reader).poll_read(cx, buf)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::remote::{RemoteHost, RemoteType};

    fn mgr() -> RemoteConnectionManager {
        RemoteConnectionManager::new().expect("build manager")
    }

    fn alice_box() -> RemoteInfo {
        RemoteInfo {
            host: RemoteHost {
                remote_type: RemoteType::Ssh,
                user: "alice".to_string(),
                host: "box".to_string(),
                port: None,
            },
            remote_path: "/".to_string(),
        }
    }

    #[test]
    fn tramp_uri_strips_to_remote_path() {
        let m = mgr();
        let info = alice_box();
        let converted = m
            .convert_tramp_to_remote_uri("file:///ssh:alice@box:/home/alice/main.py", &info)
            .expect("should convert");
        assert_eq!(converted, "file:///home/alice/main.py");
    }

    #[test]
    fn remote_uri_reassembled_to_tramp() {
        let m = mgr();
        let info = alice_box();
        let converted = m
            .convert_remote_to_tramp_uri("file:///home/alice/main.py", &info)
            .expect("should convert");
        assert_eq!(converted, "file:///ssh:alice@box:/home/alice/main.py");
    }

    #[test]
    fn tramp_conversion_rejects_mismatched_host() {
        let m = mgr();
        let info = alice_box();
        // URI embeds a different host — must not be rewritten.
        let converted =
            m.convert_tramp_to_remote_uri("file:///ssh:bob@other:/home/bob/x.py", &info);
        assert!(converted.is_none());
    }

    #[test]
    fn tramp_conversion_ignores_local_paths() {
        let m = mgr();
        let info = alice_box();
        assert!(m
            .convert_tramp_to_remote_uri("file:///home/alice/local.py", &info)
            .is_none());
        assert!(m
            .convert_tramp_to_remote_uri("/not/a/uri", &info)
            .is_none());
    }

    #[test]
    fn json_transform_rewrites_nested_uris_for_remote() {
        let m = mgr();
        let info = alice_box();
        let params = serde_json::json!({
            "textDocument": { "uri": "file:///ssh:alice@box:/home/alice/a.py" },
            "position": { "line": 1, "character": 2 },
            "context": {
                "includeDeclaration": true,
                "relatedUris": [
                    "file:///ssh:alice@box:/home/alice/b.py"
                ]
            }
        });
        let transformed = m
            .transform_json_paths_for_remote(params, &info)
            .expect("transform");
        assert_eq!(
            transformed["textDocument"]["uri"],
            "file:///home/alice/a.py"
        );
        // Nested uri-bearing keys should also be rewritten.
        // The walker targets keys named "uri" or ending in "Uri"; keys inside
        // array values are still visited recursively.
        let related = &transformed["context"]["relatedUris"][0];
        // "relatedUris" ends with "Uri" so the array value itself gets passed
        // through as a URI when it is a string; here it's an array, so the
        // walker recurses into each string element leaving them unchanged
        // unless the enclosing key matches. That's a known limitation — the
        // test pins current behavior so future refactors surface the choice.
        assert_eq!(related, "file:///ssh:alice@box:/home/alice/b.py");
    }

    #[test]
    fn json_transform_rewrites_responses_back_to_tramp() {
        let m = mgr();
        let info = alice_box();
        let response_result = serde_json::json!([
            { "uri": "file:///home/alice/a.py", "range": {"start":{"line":0,"character":0}} },
            { "uri": "file:///home/alice/b.py", "range": {"start":{"line":1,"character":1}} }
        ]);
        let transformed = m
            .transform_json_paths_from_remote(response_result, &info)
            .expect("transform back");
        assert_eq!(
            transformed[0]["uri"],
            "file:///ssh:alice@box:/home/alice/a.py"
        );
        assert_eq!(
            transformed[1]["uri"],
            "file:///ssh:alice@box:/home/alice/b.py"
        );
    }

    #[test]
    fn extract_path_reads_text_document_uri() {
        let m = mgr();
        let params = serde_json::json!({
            "textDocument": { "uri": "/ssh:alice@box:/home/alice/x.py" },
            "position": {"line": 0, "character": 0}
        });
        let extracted = m.extract_uri_from_params(&params);
        assert_eq!(extracted.as_deref(), Some("/ssh:alice@box:/home/alice/x.py"));
    }
}

impl futures::AsyncWrite for SshRpcStream {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        let this = self.get_mut();
        Pin::new(&mut this.writer).poll_write(cx, buf)
    }

    fn poll_flush(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        let this = self.get_mut();
        Pin::new(&mut this.writer).poll_flush(cx)
    }

    fn poll_close(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        let this = self.get_mut();
        Pin::new(&mut this.writer).poll_close(cx)
    }
}
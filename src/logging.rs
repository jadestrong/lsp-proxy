use serde_json::Value;
use tracing::{debug, error, info};
use tracing_subscriber::fmt::time;

/// A structured logger for LSP server communication
pub struct LspLogger {
    server_name: String,
    server_id: Option<u32>,
}

#[allow(dead_code)]
impl LspLogger {
    /// Create a new LSP logger for a specific server
    pub fn new(server_name: String, server_id: Option<u32>) -> Self {
        Self {
            server_name,
            server_id,
        }
    }

    /// Log an outgoing LSP request
    pub fn log_request(&self, method: &str, content: &str) {
        match self.pretty_print_json(content) {
            Ok(formatted) => {
                // Use info level for LSP communication logs
                info!(
                    server = %self.server_name,
                    server_id = self.server_id,
                    direction = "outgoing",
                    message_type = "request",
                    method = method,
                    "LSP Request:\n{}",
                    formatted
                );
            }
            Err(_) => {
                info!(
                    server = %self.server_name,
                    server_id = self.server_id,
                    direction = "outgoing",
                    message_type = "request",
                    method = method,
                    raw_content = content,
                    "LSP Request (invalid JSON)"
                );
            }
        }
    }

    /// Log an incoming LSP response
    pub fn log_response(&self, content: &str) {
        match self.pretty_print_json(content) {
            Ok(formatted) => {
                // Use info level for LSP communication logs
                info!(
                    server = %self.server_name,
                    server_id = self.server_id,
                    direction = "incoming",
                    message_type = "response",
                    "LSP Response:\n{}",
                    formatted
                );
            }
            Err(_) => {
                info!(
                    server = %self.server_name,
                    server_id = self.server_id,
                    direction = "incoming",
                    message_type = "response",
                    raw_content = content,
                    "LSP Response (invalid JSON)"
                );
            }
        }
    }

    /// Log an incoming LSP notification
    pub fn log_notification(&self, method: &str, content: &str) {
        match self.pretty_print_json(content) {
            Ok(formatted) => {
                // Use info level for LSP communication logs
                info!(
                    server = %self.server_name,
                    server_id = self.server_id,
                    direction = "incoming",
                    message_type = "notification",
                    method = method,
                    "LSP Notification:\n{}",
                    formatted
                );
            }
            Err(_) => {
                info!(
                    server = %self.server_name,
                    server_id = self.server_id,
                    direction = "incoming",
                    message_type = "notification",
                    method = method,
                    raw_content = content,
                    "LSP Notification (invalid JSON)"
                );
            }
        }
    }

    /// Log LSP server errors
    pub fn log_error(&self, error: &str) {
        error!(
            server = %self.server_name,
            server_id = self.server_id,
            message_type = "error",
            "LSP Server Error: {}",
            error
        );
    }

    /// Log general debug information
    pub fn log_debug(&self, message: &str) {
        debug!(
            server = %self.server_name,
            server_id = self.server_id,
            "{}",
            message
        );
    }

    /// Log general info
    pub fn log_info(&self, message: &str) {
        info!(
            server = %self.server_name,
            server_id = self.server_id,
            "{}",
            message
        );
    }

    /// Pretty print JSON if possible, otherwise return error
    fn pretty_print_json(&self, json_str: &str) -> Result<String, serde_json::Error> {
        let value: Value = serde_json::from_str(json_str)?;
        serde_json::to_string_pretty(&value)
    }

    /// Extract method from JSON-RPC message
    pub fn extract_method(json_str: &str) -> Option<String> {
        if let Ok(value) = serde_json::from_str::<Value>(json_str) {
            value
                .get("method")
                .and_then(|m| m.as_str())
                .map(|s| s.to_string())
        } else {
            None
        }
    }
}

/// Initialize the tracing subscriber for structured logging
pub fn init_tracing(log_level: u64) -> anyhow::Result<()> {
    use crate::config::log_file;
    use tracing_appender::rolling::{RollingFileAppender, Rotation};
    use tracing_subscriber::EnvFilter;

    let filter_directive = match log_level {
        0 => "warn", // Only errors and warnings
        1 => "emacs_lsp_proxy=info,emacs_lsp_proxy::lsp=info,emacs_lsp_proxy::transport=info", // Only LSP communication (info level)
        2 => "emacs_lsp_proxy=debug", // Service debug logs + LSP logs
        _ => "emacs_lsp_proxy=trace", // Everything (most verbose)
    };

    // Create filter for tracing events with custom logic per level
    let env_filter =
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new(filter_directive));

    // Get the log file path and prepare directory
    let log_file_path = log_file();
    let log_dir = log_file_path
        .parent()
        .ok_or_else(|| anyhow::anyhow!("Invalid log file path: {:?}", log_file_path))?;
    let log_filename = log_file_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("lsp-proxy");

    // Create rolling file appender (no rotation for now, just single file)
    let file_appender = RollingFileAppender::builder()
        .rotation(Rotation::NEVER)
        .filename_prefix(log_filename)
        .filename_suffix("log")
        .build(log_dir)
        .map_err(|e| anyhow::anyhow!("Failed to create file appender: {}", e))?;

    // Create non-blocking writer for better performance
    let (file_writer, _guard) = tracing_appender::non_blocking(file_appender);

    // Simple fmt subscriber with file output (no ANSI colors for file)
    tracing_subscriber::fmt()
        .with_target(true)
        .with_thread_ids(false)
        .with_thread_names(false)
        .with_file(false)
        .with_line_number(false)
        .with_ansi(false) // Disable ANSI colors for file output
        .with_timer(time::LocalTime::rfc_3339())
        .compact()
        .with_writer(file_writer)
        .with_env_filter(env_filter)
        .try_init()
        .map_err(|e| anyhow::anyhow!("Failed to initialize tracing subscriber: {}", e))?;

    // Store the guard to prevent the non_blocking writer from being dropped
    std::mem::forget(_guard);

    Ok(())
}

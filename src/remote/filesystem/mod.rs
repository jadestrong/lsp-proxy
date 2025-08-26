//! Remote filesystem with local caching
//! 
//! Provides transparent access to remote files with intelligent caching
//! and synchronization capabilities.

use async_trait::async_trait;
use anyhow::Result;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::sync::RwLock;
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::remote::connection::Connection;

/// Remote file system interface
#[async_trait]
pub trait RemoteFileSystem: Send + Sync {
    /// Read a file from the remote system (with caching)
    async fn read_file(&self, path: &Path) -> Result<Vec<u8>>;
    
    /// Write a file to the remote system
    async fn write_file(&self, path: &Path, content: &[u8]) -> Result<()>;
    
    /// Check if a file exists
    async fn exists(&self, path: &Path) -> Result<bool>;
    
    /// List directory contents
    async fn list_dir(&self, path: &Path) -> Result<Vec<PathBuf>>;
    
    /// Get file metadata
    async fn metadata(&self, path: &Path) -> Result<FileMetadata>;
    
    /// Sync changes with remote
    async fn sync(&self) -> Result<()>;
    
    /// Invalidate cache for a path
    async fn invalidate(&self, path: &Path) -> Result<()>;
}

/// File metadata
#[derive(Debug, Clone)]
pub struct FileMetadata {
    pub size: u64,
    pub modified: SystemTime,
    pub is_file: bool,
    pub is_dir: bool,
}

/// Cached file entry
#[derive(Debug, Clone)]
struct CacheEntry {
    content: Vec<u8>,
    metadata: FileMetadata,
    local_modified: SystemTime,
    dirty: bool,
}

/// Hybrid file system with local caching
pub struct HybridFileSystem {
    connection: Arc<dyn Connection>,
    cache: Arc<RwLock<HashMap<PathBuf, CacheEntry>>>,
    cache_dir: PathBuf,
    max_cache_size: usize,
}

impl HybridFileSystem {
    /// Create a new hybrid file system
    pub fn new(connection: Arc<dyn Connection>, cache_dir: PathBuf) -> Self {
        Self {
            connection,
            cache: Arc::new(RwLock::new(HashMap::new())),
            cache_dir,
            max_cache_size: 100 * 1024 * 1024, // 100MB default
        }
    }
    
    /// Get cache file path
    fn cache_path(&self, path: &Path) -> PathBuf {
        let hash = format!("{:x}", md5::compute(path.to_string_lossy().as_bytes()));
        self.cache_dir.join(format!("{}.cache", hash))
    }
    
    /// Load cache entry from disk
    async fn load_cache_entry(&self, path: &Path) -> Result<Option<CacheEntry>> {
        let cache_path = self.cache_path(path);
        if !cache_path.exists() {
            return Ok(None);
        }
        
        let content = tokio::fs::read(&cache_path).await?;
        
        // Simple format: metadata_size (8 bytes) + metadata (JSON) + content
        if content.len() < 8 {
            return Ok(None);
        }
        
        let metadata_size = u64::from_le_bytes(content[0..8].try_into().unwrap()) as usize;
        if content.len() < 8 + metadata_size {
            return Ok(None);
        }
        
        let metadata_json = &content[8..8 + metadata_size];
        let file_content = &content[8 + metadata_size..];
        
        let cached_meta: serde_json::Value = serde_json::from_slice(metadata_json)?;
        
        // Parse cached metadata
        let metadata = FileMetadata {
            size: cached_meta["size"].as_u64().unwrap_or(0),
            modified: UNIX_EPOCH + std::time::Duration::from_secs(
                cached_meta["modified"].as_u64().unwrap_or(0)
            ),
            is_file: cached_meta["is_file"].as_bool().unwrap_or(true),
            is_dir: cached_meta["is_dir"].as_bool().unwrap_or(false),
        };
        
        Ok(Some(CacheEntry {
            content: file_content.to_vec(),
            metadata,
            local_modified: SystemTime::now(),
            dirty: false,
        }))
    }
    
    /// Save cache entry to disk
    async fn save_cache_entry(&self, path: &Path, entry: &CacheEntry) -> Result<()> {
        let cache_path = self.cache_path(path);
        
        // Ensure cache directory exists
        if let Some(parent) = cache_path.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }
        
        // Serialize metadata
        let metadata_json = serde_json::json!({
            "size": entry.metadata.size,
            "modified": entry.metadata.modified.duration_since(UNIX_EPOCH)?.as_secs(),
            "is_file": entry.metadata.is_file,
            "is_dir": entry.metadata.is_dir,
        });
        
        let metadata_bytes = serde_json::to_vec(&metadata_json)?;
        let metadata_size = metadata_bytes.len() as u64;
        
        // Write cache file
        let mut content = Vec::new();
        content.extend_from_slice(&metadata_size.to_le_bytes());
        content.extend_from_slice(&metadata_bytes);
        content.extend_from_slice(&entry.content);
        
        tokio::fs::write(&cache_path, content).await?;
        Ok(())
    }
}

#[async_trait]
impl RemoteFileSystem for HybridFileSystem {
    async fn read_file(&self, path: &Path) -> Result<Vec<u8>> {
        // Check memory cache first
        {
            let cache = self.cache.read().await;
            if let Some(entry) = cache.get(path) {
                if !entry.dirty && entry.local_modified.elapsed()?.as_secs() < 300 { // 5 min cache
                    return Ok(entry.content.clone());
                }
            }
        }
        
        // Check disk cache
        if let Some(entry) = self.load_cache_entry(path).await? {
            if entry.local_modified.elapsed()?.as_secs() < 3600 { // 1 hour disk cache
                // Update memory cache
                {
                    let mut cache = self.cache.write().await;
                    cache.insert(path.to_path_buf(), entry.clone());
                }
                return Ok(entry.content);
            }
        }
        
        // Fetch from remote
        let file_transfer = self.connection.file_transfer().await?;
        let content = file_transfer.read_file(&path.to_string_lossy()).await?;
        
        // Get metadata
        let metadata = self.metadata(path).await.unwrap_or(FileMetadata {
            size: content.len() as u64,
            modified: SystemTime::now(),
            is_file: true,
            is_dir: false,
        });
        
        let entry = CacheEntry {
            content: content.clone(),
            metadata,
            local_modified: SystemTime::now(),
            dirty: false,
        };
        
        // Cache the entry
        {
            let mut cache = self.cache.write().await;
            cache.insert(path.to_path_buf(), entry.clone());
        }
        
        // Save to disk cache
        let _ = self.save_cache_entry(path, &entry).await;
        
        Ok(content)
    }
    
    async fn write_file(&self, path: &Path, content: &[u8]) -> Result<()> {
        // Write to remote
        let file_transfer = self.connection.file_transfer().await?;
        file_transfer.write_file(&path.to_string_lossy(), content).await?;
        
        // Update cache
        let metadata = FileMetadata {
            size: content.len() as u64,
            modified: SystemTime::now(),
            is_file: true,
            is_dir: false,
        };
        
        let entry = CacheEntry {
            content: content.to_vec(),
            metadata,
            local_modified: SystemTime::now(),
            dirty: false,
        };
        
        // Update memory cache
        {
            let mut cache = self.cache.write().await;
            cache.insert(path.to_path_buf(), entry.clone());
        }
        
        // Update disk cache
        let _ = self.save_cache_entry(path, &entry).await;
        
        Ok(())
    }
    
    async fn exists(&self, path: &Path) -> Result<bool> {
        let file_transfer = self.connection.file_transfer().await?;
        file_transfer.exists(&path.to_string_lossy()).await
    }
    
    async fn list_dir(&self, path: &Path) -> Result<Vec<PathBuf>> {
        let file_transfer = self.connection.file_transfer().await?;
        let entries = file_transfer.list_dir(&path.to_string_lossy()).await?;
        Ok(entries.into_iter().map(PathBuf::from).collect())
    }
    
    async fn metadata(&self, path: &Path) -> Result<FileMetadata> {
        // Use stat command to get file metadata
        let result = self.connection.execute_command(
            "stat",
            &["-c", "%s %Y %F", &path.to_string_lossy()]
        ).await?;
        
        if result.exit_code != 0 {
            return Err(anyhow::anyhow!("Failed to get file metadata: {}", result.stderr));
        }
        
        let parts: Vec<&str> = result.stdout.trim().split_whitespace().collect();
        if parts.len() < 3 {
            return Err(anyhow::anyhow!("Invalid stat output"));
        }
        
        let size = parts[0].parse::<u64>()?;
        let modified_ts = parts[1].parse::<u64>()?;
        let file_type = parts[2];
        
        Ok(FileMetadata {
            size,
            modified: UNIX_EPOCH + std::time::Duration::from_secs(modified_ts),
            is_file: file_type == "regular file",
            is_dir: file_type == "directory",
        })
    }
    
    async fn sync(&self) -> Result<()> {
        // Sync all dirty entries
        let cache = self.cache.read().await;
        let dirty_entries: Vec<_> = cache.iter()
            .filter(|(_, entry)| entry.dirty)
            .map(|(path, entry)| (path.clone(), entry.clone()))
            .collect();
        drop(cache);
        
        for (path, entry) in dirty_entries {
            let file_transfer = self.connection.file_transfer().await?;
            file_transfer.write_file(&path.to_string_lossy(), &entry.content).await?;
            
            // Mark as clean
            let mut cache = self.cache.write().await;
            if let Some(cached_entry) = cache.get_mut(&path) {
                cached_entry.dirty = false;
            }
        }
        
        Ok(())
    }
    
    async fn invalidate(&self, path: &Path) -> Result<()> {
        // Remove from memory cache
        {
            let mut cache = self.cache.write().await;
            cache.remove(path);
        }
        
        // Remove from disk cache
        let cache_path = self.cache_path(path);
        if cache_path.exists() {
            let _ = tokio::fs::remove_file(cache_path).await;
        }
        
        Ok(())
    }
}

/// Create a remote file system
pub async fn create_filesystem(connection: Arc<dyn Connection>) -> Result<Arc<dyn RemoteFileSystem>> {
    let cache_dir = dirs::cache_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join("lsp-proxy")
        .join("remote-cache");
    
    Ok(Arc::new(HybridFileSystem::new(connection, cache_dir)))
}
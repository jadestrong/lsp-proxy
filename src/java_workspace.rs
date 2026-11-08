//! JDTLS workspace-cache discovery and reversible cleanup.
use anyhow::{bail, Context, Result};
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use sha1::{Digest, Sha1};
use std::{
    collections::{HashMap, HashSet},
    fs::{self, File, OpenOptions},
    path::{Path, PathBuf},
};

static CLEANING: Lazy<Mutex<HashSet<PathBuf>>> = Lazy::new(|| Mutex::new(HashSet::new()));

pub(crate) fn data_path(
    command: &Path,
    name: &str,
    args: &[String],
    root: &Path,
    environment: &HashMap<String, String>,
) -> Option<PathBuf> {
    let launcher = matches!(
        command.file_name()?.to_str()?,
        "jdtls" | "jdtls.bat" | "jdtls.cmd"
    );
    if name != "jdtls"
        && !launcher
        && !args
            .iter()
            .any(|arg| arg.contains("org.eclipse.jdt.ls.core"))
    {
        return None;
    }
    let explicit = args
        .iter()
        .enumerate()
        .filter_map(|(i, arg)| {
            if arg == "-data" {
                args.get(i + 1)
                    .filter(|path| !path.starts_with('-'))
                    .cloned()
            } else {
                arg.strip_prefix("-data=").map(str::to_owned)
            }
        })
        .last();
    if let Some(path) = explicit {
        if path.is_empty() {
            return None;
        }
        let path = PathBuf::from(path);
        return Some(if path.is_absolute() {
            path
        } else {
            root.join(path)
        });
    }
    // Custom wrappers may choose their own workspace: never guess their -data.
    if !launcher
        || args
            .iter()
            .any(|arg| arg == "-data" || arg.starts_with("-data="))
    {
        return None;
    }
    let variable = if cfg!(windows) { "APPDATA" } else { "HOME" };
    let home = environment
        .get(variable)
        .map(PathBuf::from)
        .or_else(|| std::env::var_os(variable).map(PathBuf::from));
    let base = match home {
        Some(home) if cfg!(target_os = "macos") => home.join("Library/Caches"),
        Some(home) if cfg!(windows) => home,
        Some(home) => home.join(".cache"),
        None => std::env::temp_dir(),
    };
    // bin/jdtls.py hashes the cwd basename, not the entire project path.
    let cwd = fs::canonicalize(root).unwrap_or_else(|_| root.to_owned());
    let basename = cwd.file_name()?.to_str()?;
    Some(
        base.join("jdtls")
            .join(format!("jdtls-{:x}", Sha1::digest(basename.as_bytes()))),
    )
}

pub(crate) fn normalized(path: &Path) -> PathBuf {
    let path = crate::utils::path::canonicalize(path);
    let mut ancestor = path.as_path();
    let mut missing = Vec::new();
    loop {
        if let Ok(mut resolved) = fs::canonicalize(ancestor) {
            for component in missing.iter().rev() {
                resolved.push(component);
            }
            return resolved;
        }
        let Some(name) = ancestor.file_name() else {
            return path;
        };
        missing.push(name.to_owned());
        let Some(parent) = ancestor.parent() else {
            return path;
        };
        ancestor = parent;
    }
}

pub(crate) fn is_cleaning(path: &Path) -> bool {
    CLEANING.lock().contains(&normalized(path))
}

pub(crate) struct Reservation(PathBuf);

impl Reservation {
    pub(crate) fn acquire(path: &Path) -> Result<Self> {
        let path = normalized(path);
        if !CLEANING.lock().insert(path.clone()) {
            bail!(
                "Java workspace cache is already being cleaned: {}",
                path.display()
            );
        }
        Ok(Self(path))
    }
}

impl Drop for Reservation {
    fn drop(&mut self) {
        CLEANING.lock().remove(&self.0);
    }
}

pub(crate) fn validate_cache(cache: &Path, workspace: &Path) -> Result<()> {
    let cache_path = normalized(cache);
    let workspace = normalized(workspace);
    if workspace.starts_with(&cache_path) || cache_path.parent().is_none() {
        bail!(
            "Refusing to clean a project directory or its ancestor: {}",
            cache.display()
        );
    }
    if let Some(home) = std::env::var_os(if cfg!(windows) { "USERPROFILE" } else { "HOME" }) {
        if normalized(Path::new(&home)).starts_with(&cache_path) {
            bail!(
                "Refusing to clean a home directory or its ancestor: {}",
                cache.display()
            );
        }
    }
    let metadata = match fs::symlink_metadata(cache) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(error) => return Err(error.into()),
    };
    if metadata.file_type().is_symlink() || !metadata.is_dir() {
        bail!(
            "JDTLS cache must be a directory, not a file or symlink: {}",
            cache.display()
        );
    }
    let metadata_dir = cache.join(".metadata");
    if fs::symlink_metadata(&metadata_dir)?
        .file_type()
        .is_symlink()
        || !(metadata_dir.join(".lock").exists()
            || metadata_dir.join(".plugins/org.eclipse.jdt.core").is_dir())
    {
        bail!(
            "Directory does not contain a JDTLS workspace: {}",
            cache.display()
        );
    }
    Ok(())
}

// Eclipse uses a Java FileLock on .metadata/.lock. POSIX flock is independent
// of that lock on Linux, so use a POSIX record lock rather than flock.
fn lock_workspace(cache: &Path) -> Result<Option<File>> {
    let path = cache.join(".metadata/.lock");
    if !path.exists() {
        return Ok(None);
    }
    if fs::symlink_metadata(&path)?.file_type().is_symlink() {
        bail!("Refusing a symlinked workspace lock: {}", path.display());
    }
    let mut options = OpenOptions::new();
    options.read(true).write(true);
    #[cfg(windows)]
    {
        use std::os::windows::fs::OpenOptionsExt;
        // Deny other readers/writers, but allow our directory rename.
        options.share_mode(0x4);
    }
    let file = options
        .open(&path)
        .context("Java workspace is still in use or its lock cannot be opened")?;
    #[cfg(unix)]
    {
        use std::os::fd::AsRawFd;
        let mut lock: libc::flock = unsafe { std::mem::zeroed() };
        lock.l_type = libc::F_WRLCK as _;
        lock.l_whence = libc::SEEK_SET as _;
        if unsafe { libc::fcntl(file.as_raw_fd(), libc::F_SETLK, &lock) } == -1 {
            return Err(std::io::Error::last_os_error())
                .context("Java workspace is still in use; cache was not moved");
        }
    }
    Ok(Some(file))
}

pub(crate) fn backup_cache(cache: &Path, workspace: &Path) -> Result<Option<PathBuf>> {
    validate_cache(cache, workspace)?;
    if !cache.exists() {
        return Ok(None);
    }
    let _lock = lock_workspace(cache)?;
    let backup = cache.with_file_name(format!(
        "{}.lsp-proxy-backup-{}",
        cache
            .file_name()
            .context("Cache directory has no name")?
            .to_string_lossy(),
        uuid::Uuid::new_v4()
    ));
    fs::rename(cache, &backup)
        .with_context(|| format!("Could not back up JDTLS cache {}", cache.display()))?;
    Ok(Some(backup))
}

pub(crate) struct CleanupContext {
    pub request_id: crate::msg::RequestId,
    pub cache: PathBuf,
    pub closed_documents: Vec<String>,
    pub _reservation: Reservation,
}

pub(crate) type CleanupCompleted = (PathBuf, Result<Option<PathBuf>, String>);

#[cfg(test)]
mod tests {
    use super::*;

    fn cache(root: &Path) -> PathBuf {
        let cache = root.join("jdt-data");
        fs::create_dir_all(cache.join(".metadata/.plugins/org.eclipse.jdt.core")).unwrap();
        fs::write(cache.join(".metadata/.lock"), "").unwrap();
        fs::write(
            cache.join(".metadata/.plugins/org.eclipse.jdt.core/broken.index"),
            [0xff, 0xfe],
        )
        .unwrap();
        cache
    }

    #[test]
    fn test_data_path_matches_launcher_and_explicit_arguments() {
        let root = Path::new("/project/funding");
        let mut env = HashMap::new();
        env.insert(
            if cfg!(windows) { "APPDATA" } else { "HOME" }.into(),
            "/home/test".into(),
        );
        let default = data_path(Path::new("jdtls"), "jdtls", &[], root, &env).unwrap();
        assert_eq!(
            default.file_name().unwrap(),
            format!("jdtls-{:x}", Sha1::digest(b"funding")).as_str()
        );
        assert_eq!(
            data_path(
                Path::new("wrapper"),
                "jdtls",
                &["-data".into(), ".jdt".into()],
                root,
                &env
            ),
            Some(root.join(".jdt"))
        );
        assert_eq!(
            data_path(
                Path::new("java"),
                "jdtls",
                &["-data=cache".into()],
                root,
                &env
            ),
            Some(root.join("cache"))
        );
        assert!(data_path(Path::new("wrapper"), "jdtls", &[], root, &env).is_none());
        assert!(data_path(Path::new("jdtls"), "jdtls", &["-data".into()], root, &env).is_none());
        assert!(data_path(
            Path::new("other"),
            "other",
            &["-data=cache".into()],
            root,
            &env
        )
        .is_none());
    }

    #[test]
    fn test_cache_backup_preserves_content_and_does_not_touch_project() {
        let temp = tempfile::tempdir().unwrap();
        let workspace = temp.path().join("project");
        fs::create_dir(&workspace).unwrap();
        fs::write(workspace.join("App.java"), "unsaved content is elsewhere").unwrap();
        let cache = cache(temp.path());
        let backup = backup_cache(&cache, &workspace).unwrap().unwrap();
        assert!(!cache.exists());
        assert_eq!(
            fs::read(backup.join(".metadata/.plugins/org.eclipse.jdt.core/broken.index")).unwrap(),
            [0xff, 0xfe]
        );
        assert!(workspace.join("App.java").exists());
        assert!(backup_cache(&cache, &workspace).unwrap().is_none());
    }

    #[test]
    fn test_unsafe_directories_are_refused() {
        let temp = tempfile::tempdir().unwrap();
        let workspace = temp.path().join("project");
        fs::create_dir(&workspace).unwrap();
        assert!(validate_cache(&workspace, &workspace).is_err());
        assert!(validate_cache(temp.path(), &workspace).is_err());
        let unrelated = temp.path().join("unrelated");
        fs::create_dir(&unrelated).unwrap();
        fs::write(unrelated.join("keep"), "keep").unwrap();
        assert!(backup_cache(&unrelated, &workspace).is_err());
        assert!(unrelated.join("keep").exists());
        #[cfg(unix)]
        {
            let cache = cache(temp.path());
            let alias = temp.path().join("alias");
            std::os::unix::fs::symlink(&cache, &alias).unwrap();
            assert!(backup_cache(&alias, &workspace).is_err());
            assert!(cache.exists());
        }
    }

    #[test]
    fn test_reservation_prevents_restart_until_released_even_after_rename() {
        let temp = tempfile::tempdir().unwrap();
        let cache = cache(temp.path());
        let reservation = Reservation::acquire(&cache).unwrap();
        assert!(Reservation::acquire(&cache).is_err());
        fs::rename(&cache, temp.path().join("backup")).unwrap();
        assert!(is_cleaning(&cache));
        drop(reservation);
        assert!(!is_cleaning(&cache));
    }

    #[cfg(unix)]
    #[test]
    fn test_java_style_record_lock_prevents_cleaning_live_workspace() {
        use std::io::{BufRead, BufReader};
        use std::process::{Command, Stdio};
        let temp = tempfile::tempdir().unwrap();
        let cache = cache(temp.path());
        let mut peer = Command::new("python3").args(["-u", "-c",
            "import fcntl,sys; f=open(sys.argv[1],'r+'); fcntl.lockf(f,fcntl.LOCK_EX); print('ready'); sys.stdin.read()"])
            .arg(cache.join(".metadata/.lock")).stdin(Stdio::piped()).stdout(Stdio::piped()).spawn().unwrap();
        let mut ready = String::new();
        BufReader::new(peer.stdout.take().unwrap())
            .read_line(&mut ready)
            .unwrap();
        let result = backup_cache(&cache, &temp.path().join("project"));
        peer.kill().unwrap();
        peer.wait().unwrap();
        assert_eq!(ready.trim(), "ready");
        assert!(result.unwrap_err().to_string().contains("still in use"));
        assert!(cache.exists());
    }
}

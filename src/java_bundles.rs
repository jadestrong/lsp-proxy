//! Avoid replacing JDTLS framework dependencies with identical contributed jars.
use anyhow::{bail, Context, Result};
use serde_json::Value;
use std::{
    collections::HashSet,
    fs::{self, File},
    io::Read,
    path::{Path, PathBuf},
};

const MAX_MANIFEST_SIZE: u64 = 128 * 1024;

#[derive(Clone, Debug)]
pub(crate) struct BundleSource {
    configuration: PathBuf,
    plugins: PathBuf,
}

fn local_path(value: &str, base: &Path) -> Option<PathBuf> {
    let path = if value.starts_with("file:") {
        lsp_types::Url::parse(value).ok()?.to_file_path().ok()?
    } else {
        PathBuf::from(value)
    };
    Some(if path.is_absolute() {
        path
    } else {
        base.join(path)
    })
}

pub(crate) fn discover(command: &Path, args: &[String], workspace: &Path) -> Option<BundleSource> {
    let args: Vec<_> = args
        .iter()
        .map(|arg| arg.strip_prefix("--jvm-arg=").unwrap_or(arg))
        .collect();
    let launcher = args.windows(2).rev().find_map(|pair| {
        (pair[0] == "-jar")
            .then(|| local_path(pair[1], workspace))
            .flatten()
            .filter(|path| {
                path.file_name().is_some_and(|name| {
                    name.to_string_lossy()
                        .starts_with("org.eclipse.equinox.launcher")
                })
            })
    });
    let plugins = if let Some(launcher) = launcher {
        launcher.parent()?.to_owned()
    } else {
        let command = fs::canonicalize(command).ok()?;
        if !matches!(
            command.file_name()?.to_str()?,
            "jdtls" | "jdtls.py" | "jdtls.bat" | "jdtls.cmd"
        ) {
            return None;
        }
        command.parent()?.parent()?.join("plugins")
    };
    if !plugins.is_dir() {
        return None;
    }
    let install = plugins.parent()?;
    let explicit = args
        .windows(2)
        .rev()
        .find_map(|pair| (pair[0] == "-configuration").then_some(pair[1]))
        .or_else(|| {
            args.iter()
                .rev()
                .find_map(|arg| arg.strip_prefix("-Dosgi.configuration.area="))
        })
        .or_else(|| {
            args.iter()
                .rev()
                .find_map(|arg| arg.strip_prefix("-Dosgi.sharedConfiguration.area="))
        });
    let configuration = if let Some(explicit) = explicit {
        local_path(explicit, workspace)?
    } else {
        install.join(if cfg!(target_os = "macos") {
            "config_mac"
        } else if cfg!(windows) {
            "config_win"
        } else {
            "config_linux"
        })
    };
    Some(BundleSource {
        configuration,
        plugins,
    })
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct BundleIdentity {
    name: String,
    version: (u32, u32, u32, String),
}

fn identity(manifest: &str) -> Option<BundleIdentity> {
    // Only the main section describes the bundle. Manifest continuation lines
    // remove exactly one leading space and may split a symbolic name or version.
    let mut headers: Vec<String> = Vec::new();
    for line in manifest.lines() {
        if line.is_empty() {
            break;
        }
        if let Some(continuation) = line.strip_prefix(' ') {
            headers.last_mut()?.push_str(continuation);
        } else {
            headers.push(line.to_owned());
        }
    }
    let header = |key: &str| {
        headers.iter().find_map(|line| {
            let (name, value) = line.split_once(':')?;
            name.eq_ignore_ascii_case(key).then(|| value.trim())
        })
    };
    let name = header("Bundle-SymbolicName")?.split(';').next()?.trim();
    if name.split('.').any(|part| {
        part.is_empty()
            || !part
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || b"_-".contains(&byte))
    }) {
        return None;
    }
    let parts: Vec<_> = header("Bundle-Version")?.split('.').collect();
    if parts.is_empty() || parts.len() > 4 {
        return None;
    }
    let mut numbers = [0; 3];
    for (index, part) in parts.iter().take(3).enumerate() {
        if part.is_empty() || !part.bytes().all(|byte| byte.is_ascii_digit()) {
            return None;
        }
        numbers[index] = part.parse::<u32>().ok().filter(|n| *n <= i32::MAX as u32)?;
    }
    let qualifier = parts.get(3).copied().unwrap_or("");
    if (parts.len() == 4 && qualifier.is_empty())
        || !qualifier
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || b"_-".contains(&byte))
    {
        return None;
    }
    Some(BundleIdentity {
        name: name.to_owned(),
        version: (numbers[0], numbers[1], numbers[2], qualifier.to_owned()),
    })
}

fn read_identity(path: &Path) -> Result<BundleIdentity> {
    let mut manifest = String::new();
    if path.is_dir() {
        File::open(path.join("META-INF/MANIFEST.MF"))?
            .take(MAX_MANIFEST_SIZE + 1)
            .read_to_string(&mut manifest)?;
    } else {
        let mut jar = zip::ZipArchive::new(File::open(path)?)?;
        jar.by_name("META-INF/MANIFEST.MF")?
            .take(MAX_MANIFEST_SIZE + 1)
            .read_to_string(&mut manifest)?;
    }
    if manifest.len() as u64 > MAX_MANIFEST_SIZE {
        bail!("Bundle manifest exceeds {MAX_MANIFEST_SIZE} bytes");
    }
    identity(&manifest).context("Missing or invalid OSGi bundle identity")
}

fn property(contents: &str, key: &str) -> Option<String> {
    let mut logical = String::new();
    let mut result = None;
    for line in contents.lines() {
        let line = line.trim();
        if logical.is_empty() && (line.starts_with('#') || line.starts_with('!')) {
            continue;
        }
        logical.push_str(line);
        if logical.chars().rev().take_while(|ch| *ch == '\\').count() % 2 == 1 {
            logical.pop();
            continue;
        }
        if let Some((name, value)) = logical.split_once('=') {
            if name.trim() == key {
                // Eclipse config.ini escapes URI colons as Java properties.
                result = Some(value.trim().replace("\\:", ":").replace("\\=", "="));
            }
        }
        logical.clear();
    }
    result
}

impl BundleSource {
    fn registered_path(&self, location: &str) -> Option<PathBuf> {
        let location = location.strip_prefix("reference:").unwrap_or(location);
        if location.starts_with("file:/") {
            return local_path(location, &self.plugins);
        }
        let location = location.strip_prefix("file:").unwrap_or(location);
        let path = Path::new(location);
        if path.is_absolute() {
            Some(path.to_owned())
        } else if location.starts_with("plugins/") {
            Some(self.plugins.parent()?.join(path))
        } else if location.starts_with("../") {
            Some(self.configuration.join(path))
        } else {
            Some(self.plugins.join(path))
        }
    }

    fn identities(&self) -> Result<HashSet<BundleIdentity>> {
        let config = fs::read_to_string(self.configuration.join("config.ini"))?;
        let mut paths = Vec::new();
        if let Some(bundles) = property(&config, "osgi.bundles") {
            paths.extend(
                bundles
                    .split(',')
                    .filter_map(|entry| self.registered_path(entry.split('@').next()?.trim())),
            );
        }
        // Distributions using the Equinox Simple Configurator register their
        // remaining framework bundles in bundles.info rather than config.ini.
        let info = self
            .configuration
            .join("org.eclipse.equinox.simpleconfigurator/bundles.info");
        if paths.iter().any(|path| {
            path.file_name().is_some_and(|name| {
                name.to_string_lossy()
                    .starts_with("org.eclipse.equinox.simpleconfigurator")
            })
        }) && info.is_file()
        {
            for line in fs::read_to_string(info)?
                .lines()
                .filter(|line| !line.starts_with('#'))
            {
                if let Some(path) = line
                    .split(',')
                    .nth(2)
                    .and_then(|location| self.registered_path(location))
                {
                    paths.push(path);
                }
            }
        }
        // A jar merely present in plugins/ may not be registered. Never use
        // directory enumeration here: it could discard a required extension.
        Ok(paths
            .iter()
            .filter_map(|path| read_identity(path).ok())
            .collect())
    }
}

fn filter(mut config: Value, source: Option<BundleSource>, workspace: &Path) -> Value {
    let Some(bundles) = config.get_mut("bundles").and_then(Value::as_array_mut) else {
        return config;
    };
    let mut seen = match source {
        Some(source) => source.identities().unwrap_or_else(|error| {
            log::warn!("Cannot inspect registered JDTLS bundles; retaining contributed dependencies: {error}");
            HashSet::new()
        }),
        None => HashSet::new(),
    };
    let mut paths = HashSet::new();
    bundles.retain(|bundle| {
        let Some(path) = bundle.as_str() else {
            return true;
        };
        if !paths.insert(path.to_owned()) {
            return false;
        }
        let Some(local) = local_path(path, workspace) else {
            return true;
        };
        let Ok(id) = read_identity(&local) else {
            // Let JDTLS report malformed or unknown bundles using its existing
            // diagnostics; don't silently drop a potentially required plugin.
            return true;
        };
        if seen.contains(&id) {
            log::debug!(
                "Reusing registered/contributed JDTLS bundle {} {:?}; skipping {path}",
                id.name,
                id.version
            );
            false
        } else {
            seen.insert(id);
            true
        }
    });
    config
}

pub(crate) async fn prepare(
    name: &str,
    config: Option<Value>,
    source: Option<BundleSource>,
    workspace: PathBuf,
) -> Option<Value> {
    if name != "jdtls"
        || !config.as_ref().is_some_and(|config| {
            config
                .get("bundles")
                .and_then(Value::as_array)
                .is_some_and(|bundles| !bundles.is_empty())
        })
    {
        return config;
    }
    let fallback = config.clone();
    match tokio::task::spawn_blocking(move || filter(config.unwrap(), source, &workspace)).await {
        Ok(config) => Some(config),
        Err(error) => {
            log::warn!("Cannot prepare JDTLS bundles; retaining initialization options: {error}");
            fallback
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use std::io::Write;

    fn jar(path: &Path, name: &str, version: &str) {
        let mut archive = zip::ZipWriter::new(File::create(path).unwrap());
        archive
            .start_file(
                "META-INF/MANIFEST.MF",
                zip::write::FileOptions::default()
                    .compression_method(zip::CompressionMethod::Deflated),
            )
            .unwrap();
        write!(archive, "Manifest-Version: 1.0\r\nBundle-SymbolicName: {name}\r\nBundle-Version: {version}\r\n\r\n").unwrap();
        archive.finish().unwrap();
    }

    fn source(root: &Path) -> BundleSource {
        let source = BundleSource {
            plugins: root.join("plugins"),
            configuration: root.join("config"),
        };
        fs::create_dir_all(&source.plugins).unwrap();
        fs::create_dir_all(&source.configuration).unwrap();
        fs::write(
            source.configuration.join("config.ini"),
            "osgi.bundles=reference\\:file\\:builtin.jar@4\n",
        )
        .unwrap();
        jar(
            &source.plugins.join("builtin.jar"),
            "org.objectweb.asm",
            "9.10.1",
        );
        source
    }

    #[test]
    fn test_identity_unfolds_main_headers_and_uses_osgi_version_semantics() {
        let folded = "Manifest-Version: 1.0\r\nBundle-SymbolicName: org.objectweb.\r\n asm;singleton:=true\r\nBundle-Version: 9.10\r\n .1\r\n\r\nName: ignored\r\nBundle-Version: 1.0.0\r\n";
        assert_eq!(
            identity(folded),
            identity("Bundle-SymbolicName: org.objectweb.asm\nBundle-Version: 9.10.1\n")
        );
        assert_eq!(
            identity("Bundle-SymbolicName: x\nBundle-Version: 1\n"),
            identity("Bundle-SymbolicName: x\nBundle-Version: 1.0.0\n")
        );
        assert_ne!(
            identity("Bundle-SymbolicName: x\nBundle-Version: 1.0.0.alpha\n"),
            identity("Bundle-SymbolicName: x\nBundle-Version: 1.0.0.beta\n")
        );
        for manifest in [
            "Bundle-SymbolicName: x\n",
            "Bundle-SymbolicName: x\nBundle-Version: bad\n",
            "Bundle-SymbolicName: x\nBundle-Version: 1.0.0.bad.qualifier\n",
        ] {
            assert!(identity(manifest).is_none());
        }
    }

    #[test]
    fn test_reuses_registered_identity_but_keeps_other_versions_and_unregistered_jars() {
        let temp = tempfile::tempdir().unwrap();
        let source = source(temp.path());
        jar(&source.plugins.join("unused.jar"), "test.only", "1.0.0");
        let same = temp.path().join("renamed.jar");
        let different = temp.path().join("newer.jar");
        let test = temp.path().join("test.jar");
        jar(&same, "org.objectweb.asm", "9.10.1");
        jar(&different, "org.objectweb.asm", "9.11.0");
        jar(&test, "test.only", "1.0.0");
        let output = filter(
            json!({"bundles":[same, different, test], "settings":{"java":{"home":"jdk"}}}),
            Some(source),
            temp.path(),
        );
        assert_eq!(output["bundles"], json!([different, test]));
        assert_eq!(output["settings"]["java"]["home"], "jdk");
    }

    #[test]
    fn test_contributions_deduplicate_identity_and_preserve_unknown_entries() {
        let temp = tempfile::tempdir().unwrap();
        let first = temp.path().join("first.jar");
        let second = temp.path().join("second.jar");
        let broken = temp.path().join("broken.jar");
        jar(&first, "plugin", "1.0");
        jar(&second, "plugin;singleton:=true", "1.0.0");
        fs::write(&broken, "not a jar").unwrap();
        let output = filter(
            json!({"bundles":[first, second, broken, "missing.jar", 123]}),
            None,
            temp.path(),
        );
        assert_eq!(
            output["bundles"],
            json!([first, broken, "missing.jar", 123])
        );
    }

    #[test]
    fn test_configuration_supports_continuations_and_only_registered_bundles() {
        assert_eq!(
            property(
                "osgi.bundles=reference\\:file\\:a.jar@4,\\\n  reference\\:file\\:b.jar@4\n",
                "osgi.bundles"
            ),
            Some("reference:file:a.jar@4,reference:file:b.jar@4".into())
        );
        let temp = tempfile::tempdir().unwrap();
        let source = source(temp.path());
        let launcher = source.plugins.join("org.eclipse.equinox.launcher.jar");
        let args = vec![
            "-jar".into(),
            launcher.to_string_lossy().into_owned(),
            "-configuration".into(),
            source.configuration.to_string_lossy().into_owned(),
        ];
        let discovered = discover(Path::new("java"), &args, temp.path()).unwrap();
        assert_eq!(discovered.configuration, source.configuration);
        assert_eq!(discovered.identities().unwrap().len(), 1);
    }

    #[tokio::test]
    async fn test_other_servers_and_unknown_config_are_untouched() {
        let config = Some(json!({"bundles":["duplicate", "duplicate"]}));
        assert_eq!(
            prepare("other", config.clone(), None, PathBuf::new()).await,
            config
        );
        assert_eq!(prepare("jdtls", None, None, PathBuf::new()).await, None);
        let config = Some(json!({"bundles":"bad"}));
        assert_eq!(
            prepare("jdtls", config.clone(), None, PathBuf::new()).await,
            config
        );
    }

    #[test]
    fn test_standard_launcher_and_explicit_shared_configuration_discovery() {
        let temp = tempfile::tempdir().unwrap();
        let source = source(temp.path());
        let bin = temp.path().join("bin");
        fs::create_dir(&bin).unwrap();
        let command = bin.join("jdtls");
        fs::write(&command, "launcher").unwrap();
        let args = vec![format!(
            "--jvm-arg=-Dosgi.sharedConfiguration.area={}",
            lsp_types::Url::from_directory_path(&source.configuration).unwrap()
        )];
        let discovered = discover(&command, &args, temp.path()).unwrap();
        assert_eq!(discovered.identities().unwrap().len(), 1);
        #[cfg(unix)]
        {
            let alias = temp.path().join("jdtls-alias");
            std::os::unix::fs::symlink(command, &alias).unwrap();
            assert_eq!(
                discover(&alias, &args, temp.path()).unwrap().plugins,
                fs::canonicalize(source.plugins).unwrap()
            );
        }
        assert!(discover(Path::new("unknown-wrapper"), &[], temp.path()).is_none());
    }

    #[test]
    fn test_simple_configurator_registers_only_bundles_info_entries() {
        let temp = tempfile::tempdir().unwrap();
        let source = source(temp.path());
        fs::write(source.configuration.join("config.ini"), "osgi.bundles=reference\\:file\\:org.eclipse.equinox.simpleconfigurator.jar@1\\:start\n").unwrap();
        let info = source
            .configuration
            .join("org.eclipse.equinox.simpleconfigurator");
        fs::create_dir(&info).unwrap();
        fs::write(
            info.join("bundles.info"),
            "#version=1\norg.objectweb.asm,9.10.1,plugins/builtin.jar,4,false\n",
        )
        .unwrap();
        jar(&source.plugins.join("unused.jar"), "unused", "1");
        assert_eq!(
            source.identities().unwrap(),
            HashSet::from([read_identity(&source.plugins.join("builtin.jar")).unwrap()])
        );
    }

    #[test]
    fn test_unreadable_configuration_and_large_manifest_preserve_contributions() {
        let temp = tempfile::tempdir().unwrap();
        let source = source(temp.path());
        let same = temp.path().join("asm.jar");
        jar(&same, "org.objectweb.asm", "9.10.1");
        fs::remove_file(source.configuration.join("config.ini")).unwrap();
        assert_eq!(
            filter(json!({"bundles":[same]}), Some(source), temp.path())["bundles"],
            json!([same])
        );
        let directory = temp.path().join("huge-bundle/META-INF");
        fs::create_dir_all(&directory).unwrap();
        fs::write(
            directory.join("MANIFEST.MF"),
            "x".repeat(MAX_MANIFEST_SIZE as usize + 1),
        )
        .unwrap();
        assert!(read_identity(directory.parent().unwrap())
            .unwrap_err()
            .to_string()
            .contains("exceeds"));
    }
}

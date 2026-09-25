use log::debug;
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use regex::Regex;
use serde_json::{json, Map, Value};
use std::{
    collections::HashMap,
    env, fs,
    path::{Path, PathBuf},
    process::Command,
};

static PROJECT_RUNTIME_OVERRIDES: Lazy<Mutex<HashMap<PathBuf, PathBuf>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

pub(crate) fn set_project_runtime_override(
    root_path: &Path,
    java_home: Option<&str>,
) -> Result<(Option<PathBuf>, Option<String>), String> {
    let root = normalize_dir(root_path);
    let Some(java_home) = java_home else {
        PROJECT_RUNTIME_OVERRIDES.lock().remove(&root);
        return Ok((None, None));
    };

    let home = normalize_dir(Path::new(java_home));
    if !valid_java_home(&home) {
        return Err(format!(
            "Project JDK home is not a full JDK: {}",
            home.display()
        ));
    }
    let Some(version) = java_home_major(&home) else {
        return Err(format!(
            "Could not determine Java version from JDK home: {}",
            home.display()
        ));
    };

    PROJECT_RUNTIME_OVERRIDES.lock().insert(root, home.clone());
    Ok((Some(home), Some(version)))
}

pub(crate) struct ProjectRuntime {
    pub(crate) java_home: PathBuf,
    pub(crate) java_version: String,
    pub(crate) source: &'static str,
}

pub(crate) fn project_runtime_for(
    root_path: &Path,
    doc_path: Option<&Path>,
) -> Option<ProjectRuntime> {
    if let Some(home) = project_runtime_override(root_path) {
        let Some(version) = java_home_major(&home) else {
            debug!(
                "Java project JDK: manual override {} no longer has a readable Java version",
                home.display()
            );
            return None;
        };
        return Some(ProjectRuntime {
            java_home: home,
            java_version: version,
            source: "manual",
        });
    }

    let version = detect_project_java_version(root_path, doc_path)?;
    let Some(home) = find_java_home_for_version(&version) else {
        debug!("Java project JDK: project declares Java {version}, but no matching local JDK was found");
        return None;
    };

    Some(ProjectRuntime {
        java_home: home,
        java_version: version,
        source: "detected",
    })
}

pub(crate) fn configure_jdtls_project_runtime(
    server_name: &str,
    config: Option<Value>,
    root_path: &Path,
    doc_path: Option<&Path>,
) -> Option<Value> {
    if server_name != "jdtls" || config_has_jdtls_runtimes(config.as_ref()) {
        return config;
    }

    let Some(runtime) = project_runtime_for(root_path, doc_path) else {
        return config;
    };

    debug!(
        "jdtls project JDK: using {} Java {} from {} project runtime",
        runtime.java_home.display(),
        runtime.java_version,
        runtime.source
    );
    Some(config_with_jdtls_runtime(
        config,
        &runtime.java_version,
        &runtime.java_home,
    ))
}

pub(crate) fn java_executable(java_home: &Path) -> PathBuf {
    java_home.join(if cfg!(target_os = "windows") {
        "bin/java.exe"
    } else {
        "bin/java"
    })
}

fn project_runtime_override(root_path: &Path) -> Option<PathBuf> {
    PROJECT_RUNTIME_OVERRIDES
        .lock()
        .get(&normalize_dir(root_path))
        .cloned()
}

fn config_has_jdtls_runtimes(config: Option<&Value>) -> bool {
    config
        .and_then(|value| value.get("settings"))
        .and_then(|value| value.get("java"))
        .and_then(|value| value.get("configuration"))
        .and_then(|value| value.get("runtimes"))
        .is_some()
}

fn config_with_jdtls_runtime(config: Option<Value>, version: &str, home: &Path) -> Value {
    let mut config = config.unwrap_or_else(|| json!({}));
    let config_obj = ensure_object(&mut config);
    let settings = ensure_child_object(config_obj, "settings");
    let java = ensure_child_object(settings, "java");
    let configuration = ensure_child_object(java, "configuration");
    configuration.insert(
        "runtimes".into(),
        json!([{
            "name": execution_environment_name(version),
            "path": home.to_string_lossy(),
            "default": true
        }]),
    );
    config
}

fn ensure_object(value: &mut Value) -> &mut Map<String, Value> {
    if !value.is_object() {
        *value = json!({});
    }
    value.as_object_mut().expect("object was just installed")
}

fn ensure_child_object<'a>(
    object: &'a mut Map<String, Value>,
    key: &str,
) -> &'a mut Map<String, Value> {
    let value = object.entry(key.to_string()).or_insert_with(|| json!({}));
    ensure_object(value)
}

fn execution_environment_name(version: &str) -> String {
    if version == "8" {
        "JavaSE-1.8".into()
    } else {
        format!("JavaSE-{version}")
    }
}

fn detect_project_java_version(root_path: &Path, doc_path: Option<&Path>) -> Option<String> {
    let dirs = project_dirs(root_path, doc_path);
    find_version_file(&dirs, ".java-version", read_first_line)
        .or_else(|| find_version_file(&dirs, ".tool-versions", parse_tool_versions))
        .or_else(|| find_version_file(&dirs, ".sdkmanrc", parse_sdkmanrc))
        .or_else(|| find_version_file(&dirs, "pom.xml", parse_maven_java_version))
        .or_else(|| find_version_file(&dirs, "build.gradle.kts", parse_gradle_java_version))
        .or_else(|| find_version_file(&dirs, "build.gradle", parse_gradle_java_version))
}

fn project_dirs(root_path: &Path, doc_path: Option<&Path>) -> Vec<PathBuf> {
    let root = normalize_dir(root_path);
    let start = doc_path
        .and_then(Path::parent)
        .map(normalize_dir)
        .filter(|dir| dir.starts_with(&root))
        .unwrap_or_else(|| root.clone());

    let mut dirs = Vec::new();
    for ancestor in start.ancestors() {
        dirs.push(ancestor.to_path_buf());
        if ancestor == root {
            break;
        }
    }
    if !dirs.iter().any(|dir| dir == &root) {
        dirs.push(root);
    }
    dirs
}

fn normalize_dir(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

fn find_version_file(
    dirs: &[PathBuf],
    file_name: &str,
    parser: fn(&str) -> Option<String>,
) -> Option<String> {
    dirs.iter()
        .map(|dir| dir.join(file_name))
        .find_map(|file| fs::read_to_string(file).ok().and_then(|s| parser(&s)))
        .and_then(|raw| extract_java_major(&raw))
}

fn read_first_line(contents: &str) -> Option<String> {
    contents
        .lines()
        .map(str::trim)
        .find(|line| !line.is_empty())
        .map(str::to_string)
}

fn parse_tool_versions(contents: &str) -> Option<String> {
    contents.lines().find_map(|line| {
        let line = line.trim();
        if line.starts_with('#') {
            return None;
        }
        let mut parts = line.split_whitespace();
        match (parts.next(), parts.next()) {
            (Some("java"), Some(version)) => Some(version.to_string()),
            _ => None,
        }
    })
}

fn parse_sdkmanrc(contents: &str) -> Option<String> {
    contents.lines().find_map(|line| {
        let line = line.trim();
        if line.starts_with('#') {
            return None;
        }
        line.strip_prefix("java=")
            .map(str::trim)
            .filter(|value| !value.is_empty())
            .map(str::to_string)
    })
}

fn parse_maven_java_version(contents: &str) -> Option<String> {
    first_capture(
        contents,
        &[
            r"<maven\.compiler\.release>\s*([^<]+)\s*</maven\.compiler\.release>",
            r"<maven\.compiler\.source>\s*([^<]+)\s*</maven\.compiler\.source>",
            r"<maven\.compiler\.target>\s*([^<]+)\s*</maven\.compiler\.target>",
            r"<release>\s*([^<]+)\s*</release>",
            r"<source>\s*([^<]+)\s*</source>",
            r"<target>\s*([^<]+)\s*</target>",
            r"<java\.version>\s*([^<]+)\s*</java\.version>",
        ],
    )
}

fn parse_gradle_java_version(contents: &str) -> Option<String> {
    first_capture(
        contents,
        &[
            r"JavaLanguageVersion\.of\(\s*([0-9]+)\s*\)",
            r"languageVersion\s*=\s*JavaLanguageVersion\.of\(\s*([0-9]+)\s*\)",
            r"sourceCompatibility\s*=\s*(?:JavaVersion\.)?VERSION_?([0-9_\.]+)",
            r"targetCompatibility\s*=\s*(?:JavaVersion\.)?VERSION_?([0-9_\.]+)",
            r#"sourceCompatibility\s*=\s*["']([^"']+)["']"#,
            r#"targetCompatibility\s*=\s*["']([^"']+)["']"#,
        ],
    )
}

fn first_capture(contents: &str, patterns: &[&str]) -> Option<String> {
    patterns.iter().find_map(|pattern| {
        let re = Regex::new(pattern).ok()?;
        re.captures(contents)
            .and_then(|caps| caps.get(1))
            .map(|m| m.as_str().replace('_', "."))
    })
}

fn extract_java_major(value: &str) -> Option<String> {
    let trimmed = value.trim();
    let re = Regex::new(r"^(?:\D*)?(?:(1)\.)?([0-9]+)").ok()?;
    let caps = re.captures(trimmed)?;
    let major = if caps.get(1).is_some() {
        caps.get(2)?.as_str()
    } else {
        caps.get(2)?.as_str()
    };
    Some(major.to_string())
}

fn find_java_home_for_version(version: &str) -> Option<PathBuf> {
    java_home_candidates(version)
        .into_iter()
        .find(|home| java_home_major(home).as_deref() == Some(version))
}

fn java_home_candidates(version: &str) -> Vec<PathBuf> {
    let mut homes = Vec::new();
    if let Some(home) = env::var_os("JAVA_HOME") {
        homes.push(PathBuf::from(home));
    }
    if let Some(home) = java_home_from_path() {
        homes.push(home);
    }
    homes.extend(sdkman_java_homes());
    if cfg!(target_os = "macos") {
        homes.extend(macos_java_homes(version));
    }
    if cfg!(target_os = "linux") {
        homes.extend(linux_java_homes());
    }
    dedupe_valid_homes(homes)
}

fn java_home_from_path() -> Option<PathBuf> {
    let java = which::which("java").ok()?;
    let real_java = java.canonicalize().unwrap_or(java);
    if cfg!(target_os = "macos") && real_java == Path::new("/usr/bin/java") {
        return None;
    }
    let home = real_java.parent()?.parent()?.to_path_buf();
    valid_java_home(&home).then_some(home)
}

fn sdkman_java_homes() -> Vec<PathBuf> {
    let Some(home) = env::var_os("HOME") else {
        return Vec::new();
    };
    let candidates = PathBuf::from(home).join(".sdkman/candidates/java");
    let Ok(entries) = fs::read_dir(candidates) else {
        return Vec::new();
    };
    entries
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| valid_java_home(path))
        .collect()
}

fn macos_java_homes(version: &str) -> Vec<PathBuf> {
    let output = Command::new("/usr/libexec/java_home")
        .args(["-v", version, "-X"])
        .output();
    let Ok(output) = output else {
        return Vec::new();
    };
    if !output.status.success() {
        return Vec::new();
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    let Ok(re) = Regex::new(r"<string>(/[^<]+/Contents/Home)</string>") else {
        return Vec::new();
    };
    re.captures_iter(&stdout)
        .filter_map(|caps| caps.get(1).map(|m| PathBuf::from(m.as_str())))
        .collect()
}

fn linux_java_homes() -> Vec<PathBuf> {
    let root = Path::new("/usr/lib/jvm");
    let Ok(entries) = fs::read_dir(root) else {
        return Vec::new();
    };
    entries
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| valid_java_home(path))
        .collect()
}

fn dedupe_valid_homes(homes: Vec<PathBuf>) -> Vec<PathBuf> {
    let mut out = Vec::new();
    for home in homes {
        let home = normalize_dir(&home);
        if valid_java_home(&home) && !out.iter().any(|existing| existing == &home) {
            out.push(home);
        }
    }
    out
}

fn valid_java_home(path: &Path) -> bool {
    path.is_dir() && path.join("bin/java").is_file() && path.join("bin/javac").is_file()
}

fn java_home_major(path: &Path) -> Option<String> {
    let release = fs::read_to_string(path.join("release")).ok()?;
    let re = Regex::new(r#"(?m)^JAVA_VERSION="([^"]+)""#).ok()?;
    let raw = re
        .captures(&release)
        .and_then(|caps| caps.get(1))
        .map(|m| m.as_str())?;
    extract_java_major(raw)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn write(path: &Path, contents: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(path, contents).unwrap();
    }

    fn fake_jdk(root: &Path, version: &str) -> PathBuf {
        let jdk = root.join(format!("jdk-{version}"));
        fs::create_dir_all(jdk.join("bin")).unwrap();
        write(&jdk.join("bin/java"), "");
        write(&jdk.join("bin/javac"), "");
        write(
            &jdk.join("release"),
            &format!("JAVA_VERSION=\"{version}.0.1\"\n"),
        );
        jdk
    }

    #[test]
    fn manual_project_runtime_override_is_used_before_detection() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path().join("project");
        let source = root.join("src/main/java/App.java");
        write(&source, "");
        write(&root.join(".java-version"), "99\n");
        let jdk = fake_jdk(tmp.path(), "17");

        let (home, version) =
            set_project_runtime_override(&root, Some(jdk.to_str().unwrap())).unwrap();
        assert_eq!(
            home.as_deref().map(normalize_dir),
            Some(normalize_dir(&jdk))
        );
        assert_eq!(version.as_deref(), Some("17"));

        let config = configure_jdtls_project_runtime("jdtls", None, &root, Some(&source)).unwrap();
        assert_eq!(
            config["settings"]["java"]["configuration"]["runtimes"][0]["name"],
            json!("JavaSE-17")
        );

        let runtime = project_runtime_for(&root, Some(&source)).unwrap();
        assert_eq!(runtime.java_version, "17");
        assert_eq!(runtime.source, "manual");
        assert_eq!(normalize_dir(&runtime.java_home), normalize_dir(&jdk));

        set_project_runtime_override(&root, None).unwrap();
        let config = configure_jdtls_project_runtime("jdtls", None, &root, Some(&source));
        assert!(config.is_none());
    }

    #[test]
    fn declared_project_version_prefers_dot_java_version() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        let source = root.join("src/main/java/App.java");
        write(&source, "");
        write(&root.join(".java-version"), "1.8.0_402\n");
        write(&root.join("pom.xml"), "<java.version>17</java.version>");

        assert_eq!(
            detect_project_java_version(root, Some(&source)).as_deref(),
            Some("8")
        );
    }

    #[test]
    fn declared_project_version_uses_nearest_module_file() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        let module = root.join("module-a");
        let source = module.join("src/main/java/App.java");
        write(&source, "");
        write(&root.join(".sdkmanrc"), "java=17.0.10-tem\n");
        write(&module.join(".sdkmanrc"), "java=21.0.4-tem\n");

        assert_eq!(
            detect_project_java_version(root, Some(&source)).as_deref(),
            Some("21")
        );
    }

    #[test]
    fn declared_project_version_reads_maven_and_gradle() {
        let tmp = TempDir::new().unwrap();
        let root = tmp.path();
        write(
            &root.join("pom.xml"),
            "<properties><maven.compiler.release>11</maven.compiler.release></properties>",
        );
        assert_eq!(
            detect_project_java_version(root, None).as_deref(),
            Some("11")
        );

        fs::remove_file(root.join("pom.xml")).unwrap();
        write(
            &root.join("build.gradle.kts"),
            "java { toolchain { languageVersion = JavaLanguageVersion.of(17) } }",
        );
        assert_eq!(
            detect_project_java_version(root, None).as_deref(),
            Some("17")
        );
    }

    #[test]
    fn jdtls_runtime_injection_preserves_existing_options() {
        let tmp = TempDir::new().unwrap();
        let jdk = fake_jdk(tmp.path(), "17");
        let config = config_with_jdtls_runtime(
            Some(json!({"extendedClientCapabilities": {"classFileContentsSupport": true}})),
            "17",
            &jdk,
        );

        assert_eq!(
            config["extendedClientCapabilities"]["classFileContentsSupport"],
            json!(true)
        );
        assert_eq!(
            config["settings"]["java"]["configuration"]["runtimes"][0]["name"],
            json!("JavaSE-17")
        );
        assert_eq!(
            config["settings"]["java"]["configuration"]["runtimes"][0]["path"],
            json!(jdk.to_string_lossy())
        );
    }

    #[test]
    fn jdtls_runtime_injection_does_not_override_explicit_runtimes() {
        let config = json!({
            "settings": {
                "java": {
                    "configuration": {
                        "runtimes": [{"name": "JavaSE-1.8", "path": "/jdk8"}]
                    }
                }
            }
        });

        assert!(config_has_jdtls_runtimes(Some(&config)));
    }
}

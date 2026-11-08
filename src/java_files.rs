//! Production file discovery from JDTLS source entries, independent of editor project plugins.
use crate::{
    client::Client,
    lsp_ext::{JavaProductionFilesParams, JavaProductionFilesResult},
    msg::RequestId,
};
use anyhow::{bail, Context, Result};
use futures_util::{stream, StreamExt};
use lsp_types::Url;
use serde::Deserialize;
use serde_json::{json, Value};
use std::{
    collections::{BTreeSet, HashSet},
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

const CLASSPATH_ENTRIES: &str = "org.eclipse.jdt.ls.core.classpathEntries";

#[derive(Clone, Debug)]
struct SourceRoot {
    path: PathBuf,
    test: bool,
    generated: bool,
}

#[derive(Deserialize)]
struct ClasspathEntry {
    kind: u32,
    path: Option<PathBuf>,
    #[serde(default)]
    attributes: serde_json::Map<String, Value>,
}

fn local_path(uri: &str) -> Result<PathBuf> {
    Url::parse(uri)?
        .to_file_path()
        .map_err(|_| anyhow::anyhow!("Expected a local file URI, got {uri}"))
}

fn normalized(path: &Path) -> PathBuf {
    dunce::canonicalize(path).unwrap_or_else(|_| crate::utils::path::canonicalize(path))
}

fn true_attribute(attributes: &serde_json::Map<String, Value>, name: &str) -> bool {
    attributes
        .get(name)
        .is_some_and(|v| v == "true" || v == true)
}

fn generated_path(path: &Path) -> bool {
    let parts: Vec<_> = path.iter().filter_map(|s| s.to_str()).collect();
    parts.windows(2).any(|p| {
        (p[0] == "target" && matches!(p[1], "generated-sources" | "generated-test-sources"))
            || (p[0] == "build" && p[1] == "generated")
    })
}

fn source_roots(settings: Value) -> Result<Vec<SourceRoot>> {
    let entries: Vec<ClasspathEntry> = serde_json::from_value(
        settings
            .get(CLASSPATH_ENTRIES)
            .cloned()
            .context("JDTLS did not return classpathEntries")?,
    )?;
    entries
        .into_iter()
        .filter(|e| e.kind == 3)
        .map(|e| {
            let path = e.path.context("Source entry has no path")?;
            if !path.is_absolute() {
                bail!("Source entry path is not absolute: {}", path.display());
            }
            Ok(SourceRoot {
                generated: true_attribute(&e.attributes, "m2e-apt")
                    || true_attribute(&e.attributes, "generated")
                    || generated_path(&path),
                test: true_attribute(&e.attributes, "test"),
                path: normalized(&path),
            })
        })
        .collect()
}

pub(crate) fn relevant_client(client: &Client, project: &Path, server: &str) -> bool {
    let overlaps = |root: &Path| {
        let root = normalized(root);
        root.starts_with(project) || project.starts_with(&root)
    };
    client.name() == server
        && client.is_initialized()
        && (overlaps(&client.root_path)
            || client
                .workspace_folders
                .lock()
                .iter()
                .filter_map(|folder| folder.uri.to_file_path().ok())
                .any(|root| overlaps(&root)))
}

async fn command(
    client: &Client,
    request: &RequestId,
    name: &str,
    arguments: Value,
) -> Result<Value> {
    // Each subrequest must have its own ID, including concurrent requests on one server.
    let id = format!("java-files:{request:?}:{}:{name}:{arguments}", client.id()).into();
    client
        .call_method(
            id,
            "workspace/executeCommand".into(),
            json!({
                "command": name, "arguments": arguments,
            }),
        )
        .await
        .map_err(Into::into)
}

pub(crate) async fn production_files(
    request: &RequestId,
    params: JavaProductionFilesParams,
    clients: Vec<Arc<Client>>,
) -> Result<JavaProductionFilesResult> {
    let project = normalized(&local_path(&params.project_uri)?);
    if !project.is_dir() {
        bail!("Project directory does not exist: {}", project.display());
    }
    if params.extensions.as_ref().is_some_and(|exts| {
        exts.iter()
            .any(|e| e.is_empty() || e.contains(['/', '\\', '.']))
    }) {
        bail!("File extensions must be nonempty names without dots or path separators");
    }
    let mut seen = HashSet::new();
    let clients: Vec<_> = clients
        .into_iter()
        .filter(|c| relevant_client(c, &project, &params.server_name) && seen.insert(c.id()))
        .collect();
    let mut roots = Vec::new();
    let mut modeled_modules = HashSet::new();
    let mut warnings = Vec::new();
    let results = stream::iter(clients.into_iter().map(|client| async move {
        let result = command(&client, request, "java.project.getAll", json!([])).await;
        (client, result)
    }))
    .buffer_unordered(8)
    .collect::<Vec<_>>()
    .await;
    let mut jobs = Vec::new();
    let mut seen_projects = HashSet::new();
    for (client, result) in results {
        match result.and_then(|value| Ok(serde_json::from_value::<Vec<String>>(value)?)) {
            Ok(uris) => {
                for uri in uris {
                    if let Ok(path) = local_path(&uri) {
                        let path = normalized(&path);
                        // Default/scratch projects and unrelated imported projects are not authoritative here.
                        if path.starts_with(&project)
                            && seen_projects.insert((client.id(), path.clone()))
                        {
                            jobs.push((client.clone(), uri, path));
                        }
                    }
                }
            }
            Err(error) => warnings.push(format!("{}: java.project.getAll: {error}", client.name())),
        }
    }
    let results = stream::iter(jobs.into_iter().map(|(client, uri, path)| async move {
        let result = command(
            &client,
            request,
            "java.project.getSettings",
            json!([uri, [CLASSPATH_ENTRIES]]),
        )
        .await
        .and_then(source_roots);
        (path, result)
    }))
    .buffer_unordered(8)
    .collect::<Vec<_>>()
    .await;
    for (path, result) in results {
        match result {
            Ok(entries) => {
                modeled_modules.insert(path);
                roots.extend(entries);
            }
            Err(error) => warnings.push(format!(
                "{}: java.project.getSettings: {error}",
                path.display()
            )),
        }
    }
    tokio::task::spawn_blocking(move || {
        let (fallback, fallback_modules) = conventional_roots(&project, &modeled_modules)?;
        roots.extend(fallback);
        let files = scan_files(
            &roots,
            params.extensions.as_deref(),
            params.include_generated,
            &mut warnings,
        )?;
        let source_roots = roots
            .iter()
            .filter(|r| !r.test && (params.include_generated || !r.generated))
            .map(|r| r.path.to_string_lossy().into_owned())
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect();
        Ok(JavaProductionFilesResult {
            files,
            source_roots,
            fallback_modules,
            warnings,
        })
    })
    .await?
}

fn skip_directory(name: &str) -> bool {
    matches!(
        name,
        ".git"
            | ".hg"
            | ".svn"
            | ".gradle"
            | ".idea"
            | "node_modules"
            | "target"
            | "build"
            | "out"
            | ".metadata"
    )
}

fn conventional_roots(
    project: &Path,
    modeled: &HashSet<PathBuf>,
) -> Result<(Vec<SourceRoot>, Vec<String>)> {
    let mut roots = Vec::new();
    let mut modules = BTreeSet::new();
    let mut pending = vec![project.to_owned()];
    while let Some(dir) = pending.pop() {
        if !modeled.contains(&normalized(&dir)) {
            for (relative, test, generated) in [
                ("src/main/java", false, false),
                ("src/main/resources", false, false),
                ("src/test/java", true, false),
                ("src/test/resources", true, false),
                ("src/it/java", true, false),
                ("src/integrationTest/java", true, false),
                ("src/testFixtures/java", true, false),
                ("target/generated-sources", false, true),
                ("target/generated-test-sources", true, true),
                (
                    "build/generated/sources/annotationProcessor/java/main",
                    false,
                    true,
                ),
                (
                    "build/generated/sources/annotationProcessor/java/test",
                    true,
                    true,
                ),
            ] {
                let path = dir.join(relative);
                if path.is_dir() {
                    modules.insert(dir.to_string_lossy().into_owned());
                    roots.push(SourceRoot {
                        path: normalized(&path),
                        test,
                        generated,
                    });
                }
            }
        }
        for entry in fs::read_dir(&dir).with_context(|| format!("Cannot read {}", dir.display()))? {
            let entry = entry?;
            // Do not recurse into sources or follow symlinks while discovering module directories.
            if entry.file_type()?.is_dir()
                && entry.file_name() != "src"
                && !skip_directory(&entry.file_name().to_string_lossy())
            {
                pending.push(entry.path());
            }
        }
    }
    Ok((roots, modules.into_iter().collect()))
}

fn production_path(path: &Path, roots: &[SourceRoot], include_generated: bool) -> bool {
    let nearest = roots
        .iter()
        .filter(|r| path.starts_with(&r.path))
        .map(|r| r.path.components().count())
        .max();
    nearest.is_some_and(|depth| {
        let mut matching = roots
            .iter()
            .filter(|r| path.starts_with(&r.path) && r.path.components().count() == depth);
        // A test/excluded entry wins if overlapping modules disagree about the same root.
        matching.all(|r| !r.test && (include_generated || !r.generated))
    })
}

fn scan_files(
    roots: &[SourceRoot],
    extensions: Option<&[String]>,
    include_generated: bool,
    warnings: &mut Vec<String>,
) -> Result<Vec<String>> {
    let mut visited = HashSet::new();
    let mut files = BTreeSet::new();
    let mut pending: Vec<_> = roots
        .iter()
        .filter(|r| !r.test && (include_generated || !r.generated))
        .map(|r| r.path.clone())
        .collect();
    while let Some(path) = pending.pop() {
        if !visited.insert(path.clone()) {
            continue;
        }
        let entries = match fs::read_dir(&path) {
            Ok(entries) => entries,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => continue,
            Err(error) => {
                warnings.push(format!("Cannot scan {}: {error}", path.display()));
                continue;
            }
        };
        for entry in entries {
            let entry = entry?;
            let kind = entry.file_type()?;
            let path = entry.path();
            if kind.is_dir() {
                if !matches!(
                    entry.file_name().to_str(),
                    Some(".git" | ".hg" | ".svn" | ".metadata")
                ) && production_path(&path, roots, include_generated)
                {
                    pending.push(path);
                }
            } else if kind.is_file()
                && production_path(&path, roots, include_generated)
                && extensions.is_none_or(|exts| {
                    path.extension()
                        .and_then(|e| e.to_str())
                        .is_some_and(|ext| exts.iter().any(|e| e.eq_ignore_ascii_case(ext)))
                })
            {
                files.insert(path.to_string_lossy().into_owned());
            }
            // Source roots themselves may be links; nested links are skipped to avoid cycles and leaking tests.
        }
    }
    Ok(files.into_iter().collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn write(path: &Path) {
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, "").unwrap();
    }

    fn root(path: &Path, test: bool, generated: bool) -> SourceRoot {
        SourceRoot {
            path: normalized(path),
            test,
            generated,
        }
    }

    fn scan(roots: &[SourceRoot], generated: bool) -> Vec<String> {
        scan_files(roots, Some(&["java".into()]), generated, &mut Vec::new()).unwrap()
    }

    #[test]
    fn classifies_by_source_entry_attributes_not_names() {
        let roots = source_roots(json!({CLASSPATH_ENTRIES: [
            {"kind":3,"path":"/custom-production","attributes":{}},
            {"kind":3,"path":"/custom-tests","attributes":{"test":"true"}},
            {"kind":1,"path":"/dependencies.jar"},
            {"kind":3,"path":"/generated","attributes":{"m2e-apt":"true"}},
            {"kind":3,"path":"/test-generated","attributes":{"test":true,"generated":true}}
        ]}))
        .unwrap();
        assert_eq!(roots.len(), 4);
        assert!(!roots[0].test);
        assert!(roots[1].test);
        assert!(roots[2].generated);
        assert!(roots[3].test && roots[3].generated);
        assert!(source_roots(json!({CLASSPATH_ENTRIES: []}))
            .unwrap()
            .is_empty());
    }

    #[test]
    fn rejects_missing_or_malformed_source_models() {
        for value in [
            json!(null),
            json!({}),
            json!({CLASSPATH_ENTRIES:null}),
            json!({CLASSPATH_ENTRIES:[{"kind":3,"path":"relative"}]}),
            json!({CLASSPATH_ENTRIES:[{"kind":3}]}),
        ] {
            assert!(source_roots(value).is_err());
        }
        assert!(local_path("jdt://contents/Widget.class").is_err());
    }

    #[test]
    fn custom_roots_and_nested_tests_are_respected() {
        let tmp = TempDir::new().unwrap();
        let production = tmp.path().join("custom-production");
        let tests = production.join("nested-tests");
        write(&production.join("UserServiceTest.java"));
        write(&tests.join("UserService.java"));
        write(&tmp.path().join("custom-tests/Other.java"));
        write(&production.join("settings.properties"));
        let roots = [
            root(&production, false, false),
            root(&tests, true, false),
            root(&tmp.path().join("custom-tests"), true, false),
        ];
        assert_eq!(
            scan(&roots, true),
            vec![normalized(&production.join("UserServiceTest.java")).to_string_lossy()]
        );
        assert_eq!(
            scan_files(&roots, None, true, &mut vec![]).unwrap().len(),
            2
        );
    }

    #[test]
    fn generated_roots_external_roots_and_duplicates() {
        let tmp = TempDir::new().unwrap();
        let source = tmp.path().join("sources");
        let generated = source.join("generated");
        let external = tmp.path().join("external");
        for path in [
            source.join("App.java"),
            generated.join("Generated.java"),
            external.join("Extra.java"),
        ] {
            write(&path);
        }
        let roots = [
            root(&source, false, false),
            root(&generated, false, true),
            root(&external, false, false),
            root(&source, false, false),
        ];
        assert_eq!(scan(&roots, true).len(), 3);
        assert_eq!(scan(&roots, false).len(), 2);
        assert!(!production_path(
            &tmp.path().join("sources-other/Test.java"),
            &roots,
            true
        ));
        assert!(generated_path(Path::new(
            "/p/target/generated-sources/annotations"
        )));
        assert!(generated_path(Path::new("/p/build/generated/custom/main")));
    }

    #[test]
    fn conflicting_test_classification_wins() {
        let tmp = TempDir::new().unwrap();
        let roots = [
            root(tmp.path(), false, false),
            root(tmp.path(), true, false),
        ];
        write(&tmp.path().join("App.java"));
        assert!(scan(&roots, true).is_empty());
    }

    #[test]
    fn modeled_modules_do_not_gain_conventional_roots() {
        let tmp = TempDir::new().unwrap();
        for file in [
            "module-a/src/main/java/Old.java",
            "module-b/src/main/java/Main.java",
            "module-b/src/test/java/Test.java",
            "module-b/target/generated-sources/A.java",
            "node_modules/fake/src/main/java/Fake.java",
        ] {
            write(&tmp.path().join(file));
        }
        let modeled = HashSet::from([normalized(&tmp.path().join("module-a"))]);
        let (roots, modules) = conventional_roots(tmp.path(), &modeled).unwrap();
        assert_eq!(modules.len(), 1);
        assert_eq!(scan(&roots, true).len(), 2);
        assert_eq!(scan(&roots, false).len(), 1);
        assert!(roots.iter().any(|r| r.test));
    }

    #[cfg(unix)]
    #[test]
    fn linked_roots_work_but_nested_links_do_not_escape_or_cycle() {
        use std::os::unix::fs::symlink;
        let tmp = TempDir::new().unwrap();
        write(&tmp.path().join("real/App.java"));
        write(&tmp.path().join("tests/Test.java"));
        symlink(tmp.path().join("real"), tmp.path().join("link")).unwrap();
        symlink(tmp.path().join("tests"), tmp.path().join("real/tests")).unwrap();
        symlink(tmp.path().join("real"), tmp.path().join("real/loop")).unwrap();
        assert_eq!(
            scan(&[root(&tmp.path().join("link"), false, false)], true).len(),
            1
        );
    }

    const PEER: &str = r#"
import sys,json
fixture=json.load(open(sys.argv[1]))
while True:
    headers={}
    while True:
        line=sys.stdin.buffer.readline()
        if not line: sys.exit(0)
        if line==b'\r\n': break
        k,v=line.decode().split(':',1); headers[k.lower()]=v.strip()
    msg=json.loads(sys.stdin.buffer.read(int(headers['content-length'])))
    if msg.get('method')=='exit': sys.exit(0)
    if 'id' not in msg: continue
    command=msg.get('params',{}).get('command')
    if command=='java.project.getAll': result=fixture['projects']
    elif command=='java.project.getSettings':
        args=msg['params']['arguments']
        assert args[1]==['org.eclipse.jdt.ls.core.classpathEntries']
        result=fixture['settings'].get(args[0])
    else: result=None
    body=json.dumps({'jsonrpc':'2.0','id':msg['id'],'result':result}).encode()
    sys.stdout.buffer.write(('Content-Length: %d\r\n\r\n'%len(body)).encode()+body)
    sys.stdout.buffer.flush()
"#;

    async fn client(tmp: &Path, id: usize, project: &Path, fixture: Value) -> Arc<Client> {
        let fixture_file = tmp.join(format!("fixture-{id}.json"));
        fs::write(&fixture_file, fixture.to_string()).unwrap();
        let (mut client, _, notify, _) = Client::start(
            "python3",
            &[
                "-u".into(),
                "-c".into(),
                PEER.into(),
                fixture_file.to_string_lossy().into_owned(),
            ],
            None,
            None,
            Default::default(),
            &[],
            id,
            "jdtls".into(),
            2,
            None,
            None,
        )
        .unwrap();
        client.root_path = project.to_owned();
        client.capabilities.set(Default::default()).unwrap();
        notify.notify_one();
        Arc::new(client)
    }

    fn params(project: &Path) -> JavaProductionFilesParams {
        JavaProductionFilesParams {
            project_uri: Url::from_directory_path(project).unwrap().to_string(),
            server_name: "jdtls".into(),
            extensions: Some(vec!["java".into()]),
            include_generated: true,
        }
    }

    #[tokio::test]
    async fn multiple_servers_custom_external_roots_and_unimported_module() {
        let tmp = TempDir::new().unwrap();
        let project = tmp.path().join("project");
        let mut clients = Vec::new();
        for id in 0..2 {
            let module = project.join(format!("module-{id}"));
            let source = module.join("custom-production");
            let tests = module.join("custom-tests");
            write(&source.join("UserServiceTest.java"));
            write(&tests.join("UserService.java"));
            // This conventional directory must not override the actual project model.
            write(&module.join("src/main/java/Unconfigured.java"));
            let uri = Url::from_directory_path(&module).unwrap().to_string();
            let external = tmp.path().join(format!("external-{id}"));
            write(&external.join("External.java"));
            let fixture = json!({"projects":[uri, uri], "settings": {uri.clone(): {CLASSPATH_ENTRIES:[
                {"kind":3,"path":source}, {"kind":3,"path":tests,"attributes":{"test":"true"}},
                {"kind":3,"path":external}]}}});
            clients.push(client(tmp.path(), id, &module, fixture).await);
        }
        write(&project.join("unimported/src/main/java/New.java"));
        write(&project.join("unimported/src/test/java/NewTest.java"));
        let result = production_files(&42.into(), params(&project), clients.clone())
            .await
            .unwrap();
        assert_eq!(result.files.len(), 5);
        assert_eq!(result.fallback_modules.len(), 1);
        assert!(result.warnings.is_empty());
        assert!(!result
            .files
            .iter()
            .any(|f| f.ends_with("UserService.java") || f.ends_with("Unconfigured.java")));
        for client in clients {
            client.force_shutdown().await.unwrap();
        }
    }

    #[tokio::test]
    async fn malformed_model_falls_back_without_inventing_custom_roots() {
        let tmp = TempDir::new().unwrap();
        write(&tmp.path().join("src/main/java/App.java"));
        write(&tmp.path().join("custom/Test.java"));
        let client = client(
            tmp.path(),
            1,
            tmp.path(),
            json!({"projects":{},"settings":{}}),
        )
        .await;
        let result = production_files(&42.into(), params(tmp.path()), vec![client.clone()])
            .await
            .unwrap();
        assert_eq!(result.files.len(), 1);
        assert_eq!(result.warnings.len(), 1);
        assert_eq!(result.fallback_modules.len(), 1);
        client.force_shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn no_server_falls_back_and_invalid_parameters_fail() {
        let tmp = TempDir::new().unwrap();
        write(&tmp.path().join("src/main/java/App.java"));
        let result = production_files(&42.into(), params(tmp.path()), vec![])
            .await
            .unwrap();
        assert_eq!(result.files.len(), 1);
        let mut invalid = params(tmp.path());
        invalid.extensions = Some(vec!["../java".into()]);
        assert!(production_files(&42.into(), invalid, vec![]).await.is_err());
    }
}

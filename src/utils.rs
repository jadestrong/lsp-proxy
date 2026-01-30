use crate::syntax::SupportWorkspace;
use log::{debug, error};
use lsp_types::{Diagnostic, DocumentSymbol, DocumentSymbolResponse, SymbolInformation, Url};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::{
    cmp::Ordering,
    path::{Path, PathBuf},
    time::{SystemTime, UNIX_EPOCH},
};
use stringslice::StringSlice;

pub fn current_working_dir() -> PathBuf {
    std::env::current_dir()
        .and_then(dunce::canonicalize)
        .expect("Cound't determine current working directory")
}

pub mod path {
    use crate::utils::current_working_dir;
    use etcetera::home_dir;
    use std::{
        borrow::Cow,
        path::{Component, Path, PathBuf},
    };

    /// Expands tilde `~` into users home directory if available, otherwise returns the path
    /// unchanged. The tilde will only be expanded when present as the first component of the path
    /// and only slash follows it.
    pub fn expand_tilde<'a, P>(path: P) -> Cow<'a, Path>
    where
        P: Into<Cow<'a, Path>>,
    {
        let path = path.into();
        let mut components = path.components();
        if let Some(Component::Normal(c)) = components.next() {
            if c == "~" {
                if let Ok(mut buf) = home_dir() {
                    buf.push(components);
                    return Cow::Owned(buf);
                }
            }
        }

        path
    }

    ///  Normalize a path without resolving symlinks. Copy from helix-stdx
    // Strategy: start from the first component and move up. Cannonicalize previous path,
    // join component, cannonicalize new path, strip prefix and join to the final result.
    pub fn normalize(path: impl AsRef<Path>) -> PathBuf {
        let mut components = path.as_ref().components().peekable();
        let mut ret = if let Some(c @ Component::Prefix(..)) = components.peek().cloned() {
            components.next();
            PathBuf::from(c.as_os_str())
        } else {
            PathBuf::new()
        };

        for component in components {
            match component {
                Component::Prefix(..) => unreachable!(),
                Component::RootDir => {
                    ret.push(component.as_os_str());
                }
                Component::CurDir => {}
                #[cfg(not(windows))]
                Component::ParentDir => {
                    ret.pop();
                }
                #[cfg(windows)]
                Component::ParentDir => {
                    if let Some(head) = ret.components().next_back() {
                        match head {
                            Component::Prefix(_) | Component::RootDir => {}
                            Component::CurDir => unreachable!(),
                            // If we left previous component as ".." it means we met a symlink before and we can't pop path.
                            Component::ParentDir => {
                                ret.push("..");
                            }
                            Component::Normal(_) => {
                                if ret.is_symlink() {
                                    ret.push("..");
                                } else {
                                    ret.pop();
                                }
                            }
                        }
                    }
                }
                #[cfg(not(windows))]
                Component::Normal(c) => {
                    ret.push(c);
                }
                #[cfg(windows)]
                Component::Normal(c) => 'normal: {
                    use std::fs::canonicalize;

                    let new_path = ret.join(c);
                    if new_path.is_symlink() {
                        ret = new_path;
                        break 'normal;
                    }
                    let (can_new, can_old) = (canonicalize(&new_path), canonicalize(&ret));
                    match (can_new, can_old) {
                        (Ok(can_new), Ok(can_old)) => {
                            let striped = can_new.strip_prefix(can_old);
                            ret.push(striped.unwrap_or_else(|_| c.as_ref()));
                        }
                        _ => ret.push(c),
                    }
                }
            }
        }
        dunce::simplified(&ret).to_path_buf()
    }

    /// Returns the canonical, absolute form of a path with all intermediate components normalized.
    ///
    /// This function is used instead of [`std::fs::canonicalize`] because we don't want to verify
    /// here if the path exists, just normalize it's components.
    pub fn canonicalize(path: impl AsRef<Path>) -> PathBuf {
        let path = expand_tilde(path.as_ref());
        let path = if path.is_relative() {
            Cow::Owned(current_working_dir().join(path))
        } else {
            path
        };

        normalize(path)
    }

    /// 判断两个给定的文件路径是否指向相同的文件或目录
    pub fn paths_are_same(path_a: &str, path_b: &str) -> bool {
        let a = canonicalize(path_a);
        let b = canonicalize(path_b);
        // let a = normalize(&path_a.expand_home().unwrap());
        // let b = normalize(&path_b.expand_home().unwrap());
        if a.exists() && b.exists() {
            a == b
        } else {
            false
        }
    }

    pub fn path_is_ancestor_of(path_a: &str, path_b: &str) -> bool {
        if paths_are_same(path_a, path_b) {
            return false;
        }
        let a = canonicalize(path_a);
        let b = canonicalize(path_b);
        // let a = normalize(&path_a.expand_home().unwrap());
        // let b = normalize(&path_b.expand_home().unwrap());

        if !a.exists() || !b.exists() {
            return false;
        }

        b.starts_with(a)
    }
}

/// Finds the current workspace folder.
/// Used as a ceiling dir for LSP root resolution, the filepicker and potentially as a future filewatching root
///
/// This function starts searching the FS upward from the CWD
/// and returns the first directory that contains `.git`.
/// If no workspace was found returns (CWD, true).
/// Otherwise (workspace, false) is returned.
pub fn find_workspace_for_file(filepath: &Path) -> (PathBuf, bool) {
    let current_dir = filepath.parent().unwrap().to_path_buf();
    for ancestor in current_dir.ancestors() {
        if ancestor.join(".git").exists() {
            return (ancestor.to_owned(), false);
        }
    }
    (current_dir, true)
}

pub fn find_workspace_folder_for_uri(uri: &lsp_types::Url) -> Option<(String, String)> {
    let current_dir = uri.to_file_path();
    if let Ok(current_dir) = current_dir {
        for ancestor in current_dir.ancestors() {
            if ancestor.join(".git").exists() {
                let filepath = ancestor.to_str().expect("convert ancestor to a str");
                let filename = ancestor
                    .file_name()
                    .map(|f| f.to_string_lossy().to_string())
                    .expect("ancestor's filename");
                // let url = Url::parse(filepath);
                let url = Url::from_file_path(filepath);
                return match url {
                    Ok(val) => Some((val.to_string(), filename)),
                    Err(_) => None,
                };
            }
        }
    }
    None
}

/// Find LSP workspace directory based on support_workspace configuration.
///
/// This function supports different workspace strategies:
/// - `SupportWorkspace::WorkspaceRoots(markers)`:  search for the closest workspace root marker
/// - `SupportWorkspace::Bool(_)`: search for the topmost root marker
pub fn find_lsp_workspace(
    doc_path: Option<&Path>,
    root_markers: &[String],
    support_workspace: &SupportWorkspace,
) -> PathBuf {
    let Some(doc_path) = doc_path else {
        return current_working_dir();
    };

    let (git_workspace, workspace_is_cwd) = find_workspace_for_file(doc_path);
    let git_workspace = path::normalize(&git_workspace);

    let lsp_workspace = match support_workspace {
        // add a check that if support_roots is not empty, then use the closest marker
        // otherwise, use the topmost marker
        SupportWorkspace::WorkspaceRoots(workspace_root_markers) if !workspace_root_markers.is_empty() => {
            // Monorepo project: try to find the closest workspace root marker first
            find_lsp_workspace_closest(doc_path, workspace_root_markers, &git_workspace)
                // Fall back to topmost strategy if no workspace markers found
                .or_else(|| find_lsp_workspace_topmost(doc_path, root_markers, &git_workspace, workspace_is_cwd))
        }
        _ => {
            // Traditional project: find the topmost root marker
            find_lsp_workspace_topmost(doc_path, root_markers, &git_workspace, workspace_is_cwd)
        }
    };

    lsp_workspace.unwrap_or(current_working_dir())
}

/// Find the closest root directory for a file.
/// This function searches upward from the file path to find the most specific
/// directory that contains any of the workspace root markers, but doesn't go beyond the git workspace.
///
/// Returns the most specific (closest to file) root marker directory if found,
/// or None if no workspace markers are found.
fn find_lsp_workspace_closest(
    doc_path: &Path,
    workspace_root_markers: &[String],
    workspace: &Path,
) -> Option<PathBuf> {
    let current_dir = doc_path.parent().unwrap_or(doc_path);

    // Try to find workspace markers first
    current_dir
        .ancestors()
        .take_while(|ancestor| {
            // include git_workspace itself, but not beyond it
            ancestor.starts_with(workspace) || *ancestor == workspace
        })
        .find(|ancestor| {
            workspace_root_markers
                .iter()
                .any(|marker| ancestor.join(marker).exists())
        })
        .map(|p| p.to_path_buf())
    // Return None if no workspace markers found (instead of falling back to workspace)
}

fn find_lsp_workspace_topmost(
    file: &Path,
    root_markers: &[String],
    workspace: &Path,
    workspace_is_cwd: bool,
) -> Option<PathBuf> {
    // convert file to an absolute path
    let mut file = if file.is_absolute() {
        file.to_path_buf()
    } else {
        error!("Not support relative path");
        let current_dir = current_working_dir();
        current_dir.join(file)
    };
    file = path::normalize(&file);

    // This only possible when the passed-in file is an absolute path that is not
    // under the workspace path. In this case, we should return None.
    if !file.starts_with(workspace) {
        return None;
    }

    let mut top_marker = None;
    for ancestor in file.ancestors() {
        if root_markers
            .iter()
            .any(|marker| ancestor.join(marker).exists())
        {
            top_marker = Some(ancestor);
        }

        if ancestor == workspace {
            debug!("~~~ {workspace:?}");
            // if the workspace is the CWD, let the LSP decide what the workspace
            // is
            return top_marker
                .or_else(|| (!workspace_is_cwd).then_some(workspace))
                .map(Path::to_owned);
        }
    }
    debug_assert!(false, "workspace must be an ancestor of <file>");
    None
}

pub fn from_json<T: DeserializeOwned>(
    what: &'static str,
    json: &serde_json::Value,
) -> anyhow::Result<T> {
    serde_json::from_value(json.clone())
        .map_err(|e| anyhow::format_err!("Failed to deserialize {what}: {e}; {json}"))
}

fn are_diagnostic_equal(d1: &Diagnostic, d2: &Diagnostic) -> bool {
    d1.range.start.eq(&d2.range.start)
        && d1.range.end.eq(&d2.range.end)
        && d1.message == d2.message
        && d1.severity == d2.severity
        && d1.code == d2.code
        && d1.source == d2.source
}

pub fn is_diagnostic_vectors_equal(vec1: &Vec<&Diagnostic>, vec2: &Vec<Diagnostic>) -> bool {
    if vec1.len() != vec2.len() {
        return false;
    }
    for (d1, d2) in vec1.iter().zip(vec2.iter()) {
        if !are_diagnostic_equal(d1, d2) {
            return false;
        }
    }
    true
}

pub fn truncate_completion_item(mut item: lsp_types::CompletionItem) -> lsp_types::CompletionItem {
    let max_total_length = 75;
    let max_label_length = 30;

    let origin_label = item.label.clone();
    let label_description = item
        .label_details
        .as_ref()
        .and_then(|details| details.description.clone())
        .unwrap_or_default();

    if origin_label.chars().count() + label_description.chars().count() <= max_total_length {
        return item;
    }

    let mut new_label = item.label.clone();
    let mut new_description = label_description.clone();

    // 如果 label 超过最大长度，则只截取前27的字符并拼接"..."
    if new_label.chars().count() > max_label_length {
        new_label = format!("{}...", &new_label.slice(..27));
    }

    let remaining_length = max_total_length - new_label.chars().count();

    if remaining_length < new_description.chars().count() {
        new_description = format!(
            "{}...",
            &new_description.slice(..remaining_length.saturating_sub(3))
        );
    }

    item.label = new_label;
    if let Some(ref mut details) = item.label_details {
        details.description = Some(new_description);
    }

    if item.insert_text.is_none() && item.label != origin_label {
        item.insert_text = Some(origin_label);
    }

    item
}

pub fn get_activate_time() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis()
}

/// Merge two TOML documents, merging values from `right` onto `left`
///
/// When an array exists in both `left` and `right`, `right`'s array is
/// used (arrays are always replaced, not merged). When a table exists in
/// both `left` and `right`, the merged table consists of all keys in
/// `left`'s table unioned with all keys in `right` with the values of
/// `right` being merged recursively onto values of `left`.
///
/// Top-level arrays with `name` fields (like `[[language]]`) are merged
/// by matching `name` values, allowing partial overrides.
pub fn merge_toml_values(left: toml::Value, right: toml::Value, merge_depth: usize) -> toml::Value {
    use toml::Value;

    fn get_name(v: &Value) -> Option<&str> {
        v.get("name").and_then(Value::as_str)
    }

    match (left, right) {
        (Value::Array(mut left_items), Value::Array(right_items)) => {
            // Only merge arrays at top level (when they have 'name' fields)
            // All other arrays are replaced
            if merge_depth > 0 && right_items.first().and_then(get_name).is_some() {
                left_items.reserve(right_items.len());
                for rvalue in right_items {
                    let lvalue = get_name(&rvalue)
                        .and_then(|rname| {
                            left_items.iter().position(|v| get_name(v) == Some(rname))
                        })
                        .map(|lpos| left_items.remove(lpos));
                    let mvalue = match lvalue {
                        Some(lvalue) => merge_toml_values(lvalue, rvalue, merge_depth - 1),
                        None => rvalue,
                    };
                    left_items.push(mvalue);
                }
                Value::Array(left_items)
            } else {
                // Replace arrays without 'name' fields (like args, environment vars)
                Value::Array(right_items)
            }
        }
        (Value::Table(mut left_map), Value::Table(right_map)) => {
            // Always merge tables recursively
            for (rname, rvalue) in right_map {
                match left_map.remove(&rname) {
                    Some(lvalue) => {
                        // Special case: language-servers should be replaced, not merged
                        let merged_value = if rname == "language-servers" {
                            rvalue // Replace instead of merge
                        } else {
                            merge_toml_values(lvalue, rvalue, merge_depth.saturating_sub(1))
                        };
                        left_map.insert(rname, merged_value);
                    }
                    None => {
                        left_map.insert(rname, rvalue);
                    }
                }
            }
            Value::Table(left_map)
        }
        // Catch everything else we didn't handle, and use the right value
        (_, value) => value,
    }
}

pub fn ensure_parent_dir(path: &Path) {
    if let Some(parent) = path.parent() {
        if !parent.exists() {
            std::fs::create_dir_all(parent).ok();
        }
    }
}

pub fn uri_to_path(uri: &Url) -> PathBuf {
    uri.to_file_path().ok().unwrap_or_default()
}

pub struct Deferred<F: FnOnce()>(Option<F>);

impl<F: FnOnce()> Deferred<F> {
    pub fn abort(mut self) {
        self.0.take();
    }
}

impl<F: FnOnce()> Drop for Deferred<F> {
    fn drop(&mut self) {
        if let Some(f) = self.0.take() {
            f()
        }
    }
}

#[must_use]
pub fn defer<F: FnOnce()>(f: F) -> Deferred<F> {
    Deferred(Some(f))
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ImenuEntry {
    Group(Vec<(String, ImenuEntry)>),
    Position((u32, u32)), // (line, character)
}

impl ImenuEntry {
    /// Retrieve the position information of the entry for sorting comparison
    fn get_position(&self) -> (u32, u32) {
        match self {
            ImenuEntry::Group(children) => {
                // For a Group, use the position of the first child, or (0, 0) if it is empty
                children
                    .first()
                    .map(|(_, entry)| entry.get_position())
                    .unwrap_or((0, 0))
            }
            ImenuEntry::Position(pos) => *pos,
        }
    }
}

pub fn lsp_symbols_to_imenu(response: Option<DocumentSymbolResponse>) -> Vec<(String, ImenuEntry)> {
    response.map_or(Vec::new(), |symbols| match symbols {
        DocumentSymbolResponse::Flat(symbols) => flat_symbols_to_imenu(&symbols),
        DocumentSymbolResponse::Nested(symbols) => nested_symbols_to_imenu(&symbols),
    })
}

// Process Nested DocumentSymbols
fn nested_symbols_to_imenu(symbols: &[DocumentSymbol]) -> Vec<(String, ImenuEntry)> {
    symbols
        .iter()
        .flat_map(|sym| {
            let name = sym.name.clone();
            let lsp_types::Position { line, character } = sym.range.start;
            let position_entry = (name.clone(), ImenuEntry::Position((line, character)));

            match &sym.children {
                Some(children) if !children.is_empty() => {
                    let children_entries = nested_symbols_to_imenu(children);
                    let group_entry = (name.clone(), ImenuEntry::Group(children_entries));
                    vec![position_entry, group_entry]
                }
                _ => vec![position_entry],
            }
        })
        // .map(|sym| {
        //     let name = sym.name.clone();
        //     sym.children.as_ref().map_or_else(
        //         || {
        //             let lsp_types::Position { line, character } = sym.range.start;
        //             (name.clone(), ImenuEntry::Position((line, character)))
        //         },
        //         |children| {
        //             if children.is_empty() {
        //                 let lsp_types::Position { line, character } = sym.range.start;
        //                 return (name.clone(), ImenuEntry::Position((line, character)));
        //             } else {
        //                 let children = nested_symbols_to_imenu(&children);
        //                 (name.clone(), ImenuEntry::Group(children))
        //             }
        //         },
        //     )
        // })
        .collect()
}

// Process Flat SymbolInformation
fn flat_symbols_to_imenu(symbols: &[SymbolInformation]) -> Vec<(String, ImenuEntry)> {
    symbols
        .iter()
        .map(|sym| {
            let name = sym.name.clone();
            let lsp_types::Position { line, character } = sym.location.range.start;
            (name, ImenuEntry::Position((line, character)))
        })
        .collect()
}

pub fn sort_imenu_entries_grouped(entries: &mut Vec<(String, ImenuEntry)>) {
    // Create a temporary structure to hold the grouping information
    let mut groups: Vec<Vec<(String, ImenuEntry)>> = Vec::new();
    let mut current_group: Vec<(String, ImenuEntry)> = Vec::new();
    let mut last_name: Option<String> = None;

    // First, sort all entries by their positions.
    entries.sort_by(|a, b| {
        let pos_a = a.1.get_position();
        let pos_b = b.1.get_position();
        pos_a.0.cmp(&pos_b.0).then_with(|| pos_a.1.cmp(&pos_b.1))
    });

    for (name, entry) in entries.drain(..) {
        match &last_name {
            Some(last) if last == &name => {
                // For entries with the same name, add them to the current group.
                current_group.push((name.clone(), entry));
            }
            _ => {
                // For a new name, save the current group and start a new group
                if !current_group.is_empty() {
                    groups.push(current_group);
                    current_group = Vec::new();
                }
                current_group.push((name.clone(), entry));
                last_name = Some(name);
            }
        }
    }

    if !current_group.is_empty() {
        groups.push(current_group);
    }

    for group in groups.iter_mut() {
        group.sort_by(|a, b| match (&a.1, &b.1) {
            (ImenuEntry::Position(_), ImenuEntry::Group(_)) => Ordering::Less,
            (ImenuEntry::Group(_), ImenuEntry::Position(_)) => Ordering::Greater,
            _ => Ordering::Equal,
        });

        // Recursively sort the contents within the Group
        for (_, entry) in group.iter_mut() {
            if let ImenuEntry::Group(ref mut children) = entry {
                sort_imenu_entries_grouped(children);
            }
        }
    }

    // Flatten the grouped results back into the original Vec
    *entries = groups.into_iter().flatten().collect();
}

const DIAGNOSTIC_SEVERITY_PRIORITY: [lsp_types::DiagnosticSeverity; 4] = [
    lsp_types::DiagnosticSeverity::ERROR,
    lsp_types::DiagnosticSeverity::WARNING,
    lsp_types::DiagnosticSeverity::INFORMATION,
    lsp_types::DiagnosticSeverity::HINT,
];

pub fn limit_diagnostics_for_push(
    diagnostics: &[lsp_types::Diagnostic],
    max_count: usize,
) -> Vec<lsp_types::Diagnostic> {
    if diagnostics.len() <= max_count {
        return diagnostics.to_vec();
    }

    let mut sorted_diagnostics = diagnostics.to_vec();
    sorted_diagnostics.sort_by(|a, b| {
        let severity_a = a.severity.unwrap_or(lsp_types::DiagnosticSeverity::HINT);
        let severity_b = b.severity.unwrap_or(lsp_types::DiagnosticSeverity::HINT);

        let priority_a = DIAGNOSTIC_SEVERITY_PRIORITY
            .iter()
            .position(|&s| s == severity_a)
            .unwrap_or(3);
        let priority_b = DIAGNOSTIC_SEVERITY_PRIORITY
            .iter()
            .position(|&s| s == severity_b)
            .unwrap_or(3);

        priority_a.cmp(&priority_b)
    });

    sorted_diagnostics.into_iter().take(max_count).collect()
}

#[cfg(test)]
mod test {
    use super::merge_toml_values;
    use toml::Value;

    #[test]
    fn test_merge_simple_tables() {
        let left = toml::from_str(
            r#"
        [server]
        command = "default-cmd"
        timeout = 30
    "#,
        )
        .unwrap();

        let right = toml::from_str(
            r#"
        [server]
        command = "custom-cmd"
    "#,
        )
        .unwrap();

        let merged = merge_toml_values(left, right, 5);
        let server = merged.get("server").unwrap();

        assert_eq!(server.get("command").unwrap().as_str(), Some("custom-cmd"));
        assert_eq!(server.get("timeout").unwrap().as_integer(), Some(30));
    }

    #[test]
    fn test_merge_arrays_without_name_replaces() {
        let left = toml::from_str(
            r#"
        [server]
        args = ["--stdio"]
    "#,
        )
        .unwrap();

        let right = toml::from_str(
            r#"
        [server]
        args = ["--log-level", "verbose"]
    "#,
        )
        .unwrap();

        let merged = merge_toml_values(left, right, 5);
        let args = merged
            .get("server")
            .unwrap()
            .get("args")
            .unwrap()
            .as_array()
            .unwrap();

        assert_eq!(args.len(), 2);
        assert_eq!(args[0].as_str(), Some("--log-level"));
        assert_eq!(args[1].as_str(), Some("verbose"));
    }

    #[test]
    fn test_merge_arrays_with_name_merges() {
        let left: Value = toml::from_str(
            r#"
        [[language]]
        name = "rust"
        file-types = ["rs"]
        
        [[language]]
        name = "toml"
        file-types = ["toml"]
    "#,
        )
        .unwrap();

        let right: Value = toml::from_str(
            r#"
        [[language]]
        name = "rust"
        roots = ["Cargo.toml"]
        file-types = ["rs"]
    "#,
        )
        .unwrap();

        let merged = merge_toml_values(left, right, 5);
        let languages = merged.get("language").unwrap().as_array().unwrap();

        // Should have 2 languages: rust (merged) and toml (kept from left)
        assert_eq!(languages.len(), 2);

        let rust = languages
            .iter()
            .find(|l| l.get("name").unwrap().as_str() == Some("rust"))
            .unwrap();
        assert!(rust.get("file-types").is_some(), "file-types should exist");
        assert!(rust.get("roots").is_some(), "roots should exist");
        assert_eq!(
            rust.get("file-types").unwrap().as_array().unwrap()[0].as_str(),
            Some("rs")
        );
        assert_eq!(
            rust.get("roots").unwrap().as_array().unwrap()[0].as_str(),
            Some("Cargo.toml")
        );

        let toml_lang = languages
            .iter()
            .find(|l| l.get("name").unwrap().as_str() == Some("toml"))
            .unwrap();
        assert_eq!(
            toml_lang.get("file-types").unwrap().as_array().unwrap()[0].as_str(),
            Some("toml")
        );
    }

    #[test]
    fn test_merge_deep_nested_config() {
        let left = toml::from_str(
            r#"
        [language-server.vtsls.config.typescript.preferences]
        importModuleSpecifier = "relative"
        quoteStyle = "double"
        
        [language-server.vtsls.config.typescript.inlayHints]
        enabled = true
    "#,
        )
        .unwrap();

        let right = toml::from_str(
            r#"
        [language-server.vtsls.config.typescript.preferences]
        importModuleSpecifier = "non-relative"
    "#,
        )
        .unwrap();

        let merged = merge_toml_values(left, right, 5);
        let prefs = merged
            .get("language-server")
            .unwrap()
            .get("vtsls")
            .unwrap()
            .get("config")
            .unwrap()
            .get("typescript")
            .unwrap()
            .get("preferences")
            .unwrap();

        assert_eq!(
            prefs.get("importModuleSpecifier").unwrap().as_str(),
            Some("non-relative")
        );
        assert_eq!(prefs.get("quoteStyle").unwrap().as_str(), Some("double"));

        let hints = merged
            .get("language-server")
            .unwrap()
            .get("vtsls")
            .unwrap()
            .get("config")
            .unwrap()
            .get("typescript")
            .unwrap()
            .get("inlayHints")
            .unwrap();
        assert_eq!(hints.get("enabled").unwrap().as_bool(), Some(true));
    }

    #[test]
    fn test_merge_adds_new_keys() {
        let left = toml::from_str(
            r#"
        [server]
        command = "cmd"
    "#,
        )
        .unwrap();

        let right = toml::from_str(
            r#"
        [server]
        timeout = 60
        
        [client]
        name = "test"
    "#,
        )
        .unwrap();

        let merged = merge_toml_values(left, right, 5);

        assert_eq!(
            merged
                .get("server")
                .unwrap()
                .get("command")
                .unwrap()
                .as_str(),
            Some("cmd")
        );
        assert_eq!(
            merged
                .get("server")
                .unwrap()
                .get("timeout")
                .unwrap()
                .as_integer(),
            Some(60)
        );
        assert_eq!(
            merged.get("client").unwrap().get("name").unwrap().as_str(),
            Some("test")
        );
    }

    #[test]
    fn test_merge_primitives_replace() {
        let left = Value::String("old".to_string());
        let right = Value::String("new".to_string());

        let merged = merge_toml_values(left, right, 5);
        assert_eq!(merged.as_str(), Some("new"));
    }

    #[test]
    fn test_merge_environment_variables() {
        let left = toml::from_str(
            r#"
        [language-server.rust-analyzer.environment]
        RUST_BACKTRACE = "1"
    "#,
        )
        .unwrap();

        let right = toml::from_str(
            r#"
        [language-server.rust-analyzer.environment]
        RUST_LOG = "debug"
    "#,
        )
        .unwrap();

        let merged = merge_toml_values(left, right, 5);
        let env = merged
            .get("language-server")
            .unwrap()
            .get("rust-analyzer")
            .unwrap()
            .get("environment")
            .unwrap();

        assert_eq!(env.get("RUST_BACKTRACE").unwrap().as_str(), Some("1"));
        assert_eq!(env.get("RUST_LOG").unwrap().as_str(), Some("debug"));
    }

    #[test]
    fn test_deep_nested_array_with_name_should_replace() {
        // Test that deep nested arrays with 'name' fields are replaced, not merged
        // This is the key behavior controlled by merge_depth
        let left = toml::from_str(
            r#"
        [server.config.nested.deep.array]
        items = [
            { name = "a", value = 1 },
            { name = "b", value = 2 },
            { name = "c", value = 3 }
        ]
    "#,
        )
        .unwrap();

        let right = toml::from_str(
            r#"
        [server.config.nested.deep.array]
        items = [
            { name = "a", value = 99 }
        ]
    "#,
        )
        .unwrap();

        let merged = merge_toml_values(left, right, 5);
        let items = merged
            .get("server")
            .unwrap()
            .get("config")
            .unwrap()
            .get("nested")
            .unwrap()
            .get("deep")
            .unwrap()
            .get("array")
            .unwrap()
            .get("items")
            .unwrap()
            .as_array()
            .unwrap();

        // Should be replaced, not merged - only 'a' should exist
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].get("name").unwrap().as_str(), Some("a"));
        assert_eq!(items[0].get("value").unwrap().as_integer(), Some(99));
    }
}

#[test]
fn test_language_servers_should_replace() {
    // Test that language-servers array is replaced, not merged by name
    let left = toml::from_str(
        r#"
        [[language]]
        name = "typescript"
        file-types = ["ts"]
        language-servers = [
            { name = "vtsls", except-features = ["format"] },
            { name = "eslint" },
            { name = "old-server" }
        ]
    "#,
    )
    .unwrap();

    let right = toml::from_str(
        r#"
        [[language]]
        name = "typescript"
        language-servers = [
            { name = "vtsls", except-features = ["format"] },
            { name = "eslint" }
        ]
    "#,
    )
    .unwrap();

    let merged = merge_toml_values(left, right, 5);
    let languages = merged.get("language").unwrap().as_array().unwrap();
    let ts = languages
        .iter()
        .find(|l| l.get("name").unwrap().as_str() == Some("typescript"))
        .unwrap();
    let servers = ts.get("language-servers").unwrap().as_array().unwrap();

    // Should be replaced, not merged - only 2 servers, no old-server
    assert_eq!(servers.len(), 2);
    assert!(servers
        .iter()
        .any(|s| s.get("name").unwrap().as_str() == Some("vtsls")));
    assert!(servers
        .iter()
        .any(|s| s.get("name").unwrap().as_str() == Some("eslint")));
    assert!(!servers
        .iter()
        .any(|s| s.get("name").unwrap().as_str() == Some("old-server")));
}

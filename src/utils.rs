use lsp_types::{Diagnostic, DocumentSymbol, DocumentSymbolResponse, SymbolInformation, Url};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::{
    cmp::Ordering, path::{Path, PathBuf}, time::{SystemTime, UNIX_EPOCH}
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
        let a = canonicalize(&path_a);
        let b = canonicalize(&path_b);
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
        let a = canonicalize(&path_a);
        let b = canonicalize(&path_b);
        // let a = normalize(&path_a.expand_home().unwrap());
        // let b = normalize(&path_b.expand_home().unwrap());

        if !a.exists() || !b.exists() {
            return false;
        }

        return b.starts_with(a);
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

/// Find the closest root directory for a file.
/// This function searches upward from the file path to find the most specific
/// directory that contains any of the root markers, but doesn't go beyond the git workspace.
///
/// Returns the most specific (closest to file) root marker directory,
/// or the git workspace root if no markers are found.
pub fn find_lsp_workspace(doc_path: Option<&Path>, root_markers: &[String]) -> PathBuf {
    let Some(doc_path) = doc_path else {
        return current_working_dir();
    };

    let (git_workspace, _) = find_workspace_for_file(doc_path);
    let git_workspace = path::normalize(&git_workspace);
    let current_dir = doc_path.parent().unwrap_or(doc_path);

    current_dir
        .ancestors()
        .take_while(|ancestor| {
            // 包含 git_workspace 本身，但不超过它
            ancestor.starts_with(&git_workspace) || *ancestor == git_workspace
        })
        .find(|ancestor| {
            root_markers.iter().any(|marker| ancestor.join(marker).exists())
        })
        .map(|p| p.to_path_buf())
        .unwrap_or(git_workspace)
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
/// used. When a table exists in both `left` and `right`, the merged table
/// consists of all keys in `left`'s table unioned with all keys in `right`
/// with the values of `right` being merged recursively onto values of
/// `left`.
///
/// `merge_toplevel_arrays` controls whether a top-level array in the TOML
/// document is merged instead of overridden. This is useful for TOML
/// documents that use a top-level array of values like the `languages.toml`,
/// where one usually wants to override or add to the array instead of
/// replacing it altogether.
pub fn merge_toml_values(left: toml::Value, right: toml::Value, merge_depth: usize) -> toml::Value {
    use toml::Value;

    fn get_name(v: &Value) -> Option<&str> {
        v.get("name").and_then(Value::as_str)
    }

    match (left, right) {
        (Value::Array(mut left_items), Value::Array(right_items)) => {
            // The top-level arrays should be merged but nested arrays should
            // act as overrides. For the `languages.toml` config, this means
            // that you can specify a sub-set of languages in an overriding
            // `languages.toml` but that nested arrays like Language Server
            // arguments are replaced instead of merged.
            if merge_depth > 0 {
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
                Value::Array(right_items)
            }
        }
        (Value::Table(mut left_map), Value::Table(right_map)) => {
            if merge_depth > 0 {
                for (rname, rvalue) in right_map {
                    match left_map.remove(&rname) {
                        Some(lvalue) => {
                            let merged_value = merge_toml_values(lvalue, rvalue, merge_depth - 1);
                            left_map.insert(rname, merged_value);
                        }
                        None => {
                            left_map.insert(rname, rvalue);
                        }
                    }
                }
                Value::Table(left_map)
            } else {
                Value::Table(right_map)
            }
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
                    let children_entries = nested_symbols_to_imenu(&children);
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
            },
            _ => {
                // For a new name, save the current group and start a new group
                if !current_group.is_empty() {
                    groups.push(current_group);
                    current_group = Vec::new();
                }
                current_group.push((name.clone(), entry));
                last_name = Some(name);
            },
        }
    }

    if !current_group.is_empty() {
        groups.push(current_group);
    }

    for group in groups.iter_mut() {
        group.sort_by(|a, b| {
            match (&a.1, &b.1) {
                (ImenuEntry::Position(_), ImenuEntry::Group(_)) => Ordering::Less,
                (ImenuEntry::Group(_), ImenuEntry::Position(_)) => Ordering::Greater,
                _ => Ordering::Equal,
            }
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

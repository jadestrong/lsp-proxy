use lsp_types::{CodeAction, CodeActionOrCommand};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct CodeActionOrCommandItem {
    pub lsp_item: CodeActionOrCommand,
    pub language_server_id: usize,
    pub language_server_name: String,
}

pub fn action_category(action: &CodeActionOrCommand) -> u32 {
    if let CodeActionOrCommand::CodeAction(CodeAction {
        kind: Some(kind), ..
    }) = action
    {
        let mut components = kind.as_str().split('.');
        match components.next() {
            Some("quickfix") => 0,
            Some("refactor") => match components.next() {
                Some("extract") => 1,
                Some("inline") => 2,
                Some("rewrite") => 3,
                Some("move") => 4,
                Some("surround") => 5,
                _ => 7,
            },
            Some("source") => 6,
            _ => 7,
        }
    } else {
        7
    }
}

pub fn action_preferred(action: &CodeActionOrCommand) -> bool {
    matches!(
        action,
        CodeActionOrCommand::CodeAction(CodeAction {
            is_preferred: Some(true),
            ..
        })
    )
}

pub fn action_fixes_diagnostics(action: &CodeActionOrCommand) -> bool {
    matches!(
        action,
        CodeActionOrCommand::CodeAction(CodeAction {
            diagnostics: Some(diagnotics),
            ..
        }) if !diagnotics.is_empty()
    )
}

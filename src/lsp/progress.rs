//! Guard server-created progress against notifications arriving after `end`.
use lsp_types::{ProgressParams, ProgressParamsValue, ProgressToken, WorkDoneProgress};
use std::collections::{HashMap, HashSet, VecDeque};

const MAX_COMPLETED_PER_SERVER: usize = 256;

#[derive(Default)]
pub(crate) struct WorkDoneProgressTracker {
    created: HashSet<(usize, ProgressToken)>,
    completed: HashMap<usize, VecDeque<ProgressToken>>,
}

impl WorkDoneProgressTracker {
    pub(crate) fn create(&mut self, server_id: usize, token: ProgressToken) {
        if let Some(completed) = self.completed.get_mut(&server_id) {
            completed.retain(|old| old != &token);
        }
        self.created.insert((server_id, token));
    }

    pub(crate) fn should_forward(&mut self, server_id: usize, params: &ProgressParams) -> bool {
        let ProgressParamsValue::WorkDone(value) = &params.value;
        match value {
            WorkDoneProgress::End(_) => {
                if self.created.remove(&(server_id, params.token.clone())) {
                    let completed = self.completed.entry(server_id).or_default();
                    completed.push_back(params.token.clone());
                    if completed.len() > MAX_COMPLETED_PER_SERVER {
                        completed.pop_front();
                    }
                }
                true
            }
            WorkDoneProgress::Begin(_) | WorkDoneProgress::Report(_) => {
                // Spring Tools can send report/end while create is awaiting its
                // response, then send begin from the response callback. That late
                // begin must not resurrect the completed task. A new create resets
                // the token; client-supplied tokens (without create) are unaffected.
                !self
                    .completed
                    .get(&server_id)
                    .is_some_and(|completed| completed.contains(&params.token))
            }
        }
    }

    pub(crate) fn remove_server(&mut self, server_id: usize) {
        self.created.retain(|(id, _)| *id != server_id);
        self.completed.remove(&server_id);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn progress(token: ProgressToken, kind: &str) -> ProgressParams {
        let value = if kind == "begin" {
            json!({"kind":kind, "title":"Indexing", "percentage":0})
        } else {
            json!({"kind":kind})
        };
        serde_json::from_value(json!({"token":token,"value":value})).unwrap()
    }

    #[test]
    fn test_spring_end_before_begin_does_not_resurrect_progress() {
        let mut tracker = WorkDoneProgressTracker::default();
        let token = ProgressToken::String("index-java-source-files-task-funding-client-5".into());
        tracker.create(1, token.clone());
        for percentage in 1..=100 {
            let params = serde_json::from_value(json!({
                "token":token, "value":{"kind":"report","percentage":percentage}
            }))
            .unwrap();
            assert!(tracker.should_forward(1, &params));
        }
        assert!(tracker.should_forward(1, &progress(token.clone(), "end")));
        assert!(!tracker.should_forward(1, &progress(token.clone(), "begin")));
        assert!(!tracker.should_forward(1, &progress(token, "report")));
    }

    #[test]
    fn test_normal_progress_and_token_recreation() {
        let mut tracker = WorkDoneProgressTracker::default();
        let token = ProgressToken::Number(42);
        for _ in 0..2 {
            tracker.create(1, token.clone());
            for kind in ["begin", "report", "end"] {
                assert!(tracker.should_forward(1, &progress(token.clone(), kind)));
            }
            assert!(!tracker.should_forward(1, &progress(token.clone(), "begin")));
        }
    }

    #[test]
    fn test_servers_are_isolated_and_exit_clears_state() {
        let mut tracker = WorkDoneProgressTracker::default();
        let token = ProgressToken::Number(1);
        tracker.create(1, token.clone());
        tracker.create(2, token.clone());
        assert!(tracker.should_forward(1, &progress(token.clone(), "end")));
        assert!(tracker.should_forward(2, &progress(token.clone(), "begin")));
        tracker.remove_server(2);
        assert!(!tracker.should_forward(1, &progress(token.clone(), "begin")));
        tracker.remove_server(1);
        assert!(tracker.created.is_empty());
        assert!(tracker.completed.is_empty());
        assert!(tracker.should_forward(1, &progress(token, "begin")));
    }

    #[test]
    fn test_client_supplied_tokens_do_not_need_create_to_be_reused() {
        let mut tracker = WorkDoneProgressTracker::default();
        let token = ProgressToken::String("client-progress".into());
        for kind in ["report", "end", "begin", "report", "end"] {
            assert!(tracker.should_forward(1, &progress(token.clone(), kind)));
        }
        assert!(tracker.completed.is_empty());
    }

    #[test]
    fn test_completed_history_is_bounded_and_duplicate_end_is_harmless() {
        let mut tracker = WorkDoneProgressTracker::default();
        for id in 0..=MAX_COMPLETED_PER_SERVER as i32 {
            let token = ProgressToken::Number(id);
            tracker.create(1, token.clone());
            assert!(tracker.should_forward(1, &progress(token.clone(), "end")));
            assert!(tracker.should_forward(1, &progress(token, "end")));
        }
        assert!(tracker.created.is_empty());
        assert_eq!(tracker.completed[&1].len(), MAX_COMPLETED_PER_SERVER);
        assert!(tracker.should_forward(1, &progress(ProgressToken::Number(0), "begin")));
        assert!(!tracker.should_forward(1, &progress(ProgressToken::Number(1), "begin")));
    }
}

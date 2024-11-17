use crate::{
    fuzzy::{fuzzy::any_score, strings::compare_ignore_case},
    lsp_ext::CompletionItem,
};
use log::error;
use lsp_types::Position;
use std::cmp::Ordering;
use stringslice::StringSlice;

use super::fuzzy::{fuzzy_score, FuzzyScore, FuzzyScoreOptions};

pub struct MatchCompletionItem {
    origin_item: CompletionItem,
    score: FuzzyScore,
}

pub fn filter(
    pretext: &str,
    items: &Vec<CompletionItem>,
    position: Position,
    backup_prefix: &str,
) -> Vec<CompletionItem> {
    let mut arr: Vec<_> = vec![];
    for item in items {
        // guess prefix
        let start_character: u32;
        if let Some(text_edit) = &item.item.text_edit {
            match text_edit {
                lsp_types::CompletionTextEdit::Edit(edit) => {
                    start_character = edit.range.start.character;
                }
                lsp_types::CompletionTextEdit::InsertAndReplace(insert_replace_edit) => {
                    start_character = insert_replace_edit.replace.start.character;
                }
            }
        } else {
            // FIXME
            start_character = (pretext.chars().count() - backup_prefix.chars().count()) as u32;
        }

        let mut word = "";
        let mut word_low = String::new();
        // server return start.character maybe great than client position.character, some as input "<" in html/tsx
        let word_len = if position.character < start_character {
            error!("unexpect start_character {:?}", pretext);
            0
        } else {
            position.character - start_character
        };

        if word.chars().count() != (word_len as usize) {
            word = if word_len == 0 {
                ""
            } else {
                let range_start = pretext.chars().count() - (word_len as usize);
                pretext.slice(range_start..)
            };
            word_low = word.to_lowercase();
        }

        if word_len == 0 {
            arr.push(MatchCompletionItem {
                origin_item: item.clone(),
                score: FuzzyScore::default(),
            });
        } else {
            let mut word_pos: usize = 0;
            while word_pos < word_len as usize {
                let ch = word.chars().nth(word_pos as usize);
                if ch.is_none() {
                    error!("position {:?}", position);
                    error!("item {:?}", item.item);
                    error!("word = {:?}", word);
                    error!("unexpect word_pos {:?} word_len {:?}", word_pos, word_len);
                }
                let code = u32::from(ch.unwrap());
                if code == 32 || code == 9 {
                    word_pos += 1;
                } else {
                    break;
                }
            }

            if word_pos >= word_len as usize {
                arr.push(MatchCompletionItem {
                    origin_item: item.clone(),
                    score: FuzzyScore::default(),
                });
            } else if let Some(filter_text) = &item.item.filter_text {
                let score = fuzzy_score(
                    word,
                    &word_low,
                    word_pos,
                    filter_text,
                    &filter_text.to_lowercase(),
                    0,
                    FuzzyScoreOptions::default(),
                );
                if let Some(score) = score {
                    if compare_ignore_case(&filter_text, &item.item.label) == 0 {
                        arr.push(MatchCompletionItem {
                            score,
                            origin_item: item.clone(),
                        });
                    } else {
                        let label_low = item.item.label.to_lowercase();
                        let new_score =
                            any_score(word, &word_low, word_pos, &item.item.label, &label_low, 0);
                        arr.push(MatchCompletionItem {
                            origin_item: item.clone(),
                            score: FuzzyScore(score.0, new_score.1, new_score.2),
                        })
                    }
                } else {
                    continue;
                }
            } else {
                let score = fuzzy_score(
                    word,
                    &word_low,
                    word_pos,
                    &item.item.label,
                    &item.item.label.to_lowercase(),
                    0,
                    FuzzyScoreOptions::default(),
                );
                if let Some(score) = score {
                    arr.push(MatchCompletionItem {
                        score,
                        origin_item: item.clone(),
                    });
                } else {
                    continue;
                }
            }
        }
    }
    arr.sort_by(|a, b| {
        let (FuzzyScore(a0, a1, _), FuzzyScore(b0, b1, _)) = (&a.score, &b.score);
        match a0.cmp(&b0).reverse() {
            Ordering::Equal => match a1.cmp(&b1) {
                Ordering::Equal => {
                    let a_sort_text = a
                        .origin_item
                        .item
                        .sort_text
                        .as_ref()
                        .unwrap_or(&a.origin_item.item.label);
                    let b_sort_text = b
                        .origin_item
                        .item
                        .sort_text
                        .as_ref()
                        .unwrap_or(&b.origin_item.item.label);
                    a_sort_text.cmp(b_sort_text)
                }
                other => other,
            },
            other => other,
        }
    });
    // arr.into_iter()
    //     .map(|item| {
    //         let mut new_item = item.origin_item.clone();
    //         if let Some(text_edit) = &mut new_item.item.text_edit {
    //             if let lsp_types::CompletionTextEdit::InsertAndReplace(replace_text_edit) =
    //                 text_edit
    //             {
    //                 new_item.item.text_edit =
    //                     Some(lsp_types::CompletionTextEdit::Edit(lsp_types::TextEdit {
    //                         range: replace_text_edit.replace,
    //                         new_text: replace_text_edit.new_text.clone(),
    //                     }));
    //             }
    //         }
    //         new_item
    //     })
    //     .collect()
    arr.into_iter().map(|item| item.origin_item).collect()
}

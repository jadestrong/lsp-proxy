#[derive(Debug, PartialEq)]
pub struct IMatch {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct FuzzyScore(pub i32, pub usize, pub Vec<usize>);

impl Default for FuzzyScore {
    fn default() -> Self {
        Self(-100, 0, vec![])
    }
}

#[derive(Debug)]
pub struct FuzzyScoreOptions {
    pub first_match_can_be_weak: bool,
    pub boost_full_match: bool,
}

impl Default for FuzzyScoreOptions {
    fn default() -> Self {
        FuzzyScoreOptions {
            first_match_can_be_weak: false,
            boost_full_match: true,
        }
    }
}

// Define the create_matches function
#[allow(dead_code)]
fn create_matches(score: Option<FuzzyScore>) -> Vec<IMatch> {
    match score {
        None => vec![],
        Some(score) => {
            let mut res: Vec<IMatch> = vec![];
            let word_pos = score.1;
            let matches = score.2;
            for item in matches.iter().rev() {
                let pos = item + word_pos;
                if let Some(last) = res.last_mut() {
                    if last.end == pos {
                        last.end += 1;
                    } else {
                        res.push(IMatch {
                            start: pos,
                            end: pos + 1,
                        });
                    }
                } else {
                    res.push(IMatch {
                        start: pos,
                        end: pos + 1,
                    });
                }
            }

            // res.reverse(); // Since we built res backwards, reverse it before returning
            res
        }
    }
}

const MAX_LEN: usize = 128;
const MIN_SAFE_INTEGER: i32 = -10000;

type InitTable = [[i32; MAX_LEN + 1]; MAX_LEN + 1];

static mut MIN_WORD_MATCH_POS: [usize; 2 * MAX_LEN + 1] = [0; 2 * MAX_LEN + 1];
static mut MAX_WORD_MATCH_POS: [usize; 2 * MAX_LEN + 1] = [0; 2 * MAX_LEN + 1];
static mut _DIAG: InitTable = [[0; MAX_LEN + 1]; MAX_LEN + 1];
static mut _TABLE: InitTable = [[0; MAX_LEN + 1]; MAX_LEN + 1];
static mut _ARROWS: InitTable = [[0; MAX_LEN + 1]; MAX_LEN + 1];

fn _fill_in_max_word_match_pos(
    pattern_len: usize,
    word_len: usize,
    pattern_start: usize,
    word_start: usize,
    pattern_low: &str,
    word_low: &str,
) {
    let mut pattern_pos = if pattern_len > 0 { pattern_len - 1 } else { 0 };
    let mut word_pos = if word_len > 0 { word_len - 1 } else { 0 };

    while pattern_pos >= pattern_start && word_pos >= word_start {
        if pattern_low.chars().nth(pattern_pos) == word_low.chars().nth(word_pos) {
            unsafe {
                MAX_WORD_MATCH_POS[pattern_pos] = word_pos;
            }
            if pattern_pos == 0 {
                break; // Prevents underflow
            }
            pattern_pos -= 1;
        }
        if word_pos == 0 {
            break; // Prevents underflow
        }
        word_pos -= 1;
    }
}

fn is_pattern_in_word(
    pattern_low: &str,
    mut pattern_pos: usize,
    pattern_len: usize,
    word_low: &str,
    mut word_pos: usize,
    word_len: usize,
    fill_min_word_pos_arr: bool,
) -> bool {
    while pattern_pos < pattern_len && word_pos < word_len {
        if pattern_low.chars().nth(pattern_pos) == word_low.chars().nth(word_pos)
        {
            if fill_min_word_pos_arr {
                unsafe {
                    MIN_WORD_MATCH_POS[pattern_pos] = word_pos;
                }
            }
            pattern_pos += 1;
        }
        word_pos += 1;
    }
    pattern_pos == pattern_len // pattern must be exhausted
}

// Function to check if a character at a specific position is a separator
fn is_separator_at_pos(value: &str, index: usize) -> bool {
    if index > value.chars().count() {
        return false;
    }
    if let Some(ch) = value.chars().nth(index) {
        match ch {
            '_' | '-' | '.' | ' ' | '/' | '\\' | '\'' | '\"' | ':' | '$' | '<' | '>' | '('
            | ')' | '[' | ']' | '{' | '}' => true,
            _ => false,
        }
    } else {
        false
    }
}

// Function to check if a character at a specific position is whitespace
fn is_whitespace_at_pos(value: &str, index: usize) -> bool {
    if index > value.chars().count() {
        return false;
    }
    if let Some(ch) = value.chars().nth(index) {
        match ch {
            ' ' | '\t' => true,
            _ => false,
        }
    } else {
        false
    }
}

// Function to check if a character at a specific position in 'word' is uppercase
fn is_upper_case_at_pos(pos: usize, word: &str, word_low: &str) -> bool {
    if let (Some(ch), Some(ch_low)) = (
        word.chars().nth(pos),
        word_low.chars().nth(pos),
    ) {
        ch != ch_low && ch.to_ascii_uppercase() == ch
    } else {
        false
    }
}

fn _do_score(
    pattern: &str,
    pattern_low: &str,
    pattern_pos: usize,
    pattern_start: usize,
    word: &str,
    word_low: &str,
    word_pos: usize,
    word_len: usize,
    word_start: usize,
    new_match_start: bool,
    out_first_match_strong: &mut [bool],
) -> i32 {
    if pattern_low.chars().nth(pattern_pos) != word_low.chars().nth(word_pos) {
        return MIN_SAFE_INTEGER;
    }

    let mut score = 1;
    let mut is_gap_location = false;
    if word_pos == pattern_pos - pattern_start {
        // common prefix: `foobar <-> foobaz`
        score = if pattern.chars().nth(pattern_pos) == word.chars().nth(word_pos) {
            7
        } else {
            5
        };
    } else if is_upper_case_at_pos(word_pos, word, word_low)
        && (word_pos == 0 || !is_upper_case_at_pos(word_pos - 1, word, word_low))
    {
        // hitting upper-case: `foo <-> forOthers`
        score = if pattern.chars().nth(pattern_pos) == word.chars().nth(word_pos) {
            7
        } else {
            5
        };
        is_gap_location = true;
    } else if is_separator_at_pos(word_low, word_pos)
        && (word_pos == 0 || !is_separator_at_pos(word_low, word_pos - 1))
    {
        // hitting a separator: `. <-> foo.bar`
        score = 5;
    } else if word_pos > 0
        && (is_separator_at_pos(word_low, word_pos - 1)
            || is_whitespace_at_pos(word_low, word_pos - 1))
    {
        // hitting a separator: `. <-> foo.bar`
        score = 5;
        is_gap_location = true;
    }

    if score > 1 && pattern_pos == pattern_start {
        out_first_match_strong[0] = true;
    }

    if !is_gap_location {
        is_gap_location = is_upper_case_at_pos(word_pos, word, word_low)
            || (word_pos > 0 && is_separator_at_pos(word_low, word_pos - 1))
            || (word_pos > 0 && is_whitespace_at_pos(word_low, word_pos - 1));
    }

    if pattern_pos == pattern_start {
        if word_pos > word_start {
            score -= if is_gap_location { 3 } else { 5 };
        }
    } else if new_match_start {
        score += if is_gap_location { 2 } else { 0 };
    } else {
        score += if is_gap_location { 0 } else { 1 };
    }

    if word_pos + 1 == word_len {
        score -= if is_gap_location { 3 } else { 5 };
    }
    score
}

pub fn fuzzy_score(
    pattern: &str,
    pattern_low: &str,
    pattern_start: usize,
    word: &str,
    word_low: &str,
    word_start: usize,
    options: FuzzyScoreOptions,
) -> Option<FuzzyScore> {
    let pattern_len = if pattern.chars().count() > MAX_LEN {
        MAX_LEN
    } else {
        pattern.chars().count()
    };
    let word_len = if word.chars().count() > MAX_LEN {
        MAX_LEN
    } else {
        word.chars().count()
    };
    if pattern_start >= pattern_len
        || word_start >= word_len
        || (pattern_len - pattern_start) > (word_len - word_start)
    {
        return None;
    }

    if !is_pattern_in_word(
        pattern_low,
        pattern_start,
        pattern_len,
        word_low,
        word_start,
        word_len,
        true,
    ) {
        return None;
    }

    _fill_in_max_word_match_pos(
        pattern_len,
        word_len,
        pattern_start,
        word_start,
        pattern_low,
        word_low,
    );

    let mut row: usize = 1;
    let mut column: usize = 1;

    let mut has_strong_first_match = [false];

    for pattern_pos in pattern_start..pattern_len {
        row = pattern_pos - pattern_start + 1;
        let min_word_match_pos = unsafe { MIN_WORD_MATCH_POS[pattern_pos] };
        let max_word_match_pos = unsafe { MAX_WORD_MATCH_POS[pattern_pos] };
        let next_max_word_match_pos = if pattern_pos + 1 < pattern_len {
            unsafe { MAX_WORD_MATCH_POS[pattern_pos + 1] }
        } else {
            word_len
        };

        for word_pos in min_word_match_pos..next_max_word_match_pos {
            column = word_pos - word_start + 1;

            let mut score = MIN_SAFE_INTEGER;

            // 当前至该字符最后一次出现的位置之间
            if word_pos <= max_word_match_pos {
                score = _do_score(
                    pattern,
                    pattern_low,
                    pattern_pos,
                    pattern_start,
                    word,
                    word_low,
                    word_pos,
                    word_len,
                    word_start,
                    unsafe { _DIAG[row - 1][column - 1] == 0 },
                    &mut has_strong_first_match,
                );
            }

            let can_come_diag = true;
            let diag_score = score + unsafe { _TABLE[row - 1][column - 1] };

            let can_come_left = word_pos > min_word_match_pos;
            let left_score = if can_come_left {
                let tl = unsafe { _TABLE[row][column - 1] };
                let rd = if unsafe { _DIAG[row][column - 1] } > 0 {
                    -5
                } else {
                    0
                };
                tl + rd
            } else {
                0
            };
            let can_come_left_left =
                word_pos > min_word_match_pos + 1 && unsafe { _DIAG[row][column - 1] > 0 };
            let left_left_score = if can_come_left_left {
                let tl = unsafe { _TABLE[row][column - 2] };
                let dr = unsafe {
                    if _DIAG[row][column - 2] > 0 {
                        -5
                    } else {
                        0
                    }
                };
                tl + dr
            } else {
                0
            };

            if can_come_left_left
                && (left_left_score >= left_score)
                && (left_left_score >= diag_score)
            {
                unsafe {
                    _TABLE[row][column] = left_left_score;
                    _ARROWS[row][column] = 3;
                    _DIAG[row][column] = 0;
                };
            } else if can_come_left && (left_score >= diag_score) {
                unsafe {
                    _TABLE[row][column] = left_score;
                    _ARROWS[row][column] = 2;
                    _DIAG[row][column] = 0;
                };
            } else if can_come_diag {
                unsafe {
                    _TABLE[row][column] = diag_score;
                    _ARROWS[row][column] = 1;
                    _DIAG[row][column] = _DIAG[row - 1][column - 1] + 1;
                };
            } else {
                panic!("not possible");
            }
        }
    }
    if !has_strong_first_match[0] && !options.first_match_can_be_weak {
        return None;
    }

    let mut result = unsafe { FuzzyScore(_TABLE[row][column], word_start, vec![]) };

    let mut backwards_diag_length = 0;
    let mut max_match_column = 0;

    while row >= 1 {
        let mut diag_column = column;
        loop {
            unsafe {
                match _ARROWS[row][diag_column] {
                    3 => diag_column -= 2,
                    2 => diag_column -= 1,
                    _ => break,
                }
            }
        }
        if backwards_diag_length > 1
            && pattern_low.chars().nth(pattern_start + row - 1)
                == word_low.chars().nth(word_start + column - 1)
            && !is_upper_case_at_pos(diag_column + word_start - 1, word, word_low)
            && backwards_diag_length + 1 > unsafe { _DIAG[row][diag_column] }
        {
            diag_column = column;
        }

        if diag_column == column {
            backwards_diag_length += 1;
        } else {
            backwards_diag_length = 1;
        }

        if max_match_column == 0 {
            max_match_column = diag_column;
        }

        row -= 1;
        column = diag_column - 1;
        result.2.push(column);
    }

    if word_len == pattern_len && options.boost_full_match {
        result.0 += 2;
    }

    let skipped_chars_count = max_match_column - pattern_len;
    result.0 -= skipped_chars_count as i32;
    Some(result)
}

// Assuming fuzzy_score and create_matches functions are defined elsewhere
#[allow(dead_code)]
pub fn matches_fuzzy(pattern: &str, word: &str) -> Option<Vec<IMatch>> {
    let score = fuzzy_score(
        pattern,
        &pattern.to_lowercase(),
        0,
        word,
        &word.to_lowercase(),
        0,
        FuzzyScoreOptions {
            first_match_can_be_weak: true,
            boost_full_match: true,
        },
    );
    score.map(|score| create_matches(Some(score)))
}

// Assuming fuzzy_score function is defined elsewhere
pub fn any_score(
    pattern: &str,
    low_pattern: &str,
    mut pattern_pos: usize,
    word: &str,
    low_word: &str,
    word_pos: usize,
) -> FuzzyScore {
    let max = std::cmp::min(13, pattern.chars().count());
    while pattern_pos < max {
        let result = fuzzy_score(
            pattern,
            low_pattern,
            pattern_pos,
            word,
            low_word,
            word_pos,
            FuzzyScoreOptions {
                first_match_can_be_weak: true,
                boost_full_match: true,
            },
        );
        if let Some(result) = result {
            return result;
        }
        pattern_pos += 1;
    }
    FuzzyScore(0, word_pos, vec![])
}

#[cfg(test)]
mod test {
    use super::*;

    struct FilterOptions {
        pattern_pos: Option<usize>,
        word_pos: Option<usize>,
        first_match_can_be_weak: Option<bool>,
    }

    fn assert_matches<F>(
        pattern: &str,
        word: &str,
        decorated_word: Option<&str>,
        filter: F,
        opts: Option<&FilterOptions>,
    ) where
        F: Fn(&str, &str, usize, &str, &str, usize, FuzzyScoreOptions) -> Option<FuzzyScore>,
    {
        let pattern_pos = opts.map_or(0, |o| o.pattern_pos.unwrap_or(0));
        let word_pos = opts.map_or(0, |o| o.word_pos.unwrap_or(0));
        let first_match_can_be_weak =
            opts.is_some_and(|o| o.first_match_can_be_weak.unwrap_or(false));

        let filter_opts = FuzzyScoreOptions {
            first_match_can_be_weak,
            boost_full_match: true,
        };

        let result = filter(
            pattern,
            &pattern.to_lowercase(),
            pattern_pos,
            word,
            &word.to_lowercase(),
            word_pos,
            filter_opts,
        );

        assert_eq!(decorated_word.is_some(), result.is_some());

        let matches = create_matches(result);
        let mut actual_word = String::new();
        let mut pos = 0;
        for m in matches {
            actual_word.push_str(&word[pos..m.start]);
            actual_word.push('^');
            let chars: &Vec<_> = &word[m.start..m.end].chars().collect();
            // let char_vec: Vec<char> = chars.collect();
            let joined_word: String =
                itertools::Itertools::intersperse(chars.clone().into_iter(), '^').collect();
            actual_word.push_str(&joined_word);
            pos = m.end;
        }

        actual_word.push_str(&word[pos..]);
        assert_eq!(actual_word, decorated_word.unwrap());
    }

    #[test]
    fn test_fuzzy_score_23215() {
        assert_matches("tit", "win.tit", Some("win.^t^i^t"), fuzzy_score, None);
        assert_matches(
            "title",
            "win.title",
            Some("win.^t^i^t^l^e"),
            fuzzy_score,
            None,
        );
        assert_matches(
            "WordCla",
            "WordCharacterClassifier",
            Some("^W^o^r^dCharacter^C^l^assifier"),
            fuzzy_score,
            None,
        );
        assert_matches(
            "WordCCla",
            "WordCharacterClassifier",
            Some("^W^o^r^d^Character^C^l^assifier"),
            fuzzy_score,
            None,
        );
    }
}

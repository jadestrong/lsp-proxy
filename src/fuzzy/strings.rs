use stringslice::StringSlice;

pub fn compare_ignore_case(a: &str, b: &str) -> i32 {
    compare_substring_ignore_case(a, b, 0, a.chars().count(), 0, b.chars().count())
}

fn compare_substring(
    a: &str,
    b: &str,
    a_start: usize,
    a_end: usize,
    b_start: usize,
    b_end: usize,
) -> i32 {
    let a_slice = &a.slice(a_start..a_end);
    let b_slice = &b.slice(b_start..b_end);
    for (ch_a, ch_b) in a_slice.chars().zip(b_slice.chars()) {
        let code_a = ch_a as u32;
        let code_b = ch_b as u32;
        if code_a < code_b {
            return -1;
        } else if code_a > code_b {
            return 1;
        }
    }

    let a_len = a_end - a_start;
    let b_len = b_end - b_start;
    if a_len < b_len {
        return -1;
    } else if a_len > b_len {
        return 1;
    }
    return 0;
}

fn compare_substring_ignore_case(
    a: &str,
    b: &str,
    a_start: usize,
    a_end: usize,
    b_start: usize,
    b_end: usize,
) -> i32 {
    let a_slice = a.slice(a_start..a_end);
    let b_slice = b.slice(b_start..b_end);
    for (ch_a, ch_b) in a_slice.chars().zip(b_slice.chars()) {
        let mut code_a = u32::from(ch_a);
        let mut code_b = u32::from(ch_b);
        if code_a == code_b {
            continue;
        }

        if code_a >= 128 || code_b >= 128 {
            let a_low = a.to_lowercase();
            let b_low = b.to_lowercase();
            return compare_substring(&a_low, &b_low, a_start, a_end, b_start, b_end);
        }

        if is_lower_ascii_letter(code_a) {
            code_a -= 32;
        }
        if is_lower_ascii_letter(code_b) {
            code_b -= 32;
        }

        let diff = code_a - code_b;
        if diff == 0 {
            continue;
        }

        return diff as i32;
    }
    let a_len = a_end - a_start;
    let b_len = b_end - b_start;
    if a_len < b_len {
        return -1;
    } else if a_len > b_len {
        return 1;
    }
    return 0;
}

fn is_lower_ascii_letter(code: u32) -> bool {
    code >= 97 && code <= 122
}

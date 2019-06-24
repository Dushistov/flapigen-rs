pub(crate) fn replace_first_and_other(src: &str, from: &str, first: &str, other: &str) -> String {
    let mut result = String::new();
    let mut last_end = 0;
    let mut matched = false;
    for (start, part) in src.match_indices(from) {
        result.push_str(unsafe { src.get_unchecked(last_end..start) });
        if matched {
            result.push_str(other);
        } else {
            result.push_str(first);
            matched = true;
        }
        last_end = start + part.len();
    }
    result.push_str(unsafe { src.get_unchecked(last_end..src.len()) });
    result
}

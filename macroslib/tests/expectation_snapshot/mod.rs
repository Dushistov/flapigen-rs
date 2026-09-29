use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::Path,
};

use serde_json::Value;
use snapbox::{assert::Action, data::DataFormat, Data};

const START: &str = "@@expect ";
const END: &str = "\n@@end";

#[derive(Debug)]
struct Section {
    header: String,
    expected: String,
    selector: Selector,
}

#[derive(Debug)]
enum Selector {
    File {
        file: String,
    },
    Between {
        file: String,
        before: String,
        after: String,
        greedy_match: bool,
    },
    Item {
        file: String,
        name: String,
        form: String,
    },
}

impl Selector {
    fn file(&self) -> &str {
        match self {
            Self::File { file } | Self::Between { file, .. } | Self::Item { file, .. } => file,
        }
    }
}

fn required_string<'a>(value: &'a Value, field: &str) -> Result<&'a str, String> {
    value
        .get(field)
        .and_then(Value::as_str)
        .ok_or_else(|| format!("missing string field {field:?}"))
}

fn optional_string<'a>(value: &'a Value, field: &str) -> Result<&'a str, String> {
    match value.get(field) {
        Some(value) => value
            .as_str()
            .ok_or_else(|| format!("field {field:?} must be a string")),
        None => Ok(""),
    }
}

fn parse_selector(header: &str) -> Result<Selector, String> {
    let value: Value = serde_json::from_str(header).map_err(|err| err.to_string())?;
    if value.get("after_last").is_some() {
        return Err("after_last was renamed to greedy_match".into());
    }
    let file = required_string(&value, "file")?.to_owned();
    // is_separator is platform-specific; reject both separators in snapshots on every OS.
    if file.is_empty() || file.contains('/') || file.contains('\\') {
        return Err(format!("invalid generated filename {file:?}"));
    }
    match required_string(&value, "kind")? {
        "file" => Ok(Selector::File { file }),
        "between" => {
            let before = optional_string(&value, "before")?.to_owned();
            let after = optional_string(&value, "after")?.to_owned();
            let greedy_match = match value.get("greedy_match") {
                Some(value) => value
                    .as_bool()
                    .ok_or_else(|| "field \"greedy_match\" must be a boolean".to_owned())?,
                None => false,
            };
            if before.is_empty() && after.is_empty() {
                return Err("between selector needs at least one boundary".into());
            }
            Ok(Selector::Between {
                file,
                before,
                after,
                greedy_match,
            })
        }
        "item" => {
            let name = required_string(&value, "name")?.to_owned();
            let form = required_string(&value, "form")?.to_owned();
            if name.is_empty() || !matches!(form.as_str(), "declaration" | "definition") {
                return Err("item requires a name and declaration/definition form".into());
            }
            Ok(Selector::Item { file, name, form })
        }
        kind => Err(format!("unknown selector kind {kind:?}")),
    }
}

fn parse_sections(input: &str) -> Result<Vec<Section>, String> {
    let mut rest = input;
    let mut sections = Vec::new();
    while !rest.is_empty() {
        rest = rest.trim_start_matches('\n');
        if rest.is_empty() {
            break;
        }
        let header_line = rest
            .strip_prefix(START)
            .ok_or_else(|| "expected an @@expect header".to_owned())?;
        let (header, following) = header_line
            .split_once('\n')
            .ok_or_else(|| "unterminated @@expect header".to_owned())?;
        let selector = parse_selector(header)?;
        let end = following
            .find(END)
            .ok_or_else(|| "missing @@end marker".to_owned())?;
        let expected = following[..end].to_owned();
        rest = &following[end + END.len()..];
        if !rest.is_empty() {
            rest = rest
                .strip_prefix('\n')
                .ok_or_else(|| "expected newline after @@end".to_owned())?;
        }
        sections.push(Section {
            header: header.to_owned(),
            expected,
            selector,
        });
    }
    if sections.is_empty() {
        return Err("expectation file has no sections".into());
    }
    Ok(sections)
}

fn render_sections(sections: &[Section], actual: &[String]) -> String {
    let mut output = String::new();
    for (index, (section, body)) in sections.iter().zip(actual).enumerate() {
        if index > 0 {
            output.push('\n');
        }
        output.push_str(START);
        output.push_str(&section.header);
        output.push('\n');
        output.push_str(body);
        output.push_str(END);
        output.push('\n');
    }
    output
}

fn select_between<'a>(
    code: &'a str,
    before: &str,
    after: &str,
    greedy_match: bool,
) -> Result<&'a str, String> {
    let starts: Vec<usize> = if before.is_empty() {
        vec![0]
    } else {
        code.match_indices(before)
            .map(|(offset, _)| offset + before.len())
            .collect()
    };
    let mut matches = Vec::new();
    for start in starts {
        let end = if after.is_empty() {
            Some(code.len())
        } else if greedy_match {
            code[start..].rfind(after).map(|offset| start + offset)
        } else {
            code[start..].find(after).map(|offset| start + offset)
        };
        if let Some(end) = end {
            matches.push(&code[start..end]);
        }
    }
    match matches.as_slice() {
        [found] => Ok(found),
        [] => Err("selector did not match generated code".into()),
        _ => Err(format!("selector matched {} locations", matches.len())),
    }
}

fn is_identifier_byte(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
}

// Keep byte offsets intact while hiding delimiters in comments and string literals.
fn mask_non_code(code: &str) -> Vec<u8> {
    let input = code.as_bytes();
    let mut masked = input.to_vec();
    let mut index = 0;
    while index < input.len() {
        let quote = match (input[index], input.get(index + 1)) {
            (b'/', Some(b'/')) => {
                let start = index;
                index += 2;
                while index < input.len() && input[index] != b'\n' {
                    index += 1;
                }
                masked[start..index].fill(b' ');
                continue;
            }
            (b'/', Some(b'*')) => {
                let start = index;
                index += 2;
                while index + 1 < input.len() && &input[index..index + 2] != b"*/" {
                    index += 1;
                }
                index = (index + 2).min(input.len());
                for byte in &mut masked[start..index] {
                    if *byte != b'\n' {
                        *byte = b' ';
                    }
                }
                continue;
            }
            (b'"', _) => b'"',
            (b'\'', _) => {
                let next_line = input[index + 1..]
                    .iter()
                    .position(|byte| *byte == b'\n')
                    .map_or(input.len(), |offset| index + 1 + offset);
                let nearby_end = (index + 8).min(next_line);
                let closing = input[index + 1..nearby_end]
                    .iter()
                    .position(|byte| *byte == b'\'')
                    .map(|offset| index + 1 + offset);
                let is_char = closing.is_some_and(|end| {
                    let contents = &code[index + 1..end];
                    contents.starts_with('\\') || contents.chars().count() == 1
                });
                if !is_char {
                    // A Rust lifetime such as 'a is not a character literal.
                    index += 1;
                    continue;
                }
                b'\''
            }
            _ => {
                index += 1;
                continue;
            }
        };
        let start = index;
        index += 1;
        while index < input.len() {
            if input[index] == b'\\' {
                index = (index + 2).min(input.len());
            } else if input[index] == quote {
                index += 1;
                break;
            } else {
                index += 1;
            }
        }
        for byte in &mut masked[start..index] {
            if *byte != b'\n' {
                *byte = b' ';
            }
        }
    }
    masked
}

fn item_end(masked: &[u8], from: usize, form: &str) -> Option<usize> {
    let mut paren = 0usize;
    let mut square = 0usize;
    for index in from..masked.len() {
        match masked[index] {
            b'(' => paren += 1,
            b')' => paren = paren.saturating_sub(1),
            b'[' => square += 1,
            b']' => square = square.saturating_sub(1),
            b';' if paren == 0 && square == 0 => {
                return (form == "declaration").then_some(index + 1);
            }
            b'{' if paren == 0 && square == 0 => {
                if form != "definition" {
                    return None;
                }
                let mut depth = 1usize;
                for end in index + 1..masked.len() {
                    match masked[end] {
                        b'{' => depth += 1,
                        b'}' => {
                            depth -= 1;
                            if depth == 0 {
                                return Some(if masked.get(end + 1) == Some(&b';') {
                                    end + 2
                                } else {
                                    end + 1
                                });
                            }
                        }
                        _ => {}
                    }
                }
                return None;
            }
            _ => {}
        }
    }
    None
}

fn select_item<'a>(code: &'a str, name: &str, form: &str) -> Result<&'a str, String> {
    // The generated languages share enough declaration syntax for this small
    // scanner. An explicit between selector handles exceptional constructs.
    let masked = mask_non_code(code);
    let mut matches = Vec::new();
    for (name_at, _) in code.match_indices(name) {
        let name_end = name_at + name.len();
        if masked[name_at..name_end] != code.as_bytes()[name_at..name_end] {
            continue;
        }
        let before = code.as_bytes().get(name_at.wrapping_sub(1)).copied();
        let after = code.as_bytes().get(name_end).copied();
        if before.is_some_and(is_identifier_byte) || after.is_some_and(is_identifier_byte) {
            continue;
        }
        let mut item_start = code[..name_at].rfind('\n').map_or(0, |idx| idx + 1);
        while item_start > 0 {
            let previous_start = code[..item_start - 1].rfind('\n').map_or(0, |idx| idx + 1);
            let previous = code[previous_start..item_start].trim();
            if previous.starts_with("template<")
                || previous.starts_with("#[")
                || previous.starts_with('@')
            {
                item_start = previous_start;
            } else {
                break;
            }
        }
        item_start += code[item_start..]
            .bytes()
            .take_while(|byte| *byte == b' ' || *byte == b'\t')
            .count();
        if let Some(end) = item_end(&masked, name_end, form) {
            matches.push(&code[item_start..end]);
        }
    }
    match matches.as_slice() {
        [found] => Ok(found),
        [] => Err(format!("item {name:?} ({form}) was not found")),
        _ => Err(format!(
            "item {name:?} ({form}) matched {} locations",
            matches.len()
        )),
    }
}

fn select<'a>(selector: &Selector, files: &'a BTreeMap<String, String>) -> Result<&'a str, String> {
    let filename = selector.file();
    let code = files
        .get(filename)
        .ok_or_else(|| format!("generated file {filename:?} was not found"))?;
    match selector {
        Selector::File { .. } => Ok(code),
        Selector::Between {
            before,
            after,
            greedy_match,
            ..
        } => select_between(code, before, after, *greedy_match),
        Selector::Item { name, form, .. } => select_item(code, name, form),
    }
}

fn generated_file_dump(name: &str, files: &BTreeMap<String, String>) -> String {
    match files.get(name) {
        Some(code) => format!("\n--- generated file: {name} ---\n{code}\n--- end of {name} ---"),
        None => {
            let mut dump = format!("\nGenerated file {name:?} was not found. Available files:");
            for (available_name, code) in files {
                dump.push_str(&format!(
                    "\n--- generated file: {available_name} ---\n{code}\n--- end of {available_name} ---"
                ));
            }
            dump
        }
    }
}

pub(crate) fn check(path: &Path, files: &BTreeMap<String, String>) -> Result<(), String> {
    let action = if std::env::var("UPDATE_EXPECT").as_deref() == Ok("1") {
        Action::Overwrite
    } else {
        Action::Verify
    };
    check_with_action(path, files, action)
}

fn check_with_action(
    path: &Path,
    files: &BTreeMap<String, String>,
    action: Action,
) -> Result<(), String> {
    let source = fs::read_to_string(path).map_err(|err| err.to_string())?;
    let source = source.replace("\r\n", "\n");
    let sections = parse_sections(&source).map_err(|err| {
        let files_dump = files
            .keys()
            .map(|file| generated_file_dump(file, files))
            .collect::<String>();
        format!("{}: {err}{files_dump}", path.display())
    })?;
    let mut actual = Vec::with_capacity(sections.len());
    let mut mismatched_files = BTreeSet::new();
    for (index, section) in sections.iter().enumerate() {
        let file = section.selector.file();
        let body = select(&section.selector, files).map_err(|err| {
            format!(
                "{} section {}: {err}{}",
                path.display(),
                index + 1,
                generated_file_dump(file, files)
            )
        })?;
        if body != section.expected {
            mismatched_files.insert(file);
        }
        actual.push(body.to_owned());
    }
    let actual = render_sections(&sections, &actual);
    let assertion = snapbox::Assert::new().action(action);
    assertion
        .try_eq(
            Some(&"Generated code"),
            Data::text(actual),
            Data::read_from(path, Some(DataFormat::Text)).raw(),
        )
        .map_err(|err| {
            if mismatched_files.is_empty() {
                mismatched_files.extend(sections.iter().map(|section| section.selector.file()));
            }
            let files_dump = mismatched_files
                .iter()
                .map(|file| generated_file_dump(file, files))
                .collect::<String>();
            format!("{err}{files_dump}")
        })?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn between_preserves_partial_fragment() {
        let code = "before void f1(std::optional<double> x); after";
        assert_eq!(
            select_between(code, "before ", " x); after", false).unwrap(),
            "void f1(std::optional<double>"
        );
    }

    #[test]
    fn omitted_boundaries_mean_file_edges() {
        let files = BTreeMap::from([("A.h".to_owned(), "prefix!suffix".to_owned())]);
        let from_start = parse_selector(r#"{"file":"A.h","kind":"between","after":"!"}"#).unwrap();
        let to_end = parse_selector(r#"{"file":"A.h","kind":"between","before":"!"}"#).unwrap();
        assert_eq!(select(&from_start, &files).unwrap(), "prefix");
        assert_eq!(select(&to_end, &files).unwrap(), "suffix");
        assert!(parse_selector(r#"{"file":"A.h","kind":"between"}"#).is_err());
    }

    #[test]
    fn greedy_match_uses_last_end_boundary() {
        let files = BTreeMap::from([("A.h".to_owned(), "first|middle|last".to_owned())]);
        let normal = parse_selector(r#"{"file":"A.h","kind":"between","after":"|"}"#).unwrap();
        let greedy =
            parse_selector(r#"{"file":"A.h","kind":"between","after":"|","greedy_match":true}"#)
                .unwrap();
        assert_eq!(select(&normal, &files).unwrap(), "first");
        assert_eq!(select(&greedy, &files).unwrap(), "first|middle");
        assert!(
            parse_selector(r#"{"file":"A.h","kind":"between","after":"|","after_last":true}"#)
                .is_err()
        );
    }

    #[test]
    fn missing_and_ambiguous_boundaries_fail() {
        assert!(select_between("abcdef", "missing", "f", false).is_err());
        assert!(select_between("xAy xBz xCy", "x", "y", false).is_err());
    }

    #[test]
    fn snapshot_format_round_trip() {
        let input = "@@expect {\"file\":\"Foo.hpp\",\"kind\":\"between\",\"before\":\"A\",\"after\":\"Z\"}\nB\n@@end\n";
        let sections = parse_sections(input).unwrap();
        assert_eq!(
            render_sections(&sections, &[sections[0].expected.clone()]),
            input
        );
    }

    #[test]
    fn item_selector_ignores_comments_and_string_delimiters() {
        let code =
            "// target() {; }\nvoid target() { const char *s = \"};\"; if (true) { run(); } }\n";
        assert_eq!(
            select_item(code, "target", "definition").unwrap(),
            "void target() { const char *s = \"};\"; if (true) { run(); } }"
        );
        assert_eq!(
            select_item("void target();\n", "target", "declaration").unwrap(),
            "void target();"
        );
        assert_eq!(
            select_item(
                "fn target<'a>(x: &'a str) { use_it(x); }\n",
                "target",
                "definition"
            )
            .unwrap(),
            "fn target<'a>(x: &'a str) { use_it(x); }"
        );
    }

    #[test]
    fn overwrite_updates_only_selected_body() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("sample.cpp");
        let header =
            "@@expect {\"file\":\"A.h\",\"kind\":\"between\",\"before\":\"<\",\"after\":\">\"}";
        fs::write(&path, format!("{header}\nold\n@@end\n")).unwrap();
        let files = BTreeMap::from([("A.h".to_owned(), "<new>".to_owned())]);
        check_with_action(&path, &files, Action::Overwrite).unwrap();
        assert_eq!(
            fs::read_to_string(&path).unwrap(),
            format!("{header}\nnew\n@@end\n")
        );
        check_with_action(&path, &files, Action::Verify).unwrap();
    }

    #[test]
    fn failed_expectations_show_complete_generated_file() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("sample.cpp");
        let code = "first line\nsecond line\nlast line\n";
        let files = BTreeMap::from([("Foo.hpp".to_owned(), code.to_owned())]);

        fs::write(
            &path,
            "@@expect {\"file\":\"Foo.hpp\",\"kind\":\"between\",\"after\":\"BUNG\"}\nwrong\n@@end\n",
        )
        .unwrap();
        let selector_error = check_with_action(&path, &files, Action::Verify).unwrap_err();
        assert!(selector_error.contains("selector did not match"));
        assert!(selector_error.contains("Foo.hpp"));
        assert!(selector_error.contains(code));

        fs::write(
            &path,
            "@@expect {\"file\":\"Foo.hpp\",\"kind\":\"file\"}\nwrong\n@@end\n",
        )
        .unwrap();
        let mismatch_error = check_with_action(&path, &files, Action::Verify).unwrap_err();
        assert!(mismatch_error.contains("Foo.hpp"));
        assert!(mismatch_error.contains(code));
    }

    #[test]
    fn crlf_expectation_matches_lf_output() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("sample.cpp");
        fs::write(
            &path,
            b"@@expect {\"file\":\"A.h\",\"kind\":\"file\"}\r\nline\r\n@@end\r\n",
        )
        .unwrap();
        let files = BTreeMap::from([("A.h".to_owned(), "line".to_owned())]);
        check_with_action(&path, &files, Action::Verify).unwrap();
    }
}

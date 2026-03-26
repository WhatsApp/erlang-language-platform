/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Parser for Erlang format strings (io:format/io_lib:format style).
//!
//! Parses format specifiers like `~p`, `~.2f`, `~-10s`, `~*W`, etc.
//! from the raw source text of a string literal. Byte offsets in the
//! returned ranges map directly to source positions (relative to the
//! start of the string content, after the opening quote).
//!
//! Ported from `erlang_service/src/elp_lint.erl` `extract_sequences`,
//! and MUST be kept in sync with it.

// @fb-only: mod meta_only;

use std::ops::Range;

use elp_syntax::TextRange;
use elp_syntax::TextSize;

/// A single format specifier parsed from a format string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatSpecifier {
    /// Byte range of the entire specifier within the raw string content
    /// (from `~` through the control character, inclusive).
    pub range: Range<usize>,
    /// The control character (e.g., 'p', 's', 'w', 'W', 'n', '~').
    pub control: char,
    /// How many arguments this specifier consumes from the arg list.
    pub args_consumed: usize,
}

/// Result of parsing a format string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedFormatString {
    /// All format specifiers found, in order.
    pub specifiers: Vec<FormatSpecifier>,
    /// Maps argument index to specifier index.
    /// For each argument consumed (in order), which specifier it belongs to.
    pub arg_to_specifier: Vec<usize>,
}

/// Parse an Erlang format string from raw source text (the content between quotes,
/// not including the quotes themselves).
///
/// Returns `None` if the format string is malformed.
pub fn parse_format_string(raw_content: &str) -> Option<ParsedFormatString> {
    let bytes = raw_content.as_bytes();
    let len = bytes.len();
    let mut pos = 0;
    let mut specifiers = Vec::new();

    while pos < len {
        if bytes[pos] == b'\\' {
            // Skip escape sequence
            pos += 1;
            if pos < len {
                pos += 1;
            }
            continue;
        }
        if bytes[pos] == b'~' {
            let start = pos;
            pos += 1; // skip '~'
            let (spec, new_pos) = parse_specifier(bytes, pos, start)?;
            specifiers.push(spec);
            pos = new_pos;
        } else {
            pos += 1;
        }
    }

    // Build arg_to_specifier mapping
    let mut arg_to_specifier = Vec::new();
    for (i, spec) in specifiers.iter().enumerate() {
        for _ in 0..spec.args_consumed {
            arg_to_specifier.push(i);
        }
    }

    Some(ParsedFormatString {
        specifiers,
        arg_to_specifier,
    })
}

/// Parse a single format specifier starting after the `~`.
/// Returns the FormatSpecifier and the position after the control character.
fn parse_specifier(bytes: &[u8], mut pos: usize, start: usize) -> Option<(FormatSpecifier, usize)> {
    let len = bytes.len();
    let mut extra_args: usize = 0;

    // Stage 1: Field width - optional '-', then digits or '*'
    if pos < len && bytes[pos] == b'-' {
        pos += 1;
        if pos < len && bytes[pos] == b'*' {
            extra_args += 1;
            pos += 1;
        } else {
            pos = skip_digits(bytes, pos);
        }
    } else if pos < len && bytes[pos] == b'*' {
        extra_args += 1;
        pos += 1;
    } else {
        pos = skip_digits(bytes, pos);
    }

    // Stage 2: Precision - optional '.' then digits or '.*'
    if pos < len && bytes[pos] == b'.' {
        pos += 1;
        if pos < len && bytes[pos] == b'*' {
            extra_args += 1;
            pos += 1;
        } else if pos < len && bytes[pos].is_ascii_digit() {
            pos = skip_digits(bytes, pos);
        }
        // Stage 3: Secondary precision - optional '.*' or '.X'
        if pos < len && bytes[pos] == b'.' {
            pos += 1;
            if pos < len && bytes[pos] == b'*' {
                extra_args += 1;
                pos += 1;
            } else if pos < len {
                pos += 1; // skip single char
            }
        }
    }

    // Stage 4: Modifiers - k, K, l, t
    let mut has_k_upper = false;
    while pos < len && is_modifier(bytes[pos]) {
        if bytes[pos] == b'K' {
            has_k_upper = true;
        }
        pos += 1;
    }
    if has_k_upper {
        extra_args += 1; // K modifier adds a sort fun argument
    }

    // Stage 5: Control character
    if pos >= len {
        return None; // truncated
    }
    let control = bytes[pos] as char;
    pos += 1;

    let base_args = control_type_args(control)?;
    let args_consumed = base_args + extra_args;

    Some((
        FormatSpecifier {
            range: start..pos,
            control,
            args_consumed,
        },
        pos,
    ))
}

fn skip_digits(bytes: &[u8], mut pos: usize) -> usize {
    while pos < bytes.len() && bytes[pos].is_ascii_digit() {
        pos += 1;
    }
    pos
}

fn is_modifier(b: u8) -> bool {
    matches!(b, b'k' | b'K' | b'l' | b't')
}

/// Returns the base number of arguments consumed by a control character,
/// or `None` if the control character is invalid.
fn control_type_args(c: char) -> Option<usize> {
    match c {
        '~' | 'n' => Some(0),
        'c' | 'f' | 'e' | 'g' | 's' | 'w' | 'p' | 'b' | 'B' | 'i' | '+' | '#' => Some(1),
        'W' | 'P' | 'x' | 'X' => Some(2),
        _ => None,
    }
}

/// Compute the byte offset from the start of an AST String node's raw text
/// to the start of the string content (after opening quotes/sigils).
///
/// Examples:
/// - `"hello"` → 1
/// - `~s"hello"` → 3
/// - `~"hello"` → 2
/// - `"""hello"""` → 3
/// - `~s"""hello"""` → 5
pub fn string_content_offset(raw_text: &str) -> usize {
    string_quote_info(raw_text).0
}

/// Compute the byte length of the closing quote sequence for an AST String
/// node's raw text. Inferred from the opening quote style (triple vs single).
///
/// This is more reliable than checking `ends_with("\"\"\"")` on the raw text,
/// which can false-positive on regular strings ending with escaped quotes.
pub fn string_closing_quotes_len(raw_text: &str) -> usize {
    string_quote_info(raw_text).1
}

/// Returns `(content_offset, closing_quotes_len)` for a string literal's raw text.
fn string_quote_info(raw_text: &str) -> (usize, usize) {
    let bytes = raw_text.as_bytes();
    let mut pos = 0;

    // Optional sigil: ~[bBsS]
    if pos < bytes.len() && bytes[pos] == b'~' {
        pos += 1;
        if pos < bytes.len() && matches!(bytes[pos], b'b' | b'B' | b's' | b'S') {
            pos += 1;
        }
    }

    // Opening quotes: """ or "
    if pos + 2 < bytes.len() && &bytes[pos..pos + 3] == b"\"\"\"" {
        (pos + 3, 3)
    } else if pos < bytes.len() && bytes[pos] == b'"' {
        (pos + 1, 1)
    } else {
        (pos, 0)
    }
}

/// Result of extracting and parsing a format string from source text.
#[derive(Debug, Clone)]
pub struct ParsedFormatSource {
    /// The parsed specifiers and argument mapping.
    pub parsed: ParsedFormatString,
    /// Byte offset from the start of the string node to the content.
    pub content_offset: usize,
    /// Source range of the string literal in the file.
    pub string_range: TextRange,
}

/// Extract and parse a format string from a string literal's source range.
///
/// Given the full file text and the `TextRange` of a string literal AST node,
/// strips quotes/sigils and parses format specifiers. Returns `None` if the
/// range is out of bounds, falls on a UTF-8 boundary, or the format string
/// is malformed.
pub fn parse_format_source(file_text: &str, string_range: TextRange) -> Option<ParsedFormatSource> {
    let raw_text =
        file_text.get(usize::from(string_range.start())..usize::from(string_range.end()))?;
    let content_offset = string_content_offset(raw_text);
    let closing_quotes_len = string_closing_quotes_len(raw_text);
    let raw_content = raw_text.get(content_offset..raw_text.len() - closing_quotes_len)?;
    let parsed = parse_format_string(raw_content)?;
    Some(ParsedFormatSource {
        parsed,
        content_offset,
        string_range,
    })
}

/// Convert a format specifier's byte range to a `TextRange` in the source file.
pub fn specifier_source_range(
    string_range: TextRange,
    content_offset: usize,
    spec: &FormatSpecifier,
) -> TextRange {
    let start = string_range.start() + TextSize::from((content_offset + spec.range.start) as u32);
    let end = string_range.start() + TextSize::from((content_offset + spec.range.end) as u32);
    TextRange::new(start, end)
}

/// Information about a recognized format function call.
#[derive(Debug, Clone, Copy)]
pub struct FormatFunctionInfo {
    /// Index of the format string argument in the call's args list.
    pub format_arg_index: usize,
    /// Index of the arguments list argument in the call's args list.
    pub args_list_index: usize,
}

/// Check if a (module, function, arity) triple corresponds to a known
/// format function. Returns the argument indices if it matches.
pub fn match_format_function(
    module: &str,
    function: &str,
    arity: usize,
) -> Option<FormatFunctionInfo> {
    match (module, function, arity) {
        // io:format/2 - format(Format, Args)
        ("io", "format", 2) => Some(FormatFunctionInfo {
            format_arg_index: 0,
            args_list_index: 1,
        }),
        // io:format/3 - format(IoDevice, Format, Args)
        ("io", "format", 3) => Some(FormatFunctionInfo {
            format_arg_index: 1,
            args_list_index: 2,
        }),
        // io:fwrite/2 - fwrite(Format, Args)
        ("io", "fwrite", 2) => Some(FormatFunctionInfo {
            format_arg_index: 0,
            args_list_index: 1,
        }),
        // io:fwrite/3 - fwrite(IoDevice, Format, Args)
        ("io", "fwrite", 3) => Some(FormatFunctionInfo {
            format_arg_index: 1,
            args_list_index: 2,
        }),
        // io_lib:format/2 - format(Format, Args)
        ("io_lib", "format", 2) => Some(FormatFunctionInfo {
            format_arg_index: 0,
            args_list_index: 1,
        }),
        // io_lib:fwrite/2 - fwrite(Format, Args)
        ("io_lib", "fwrite", 2) => Some(FormatFunctionInfo {
            format_arg_index: 0,
            args_list_index: 1,
        }),
        // @fb-only: _ => meta_only::match_format_function(module, function, arity),
                                                                        _ => None, // @oss-only
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_specifiers() {
        let result = parse_format_string("hello ~p world ~s").unwrap();
        assert_eq!(result.specifiers.len(), 2);
        assert_eq!(result.specifiers[0].control, 'p');
        assert_eq!(result.specifiers[0].args_consumed, 1);
        assert_eq!(result.specifiers[0].range, 6..8); // "~p"
        assert_eq!(result.specifiers[1].control, 's');
        assert_eq!(result.specifiers[1].args_consumed, 1);
        assert_eq!(result.specifiers[1].range, 15..17); // "~s"
        assert_eq!(result.arg_to_specifier, vec![0, 1]);
    }

    #[test]
    fn non_consuming_specifiers() {
        let result = parse_format_string("~p~n~~").unwrap();
        assert_eq!(result.specifiers.len(), 3);
        assert_eq!(result.specifiers[0].control, 'p');
        assert_eq!(result.specifiers[0].args_consumed, 1);
        assert_eq!(result.specifiers[1].control, 'n');
        assert_eq!(result.specifiers[1].args_consumed, 0);
        assert_eq!(result.specifiers[2].control, '~');
        assert_eq!(result.specifiers[2].args_consumed, 0);
        assert_eq!(result.arg_to_specifier, vec![0]); // only ~p consumes an arg
    }

    #[test]
    fn multi_arg_specifiers() {
        let result = parse_format_string("~W ~P").unwrap();
        assert_eq!(result.specifiers.len(), 2);
        assert_eq!(result.specifiers[0].control, 'W');
        assert_eq!(result.specifiers[0].args_consumed, 2);
        assert_eq!(result.specifiers[1].control, 'P');
        assert_eq!(result.specifiers[1].args_consumed, 2);
        assert_eq!(result.arg_to_specifier, vec![0, 0, 1, 1]);
    }

    #[test]
    fn specifier_with_width() {
        let result = parse_format_string("~10p").unwrap();
        assert_eq!(result.specifiers.len(), 1);
        assert_eq!(result.specifiers[0].control, 'p');
        assert_eq!(result.specifiers[0].args_consumed, 1);
        assert_eq!(result.specifiers[0].range, 0..4); // "~10p"
    }

    #[test]
    fn specifier_with_negative_width() {
        let result = parse_format_string("~-20s").unwrap();
        assert_eq!(result.specifiers.len(), 1);
        assert_eq!(result.specifiers[0].control, 's');
        assert_eq!(result.specifiers[0].args_consumed, 1);
        assert_eq!(result.specifiers[0].range, 0..5); // "~-20s"
    }

    #[test]
    fn specifier_with_width_and_precision() {
        let result = parse_format_string("~10.5f").unwrap();
        assert_eq!(result.specifiers.len(), 1);
        assert_eq!(result.specifiers[0].control, 'f');
        assert_eq!(result.specifiers[0].args_consumed, 1);
        assert_eq!(result.specifiers[0].range, 0..6); // "~10.5f"
    }

    #[test]
    fn star_width() {
        let result = parse_format_string("~*p").unwrap();
        assert_eq!(result.specifiers.len(), 1);
        assert_eq!(result.specifiers[0].control, 'p');
        assert_eq!(result.specifiers[0].args_consumed, 2); // 1 for *, 1 for p
    }

    #[test]
    fn star_precision() {
        let result = parse_format_string("~.*f").unwrap();
        assert_eq!(result.specifiers.len(), 1);
        assert_eq!(result.specifiers[0].control, 'f');
        assert_eq!(result.specifiers[0].args_consumed, 2); // 1 for .*, 1 for f
    }

    #[test]
    fn negative_star_width() {
        let result = parse_format_string("~-*s").unwrap();
        assert_eq!(result.specifiers.len(), 1);
        assert_eq!(result.specifiers[0].control, 's');
        assert_eq!(result.specifiers[0].args_consumed, 2); // 1 for -*, 1 for s
    }

    #[test]
    fn modifier_k_upper() {
        let result = parse_format_string("~Kp").unwrap();
        assert_eq!(result.specifiers.len(), 1);
        assert_eq!(result.specifiers[0].control, 'p');
        assert_eq!(result.specifiers[0].args_consumed, 2); // 1 for K fun, 1 for p
    }

    #[test]
    fn modifier_l() {
        let result = parse_format_string("~lp").unwrap();
        assert_eq!(result.specifiers.len(), 1);
        assert_eq!(result.specifiers[0].control, 'p');
        assert_eq!(result.specifiers[0].args_consumed, 1); // l doesn't add args
    }

    #[test]
    fn modifier_t() {
        let result = parse_format_string("~tp").unwrap();
        assert_eq!(result.specifiers.len(), 1);
        assert_eq!(result.specifiers[0].control, 'p');
        assert_eq!(result.specifiers[0].args_consumed, 1);
    }

    #[test]
    fn all_control_types() {
        for (c, expected) in [
            ('~', 0),
            ('n', 0),
            ('c', 1),
            ('f', 1),
            ('e', 1),
            ('g', 1),
            ('s', 1),
            ('w', 1),
            ('p', 1),
            ('b', 1),
            ('B', 1),
            ('i', 1),
            ('+', 1),
            ('#', 1),
            ('W', 2),
            ('P', 2),
            ('x', 2),
            ('X', 2),
        ] {
            let input = format!("~{c}");
            let result = parse_format_string(&input).unwrap();
            assert_eq!(
                result.specifiers[0].args_consumed, expected,
                "control '~{c}' should consume {expected} args"
            );
        }
    }

    #[test]
    fn invalid_control() {
        assert!(parse_format_string("~z").is_none());
        assert!(parse_format_string("~Z").is_none());
        assert!(parse_format_string("~q").is_none());
    }

    #[test]
    fn truncated() {
        assert!(parse_format_string("~").is_none());
        assert!(parse_format_string("hello ~").is_none());
        assert!(parse_format_string("~10").is_none());
    }

    #[test]
    fn escape_sequences_in_string() {
        // \n is an escape sequence, not a ~
        let result = parse_format_string("~p\\n~s").unwrap();
        assert_eq!(result.specifiers.len(), 2);
        assert_eq!(result.specifiers[0].control, 'p');
        assert_eq!(result.specifiers[0].range, 0..2);
        assert_eq!(result.specifiers[1].control, 's');
        assert_eq!(result.specifiers[1].range, 4..6);
    }

    #[test]
    fn empty_string() {
        let result = parse_format_string("").unwrap();
        assert_eq!(result.specifiers.len(), 0);
        assert_eq!(result.arg_to_specifier.len(), 0);
    }

    #[test]
    fn no_specifiers() {
        let result = parse_format_string("hello world").unwrap();
        assert_eq!(result.specifiers.len(), 0);
    }

    #[test]
    fn complex_format() {
        // "Value: ~10.5f, Name: ~-20s, Debug: ~W~n"
        let result = parse_format_string("Value: ~10.5f, Name: ~-20s, Debug: ~W~n").unwrap();
        assert_eq!(result.specifiers.len(), 4);
        assert_eq!(result.specifiers[0].control, 'f');
        assert_eq!(result.specifiers[0].args_consumed, 1);
        assert_eq!(result.specifiers[1].control, 's');
        assert_eq!(result.specifiers[1].args_consumed, 1);
        assert_eq!(result.specifiers[2].control, 'W');
        assert_eq!(result.specifiers[2].args_consumed, 2);
        assert_eq!(result.specifiers[3].control, 'n');
        assert_eq!(result.specifiers[3].args_consumed, 0);
        assert_eq!(result.arg_to_specifier, vec![0, 1, 2, 2]);
    }

    #[test]
    fn string_content_offset_simple() {
        assert_eq!(string_content_offset("\"hello\""), 1);
    }

    #[test]
    fn string_content_offset_sigil_s() {
        assert_eq!(string_content_offset("~s\"hello\""), 3);
    }

    #[test]
    fn string_content_offset_sigil_bare() {
        assert_eq!(string_content_offset("~\"hello\""), 2);
    }

    #[test]
    fn string_content_offset_triple_quote() {
        assert_eq!(string_content_offset("\"\"\"hello\"\"\""), 3);
    }

    #[test]
    fn string_content_offset_sigil_triple_quote() {
        assert_eq!(string_content_offset("~s\"\"\"hello\"\"\""), 5);
    }

    #[test]
    fn string_closing_quotes_len_simple() {
        assert_eq!(string_closing_quotes_len("\"hello\""), 1);
    }

    #[test]
    fn string_closing_quotes_len_triple() {
        assert_eq!(string_closing_quotes_len("\"\"\"hello\"\"\""), 3);
    }

    #[test]
    fn string_closing_quotes_len_sigil() {
        assert_eq!(string_closing_quotes_len("~s\"hello\""), 1);
    }

    #[test]
    fn string_closing_quotes_len_sigil_triple() {
        assert_eq!(string_closing_quotes_len("~s\"\"\"hello\"\"\""), 3);
    }

    #[test]
    fn match_format_function_io_format() {
        let info = match_format_function("io", "format", 2).unwrap();
        assert_eq!(info.format_arg_index, 0);
        assert_eq!(info.args_list_index, 1);

        let info = match_format_function("io", "format", 3).unwrap();
        assert_eq!(info.format_arg_index, 1);
        assert_eq!(info.args_list_index, 2);
    }

    #[test]
    fn match_format_function_io_lib() {
        let info = match_format_function("io_lib", "format", 2).unwrap();
        assert_eq!(info.format_arg_index, 0);
        assert_eq!(info.args_list_index, 1);
    }

    #[test]
    fn match_format_function_unknown() {
        assert!(match_format_function("io", "read", 1).is_none());
        assert!(match_format_function("lists", "format", 2).is_none());
    }
}

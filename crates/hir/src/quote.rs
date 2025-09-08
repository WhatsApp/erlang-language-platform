/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

pub fn escape_and_quote_string(raw: &str) -> String {
    escape_and_quote(StringLike::ListString, raw)
}

pub fn escape_and_quote_binary_string(raw: &str) -> String {
    escape_and_quote(StringLike::BinaryString, raw)
}

pub fn escape_and_quote_atom(raw: &str) -> String {
    escape_and_quote(StringLike::Atom, raw)
}

#[derive(Eq, PartialEq)]
enum StringLike {
    Atom,
    ListString,
    BinaryString,
}

// See reference implementation:
// https://github.com/erlang/otp/blob/ae81b2f6ff2d541c01242f12cdbd5238aa4b26bd/lib/stdlib/src/io_lib.erl#L1120
//
// N.B. assumes Erlang's unicode mode
fn escape_and_quote(kind: StringLike, raw: &str) -> String {
    let mut result = String::with_capacity(raw.len());
    // Open quotes
    match kind {
        StringLike::Atom if atom_needs_escape(raw) => result.push('\''),
        StringLike::Atom => {}
        StringLike::ListString => result.push('\"'),
        StringLike::BinaryString => result.push_str("~\""),
    }
    // String-like contents
    for char in raw.chars() {
        match char {
            '\n' => result.push_str(r"\n"),
            '\t' => result.push_str(r"\t"),
            '\r' => result.push_str(r"\r"),
            '\x0B' => result.push_str(r"\v"),
            '\x08' => result.push_str(r"\b"),
            '\x0F' => result.push_str(r"\f"),
            '\x1B' => result.push_str(r"\e"),
            '\x7F' => result.push_str(r"\d"),
            '\\' => result.push_str(r"\\"),
            '\'' if kind == StringLike::Atom => {
                result.push_str("\\'");
            }
            '\"' if kind == StringLike::BinaryString || kind == StringLike::ListString => {
                result.push_str("\\\"");
            }
            _ if (' '..='~').contains(&char) => result.push(char),
            _ if (char < '\u{00A0}') => {
                result.push('\\');
                result.push(char::from_u32((Into::<u32>::into(char) >> 6) + 48).unwrap());
                result.push(char::from_u32(((Into::<u32>::into(char) >> 3) & 7) + 48).unwrap());
                result.push(char::from_u32((Into::<u32>::into(char) & 7) + 48).unwrap());
            }
            _ => result.push(char),
        }
    }
    // Close quotes
    match kind {
        StringLike::Atom if atom_needs_escape(raw) => result.push('\''),
        StringLike::Atom => {}
        StringLike::ListString => result.push('\"'),
        StringLike::BinaryString => result.push('\"'),
    }
    result
}

// See: https://github.com/erlang/otp/blob/d04f6a0b261cf23e316e48e2febe14b30c592dc5/lib/stdlib/src/erl_scan.erl#L2272
pub fn atom_is_reserved_word(raw: &str) -> bool {
    match raw {
        "after" => true,
        "begin" => true,
        "case" => true,
        "try" => true,
        "cond" => true,
        "catch" => true,
        "andalso" => true,
        "orelse" => true,
        "end" => true,
        "fun" => true,
        "if" => true,
        "let" => true,
        "of" => true,
        "receive" => true,
        "when" => true,
        "bnot" => true,
        "not" => true,
        "div" => true,
        "rem" => true,
        "band" => true,
        "and" => true,
        "bor" => true,
        "bxor" => true,
        "bsl" => true,
        "bsr" => true,
        "or" => true,
        "xor" => true,
        _ => false,
    }
}

pub fn atom_needs_escape(raw: &str) -> bool {
    // TODO Handle erl_features too
    if raw.is_empty() || atom_is_reserved_word(raw) {
        true
    } else {
        // We already checked that the atom is not empty, so it's safe to unwrap the first char
        let first_char = raw.chars().next().unwrap();
        // First char is checked differently to subsequent chars:
        // https://github.com/erlang/otp/blob/ae81b2f6ff2d541c01242f12cdbd5238aa4b26bd/lib/stdlib/src/io_lib.erl#L934
        if first_char.is_ascii_lowercase()
            || (('ß'..='ÿ').contains(&first_char) && first_char != '÷')
        {
            // See: https://github.com/erlang/otp/blob/ae81b2f6ff2d541c01242f12cdbd5238aa4b26bd/lib/stdlib/src/io_lib.erl#L943
            !raw.chars().all(|char: char| {
                char.is_ascii_lowercase()
                    || (('ß'..='ÿ').contains(&char) && char != '÷')
                    || char.is_ascii_uppercase()
                    || (('À'..='Þ').contains(&char) && char != '×')
                    || char.is_ascii_digit()
                    || (char == '_')
                    || (char == '@')
            })
        } else {
            true
        }
    }
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::quote::escape_and_quote_atom;
    use crate::quote::escape_and_quote_binary_string;
    use crate::quote::escape_and_quote_string;

    #[test]
    fn check_simple_atom_that_does_not_need_quotes() {
        expect![[r#"foo"#]].assert_eq(&escape_and_quote_atom("foo"))
    }

    #[test]
    fn check_underscore_atom_does_not_need_quotes() {
        expect![[r#"foo_bar"#]].assert_eq(&escape_and_quote_atom("foo_bar"))
    }

    #[test]
    fn check_initial_underscore_atom_does_need_quotes() {
        expect![[r#"'_foo_bar'"#]].assert_eq(&escape_and_quote_atom("_foo_bar"))
    }

    #[test]
    fn check_lone_underscore_atom_does_need_quotes() {
        expect![[r#"'_'"#]].assert_eq(&escape_and_quote_atom("_"))
    }

    #[test]
    fn check_at_atom_does_not_need_quotes() {
        expect![[r#"foo@bar"#]].assert_eq(&escape_and_quote_atom("foo@bar"))
    }

    #[test]
    fn check_initial_at_atom_does_need_quotes() {
        expect![[r#"'@foo'"#]].assert_eq(&escape_and_quote_atom("@foo"))
    }

    #[test]
    fn check_lone_at_atom_does_need_quotes() {
        expect![[r#"'@'"#]].assert_eq(&escape_and_quote_atom("@"))
    }

    #[test]
    fn check_empty_atom_is_quoted() {
        expect![[r#"''"#]].assert_eq(&escape_and_quote_atom(""))
    }

    #[test]
    fn check_simple_string() {
        expect![[r#""foo""#]].assert_eq(&escape_and_quote_string("foo"))
    }

    #[test]
    fn check_simple_binary_string() {
        expect![[r#"~"foo""#]].assert_eq(&escape_and_quote_binary_string("foo"))
    }

    #[test]
    fn check_emoji_atom() {
        expect![[r#"'🦹🏻‍♂️'"#]].assert_eq(&escape_and_quote_atom("🦹🏻‍♂️"))
    }

    #[test]
    fn check_emoji_string() {
        expect![[r#""🦹🏻‍♂️""#]].assert_eq(&escape_and_quote_string("🦹🏻‍♂️"))
    }

    #[test]
    fn check_emoji_binary_string() {
        expect![[r#"~"🦹🏻‍♂️""#]].assert_eq(&escape_and_quote_binary_string("🦹🏻‍♂️"))
    }

    #[test]
    fn check_erlang_escape_chars_atom() {
        expect![[r#"'\n\t\r\v\b\f\e\d'"#]]
            .assert_eq(&escape_and_quote_atom("\n\t\r\x0B\x08\x0F\x1B\x7F"))
    }

    #[test]
    fn check_erlang_escape_chars_string() {
        expect![[r#""\n\t\r\v\b\f\e\d""#]]
            .assert_eq(&escape_and_quote_string("\n\t\r\x0B\x08\x0F\x1B\x7F"))
    }

    #[test]
    fn check_erlang_escape_chars_binary_string() {
        expect![[r#"~"\n\t\r\v\b\f\e\d""#]].assert_eq(&escape_and_quote_binary_string(
            "\n\t\r\x0B\x08\x0F\x1B\x7F",
        ))
    }

    #[test]
    fn check_erlang_backslash_char_atom() {
        expect![[r#"'\\'"#]].assert_eq(&escape_and_quote_atom(r"\"))
    }

    #[test]
    fn check_erlang_backslash_char_string() {
        expect![[r#""\\""#]].assert_eq(&escape_and_quote_string(r"\"))
    }

    #[test]
    fn check_erlang_backslash_char_binary_string() {
        expect![[r#"~"\\""#]].assert_eq(&escape_and_quote_binary_string(r"\"))
    }

    #[test]
    fn check_erlang_quote_char_atom() {
        expect![[r#"'\''"#]].assert_eq(&escape_and_quote_atom(r"'"))
    }

    #[test]
    fn check_erlang_quote_char_string() {
        expect![[r#""\"""#]].assert_eq(&escape_and_quote_string("\""))
    }

    #[test]
    fn check_erlang_quote_char_binary_string() {
        expect![[r#"~"\"""#]].assert_eq(&escape_and_quote_binary_string("\""))
    }

    #[test]
    fn check_needs_quote_for_simple_atom() {
        assert!(!crate::quote::atom_needs_escape("foo"))
    }

    #[test]
    fn check_needs_quote_for_empty_atom() {
        assert!(crate::quote::atom_needs_escape(""))
    }

    #[test]
    fn check_needs_quote_for_atom_with_special_characters() {
        assert!(crate::quote::atom_needs_escape("foo bar"));
        assert!(crate::quote::atom_needs_escape("foo÷bar"));
        assert!(crate::quote::atom_needs_escape("foo😵‍💫bar"));
        assert!(crate::quote::atom_needs_escape("foo'bar"))
    }

    #[test]
    fn check_atom_is_reserved_keyword_true() {
        assert!(crate::quote::atom_is_reserved_word("andalso"))
    }

    #[test]
    fn check_atom_is_reserved_keyword_false() {
        assert!(!crate::quote::atom_is_reserved_word("foo"))
    }
}

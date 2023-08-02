/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::char;
/// unescape Erlang strings
/// Based on https://docs.rs/unescape/latest/unescape/fn.unescape.html
use std::collections::VecDeque;
use std::ops::BitAnd;

macro_rules! try_option {
    ($o:expr) => {
        match $o {
            Some(s) => s,
            None => return None,
        }
    };
}

/// Takes in a string with backslash escapes written out with literal
/// backslash characters and converts it to a string with the proper
/// escaped characters, according to Erlang syntax.
/// Only unescape if the string is surrounded by ' or " chars
pub fn unescape_string(s_in: &str) -> Option<Cow<str>> {
    if !s_in.contains(['\'', '"', '\\', '$']) {
        return Some(Cow::Borrowed(s_in));
    }

    let mut queue: VecDeque<_> = String::from(s_in).chars().collect();
    let mut s = String::new();

    if let Some(&c) = queue.get(0) {
        if c != '\'' && c != '\"' && c != '$' {
            return Some(Cow::Borrowed(s_in));
        } else {
            // Remove leading delimiter
            queue.pop_front();
        }
    }

    while let Some(c) = queue.pop_front() {
        if (c == '\'' || c == '\"') && queue.is_empty() {
            return Some(Cow::Owned(s));
        }
        if c != '\\' {
            s.push(c);
            continue;
        }

        // Based on https://www.erlang.org/doc/reference_manual/data_types.html#escape-sequences
        // Sequence	Description
        // \b	Backspace
        // \d	Delete
        // \e	Escape
        // \f	Form feed
        // \n	Newline
        // \r	Carriage return
        // \s	Space
        // \t	Tab
        // \v	Vertical tab
        // \XYZ, \YZ, \Z	Character with octal representation XYZ, YZ or Z
        // \xXY	Character with hexadecimal representation XY
        // \x{X...}	Character with hexadecimal representation; X... is one or more hexadecimal characters
        // \^a...\^z
        // \^A...\^Z	Control A to control Z
        // \'	Single quote
        // \"	Double quote
        // \\	Backslash

        match queue.pop_front() {
            Some('b') => s.push('\u{0008}'),
            Some('d') => s.push('\u{007F}'),
            Some('e') => s.push('\u{001B}'),
            Some('f') => s.push('\u{000C}'),
            Some('n') => s.push('\n'),
            Some('r') => s.push('\r'),
            Some('s') => s.push(' '),
            Some('t') => s.push('\t'),
            Some('v') => s.push('\u{000B}'),
            Some(c) if c.is_digit(8) => s.push(try_option!(unescape_octal(c, &mut queue))),
            Some('x') => s.push(try_option!(unescape_hex(&mut queue))),
            Some('^') => s.push(try_option!(unescape_control(&mut queue))),
            Some('\'') => s.push('\''),
            Some('\"') => s.push('\"'),
            Some('\\') => s.push('\\'),
            Some(c) => s.push(c),
            None => {}
        };
    }

    Some(Cow::Owned(s))
}

fn unescape_octal(c: char, queue: &mut VecDeque<char>) -> Option<char> {
    let mut s = String::new();
    s.push(c);
    if next_digit(8, &mut s, queue) {
        next_digit(8, &mut s, queue);
    }
    if let Ok(u) = u32::from_str_radix(&s, 8) {
        char::from_u32(u)
    } else {
        None
    }
}

fn next_digit(radix: u32, s: &mut String, queue: &mut VecDeque<char>) -> bool {
    if let Some(d) = queue.pop_front() {
        if d.is_digit(radix) {
            s.push(d);
            true
        } else {
            queue.push_front(d);
            false
        }
    } else {
        false
    }
}

fn unescape_hex(queue: &mut VecDeque<char>) -> Option<char> {
    let mut s = String::new();

    if let Some(c) = queue.pop_front() {
        if c == '{' {
            return unescape_hex_curly(queue);
        } else {
            s.push(c);
            next_digit(16, &mut s, queue);
        }
    }

    let u = try_option!(u32::from_str_radix(&s, 16).ok());
    char::from_u32(u)
}

fn unescape_hex_curly(queue: &mut VecDeque<char>) -> Option<char> {
    let mut s = String::new();

    let mut i = 0;
    while i < 4 && next_digit(16, &mut s, queue) {
        i += 1;
    }
    // Skip trailing curly brace
    let _ = queue.pop_front();

    let u = try_option!(u32::from_str_radix(&s, 16).ok());
    char::from_u32(u)
}

// In erlang, \n is returned unchanged, all others anded with 31
fn unescape_control(queue: &mut VecDeque<char>) -> Option<char> {
    if let Some(c) = queue.pop_front() {
        if c == '\n' {
            Some(c)
        } else {
            char::from_u32((c as u32).bitand(31))
        }
    } else {
        None
    }
}

// ---------------------------------------------------------------------
#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::unescape_string;

    #[test]
    fn unescape_string_plain() {
        expect![[r#"foo"#]].assert_eq(&unescape_string(r#"foo"#).unwrap());
    }
    #[test]
    fn unescape_string_quotes_only() {
        expect![[r#"foo"#]].assert_eq(&unescape_string(r#"'foo'"#).unwrap());
    }
    #[test]
    fn unescape_string_unicode_chars() {
        expect![[r#"!@#$%% ä"#]].assert_eq(&unescape_string(r#"'!@#$%% ä'"#).unwrap());
    }

    #[test]
    fn unescape_string_char() {
        expect![[r#"a"#]].assert_eq(&unescape_string(r#"$a"#).unwrap());
    }

    #[test]
    fn unescape_string_string() {
        expect![[r#"abc"#]].assert_eq(&unescape_string(r#""abc""#).unwrap());
    }

    #[test]
    fn unescape_string_esc_chars() {
        assert_eq!(
            "a\u{8}\u{7f}\u{1b}\u{c}\n \t\u{b}b",
            &unescape_string(r#"'a\b\d\e\f\n\s\t\vb"#).unwrap()
        );
    }
    #[test]
    fn unescape_string_esc_cr() {
        assert_eq!("a\rb", &unescape_string(r#"'a\rb"#).unwrap());
    }

    #[test]
    fn unescape_string_esc_octal() {
        assert_eq!(
            "a b\u{7}c\u{0}d",
            &unescape_string(r#"'a\040b\07c\0d"#).unwrap()
        );
    }

    #[test]
    fn unescape_string_esc_hex_simple() {
        assert_eq!(
            "a\x00b8<\x3f",
            &unescape_string(r#"'a\x00b\x38\x3c\x3F"#).unwrap()
        );
    }

    #[test]
    fn unescape_string_esc_hex_braces() {
        assert_eq!(
            "a\x00b8<\x3f",
            &unescape_string(r#"'a\x{00}b\x{38}\x3c\x{003F}"#).unwrap()
        );
    }

    #[test]
    fn unescape_string_esc_control_chars() {
        assert_eq!(
            "a\x07b\x07c\x1dd",
            &unescape_string(r#"'a\^Gb\^gc\^]d"#).unwrap()
        );
    }

    #[test]
    fn unescape_string_esc_self_escape() {
        assert_eq!(
            "aGb%cd'\"\\",
            &unescape_string(r#"'a\Gb\%cd\'\"\\"#).unwrap()
        );
    }
}

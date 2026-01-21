/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

pub use dissimilar::diff as __diff;

// ---------------------------------------------------------------------

// From rust-analyzer test_utils/src/lib.rs
//
/// Asserts that two strings are equal, otherwise displays a rich diff between them.
///
/// The diff shows changes from the "original" left string to the "actual" right string.
///
/// All arguments starting from and including the 3rd one are passed to
/// `eprintln!()` macro in case of text inequality.
#[macro_export]
macro_rules! assert_eq_text {
    ($left:expr, $right:expr) => {
        assert_eq_text!($left, $right,)
    };
    ($left:expr, $right:expr, $($tt:tt)*) => {{
        let left = $left;
        let right = $right;
        if left != right {
            if left.trim() == right.trim() {
                std::eprintln!("Left:\n{:?}\n\nRight:\n{:?}\n\nWhitespace difference\n", left, right);
            } else {
                let diff = $crate::test_utils::__diff(left, right);
                std::eprintln!("Left:\n{}\n\nRight:\n{}\n\nDiff:\n{}\n", left, right, $crate::test_utils::format_diff(diff));
            }
            std::eprintln!($($tt)*);
            panic!("text differs");
        }
    }};
}

/// Asserts that two values are equal, using "expected" and "actual" labels
/// in the error message instead of the confusing "left" and "right".
///
/// # Example
/// ```ignore
/// assert_eq_expected!(vec![1, 2, 3], actual_values);
/// ```
///
/// On failure, prints:
/// ```text
/// assertion failed
///   expected: [1, 2, 3]
///   actual  : [1, 2, 4]
/// ```
#[macro_export]
macro_rules! assert_eq_expected {
    ($expected:expr, $actual:expr) => {{
        let expected = &$expected;
        let actual = &$actual;
        assert!(
            *expected == *actual,
            "assertion failed\n  expected: {expected:?}\n  actual  : {actual:?}"
        );
    }};
    ($expected:expr, $actual:expr, $($arg:tt)+) => {{
        let expected = &$expected;
        let actual = &$actual;
        assert!(
            *expected == *actual,
            "{}\n  expected: {expected:?}\n  actual  : {actual:?}",
            format_args!($($arg)+)
        );
    }};
}

pub fn format_diff(chunks: Vec<dissimilar::Chunk>) -> String {
    let mut buf = String::new();
    for chunk in chunks {
        let formatted = match chunk {
            dissimilar::Chunk::Equal(text) => text.into(),
            dissimilar::Chunk::Delete(text) => format!("\x1b[41m{text}\x1b[0m"),
            dissimilar::Chunk::Insert(text) => format!("\x1b[42m{text}\x1b[0m"),
        };
        buf.push_str(&formatted);
    }
    buf
}

// ---------------------------------------------------------------------

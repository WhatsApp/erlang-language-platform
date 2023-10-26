/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::fmt::Write;
use std::hash::Hash;
use std::ops::Range;

use imara_diff::diff;
use imara_diff::intern::InternedInput;
use imara_diff::intern::Interner;
use imara_diff::intern::Token;
use imara_diff::Algorithm;
use imara_diff::Sink;

pub fn diff_from_textedit(before: &str, after: &str) -> (Vec<DiffRange>, Option<String>) {
    let input = InternedInput::new(before, after);
    let (unified, diff_range) = diff(Algorithm::Histogram, &input, DiffRangeBuilder::new(&input));
    (diff_range, Some(unified))
}

// ---------------------------------------------------------------------
// Based on [`UnifiedDiffBuilder`](imara_diff::UnifiedDiffBuilder),
// with additional output of the changed ranges

pub struct DiffRangeBuilder<'a, W, T>
where
    W: Write,
    T: Hash + Eq + Display,
{
    before: &'a [Token],
    after: &'a [Token],
    interner: &'a Interner<T>,

    pos: u32,
    before_hunk_start: u32,
    after_hunk_start: u32,
    before_hunk_len: u32,
    after_hunk_len: u32,

    buffer: String,
    dst: W,

    out: Vec<DiffRange>,
}

impl<'a, T> DiffRangeBuilder<'a, String, T>
where
    T: Hash + Eq + Display,
{
    pub fn new(input: &'a InternedInput<T>) -> Self {
        Self {
            before_hunk_start: 0,
            after_hunk_start: 0,
            before_hunk_len: 0,
            after_hunk_len: 0,
            buffer: String::with_capacity(8),
            dst: String::new(),
            interner: &input.interner,
            before: &input.before,
            after: &input.after,
            pos: 0,
            out: Vec::default(),
        }
    }
}

#[derive(Debug)]
pub struct DiffRange {
    pub after_start: u32,
}

impl<'a, W, T> DiffRangeBuilder<'a, W, T>
where
    W: Write,
    T: Hash + Eq + Display,
{
    /// Create a new `UnifiedDiffBuilder` for the given `input`,
    /// that will writes it output to the provided implementation of [`Write`](std::fmt::Write).
    pub fn with_writer(input: &'a InternedInput<T>, writer: W) -> Self {
        Self {
            before_hunk_start: 0,
            after_hunk_start: 0,
            before_hunk_len: 0,
            after_hunk_len: 0,
            buffer: String::with_capacity(8),
            dst: writer,
            interner: &input.interner,
            before: &input.before,
            after: &input.after,
            pos: 0,
            out: Vec::default(),
        }
    }

    fn print_tokens(&mut self, tokens: &[Token], prefix: char) {
        for &token in tokens {
            writeln!(&mut self.buffer, "{prefix}{}", self.interner[token]).unwrap();
        }
    }

    fn flush(&mut self) {
        if self.before_hunk_len == 0 && self.after_hunk_len == 0 {
            return;
        }

        let end = (self.pos + 3).min(self.before.len() as u32);
        self.update_pos(end, end, None);

        writeln!(
            &mut self.dst,
            "@@ -{},{} +{},{} @@",
            self.before_hunk_start + 1,
            self.before_hunk_len,
            self.after_hunk_start + 1,
            self.after_hunk_len,
        )
        .unwrap();
        write!(&mut self.dst, "{}", &self.buffer).unwrap();
        self.buffer.clear();
        self.before_hunk_len = 0;
        self.after_hunk_len = 0
    }

    fn update_pos(&mut self, print_to: u32, move_to: u32, after: Option<Range<u32>>) {
        self.print_tokens(&self.before[self.pos as usize..print_to as usize], ' ');
        let len = print_to - self.pos;
        self.pos = move_to;
        self.before_hunk_len += len;
        self.after_hunk_len += len;

        if let Some(after) = after {
            self.out.push(DiffRange {
                after_start: after.start,
            });
        }
    }
}

impl<W, T> Sink for DiffRangeBuilder<'_, W, T>
where
    W: Write,
    T: Hash + Eq + Display,
{
    type Out = (W, Vec<DiffRange>);

    fn process_change(&mut self, before: Range<u32>, after: Range<u32>) {
        if before.start - self.pos > 6 {
            self.flush();
            self.pos = before.start - 3;
            self.before_hunk_start = self.pos;
            self.after_hunk_start = after.start - 3;
        }
        self.update_pos(before.start, before.end, Some(after.clone()));
        self.before_hunk_len += before.end - before.start;
        self.after_hunk_len += after.end - after.start;
        self.print_tokens(
            &self.before[before.start as usize..before.end as usize],
            '-',
        );
        self.print_tokens(&self.after[after.start as usize..after.end as usize], '+');
    }

    fn finish(mut self) -> Self::Out {
        self.flush();
        (self.dst, self.out)
    }
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::diff_from_textedit;

    #[test]
    fn diff_provides_correct_change_range() {
        let before = r#"
          line 1
          line 2
          line 3
          line 4
          line 5
          line 6
          "#;
        let after = r#"
          line 1
          line 3
          line 4
          line 5a
          line 5b
          line 6
          "#;

        let (diff, unified) = diff_from_textedit(before, after);
        expect![[r#"
            [
                DiffRange {
                    after_start: 2,
                },
                DiffRange {
                    after_start: 4,
                },
            ]
        "#]]
        .assert_debug_eq(&diff);
        expect![[r#"
            @@ -1,8 +1,8 @@
 
                       line 1
            -          line 2
                       line 3
                       line 4
            -          line 5
            +          line 5a
            +          line 5b
                       line 6
           
        "#]]
        .assert_eq(
            &unified
                .unwrap()
                .split('\n')
                .map(str::trim_end)
                .collect::<Vec<_>>()
                .join("\n"),
        );
    }
}

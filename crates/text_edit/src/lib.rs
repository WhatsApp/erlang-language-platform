/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Taken from https://github.com/rust-lang/rust-analyzer/blob/2024-10-21/crates/text-edit/src/lib.rs

//! Representation of a `TextEdit`.
//!
//! `rust-analyzer` never mutates text itself and only sends diffs to clients,
//! so `TextEdit` is the ultimate representation of the work done by
//! rust-analyzer.

use std::cmp::max;

use itertools::Itertools;
pub use text_size::TextRange;
pub use text_size::TextSize;

/// `InsertDelete` -- a single "atomic" change to text
///
/// Must not overlap with other `InDel`s
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Indel {
    pub insert: String,
    /// Refers to offsets in the original text
    pub delete: TextRange,
}

#[derive(Default, Debug, Clone)]
pub struct TextEdit {
    /// Invariant: disjoint and sorted by `delete`.
    indels: Vec<Indel>,
}

#[derive(Debug, Default, Clone)]
pub struct TextEditBuilder {
    indels: Vec<Indel>,
}

impl Indel {
    pub fn insert(offset: TextSize, text: String) -> Indel {
        Indel::replace(TextRange::empty(offset), text)
    }
    pub fn delete(range: TextRange) -> Indel {
        Indel::replace(range, String::new())
    }
    pub fn replace(range: TextRange, replace_with: String) -> Indel {
        Indel {
            delete: range,
            insert: replace_with,
        }
    }

    pub fn apply(&self, text: &mut String) {
        let start: usize = self.delete.start().into();
        let end: usize = self.delete.end().into();
        text.replace_range(start..end, &self.insert);
    }
}

impl TextEdit {
    pub fn builder() -> TextEditBuilder {
        TextEditBuilder::default()
    }

    pub fn insert(offset: TextSize, text: String) -> TextEdit {
        let mut builder = TextEdit::builder();
        builder.insert(offset, text);
        builder.finish()
    }

    pub fn delete(range: TextRange) -> TextEdit {
        let mut builder = TextEdit::builder();
        builder.delete(range);
        builder.finish()
    }

    pub fn replace(range: TextRange, replace_with: String) -> TextEdit {
        let mut builder = TextEdit::builder();
        builder.replace(range, replace_with);
        builder.finish()
    }

    pub fn len(&self) -> usize {
        self.indels.len()
    }

    pub fn is_empty(&self) -> bool {
        self.indels.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Indel> {
        self.into_iter()
    }

    pub fn apply(&self, text: &mut String) {
        match self.len() {
            0 => return,
            1 => {
                self.indels[0].apply(text);
                return;
            }
            _ => (),
        }

        let text_size = TextSize::of(&*text);
        let mut total_len = text_size;
        let mut max_total_len = text_size;
        for indel in &self.indels {
            total_len += TextSize::of(&indel.insert);
            total_len -= indel.delete.len();
            max_total_len = max(max_total_len, total_len);
        }

        if let Some(additional) = max_total_len.checked_sub(text_size) {
            text.reserve(additional.into());
        }

        for indel in self.indels.iter().rev() {
            indel.apply(text);
        }

        assert_eq!(TextSize::of(&*text), total_len);
    }

    pub fn union(&mut self, other: TextEdit) -> Result<(), TextEdit> {
        let iter_merge = self
            .iter()
            .merge_by(other.iter(), |l, r| l.delete.start() <= r.delete.start());
        if !check_disjoint(&mut iter_merge.clone()) {
            return Err(other);
        }

        // Only dedup deletions and replacements, keep all insertions
        self.indels = iter_merge
            .dedup_by(|a, b| a == b && !a.delete.is_empty())
            .cloned()
            .collect();
        Ok(())
    }

    pub fn apply_to_offset(&self, offset: TextSize) -> Option<TextSize> {
        let mut res = offset;
        for indel in &self.indels {
            if indel.delete.start() >= offset {
                break;
            }
            if offset < indel.delete.end() {
                return None;
            }
            res += TextSize::of(&indel.insert);
            res -= indel.delete.len();
        }
        Some(res)
    }
}

impl IntoIterator for TextEdit {
    type Item = Indel;
    type IntoIter = std::vec::IntoIter<Indel>;

    fn into_iter(self) -> Self::IntoIter {
        self.indels.into_iter()
    }
}

impl<'a> IntoIterator for &'a TextEdit {
    type Item = &'a Indel;
    type IntoIter = std::slice::Iter<'a, Indel>;

    fn into_iter(self) -> Self::IntoIter {
        self.indels.iter()
    }
}

impl TextEditBuilder {
    pub fn is_empty(&self) -> bool {
        self.indels.is_empty()
    }
    pub fn replace(&mut self, range: TextRange, replace_with: String) {
        self.indel(Indel::replace(range, replace_with));
    }
    pub fn delete(&mut self, range: TextRange) {
        self.indel(Indel::delete(range));
    }
    pub fn insert(&mut self, offset: TextSize, text: String) {
        self.indel(Indel::insert(offset, text));
    }
    pub fn finish(self) -> TextEdit {
        let mut indels = self.indels;
        assert_disjoint_or_equal(&mut indels);
        indels = coalesce_indels(indels);
        TextEdit { indels }
    }
    pub fn invalidates_offset(&self, offset: TextSize) -> bool {
        self.indels
            .iter()
            .any(|indel| indel.delete.contains_inclusive(offset))
    }
    fn indel(&mut self, indel: Indel) {
        self.indels.push(indel);
        if self.indels.len() <= 16 {
            assert_disjoint_or_equal(&mut self.indels);
        }
    }
}

fn assert_disjoint_or_equal(indels: &mut [Indel]) {
    assert!(check_disjoint_and_sort(indels));
}

fn check_disjoint_and_sort(indels: &mut [Indel]) -> bool {
    indels.sort_by_key(|indel| (indel.delete.start(), indel.delete.end()));
    check_disjoint(&mut indels.iter())
}

fn check_disjoint<'a, I>(indels: &mut I) -> bool
where
    I: std::iter::Iterator<Item = &'a Indel> + Clone,
{
    indels
        .clone()
        .zip(indels.skip(1))
        .all(|(l, r)| l.delete.end() <= r.delete.start() || l == r)
}

fn coalesce_indels(indels: Vec<Indel>) -> Vec<Indel> {
    indels
        .into_iter()
        .coalesce(|mut a, b| {
            if a.delete.end() == b.delete.start() {
                a.insert.push_str(&b.insert);
                a.delete = TextRange::new(a.delete.start(), b.delete.end());
                Ok(a)
            } else {
                Err((a, b))
            }
        })
        .collect_vec()
}

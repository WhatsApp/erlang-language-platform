/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! This modules defines type to represent changes to the source code, that flow
//! from the server to the client.
//!
//! It can be viewed as a dual for `Change`.

use std::collections::hash_map::Entry;
use std::iter;
use std::mem;

use elp_base_db::AnchoredPathBuf;
use elp_base_db::FileId;
use elp_syntax::SyntaxNode;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use fxhash::FxHashMap;
use stdx::never;

use crate::helpers::SnippetCap;
use crate::text_edit::TextEdit;
use crate::text_edit::TextEditBuilder;
use crate::tree_diff::diff;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct HashableAnchoredPathBuf {
    /// File that this path is relative to.
    pub anchor: FileId,
    /// Path relative to `anchor`'s containing directory.
    pub path: String,
}

impl From<AnchoredPathBuf> for HashableAnchoredPathBuf {
    fn from(value: AnchoredPathBuf) -> Self {
        HashableAnchoredPathBuf {
            anchor: value.anchor,
            path: value.path,
        }
    }
}

impl From<HashableAnchoredPathBuf> for AnchoredPathBuf {
    fn from(value: HashableAnchoredPathBuf) -> Self {
        AnchoredPathBuf {
            anchor: value.anchor,
            path: value.path,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct SourceChange {
    pub source_file_edits: FxHashMap<FileId, TextEdit>,
    pub new_file_edits: FxHashMap<HashableAnchoredPathBuf, TextEdit>,
    pub file_system_edits: Vec<FileSystemEdit>,
    pub is_snippet: bool,
}

impl SourceChange {
    /// Creates a new SourceChange with the given label
    /// from the edits.
    pub fn from_edits(
        source_file_edits: FxHashMap<FileId, TextEdit>,
        file_system_edits: Vec<FileSystemEdit>,
    ) -> Self {
        SourceChange {
            source_file_edits,
            new_file_edits: FxHashMap::default(),
            file_system_edits,
            is_snippet: false,
        }
    }

    pub fn from_text_edit(file_id: FileId, edit: TextEdit) -> Self {
        SourceChange {
            source_file_edits: iter::once((file_id, edit)).collect(),
            ..Default::default()
        }
    }

    /// Inserts a [`TextEdit`] for the given [`FileId`]. This properly handles merging existing
    /// edits for a file if some already exist.
    pub fn insert_source_edit(&mut self, file_id: FileId, edit: TextEdit) {
        match self.source_file_edits.entry(file_id) {
            Entry::Occupied(mut entry) => {
                never!(
                    entry.get_mut().union(edit).is_err(),
                    "overlapping edits for same file"
                );
            }
            Entry::Vacant(entry) => {
                entry.insert(edit);
            }
        }
    }

    /// Inserts a [`TextEdit`] for the given [`AnchoredPathBuf`]. This properly handles merging existing
    /// edits for a file if some already exist.
    pub fn insert_new_source_edit(&mut self, file_id: HashableAnchoredPathBuf, edit: TextEdit) {
        match self.new_file_edits.entry(file_id) {
            Entry::Occupied(mut entry) => {
                never!(
                    entry.get_mut().union(edit).is_err(),
                    "overlapping edits for same file"
                );
            }
            Entry::Vacant(entry) => {
                entry.insert(edit);
            }
        }
    }

    pub fn push_file_system_edit(&mut self, edit: FileSystemEdit) {
        self.file_system_edits.push(edit);
    }

    pub fn get_source_edit(&self, file_id: FileId) -> Option<&TextEdit> {
        self.source_file_edits.get(&file_id)
    }

    pub fn merge(mut self, other: SourceChange) -> SourceChange {
        self.extend(other.source_file_edits);
        self.extend(other.file_system_edits);
        self.extend(other.new_file_edits);
        self.is_snippet |= other.is_snippet;
        self
    }

    pub fn is_empty(&self) -> bool {
        self.source_file_edits.is_empty()
            && self.file_system_edits.is_empty()
            && self.new_file_edits.is_empty()
    }

    pub fn text_range(&self, file_id: FileId) -> Option<TextRange> {
        let edit = self.source_file_edits.get(&file_id)?;
        Some(
            edit.iter()
                .fold(TextRange::empty(0.into()), |acc, r| acc.cover(r.delete)),
        )
    }
}

impl Extend<(FileId, TextEdit)> for SourceChange {
    fn extend<T: IntoIterator<Item = (FileId, TextEdit)>>(&mut self, iter: T) {
        iter.into_iter()
            .for_each(|(file_id, edit)| self.insert_source_edit(file_id, edit));
    }
}

impl Extend<FileSystemEdit> for SourceChange {
    fn extend<T: IntoIterator<Item = FileSystemEdit>>(&mut self, iter: T) {
        iter.into_iter()
            .for_each(|edit| self.push_file_system_edit(edit));
    }
}

impl Extend<(HashableAnchoredPathBuf, TextEdit)> for SourceChange {
    fn extend<T: IntoIterator<Item = (HashableAnchoredPathBuf, TextEdit)>>(&mut self, iter: T) {
        iter.into_iter()
            .for_each(|(file_id, edit)| self.insert_new_source_edit(file_id, edit));
    }
}

impl From<FxHashMap<FileId, TextEdit>> for SourceChange {
    fn from(source_file_edits: FxHashMap<FileId, TextEdit>) -> SourceChange {
        SourceChange {
            source_file_edits,
            new_file_edits: FxHashMap::default(),
            file_system_edits: Vec::new(),
            is_snippet: false,
        }
    }
}

// ---------------------------------------------------------------------

#[derive(Debug)]
pub struct SourceChangeBuilder {
    edit: TextEditBuilder,
    edits_count: usize,

    drop_edits_in_changed_ranges: bool,
    changed_ranges: Vec<TextRange>,

    file_id: FileId,
    source_change: SourceChange,

    /// Maps the original, immutable `SyntaxNode` to a `clone_for_update` twin.
    mutated_tree: Option<TreeMutator>,
}

#[derive(Debug)]
pub struct TreeMutator {
    immutable: SyntaxNode,
    mutable_clone: SyntaxNode,
}

impl TreeMutator {}

impl SourceChangeBuilder {
    pub fn new(file_id: FileId) -> SourceChangeBuilder {
        SourceChangeBuilder {
            edit: TextEdit::builder(),
            edits_count: 0,
            changed_ranges: vec![],
            drop_edits_in_changed_ranges: false,
            file_id,
            source_change: SourceChange::default(),
            mutated_tree: None,
        }
    }

    pub fn edit_file(&mut self, file_id: FileId) {
        self.commit();
        self.file_id = file_id;
    }

    fn commit(&mut self) {
        if let Some(tm) = self.mutated_tree.take() {
            diff(&tm.immutable, &tm.mutable_clone).into_text_edit(&mut self.edit)
        }

        let edit = mem::take(&mut self.edit).finish();
        if !edit.is_empty() {
            self.source_change.insert_source_edit(self.file_id, edit);
        }
    }

    pub fn drop_edits_in_changed_ranges(&mut self, value: bool) {
        self.drop_edits_in_changed_ranges = value;
    }

    fn is_subsumed_by_existing_ranges(&self, range: TextRange) -> bool {
        self.changed_ranges
            .iter()
            .any(|changed_range| changed_range.cover(range) == *changed_range)
    }

    pub fn insert(&mut self, offset: TextSize, text: impl Into<String>) {
        if self.drop_edits_in_changed_ranges && self.edit.invalidates_offset(offset) {
            return;
        }
        self.edits_count += 1;
        self.edit.insert(offset, text.into())
    }
    /// Append specified `snippet` at the given `offset`
    pub fn insert_snippet(
        &mut self,
        _cap: SnippetCap,
        offset: TextSize,
        snippet: impl Into<String>,
    ) {
        self.source_change.is_snippet = true;
        self.insert(offset, snippet);
    }

    /// Remove specified `range` of text.
    pub fn delete(&mut self, range: TextRange) {
        if self.drop_edits_in_changed_ranges && self.is_subsumed_by_existing_ranges(range) {
            return;
        }
        self.edits_count += 1;
        self.changed_ranges.push(range);
        self.edit.delete(range)
    }
    /// Replaces specified `range` of text with a given string.
    pub fn replace(&mut self, range: TextRange, replace_with: impl Into<String>) {
        if self.drop_edits_in_changed_ranges && self.is_subsumed_by_existing_ranges(range) {
            return;
        }
        self.edits_count += 1;
        self.changed_ranges.push(range);
        self.edit.replace(range, replace_with.into())
    }

    pub fn edits_count(&self) -> usize {
        self.edits_count
    }

    pub fn finish(mut self) -> SourceChange {
        self.commit();
        mem::take(&mut self.source_change)
    }

    pub fn apply_source_change(&mut self, edits: SourceChange) {
        edits.source_file_edits.iter().for_each(|(file_id, edit)| {
            self.edit_file(*file_id);
            edit.iter().for_each(|e| {
                self.delete(e.delete);
                self.insert(e.delete.start(), e.insert.clone());
            });
        });
    }
}

// ---------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum FileSystemEdit {
    CreateFile {
        dst: AnchoredPathBuf,
        initial_contents: String,
    },
    MoveFile {
        src: FileId,
        dst: AnchoredPathBuf,
    },
}

impl From<FileSystemEdit> for SourceChange {
    fn from(edit: FileSystemEdit) -> SourceChange {
        SourceChange {
            source_file_edits: Default::default(),
            new_file_edits: Default::default(),
            file_system_edits: vec![edit],
            is_snippet: false,
        }
    }
}

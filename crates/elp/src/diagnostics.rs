/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// From https://github.com/rust-lang/rust-analyzer/blob/cf44953210cbfe189043417690fabd0037a6e74e/crates/rust-analyzer/src/diagnostics.rs

use std::mem;
use std::str::FromStr;

use elp_ide::elp_ide_db::elp_base_db::FileId;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use lsp_types::Diagnostic;

#[derive(Debug, Default, Clone)]
pub(crate) struct DiagnosticCollection {
    pub(crate) native: FxHashMap<FileId, Vec<Diagnostic>>,
    pub(crate) erlang_service: FxHashMap<FileId, Vec<Diagnostic>>,
    pub(crate) eqwalizer: FxHashMap<FileId, Vec<Diagnostic>>,
    pub(crate) edoc: FxHashMap<FileId, Vec<Diagnostic>>,
    changes: FxHashSet<FileId>,
}

impl DiagnosticCollection {
    pub fn set_native(&mut self, file_id: FileId, diagnostics: Vec<Diagnostic>) {
        if !are_all_diagnostics_equal(&self.native, file_id, &diagnostics) {
            set_diagnostics(&mut self.native, file_id, diagnostics);
            self.changes.insert(file_id);
        }
    }

    pub fn set_eqwalizer(&mut self, file_id: FileId, diagnostics: Vec<Diagnostic>) {
        if !are_all_diagnostics_equal(&self.eqwalizer, file_id, &diagnostics) {
            set_diagnostics(&mut self.eqwalizer, file_id, diagnostics);
            self.changes.insert(file_id);
        }
    }

    pub fn set_edoc(&mut self, file_id: FileId, diagnostics: Vec<Diagnostic>) {
        if !are_all_diagnostics_equal(&self.edoc, file_id, &diagnostics) {
            set_diagnostics(&mut self.edoc, file_id, diagnostics);
            self.changes.insert(file_id);
        }
    }

    pub fn set_erlang_service(&mut self, file_id: FileId, diagnostics: Vec<Diagnostic>) {
        if !are_all_diagnostics_equal(&self.erlang_service, file_id, &diagnostics) {
            set_diagnostics(&mut self.erlang_service, file_id, diagnostics);
            self.changes.insert(file_id);
        }
    }

    pub fn diagnostics_for(&self, file_id: FileId) -> impl Iterator<Item = &Diagnostic> {
        let native = self.native.get(&file_id).into_iter().flatten();
        let erlang_service = self.erlang_service.get(&file_id).into_iter().flatten();
        let eqwalizer = self.eqwalizer.get(&file_id).into_iter().flatten();
        let edoc = self.edoc.get(&file_id).into_iter().flatten();
        native.chain(erlang_service).chain(eqwalizer).chain(edoc)
    }

    pub fn take_changes(&mut self) -> Option<FxHashSet<FileId>> {
        if self.changes.is_empty() {
            return None;
        }
        Some(mem::take(&mut self.changes))
    }
}

fn are_all_diagnostics_equal(
    map: &FxHashMap<FileId, Vec<Diagnostic>>,
    file_id: FileId,
    new: &[Diagnostic],
) -> bool {
    let existing = map.get(&file_id).map(Vec::as_slice).unwrap_or_default();

    existing.len() == new.len()
        && new
            .iter()
            .zip(existing)
            .all(|(left, right)| are_diagnostics_equal(left, right))
}

fn are_diagnostics_equal(left: &Diagnostic, right: &Diagnostic) -> bool {
    left.source == right.source
        && left.severity == right.severity
        && left.range == right.range
        && left.message == right.message
}

fn set_diagnostics(
    map: &mut FxHashMap<FileId, Vec<Diagnostic>>,
    file_id: FileId,
    new: Vec<Diagnostic>,
) {
    if new.is_empty() {
        map.remove(&file_id);
    } else {
        map.insert(file_id, new);
    }
}

#[derive(Clone, Debug)]
pub enum DiagnosticSource {
    ErlangLsCompiler,
}

impl FromStr for DiagnosticSource {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Compiler" => Ok(DiagnosticSource::ErlangLsCompiler),
            unknown => Err(format!("Unknown DiagnosticSource: '{unknown}'")),
        }
    }
}

// ---------------------------------------------------------------------
#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn does_not_mark_change_from_empty_to_empty() {
        let mut diagnostics = DiagnosticCollection::default();
        let file_id = FileId(0);

        diagnostics.set_eqwalizer(file_id, vec![]);
        diagnostics.set_native(file_id, vec![]);

        assert_eq!(diagnostics.take_changes(), None);
        assert_eq!(diagnostics.diagnostics_for(file_id).next(), None);
    }

    #[test]
    fn resets_diagnostics() {
        let mut diagnostics = DiagnosticCollection::default();
        let file_id = FileId(0);

        let diagnostic = Diagnostic::default();

        // Set some diagnostic initially
        diagnostics.set_native(file_id, vec![diagnostic.clone()]);

        let changes = diagnostics.take_changes();
        let mut expected_changes = FxHashSet::default();
        expected_changes.insert(file_id);
        assert_eq!(changes.as_ref(), Some(&expected_changes));

        let stored = diagnostics.diagnostics_for(file_id).collect::<Vec<_>>();
        assert_eq!(stored, vec![&diagnostic]);

        // Reset to empty
        diagnostics.set_native(file_id, vec![]);

        let changes = diagnostics.take_changes();
        assert_eq!(changes.as_ref(), Some(&expected_changes));
        assert_eq!(diagnostics.diagnostics_for(file_id).next(), None);
    }
}

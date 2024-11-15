/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::mem;

use fxhash::FxHashMap;
use fxhash::FxHashSet;
use itertools::Itertools;

use crate::diagnostics::attach_related_diagnostics;
use crate::Diagnostic;
use crate::FileId;
use crate::LabeledDiagnostics;

#[derive(Debug, Default, Clone)]
pub struct DiagnosticCollection {
    pub(crate) native: FxHashMap<FileId, LabeledDiagnostics>,
    pub(crate) erlang_service: FxHashMap<FileId, LabeledDiagnostics>,
    pub(crate) eqwalizer: FxHashMap<FileId, Vec<Diagnostic>>,
    pub(crate) eqwalizer_project: FxHashMap<FileId, Vec<Diagnostic>>,
    pub(crate) edoc: FxHashMap<FileId, Vec<Diagnostic>>,
    pub(crate) ct: FxHashMap<FileId, Vec<Diagnostic>>,
    changes: FxHashSet<FileId>,
}

impl DiagnosticCollection {
    pub fn set_native(&mut self, file_id: FileId, diagnostics: LabeledDiagnostics) {
        if !are_all_labeled_diagnostics_equal(&self.native, file_id, &diagnostics) {
            set_labeled_diagnostics(&mut self.native, file_id, diagnostics);
            self.changes.insert(file_id);
        }
    }

    pub fn set_eqwalizer(&mut self, file_id: FileId, diagnostics: Vec<Diagnostic>) {
        if !are_all_diagnostics_equal(&self.eqwalizer, file_id, &diagnostics) {
            set_diagnostics(&mut self.eqwalizer, file_id, diagnostics);
            self.changes.insert(file_id);
        }
    }

    pub fn set_eqwalizer_project(&mut self, diagnostics: Vec<(FileId, Vec<Diagnostic>)>) {
        let mut visited = FxHashSet::default();
        for (file_id, diagnostics) in diagnostics {
            if !are_all_diagnostics_equal(&self.eqwalizer_project, file_id, &diagnostics) {
                set_diagnostics(&mut self.eqwalizer_project, file_id, diagnostics);
                self.changes.insert(file_id);
            };
            visited.insert(file_id);
        }
        let previous: Vec<FileId> = self.eqwalizer_project.keys().cloned().collect();
        self.eqwalizer_project
            .retain(|file_id, _| visited.contains(file_id));
        for file_id in previous {
            if !visited.contains(&file_id) {
                self.changes.insert(file_id);
            }
        }
    }

    pub fn set_edoc(&mut self, file_id: FileId, diagnostics: Vec<Diagnostic>) {
        if !are_all_diagnostics_equal(&self.edoc, file_id, &diagnostics) {
            set_diagnostics(&mut self.edoc, file_id, diagnostics);
            self.changes.insert(file_id);
        }
    }

    pub fn set_ct(&mut self, file_id: FileId, diagnostics: Vec<Diagnostic>) {
        if !are_all_diagnostics_equal(&self.ct, file_id, &diagnostics) {
            set_diagnostics(&mut self.ct, file_id, diagnostics);
            self.changes.insert(file_id);
        }
    }

    pub fn set_erlang_service(&mut self, file_id: FileId, diagnostics: LabeledDiagnostics) {
        if !are_all_labeled_diagnostics_equal(&self.erlang_service, file_id, &diagnostics) {
            set_labeled_diagnostics(&mut self.erlang_service, file_id, diagnostics);
            self.changes.insert(file_id);
        }
    }

    pub fn diagnostics_for(&self, file_id: FileId) -> Vec<Diagnostic> {
        let empty_diags = LabeledDiagnostics::default();
        let native = self.native.get(&file_id).unwrap_or(&empty_diags);
        let erlang_service = self.erlang_service.get(&file_id).unwrap_or(&empty_diags);
        let mut combined: Vec<Diagnostic> =
            attach_related_diagnostics(native.clone(), erlang_service.clone());
        let eqwalizer = self.eqwalizer.get(&file_id).into_iter().flatten().cloned();
        let eqwalizer_project = self
            .eqwalizer_project
            .get(&file_id)
            .into_iter()
            .flatten()
            .cloned();
        // The eqwalizer and eqwalizer_project could have duplicated entries by nature. Dedup.
        let eqwalizer_combined = eqwalizer
            .into_iter()
            .chain(eqwalizer_project)
            .dedup_by(|a, b| are_diagnostics_equal(a, b));
        let edoc = self.edoc.get(&file_id).into_iter().flatten().cloned();
        let ct = self.ct.get(&file_id).into_iter().flatten().cloned();
        combined.extend(eqwalizer_combined);
        combined.extend(edoc);
        combined.extend(ct);
        combined
    }

    pub fn move_eqwalizer_diagnostics_to_project_diagnostics(&mut self, file_id: FileId) {
        let diagnostics = self
            .eqwalizer
            .get(&file_id)
            .into_iter()
            .flatten()
            .cloned()
            .collect();
        set_diagnostics(&mut self.eqwalizer, file_id, Vec::new());
        set_diagnostics(&mut self.eqwalizer_project, file_id, diagnostics);
    }

    pub fn project_diagnostics_for(&self, file_id: FileId) -> Vec<Diagnostic> {
        let eqwalizer = self.eqwalizer.get(&file_id).into_iter().flatten().cloned();
        let eqwalizer_project = self
            .eqwalizer_project
            .get(&file_id)
            .into_iter()
            .flatten()
            .cloned();
        // The eqwalizer and eqwalizer_project could have duplicated entries by nature. Dedup.
        eqwalizer
            .into_iter()
            .chain(eqwalizer_project)
            .dedup_by(|a, b| are_diagnostics_equal(a, b))
            .collect()
    }

    pub fn take_changes(&mut self) -> Option<FxHashSet<FileId>> {
        if self.changes.is_empty() {
            return None;
        }
        Some(mem::take(&mut self.changes))
    }

    pub fn return_changes(&mut self, changes: FxHashSet<FileId>) {
        self.changes.extend(changes);
    }

    pub fn is_empty(&self) -> bool {
        // Use a match to ensure we check all fields, future proofing
        let DiagnosticCollection {
            native,
            erlang_service,
            eqwalizer,
            eqwalizer_project,
            edoc,
            ct,
            changes,
        } = self;
        native.is_empty()
            && erlang_service.is_empty()
            && eqwalizer.is_empty()
            && eqwalizer_project.is_empty()
            && edoc.is_empty()
            && ct.is_empty()
            && changes.is_empty()
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

fn are_all_labeled_diagnostics_equal(
    map: &FxHashMap<FileId, LabeledDiagnostics>,
    file_id: FileId,
    new: &LabeledDiagnostics,
) -> bool {
    let empty_diags = LabeledDiagnostics::default();
    let existing = map.get(&file_id).unwrap_or(&empty_diags);

    // len() is a coarse sanity check, it does not count the actual
    // number of diagnostics. In particular, there is no guarantee that
    // x.len() == x.iter().len()
    existing.len() == new.len() && {
        itertools::equal(
            existing.iter().map(CompareDiagnostic),
            new.iter().map(CompareDiagnostic),
        )
    }
}

#[derive(Debug)]
struct CompareDiagnostic<'a>(&'a Diagnostic);

impl PartialEq for CompareDiagnostic<'_> {
    fn eq(&self, other: &Self) -> bool {
        are_diagnostics_equal(self.0, other.0)
    }
}

fn are_diagnostics_equal(left: &Diagnostic, right: &Diagnostic) -> bool {
    left.code == right.code
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

fn set_labeled_diagnostics(
    map: &mut FxHashMap<FileId, LabeledDiagnostics>,
    file_id: FileId,
    new: LabeledDiagnostics,
) {
    if new.is_empty() {
        map.remove(&file_id);
    } else {
        map.insert(file_id, new);
    }
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {

    use std::iter::once;

    use elp_ide_db::elp_base_db::FileId;
    use elp_syntax::label::Label;
    use fxhash::FxHashMap;
    use fxhash::FxHashSet;
    use text_edit::TextRange;

    use super::are_diagnostics_equal;
    use crate::diagnostics;
    use crate::diagnostics::attach_related_diagnostics;
    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::LabeledDiagnostics;
    use crate::diagnostics::Severity;
    use crate::diagnostics_collection::are_all_labeled_diagnostics_equal;
    use crate::diagnostics_collection::DiagnosticCollection;
    use crate::elp_ide_db::elp_base_db::fixture::extract_annotations;
    use crate::elp_ide_db::elp_base_db::fixture::WithFixture;
    use crate::elp_ide_db::elp_base_db::FileLoader;
    use crate::elp_ide_db::RootDatabase;
    use crate::DiagnosticsConfig;

    fn are_diagnostics_equal_vec(old: &[Diagnostic], new: &[Diagnostic]) -> bool {
        new.iter()
            .zip(old)
            .all(|(left, right)| are_diagnostics_equal(left, right))
    }

    #[test]
    fn does_not_mark_change_from_empty_to_empty() {
        let (_db, file_id) = RootDatabase::with_single_file(
            r#"
            -module(test).
            "#,
        );
        let mut diagnostics = DiagnosticCollection::default();

        diagnostics.set_eqwalizer(file_id, vec![]);
        diagnostics.set_native(file_id, LabeledDiagnostics::default());

        assert_eq!(diagnostics.take_changes(), None);
        assert_eq!(diagnostics.diagnostics_for(file_id).len(), 0);
    }

    #[test]
    fn resets_diagnostics() {
        let (_db, file_id) = RootDatabase::with_single_file(
            r#"
            -module(test).
            "#,
        );
        let mut diagnostics = DiagnosticCollection::default();

        let diagnostic = Diagnostic::default();

        // Set some diagnostic initially
        diagnostics.set_native(file_id, LabeledDiagnostics::new(vec![diagnostic.clone()]));

        let changes = diagnostics.take_changes();
        let mut expected_changes = FxHashSet::default();
        expected_changes.insert(file_id);
        assert_eq!(changes.as_ref(), Some(&expected_changes));

        let stored = diagnostics.diagnostics_for(file_id);
        assert!(are_diagnostics_equal_vec(&stored, &vec![diagnostic]),);

        // Reset to empty
        diagnostics.set_native(file_id, LabeledDiagnostics::new(vec![]));

        let changes = diagnostics.take_changes();
        assert_eq!(changes.as_ref(), Some(&expected_changes));
        assert_eq!(diagnostics.diagnostics_for(file_id).len(), 0);
    }

    // -----------------------------------------------------------------

    #[track_caller]
    pub(crate) fn check_diagnostics_with_config_and_extra(
        config: DiagnosticsConfig,
        extra_diags: &LabeledDiagnostics,
        elp_fixture: &str,
    ) {
        let (db, files, _diagnostics_enabled) = RootDatabase::with_many_files(elp_fixture);
        for file_id in files {
            let diagnostics = diagnostics::native_diagnostics(&db, &config, &vec![], file_id);

            let combined = attach_related_diagnostics(diagnostics, extra_diags.clone());
            let expected = extract_annotations(&db.file_text(file_id));
            let mut actual = combined
                .into_iter()
                .map(|d| {
                    let mut annotation = String::new();
                    annotation.push_str(match d.severity {
                        Severity::Error => "error",
                        Severity::Warning => "warning",
                        Severity::Information => "information",
                        Severity::WeakWarning => "hint",
                    });
                    annotation.push_str(": ");
                    annotation.push_str(&d.message);
                    (d.range, annotation)
                })
                .collect::<Vec<_>>();
            actual.sort_by_key(|(range, _)| range.start());
            assert_eq!(expected, actual);
        }
    }

    fn make_diag(message: &str, code: &str, range: TextRange) -> Diagnostic {
        Diagnostic::new(code.into(), message, range)
    }

    #[test]
    fn group_related_diagnostics() {
        let labeled_undefined_errors = FxHashMap::from_iter([(
            Some(diagnostics::DiagnosticLabel::MFA(Label::new_raw("foo/0"))),
            vec![
                make_diag(
                    "function foo/0 undefined",
                    "L1227",
                    TextRange::new(3.into(), 5.into()),
                ),
                make_diag(
                    "function foo/0 undefined",
                    "L1227",
                    TextRange::new(3.into(), 5.into()),
                ),
                make_diag(
                    "spec for undefined function foo/0",
                    "L1308",
                    TextRange::new(8.into(), 10.into()),
                ),
            ],
        )]);
        let labeled_syntax_errors = FxHashMap::from_iter([(
            Some(diagnostics::DiagnosticLabel::MFA(Label::new_raw("foo/0"))),
            vec![make_diag(
                "syntax error before: '->'",
                "P1711",
                TextRange::new(106.into(), 110.into()),
            )],
        )]);
        let extra_diags = LabeledDiagnostics {
            normal: vec![],
            labeled_syntax_errors,
            labeled_undefined_errors,
        };

        let config = DiagnosticsConfig::default();
        check_diagnostics_with_config_and_extra(
            config,
            &extra_diags,
            r#"
             -module(main).

             -export([foo/0,bar/0]).

             -spec bar() -> ok.
             bar() -> foo().

             -spec foo() -> ok.
             foo( -> ok. %%
             %%  ^ error: Missing ')'
            "#,
        );
    }

    #[test]
    fn are_labeled_diagnostics_equal() {
        let labeled_one = FxHashMap::from_iter([(
            None,
            vec![make_diag(
                "function foo/0 undefined",
                "L1227",
                TextRange::new(3.into(), 5.into()),
            )],
        )]);
        let labeled_two = FxHashMap::from_iter([(
            None,
            vec![
                make_diag(
                    "function foo/0 undefined",
                    "L1227",
                    TextRange::new(3.into(), 5.into()),
                ),
                make_diag(
                    "spec for undefined function foo/0",
                    "L1308",
                    TextRange::new(8.into(), 10.into()),
                ),
            ],
        )]);
        let diags_one = LabeledDiagnostics {
            normal: vec![],
            labeled_syntax_errors: FxHashMap::default(),
            labeled_undefined_errors: labeled_one,
        };
        let diags_two = LabeledDiagnostics {
            normal: vec![],
            labeled_syntax_errors: labeled_two,
            labeled_undefined_errors: FxHashMap::default(),
        };

        let file_id = FileId::from_raw(0);
        assert_eq!(
            are_all_labeled_diagnostics_equal(
                &FxHashMap::from_iter(once((file_id, diags_one))),
                file_id,
                &diags_two,
            ),
            false
        );
    }
}

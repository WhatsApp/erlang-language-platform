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

use elp_ide::diagnostics::already_reported;
use elp_ide::diagnostics::LabeledDiagnostics;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_syntax::label::Label;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use itertools::Itertools;
use lsp_types::Diagnostic;
use lsp_types::DiagnosticRelatedInformation;
use lsp_types::Location;
use lsp_types::Url;

#[derive(Debug, Default, Clone)]
pub(crate) struct DiagnosticCollection {
    pub(crate) native: FxHashMap<FileId, LabeledDiagnostics<Diagnostic>>,
    pub(crate) erlang_service: FxHashMap<FileId, LabeledDiagnostics<Diagnostic>>,
    pub(crate) eqwalizer: FxHashMap<FileId, Vec<Diagnostic>>,
    pub(crate) edoc: FxHashMap<FileId, Vec<Diagnostic>>,
    pub(crate) ct: FxHashMap<FileId, Vec<Diagnostic>>,
    changes: FxHashSet<FileId>,
}

impl DiagnosticCollection {
    pub fn set_native(&mut self, file_id: FileId, diagnostics: LabeledDiagnostics<Diagnostic>) {
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

    pub fn set_erlang_service(
        &mut self,
        file_id: FileId,
        diagnostics: LabeledDiagnostics<Diagnostic>,
    ) {
        if !are_all_labeled_diagnostics_equal(&self.erlang_service, file_id, &diagnostics) {
            set_labeled_diagnostics(&mut self.erlang_service, file_id, diagnostics);
            self.changes.insert(file_id);
        }
    }

    pub fn diagnostics_for<'a>(&'a mut self, file_id: FileId, url: &'a Url) -> Vec<Diagnostic> {
        let empty_diags = LabeledDiagnostics::default();
        let native = self.native.get(&file_id).unwrap_or(&empty_diags);
        let erlang_service = self.erlang_service.get(&file_id).unwrap_or(&empty_diags);
        let mut combined = attach_related_diagnostics(url, native, erlang_service);

        let eqwalizer = self.eqwalizer.get(&file_id).into_iter().flatten().cloned();
        let edoc = self.edoc.get(&file_id).into_iter().flatten().cloned();
        let ct = self.ct.get(&file_id).into_iter().flatten().cloned();
        combined.extend(eqwalizer);
        combined.extend(edoc);
        combined.extend(ct);
        combined
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

fn are_all_labeled_diagnostics_equal(
    map: &FxHashMap<FileId, LabeledDiagnostics<Diagnostic>>,
    file_id: FileId,
    new: &LabeledDiagnostics<Diagnostic>,
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

fn set_labeled_diagnostics(
    map: &mut FxHashMap<FileId, LabeledDiagnostics<Diagnostic>>,
    file_id: FileId,
    new: LabeledDiagnostics<Diagnostic>,
) {
    if new.is_empty() {
        map.remove(&file_id);
    } else {
        map.insert(file_id, new);
    }
}

// ---------------------------------------------------------------------

/// Combine the ELP and erlang_service diagnostics.  In particular,
/// flatten any cascading diagnostics if possible.
pub fn attach_related_diagnostics(
    url: &Url,
    native: &LabeledDiagnostics<lsp_types::Diagnostic>,
    erlang_service: &LabeledDiagnostics<lsp_types::Diagnostic>,
) -> Vec<lsp_types::Diagnostic> {
    // `native` is labeled with the MFA of functions having syntax
    // errors in them. `erlang_service` is labeled with the MFA of
    // undefined functions.  For each labeled one in `native`, add the
    // corresponding ones from `erlang_service`, remember the label,
    // and when done delete all `erlang_service` diagnostics with that
    // label.

    // There is a many-to-many relationship between syntax errors and
    // related info, because there can be more than one syntax error
    // in a given function.  So we have to keep the original lookup
    // intact, then remove ones that are used at least once at the
    // end.
    let mut to_remove: FxHashSet<&Option<Label>> = FxHashSet::default();
    let updated = native
        .labeled
        .iter()
        .flat_map(|(label, diags)| {
            if let Some(related) = erlang_service.labeled.get(label) {
                to_remove.insert(label);
                let related_info = related
                    .iter()
                    .map(|(_, d)| as_related(d, url))
                    .collect_vec();
                diags
                    .iter()
                    .map(|(r, d)| (*r, with_related(d.clone(), related_info.clone())))
                    .collect_vec()
            } else {
                diags.to_vec()
            }
        })
        .collect_vec();

    let es = erlang_service
        .labeled
        .iter()
        .filter(|(k, _)| !to_remove.contains(k))
        .flat_map(|(_, v)| v)
        .filter(|(r, _)| !already_reported(&native.syntax_error_form_ranges, r))
        .cloned();

    native
        .normal
        .clone()
        .into_iter()
        .chain(updated)
        .chain(es)
        .map(|(_, d)| d)
        .collect_vec()
}

// ---------------------------------------------------------------------

fn with_related(
    mut diagnostic: Diagnostic,
    related: Vec<DiagnosticRelatedInformation>,
) -> Diagnostic {
    diagnostic.related_information = Some(related);
    diagnostic
}

fn as_related(diagnostic: &Diagnostic, url: &Url) -> DiagnosticRelatedInformation {
    DiagnosticRelatedInformation {
        location: Location::new(url.clone(), diagnostic.range),
        message: diagnostic.message.clone(),
    }
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {

    use std::iter::once;

    use elp_ide::diagnostics;
    use elp_ide::diagnostics::DiagnosticCode;
    use elp_ide::diagnostics::DiagnosticsConfig;
    use elp_ide::elp_ide_db::elp_base_db::fixture::extract_annotations;
    use elp_ide::elp_ide_db::elp_base_db::fixture::WithFixture;
    use elp_ide::elp_ide_db::elp_base_db::FileLoader;
    use elp_ide::elp_ide_db::LineIndexDatabase;
    use elp_ide::elp_ide_db::RootDatabase;
    use lsp_types::DiagnosticSeverity;
    use lsp_types::NumberOrString;
    use lsp_types::Position;
    use lsp_types::Range;
    use range_set::RangeSet;
    use text_edit::TextRange;

    use super::*;
    use crate::convert::ide_to_lsp_diagnostic;
    use crate::from_proto;

    #[test]
    fn does_not_mark_change_from_empty_to_empty() {
        let url = lsp_types::Url::parse("file:///foo").ok().unwrap();
        let (_db, file_id) = RootDatabase::with_single_file(
            r#"
            -module(test).
            "#,
        );
        let mut diagnostics = DiagnosticCollection::default();

        diagnostics.set_eqwalizer(file_id, vec![]);
        diagnostics.set_native(file_id, LabeledDiagnostics::default());

        assert_eq!(diagnostics.take_changes(), None);
        assert_eq!(diagnostics.diagnostics_for(file_id, &url).len(), 0);
    }

    #[test]
    fn resets_diagnostics() {
        let url = lsp_types::Url::parse("file:///foo").ok().unwrap();
        let (_db, file_id) = RootDatabase::with_single_file(
            r#"
            -module(test).
            "#,
        );
        let mut diagnostics = DiagnosticCollection::default();

        let diagnostic = Diagnostic::default();
        let text_range = TextRange::new(0.into(), 0.into());

        // Set some diagnostic initially
        diagnostics.set_native(
            file_id,
            LabeledDiagnostics::new(vec![(text_range, diagnostic.clone())]),
        );

        let changes = diagnostics.take_changes();
        let mut expected_changes = FxHashSet::default();
        expected_changes.insert(file_id);
        assert_eq!(changes.as_ref(), Some(&expected_changes));

        let stored = diagnostics.diagnostics_for(file_id, &url);
        assert_eq!(stored, vec![diagnostic]);

        // Reset to empty
        diagnostics.set_native(file_id, LabeledDiagnostics::new(vec![]));

        let changes = diagnostics.take_changes();
        assert_eq!(changes.as_ref(), Some(&expected_changes));
        assert_eq!(diagnostics.diagnostics_for(file_id, &url).len(), 0);
    }

    // -----------------------------------------------------------------

    #[track_caller]
    pub(crate) fn check_diagnostics_with_config_and_extra(
        config: DiagnosticsConfig,
        extra_diags: &LabeledDiagnostics<Diagnostic>,
        elp_fixture: &str,
    ) {
        let (db, files) = RootDatabase::with_many_files(elp_fixture);
        let url = lsp_types::Url::parse("file:///foo").ok().unwrap();
        for file_id in files {
            let line_index = db.file_line_index(file_id);
            let diagnostics = diagnostics::diagnostics(&db, &config, file_id, true);
            let diagnostics = diagnostics.convert(&|d| ide_to_lsp_diagnostic(&line_index, &url, d));

            let combined = attach_related_diagnostics(&url, &diagnostics, extra_diags);
            let expected = extract_annotations(&db.file_text(file_id));
            let mut actual = combined
                .into_iter()
                .map(|d| {
                    let mut annotation = String::new();
                    annotation.push_str(match d.severity {
                        Some(DiagnosticSeverity::ERROR) => "error",
                        Some(DiagnosticSeverity::WARNING) => "warning",
                        Some(DiagnosticSeverity::INFORMATION) => "information",
                        Some(DiagnosticSeverity::HINT) => "hint",
                        _ => "unknown",
                    });
                    annotation.push_str(": ");
                    annotation.push_str(&d.message);
                    (from_proto::text_range(&line_index, d.range), annotation)
                })
                .collect::<Vec<_>>();
            actual.sort_by_key(|(range, _)| range.start());
            assert_eq!(expected, actual);
        }
    }

    fn make_diag(
        message: &str,
        code: &str,
        line: u32,
        cols: u32,
        cole: u32,
    ) -> (TextRange, Diagnostic) {
        (
            TextRange::new(0.into(), 0.into()),
            Diagnostic {
                message: message.to_string(),
                range: Range::new(Position::new(line, cols), Position::new(line, cole)),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String(code.into())),
                code_description: None,
                source: None,
                related_information: None,
                tags: None,
                data: None,
            },
        )
    }

    #[test]
    fn group_related_diagnostics() {
        let labeled = FxHashMap::from_iter([(
            Some(Label::new_raw("foo/0")),
            vec![
                make_diag("function foo/0 undefined", "L1227", 3, 1, 2),
                make_diag("function foo/0 undefined", "L1227", 3, 6, 7),
                make_diag("spec for undefined function foo/0", "L1308", 8, 1, 2),
            ],
        )]);
        let extra_diags = LabeledDiagnostics {
            syntax_error_form_ranges: RangeSet::from_elements(vec![]),
            normal: vec![make_diag("syntax error before: '->'", "P1711", 8, 10, 12)],
            labeled,
        };

        let config =
            DiagnosticsConfig::default().disable(DiagnosticCode::MissingCompileWarnMissingSpec);
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
             %%  ^ error: Syntax Error: Missing )
            "#,
        );
    }

    #[test]
    fn are_labeled_diagnostics_equal() {
        let labeled_one = FxHashMap::from_iter([(
            None,
            vec![make_diag("function foo/0 undefined", "L1227", 3, 1, 2)],
        )]);
        let labeled_two = FxHashMap::from_iter([(
            None,
            vec![
                make_diag("function foo/0 undefined", "L1227", 3, 1, 2),
                make_diag("spec for undefined function foo/0", "L1308", 8, 1, 2),
            ],
        )]);
        let diags_one = LabeledDiagnostics {
            syntax_error_form_ranges: RangeSet::from_elements(vec![]),
            normal: vec![],
            labeled: labeled_one,
        };
        let diags_two = LabeledDiagnostics {
            syntax_error_form_ranges: RangeSet::from_elements(vec![]),
            normal: vec![],
            labeled: labeled_two,
        };

        let file_id = FileId(0);
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

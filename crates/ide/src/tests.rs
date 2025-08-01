/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// To run the tests via cargo
// cargo test --package elp_ide --lib

use elp_ide_assists::Assist;
use elp_ide_db::RootDatabase;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::elp_base_db::SourceDatabaseExt;
use elp_ide_db::elp_base_db::assert_eq_text;
use elp_ide_db::elp_base_db::fixture::WithFixture;
use elp_ide_db::elp_base_db::remove_annotations;
use elp_project_model::test_fixture::trim_indent;
use elp_text_edit::TextRange;
use expect_test::Expect;
use itertools::Itertools;

use crate::Analysis;
use crate::AnalysisHost;
use crate::DiagnosticsConfig;
use crate::NavigationTarget;
use crate::diagnostics;
use crate::diagnostics::AdhocSemanticDiagnostics;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::LabeledDiagnostics;
use crate::diagnostics::Severity;
use crate::fixture;

#[track_caller]
pub(crate) fn check_ct_fix(fixture_before: &str, fixture_after: &str) {
    let config = DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction);
    let diagnostic_filter = &|_d: &Diagnostic| true;
    let assist_filter = &|_d: &Assist| true;
    check_filtered_ct_fix_with_config(
        fixture_before,
        fixture_after,
        config,
        &vec![],
        diagnostic_filter,
        assist_filter,
    );
}

#[track_caller]
pub(crate) fn check_filtered_ct_fix(
    fixture_before: &str,
    fixture_after: &str,
    diagnostic_filter: &dyn Fn(&Diagnostic) -> bool,
    assist_filter: &dyn Fn(&Assist) -> bool,
) {
    let config = DiagnosticsConfig::default().disable(DiagnosticCode::UndefinedFunction);
    check_filtered_ct_fix_with_config(
        fixture_before,
        fixture_after,
        config,
        &vec![],
        diagnostic_filter,
        assist_filter,
    );
}

#[track_caller]
pub(crate) fn check_filtered_ct_fix_with_config(
    fixture_before: &str,
    fixture_after: &str,
    config: DiagnosticsConfig,
    adhoc_semantic_diagnostics: &Vec<&dyn AdhocSemanticDiagnostics>,
    diagnostic_filter: &dyn Fn(&Diagnostic) -> bool,
    assist_filter: &dyn Fn(&Assist) -> bool,
) {
    let after = trim_indent(fixture_after);
    let (analysis, pos, diagnostics_enabled) = fixture::position(fixture_before);
    diagnostics_enabled.assert_ct_enabled();

    check_no_parse_errors_with_config(&analysis, pos.file_id, &config, adhoc_semantic_diagnostics);

    let diagnostics = fixture::diagnostics_for(
        &analysis,
        pos.file_id,
        &config,
        adhoc_semantic_diagnostics,
        &diagnostics_enabled,
    );
    let diagnostic = diagnostics
        .diagnostics_for(pos.file_id)
        .into_iter()
        .filter(diagnostic_filter)
        .next_back()
        .expect("no diagnostics")
        .clone();
    let fixes = &diagnostic
        .fixes
        .expect("diagnostic misses fixes")
        .into_iter()
        .filter(assist_filter)
        .collect::<Vec<_>>();
    let fix = fixes.first().expect("filtered fixes are empty");
    let actual = {
        let source_change = fix.source_change.as_ref().unwrap();
        let file_id = *source_change.source_file_edits.keys().next().unwrap();
        let mut actual = analysis.db.file_text(file_id).to_string();

        for edit in source_change.source_file_edits.values() {
            edit.apply(&mut actual);
        }
        actual
    };
    assert!(
        fix.target.contains_inclusive(pos.offset),
        "diagnostic fix range {:?} does not touch cursor position {:?}",
        fix.target,
        pos.offset
    );
    assert_eq_text!(&after, &actual);
}

/// Takes a multi-file input fixture with annotated cursor positions,
/// and checks that:
///  * a diagnostic is produced
///  * the first diagnostic fix trigger range touches the input cursor position
///  * that the contents of the file containing the cursor match `after` after the diagnostic fix is applied
#[track_caller]
pub(crate) fn check_fix(fixture_before: &str, fixture_after: Expect) {
    let config = DiagnosticsConfig::default().set_experimental(true);
    check_nth_fix(
        0,
        fixture_before,
        fixture_after,
        config,
        &vec![],
        IncludeCodeActionAssists::No,
    );
}

///  Like `check_fix` but with a custom DiagnosticsConfig
#[track_caller]
pub(crate) fn check_fix_with_config(
    config: DiagnosticsConfig,
    fixture_before: &str,
    fixture_after: Expect,
) {
    check_fix_with_config_and_adhoc(config, &vec![], fixture_before, fixture_after);
}

#[track_caller]
pub(crate) fn check_fix_with_config_and_adhoc(
    config: DiagnosticsConfig,
    adhoc_semantic_diagnostics: &Vec<&dyn AdhocSemanticDiagnostics>,
    fixture_before: &str,
    fixture_after: Expect,
) {
    check_nth_fix(
        0,
        fixture_before,
        fixture_after,
        config,
        adhoc_semantic_diagnostics,
        IncludeCodeActionAssists::No,
    );
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IncludeCodeActionAssists {
    Yes,
    No,
}

#[track_caller]
#[allow(unused)]
pub(crate) fn check_fix_including_assists(fixture_before: &str, fixture_after: Expect) {
    let config = DiagnosticsConfig::default().set_experimental(true);
    check_nth_fix(
        0,
        fixture_before,
        fixture_after,
        config,
        &vec![],
        IncludeCodeActionAssists::Yes,
    )
}

#[track_caller]
pub(crate) fn check_nth_fix(
    nth: usize,
    fixture_before: &str,
    fixture_after: Expect,
    config: DiagnosticsConfig,
    adhoc_semantic_diagnostics: &Vec<&dyn AdhocSemanticDiagnostics>,
    include_assists: IncludeCodeActionAssists,
) {
    let (analysis, pos, diagnostics_enabled) = fixture::position(fixture_before);

    let diagnostics = fixture::diagnostics_for(
        &analysis,
        pos.file_id,
        &config,
        adhoc_semantic_diagnostics,
        &diagnostics_enabled,
    );
    let fix = diagnostics
        .diagnostics_for(pos.file_id)
        .into_iter()
        .filter_map(|d| {
            let fixes = get_fixes(&analysis, include_assists, pos, d);
            let fix = fixes.get(nth)?;
            if fix.target.contains_inclusive(pos.offset) {
                Some(fix.clone())
            } else {
                None
            }
        })
        .next_back()
        .expect("no diagnostics")
        .clone();
    let actual = {
        let source_change = fix.source_change.as_ref().unwrap();
        let file_id = *source_change.source_file_edits.keys().next().unwrap();
        let mut actual = analysis.db.file_text(file_id).to_string();

        for edit in source_change.source_file_edits.values() {
            edit.apply(&mut actual);
        }
        actual
    };
    let actual = remove_annotations(None, &actual);
    assert!(
        fix.target.contains_inclusive(pos.offset),
        "diagnostic fix range {:?} does not touch cursor position {:?}",
        fix.target,
        pos.offset
    );
    fixture_after.assert_eq(&actual);
}

fn get_fixes(
    analysis: &Analysis,
    include_assists: IncludeCodeActionAssists,
    pos: FilePosition,
    diagnostic: Diagnostic,
) -> Vec<Assist> {
    match include_assists {
        IncludeCodeActionAssists::Yes => diagnostic.get_diagnostic_fixes(&analysis.db, pos.file_id),
        IncludeCodeActionAssists::No => diagnostic.fixes.unwrap_or_default(),
    }
}

/// Takes a multi-file input fixture with annotated cursor positions,
/// and checks that:
///  * a diagnostic is produced
///  * the first diagnostic fix trigger range touches the input cursor position
///  * that the contents of the file containing the cursor match `after` after the diagnostic fix is applied
#[track_caller]
pub(crate) fn check_specific_fix(assist_label: &str, fixture_before: &str, fixture_after: Expect) {
    let config = DiagnosticsConfig::default().set_experimental(true);
    check_specific_fix_with_config(Some(assist_label), fixture_before, fixture_after, config);
}

#[track_caller]
pub(crate) fn check_specific_fix_with_config(
    assist_label: Option<&str>,
    fixture_before: &str,
    fixture_after: Expect,
    config: DiagnosticsConfig,
) {
    check_specific_fix_with_config_and_adhoc(
        assist_label,
        fixture_before,
        fixture_after,
        config,
        &vec![],
    );
}

#[track_caller]
pub(crate) fn check_specific_fix_with_config_and_adhoc(
    assist_label: Option<&str>,
    fixture_before: &str,
    fixture_after: Expect,
    config: DiagnosticsConfig,
    adhoc_semantic_diagnostics: &Vec<&dyn AdhocSemanticDiagnostics>,
) {
    let trimmed_fixture_before = trim_indent(fixture_before);
    let (analysis, fixture) = fixture::with_fixture(&trimmed_fixture_before);
    let file_id = fixture.file_id();
    let pos = fixture.position();
    let diagnostics = fixture::diagnostics_for(
        &analysis,
        file_id,
        &config,
        adhoc_semantic_diagnostics,
        &fixture.diagnostics_enabled,
    );
    let diagnostics = diagnostics.diagnostics_for(file_id);

    let expected = fixture.annotations_by_file_id(&file_id);
    let actual = convert_diagnostics_to_annotations(diagnostics.clone());
    assert_eq!(expected, actual);

    let fix: Assist = if let Some(label) = assist_label {
        let fixes: Vec<_> = diagnostics
            .iter()
            .flat_map(|d| d.fixes.clone().unwrap_or_default())
            .filter(|f| f.label == label)
            .collect();
        if fixes.len() == 1 {
            fixes.into_iter().next().unwrap()
        } else if fixes.len() > 1 {
            panic!(
                "Expecting one \"{}\", but multiple found in {:?}",
                label,
                diagnostics
                    .iter()
                    .flat_map(|d| match d.fixes.as_ref() {
                        None => vec![],
                        Some(fixes) => fixes
                            .iter()
                            .map(|f| (d.code.clone(), f.label.clone()))
                            .collect_vec(),
                    })
                    .collect_vec()
            );
        } else {
            panic!(
                "Expecting \"{}\", but not found in {:?}",
                label,
                diagnostics
                    .iter()
                    .flat_map(|d| match d.fixes.as_ref() {
                        None => vec![],
                        Some(fixes) => fixes
                            .iter()
                            .map(|f| (d.code.clone(), f.label.clone()))
                            .collect_vec(),
                    })
                    .collect_vec()
            );
        }
    } else {
        panic!("No assists found");
    };

    let actual = {
        let source_change = fix.source_change.as_ref().unwrap();
        let file_id = *source_change.source_file_edits.keys().next().unwrap();
        let mut actual = analysis.db.file_text(file_id).to_string();

        for edit in source_change.source_file_edits.values() {
            edit.apply(&mut actual);
        }
        actual
    };
    let actual = remove_annotations(None, &actual);
    let actual = trim_indent(&actual);
    assert!(
        fix.target.contains_inclusive(pos.offset),
        "diagnostic fix range {:?} does not touch cursor position {:?}",
        fix.target,
        pos.offset
    );
    fixture_after.assert_eq(&actual);
}

#[track_caller]
pub(crate) fn check_diagnostics(fixture: &str) {
    let config = DiagnosticsConfig::default()
        .set_experimental(true)
        .disable(DiagnosticCode::UnspecificInclude)
        .disable(DiagnosticCode::BinaryStringToSigil);
    check_diagnostics_with_config(config, fixture)
}

fn convert_diagnostics_to_annotations(diagnostics: Vec<Diagnostic>) -> Vec<(TextRange, String)> {
    let mut actual = diagnostics
        .into_iter()
        .map(|d| {
            let mut annotation = String::new();
            if let Some(fixes) = &d.fixes {
                assert!(!fixes.is_empty());
                annotation.push_str("💡 ")
            }
            annotation.push_str(match d.severity {
                Severity::Error => "error",
                Severity::Warning => "warning",
                Severity::WeakWarning => "weak",
                Severity::Information => "information",
            });
            annotation.push_str(": ");
            annotation.push_str(&convert_diagnostic_message(&d));
            (d.range, annotation)
        })
        .collect::<Vec<_>>();
    actual.sort_by_key(|(range, _)| range.start());
    actual
}

fn convert_diagnostic_message(d: &Diagnostic) -> String {
    match &d.code {
        DiagnosticCode::Eqwalizer(_) => d.code.as_code(),
        _ => d.message.clone(),
    }
}

#[track_caller]
pub(crate) fn check_ct_diagnostics(elp_fixture: &str) {
    let (analysis, fixture) = fixture::with_fixture(elp_fixture);
    fixture.diagnostics_enabled.assert_ct_enabled();
    let file_id = fixture.file_id();
    let config = DiagnosticsConfig::default();
    let diagnostics = fixture::diagnostics_for(
        &analysis,
        file_id,
        &config,
        &vec![],
        &fixture.diagnostics_enabled,
    );
    let diagnostics = diagnostics.diagnostics_for(file_id);
    let expected = fixture.annotations_by_file_id(&file_id);
    let actual = convert_diagnostics_to_annotations(diagnostics);
    assert_eq!(expected, actual);
}

#[track_caller]
pub(crate) fn check_diagnostics_with_config(config: DiagnosticsConfig, elp_fixture: &str) {
    check_diagnostics_with_config_and_ad_hoc(config, &vec![], elp_fixture);
}

#[track_caller]
pub(crate) fn check_diagnostics_with_config_and_ad_hoc(
    config: DiagnosticsConfig,
    adhoc_semantic_diagnostics: &Vec<&dyn AdhocSemanticDiagnostics>,
    elp_fixture: &str,
) {
    let (db, fixture) = RootDatabase::with_fixture(elp_fixture);
    let host = AnalysisHost { db };
    let analysis = host.analysis();
    for file_id in &fixture.files {
        let file_id = *file_id;
        let diagnostics = fixture::diagnostics_for(
            &analysis,
            file_id,
            &config,
            adhoc_semantic_diagnostics,
            &fixture.diagnostics_enabled,
        );
        let diagnostics = diagnostics.diagnostics_for(file_id);

        let expected = fixture.annotations_by_file_id(&file_id);
        let actual = convert_diagnostics_to_annotations(diagnostics);
        assert_eq!(expected, actual);
    }
}

#[track_caller]
pub(crate) fn check_filtered_diagnostics(elp_fixture: &str, filter: &dyn Fn(&Diagnostic) -> bool) {
    let config = DiagnosticsConfig::default();
    check_filtered_diagnostics_with_config(config, &vec![], elp_fixture, filter)
}

#[track_caller]
pub(crate) fn check_filtered_diagnostics_with_config(
    config: DiagnosticsConfig,
    adhoc_semantic_diagnostics: &Vec<&dyn AdhocSemanticDiagnostics>,
    elp_fixture: &str,
    filter: &dyn Fn(&Diagnostic) -> bool,
) {
    let (db, fixture) = RootDatabase::with_fixture(elp_fixture);
    let host = AnalysisHost { db };
    let analysis = host.analysis();
    for file_id in &fixture.files {
        let file_id = *file_id;
        let diagnostics = fixture::diagnostics_for(
            &analysis,
            file_id,
            &config,
            adhoc_semantic_diagnostics,
            &fixture.diagnostics_enabled,
        );
        let diagnostics = diagnostics
            .diagnostics_for(file_id)
            .into_iter()
            .filter(|d| filter(d) || d.code == DiagnosticCode::SyntaxError)
            .collect();
        let expected = fixture.annotations_by_file_id(&file_id);
        let actual = convert_diagnostics_to_annotations(diagnostics);
        assert_eq!(expected, actual);
    }
}

#[track_caller]
pub(crate) fn check_diagnostics_with_config_and_extra(
    config: DiagnosticsConfig,
    extra_diags: &LabeledDiagnostics,
    elp_fixture: &str,
) {
    let (db, fixture) = RootDatabase::with_fixture(elp_fixture);
    let host = AnalysisHost { db };
    let analysis = host.analysis();
    for file_id in &fixture.files {
        let file_id = *file_id;
        let diagnostics = diagnostics::native_diagnostics(&analysis.db, &config, &vec![], file_id);
        let diagnostics = diagnostics::attach_related_diagnostics(diagnostics, extra_diags.clone());

        let expected = fixture.annotations_by_file_id(&file_id);
        let actual = convert_diagnostics_to_annotations(diagnostics);
        assert_eq!(expected, actual);
    }
}

#[track_caller]
pub fn check_no_parse_errors(analysis: &Analysis, file_id: FileId) {
    let config = DiagnosticsConfig::default()
        .disable(DiagnosticCode::UnspecificInclude)
        .disable(DiagnosticCode::UndefinedFunction)
        .disable(DiagnosticCode::NoCatch);
    check_no_parse_errors_with_config(analysis, file_id, &config, &vec![]);
}

#[track_caller]
pub fn check_no_parse_errors_with_config(
    analysis: &Analysis,
    file_id: FileId,
    config: &DiagnosticsConfig,
    adhoc_semantic_diagnostics: &Vec<&dyn AdhocSemanticDiagnostics>,
) {
    let diags = analysis
        .native_diagnostics(config, adhoc_semantic_diagnostics, file_id)
        .unwrap();
    assert!(
        diags.is_empty(),
        "didn't expect parse errors in files: {diags:?}"
    );
}

#[track_caller]
pub fn check_navs(navs: Vec<NavigationTarget>, expected: Vec<(FileRange, String)>) {
    let ranges = navs
        .into_iter()
        .map(|nav| nav.file_range())
        .collect::<Vec<_>>();
    check_file_ranges(ranges, expected)
}

pub fn check_file_ranges(mut ranges: Vec<FileRange>, expected: Vec<(FileRange, String)>) {
    let cmp = |&FileRange { file_id, range }: &_| (file_id, range.start());
    let mut expected = expected
        .into_iter()
        .map(|(range, _)| range)
        .collect::<Vec<_>>();
    ranges.sort_by_key(cmp);
    expected.sort_by_key(cmp);
    assert_eq!(expected, ranges);
}

#[track_caller]
pub fn check_call_hierarchy(prepare_fixture: &str, incoming_fixture: &str, outgoing_fixture: &str) {
    check_call_hierarchy_prepare(prepare_fixture);
    check_call_hierarchy_incoming_calls(incoming_fixture);
    check_call_hierarchy_outgoing_calls(outgoing_fixture);
}

fn check_call_hierarchy_prepare(fixture: &str) {
    let trimmed_fixture = trim_indent(fixture);
    let (analysis, fixture) = fixture::with_fixture(trimmed_fixture.as_str());
    let mut annotations = fixture.annotations();
    let mut navs = analysis
        .call_hierarchy_prepare(fixture.position())
        .unwrap()
        .unwrap()
        .info;
    assert_eq!(navs.len(), 1);
    assert_eq!(annotations.len(), 1);
    let nav = navs.pop().unwrap();
    let (expected_range, _text) = annotations.pop().unwrap();
    let actual_range = FileRange {
        file_id: nav.file_id,
        range: nav.focus_range.unwrap(),
    };
    assert_eq!(expected_range, actual_range);
}

fn check_call_hierarchy_incoming_calls(fixture: &str) {
    let trimmed_fixture = trim_indent(fixture);
    let (analysis, fixture) = fixture::with_fixture(trimmed_fixture.as_str());
    let expected = fixture.annotations();
    let incoming_calls = analysis
        .incoming_calls(fixture.position())
        .unwrap()
        .unwrap();
    let mut actual = Vec::new();
    for call in incoming_calls {
        actual.push((
            FileRange {
                file_id: call.target.file_id,
                range: call.target.focus_range.unwrap(),
            },
            format!("from: {}", call.target.name),
        ));
        for range in call.ranges {
            actual.push((
                FileRange {
                    file_id: call.target.file_id,
                    range,
                },
                format!("from_range: {}", call.target.name),
            ));
        }
    }
    let cmp =
        |(frange, text): &(FileRange, String)| (frange.file_id, frange.range.start(), text.clone());
    actual.sort_by_key(cmp);
    assert_eq!(actual, expected);
}

#[track_caller]
fn check_call_hierarchy_outgoing_calls(fixture: &str) {
    let trimmed_fixture = trim_indent(fixture);
    let (analysis, fixture) = fixture::with_fixture(trimmed_fixture.as_str());
    let expected = fixture.annotations();
    let outgoing_calls = analysis
        .outgoing_calls(fixture.position())
        .unwrap()
        .unwrap();
    let mut actual = Vec::new();
    for call in outgoing_calls {
        actual.push((
            FileRange {
                file_id: call.target.file_id,
                range: call.target.focus_range.unwrap(),
            },
            format!("to: {}", call.target.name),
        ));
        for range in call.ranges {
            actual.push((
                FileRange {
                    file_id: fixture.file_id(),
                    range,
                },
                format!("from_range: {}", call.target.name),
            ));
        }
    }
    let cmp =
        |(frange, text): &(FileRange, String)| (frange.file_id, frange.range.start(), text.clone());
    actual.sort_by_key(cmp);
    assert_eq!(actual, expected);
}

#[cfg(test)]
mod test {
    use elp_ide_db::DiagnosticCode;

    use super::check_filtered_diagnostics;
    use crate::diagnostics::Diagnostic;

    fn filter(d: &Diagnostic) -> bool {
        // Only allows through diagnostics where it returns true
        d.code == DiagnosticCode::MissingModule
    }

    #[test]
    fn filtered_diagnostics_passes_syntax_errors() {
        check_filtered_diagnostics(
            r#"
            %%<^^^^^^^^^^^^ error: no module definition
            foo() ->
               bug bug.
                %% ^^^^ error: Syntax Error
               
            "#,
            &filter,
        )
    }
}

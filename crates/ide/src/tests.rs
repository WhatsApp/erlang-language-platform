/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// To run the tests via cargo
// cargo test --package elp_ide --lib

use elp_ide_assists::Assist;
use elp_ide_db::elp_base_db::assert_eq_text;
use elp_ide_db::elp_base_db::fixture::extract_annotations;
use elp_ide_db::elp_base_db::fixture::remove_annotations;
use elp_ide_db::elp_base_db::fixture::WithFixture;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide_db::elp_base_db::SourceDatabaseExt;
use elp_ide_db::RootDatabase;
use elp_project_model::test_fixture::trim_indent;
use itertools::Itertools;
use text_edit::TextRange;

use crate::diagnostics;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::LabeledDiagnostics;
use crate::diagnostics::Severity;
use crate::fixture;
use crate::Analysis;
use crate::AnalysisHost;
use crate::DiagnosticsConfig;
use crate::NavigationTarget;

#[track_caller]
pub(crate) fn check_ct_fix(fixture_before: &str, fixture_after: &str) {
    let config = DiagnosticsConfig::default()
        .disable(DiagnosticCode::MissingCompileWarnMissingSpec)
        .disable(DiagnosticCode::UndefinedFunction);
    let diagnostic_filter = &|_d: &Diagnostic| true;
    let assist_filter = &|_d: &Assist| true;
    check_filtered_ct_fix_with_config(
        fixture_before,
        fixture_after,
        config,
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
    let config = DiagnosticsConfig::default()
        .disable(DiagnosticCode::MissingCompileWarnMissingSpec)
        .disable(DiagnosticCode::UndefinedFunction);
    check_filtered_ct_fix_with_config(
        fixture_before,
        fixture_after,
        config,
        diagnostic_filter,
        assist_filter,
    );
}

#[track_caller]
pub(crate) fn check_filtered_ct_fix_with_config(
    fixture_before: &str,
    fixture_after: &str,
    config: DiagnosticsConfig,
    diagnostic_filter: &dyn Fn(&Diagnostic) -> bool,
    assist_filter: &dyn Fn(&Assist) -> bool,
) {
    let after = trim_indent(fixture_after);
    let (analysis, pos, diagnostics_enabled) = fixture::position(fixture_before);
    diagnostics_enabled.assert_ct_enabled();

    check_no_parse_errors_with_config(&analysis, pos.file_id, &config);

    let diagnostics =
        fixture::diagnostics_for(&analysis, pos.file_id, &config, &diagnostics_enabled);
    let diagnostic = diagnostics
        .diagnostics_for(pos.file_id)
        .into_iter()
        .filter(diagnostic_filter)
        .last()
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
pub(crate) fn check_fix(fixture_before: &str, fixture_after: &str) {
    let config = DiagnosticsConfig::default()
        .disable(DiagnosticCode::MissingCompileWarnMissingSpec)
        .set_experimental(true);
    check_nth_fix(0, fixture_before, fixture_after, config);
}

///  Like `check_fix` but with a custom DiagnosticsConfig
#[track_caller]
pub(crate) fn check_fix_with_config(
    config: DiagnosticsConfig,
    fixture_before: &str,
    fixture_after: &str,
) {
    check_nth_fix(0, fixture_before, fixture_after, config);
}

#[track_caller]
pub(crate) fn check_nth_fix(
    nth: usize,
    fixture_before: &str,
    fixture_after: &str,
    config: DiagnosticsConfig,
) {
    let after = trim_indent(fixture_after);

    let (analysis, pos, diagnostics_enabled) = fixture::position(fixture_before);

    let diagnostics =
        fixture::diagnostics_for(&analysis, pos.file_id, &config, &diagnostics_enabled);
    let diagnostic = diagnostics
        .diagnostics_for(pos.file_id)
        .into_iter()
        .last()
        .expect("no diagnostics")
        .clone();
    let fix = &diagnostic.fixes.expect("diagnostic misses fixes")[nth];
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
    assert_eq_text!(&after, &actual);
}

/// Takes a multi-file input fixture with annotated cursor positions,
/// and checks that:
///  * a diagnostic is produced
///  * the first diagnostic fix trigger range touches the input cursor position
///  * that the contents of the file containing the cursor match `after` after the diagnostic fix is applied
#[track_caller]
pub(crate) fn check_specific_fix(assist_label: &str, fixture_before: &str, fixture_after: &str) {
    let config = DiagnosticsConfig::default()
        .disable(DiagnosticCode::MissingCompileWarnMissingSpec)
        .set_experimental(true);
    check_specific_fix_with_config(Some(assist_label), fixture_before, fixture_after, config);
}

#[track_caller]
pub(crate) fn check_specific_fix_with_config(
    assist_label: Option<&str>,
    fixture_before: &str,
    fixture_after: &str,
    config: DiagnosticsConfig,
) {
    let after = trim_indent(fixture_after);

    let (analysis, pos, diagnostics_enabled) = fixture::position(fixture_before);
    let diagnostics =
        fixture::diagnostics_for(&analysis, pos.file_id, &config, &diagnostics_enabled);
    let diagnostics = diagnostics.diagnostics_for(pos.file_id);
    let fix: &Assist = if let Some(label) = assist_label {
        if let Some(fix) = diagnostics
            .iter()
            .filter_map(|d| {
                d.fixes
                    .as_ref()
                    .and_then(|fixes| fixes.into_iter().find(|f| f.label == label))
            })
            .next()
        {
            &fix
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
    assert!(
        fix.target.contains_inclusive(pos.offset),
        "diagnostic fix range {:?} does not touch cursor position {:?}",
        fix.target,
        pos.offset
    );
    assert_eq_text!(&after, &actual);
}

#[track_caller]
pub(crate) fn check_diagnostics(fixture: &str) {
    let config = DiagnosticsConfig::default()
        .disable(DiagnosticCode::MissingCompileWarnMissingSpec)
        .set_experimental(true);
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
            annotation.push_str(&&convert_diagnostic_message(&d));
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
    let (analysis, pos, diagnostics_enabled) = fixture::position(elp_fixture);
    diagnostics_enabled.assert_ct_enabled();
    let file_id = pos.file_id;
    let config = DiagnosticsConfig::default();
    let diagnostics = fixture::diagnostics_for(&analysis, file_id, &config, &diagnostics_enabled);
    let diagnostics = diagnostics.diagnostics_for(file_id);
    let expected = extract_annotations(&analysis.db.file_text(file_id));
    let actual = convert_diagnostics_to_annotations(diagnostics);
    assert_eq!(expected, actual);
}

#[track_caller]
pub(crate) fn check_diagnostics_with_config(config: DiagnosticsConfig, elp_fixture: &str) {
    let (db, files, diagnostics_enabled) = RootDatabase::with_many_files(elp_fixture);
    if diagnostics_enabled.needs_erlang_service() {
        let file_id = FileId(0);
        let project_id = db.file_project_id(file_id).unwrap();
        db.ensure_erlang_service(project_id).unwrap();
    }
    let host = AnalysisHost { db };
    let analysis = host.analysis();
    for file_id in files {
        let diagnostics =
            fixture::diagnostics_for(&analysis, file_id, &config, &diagnostics_enabled);
        let diagnostics = diagnostics.diagnostics_for(file_id);

        let mut expected = extract_annotations(&analysis.db.file_text(file_id));
        expected.sort_by_key(|(r1, _)| r1.start());
        let actual = convert_diagnostics_to_annotations(diagnostics);
        assert_eq!(expected, actual);
    }
}

#[track_caller]
pub(crate) fn check_filtered_diagnostics(elp_fixture: &str, filter: &dyn Fn(&Diagnostic) -> bool) {
    let config = DiagnosticsConfig::default();
    check_filtered_diagnostics_with_config(config, elp_fixture, filter)
}

#[track_caller]
pub(crate) fn check_filtered_diagnostics_with_config(
    config: DiagnosticsConfig,
    elp_fixture: &str,
    filter: &dyn Fn(&Diagnostic) -> bool,
) {
    let (db, files, diagnostics_enabled) = RootDatabase::with_many_files(elp_fixture);
    if diagnostics_enabled.needs_erlang_service() {
        let file_id = FileId(0);
        let project_id = db.file_project_id(file_id).unwrap();
        db.ensure_erlang_service(project_id).unwrap();
    }
    let host = AnalysisHost { db };
    let analysis = host.analysis();
    for file_id in files {
        let diagnostics =
            fixture::diagnostics_for(&analysis, file_id, &config, &diagnostics_enabled);
        let diagnostics = diagnostics
            .diagnostics_for(file_id)
            .into_iter()
            .filter(filter)
            .collect();
        let mut expected = extract_annotations(&analysis.db.file_text(file_id));
        expected.sort_by_key(|(r1, _)| r1.start());
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
    let (db, files, _diagnostics_enabled) = RootDatabase::with_many_files(elp_fixture);
    let host = AnalysisHost { db };
    let analysis = host.analysis();
    for file_id in files {
        let diagnostics = diagnostics::native_diagnostics(&analysis.db, &config, file_id);
        let diagnostics = diagnostics::attach_related_diagnostics(diagnostics, extra_diags);

        let mut expected = extract_annotations(&analysis.db.file_text(file_id));
        expected.sort_by_key(|(r1, _)| r1.start());
        let actual = convert_diagnostics_to_annotations(diagnostics);
        assert_eq!(expected, actual);
    }
}

#[track_caller]
pub fn check_no_parse_errors(analysis: &Analysis, file_id: FileId) {
    let config = DiagnosticsConfig::default()
        .disable(DiagnosticCode::MissingCompileWarnMissingSpec)
        .disable(DiagnosticCode::UndefinedFunction);
    check_no_parse_errors_with_config(analysis, file_id, &config);
}

#[track_caller]
pub fn check_no_parse_errors_with_config(
    analysis: &Analysis,
    file_id: FileId,
    config: &DiagnosticsConfig,
) {
    let diags = analysis.native_diagnostics(&config, file_id).unwrap();
    assert!(
        diags.is_empty(),
        "didn't expect parse errors in files: {:?}",
        diags
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
    let (analysis, pos, _diagnostics_enabled, _guard, mut annotations) =
        fixture::annotations(trimmed_fixture.as_str());
    let mut navs = analysis.call_hierarchy_prepare(pos).unwrap().unwrap().info;
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
    let (analysis, pos, _diagnostics_enabled, _guard, mut expected) =
        fixture::annotations(trimmed_fixture.as_str());
    let incoming_calls = analysis.incoming_calls(pos).unwrap().unwrap();
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
    expected.sort_by_key(cmp);
    assert_eq!(actual, expected);
}

fn check_call_hierarchy_outgoing_calls(fixture: &str) {
    let trimmed_fixture = trim_indent(fixture);
    let (analysis, pos, _diagnostics_enabled, _guard, mut expected) =
        fixture::annotations(trimmed_fixture.as_str());
    let outgoing_calls = analysis.outgoing_calls(pos).unwrap().unwrap();
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
                    file_id: pos.file_id,
                    range,
                },
                format!("from_range: {}", call.target.name),
            ));
        }
    }
    let cmp =
        |(frange, text): &(FileRange, String)| (frange.file_id, frange.range.start(), text.clone());
    actual.sort_by_key(cmp);
    expected.sort_by_key(cmp);
    assert_eq!(actual, expected);
}

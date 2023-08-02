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

use elp_ide_db::elp_base_db::assert_eq_text;
use elp_ide_db::elp_base_db::fixture::extract_annotations;
use elp_ide_db::elp_base_db::fixture::WithFixture;
use elp_ide_db::elp_base_db::test_fixture::trim_indent;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::elp_base_db::SourceDatabaseExt;
use elp_ide_db::RootDatabase;
use fxhash::FxHashSet;

use crate::diagnostics;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::Severity;
use crate::fixture;
use crate::Analysis;
use crate::DiagnosticsConfig;
use crate::NavigationTarget;

/// Takes a multi-file input fixture with annotated cursor positions,
/// and checks that:
///  * a diagnostic is produced
///  * the first diagnostic fix trigger range touches the input cursor position
///  * that the contents of the file containing the cursor match `after` after the diagnostic fix is applied
#[track_caller]
pub(crate) fn check_fix(fixture_before: &str, fixture_after: &str) {
    let config =
        DiagnosticsConfig::default().disable(DiagnosticCode::MissingCompileWarnMissingSpec);
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
fn check_nth_fix(nth: usize, fixture_before: &str, fixture_after: &str, config: DiagnosticsConfig) {
    let after = trim_indent(fixture_after);

    let (db, file_position) = RootDatabase::with_position(fixture_before);
    let diagnostic = diagnostics::diagnostics(&db, &config, file_position.file_id, true)
        .pop()
        .expect("no diagnostics");
    let fix = &diagnostic.fixes.expect("diagnostic misses fixes")[nth];
    let actual = {
        let source_change = fix.source_change.as_ref().unwrap();
        let file_id = *source_change.source_file_edits.keys().next().unwrap();
        let mut actual = db.file_text(file_id).to_string();

        for edit in source_change.source_file_edits.values() {
            edit.apply(&mut actual);
        }
        actual
    };
    assert!(
        fix.target.contains_inclusive(file_position.offset),
        "diagnostic fix range {:?} does not touch cursor position {:?}",
        fix.target,
        file_position.offset
    );
    assert_eq_text!(&after, &actual);
}

#[track_caller]
pub(crate) fn check_diagnostics(ra_fixture: &str) {
    let config =
        DiagnosticsConfig::default().disable(DiagnosticCode::MissingCompileWarnMissingSpec);
    check_diagnostics_with_config(config, ra_fixture)
}

#[track_caller]
pub(crate) fn check_diagnostics_with_config(config: DiagnosticsConfig, elp_fixture: &str) {
    let (db, files) = RootDatabase::with_many_files(elp_fixture);
    for file_id in files {
        let diagnostics = diagnostics::diagnostics(&db, &config, file_id, true);

        let expected = extract_annotations(&*db.file_text(file_id));
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

#[track_caller]
pub fn check_no_parse_errors(analysis: &Analysis, file_id: FileId) {
    let config = DiagnosticsConfig::new(true, FxHashSet::default(), vec![])
        .disable(DiagnosticCode::MissingCompileWarnMissingSpec);
    let diags = analysis.diagnostics(&config, file_id, true).unwrap();
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
    let (analysis, pos, mut annotations) = fixture::annotations(trim_indent(fixture).as_str());
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
    let (analysis, pos, mut expected) = fixture::annotations(trim_indent(fixture).as_str());
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
    let (analysis, pos, mut expected) = fixture::annotations(trim_indent(fixture).as_str());
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

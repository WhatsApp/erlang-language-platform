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
use elp_ide_db::assists::GroupLabel;
use elp_ide_db::elp_base_db::CURSOR_MARKER;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::elp_base_db::RangeOrOffset;
use elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide_db::elp_base_db::assert_eq_expected;
use elp_ide_db::elp_base_db::fixture::ChangeFixture;
use elp_ide_db::elp_base_db::fixture::WithFixture;
use elp_ide_db::elp_base_db::remove_annotations;
use elp_ide_db::elp_base_db::render_annotations;
use elp_ide_db::text_edit::TextRange;
use elp_project_model::test_fixture::FixtureWithProjectMeta;
use elp_project_model::test_fixture::patch_fixture_literal;
use elp_project_model::test_fixture::trim_indent;
use elp_project_model::test_fixture::update_fixtures_requested;
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
use crate::diagnostics::DiagnosticsTrigger;
use crate::diagnostics::LabeledDiagnostics;
use crate::diagnostics::Severity;
use crate::fixture;

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
        let mut actual = analysis
            .db
            .file_text(file_id)
            .text(&analysis.db)
            .to_string();

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

    let actual = convert_diagnostics_to_annotations(diagnostics.clone());
    assert_fixture_annotations(
        &analysis,
        fixture_before,
        fixture.files.len(),
        &[FileAnnotations::new(&fixture, file_id, actual)],
    );

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
        let mut actual = analysis
            .db
            .file_text(file_id)
            .text(&analysis.db)
            .to_string();

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
        .disable(DiagnosticCode::BinaryStringToSigil)
        .disable(DiagnosticCode::HirUnresolvedMacro)
        .disable(DiagnosticCode::BoundVarInLhs)
        .disable(DiagnosticCode::HirUnresolvedInclude);
    check_diagnostics_with_config(config, fixture)
}

pub(crate) fn convert_diagnostics_to_annotations(
    diagnostics: Vec<Diagnostic>,
) -> Vec<(TextRange, String)> {
    let mut actual = diagnostics
        .into_iter()
        .map(|d| {
            let mut annotation = String::new();
            if let Some(fixes) = &d.fixes {
                assert!(!fixes.is_empty());
            }
            annotation.push_str(match d.severity {
                Severity::Error => "error",
                Severity::Warning => "warning",
                Severity::WeakWarning => "weak",
                Severity::Information => "information",
            });
            annotation.push_str(": ");
            annotation.push_str(&d.code.as_code());
            annotation.push_str(": ");
            annotation.push_str(&convert_diagnostic_message(&d));

            // Append related info to the annotation
            if let Some(related_info) = &d.related_info {
                // Sort related info alphabetically by message for consistent test output
                let mut sorted_info = related_info.clone();
                sorted_info.sort_by(|a, b| a.message.cmp(&b.message));
                for info in sorted_info {
                    annotation.push_str(&format!(
                        "\nRelated info: {}:{}-{} {}",
                        info.file_id.index(),
                        u32::from(info.range.start()),
                        u32::from(info.range.end()),
                        info.message
                    ));
                }
            }

            // Append the offered fixes. Their wording is usually tailored to
            // the specific occurrence, so it is worth pinning down. The
            // suppression pair every suppressible diagnostic carries is the
            // same everywhere, so it collapses to one line.
            if let Some(fixes) = &d.fixes {
                let (suppression, tailored): (Vec<_>, Vec<_>) =
                    fixes.iter().partition(|fix| is_suppression(fix));
                for fix in tailored {
                    annotation.push_str(&format!("\n💡 {}", fix.label));
                }
                if is_default_suppression_pair(&suppression) {
                    annotation.push_str("\n💡 <suppression>");
                } else {
                    for fix in suppression {
                        annotation.push_str(&format!("\n💡 {}", fix.label));
                    }
                }
            }

            (d.range, annotation)
        })
        .collect::<Vec<_>>();
    actual.sort_by_key(|(range, _)| range.start());
    actual
}

thread_local! {
    /// Set while a test is deliberately provoking an annotation mismatch, so
    /// that an update run reports it rather than "repairing" the fixture.
    static SUPPRESS_FIXTURE_UPDATE: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Run `f` with in-place fixture rewriting disabled.
fn without_fixture_updates<R>(f: impl FnOnce() -> R) -> R {
    SUPPRESS_FIXTURE_UPDATE.with(|flag| flag.set(true));
    let res = f();
    SUPPRESS_FIXTURE_UPDATE.with(|flag| flag.set(false));
    res
}

/// The annotations one file of a fixture declares, and the ones its
/// diagnostics actually produced.
pub(crate) struct FileAnnotations {
    file_id: FileId,
    /// Position of this file's section in the fixture.
    index: usize,
    /// Where this file's cursor marker sits, if it has one. The database text
    /// has the marker stripped, so it has to be put back before the fixture is
    /// rewritten -- otherwise the update would silently delete it.
    cursor: Option<RangeOrOffset>,
    expected: Vec<(TextRange, String)>,
    actual: Vec<(TextRange, String)>,
}

impl FileAnnotations {
    fn new(fixture: &ChangeFixture, file_id: FileId, actual: Vec<(TextRange, String)>) -> Self {
        let cursor = fixture
            .file_position
            .and_then(|(cursor_file, pos)| (cursor_file == file_id).then_some(pos));
        let index = fixture
            .files
            .iter()
            .position(|candidate| *candidate == file_id)
            .expect("file is part of the fixture");
        FileAnnotations {
            file_id,
            index,
            cursor,
            expected: fixture.annotations_by_file_id(&file_id),
            actual,
        }
    }

    /// The file as the fixture spells it: database text with the cursor marker
    /// put back. `None` when the marker cannot be reinstated, which stops the
    /// fixture being rewritten at all.
    fn fixture_text(&self, analysis: &Analysis) -> Option<String> {
        let mut text = analysis
            .db
            .file_text(self.file_id)
            .text(&analysis.db)
            .to_string();
        match self.cursor {
            None => Some(text),
            Some(RangeOrOffset::Offset(offset)) => {
                text.insert_str(u32::from(offset) as usize, CURSOR_MARKER);
                Some(text)
            }
            // A `$0`-delimited range needs two markers; not worth supporting
            // until a fixture actually uses one.
            Some(RangeOrOffset::Range(_)) => None,
        }
    }
}

/// Compare the annotations every file of `elp_fixture` declares against the
/// ones its diagnostics produce.
///
/// Under `UPDATE_EXPECT` a mismatch rewrites the fixture in place instead of
/// failing. The rewrite only happens once the new fixture has been parsed back
/// and confirmed to yield exactly the annotations the diagnostics produced, so
/// a fixture is never left saying something the harness cannot reproduce.
#[track_caller]
fn assert_fixture_annotations(
    analysis: &Analysis,
    elp_fixture: &str,
    sections: usize,
    files: &[FileAnnotations],
) {
    if files.iter().all(|f| f.expected == f.actual) {
        return;
    }

    // Each step can decline; say which one did, so a fixture the harness
    // cannot rewrite is a lead rather than a mystery.
    // Only the files that were checked get re-rendered; the rest of the
    // fixture's sections are carried over untouched. `check_specific_fix` looks
    // at one file of a fixture that may well have several.
    let mut rendered: Vec<Option<String>> = vec![None; sections];
    let mut renderable = true;
    for file in files {
        match file
            .fixture_text(analysis)
            .and_then(|text| render_annotations(&text, &file.actual))
        {
            Some(text) if file.index < sections => rendered[file.index] = Some(text),
            _ => renderable = false,
        }
    }

    let updated = match renderable {
        false => Err("the annotations cannot be expressed in the caret syntax"),
        true => match splice_fixture_files(elp_fixture, &rendered) {
            None => Err("the fixture's file sections could not be reassembled"),
            Some(updated) => {
                if fixture_yields(&updated, analysis, files) {
                    Ok(updated)
                } else {
                    Err("re-parsing the rewritten fixture did not reproduce it exactly")
                }
            }
        },
    };

    if let Ok(updated) = &updated
        && update_fixtures_requested()
        && !SUPPRESS_FIXTURE_UPDATE.with(|flag| flag.get())
    {
        let caller = std::panic::Location::caller();
        match patch_fixture_literal(caller, elp_fixture, updated) {
            Ok(true) => return,
            Ok(false) => panic!(
                "UPDATE_EXPECT: could not find the fixture literal at {caller}.\n\
                 The updated fixture is:\n{updated}"
            ),
            Err(err) => panic!("UPDATE_EXPECT: could not rewrite {caller}: {err}"),
        }
    }

    let expected = files.iter().map(|f| &f.expected).collect_vec();
    let actual = files.iter().map(|f| &f.actual).collect_vec();
    match updated {
        Ok(updated) => panic!(
            "assertion failed\n  expected: {expected:?}\n  actual  : {actual:?}\n\n\
             re-run with `-m fbcode//whatsapp/elp:expect[update]` to apply:\n{updated}"
        ),
        Err(reason) => panic!(
            "assertion failed\n  expected: {expected:?}\n  actual  : {actual:?}\n\n\
             cannot rewrite this fixture: {reason}"
        ),
    }
}

/// Rebuild `elp_fixture`, replacing each file section for which `rendered`
/// supplies a body and copying the rest through unchanged.
///
/// Sections are delimited by `//- /path` marker lines. The optional
/// `//- erlang_service`-style meta lines that may precede them are not file
/// markers and are copied through. A fixture with no marker at all is a single
/// implicit file.
fn splice_fixture_files(elp_fixture: &str, rendered: &[Option<String>]) -> Option<String> {
    let trimmed = trim_indent(elp_fixture);
    let mut prelude = String::new();
    let mut sections: Vec<(String, String)> = Vec::new();

    for line in trimmed.split_inclusive('\n') {
        if line.starts_with("//- /") {
            sections.push((line.to_string(), String::new()));
        } else if sections.is_empty() && line.starts_with("//-") {
            prelude.push_str(line);
        } else {
            if sections.is_empty() {
                sections.push((String::new(), String::new()));
            }
            sections.last_mut()?.1.push_str(line);
        }
    }

    if sections.len() != rendered.len() {
        return None;
    }

    let mut res = prelude;
    for ((marker, body), replacement) in sections.iter().zip(rendered) {
        // A rendered section need not end in a newline, but the marker that
        // follows it has to start its own line.
        if !res.is_empty() && !res.ends_with('\n') {
            res.push('\n');
        }
        res.push_str(marker);
        res.push_str(replacement.as_deref().unwrap_or(body));
    }
    Some(res)
}

/// Does re-parsing `fixture` reproduce every file's contents, cursor and
/// annotations exactly?
///
/// The annotations are the point of the exercise, but checking the text and
/// cursor too is what makes the rewrite safe: it catches a render that lost the
/// cursor marker, or a splice that put a section back in the wrong place.
fn fixture_yields(fixture: &str, analysis: &Analysis, files: &[FileAnnotations]) -> bool {
    // `parse` panics rather than erroring on a fixture it cannot make sense of,
    // and a rendered fixture is exactly the sort of thing that might not parse.
    // Treat that as "do not rewrite", not as a test failure.
    let Ok(parsed) = std::panic::catch_unwind(|| FixtureWithProjectMeta::parse(fixture)) else {
        return false;
    };
    files.iter().all(|file| {
        let Some(parsed) = parsed.fixture.get(file.index) else {
            return false;
        };
        let mut annotations = parsed.annotations.clone();
        annotations.sort_by_key(|(range, _)| range.start());
        let db_text = analysis
            .db
            .file_text(file.file_id)
            .text(&analysis.db)
            .to_string();
        annotations == file.actual && parsed.marker_pos == file.cursor && parsed.text == db_text
    })
}

fn is_suppression(fix: &Assist) -> bool {
    fix.group == Some(GroupLabel::ignore()) || fix.group == Some(GroupLabel::fixme())
}

/// The `ignore` + `fixme` pair that `with_default_suppression_fixes` attaches
/// to every suppressible diagnostic.
fn is_default_suppression_pair(fixes: &[&Assist]) -> bool {
    fixes.len() == 2
        && fixes[0].group == Some(GroupLabel::ignore())
        && fixes[1].group == Some(GroupLabel::fixme())
}

fn convert_diagnostic_message(d: &Diagnostic) -> String {
    match &d.code {
        DiagnosticCode::Eqwalizer(_) => d.code.as_code(),
        _ => d.message.clone(),
    }
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
    let files = fixture
        .files
        .iter()
        .map(|file_id| {
            let file_id = *file_id;
            let diagnostics = fixture::diagnostics_for(
                &analysis,
                file_id,
                &config,
                adhoc_semantic_diagnostics,
                &fixture.diagnostics_enabled,
            );
            let diagnostics = diagnostics.diagnostics_for(file_id);
            FileAnnotations::new(
                &fixture,
                file_id,
                convert_diagnostics_to_annotations(diagnostics),
            )
        })
        .collect_vec();
    assert_fixture_annotations(&analysis, elp_fixture, fixture.files.len(), &files);
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
    let files = fixture
        .files
        .iter()
        .map(|file_id| {
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
            FileAnnotations::new(
                &fixture,
                file_id,
                convert_diagnostics_to_annotations(diagnostics),
            )
        })
        .collect_vec();
    assert_fixture_annotations(&analysis, elp_fixture, fixture.files.len(), &files);
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
    let files = fixture
        .files
        .iter()
        .map(|file_id| {
            let file_id = *file_id;
            let diagnostics = diagnostics::native_diagnostics(
                &analysis.db,
                &config,
                &DiagnosticsTrigger::Cli,
                &vec![],
                file_id,
            );
            let diagnostics =
                diagnostics::attach_related_diagnostics(file_id, diagnostics, extra_diags.clone());
            FileAnnotations::new(
                &fixture,
                file_id,
                convert_diagnostics_to_annotations(diagnostics),
            )
        })
        .collect_vec();
    assert_fixture_annotations(&analysis, elp_fixture, fixture.files.len(), &files);
}

#[track_caller]
pub fn check_no_parse_errors(analysis: &Analysis, file_id: FileId) {
    let config = DiagnosticsConfig::default()
        .disable(DiagnosticCode::UnspecificInclude)
        .disable(DiagnosticCode::UndefinedFunction)
        .disable(DiagnosticCode::HirUnresolvedMacro)
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
        .native_diagnostics(
            config,
            &DiagnosticsTrigger::Cli,
            adhoc_semantic_diagnostics,
            file_id,
        )
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
    assert_eq_expected!(expected, ranges);
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
    assert_eq_expected!(expected_range, actual_range);
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
    assert_eq_expected!(expected, actual);
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
    assert_eq_expected!(expected, actual);
}

#[cfg(test)]
mod test {
    use elp_ide_db::DiagnosticCode;
    use expect_test::expect;

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
            //- expect_parse_errors
            foo() ->
            %%<^^^^^^^^^^^^ error: L1201: no module definition
            %%            | 💡 <suppression>
               bug bug.
            %%     ^^^^ error: P1711: Syntax Error

            "#,
            &filter,
        )
    }

    #[test]
    fn diagnostic_assertion_failure_message_format() {
        // Test that uses a mismatched diagnostic annotation to verify
        // the new error message format shows "expected:" and "actual:" labels

        let result = super::without_fixture_updates(|| {
            std::panic::catch_unwind(|| {
                // This fixture has a wrong diagnostic annotation - it expects W9999
                // but the actual diagnostic will be MissingModule (L1201)
                check_filtered_diagnostics(
                    r#"
                //- expect_parse_errors
                %%<^^^^^^^^^^^^ error: W9999: wrong diagnostic
                foo() -> ok.
                "#,
                    &filter,
                )
            })
        });

        // The test should panic due to mismatch
        assert!(
            result.is_err(),
            "Expected a panic due to diagnostic mismatch"
        );

        // Extract the panic message
        let panic_info = result.unwrap_err();
        let message = if let Some(s) = panic_info.downcast_ref::<String>() {
            s.clone()
        } else if let Some(s) = panic_info.downcast_ref::<&str>() {
            s.to_string()
        } else {
            panic!("Unable to extract panic message");
        };

        expect![[r#"
            assertion failed
              expected: [[(0..15, "error: W9999: wrong diagnostic")]]
              actual  : [[(0..12, "error: L1201: no module definition\n💡 <suppression>")]]

            re-run with `-m fbcode//whatsapp/elp:expect[update]` to apply:
            //- expect_parse_errors
            foo() -> ok.
            %%<^^^^^^^^^ error: L1201: no module definition
            %%         | 💡 <suppression>
        "#]]
        .assert_eq(&message);
    }
}

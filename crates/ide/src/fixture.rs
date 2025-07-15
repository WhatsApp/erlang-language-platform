/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Utilities for creating `Analysis` instances for tests.

use elp_ide_db::RootDatabase;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::elp_base_db::RootQueryDb;
use elp_ide_db::elp_base_db::fixture::ChangeFixture;
use elp_ide_db::elp_base_db::fixture::WithFixture;
use elp_project_model::test_fixture::DiagnosticsEnabled;

use crate::Analysis;
use crate::AnalysisHost;
use crate::FilePosition;
use crate::diagnostics::AdhocSemanticDiagnostics;
use crate::diagnostics::DiagnosticsConfig;
use crate::diagnostics::RemoveElpReported;
use crate::diagnostics_collection::DiagnosticCollection;

/// Creates analysis from a single file fixture
#[track_caller]
pub fn with_fixture(fixture: &str) -> (Analysis, ChangeFixture) {
    let (db, fixture) = RootDatabase::with_fixture(fixture);
    let host = AnalysisHost { db };
    (host.analysis(), fixture)
}

/// Creates analysis from a single file fixture, returns the file id
#[track_caller]
pub(crate) fn single_file_annotations(
    fixture: &str,
) -> (Analysis, FileId, Vec<(FileRange, String)>) {
    let (db, fixture) = RootDatabase::with_fixture(fixture);
    let annotations = fixture.annotations();
    let host = AnalysisHost { db };
    assert_eq!(fixture.files.len(), 1);
    (host.analysis(), fixture.files[0], annotations)
}

/// Creates analysis from a multi-file fixture, returns position marked with the [`CURSOR_MARKER`]
#[track_caller]
pub(crate) fn position(fixture: &str) -> (Analysis, FilePosition, DiagnosticsEnabled) {
    let (db, fixture) = RootDatabase::with_fixture(fixture);
    let position = fixture.position();
    let diagnostics_enabled = fixture.diagnostics_enabled;
    let host = AnalysisHost { db };
    (host.analysis(), position, diagnostics_enabled)
}

/// Creates analysis from a multi-file fixture
#[track_caller]
pub(crate) fn multi_file(fixture: &str) -> Analysis {
    let (db, _fixture) = RootDatabase::with_fixture(fixture);
    let host = AnalysisHost { db };
    host.analysis()
}

pub fn check_no_parse_errors(analysis: &Analysis, file_id: FileId) -> Option<()> {
    // Check that we have a syntactically valid starting point
    let text = analysis.file_text(file_id).ok()?;
    let parse = analysis.db.parse(file_id);
    let errors = parse.errors();
    if !errors.is_empty() {
        assert_eq!(format!("{errors:?}\nin\n{text}"), "");
    };
    Some(())
}

// TODO: Further up the stack, once all diagnostics sources populated:
//   Move to Analysis, using ? not unwrap()
//   And then used in elp_lint too
pub fn diagnostics_for(
    analysis: &Analysis,
    file_id: FileId,
    config: &DiagnosticsConfig,
    adhoc_semantic_diagnostics: &Vec<&dyn AdhocSemanticDiagnostics>,
    diagnostics_enabled: &DiagnosticsEnabled,
) -> DiagnosticCollection {
    let mut diagnostics = DiagnosticCollection::default();
    let DiagnosticsEnabled {
        use_native,
        use_erlang_service,
        use_eqwalizer,
        use_ct,
        use_edoc,
        tmp_dir: _,
    } = diagnostics_enabled;
    if *use_native {
        diagnostics.set_native(
            file_id,
            analysis
                .native_diagnostics(config, adhoc_semantic_diagnostics, file_id)
                .unwrap(),
        );
    }
    if *use_erlang_service {
        let erlang_service_diagnostics = analysis
            .erlang_service_diagnostics(file_id, config, RemoveElpReported::Yes)
            .unwrap();
        for (file_id, diags) in erlang_service_diagnostics {
            diagnostics.set_erlang_service(file_id, diags)
        }
    }
    if *use_eqwalizer {
        if let Some(diags) = analysis.eqwalizer_diagnostics_for_file(file_id).unwrap() {
            diagnostics.set_eqwalizer(file_id, diags);
        }
    }
    if *use_ct {
        diagnostics.set_ct(file_id, analysis.ct_diagnostics(file_id, config).unwrap());
    }
    if *use_edoc {
        let edoc_diagnostics = analysis.edoc_diagnostics(file_id, config).unwrap();
        for (file_id, diags) in edoc_diagnostics {
            diagnostics.set_edoc(file_id, diags);
        }
    }
    diagnostics
}

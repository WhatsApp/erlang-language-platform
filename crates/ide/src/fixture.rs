/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Utilities for creating `Analysis` instances for tests.

use elp_ide_db::elp_base_db::fixture::WithFixture;
use elp_ide_db::elp_base_db::fixture::CURSOR_MARKER;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide_db::RootDatabase;

use crate::Analysis;
use crate::AnalysisHost;
use crate::FilePosition;

/// Creates analysis from a single file fixture, returns the file id
pub(crate) fn single_file(fixture: &str) -> (Analysis, FileId) {
    let (db, file_id) = RootDatabase::with_single_file(fixture);
    let host = AnalysisHost { db };
    (host.analysis(), file_id)
}

/// Creates analysis from a multi-file fixture, returns position marked with the [`CURSOR_MARKER`]
pub(crate) fn position(fixture: &str) -> (Analysis, FilePosition) {
    let (db, position) = RootDatabase::with_position(fixture);
    let host = AnalysisHost { db };
    (host.analysis(), position)
}

/// Creates analysis from a multi-file fixture
pub(crate) fn multi_file(fixture: &str) -> Analysis {
    let (db, _) = RootDatabase::with_fixture(fixture);
    let host = AnalysisHost { db };
    host.analysis()
}

/// Creates analysis from a multi-file fixture, returns first position marked with [`CURSOR_MARKER`]
/// and annotations marked with sequence of %% ^^^
pub fn annotations(fixture: &str) -> (Analysis, FilePosition, Vec<(FileRange, String)>) {
    let (db, fixture) = RootDatabase::with_fixture(fixture);
    let (file_id, range_or_offset) = fixture
        .file_position
        .expect(&format!("expected a marker ({})", CURSOR_MARKER));
    let offset = range_or_offset.expect_offset();

    let annotations = fixture.annotations(&db);
    let analysis = AnalysisHost { db }.analysis();
    (analysis, FilePosition { file_id, offset }, annotations)
}

pub fn check_no_parse_errors(analysis: &Analysis, file_id: FileId) -> Option<()> {
    // Check that we have a syntactically valid starting point
    let text = analysis.file_text(file_id).ok()?;
    let parse = analysis.db.parse(file_id);
    let errors = parse.errors();
    if !errors.is_empty() {
        assert_eq!(format!("{:?}\nin\n{text}", errors), "");
    };
    Some(())
}

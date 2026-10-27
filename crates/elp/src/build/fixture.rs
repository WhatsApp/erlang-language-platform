/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide::AnalysisHost;
use elp_ide::elp_ide_db::RootDatabase;
use elp_ide::elp_ide_db::elp_base_db::FileSetConfig;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::fixture::ChangeFixture;

use super::types::LoadResult;

/// Creates analysis from a multi-file fixture
#[track_caller]
pub fn load_result(fixture_str: &str) -> LoadResult {
    let mut db = RootDatabase::default();
    let (_fixture, change, project) = ChangeFixture::parse_detail(&db, fixture_str);
    let file_map = change.apply(&mut db);

    LoadResult::new(
        AnalysisHost::new(db),
        file_map,
        ProjectId(0),
        project,
        FileSetConfig::default(),
    )
}

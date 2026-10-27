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
use elp_ide::elp_ide_db::elp_base_db::FileMap;
use elp_ide::elp_ide_db::elp_base_db::FileSetConfig;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::fixture::ChangeFixture;
use elp_project_model::test_fixture::FixtureWithProjectMeta;
use fxhash::FxHashMap;
use vfs::AbsPathBuf;
use vfs::FileId;
use vfs::VfsPath;

use super::types::LoadResult;
use crate::document::Document;
use crate::line_endings::LineEndings;

/// Creates analysis from a multi-file fixture
#[track_caller]
pub fn load_result(fixture_str: &str) -> LoadResult {
    let (fixture, change, project) = ChangeFixture::parse_detail(fixture_str);
    let mut db = RootDatabase::default();
    change.apply(&mut db, &|path| fixture.resolve_file_id(path));

    let analysis_host = AnalysisHost::new(db);
    let (file_map, line_ending_map) = load_info_from_fixture(fixture_str);

    LoadResult::new(
        analysis_host,
        file_map,
        line_ending_map,
        ProjectId(0),
        project,
        FileSetConfig::default(),
    )
}

fn load_info_from_fixture(fixture: &str) -> (FileMap, FxHashMap<FileId, LineEndings>) {
    let file_map = FileMap::default();
    let mut line_ending_map = FxHashMap::default();

    let fixtures = FixtureWithProjectMeta::parse(fixture);
    for fixture in &fixtures.fixture {
        let path = VfsPath::from(AbsPathBuf::assert(fixture.path.clone().into()));
        let document = Document::from_bytes(fixture.text.as_bytes());
        let file_id = file_map.intern(path);
        file_map.set_exists(file_id, true);
        let (_text, line_ending) = document.vfs_to_salsa();
        line_ending_map.insert(file_id, line_ending);
    }
    (file_map, line_ending_map)
}

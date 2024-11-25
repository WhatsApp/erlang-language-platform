/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide::elp_ide_db::elp_base_db::fixture::ChangeFixture;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::RootDatabase;
use elp_ide::AnalysisHost;
use elp_project_model::test_fixture::FixtureWithProjectMeta;
use fxhash::FxHashMap;
use vfs::file_set::FileSetConfig;
use vfs::AbsPathBuf;
use vfs::FileId;
use vfs::Vfs;
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
    let (vfs, line_ending_map) = load_info_from_fixture(fixture_str);

    LoadResult {
        analysis_host,
        vfs,
        line_ending_map,
        project_id: ProjectId(0),
        project,
        file_set_config: FileSetConfig::default(),
    }
}

fn load_info_from_fixture(fixture: &str) -> (Vfs, FxHashMap<FileId, LineEndings>) {
    let mut vfs = Vfs::default();
    let mut line_ending_map = FxHashMap::default();

    let fixtures = FixtureWithProjectMeta::parse(fixture);
    for fixture in &fixtures.fixture {
        let path = VfsPath::from(AbsPathBuf::assert(fixture.path.clone().into()));
        let bytes = fixture.text.clone().into_bytes();
        let document = Document::from_bytes(&bytes);
        vfs.set_file_contents(path.clone(), Some(bytes));
        let (_text, line_ending) = document.vfs_to_salsa();
        let file_id = vfs.file_id(&path).unwrap();
        line_ending_map.insert(file_id, line_ending);
    }
    (vfs, line_ending_map)
}

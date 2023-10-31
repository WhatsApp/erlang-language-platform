/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;
use std::sync::Arc;

use elp_base_db::salsa;
use elp_base_db::AbsPath;
use elp_base_db::AbsPathBuf;
use elp_base_db::FileId;
use elp_base_db::ProjectId;
use elp_base_db::SourceDatabase;
use elp_erlang_service::Format;
use elp_erlang_service::ParseError;
use elp_erlang_service::ParseResult;

use crate::erlang_service::CompileOption;
use crate::erlang_service::ParseRequest;
use crate::fixmes;
use crate::LineIndexDatabase;

pub trait AstLoader {
    fn load_ast(
        &self,
        project_id: ProjectId,
        path: &AbsPath,
        include_path: &[AbsPathBuf],
        macros: &[eetf::Term],
        parse_transforms: &[eetf::Term],
        elp_metadata: eetf::Term,
        format: Format,
    ) -> ParseResult;
}

impl AstLoader for crate::RootDatabase {
    fn load_ast(
        &self,
        project_id: ProjectId,
        path: &AbsPath,
        include_path: &[AbsPathBuf],
        macros: &[eetf::Term],
        parse_transforms: &[eetf::Term],
        elp_metadata: eetf::Term,
        format: Format,
    ) -> ParseResult {
        log::warn!("erl_ast:entry:{:?}", &path);
        let includes = include_path
            .iter()
            .map(|path| path.clone().into())
            .collect();
        let options = vec![
            CompileOption::Includes(includes),
            CompileOption::Macros(macros.to_vec()),
            CompileOption::ParseTransforms(parse_transforms.to_vec()),
            CompileOption::ElpMetadata(elp_metadata),
        ];
        let path = path.to_path_buf().into();
        let req = ParseRequest {
            options,
            path,
            format,
        };

        if let Some(erlang_service) = self.erlang_services.read().get(&project_id).cloned() {
            erlang_service.request_parse(req)
        } else {
            log::error!("No parse server for project: {:?}", project_id);
            ParseResult::error(ParseError {
                path: PathBuf::new(),
                location: None,
                msg: "Unknown application".to_string(),
                code: "L0004".to_string(),
            })
        }
    }
}

#[salsa::query_group(ErlAstDatabaseStorage)]
pub trait ErlAstDatabase: SourceDatabase + AstLoader + LineIndexDatabase {
    fn module_ast(&self, file_id: FileId, format: Format) -> Arc<ParseResult>;
}

fn module_ast(db: &dyn ErlAstDatabase, file_id: FileId, format: Format) -> Arc<ParseResult> {
    log::warn!("module_ast:{:?}", &file_id);
    // Dummy read of file text and global revision ID to make DB track changes
    let _track_changes_to_file = db.file_text(file_id);
    let _track_global_changes = db.include_files_revision();

    let root_id = db.file_source_root(file_id);
    let root = db.source_root(root_id);
    let path = root.path_for_file(&file_id).unwrap().as_path().unwrap();
    let app_data = if let Some(app_data) = db.app_data(root_id) {
        app_data
    } else {
        return Arc::new(ParseResult::error(ParseError {
            path: path.to_path_buf().into(),
            location: None,
            msg: "Unknown application".to_string(),
            code: "L0003".to_string(),
        }));
    };
    let metadata = elp_metadata(db, file_id);

    log::warn!("module_ast:before load_ast{:?}", &file_id);
    let r = Arc::new(db.load_ast(
        app_data.project_id,
        path,
        &app_data.include_path,
        &app_data.macros,
        &app_data.parse_transforms,
        metadata,
        format,
    ));
    log::warn!("module_ast:load_ast:{:?}, {:?}", &r.errors, &r.warnings);
    r
}

fn elp_metadata(db: &dyn ErlAstDatabase, file_id: FileId) -> eetf::Term {
    let line_index = db.file_line_index(file_id);
    let file_text = db.file_text(file_id);
    let fixmes = fixmes::fixmes_eetf(&line_index, &file_text);
    // Erlang proplist: [{eqwalizer_fixmes, [Fixme1, Fixme2....]}]
    eetf::List::from(vec![eetf::Tuple::from(vec![
        eetf::Atom::from("eqwalizer_fixmes").into(),
        fixmes,
    ])
    .into()])
    .into()
}

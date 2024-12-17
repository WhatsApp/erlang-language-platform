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

use elp_base_db::path_for_file;
use elp_base_db::salsa;
use elp_base_db::salsa::Database;
use elp_base_db::AbsPath;
use elp_base_db::FileId;
use elp_base_db::FileLoader;
use elp_base_db::IncludeCtx;
use elp_base_db::ProjectId;
use elp_base_db::SourceDatabase;
use elp_erlang_service::Format;
use elp_erlang_service::IncludeType;
use elp_erlang_service::ParseError;
use elp_erlang_service::ParseResult;

use crate::erlang_service::CompileOption;
use crate::erlang_service::ParseRequest;
use crate::metadata;
use crate::metadata::Metadata;
use crate::LineIndexDatabase;

pub trait AstLoader {
    fn load_ast(
        &self,
        project_id: ProjectId,
        file_id: FileId,
        path: &AbsPath,
        macros: &[eetf::Term],
        parse_transforms: &[eetf::Term],
        compile_options: Vec<CompileOption>,
        override_compile_options: Vec<CompileOption>,
        elp_metadata: eetf::Term,
        format: Format,
    ) -> ParseResult;
}

impl AstLoader for crate::RootDatabase {
    fn load_ast(
        &self,
        project_id: ProjectId,
        file_id: FileId,
        path: &AbsPath,
        macros: &[eetf::Term],
        parse_transforms: &[eetf::Term],
        compile_options: Vec<CompileOption>,
        override_compile_options: Vec<CompileOption>,
        elp_metadata: eetf::Term,
        format: Format,
    ) -> ParseResult {
        let mut options = vec![
            CompileOption::Macros(macros.to_vec()),
            CompileOption::ParseTransforms(parse_transforms.to_vec()),
            CompileOption::ElpMetadata(elp_metadata),
        ];
        let mut override_options = vec![];
        for option in compile_options {
            options.push(option.clone());
        }
        for option in override_compile_options {
            override_options.push(option.clone());
        }
        let path: PathBuf = path.to_path_buf().into();
        let file_text = self.file_text(file_id);
        let req = ParseRequest {
            options,
            override_options,
            file_id,
            path: path.clone(),
            format,
            file_text,
        };
        let erlang_service = self.erlang_service_for(project_id);
        let r = erlang_service.request_parse(
            req,
            || self.unwind_if_cancelled(),
            &move |file_id, include_type, path| resolve_include(self, file_id, include_type, &path),
        );
        r
    }
}

fn resolve_include(
    db: &dyn SourceDatabase,
    file_id: FileId,
    include_type: IncludeType,
    path: &str,
) -> Option<(String, FileId, Arc<str>)> {
    let include_file_id = match include_type {
        IncludeType::Normal => IncludeCtx::new(db, file_id).resolve_include(path)?,
        IncludeType::Lib => IncludeCtx::new(db, file_id).resolve_include_lib(path)?,
        IncludeType::Doc => IncludeCtx::new(db, file_id).resolve_include_doc(&path)?,
    };
    let path = path_for_file(db, include_file_id).map(|vfs_path| vfs_path.to_string())?;
    Some((path, include_file_id, db.file_text(include_file_id)))
}

#[salsa::query_group(ErlAstDatabaseStorage)]
pub trait ErlAstDatabase: SourceDatabase + AstLoader + LineIndexDatabase {
    fn module_ast(
        &self,
        file_id: FileId,
        format: Format,
        compile_options: Vec<CompileOption>,
        override_compile_options: Vec<CompileOption>,
    ) -> Arc<ParseResult>;
    fn elp_metadata(&self, file_id: FileId) -> Metadata;
}

fn module_ast(
    db: &dyn ErlAstDatabase,
    file_id: FileId,
    format: Format,
    compile_options: Vec<CompileOption>,
    override_compile_options: Vec<CompileOption>,
) -> Arc<ParseResult> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nmodule_ast: {:?}", file_id));
    let root_id = db.file_source_root(file_id);
    let root = db.source_root(root_id);
    let path = root.path_for_file(&file_id).unwrap().as_path().unwrap();
    let metadata = db.elp_metadata(file_id);
    let app_data = if let Some(app_data) = db.file_app_data(file_id) {
        app_data
    } else {
        return Arc::new(ParseResult::error(ParseError {
            path: path.to_path_buf().into(),
            location: None,
            msg: "Unknown application".to_string(),
            code: "L0003".to_string(),
        }));
    };
    Arc::new(db.load_ast(
        app_data.project_id,
        file_id,
        path,
        &app_data.macros,
        &app_data.parse_transforms,
        compile_options,
        override_compile_options,
        metadata.into(),
        format,
    ))
}

fn elp_metadata(db: &dyn ErlAstDatabase, file_id: FileId) -> Metadata {
    let line_index = db.file_line_index(file_id);
    let file_text = db.file_text(file_id);
    let source = db.parse(file_id);
    metadata::collect_metadata(&line_index, &file_text, &source)
}

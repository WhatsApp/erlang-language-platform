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
use elp_base_db::salsa::Database;
use elp_base_db::AbsPath;
use elp_base_db::AbsPathBuf;
use elp_base_db::FileId;
use elp_base_db::ProjectId;
use elp_base_db::SourceDatabase;
use elp_base_db::VfsPath;
use elp_erlang_service::Format;
use elp_erlang_service::ParseError;
use elp_erlang_service::ParseResult;

use crate::erlang_service::CompileOption;
use crate::erlang_service::ParseRequest;
use crate::metadata;
use crate::metadata::Metadata;
use crate::LineIndexDatabase;
use crate::RootDatabase;

pub trait AstLoader {
    fn load_ast(
        &self,
        project_id: ProjectId,
        path: &AbsPath,
        include_path: &[AbsPathBuf],
        macros: &[eetf::Term],
        parse_transforms: &[eetf::Term],
        compile_options: Vec<CompileOption>,
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
        compile_options: Vec<CompileOption>,
        elp_metadata: eetf::Term,
        format: Format,
    ) -> ParseResult {
        let includes = include_path
            .iter()
            .map(|path| path.clone().into())
            .collect();
        let mut options = vec![
            CompileOption::Includes(includes),
            CompileOption::Macros(macros.to_vec()),
            CompileOption::ParseTransforms(parse_transforms.to_vec()),
            CompileOption::ElpMetadata(elp_metadata),
        ];
        for option in compile_options {
            options.push(option.clone());
        }
        let path: PathBuf = path.to_path_buf().into();
        let req = ParseRequest {
            options,
            path: path.clone(),
            format,
        };
        if let Some(erlang_service) = self.erlang_services.read().get(&project_id).cloned() {
            let r = erlang_service.request_parse(req, || self.unwind_if_cancelled());
            let included_files = files_from_bytes(&r.files);
            for file in included_files {
                let file_path = PathBuf::from(file.clone());
                let file = if file_path.is_absolute() {
                    file
                } else {
                    match path.parent() {
                        None => file,
                        Some(file) => file
                            .to_path_buf()
                            .join(file)
                            .as_os_str()
                            .to_string_lossy()
                            .to_string(),
                    }
                };
                let file_path = VfsPath::new_real_path(file);
                if let Some(file_id) = find_path_in_project(self, project_id, &file_path) {
                    // Dummy read of file revision to make DB track changes
                    let _ = self.file_revision(file_id);
                }
            }
            r
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
    fn module_ast(
        &self,
        file_id: FileId,
        format: Format,
        compile_options: Vec<CompileOption>,
    ) -> Arc<ParseResult>;
    fn elp_metadata(&self, file_id: FileId) -> Metadata;
}

fn module_ast(
    db: &dyn ErlAstDatabase,
    file_id: FileId,
    format: Format,
    compile_options: Vec<CompileOption>,
) -> Arc<ParseResult> {
    // Dummy read of file revision make DB track changes.
    // Note that include file tracking tackes place in db.load_ast().
    let _ = db.file_revision(file_id);

    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nmodule_ast: {:?}", file_id));
    let root_id = db.file_source_root(file_id);
    let root = db.source_root(root_id);
    let path = root.path_for_file(&file_id).unwrap().as_path().unwrap();
    let metadata = db.elp_metadata(file_id);
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
    Arc::new(db.load_ast(
        app_data.project_id,
        path,
        &app_data.include_path,
        &app_data.macros,
        &app_data.parse_transforms,
        compile_options,
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

pub fn files_from_bytes(bytes: &[u8]) -> Vec<String> {
    let str = String::from_utf8_lossy(bytes);
    str.split('\n').map(|s| s.to_string()).collect::<Vec<_>>()
}

fn find_path_in_project(
    db: &RootDatabase,
    project_id: ProjectId,
    path: &VfsPath,
) -> Option<FileId> {
    let project = db.project_data(project_id);
    project
        .source_roots
        .iter()
        .find_map(|&source_root_id| db.source_root(source_root_id).file_for_path(path))
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use elp_base_db::salsa;
use elp_base_db::AbsPathBuf;
use elp_base_db::FileId;
use elp_base_db::ProjectId;
use elp_base_db::SourceDatabase;
use elp_erlang_service::common_test::ConversionError;
use elp_erlang_service::common_test::GroupDef;
use elp_erlang_service::common_test::TestDef;
use elp_erlang_service::EvalRequest;
use elp_syntax::SmolStr;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::db::MinDefDatabase;
use hir::DefMap;
use hir::Name;
use hir::NameArity;
use lazy_static::lazy_static;
use tempfile::Builder;
use tempfile::TempDir;

use crate::erlang_service::CompileOption;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CommonTestInfo {
    BadAST,
    Skipped,
    Result {
        all: FxHashSet<TestDef>,
        groups: FxHashMap<SmolStr, GroupDef>,
    },
    ConversionError(ConversionError),
    EvalError(String),
    SetupError(String),
}

#[salsa::query_group(CommonTestDatabaseStorage)]
pub trait CommonTestDatabase: MinDefDatabase + SourceDatabase + CommonTestLoader {
    #[salsa::invoke(ct_info)]
    fn ct_info(&self, file_id: FileId) -> Arc<CommonTestInfo>;
}

lazy_static! {
    static ref CT_INFO_TMP_DIR: Option<TempDir> =
        Builder::new().prefix("elp_ct_info_").tempdir().ok();
}

fn ct_info(db: &dyn CommonTestDatabase, file_id: FileId) -> Arc<CommonTestInfo> {
    let text = db.file_text(file_id);
    if let Some(tmp_dir) = &*CT_INFO_TMP_DIR {
        let root_id = db.file_source_root(file_id);
        let root = db.source_root(root_id);
        if let Some(path) = root.path_for_file(&file_id) {
            if let Some((filename, Some(extension))) = path.name_and_extension() {
                let tmp_filename = tmp_dir.path().join(format!("{filename}.{extension}"));
                let _ = fs::write(tmp_filename.clone(), String::from(&*text));
                let def_map = db.def_map(file_id);
                if let Some(project_id) = db.file_project_id(file_id) {
                    if let Some(app_data) = db.app_data(root_id) {
                        let module_index = db.module_index(project_id);
                        if let Some(module_name) = module_index.module_for_file(file_id) {
                            return Arc::new(db.check(
                                project_id,
                                module_name,
                                &def_map,
                                tmp_filename,
                                &app_data.include_path,
                                &app_data.macros,
                                &app_data.parse_transforms,
                            ));
                        }
                    }
                }
            }
        }
    }
    Arc::new(CommonTestInfo::SetupError(
        "Cannot extract CT Info".to_string(),
    ))
}

pub trait CommonTestLoader {
    fn check(
        &self,
        project_id: ProjectId,
        module_name: &str,
        def_map: &DefMap,
        src_path: PathBuf,
        include_path: &[AbsPathBuf],
        macros: &[eetf::Term],
        parse_transforms: &[eetf::Term],
    ) -> CommonTestInfo;
}

impl CommonTestLoader for crate::RootDatabase {
    fn check(
        &self,
        project_id: ProjectId,
        module_name: &str,
        def_map: &DefMap,
        src_path: PathBuf,
        include_path: &[AbsPathBuf],
        macros: &[eetf::Term],
        parse_transforms: &[eetf::Term],
    ) -> CommonTestInfo {
        if let Some(erlang_service) = self.erlang_services.read().get(&project_id).cloned() {
            let includes = include_path
                .iter()
                .map(|path| path.clone().into())
                .collect();
            let compile_options = vec![
                CompileOption::Includes(includes),
                CompileOption::Macros(macros.to_vec()),
                CompileOption::ParseTransforms(parse_transforms.to_vec()),
            ];
            let all_request = EvalRequest {
                src_path: src_path.clone(),
                expression: format!("{}:all().", module_name).to_string(),
                compile_options: compile_options.clone(),
            };
            match erlang_service.eval(all_request) {
                Ok(all_result) => match all_result.as_ct_all_result() {
                    Ok(all) => {
                        match def_map.is_function_exported(&NameArity::new(
                            Name::from_erlang_service("groups"),
                            0,
                        )) {
                            true => {
                                let groups_request = EvalRequest {
                                    src_path,
                                    expression: format!("{}:groups().", module_name).to_string(),
                                    compile_options,
                                };
                                match erlang_service.eval(groups_request) {
                                    Ok(groups_result) => {
                                        match groups_result.as_ct_groups_result() {
                                            Ok(groups) => CommonTestInfo::Result { all, groups },
                                            Err(err) => CommonTestInfo::ConversionError(err),
                                        }
                                    }
                                    Err(err) => CommonTestInfo::EvalError(err),
                                }
                            }
                            false => CommonTestInfo::Result {
                                all,
                                groups: FxHashMap::default(),
                            },
                        }
                    }
                    Err(err) => CommonTestInfo::ConversionError(err),
                },
                Err(err) => CommonTestInfo::EvalError(err),
            }
        } else {
            CommonTestInfo::SetupError("No Erlang service".to_string())
        }
    }
}

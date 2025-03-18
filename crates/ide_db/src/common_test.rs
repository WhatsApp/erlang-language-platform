/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use elp_base_db::salsa;
use elp_base_db::salsa::Database;
use elp_base_db::FileId;
use elp_base_db::ProjectId;
use elp_base_db::SourceDatabase;
use elp_erlang_service::common_test::ConversionError;
use elp_erlang_service::common_test::GroupDef;
use elp_erlang_service::common_test::TestDef;
use elp_erlang_service::CTInfoRequest;
use elp_erlang_service::Format;
use elp_syntax::SmolStr;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::db::DefDatabase;
use hir::DefMap;
use hir::Name;
use hir::NameArity;

use crate::ErlAstDatabase;

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
    NoProjectInfoError,
}

#[salsa::query_group(CommonTestDatabaseStorage)]
pub trait CommonTestDatabase: DefDatabase + SourceDatabase + CommonTestLoader {
    #[salsa::invoke(ct_info)]
    fn ct_info(&self, file_id: FileId) -> Arc<CommonTestInfo>;
}

fn ct_info(db: &dyn CommonTestDatabase, file_id: FileId) -> Arc<CommonTestInfo> {
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nct_info: {:?}", file_id));
    let def_map = db.def_map(file_id);
    if let Some(project_id) = db.file_project_id(file_id) {
        return Arc::new(db.check(project_id, file_id, &def_map));
    }
    Arc::new(CommonTestInfo::NoProjectInfoError)
}

pub trait CommonTestLoader {
    fn check(&self, project_id: ProjectId, file_id: FileId, def_map: &DefMap) -> CommonTestInfo;
}

impl CommonTestLoader for crate::RootDatabase {
    fn check(&self, project_id: ProjectId, file_id: FileId, def_map: &DefMap) -> CommonTestInfo {
        let erlang_service = self.erlang_service_for(project_id);
        let should_request_groups =
            def_map.is_function_exported(&NameArity::new(Name::from_erlang_service("groups"), 0));
        let module_ast = self.module_ast(file_id, Format::OffsetEtf, vec![], vec![]);
        if !module_ast.is_ok() {
            return CommonTestInfo::BadAST;
        }
        let index = self.module_index(project_id);
        let module = index.module_for_file(file_id);

        let request = CTInfoRequest {
            should_request_groups,
            file_abstract_forms: module_ast.ast.clone(),
        };
        match erlang_service.ct_info(module, request, || self.unwind_if_cancelled()) {
            Ok(result) => match result.all() {
                Ok(all) => match result.groups() {
                    Ok(groups) => CommonTestInfo::Result { all, groups },
                    Err(err) => CommonTestInfo::ConversionError(err),
                },
                Err(err) => CommonTestInfo::ConversionError(err),
            },
            Err(err) => CommonTestInfo::EvalError(err),
        }
    }
}

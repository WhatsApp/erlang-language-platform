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
use elp_base_db::AbsPath;
use elp_base_db::FileId;
use elp_base_db::FileSource;
use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_base_db::SourceDatabase;
use elp_base_db::SourceRootId;
use elp_eqwalizer::ast::db::EqwalizerASTDatabase;
use elp_eqwalizer::ast::db::EqwalizerErlASTStorage;
use elp_eqwalizer::ast::Error;
use elp_eqwalizer::ipc::IpcHandle;
use elp_eqwalizer::EqwalizerDiagnostics;
use elp_eqwalizer::EqwalizerDiagnosticsDatabase;
use elp_eqwalizer::EqwalizerStats;
use elp_syntax::ast;
use parking_lot::Mutex;

use crate::ErlAstDatabase;

pub trait EqwalizerLoader {
    fn typecheck(
        &self,
        project_id: ProjectId,
        build_info_path: &AbsPath,
        modules: Vec<FileId>,
    ) -> EqwalizerDiagnostics;
}

impl EqwalizerLoader for crate::RootDatabase {
    fn typecheck(
        &self,
        project_id: ProjectId,
        build_info_path: &AbsPath,
        modules: Vec<FileId>,
    ) -> EqwalizerDiagnostics {
        let module_index = self.module_index(project_id);
        let module_names: Vec<&str> = modules
            .iter()
            .map(|&f| module_index.module_for_file(f).unwrap().as_str())
            .collect();
        self.eqwalizer
            .typecheck(build_info_path.as_ref(), self, project_id, module_names)
    }
}

#[salsa::query_group(EqwalizerDatabaseStorage)]
pub trait EqwalizerDatabase:
    EqwalizerDiagnosticsDatabase
    + EqwalizerASTDatabase
    + SourceDatabase
    + EqwalizerLoader
    + ErlAstDatabase
{
    fn eqwalizer_diagnostics(
        &self,
        project_id: ProjectId,
        file_ids: Vec<FileId>,
    ) -> Arc<EqwalizerDiagnostics>;
    fn eqwalizer_stats(
        &self,
        project_id: ProjectId,
        file_id: FileId,
    ) -> Option<Arc<EqwalizerStats>>;
    fn has_eqwalizer_app_marker(&self, source_root_id: SourceRootId) -> bool;
    fn has_eqwalizer_module_marker(&self, file_id: FileId) -> bool;
    fn has_eqwalizer_ignore_marker(&self, file_id: FileId) -> bool;
    fn is_eqwalizer_enabled(&self, file_id: FileId, include_generated: bool) -> bool;
}

fn eqwalizer_diagnostics(
    db: &dyn EqwalizerDatabase,
    project_id: ProjectId,
    file_ids: Vec<FileId>,
) -> Arc<EqwalizerDiagnostics> {
    let project = db.project_data(project_id);
    if let Some(build_info_path) = &project.build_info_path {
        Arc::new(db.typecheck(project_id, build_info_path, file_ids))
    } else {
        //
        log::error!("EqWAlizing in a fixture project");
        Arc::new(EqwalizerDiagnostics::Error(
            "EqWAlizing in a fixture project".to_string(),
        ))
    }
}

fn eqwalizer_stats(
    db: &dyn EqwalizerDatabase,
    project_id: ProjectId,
    file_id: FileId,
) -> Option<Arc<EqwalizerStats>> {
    let module_index = db.module_index(project_id);
    let module_name: &str = module_index.module_for_file(file_id)?.as_str();
    db.compute_eqwalizer_stats(project_id, ModuleName::new(module_name))
}

fn is_eqwalizer_enabled(
    db: &dyn EqwalizerDatabase,
    file_id: FileId,
    include_generated: bool,
) -> bool {
    if !include_generated && db.is_generated(file_id) {
        return false;
    }

    let source_root = db.file_source_root(file_id);
    let app_data = if let Some(app_data) = db.app_data(source_root) {
        app_data
    } else {
        return false;
    };
    let project_id = app_data.project_id;
    let project = db.project_data(project_id);
    let eqwalizer_config = &project.eqwalizer_config;
    let module_index = db.module_index(project_id);
    let is_src = module_index.file_source_for_file(file_id) == Some(FileSource::Src);
    let app_or_global_opt_in =
        eqwalizer_config.enable_all || db.has_eqwalizer_app_marker(source_root);
    let opt_in = (app_or_global_opt_in && is_src) || db.has_eqwalizer_module_marker(file_id);
    let ignored = db.has_eqwalizer_ignore_marker(file_id);
    opt_in && !ignored
}

fn has_eqwalizer_app_marker(db: &dyn EqwalizerDatabase, source_root_id: SourceRootId) -> bool {
    if let Some(app_data) = db.app_data(source_root_id) {
        let source_root = db.source_root(source_root_id);
        return source_root.has_eqwalizer_marker(&app_data);
    }
    false
}

fn has_eqwalizer_module_marker(db: &dyn EqwalizerDatabase, file_id: FileId) -> bool {
    let parsed = db.parse(file_id);
    parsed
        .tree()
        .forms()
        .take_while(|form| !matches!(form, ast::Form::FunDecl(_)))
        .filter_map(|form| match form {
            ast::Form::WildAttribute(attr) => Some(attr),
            _ => None,
        })
        .filter(|attr| {
            attr.name()
                .and_then(|attr_name| match attr_name.name()? {
                    ast::Name::Atom(atom) => atom.self_token(),
                    ast::Name::MacroCallExpr(_) | ast::Name::Var(_) => None,
                })
                .map(|token| token.text() == "typing")
                .unwrap_or_default()
        })
        .any(|attr| attr.value().map(has_eqwalizer_atom).unwrap_or_default())
}

fn has_eqwalizer_ignore_marker(db: &dyn EqwalizerDatabase, file_id: FileId) -> bool {
    let parsed = db.parse(file_id);
    parsed
        .tree()
        .forms()
        .take_while(|form| !matches!(form, ast::Form::FunDecl(_)))
        .filter_map(|form| match form {
            ast::Form::WildAttribute(attr) => Some(attr),
            _ => None,
        })
        .filter(|attr| {
            attr.name()
                .and_then(|attr_name| match attr_name.name()? {
                    ast::Name::Atom(atom) => atom.self_token(),
                    ast::Name::MacroCallExpr(_) | ast::Name::Var(_) => None,
                })
                .map(|token| token.text() == "eqwalizer")
                .unwrap_or_default()
        })
        .any(|attr| {
            attr.value()
                .map(is_ignore_or_fixme_atom)
                .unwrap_or_default()
        })
}

impl EqwalizerErlASTStorage for crate::RootDatabase {
    fn get_erl_ast_bytes(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<Vec<u8>>, Error> {
        if let Some(file_id) = self.module_index(project_id).file_for_module(&module) {
            let result = self.module_ast(file_id, elp_erlang_service::Format::OffsetEtf);
            if result.is_ok() {
                Ok(result.ast.clone())
            } else {
                Err(Error::ParseError)
            }
        } else {
            Err(Error::ModuleNotFound(module.as_str().into()))
        }
    }

    fn get_erl_stub_bytes(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<Vec<u8>>, Error> {
        if let Some(file_id) = self.module_index(project_id).file_for_module(&module) {
            let result = self.module_ast(file_id, elp_erlang_service::Format::OffsetEtf);
            if result.is_ok() {
                Ok(result.stub.clone())
            } else {
                Err(Error::ParseError)
            }
        } else {
            Err(Error::ModuleNotFound(module.as_str().into()))
        }
    }
}

impl elp_eqwalizer::DbApi for crate::RootDatabase {
    fn eqwalizing_start(&self, module: String) -> () {
        if let Some(reporter) = self.eqwalizer_progress_reporter.lock().as_mut() {
            reporter.start_module(module)
        }
    }

    fn eqwalizing_done(&self, module: String) -> () {
        if let Some(reporter) = self.eqwalizer_progress_reporter.lock().as_mut() {
            reporter.done_module(&module);
        }
    }

    fn set_module_ipc_handle(&self, module: ModuleName, handle: Arc<Mutex<IpcHandle>>) -> () {
        self.ipc_handles
            .write()
            .insert(module.as_str().into(), handle.clone());
    }

    fn module_ipc_handle(&self, module: ModuleName) -> Option<Arc<Mutex<IpcHandle>>> {
        self.ipc_handles
            .read()
            .get(module.as_str().into())
            .map(|v| v.to_owned())
    }
}

fn has_eqwalizer_atom(expr: ast::Expr) -> bool {
    match expr {
        ast::Expr::ExprMax(ast::ExprMax::ParenExpr(expr)) => match expr.expr() {
            Some(ast::Expr::ExprMax(ast::ExprMax::List(list))) => {
                list.exprs().any(|expr| match expr {
                    ast::Expr::ExprMax(ast::ExprMax::Atom(atom)) => atom
                        .self_token()
                        .map(|token| token.text() == "eqwalizer")
                        .unwrap_or_default(),
                    _ => false,
                })
            }
            _ => false,
        },
        _ => false,
    }
}

fn is_ignore_or_fixme_atom(expr: ast::Expr) -> bool {
    match expr {
        ast::Expr::ExprMax(ast::ExprMax::ParenExpr(expr)) => match expr.expr() {
            Some(ast::Expr::ExprMax(ast::ExprMax::Atom(atom))) => atom
                .self_token()
                .map(|token| token.text() == "ignore" || token.text() == "fixme")
                .unwrap_or_default(),
            _ => false,
        },
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use elp_base_db::fixture::WithFixture;

    use super::*;
    use crate::RootDatabase;

    #[test]
    fn test_has_eqwalizer_module_marker() {
        let (db, file_id) = RootDatabase::with_single_file(
            r#"
-module(test).
"#,
        );

        assert!(!db.has_eqwalizer_module_marker(file_id));

        let (db, file_id) = RootDatabase::with_single_file(
            r#"
-module(test).

-typing([eqwalizer]).
"#,
        );

        assert!(db.has_eqwalizer_module_marker(file_id));
    }

    #[test]
    fn test_has_eqwalizer_app_marker() {
        let (db, file_ids) = RootDatabase::with_many_files(
            r#"
//- /src/test.erl
-module(test).
"#,
        );

        let source_root = db.file_source_root(file_ids[0]);
        assert!(!db.has_eqwalizer_app_marker(source_root));

        let (db, file_ids) = RootDatabase::with_many_files(
            r#"
//- /src/test.erl
-module(test).
//- /.eqwalizer
"#,
        );

        let source_root = db.file_source_root(file_ids[0]);
        assert!(db.has_eqwalizer_app_marker(source_root));
    }

    #[test]
    fn test_has_eqwalizer_ignore_marker() {
        let (db, file_id) = RootDatabase::with_single_file(
            r#"
-module(test).
"#,
        );

        assert!(!db.has_eqwalizer_ignore_marker(file_id));

        let (db, file_id) = RootDatabase::with_single_file(
            r#"
-module(test).

-eqwalizer(ignore).
"#,
        );

        assert!(db.has_eqwalizer_ignore_marker(file_id));

        let (db, file_id) = RootDatabase::with_single_file(
            r#"
-module(test).

-eqwalizer(fixme).
"#,
        );

        assert!(db.has_eqwalizer_ignore_marker(file_id));
    }
}

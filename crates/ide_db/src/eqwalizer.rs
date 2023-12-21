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
use elp_base_db::FilePosition;
use elp_base_db::FileRange;
use elp_base_db::FileSource;
use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_base_db::SourceDatabase;
use elp_base_db::SourceRootId;
use elp_base_db::VfsPath;
use elp_eqwalizer::ast::db::EqwalizerASTDatabase;
use elp_eqwalizer::ast::db::EqwalizerErlASTStorage;
use elp_eqwalizer::ast::types::RecordType;
use elp_eqwalizer::ast::types::Type;
use elp_eqwalizer::ast::Error;
use elp_eqwalizer::ast::Pos;
use elp_eqwalizer::ast::RemoteId;
use elp_eqwalizer::ipc::IpcHandle;
use elp_eqwalizer::EqwalizerDiagnostics;
use elp_eqwalizer::EqwalizerDiagnosticsDatabase;
use elp_eqwalizer::EqwalizerStats;
use elp_syntax::ast;
use elp_syntax::SmolStr;
use fxhash::FxHashSet;
use parking_lot::Mutex;

use crate::ErlAstDatabase;
use crate::LineCol;

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
        let mut module_names = vec![];
        for module in modules {
            match module_index.module_for_file(module) {
                Some(f) => module_names.push(f.as_str()),
                None => {
                    // Context for T171541590
                    let _ = stdx::panic_context::enter(format!("\ntypecheck: {:?}", module));
                    let source_root_id = self.file_source_root(module);
                    let source_root = self.source_root(source_root_id);
                    let path = source_root.path_for_file(&module);
                    log::error!("Can't find module for path: {:?}", path);
                    continue;
                }
            };
        }
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
    fn type_at_position(
        &self,
        project_id: ProjectId,
        position: FilePosition,
    ) -> Option<Arc<(Type, FileRange)>>;
    fn has_eqwalizer_app_marker(&self, source_root_id: SourceRootId) -> bool;
    fn has_eqwalizer_module_marker(&self, file_id: FileId) -> bool;
    fn has_eqwalizer_ignore_marker(&self, file_id: FileId) -> bool;
    fn is_eqwalizer_enabled(&self, file_id: FileId, include_generated: bool) -> bool;
}

pub fn eqwalizer_diagnostics(
    db: &dyn EqwalizerDatabase,
    project_id: ProjectId,
    file_ids: Vec<FileId>,
) -> Arc<EqwalizerDiagnostics> {
    let project = db.project_data(project_id);
    if let Some(build_info_path) = &project.build_info_path {
        Arc::new(db.typecheck(project_id, build_info_path, file_ids))
    } else {
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

fn type_at_position(
    db: &dyn EqwalizerDatabase,
    project_id: ProjectId,
    position: FilePosition,
) -> Option<Arc<(Type, FileRange)>> {
    if !db.is_eqwalizer_enabled(position.file_id, true) {
        return None;
    }
    if let EqwalizerDiagnostics::Diagnostics { type_info, .. } =
        &(*eqwalizer_diagnostics(db, project_id, vec![position.file_id]))
    {
        let offset: u32 = position.offset.into();
        let module_index = db.module_index(project_id);
        let module = module_index.module_for_file(position.file_id)?;
        let file_types = type_info.get(&module.to_string())?;
        let (text_range, ty) = file_types
            .iter()
            .filter_map(|(pos, ty)| match pos {
                Pos::TextRange(r) => {
                    if r.start_byte > offset || r.end_byte < offset {
                        None
                    } else {
                        Some((r, ty))
                    }
                }
                _ => None,
            })
            .min_by_key(|(range, _)| range.end_byte - range.start_byte)?;
        let range = FileRange {
            file_id: position.file_id,
            range: text_range.clone().into(),
        };
        return Some(Arc::new((ty.clone(), range)));
    }
    None
}

fn is_eqwalizer_enabled(
    db: &dyn EqwalizerDatabase,
    file_id: FileId,
    include_generated: bool,
) -> bool {
    if !include_generated && db.is_generated(file_id) {
        return false;
    }

    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nis_eqwalizer_enabled: {:?}", file_id));
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

fn find_path_in_project(
    db: &dyn EqwalizerDatabase,
    project_id: ProjectId,
    path: &VfsPath,
) -> Option<FileId> {
    let project = db.project_data(project_id);
    project
        .source_roots
        .iter()
        .find_map(|&source_root_id| db.source_root(source_root_id).file_for_path(path))
}

fn decl_location(
    db: &dyn EqwalizerDatabase,
    project_id: ProjectId,
    module: ModuleName,
    pos: &Pos,
    file: SmolStr,
) -> Option<FileRange> {
    let module_index = db.module_index(project_id);
    let module_file_id = module_index.file_for_module(&module)?;
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\ndecl_location: {:?}", module_file_id));
    let source_root_id = db.file_source_root(module_file_id);
    let source_root = db.source_root(source_root_id);
    let decl_file_path = &source_root
        .path_for_file(&module_file_id)?
        .join(file.as_str())?;
    let file_id = find_path_in_project(db, project_id, decl_file_path)?;
    let range: elp_syntax::TextRange = {
        match pos {
            Pos::LineAndColumn(lc) => {
                let line_index = db.file_line_index(file_id);
                let line_col = LineCol {
                    line: lc.line - 1,
                    col_utf16: lc.column - 1,
                };
                let offset = line_index.offset(line_col);
                elp_syntax::TextRange::empty(offset)
            }
            Pos::TextRange(tr) => tr.to_owned().into(),
        }
    };
    Some(FileRange { file_id, range })
}

fn id_name_and_location(
    db: &dyn EqwalizerDatabase,
    project_id: ProjectId,
    type_id: &RemoteId,
) -> Option<(SmolStr, FileRange)> {
    let module = ModuleName::new(type_id.module.as_str());
    let stub = db.transitive_stub(project_id, module.clone()).ok()?;
    let decl = stub.types.get(&type_id.to_owned().into())?;
    let loc = decl_location(db, project_id, module, &decl.location, decl.file.clone()?)?;
    Some((type_id.to_string().into(), loc))
}

fn record_name_and_location(
    db: &dyn EqwalizerDatabase,
    project_id: ProjectId,
    record: &RecordType,
) -> Option<(SmolStr, FileRange)> {
    let module = ModuleName::new(record.module.as_str());
    let stub = db.transitive_stub(project_id, module.clone()).ok()?;
    let decl = stub.records.get(&record.name)?;
    let loc = decl_location(db, project_id, module, &decl.location, decl.file.clone()?)?;
    Some((format!("#{}:{}", record.module, record.name).into(), loc))
}

fn collect_references(
    db: &dyn EqwalizerDatabase,
    project_id: ProjectId,
    ty: &Type,
    links: &mut FxHashSet<(SmolStr, FileRange)>,
) {
    match ty {
        Type::RemoteType(rt) => {
            if let Some(link) = id_name_and_location(db, project_id, &rt.id) {
                links.insert(link);
            };
        }
        Type::OpaqueType(ot) => {
            if let Some(link) = id_name_and_location(db, project_id, &ot.id) {
                links.insert(link);
            };
        }
        Type::RecordType(rt) => {
            if let Some(link) = record_name_and_location(db, project_id, rt) {
                links.insert(link);
            };
        }
        Type::RefinedRecordType(rt) => {
            if let Some(link) = record_name_and_location(db, project_id, &rt.rec_type) {
                links.insert(link);
            };
            return;
        }
        _ => (),
    }
    ty.visit_children::<()>(&mut |ty| {
        collect_references(db, project_id, ty, links);
        Ok(())
    })
    .ok();
}

pub fn type_references(
    db: &dyn EqwalizerDatabase,
    project_id: ProjectId,
    ty: &Type,
) -> Vec<(SmolStr, FileRange)> {
    let mut links = FxHashSet::default();
    collect_references(db, project_id, ty, &mut links);
    links.into_iter().collect()
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
    fn eqwalizing_start(&self, module: String) {
        if let Some(reporter) = self.eqwalizer_progress_reporter.lock().as_mut() {
            reporter.start_module(module)
        }
    }

    fn eqwalizing_done(&self, module: String) {
        if let Some(reporter) = self.eqwalizer_progress_reporter.lock().as_mut() {
            reporter.done_module(&module);
        }
    }

    fn set_module_ipc_handle(&self, module: ModuleName, handle: Option<Arc<Mutex<IpcHandle>>>) {
        match handle {
            Some(handle) => {
                self.ipc_handles
                    .write()
                    .insert(module.as_str().into(), handle);
            }
            None => {
                self.ipc_handles.write().remove(module.as_str().into());
            }
        }
    }

    fn module_ipc_handle(&self, module: ModuleName) -> Option<Arc<Mutex<IpcHandle>>> {
        self.ipc_handles
            .read()
            .get(module.as_str())
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

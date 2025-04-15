/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::sync::Arc;
use std::time::Instant;

use elp_base_db::AbsPathBuf;
use elp_base_db::AppType;
use elp_base_db::FileId;
use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_base_db::SourceDatabase;
use elp_types_db::eqwalizer::AST;
use elp_types_db::eqwalizer::Id;
use elp_types_db::eqwalizer::form::ExternalForm;
use parking_lot::Mutex;

use crate::EqwalizerConfig;
use crate::EqwalizerDiagnostics;
use crate::ast;
use crate::ast::Error;
use crate::ast::Visibility;
use crate::ast::contractivity::StubContractivityChecker;
use crate::ast::expand::StubExpander;
use crate::ast::stub::ModuleStub;
use crate::ast::stub::VStub;
use crate::ast::trans_valid::TransitiveChecker;
use crate::ast::variance_check::VarianceChecker;
use crate::get_module_diagnostics;
use crate::ipc::IpcHandle;

pub trait EqwalizerErlASTStorage {
    fn erl_ast_bytes(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<Vec<u8>>, Error>;

    fn eqwalizer_ast(&self, project_id: ProjectId, module: ModuleName) -> Result<Arc<AST>, Error> {
        let ast = self.erl_ast_bytes(project_id, module)?;
        ast::from_bytes(&ast, false).map(Arc::new)
    }

    fn eqwalizer_ast_bytes(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<Vec<u8>>, Error> {
        self.eqwalizer_ast(project_id, module).map(|ast| {
            Arc::new(ast::to_bytes(
                &ast.forms.iter().filter(is_non_stub_form).collect(),
            ))
        })
    }
}

pub trait ELPDbApi {
    fn eqwalizing_start(&self, module: String);
    fn eqwalizing_done(&self, module: String);
    fn set_module_ipc_handle(&self, module: ModuleName, handle: Option<Arc<Mutex<IpcHandle>>>);
    fn module_ipc_handle(&self, module: ModuleName) -> Option<Arc<Mutex<IpcHandle>>>;
}

#[salsa::query_group(EqwalizerDiagnosticsDatabaseStorage)]
pub trait EqwalizerDiagnosticsDatabase: EqwalizerErlASTStorage + SourceDatabase + ELPDbApi {
    #[salsa::input]
    fn eqwalizer_config(&self) -> Arc<EqwalizerConfig>;

    fn module_diagnostics(
        &self,
        project_id: ProjectId,
        module: String,
    ) -> (Arc<EqwalizerDiagnostics>, Instant);

    fn converted_stub(&self, project_id: ProjectId, module: ModuleName) -> Result<Arc<AST>, Error>;

    fn type_ids(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<BTreeMap<Id, Visibility>>, Error>;

    fn expanded_stub(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<ModuleStub>, Error>;

    fn contractive_stub(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<VStub>, Error>;

    fn covariant_stub(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<VStub>, Error>;

    fn transitive_stub(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<ModuleStub>, Error>;
    fn transitive_stub_bytes(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<Vec<u8>>, Error>;
}

fn module_diagnostics(
    db: &dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    module: String,
) -> (Arc<EqwalizerDiagnostics>, Instant) {
    // A timestamp is added to the return value to force Salsa to store new
    // diagnostics, and not attempt to back-date them if they are equal to
    // the memoized ones.
    let timestamp = Instant::now();
    // Dummy read eqWAlizer config for Salsa
    // Ideally, the config should be passed per module to eqWAlizer instead
    // of being set in the command's environment
    let _ = db.eqwalizer_config();
    match get_module_diagnostics(db, project_id, module.clone()) {
        Ok(diag) => (Arc::new(diag), timestamp),
        Err(err) => (
            Arc::new(EqwalizerDiagnostics::Error(format!(
                "eqWAlizing module {}:\n{}",
                module, err
            ))),
            timestamp,
        ),
    }
}

fn is_non_stub_form(form: &&ExternalForm) -> bool {
    match form {
        ExternalForm::Module(_) => true,
        ExternalForm::FunDecl(_) => true,
        ExternalForm::File(_) => true,
        ExternalForm::ElpMetadata(_) => true,
        ExternalForm::Behaviour(_) => true,
        ExternalForm::EqwalizerNowarnFunction(_) => true,
        ExternalForm::EqwalizerUnlimitedRefinement(_) => true,
        _ => false,
    }
}

fn converted_stub(
    db: &dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<AST>, Error> {
    if let Some(file_id) = db.module_index(project_id).file_for_module(&module) {
        if let Some(beam_path) = from_beam_path(db, file_id, &module) {
            if let Ok(beam_contents) = std::fs::read(&beam_path) {
                ast::from_beam(&beam_contents).map(Arc::new)
            } else {
                Err(Error::BEAMNotFound(beam_path.into()))
            }
        } else {
            let ast = db.erl_ast_bytes(project_id, module)?;
            ast::from_bytes(&ast, true).map(Arc::new)
        }
    } else {
        Err(Error::ModuleNotFound(module.as_str().into()))
    }
}

fn from_beam_path(
    db: &dyn EqwalizerDiagnosticsDatabase,
    file_id: FileId,
    module: &ModuleName,
) -> Option<AbsPathBuf> {
    let app_data = db.file_app_data(file_id)?;
    if app_data.app_type != AppType::Otp {
        // Only OTP modules are loaded from BEAM
        return None;
    }
    let ebin = app_data.ebin_path.as_ref()?;
    let filename = format!("{}.beam", module.as_str());
    Some(ebin.join(filename))
}

fn type_ids(
    db: &dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<BTreeMap<Id, Visibility>>, Error> {
    db.converted_stub(project_id, module)
        .map(|ast| Arc::new(ast::type_ids(&ast)))
}

fn expanded_stub(
    db: &dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<ModuleStub>, Error> {
    let ast = db.converted_stub(project_id, module.clone())?;
    let mut expander = StubExpander::new(db, project_id, module.as_str().into(), &ast);
    expander
        .expand(&ast.forms)
        .map(|()| Arc::new(expander.stub))
        .map_err(Error::TypeConversionError)
}

fn contractive_stub(
    db: &dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<VStub>, Error> {
    let stub = db.expanded_stub(project_id, module.clone())?;
    let checker = StubContractivityChecker::new(db, project_id, module.as_str().into());
    checker
        .check(stub)
        .map(Arc::new)
        .map_err(Error::ContractivityError)
}

fn covariant_stub(
    db: &dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<VStub>, Error> {
    let v_stub = db.contractive_stub(project_id, module)?;
    let checker = VarianceChecker::new(db, project_id);
    checker
        .check(&v_stub)
        .map(Arc::new)
        .map_err(Error::VarianceCheckError)
}

fn transitive_stub(
    db: &dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<ModuleStub>, Error> {
    let v_stub = db.covariant_stub(project_id, module.clone())?;
    let mut checker = TransitiveChecker::new(db, project_id, module.as_str().into());
    checker
        .check(&v_stub)
        .map(Arc::new)
        .map_err(Error::TransitiveCheckError)
}

fn transitive_stub_bytes(
    db: &dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<Vec<u8>>, Error> {
    db.transitive_stub(project_id, module)
        .map(|stub| Arc::new(stub.to_bytes()))
}

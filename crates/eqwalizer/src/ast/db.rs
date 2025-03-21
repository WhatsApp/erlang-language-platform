/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use elp_base_db::AbsPathBuf;
use elp_base_db::AppType;
use elp_base_db::FileId;
use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_base_db::SourceDatabase;
use elp_types_db::eqwalizer::form::ExternalForm;
use fxhash::FxHashMap;

use super::contractivity::StubContractivityChecker;
use super::expand::StubExpander;
use super::stub::ModuleStub;
use super::stub::VStub;
use super::trans_valid::TransitiveChecker;
use super::variance_check::VarianceChecker;
use super::Error;
use super::Id;
use super::AST;
use crate::ast::Visibility;

pub trait EqwalizerErlASTStorage {
    fn erl_ast_bytes(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<Vec<u8>>, Error>;

    fn eqwalizer_ast(&self, project_id: ProjectId, module: ModuleName) -> Result<Arc<AST>, Error> {
        let ast = self.erl_ast_bytes(project_id, module)?;
        super::from_bytes(&ast, false).map(Arc::new)
    }

    fn eqwalizer_ast_bytes(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<Vec<u8>>, Error> {
        self.eqwalizer_ast(project_id, module).map(|ast| {
            Arc::new(super::to_bytes(
                &ast.forms.iter().filter(is_non_stub_form).collect(),
            ))
        })
    }
}

#[salsa::query_group(EqwalizerASTDatabaseStorage)]
pub trait EqwalizerASTDatabase: EqwalizerErlASTStorage + SourceDatabase {
    fn converted_stub(&self, project_id: ProjectId, module: ModuleName) -> Result<Arc<AST>, Error>;

    fn type_ids(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<FxHashMap<Id, Visibility>>, Error>;

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
    db: &dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<AST>, Error> {
    if let Some(file_id) = db.module_index(project_id).file_for_module(&module) {
        if let Some(beam_path) = from_beam_path(db, file_id, &module) {
            if let Ok(beam_contents) = std::fs::read(&beam_path) {
                super::from_beam(&beam_contents).map(Arc::new)
            } else {
                Err(Error::BEAMNotFound(beam_path.into()))
            }
        } else {
            let ast = db.erl_ast_bytes(project_id, module)?;
            super::from_bytes(&ast, true).map(Arc::new)
        }
    } else {
        Err(Error::ModuleNotFound(module.as_str().into()))
    }
}

fn from_beam_path(
    db: &dyn EqwalizerASTDatabase,
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
    db: &dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<FxHashMap<Id, Visibility>>, Error> {
    db.converted_stub(project_id, module)
        .map(|ast| Arc::new(super::type_ids(&ast)))
}

fn expanded_stub(
    db: &dyn EqwalizerASTDatabase,
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
    db: &dyn EqwalizerASTDatabase,
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
    db: &dyn EqwalizerASTDatabase,
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
    db: &dyn EqwalizerASTDatabase,
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
    db: &dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<Vec<u8>>, Error> {
    db.transitive_stub(project_id, module)
        .map(|stub| Arc::new(stub.to_bytes()))
}

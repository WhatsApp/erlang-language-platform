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
use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_base_db::SourceDatabase;
use fxhash::FxHashSet;

use super::contractivity::StubContractivityChecker;
use super::expand::StubExpander;
use super::stub::ModuleStub;
use super::trans_valid::TransitiveChecker;
use super::variance_check::VarianceChecker;
use super::Error;
use super::Id;
use super::AST;

pub trait EqwalizerErlASTStorage {
    fn get_erl_ast_bytes(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<Vec<u8>>, Error>;
    fn get_erl_stub_bytes(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<Vec<u8>>, Error>;
}

#[salsa::query_group(EqwalizerASTDatabaseStorage)]
pub trait EqwalizerASTDatabase: EqwalizerErlASTStorage + SourceDatabase {
    fn from_beam(&self, project_id: ProjectId, module: ModuleName) -> bool;

    fn converted_ast(&self, project_id: ProjectId, module: ModuleName) -> Result<Arc<AST>, Error>;
    fn converted_ast_bytes(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<Vec<u8>>, Error>;
    fn converted_stub(&self, project_id: ProjectId, module: ModuleName) -> Result<Arc<AST>, Error>;

    fn type_ids(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<FxHashSet<Id>>, Error>;
    fn exported_type_ids(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<FxHashSet<Id>>, Error>;

    fn expanded_stub(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<ModuleStub>, Error>;

    fn contractive_stub(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<ModuleStub>, Error>;

    fn covariant_stub(
        &self,
        project_id: ProjectId,
        module: ModuleName,
    ) -> Result<Arc<ModuleStub>, Error>;

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

fn from_beam(db: &dyn EqwalizerASTDatabase, project_id: ProjectId, module: ModuleName) -> bool {
    if let Some(file_id) = db.module_index(project_id).file_for_module(&module) {
        // Context for T171541590
        let _ = stdx::panic_context::enter(format!("\nfrom_beam: {:?}", file_id));
        let source_root = db.file_source_root(file_id);
        if let Some(app) = db.app_data(source_root) {
            return app.app_type == AppType::Otp;
        }
    }
    false
}

fn converted_ast(
    db: &dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<AST>, Error> {
    let ast = db.get_erl_ast_bytes(project_id, module)?;
    super::from_bytes(&ast).map(Arc::new)
}

fn converted_ast_bytes(
    db: &dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<Vec<u8>>, Error> {
    db.converted_ast(project_id, module)
        .map(|ast| Arc::new(super::to_bytes(&ast)))
}

fn converted_stub(
    db: &dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<AST>, Error> {
    if db.from_beam(project_id, module.to_owned()) {
        if let Some(beam_path) = beam_path(db, project_id, module.to_owned()) {
            if let Ok(beam_contents) = std::fs::read(&beam_path) {
                super::from_beam(&beam_contents).map(Arc::new)
            } else {
                Err(Error::BEAMNotFound(beam_path.into()))
            }
        } else {
            Err(Error::ModuleNotFound(module.as_str().into()))
        }
    } else {
        let stub = db.get_erl_stub_bytes(project_id, module)?;
        super::from_bytes(&stub).map(Arc::new)
    }
}

fn beam_path(
    db: &dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Option<AbsPathBuf> {
    let file_id = db.module_index(project_id).file_for_module(&module)?;
    // Context for T171541590
    let _ = stdx::panic_context::enter(format!("\nbeam_path: {:?}", file_id));
    let source_root = db.file_source_root(file_id);
    let app = db.app_data(source_root)?;
    let ebin = app.ebin_path.as_ref()?;
    let filename = format!("{}.beam", module.as_str());
    Some(ebin.join(filename))
}

fn type_ids(
    db: &dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<FxHashSet<Id>>, Error> {
    db.converted_stub(project_id, module)
        .map(|ast| Arc::new(super::type_ids(&ast)))
}

fn exported_type_ids(
    db: &dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<FxHashSet<Id>>, Error> {
    db.converted_stub(project_id, module)
        .map(|ast| Arc::new(super::exported_type_ids(&ast)))
}

fn expanded_stub(
    db: &dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<ModuleStub>, Error> {
    let stub = db.converted_stub(project_id, module.clone())?;
    let mut expander = StubExpander::new(db, project_id, true, module.as_str().into(), &stub);
    expander
        .expand(stub.to_vec())
        .map(|()| Arc::new(expander.stub))
        .map_err(Error::TypeConversionError)
}

fn contractive_stub(
    db: &dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<ModuleStub>, Error> {
    let stub = db.expanded_stub(project_id, module.clone())?;
    let checker = StubContractivityChecker::new(db, project_id, module.as_str().into());
    checker
        .check(&stub)
        .map(Arc::new)
        .map_err(Error::ContractivityError)
}

fn covariant_stub(
    db: &dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<ModuleStub>, Error> {
    let stub = db.contractive_stub(project_id, module)?;
    let checker = VarianceChecker::new(db, project_id);
    checker
        .check(&stub)
        .map(Arc::new)
        .map_err(Error::VarianceCheckError)
}

fn transitive_stub(
    db: &dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: ModuleName,
) -> Result<Arc<ModuleStub>, Error> {
    let stub = db.covariant_stub(project_id, module.clone())?;
    let mut checker = TransitiveChecker::new(db, project_id, module.as_str().into());
    checker
        .check(&stub)
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

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This module performs the fourth and last step of stubs validation
//!
//! It ensures that declarations are transitively valid by propagating
//! all invalid declarations. I.e., if a type t1 depends on a type t2
//! and t2 is invalid, then t1 will be tagged as invalid.

use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_syntax::SmolStr;
use elp_types_db::eqwalizer::form::Callback;
use elp_types_db::eqwalizer::form::FunSpec;
use elp_types_db::eqwalizer::form::InvalidForm;
use elp_types_db::eqwalizer::form::InvalidFunSpec;
use elp_types_db::eqwalizer::form::InvalidRecDecl;
use elp_types_db::eqwalizer::form::InvalidTypeDecl;
use elp_types_db::eqwalizer::form::OverloadedFunSpec;
use elp_types_db::eqwalizer::form::RecDecl;
use elp_types_db::eqwalizer::form::TypeDecl;
use elp_types_db::eqwalizer::invalid_diagnostics::Invalid;
use elp_types_db::eqwalizer::invalid_diagnostics::TransitiveInvalid;
use elp_types_db::eqwalizer::types::Type;
use fxhash::FxHashMap;
use fxhash::FxHashSet;

use super::db::EqwalizerASTDatabase;
use super::stub::ModuleStub;
use super::Id;
use super::RemoteId;
use super::TransitiveCheckError;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Ref {
    RidRef(RemoteId),
    RecRef(SmolStr, SmolStr),
}

impl Ref {
    fn module(&self) -> &SmolStr {
        match self {
            Ref::RidRef(rid) => &rid.module,
            Ref::RecRef(module, _) => module,
        }
    }
}

pub struct TransitiveChecker<'d> {
    db: &'d dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: SmolStr,
    in_progress: FxHashSet<Ref>,
    invalid_refs: FxHashMap<Ref, FxHashSet<Ref>>,
    maybe_invalid_refs: FxHashMap<Ref, FxHashSet<Ref>>,
}

impl TransitiveChecker<'_> {
    pub fn new(
        db: &dyn EqwalizerASTDatabase,
        project_id: ProjectId,
        module: SmolStr,
    ) -> TransitiveChecker<'_> {
        TransitiveChecker {
            db,
            project_id,
            module,
            in_progress: FxHashSet::default(),
            invalid_refs: FxHashMap::default(),
            maybe_invalid_refs: FxHashMap::default(),
        }
    }

    fn show_invalids(&mut self, rref: &Ref) -> Vec<SmolStr> {
        self.invalid_refs
            .get(rref)
            .unwrap()
            .iter()
            .map(|inv| self.show(inv))
            .collect()
    }

    fn check_type_decl(
        &mut self,
        stub: &mut ModuleStub,
        t: &TypeDecl,
    ) -> Result<(), TransitiveCheckError> {
        let rref = Ref::RidRef(RemoteId {
            module: self.module.clone(),
            name: t.id.name.clone(),
            arity: t.id.arity,
        });
        if !self.is_valid(&rref)? {
            let invalids = self.show_invalids(&rref);
            let diag = Invalid::TransitiveInvalid(TransitiveInvalid {
                location: t.location.clone(),
                name: t.id.to_string().into(),
                references: invalids,
            });
            stub.types.remove(&t.id);
            stub.invalid_forms
                .push(InvalidForm::InvalidTypeDecl(InvalidTypeDecl {
                    location: t.location.clone(),
                    id: t.id.clone(),
                    te: diag,
                }))
        }
        Ok(())
    }

    fn check_private_opaque_decl(
        &mut self,
        stub: &mut ModuleStub,
        t: &TypeDecl,
    ) -> Result<(), TransitiveCheckError> {
        let rref = Ref::RidRef(RemoteId {
            module: self.module.clone(),
            name: t.id.name.clone(),
            arity: t.id.arity,
        });
        if !self.is_valid(&rref)? {
            let invalids = self.show_invalids(&rref);
            let diag = Invalid::TransitiveInvalid(TransitiveInvalid {
                location: t.location.clone(),
                name: t.id.to_string().into(),
                references: invalids,
            });
            stub.opaques.remove(&t.id);
            stub.invalid_forms
                .push(InvalidForm::InvalidTypeDecl(InvalidTypeDecl {
                    location: t.location.clone(),
                    id: t.id.clone(),
                    te: diag,
                }))
        }
        Ok(())
    }

    fn check_spec(
        &mut self,
        stub: &mut ModuleStub,
        spec: &FunSpec,
    ) -> Result<(), TransitiveCheckError> {
        let mut invalids = FxHashSet::default();
        self.collect_invalid_references(
            &mut invalids,
            &self.module.clone(),
            &Type::FunType(spec.ty.to_owned()),
            None,
        )?;
        if !invalids.is_empty() {
            let references = invalids.iter().map(|rref| self.show(rref)).collect();
            let diag = Invalid::TransitiveInvalid(TransitiveInvalid {
                location: spec.location.clone(),
                name: spec.id.to_string().into(),
                references,
            });
            stub.specs.remove(&spec.id);
            stub.invalid_forms
                .push(InvalidForm::InvalidFunSpec(InvalidFunSpec {
                    location: spec.location.clone(),
                    id: spec.id.clone(),
                    te: diag,
                }))
        }
        Ok(())
    }

    fn check_record_decl(
        &mut self,
        stub: &mut ModuleStub,
        t: &RecDecl,
    ) -> Result<(), TransitiveCheckError> {
        let rref = Ref::RecRef(self.module.clone(), t.name.clone());
        if !self.is_valid(&rref)? {
            let invalids = self.show_invalids(&rref);
            let diag = Invalid::TransitiveInvalid(TransitiveInvalid {
                location: t.location.clone(),
                name: t.name.clone(),
                references: invalids,
            });
            // we don't know at this point which fields are invalid,
            // so replacing all the fields with dynamic type
            stub.records.get_mut(&t.name).map(|rec_decl| {
                rec_decl.fields.iter_mut().for_each(|field| {
                    if field.tp.is_some() {
                        field.tp = Some(Type::DynamicType)
                    }
                })
            });
            stub.invalid_forms
                .push(InvalidForm::InvalidRecDecl(InvalidRecDecl {
                    location: t.location.clone(),
                    name: t.name.clone(),
                    te: diag,
                }))
        }
        Ok(())
    }

    fn check_overloaded_spec(
        &mut self,
        stub: &mut ModuleStub,
        spec: &OverloadedFunSpec,
    ) -> Result<(), TransitiveCheckError> {
        let mut invalids = FxHashSet::default();
        for ty in spec.tys.iter() {
            self.collect_invalid_references(
                &mut invalids,
                &self.module.clone(),
                &Type::FunType(ty.to_owned()),
                None,
            )?;
        }
        if !invalids.is_empty() {
            let references = invalids.iter().map(|rref| self.show(rref)).collect();
            let diag = Invalid::TransitiveInvalid(TransitiveInvalid {
                location: spec.location.clone(),
                name: spec.id.to_string().into(),
                references,
            });
            stub.overloaded_specs.remove(&spec.id);
            stub.invalid_forms
                .push(InvalidForm::InvalidFunSpec(InvalidFunSpec {
                    location: spec.location.clone(),
                    id: spec.id.clone(),
                    te: diag,
                }))
        }
        Ok(())
    }

    fn check_callback(
        &mut self,
        stub: &mut ModuleStub,
        cb: &Callback,
    ) -> Result<(), TransitiveCheckError> {
        let mut filtered_tys = vec![];
        for ty in cb.tys.iter() {
            let mut invalids = FxHashSet::default();
            self.collect_invalid_references(
                &mut invalids,
                &self.module.clone(),
                &Type::FunType(ty.to_owned()),
                None,
            )?;
            if invalids.is_empty() {
                filtered_tys.push(ty.clone())
            }
        }
        let new_cb = Callback {
            location: cb.location.clone(),
            id: cb.id.clone(),
            tys: filtered_tys,
        };
        stub.callbacks.push(new_cb);
        Ok(())
    }

    fn is_valid(&mut self, rref: &Ref) -> Result<bool, TransitiveCheckError> {
        let maybe_valid = self.is_maybe_valid(rref, None)?;
        let mut resolved_invalids = FxHashSet::default();
        if let Some(maybe_invalids) = self.maybe_invalid_refs.remove(rref) {
            for maybe_invalid in maybe_invalids.iter() {
                if !self.is_valid(maybe_invalid)? {
                    resolved_invalids.insert(maybe_invalid.clone());
                }
            }
        }
        let has_no_resolved_invalids = resolved_invalids.is_empty();
        self.invalid_refs
            .entry(rref.clone())
            .or_default()
            .extend(resolved_invalids);
        Ok(maybe_valid && has_no_resolved_invalids)
    }

    fn is_maybe_valid(
        &mut self,
        rref: &Ref,
        parent_ref: Option<&Ref>,
    ) -> Result<bool, TransitiveCheckError> {
        if self.in_progress.contains(rref) {
            if let Some(pref) = parent_ref {
                self.maybe_invalid_refs
                    .entry(pref.clone())
                    .or_default()
                    .insert(rref.clone());
            }
            return Ok(true);
        }
        if let Some(invs) = self.invalid_refs.get(rref) {
            return Ok(invs.is_empty());
        }
        self.in_progress.insert(rref.clone());
        let mut invalids = FxHashSet::default();
        match self
            .db
            .covariant_stub(self.project_id, ModuleName::new(rref.module().as_str()))
        {
            Ok(stub) => match rref {
                Ref::RidRef(rid) => {
                    let id = Id {
                        name: rid.name.clone(),
                        arity: rid.arity,
                    };
                    match stub.types.get(&id) {
                        Some(tdecl) => self.collect_invalid_references(
                            &mut invalids,
                            &rid.module,
                            &tdecl.body,
                            Some(rref),
                        )?,
                        None => match stub.opaques.get(&id) {
                            Some(tdecl) => self.collect_invalid_references(
                                &mut invalids,
                                &rid.module,
                                &tdecl.body,
                                Some(rref),
                            )?,
                            None => {
                                invalids.insert(rref.clone());
                            }
                        },
                    }
                }
                Ref::RecRef(module, rec_name) => match stub.records.get(rec_name) {
                    Some(rdecl) => {
                        for field in rdecl.fields.iter() {
                            if let Some(ty) = &field.tp {
                                self.collect_invalid_references(
                                    &mut invalids,
                                    module,
                                    ty,
                                    Some(rref),
                                )?;
                            }
                        }
                    }
                    None => {
                        invalids.insert(rref.clone());
                    }
                },
            },
            Err(_) => {
                invalids.insert(rref.clone());
            }
        };
        let no_invalids = invalids.is_empty();
        self.in_progress.remove(rref);
        self.invalid_refs.insert(rref.clone(), invalids);
        Ok(no_invalids)
    }

    fn collect_invalid_references(
        &mut self,
        refs: &mut FxHashSet<Ref>,
        module: &SmolStr,
        ty: &Type,
        parent_ref: Option<&Ref>,
    ) -> Result<(), TransitiveCheckError> {
        match ty {
            Type::RemoteType(rt) => {
                for arg in rt.arg_tys.iter() {
                    self.collect_invalid_references(refs, module, arg, parent_ref)?;
                }
                let rref = Ref::RidRef(rt.id.clone());
                if !self.is_maybe_valid(&rref, parent_ref)? {
                    refs.insert(rref);
                }
            }
            Type::OpaqueType(_) => {
                return Err(TransitiveCheckError::UnexpectedOpaqueType);
            }
            Type::RecordType(rt) => {
                let rref = Ref::RecRef(module.clone(), rt.name.clone());
                if !self.is_maybe_valid(&rref, parent_ref)? {
                    refs.insert(rref);
                }
            }
            Type::RefinedRecordType(rt) => {
                let rref = Ref::RecRef(module.clone(), rt.rec_type.name.clone());
                for (_, ty) in rt.fields.iter() {
                    self.collect_invalid_references(refs, module, ty, parent_ref)?;
                }
                if !self.is_maybe_valid(&rref, parent_ref)? {
                    refs.insert(rref);
                }
            }
            ty => {
                ty.walk(&mut |ty| self.collect_invalid_references(refs, module, ty, parent_ref))?
            }
        }
        Ok(())
    }

    fn show(&self, rref: &Ref) -> SmolStr {
        match rref {
            Ref::RidRef(rid) if rid.module == self.module => Id {
                name: rid.name.clone(),
                arity: rid.arity,
            }
            .to_string()
            .into(),
            Ref::RidRef(rid) => rid.to_string().into(),
            Ref::RecRef(_, name) => format!("#{}{{}}", name).into(),
        }
    }

    pub fn check(&mut self, stub: &ModuleStub) -> Result<ModuleStub, TransitiveCheckError> {
        let mut stub_result = stub.clone();
        stub_result.callbacks = vec![];
        stub.types
            .values()
            .map(|decl| self.check_type_decl(&mut stub_result, decl))
            .collect::<Result<Vec<()>, _>>()?;
        stub.opaques
            .values()
            .map(|decl| self.check_private_opaque_decl(&mut stub_result, decl))
            .collect::<Result<Vec<()>, _>>()?;
        stub.records
            .values()
            .map(|decl| self.check_record_decl(&mut stub_result, decl))
            .collect::<Result<Vec<()>, _>>()?;
        stub.specs
            .values()
            .map(|spec| self.check_spec(&mut stub_result, spec))
            .collect::<Result<Vec<()>, _>>()?;
        stub.overloaded_specs
            .values()
            .map(|spec| self.check_overloaded_spec(&mut stub_result, spec))
            .collect::<Result<Vec<()>, _>>()?;
        stub.callbacks
            .iter()
            .map(|cb| self.check_callback(&mut stub_result, cb))
            .collect::<Result<Vec<()>, _>>()?;
        Ok(stub_result)
    }
}

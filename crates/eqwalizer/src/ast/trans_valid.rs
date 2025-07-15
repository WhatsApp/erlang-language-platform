/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! This module performs the fourth and last step of stubs validation
//!
//! It ensures that declarations are transitively valid by propagating
//! all invalid declarations. I.e., if a type t1 depends on a type t2
//! and t2 is invalid, then t1 will be tagged as invalid.

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;

use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_syntax::SmolStr;
use elp_types_db::StringId;
use elp_types_db::eqwalizer::Pos;
use elp_types_db::eqwalizer::form::Callback;
use elp_types_db::eqwalizer::form::FunSpec;
use elp_types_db::eqwalizer::form::OverloadedFunSpec;
use elp_types_db::eqwalizer::form::RecDecl;
use elp_types_db::eqwalizer::form::TypeDecl;
use elp_types_db::eqwalizer::invalid_diagnostics::Invalid;
use elp_types_db::eqwalizer::invalid_diagnostics::InvalidRefInTypeCast;
use elp_types_db::eqwalizer::invalid_diagnostics::TransitiveInvalid;
use elp_types_db::eqwalizer::types::Type;

use super::Id;
use super::RemoteId;
use super::stub::ModuleStub;
use super::stub::VStub;
use crate::db::EqwalizerDiagnosticsDatabase;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Ref {
    RidRef(RemoteId),
    RecRef(StringId, StringId),
}

impl Ref {
    fn module(&self) -> StringId {
        match self {
            Ref::RidRef(rid) => rid.module,
            Ref::RecRef(module, _) => *module,
        }
    }
}

pub struct TransitiveChecker<'d> {
    db: &'d dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    module: StringId,
    in_progress: BTreeSet<Ref>,
    invalid_refs: BTreeMap<Ref, BTreeSet<Ref>>,
    maybe_invalid_refs: BTreeMap<Ref, BTreeSet<Ref>>,
}

impl TransitiveChecker<'_> {
    pub fn new(
        db: &dyn EqwalizerDiagnosticsDatabase,
        project_id: ProjectId,
        module: StringId,
    ) -> TransitiveChecker<'_> {
        TransitiveChecker {
            db,
            project_id,
            module,
            in_progress: Default::default(),
            invalid_refs: Default::default(),
            maybe_invalid_refs: Default::default(),
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

    fn check_type_decl(&mut self, stub: &mut ModuleStub, t: &TypeDecl) {
        let rref = Ref::RidRef(RemoteId {
            module: self.module,
            name: t.id.name,
            arity: t.id.arity,
        });
        if !self.is_valid(&rref) {
            let invalids = self.show_invalids(&rref);
            let diag = Invalid::TransitiveInvalid(TransitiveInvalid::new(
                t.pos.clone(),
                t.id.to_string().into(),
                invalids,
            ));
            stub.types.remove(&t.id);
            stub.invalids.push(diag);
        }
    }

    fn check_spec(&mut self, stub: &mut ModuleStub, spec: &FunSpec) {
        let mut invalids = Default::default();
        self.collect_invalid_references(
            &mut invalids,
            self.module,
            &Type::FunType(spec.ty.to_owned()),
            None,
        );
        if !invalids.is_empty() {
            let references = invalids.iter().map(|rref| self.show(rref)).collect();
            let diag = Invalid::TransitiveInvalid(TransitiveInvalid::new(
                spec.pos.clone(),
                spec.id.to_string().into(),
                references,
            ));
            stub.specs.remove(&spec.id);
            stub.invalids.push(diag);
        }
    }

    fn check_record_decl(&mut self, stub: &mut ModuleStub, t: &RecDecl) {
        let rref = Ref::RecRef(self.module, t.name);
        if !self.is_valid(&rref) {
            let invalids = self.show_invalids(&rref);
            let diag = Invalid::TransitiveInvalid(TransitiveInvalid::new(
                t.pos.clone(),
                t.name.into(),
                invalids,
            ));
            // we don't know at this point which fields are invalid,
            // so replacing all the fields with dynamic type
            if let Some(rec_decl) = stub.records.get_mut(&t.name) {
                let rec_decl_mut = Arc::make_mut(rec_decl);
                rec_decl_mut
                    .fields
                    .iter_mut()
                    .for_each(|field| field.tp = Type::DynamicType)
            };
            stub.invalids.push(diag);
        }
    }

    fn check_overloaded_spec(&mut self, stub: &mut ModuleStub, spec: &OverloadedFunSpec) {
        let mut invalids = Default::default();
        for ty in spec.tys.iter() {
            self.collect_invalid_references(
                &mut invalids,
                self.module,
                &Type::FunType(ty.to_owned()),
                None,
            );
        }
        if !invalids.is_empty() {
            let references = invalids.iter().map(|rref| self.show(rref)).collect();
            let diag = Invalid::TransitiveInvalid(TransitiveInvalid::new(
                spec.pos.clone(),
                spec.id.to_string().into(),
                references,
            ));
            stub.overloaded_specs.remove(&spec.id);
            stub.invalids.push(diag);
        }
    }

    fn normalize_callback(&mut self, cb: Callback) -> Callback {
        let mut filtered_tys = vec![];
        for ty in cb.tys.into_iter() {
            let mut invalids = Default::default();
            self.collect_invalid_references(
                &mut invalids,
                self.module,
                &Type::FunType(ty.clone()),
                None,
            );
            if invalids.is_empty() {
                filtered_tys.push(ty)
            }
        }

        Callback {
            pos: cb.pos,
            id: cb.id,
            tys: filtered_tys,
        }
    }

    fn is_valid(&mut self, rref: &Ref) -> bool {
        let maybe_valid = self.is_maybe_valid(rref, None);
        let mut resolved_invalids = BTreeSet::default();
        if let Some(maybe_invalids) = self.maybe_invalid_refs.remove(rref) {
            for maybe_invalid in maybe_invalids.iter() {
                if !self.is_valid(maybe_invalid) {
                    resolved_invalids.insert(maybe_invalid.clone());
                }
            }
        }
        let has_no_resolved_invalids = resolved_invalids.is_empty();
        self.invalid_refs
            .entry(rref.clone())
            .or_default()
            .extend(resolved_invalids);
        maybe_valid && has_no_resolved_invalids
    }

    fn is_maybe_valid(&mut self, rref: &Ref, parent_ref: Option<&Ref>) -> bool {
        if self.in_progress.contains(rref) {
            if let Some(pref) = parent_ref {
                self.maybe_invalid_refs
                    .entry(pref.clone())
                    .or_default()
                    .insert(rref.clone());
            }
            return true;
        }
        if let Some(invs) = self.invalid_refs.get(rref) {
            return invs.is_empty();
        }
        self.in_progress.insert(rref.clone());
        let mut invalids = Default::default();
        match self
            .db
            .contractive_stub(self.project_id, ModuleName::new(rref.module().as_str()))
        {
            Ok(v_stub) => match rref {
                Ref::RidRef(rid) => {
                    let id = Id {
                        name: rid.name,
                        arity: rid.arity,
                    };
                    match v_stub.get_type(&id) {
                        Some(tdecl) => self.collect_invalid_references(
                            &mut invalids,
                            rid.module,
                            &tdecl.body,
                            Some(rref),
                        ),
                        None => {
                            let _ = invalids.insert(rref.clone());
                        }
                    }
                }
                Ref::RecRef(module, rec_name) => match v_stub.get_record(*rec_name) {
                    Some(rdecl) => {
                        for field in rdecl.fields.iter() {
                            self.collect_invalid_references(
                                &mut invalids,
                                *module,
                                &field.tp,
                                Some(rref),
                            );
                        }
                    }
                    None => {
                        invalids.insert(rref.clone());
                    }
                },
            },
            _ => {
                invalids.insert(rref.clone());
            }
        };
        let no_invalids = invalids.is_empty();
        self.in_progress.remove(rref);
        self.invalid_refs.insert(rref.clone(), invalids);
        no_invalids
    }

    fn collect_invalid_references(
        &mut self,
        invalids: &mut BTreeSet<Ref>,
        module: StringId,
        ty: &Type,
        parent_ref: Option<&Ref>,
    ) {
        match ty {
            Type::RemoteType(rt) => {
                for arg in rt.arg_tys.iter() {
                    self.collect_invalid_references(invalids, module, arg, parent_ref);
                }
                let rref = Ref::RidRef(rt.id.clone());
                if !self.is_maybe_valid(&rref, parent_ref) {
                    invalids.insert(rref);
                }
            }
            Type::RecordType(rt) => {
                let rref = Ref::RecRef(module, rt.name);
                if !self.is_maybe_valid(&rref, parent_ref) {
                    invalids.insert(rref);
                }
            }
            Type::RefinedRecordType(rt) => {
                let rref = Ref::RecRef(module, rt.rec_type.name);
                for (_, ty) in rt.fields.iter() {
                    self.collect_invalid_references(invalids, module, ty, parent_ref);
                }
                if !self.is_maybe_valid(&rref, parent_ref) {
                    invalids.insert(rref);
                }
            }
            ty => {
                let _ = ty.walk::<()>(&mut |ty| {
                    self.collect_invalid_references(invalids, module, ty, parent_ref);
                    Ok(())
                });
            }
        }
    }

    fn show(&self, rref: &Ref) -> SmolStr {
        match rref {
            Ref::RidRef(rid) if rid.module == self.module => Id {
                name: rid.name,
                arity: rid.arity,
            }
            .to_string()
            .into(),
            Ref::RidRef(rid) => rid.to_string().into(),
            Ref::RecRef(_, name) => format!("#{name}{{}}").into(),
        }
    }

    pub fn check(&mut self, v_stub: &VStub) -> ModuleStub {
        let mut stub_result = v_stub.into_normalized_stub();

        for decl in v_stub.types() {
            self.check_type_decl(&mut stub_result, decl)
        }
        for decl in v_stub.records() {
            self.check_record_decl(&mut stub_result, decl)
        }
        for spec in v_stub.specs() {
            self.check_spec(&mut stub_result, spec)
        }
        for spec in v_stub.overloaded_specs() {
            self.check_overloaded_spec(&mut stub_result, spec)
        }

        let callbacks = (*stub_result.callbacks)
            .clone()
            .into_iter()
            .map(|cb| self.normalize_callback(cb))
            .collect();

        stub_result.callbacks = Arc::new(callbacks);
        stub_result
    }

    pub fn check_type(&mut self, pos: Pos, ty: Type) -> Result<Type, Invalid> {
        let mut invalids = Default::default();
        self.collect_invalid_references(&mut invalids, self.module, &ty, None);
        if !invalids.is_empty() {
            let references = invalids.iter().map(|rref| self.show(rref)).collect();
            let diag = Invalid::InvalidRefInTypeCast(InvalidRefInTypeCast::new(pos, references));
            return Err(diag);
        }
        Ok(ty)
    }
}

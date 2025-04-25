/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This module performs the second step of stubs validation
//!
//! It ensures that stubs are contractive, i.e., that the recursive type
//! aliases they define are all productive.
//! Broadly speaking, a recursive type is productive if all its infinite
//! expansions contain at least a type constructor (function type, tuple type,
//! etc. -- see function `is_producer` below) and produce a finite number
//! of distinct sub-terms.
//!
//! This property ensures that we can exhaustively and recursively compute
//! properties on recursive types in finite time, by stopping whenever we
//! visit an already-seen type.
//!
//! The main logic is function `is_contractive`, which checks whether a type
//! satisfies these properties, by recursively traversing it while keeping
//! the history of aliases seen so far.
//! Whenever we encounter a type alias that is already in the history, and
//! as long as we traversed a type constructor in the meantime, we know this
//! expansion path is contractive.
//!
//! Failure happens when we encounter a type alias that is not in the history,
//! but such that its expansion contains an embedding of an already-seen type.
//! Informally, this means that every expansion introduces a new, larger
//! sub-term. The function `is_he` (where "he" stands for "homeomorphic embedding")
//! below is responsible for checking whether a type embeds into another,
//! which can be done by "coupling" (the two types have the same top constructor)
//! or by "diving" (the first type embeds into one of the second's sub terms).
//!
//! Algorithm design by @ilyaklyuchnikov, see D30779530 for more information.

use std::collections::BTreeMap;
use std::iter;
use std::sync::Arc;

use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_types_db::StringId;
use elp_types_db::eqwalizer::form::TypeDecl;
use elp_types_db::eqwalizer::invalid_diagnostics::Invalid;
use elp_types_db::eqwalizer::invalid_diagnostics::NonProductiveRecursiveTypeAlias;
use elp_types_db::eqwalizer::types::Key;
use elp_types_db::eqwalizer::types::OpaqueType;
use elp_types_db::eqwalizer::types::Prop;
use elp_types_db::eqwalizer::types::RemoteType;
use elp_types_db::eqwalizer::types::Type;
use fxhash::FxHashMap;
use fxhash::FxHashSet;

use super::ContractivityCheckError;
use super::Id;
use super::RemoteId;
use super::stub::ModuleStub;
use super::stub::VStub;
use super::subst::Subst;
use crate::db::EqwalizerDiagnosticsDatabase;

fn is_he(s: &Type, t: &Type) -> Result<bool, ContractivityCheckError> {
    Ok(he_by_diving(s, t)? || he_by_coupling(s, t)?)
}

fn any_he<'a, I>(s: &Type, t: I) -> Result<bool, ContractivityCheckError>
where
    I: IntoIterator<Item = &'a Type>,
{
    for ty in t {
        if is_he(s, ty)? {
            return Ok(true);
        }
    }
    Ok(false)
}

fn all_he(s: &[Type], t: &[Type]) -> Result<bool, ContractivityCheckError> {
    for (ty1, ty2) in s.iter().zip(t) {
        if !is_he(ty1, ty2)? {
            return Ok(false);
        }
    }
    Ok(true)
}

fn he_by_diving(s: &Type, t: &Type) -> Result<bool, ContractivityCheckError> {
    match t {
        Type::FunType(ft) if !ft.forall.is_empty() => Err(ContractivityCheckError::NonEmptyForall),
        Type::FunType(ft) => Ok(any_he(s, cons(&*ft.res_ty, &ft.arg_tys))?),
        Type::AnyArityFunType(ft) => is_he(s, &ft.res_ty),
        Type::TupleType(tt) => any_he(s, &tt.arg_tys),
        Type::ListType(lt) => is_he(s, &lt.t),
        Type::UnionType(ut) => any_he(s, &ut.tys),
        Type::RemoteType(rt) => any_he(s, &rt.arg_tys),
        Type::OpaqueType(ot) => any_he(s, &ot.arg_tys),
        Type::MapType(m) => {
            let tys = m.props.values().map(|p| &p.tp);
            any_he(s, tys)?;
            Ok(is_he(s, &m.k_type)? || is_he(s, &m.v_type)?)
        }
        Type::RefinedRecordType(rt) => any_he(s, rt.fields.values()),
        _ => Ok(false),
    }
}

fn he_by_coupling(s: &Type, t: &Type) -> Result<bool, ContractivityCheckError> {
    match (s, t) {
        (Type::TupleType(tt1), Type::TupleType(tt2)) if tt1.arg_tys.len() == tt2.arg_tys.len() => {
            all_he(&tt1.arg_tys, &tt2.arg_tys)
        }
        (Type::TupleType(_), _) => Ok(false),
        (Type::FunType(ft1), Type::FunType(ft2)) if ft1.arg_tys.len() == ft2.arg_tys.len() => {
            if !ft1.forall.is_empty() || !ft2.forall.is_empty() {
                return Err(ContractivityCheckError::NonEmptyForall);
            }
            Ok(is_he(&ft1.res_ty, &ft2.res_ty)? && all_he(&ft1.arg_tys, &ft2.arg_tys)?)
        }
        (Type::FunType(_), _) => Ok(false),
        (Type::AnyArityFunType(ft1), Type::AnyArityFunType(ft2)) => is_he(&ft1.res_ty, &ft2.res_ty),
        (Type::ListType(lt1), Type::ListType(lt2)) => is_he(&lt1.t, &lt2.t),
        (Type::ListType(_), _) => Ok(false),
        (Type::UnionType(ut1), Type::UnionType(ut2)) if ut1.tys.len() == ut2.tys.len() => {
            all_he(&ut1.tys, &ut2.tys)
        }
        (Type::UnionType(_), _) => Ok(false),
        (Type::RemoteType(rt1), Type::RemoteType(rt2)) if rt1.id == rt2.id => {
            all_he(&rt1.arg_tys, &rt2.arg_tys)
        }
        (Type::RemoteType(_), _) => Ok(false),
        (Type::OpaqueType(ot1), Type::OpaqueType(ot2)) if ot1.id == ot2.id => {
            all_he(&ot1.arg_tys, &ot2.arg_tys)
        }
        (Type::OpaqueType(_), _) => Ok(false),
        (Type::MapType(m1), Type::MapType(m2)) if m1.props.len() == m2.props.len() => {
            Ok(all_he_prop(&m1.props, &m2.props)?
                && is_he(&m1.k_type, &m2.k_type)?
                && is_he(&m1.v_type, &m2.v_type)?)
        }
        (Type::MapType(_), _) => Ok(false),
        (Type::RefinedRecordType(rt1), Type::RefinedRecordType(rt2))
            if rt1.rec_type == rt2.rec_type =>
        {
            // Tree maps are sorted, so keys will always be in the same order
            if !rt1
                .fields
                .keys()
                .zip(rt2.fields.keys())
                .all(|(f1, f2)| f1 == f2)
            {
                return Ok(false);
            }
            for (key, field1) in &rt1.fields {
                if !is_he(field1, &rt2.fields[key])? {
                    return Ok(false);
                }
            }
            Ok(true)
        }
        (Type::RefinedRecordType(_), _) => Ok(false),
        (Type::VarType(vt1), Type::VarType(vt2)) if vt1 == vt2 => Ok(true),
        (Type::VarType(_), _) => Ok(false),
        (s, t) => Ok(s == t),
    }
}

fn all_he_prop(
    s: &BTreeMap<Key, Prop>,
    t: &BTreeMap<Key, Prop>,
) -> Result<bool, ContractivityCheckError> {
    for ((k1, p1), (k2, p2)) in s.iter().zip(t.iter()) {
        if (k1 != k2) || !he_prop(p1, p2)? {
            return Ok(false);
        }
    }
    Ok(true)
}

fn he_prop(s: &Prop, t: &Prop) -> Result<bool, ContractivityCheckError> {
    Ok(s.req == t.req && is_he(&s.tp, &t.tp)?)
}

pub struct StubContractivityChecker<'d> {
    db: &'d dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
    module: StringId,
    cache: FxHashSet<RemoteId>,
    history: Vec<RemoteType>,
    productive: Vec<RemoteType>,
}

impl StubContractivityChecker<'_> {
    pub fn new(
        db: &dyn EqwalizerDiagnosticsDatabase,
        project_id: ProjectId,
        module: StringId,
    ) -> StubContractivityChecker<'_> {
        StubContractivityChecker {
            db,
            project_id,
            module,
            cache: FxHashSet::default(),
            history: vec![],
            productive: vec![],
        }
    }

    fn check_decl(
        &mut self,
        stub: &mut VStub,
        t: &TypeDecl,
    ) -> Result<(), ContractivityCheckError> {
        let id = t.id.clone().into_remote(self.module);
        let rty = RemoteType {
            id: id.clone(),
            arg_tys: t.params.iter().cloned().map(Type::VarType).collect(),
        };
        assert!(self.history.is_empty());
        assert!(self.productive.is_empty());
        if self.is_contractive(Type::RemoteType(rty))? {
            self.cache.insert(id);
        } else {
            stub.invalid_ids.insert(t.id.clone());
            stub.invalids.push(self.to_invalid(t));
        }
        Ok(())
    }

    fn with_productive_history<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let history = std::mem::take(&mut self.history);
        let old_productive = self.productive.len();
        self.productive.extend(history);
        let res = f(self);
        let restored = self.productive.drain(old_productive..);
        self.history.extend(restored);
        res
    }

    fn with_history<R>(&mut self, ty: RemoteType, f: impl FnOnce(&mut Self) -> R) -> R {
        self.history.push(ty);
        let res = f(self);
        self.history.pop();
        res
    }

    fn to_invalid(&self, t: &TypeDecl) -> Invalid {
        Invalid::NonProductiveRecursiveTypeAlias(NonProductiveRecursiveTypeAlias {
            pos: t.pos.clone(),
            name: t.id.to_string().into(),
        })
    }

    fn is_contractive(&mut self, ty: Type) -> Result<bool, ContractivityCheckError> {
        match ty {
            Type::FunType(ft) => {
                if !ft.forall.is_empty() {
                    return Err(ContractivityCheckError::NonEmptyForall);
                }
                self.with_productive_history(|this| {
                    Ok(this.all_contractive(cons(*ft.res_ty, ft.arg_tys))?)
                })
            }
            Type::AnyArityFunType(ft) => {
                self.with_productive_history(|this| Ok(this.is_contractive(*ft.res_ty)?))
            }
            Type::TupleType(tt) => {
                self.with_productive_history(|this| Ok(this.all_contractive(tt.arg_tys)?))
            }
            Type::ListType(lt) => {
                self.with_productive_history(|this| Ok(this.is_contractive(*lt.t)?))
            }
            Type::UnionType(ut) => Ok(self.all_contractive(ut.tys)?),
            Type::OpaqueType(ot) => {
                self.with_productive_history(|this| Ok(this.all_contractive(ot.arg_tys)?))
            }
            Type::MapType(mt) => self.with_productive_history(|this| {
                let prop = mt.props.into_values().map(|prop| prop.tp);
                Ok(this.all_contractive(cons(*mt.k_type, cons(*mt.v_type, prop)))?)
            }),
            Type::RefinedRecordType(rt) => self
                .with_productive_history(|this| Ok(this.all_contractive(rt.fields.into_values())?)),
            Type::RemoteType(rt) => {
                if !rt.arg_tys.is_empty() {
                    if self.productive.contains(&rt) {
                        return Ok(true);
                    }
                    for t in self.history.iter().chain(&self.productive) {
                        if t.id == rt.id && all_he(&t.arg_tys, &rt.arg_tys)? {
                            return Ok(false);
                        }
                    }
                    match self.type_decl_body(&rt.id, &rt.arg_tys)? {
                        Some(typ) => {
                            self.with_history(rt.clone(), |this| Ok(this.is_contractive(typ)?))
                        }
                        None => Ok(true),
                    }
                } else {
                    // We optimise remote types with no arguments by caching them.
                    // It's enough to check them once, they'll always have the same
                    // result.
                    // We only cache positive result, negative result will become an error
                    // and the performance of this case is not particularly important.
                    if self.cache.contains(&rt.id) {
                        return Ok(true);
                    }
                    if self.productive.contains(&rt) {
                        self.cache.insert(rt.id.clone());
                        return Ok(true);
                    }
                    // The check above checking homomorphic embedding, in case of no arguments,
                    // boils down to just checking if we've already seen the type.
                    // We've also already checked the productive aliases, so we just need to
                    // check the history
                    if self.history.contains(&rt) {
                        return Ok(false);
                    }
                    match self.type_decl_body(&rt.id, &rt.arg_tys)? {
                        Some(typ) => {
                            let id = rt.id.clone();
                            self.with_history(rt, |this| {
                                if this.is_contractive(typ)? {
                                    this.cache.insert(id);
                                    Ok(true)
                                } else {
                                    Ok(false)
                                }
                            })
                        }
                        None => Ok(true),
                    }
                }
            }
            _ => Ok(true),
        }
    }

    fn all_contractive(
        &mut self,
        tys: impl IntoIterator<Item = Type>,
    ) -> Result<bool, ContractivityCheckError> {
        for ty in tys {
            if !self.is_contractive(ty)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn type_decl_body(
        &self,
        id: &RemoteId,
        args: &[Type],
    ) -> Result<Option<Type>, ContractivityCheckError> {
        let local_id = Id {
            name: id.name,
            arity: id.arity,
        };
        let stub = self
            .db
            .expanded_stub(self.project_id, ModuleName::new(id.module.as_str()))
            .map_err(|err| ContractivityCheckError::ErrorExpandingID(id.clone(), Box::new(err)))?;
        fn subst(decl: &TypeDecl, args: &[Type]) -> Type {
            if decl.params.is_empty() {
                decl.body.clone()
            } else {
                let sub: FxHashMap<u32, &Type> =
                    decl.params.iter().map(|v| v.n).zip(args).collect();
                Subst { sub }.apply(decl.body.clone())
            }
        }
        Ok(stub
            .types
            .get(&local_id)
            .map(|t| subst(t, args))
            .or_else(|| {
                stub.opaques.get(&local_id).map(|t| {
                    if self.module == id.module {
                        subst(t, args)
                    } else {
                        Type::OpaqueType(OpaqueType {
                            id: id.clone(),
                            arg_tys: args.to_owned(),
                        })
                    }
                })
            }))
    }

    pub fn check(&mut self, stub: Arc<ModuleStub>) -> Result<VStub, ContractivityCheckError> {
        let mut v_stub = VStub::new(stub.clone());
        for decl in stub.types.values() {
            self.check_decl(&mut v_stub, decl)?
        }
        for decl in stub.opaques.values() {
            self.check_decl(&mut v_stub, decl)?
        }
        Ok(v_stub)
    }
}

fn cons<T>(h: T, t: impl IntoIterator<Item = T>) -> impl Iterator<Item = T> {
    iter::once(h).chain(t.into_iter())
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
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
use elp_types_db::eqwalizer::types::Prop;
use elp_types_db::eqwalizer::types::RemoteType;
use elp_types_db::eqwalizer::types::Type;
use fxhash::FxHashMap;
use fxhash::FxHashSet;

use super::Id;
use super::RemoteId;
use super::stub::ModuleStub;
use super::stub::VStub;
use super::subst::Subst;
use crate::db::EqwalizerDiagnosticsDatabase;

fn is_he(s: &Type, t: &Type) -> bool {
    he_by_diving(s, t) || he_by_coupling(s, t)
}

fn any_he<'a, I>(s: &Type, t: I) -> bool
where
    I: IntoIterator<Item = &'a Type>,
{
    t.into_iter().any(|ty| is_he(s, ty))
}

fn all_he(s: &[Type], t: &[Type]) -> bool {
    s.iter().zip(t).all(|(ty1, ty2)| is_he(ty1, ty2))
}

fn he_by_diving(s: &Type, t: &Type) -> bool {
    match t {
        Type::FunType(ft) => any_he(s, cons(&*ft.res_ty, &ft.arg_tys)),
        Type::AnyArityFunType(ft) => is_he(s, &ft.res_ty),
        Type::TupleType(tt) => any_he(s, &tt.arg_tys),
        Type::ListType(lt) => is_he(s, &lt.t),
        Type::UnionType(ut) => any_he(s, &ut.tys),
        Type::RemoteType(rt) => any_he(s, &rt.arg_tys),
        Type::MapType(m) => {
            let tys = m.props.values().map(|p| &p.tp);
            any_he(s, cons(&*m.k_type, cons(&*m.v_type, tys)))
        }
        Type::RefinedRecordType(rt) => any_he(s, rt.fields.values()),
        _ => false,
    }
}

fn he_by_coupling(s: &Type, t: &Type) -> bool {
    match (s, t) {
        (Type::TupleType(tt1), Type::TupleType(tt2)) if tt1.arg_tys.len() == tt2.arg_tys.len() => {
            all_he(&tt1.arg_tys, &tt2.arg_tys)
        }
        (Type::TupleType(_), _) => false,
        (Type::FunType(ft1), Type::FunType(ft2)) if ft1.arg_tys.len() == ft2.arg_tys.len() => {
            is_he(&ft1.res_ty, &ft2.res_ty) && all_he(&ft1.arg_tys, &ft2.arg_tys)
        }
        (Type::FunType(_), _) => false,
        (Type::AnyArityFunType(ft1), Type::AnyArityFunType(ft2)) => is_he(&ft1.res_ty, &ft2.res_ty),
        (Type::ListType(lt1), Type::ListType(lt2)) => is_he(&lt1.t, &lt2.t),
        (Type::ListType(_), _) => false,
        (Type::UnionType(ut1), Type::UnionType(ut2)) if ut1.tys.len() == ut2.tys.len() => {
            all_he(&ut1.tys, &ut2.tys)
        }
        (Type::UnionType(_), _) => false,
        (Type::RemoteType(rt1), Type::RemoteType(rt2)) if rt1.id == rt2.id => {
            all_he(&rt1.arg_tys, &rt2.arg_tys)
        }
        (Type::RemoteType(_), _) => false,
        (Type::MapType(m1), Type::MapType(m2)) if m1.props.len() == m2.props.len() => {
            all_he_prop(&m1.props, &m2.props)
                && is_he(&m1.k_type, &m2.k_type)
                && is_he(&m1.v_type, &m2.v_type)
        }
        (Type::MapType(_), _) => false,
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
                return false;
            }
            for (key, field1) in &rt1.fields {
                if !is_he(field1, &rt2.fields[key]) {
                    return false;
                }
            }
            true
        }
        (Type::RefinedRecordType(_), _) => false,
        (Type::VarType(vt1), Type::VarType(vt2)) if vt1 == vt2 => true,
        (Type::VarType(_), _) => false,
        (s, t) => s == t,
    }
}

fn all_he_prop(s: &BTreeMap<Key, Prop>, t: &BTreeMap<Key, Prop>) -> bool {
    for ((k1, p1), (k2, p2)) in s.iter().zip(t.iter()) {
        if (k1 != k2) || !he_prop(p1, p2) {
            return false;
        }
    }
    true
}

fn he_prop(s: &Prop, t: &Prop) -> bool {
    s.req == t.req && is_he(&s.tp, &t.tp)
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

    fn check_decl(&mut self, stub: &mut VStub, t: &TypeDecl) {
        let id = t.id.clone().into_remote(self.module);
        let rty = RemoteType {
            id: id.clone(),
            arg_tys: t.params.iter().cloned().map(Type::VarType).collect(),
        };
        assert!(self.history.is_empty());
        assert!(self.productive.is_empty());
        if self.is_contractive(Type::RemoteType(rty)) {
            self.cache.insert(id);
        } else {
            stub.invalid_ids.insert(t.id.clone());
            stub.invalids.push(self.to_invalid(t));
        }
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

    fn is_contractive(&mut self, ty: Type) -> bool {
        match ty {
            Type::FunType(ft) => self
                .with_productive_history(|this| this.all_contractive(cons(*ft.res_ty, ft.arg_tys))),
            Type::AnyArityFunType(ft) => {
                self.with_productive_history(|this| this.is_contractive(*ft.res_ty))
            }
            Type::TupleType(tt) => {
                self.with_productive_history(|this| this.all_contractive(tt.arg_tys))
            }
            Type::ListType(lt) => self.with_productive_history(|this| this.is_contractive(*lt.t)),
            Type::UnionType(ut) => self.all_contractive(ut.tys),
            Type::MapType(mt) => self.with_productive_history(|this| {
                let prop = mt.props.into_values().map(|prop| prop.tp);
                this.all_contractive(cons(*mt.k_type, cons(*mt.v_type, prop)))
            }),
            Type::RefinedRecordType(rt) => {
                self.with_productive_history(|this| this.all_contractive(rt.fields.into_values()))
            }
            Type::RemoteType(rt) => {
                if !rt.arg_tys.is_empty() {
                    if self.productive.contains(&rt) {
                        return true;
                    }
                    for t in self.history.iter().chain(&self.productive) {
                        if t.id == rt.id && all_he(&t.arg_tys, &rt.arg_tys) {
                            return false;
                        }
                    }
                    match self.type_decl_body(&rt.id, &rt.arg_tys) {
                        Some(typ) => self.with_history(rt.clone(), |this| this.is_contractive(typ)),
                        None => true,
                    }
                } else {
                    // We optimise remote types with no arguments by caching them.
                    // It's enough to check them once, they'll always have the same
                    // result.
                    // We only cache positive result, negative result will become an error
                    // and the performance of this case is not particularly important.
                    if self.cache.contains(&rt.id) {
                        return true;
                    }
                    if self.productive.contains(&rt) {
                        self.cache.insert(rt.id.clone());
                        return true;
                    }
                    // The check above checking homomorphic embedding, in case of no arguments,
                    // boils down to just checking if we've already seen the type.
                    // We've also already checked the productive aliases, so we just need to
                    // check the history
                    if self.history.contains(&rt) {
                        return false;
                    }
                    match self.type_decl_body(&rt.id, &rt.arg_tys) {
                        Some(typ) => {
                            let id = rt.id.clone();
                            self.with_history(rt, |this| {
                                if this.is_contractive(typ) {
                                    this.cache.insert(id);
                                    true
                                } else {
                                    false
                                }
                            })
                        }
                        None => true,
                    }
                }
            }
            _ => true,
        }
    }

    fn all_contractive(&mut self, tys: impl IntoIterator<Item = Type>) -> bool {
        for ty in tys {
            if !self.is_contractive(ty) {
                return false;
            }
        }
        true
    }

    fn type_decl_body(&self, id: &RemoteId, args: &[Type]) -> Option<Type> {
        let local_id = Id {
            name: id.name,
            arity: id.arity,
        };
        // the stub is guaranteed to be present because the current call site
        // is already expanded - meaning that all the types referencing from it exist
        let stub = self
            .db
            .expanded_stub(self.project_id, ModuleName::new(id.module.as_str()))
            .expect("the stub should exist, since expansion validation has already happened");
        fn subst(decl: &TypeDecl, args: &[Type]) -> Type {
            if decl.params.is_empty() {
                decl.body.clone()
            } else {
                let sub: FxHashMap<u32, &Type> =
                    decl.params.iter().map(|v| v.n).zip(args).collect();
                Subst { sub }.apply(decl.body.clone())
            }
        }
        stub.types.get(&local_id).map(|t| subst(t, args))
    }

    pub fn check(&mut self, stub: Arc<ModuleStub>) -> VStub {
        let mut v_stub = VStub::new(stub.clone());
        for decl in stub.types.values() {
            self.check_decl(&mut v_stub, decl)
        }
        v_stub
    }
}

fn cons<T>(h: T, t: impl IntoIterator<Item = T>) -> impl Iterator<Item = T> {
    iter::once(h).chain(t)
}

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
use itertools::Itertools;

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
    I: Iterator<Item = &'a Type>,
{
    for ty in t {
        if is_he(s, ty)? {
            return Ok(true);
        }
    }
    Ok(false)
}

fn all_he<'a, I>(s: I, t: I) -> Result<bool, ContractivityCheckError>
where
    I: Iterator<Item = &'a Type>,
{
    for (ty1, ty2) in s.zip(t) {
        if !is_he(ty1, ty2)? {
            return Ok(false);
        }
    }
    Ok(true)
}

fn he_by_diving(s: &Type, t: &Type) -> Result<bool, ContractivityCheckError> {
    match t {
        Type::FunType(ft) if !ft.forall.is_empty() => Err(ContractivityCheckError::NonEmptyForall),
        Type::FunType(ft) => Ok(is_he(s, &ft.res_ty)? || any_he(s, ft.arg_tys.iter())?),
        Type::AnyArityFunType(ft) => is_he(s, &ft.res_ty),
        Type::TupleType(tt) => any_he(s, tt.arg_tys.iter()),
        Type::ListType(lt) => is_he(s, &lt.t),
        Type::UnionType(ut) => any_he(s, ut.tys.iter()),
        Type::RemoteType(rt) => any_he(s, rt.arg_tys.iter()),
        Type::OpaqueType(ot) => any_he(s, ot.arg_tys.iter()),
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
            all_he(tt1.arg_tys.iter(), tt2.arg_tys.iter())
        }
        (Type::TupleType(_), _) => Ok(false),
        (Type::FunType(ft1), Type::FunType(ft2)) if ft1.arg_tys.len() == ft2.arg_tys.len() => {
            if !ft1.forall.is_empty() || !ft2.forall.is_empty() {
                return Err(ContractivityCheckError::NonEmptyForall);
            }
            Ok(is_he(&ft1.res_ty, &ft2.res_ty)? && all_he(ft1.arg_tys.iter(), ft2.arg_tys.iter())?)
        }
        (Type::FunType(_), _) => Ok(false),
        (Type::AnyArityFunType(ft1), Type::AnyArityFunType(ft2)) => is_he(&ft1.res_ty, &ft2.res_ty),
        (Type::ListType(lt1), Type::ListType(lt2)) => is_he(&lt1.t, &lt2.t),
        (Type::ListType(_), _) => Ok(false),
        (Type::UnionType(ut1), Type::UnionType(ut2)) if ut1.tys.len() == ut2.tys.len() => {
            all_he(ut1.tys.iter(), ut2.tys.iter())
        }
        (Type::UnionType(_), _) => Ok(false),
        (Type::RemoteType(rt1), Type::RemoteType(rt2)) if rt1.id == rt2.id => {
            all_he(rt1.arg_tys.iter(), rt2.arg_tys.iter())
        }
        (Type::RemoteType(_), _) => Ok(false),
        (Type::OpaqueType(ot1), Type::OpaqueType(ot2)) if ot1.id == ot2.id => {
            all_he(ot1.arg_tys.iter(), ot2.arg_tys.iter())
        }
        (Type::OpaqueType(_), _) => Ok(false),
        (Type::MapType(m1), Type::MapType(m2)) if m1.props.len() == m2.props.len() => {
            let props1 = m1.props.iter().sorted_by_key(|p| p.0);
            let props2 = m2.props.iter().sorted_by_key(|p| p.0);
            Ok(all_he_prop(props1, props2)?
                && is_he(&m1.k_type, &m2.k_type)?
                && is_he(&m1.v_type, &m2.v_type)?)
        }
        (Type::MapType(_), _) => Ok(false),
        (Type::RefinedRecordType(rt1), Type::RefinedRecordType(rt2))
            if rt1.rec_type == rt2.rec_type =>
        {
            if rt1.fields.iter().any(|(f, _)| !rt2.fields.contains_key(f)) {
                return Ok(false);
            }
            if rt2.fields.iter().any(|(f, _)| !rt1.fields.contains_key(f)) {
                return Ok(false);
            }
            for key in rt1.fields.keys() {
                if !is_he(rt1.fields.get(key).unwrap(), rt2.fields.get(key).unwrap())? {
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

fn all_he_prop<'a, I>(s: I, t: I) -> Result<bool, ContractivityCheckError>
where
    I: Iterator<Item = (&'a Key, &'a Prop)>,
{
    for ((k1, p1), (k2, p2)) in s.zip(t) {
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
            history: vec![],
            productive: vec![],
        }
    }

    fn check_decl(
        &mut self,
        stub: &mut VStub,
        t: &TypeDecl,
    ) -> Result<(), ContractivityCheckError> {
        let rty = RemoteType {
            id: RemoteId {
                module: self.module,
                name: t.id.name,
                arity: t.id.arity,
            },
            arg_tys: t
                .params
                .iter()
                .map(|var| Type::VarType(var.clone()))
                .collect(),
        };
        assert!(self.history.is_empty());
        assert!(self.productive.is_empty());
        if !self.is_contractive(Type::RemoteType(rty))? {
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
                    Ok(this.is_contractive(*ft.res_ty)?
                        && this.all_contractive(ft.arg_tys.into_iter())?)
                })
            }
            Type::AnyArityFunType(ft) => {
                self.with_productive_history(|this| Ok(this.is_contractive(*ft.res_ty)?))
            }
            Type::TupleType(tt) => self
                .with_productive_history(|this| Ok(this.all_contractive(tt.arg_tys.into_iter())?)),
            Type::ListType(lt) => {
                self.with_productive_history(|this| Ok(this.is_contractive(*lt.t)?))
            }
            Type::UnionType(ut) => Ok(self.all_contractive(ut.tys.into_iter())?),
            Type::OpaqueType(ot) => self
                .with_productive_history(|this| Ok(this.all_contractive(ot.arg_tys.into_iter())?)),
            Type::MapType(mt) => self.with_productive_history(|this| {
                Ok(this.is_contractive(*mt.k_type)?
                    && this.is_contractive(*mt.v_type)?
                    && this.all_contractive(mt.props.into_values().map(|prop| prop.tp))?)
            }),
            Type::RefinedRecordType(rt) => self
                .with_productive_history(|this| Ok(this.all_contractive(rt.fields.into_values())?)),
            Type::RemoteType(rt) => {
                if self.productive.contains(&rt) {
                    return Ok(true);
                }
                for t in self.history.iter().chain(self.productive.iter()) {
                    if t.id == rt.id && all_he(t.arg_tys.iter(), rt.arg_tys.iter())? {
                        return Ok(false);
                    }
                }
                match self.type_decl_body(&rt.id, &rt.arg_tys)? {
                    Some(typ) => self.with_history(rt, |this| Ok(this.is_contractive(typ)?)),
                    None => Ok(true),
                }
            }
            _ => Ok(true),
        }
    }

    fn all_contractive(
        &mut self,
        tys: impl Iterator<Item = Type>,
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
                    decl.params.iter().map(|v| v.n).zip(args.iter()).collect();
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

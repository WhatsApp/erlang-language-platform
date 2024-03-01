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
//! The main logic is function `is_foldable`, which checks whether a type
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

use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_syntax::SmolStr;
use elp_types_db::eqwalizer::form::InvalidForm;
use elp_types_db::eqwalizer::form::InvalidTypeDecl;
use elp_types_db::eqwalizer::form::TypeDecl;
use elp_types_db::eqwalizer::invalid_diagnostics::Invalid;
use elp_types_db::eqwalizer::invalid_diagnostics::NonProductiveRecursiveTypeAlias;
use elp_types_db::eqwalizer::types::OpaqueType;
use elp_types_db::eqwalizer::types::Prop;
use elp_types_db::eqwalizer::types::Type;
use fxhash::FxHashMap;

use super::db::EqwalizerASTDatabase;
use super::stub::ModuleStub;
use super::subst::Subst;
use super::ContractivityCheckError;
use super::Id;
use super::RemoteId;

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
        Type::DictMap(m) => Ok(is_he(s, &m.k_type)? || is_he(s, &m.v_type)?),
        Type::ShapeMap(m) => {
            let tys: Vec<Type> = m.props.iter().map(|p| p.tp().to_owned()).collect();
            any_he(s, tys.iter())
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
        (Type::DictMap(m1), Type::DictMap(m2)) => {
            Ok(is_he(&m1.k_type, &m2.k_type)? && is_he(&m1.v_type, &m2.v_type)?)
        }
        (Type::DictMap(_), _) => Ok(false),
        (Type::ShapeMap(m1), Type::ShapeMap(m2)) if m1.props.len() == m2.props.len() => {
            let mut props1 = m1.props.clone();
            let mut props2 = m2.props.clone();
            props1.sort_by_key(|p| p.key().to_owned());
            props2.sort_by_key(|p| p.key().to_owned());
            all_he_prop(props1.iter(), props2.iter())
        }
        (Type::ShapeMap(_), _) => Ok(false),
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
    I: Iterator<Item = &'a Prop>,
{
    for (p1, p2) in s.zip(t) {
        if !he_prop(p1, p2)? {
            return Ok(false);
        }
    }
    Ok(true)
}

fn he_prop(s: &Prop, t: &Prop) -> Result<bool, ContractivityCheckError> {
    match (s, t) {
        (Prop::ReqProp(p1), Prop::ReqProp(p2)) if p1.key == p2.key => is_he(&p1.tp, &p2.tp),
        (Prop::ReqProp(_), _) => Ok(false),
        (Prop::OptProp(p1), Prop::OptProp(p2)) if p1.key == p2.key => is_he(&p1.tp, &p2.tp),
        (Prop::OptProp(_), _) => Ok(false),
    }
}

pub struct StubContractivityChecker<'d> {
    db: &'d dyn EqwalizerASTDatabase,
    project_id: ProjectId,
    module: SmolStr,
}

impl StubContractivityChecker<'_> {
    pub fn new(
        db: &dyn EqwalizerASTDatabase,
        project_id: ProjectId,
        module: SmolStr,
    ) -> StubContractivityChecker<'_> {
        StubContractivityChecker {
            db,
            project_id,
            module,
        }
    }

    fn check_type_decl(
        &self,
        stub: &mut ModuleStub,
        t: &TypeDecl,
    ) -> Result<(), ContractivityCheckError> {
        if !self.is_contractive(&t.body)? {
            stub.types.remove(&t.id);
            stub.invalid_forms.push(self.to_invalid(t));
        }
        Ok(())
    }

    fn check_opaque_decl(
        &self,
        stub: &mut ModuleStub,
        t: &TypeDecl,
    ) -> Result<(), ContractivityCheckError> {
        if !self.is_contractive(&t.body)? {
            stub.private_opaques.remove(&t.id);
            stub.public_opaques.remove(&t.id);
            stub.invalid_forms.push(self.to_invalid(t));
        }
        Ok(())
    }

    fn to_invalid(&self, t: &TypeDecl) -> InvalidForm {
        let diagnostics =
            Invalid::NonProductiveRecursiveTypeAlias(NonProductiveRecursiveTypeAlias {
                location: t.location.clone(),
                name: t.id.to_string().into(),
            });
        InvalidForm::InvalidTypeDecl(InvalidTypeDecl {
            location: t.location.clone(),
            id: t.id.clone(),
            te: diagnostics,
        })
    }

    fn is_contractive(&self, t: &Type) -> Result<bool, ContractivityCheckError> {
        self.is_foldable(t, &[])
    }

    fn is_foldable(&self, ty: &Type, history: &[&Type]) -> Result<bool, ContractivityCheckError> {
        let mut produced = false;
        for &t in history.iter().rev() {
            if produced && t == ty {
                return Ok(true);
            }
            produced = produced || self.is_producer(t)?;
        }
        let mut new_history = history.to_owned();
        new_history.push(ty);
        match ty {
            Type::FunType(ft) => {
                if !ft.forall.is_empty() {
                    return Err(ContractivityCheckError::NonEmptyForall);
                }
                Ok(self.is_foldable(&ft.res_ty, &new_history)?
                    && self.all_foldable(ft.arg_tys.iter(), &new_history)?)
            }
            Type::AnyArityFunType(ft) => Ok(self.is_foldable(&ft.res_ty, &new_history)?),
            Type::TupleType(tt) => Ok(self.all_foldable(tt.arg_tys.iter(), &new_history)?),
            Type::ListType(lt) => Ok(self.is_foldable(&lt.t, &new_history)?),
            Type::UnionType(ut) => Ok(self.all_foldable(ut.tys.iter(), &new_history)?),
            Type::OpaqueType(ot) => Ok(self.all_foldable(ot.arg_tys.iter(), &new_history)?),
            Type::DictMap(mt) => Ok(self.is_foldable(&mt.k_type, &new_history)?
                && self.is_foldable(&mt.v_type, &new_history)?),
            Type::ShapeMap(mt) => {
                Ok(self.all_foldable(mt.props.iter().map(|p| p.tp()), &new_history)?)
            }
            Type::RefinedRecordType(rt) => Ok(self.all_foldable(rt.fields.values(), &new_history)?),
            Type::RemoteType(rt) => {
                for &t in history.iter() {
                    if he_by_coupling(t, ty)? {
                        return Ok(false);
                    }
                }
                match self.type_decl_body(&rt.id, &rt.arg_tys)? {
                    Some(typ) => Ok(self.is_foldable(&typ, &new_history)?),
                    None => Ok(true),
                }
            }
            _ => Ok(true),
        }
    }

    fn all_foldable<'a, I>(
        &self,
        tys: I,
        history: &[&Type],
    ) -> Result<bool, ContractivityCheckError>
    where
        I: Iterator<Item = &'a Type>,
    {
        for ty in tys {
            if !self.is_foldable(ty, history)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    fn is_producer(&self, t: &Type) -> Result<bool, ContractivityCheckError> {
        match t {
            Type::FunType(_)
            | Type::TupleType(_)
            | Type::ListType(_)
            | Type::OpaqueType(_)
            | Type::DictMap(_)
            | Type::ShapeMap(_)
            | Type::RefinedRecordType(_)
            | Type::AnyArityFunType(_) => Ok(true),
            Type::RemoteType(_) => Ok(false),
            Type::UnionType(_) => Ok(false),
            _ => Err(ContractivityCheckError::UnexpectedType),
        }
    }

    fn type_decl_body(
        &self,
        id: &RemoteId,
        args: &[Type],
    ) -> Result<Option<Type>, ContractivityCheckError> {
        let local_id = Id {
            name: id.name.clone(),
            arity: id.arity,
        };
        let stub = self
            .db
            .expanded_stub(self.project_id, ModuleName::new(id.module.as_str()))
            .map_err(|_| ContractivityCheckError::UnexpectedID(id.clone()))?;
        fn subst(decl: &TypeDecl, args: &[Type]) -> Type {
            let sub: FxHashMap<u32, &Type> =
                decl.params.iter().map(|v| v.n).zip(args.iter()).collect();
            Subst { sub }.apply(decl.body.clone())
        }
        Ok(stub
            .types
            .get(&local_id)
            .map(|t| subst(t, args))
            .or_else(|| {
                if self.module == id.module {
                    stub.private_opaques.get(&local_id).map(|t| subst(t, args))
                } else {
                    stub.public_opaques.get(&local_id).map(|_| {
                        Type::OpaqueType(OpaqueType {
                            id: id.clone(),
                            arg_tys: args.to_owned(),
                        })
                    })
                }
            }))
    }

    pub fn check(&self, stub: &ModuleStub) -> Result<ModuleStub, ContractivityCheckError> {
        let mut stub_result = stub.clone();
        stub.types
            .values()
            .map(|decl| self.check_type_decl(&mut stub_result, decl))
            .collect::<Result<Vec<()>, _>>()?;
        stub.private_opaques
            .values()
            .map(|decl| self.check_opaque_decl(&mut stub_result, decl))
            .collect::<Result<Vec<()>, _>>()?;
        Ok(stub_result)
    }
}

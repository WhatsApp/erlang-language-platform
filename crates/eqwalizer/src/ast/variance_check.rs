/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This module performs the third step of stubs validation
//!
//! It ensures that all opaque types are covariant, and finds a witness of
//! a contravariant expansion otherwise.
//!
//! This is done according to the following steps:
//! 1. Iterate over all opaque type declarations of a stub
//! 2. Iterate over all parameters of the declaration
//! 3. For each parameter, recursively traverse the type declaration while
//!    keeping track of the variance, and expand non-opaques along the way.
//! 4. When a contravariant occurrence of the parameter is found, go up the
//!    stack and rebuild the original type once for every non-opaque
//!    expanded during the descent.
//!
//! This has the effect of producing a list of types, where the first one
//! is the original type declaration, each successive one is an expansion
//! of its predecessor, and the final one contains a contravariant occurrence
//! of a type parameter.
//!
//! The implementation is clone and allocation heavy, but only if a
//! contravariant expansion is found, which should not happen.

use std::collections::BTreeMap;

use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_types_db::eqwalizer::form::TypeDecl;
use elp_types_db::eqwalizer::invalid_diagnostics::AliasWithNonCovariantParam;
use elp_types_db::eqwalizer::invalid_diagnostics::Invalid;
use elp_types_db::eqwalizer::types::AnyArityFunType;
use elp_types_db::eqwalizer::types::FunType;
use elp_types_db::eqwalizer::types::Key;
use elp_types_db::eqwalizer::types::ListType;
use elp_types_db::eqwalizer::types::MapType;
use elp_types_db::eqwalizer::types::OpaqueType;
use elp_types_db::eqwalizer::types::Prop;
use elp_types_db::eqwalizer::types::RefinedRecordType;
use elp_types_db::eqwalizer::types::RemoteType;
use elp_types_db::eqwalizer::types::TupleType;
use elp_types_db::eqwalizer::types::Type;
use elp_types_db::eqwalizer::types::UnionType;
use elp_types_db::eqwalizer::types::VarType;
use fxhash::FxHashMap;

use super::Id;
use super::RemoteId;
use super::VarianceCheckError;
use super::stub::VStub;
use super::subst::Subst;
use crate::db::EqwalizerDiagnosticsDatabase;

pub struct VarianceChecker<'d> {
    db: &'d dyn EqwalizerDiagnosticsDatabase,
    project_id: ProjectId,
}

impl VarianceChecker<'_> {
    pub fn new(
        db: &dyn EqwalizerDiagnosticsDatabase,
        project_id: ProjectId,
    ) -> VarianceChecker<'_> {
        VarianceChecker { db, project_id }
    }

    fn check_opaque_decl(
        &self,
        v_stub: &mut VStub,
        t: &TypeDecl,
    ) -> Result<(), VarianceCheckError> {
        if let Some((ty_var, expansion)) = self.expands_to_contravariant(t)? {
            let invalid = self.to_invalid(t, &ty_var, expansion);
            v_stub.invalids.push(invalid);
            v_stub.invalid_ids.insert(t.id.clone());
        }
        Ok(())
    }

    fn expands_to_contravariant(
        &self,
        decl: &TypeDecl,
    ) -> Result<Option<(VarType, Vec<Type>)>, VarianceCheckError> {
        for tv in &decl.params {
            let expansion = self.find_contravariant_expansion(&decl.body, tv, true, &vec![])?;
            if !expansion.is_empty() {
                return Ok(Some((tv.clone(), expansion)));
            }
        }
        Ok(None)
    }

    fn find_contravariant_expansion_in_tys<I>(
        &self,
        tys: &mut I,
        tv: &VarType,
        positive: bool,
        history: &Vec<&RemoteType>,
    ) -> Result<Vec<Vec<Type>>, VarianceCheckError>
    where
        I: Iterator<Item = Type>,
    {
        if let Some(ty) = tys.next() {
            let expansion = self.find_contravariant_expansion(&ty, tv, positive, history)?;
            if !expansion.is_empty() {
                Ok(expansion
                    .into_iter()
                    .map(|ty| {
                        let mut tys2: Vec<Type> = tys.collect();
                        tys2.insert(0, ty);
                        tys2
                    })
                    .collect())
            } else {
                Ok(self
                    .find_contravariant_expansion_in_tys(tys, tv, positive, history)?
                    .into_iter()
                    .map(|mut exp| {
                        exp.insert(0, ty.clone());
                        exp
                    })
                    .collect())
            }
        } else {
            Ok(vec![])
        }
    }

    fn find_contravariant_expansion_in_props(
        &self,
        props: &BTreeMap<Key, Prop>,
        tv: &VarType,
        positive: bool,
        history: &Vec<&RemoteType>,
    ) -> Result<Vec<BTreeMap<Key, Prop>>, VarianceCheckError> {
        for (key, prop) in props.iter() {
            let expansion = self.find_contravariant_expansion(&prop.tp, tv, positive, history)?;
            if !expansion.is_empty() {
                return Ok(expansion
                    .into_iter()
                    .map(|tp| {
                        let mut props_copy = props.clone();
                        props_copy.insert(key.clone(), Prop { req: prop.req, tp });
                        props_copy
                    })
                    .collect());
            }
        }
        Ok(vec![])
    }

    fn find_contravariant_expansion(
        &self,
        ty: &Type,
        tv: &VarType,
        positive: bool,
        history: &Vec<&RemoteType>,
    ) -> Result<Vec<Type>, VarianceCheckError> {
        let tv_absent = ty
            .traverse(&mut |t| match t {
                Type::VarType(v) if v == tv => Err(()),
                _ => Ok(()),
            })
            .is_ok();
        if tv_absent {
            return Ok(vec![]);
        }
        match ty {
            Type::VarType(_) if !positive => Ok(vec![ty.clone()]),
            Type::RemoteType(rt) => {
                if history.iter().any(|&t| t == rt) {
                    return Ok(vec![]);
                }
                let mut new_history = history.clone();
                new_history.push(rt);
                if let Some(tbody) = self.type_decl_body(&rt.id, &rt.arg_tys)? {
                    let mut exps =
                        self.find_contravariant_expansion(&tbody, tv, positive, &new_history)?;
                    if !exps.is_empty() {
                        exps.insert(0, ty.clone());
                    }
                    Ok(exps)
                } else {
                    Ok(vec![])
                }
            }
            Type::FunType(ft) => {
                let arg_exps = self.find_contravariant_expansion_in_tys(
                    &mut ft.arg_tys.clone().into_iter(),
                    tv,
                    !positive,
                    history,
                )?;
                if arg_exps.is_empty() {
                    Ok(self
                        .find_contravariant_expansion(&ft.res_ty, tv, positive, history)?
                        .into_iter()
                        .map(|t| {
                            Type::FunType(FunType {
                                forall: ft.forall.clone(),
                                arg_tys: ft.arg_tys.clone(),
                                res_ty: Box::new(t),
                            })
                        })
                        .collect())
                } else {
                    Ok(arg_exps
                        .into_iter()
                        .map(|ts| {
                            Type::FunType(FunType {
                                forall: ft.forall.clone(),
                                arg_tys: ts,
                                res_ty: ft.res_ty.clone(),
                            })
                        })
                        .collect())
                }
            }
            Type::AnyArityFunType(ft) => Ok(self
                .find_contravariant_expansion(&ft.res_ty, tv, positive, history)?
                .into_iter()
                .map(|t| {
                    Type::AnyArityFunType(AnyArityFunType {
                        res_ty: Box::new(t),
                    })
                })
                .collect()),
            Type::TupleType(tt) => Ok(self
                .find_contravariant_expansion_in_tys(
                    &mut tt.arg_tys.clone().into_iter(),
                    tv,
                    positive,
                    history,
                )?
                .into_iter()
                .map(|arg_tys| Type::TupleType(TupleType { arg_tys }))
                .collect()),
            Type::ListType(lt) => Ok(self
                .find_contravariant_expansion(&lt.t, tv, positive, history)?
                .into_iter()
                .map(|t| Type::ListType(ListType { t: Box::new(t) }))
                .collect()),
            Type::UnionType(ut) => Ok(self
                .find_contravariant_expansion_in_tys(
                    &mut ut.tys.clone().into_iter(),
                    tv,
                    positive,
                    history,
                )?
                .into_iter()
                .map(|tys| Type::UnionType(UnionType { tys }))
                .collect()),
            Type::OpaqueType(ot) => Ok(self
                .find_contravariant_expansion_in_tys(
                    &mut ot.arg_tys.clone().into_iter(),
                    tv,
                    positive,
                    history,
                )?
                .into_iter()
                .map(|arg_tys| {
                    Type::OpaqueType(OpaqueType {
                        id: ot.id.clone(),
                        arg_tys,
                    })
                })
                .collect()),
            Type::RefinedRecordType(rt) => {
                for (name, ty) in rt.fields.iter() {
                    let ty_exps = self.find_contravariant_expansion(ty, tv, positive, history)?;
                    if !ty_exps.is_empty() {
                        return Ok(ty_exps
                            .into_iter()
                            .map(|field_ty| {
                                let mut new_fields = rt.fields.clone();
                                new_fields.insert(*name, field_ty);
                                new_fields
                            })
                            .map(|fields| {
                                Type::RefinedRecordType(RefinedRecordType {
                                    rec_type: rt.rec_type.clone(),
                                    fields,
                                })
                            })
                            .collect());
                    }
                }
                Ok(vec![])
            }
            Type::MapType(mt) => {
                let k_exps =
                    self.find_contravariant_expansion(&mt.k_type, tv, positive, history)?;
                if k_exps.is_empty() {
                    let v_exps =
                        self.find_contravariant_expansion(&mt.v_type, tv, positive, history)?;
                    if v_exps.is_empty() {
                        Ok(self
                            .find_contravariant_expansion_in_props(
                                &mt.props, tv, positive, history,
                            )?
                            .into_iter()
                            .map(|props| {
                                Type::MapType(MapType {
                                    props,
                                    k_type: mt.k_type.clone(),
                                    v_type: mt.v_type.clone(),
                                })
                            })
                            .collect())
                    } else {
                        Ok(v_exps
                            .into_iter()
                            .map(|v_type| {
                                Type::MapType(MapType {
                                    k_type: mt.k_type.clone(),
                                    v_type: Box::new(v_type),
                                    props: mt.props.clone(),
                                })
                            })
                            .collect())
                    }
                } else {
                    Ok(k_exps
                        .into_iter()
                        .map(|k_type| {
                            Type::MapType(MapType {
                                k_type: Box::new(k_type),
                                v_type: mt.v_type.clone(),
                                props: mt.props.clone(),
                            })
                        })
                        .collect())
                }
            }
            Type::VarType(_)
            | Type::AtomLitType(_)
            | Type::AnyFunType
            | Type::AnyTupleType
            | Type::NilType
            | Type::RecordType(_)
            | Type::BinaryType
            | Type::AnyType
            | Type::AtomType
            | Type::DynamicType
            | Type::NoneType
            | Type::PidType
            | Type::PortType
            | Type::ReferenceType
            | Type::NumberType
            | Type::BoundedDynamicType(_) => Ok(vec![]),
        }
    }

    fn to_invalid(&self, t: &TypeDecl, ty_var: &VarType, expansion: Vec<Type>) -> Invalid {
        Invalid::AliasWithNonCovariantParam(AliasWithNonCovariantParam {
            type_var: ty_var.name.into(),
            pos: t.pos.clone(),
            name: t.id.to_string().into(),
            exps: expansion,
        })
    }

    fn type_decl_body(
        &self,
        id: &RemoteId,
        args: &[Type],
    ) -> Result<Option<Type>, VarianceCheckError> {
        let local_id = Id {
            name: id.name,
            arity: id.arity,
        };
        let v_stub = self
            .db
            .contractive_stub(self.project_id, ModuleName::new(id.module.as_str()))
            .map_err(|err| VarianceCheckError::ErrorExpandingID(id.clone(), Box::new(err)))?;
        fn subst(decl: &TypeDecl, args: &[Type]) -> Type {
            if decl.params.is_empty() {
                decl.body.clone()
            } else {
                let sub: FxHashMap<u32, &Type> =
                    decl.params.iter().map(|v| v.n).zip(args.iter()).collect();
                Subst { sub }.apply(decl.body.clone())
            }
        }
        Ok(v_stub.get_type(&local_id).map(|t| subst(t, args)))
    }

    pub fn check(&self, stub: &VStub) -> Result<VStub, VarianceCheckError> {
        let mut result = stub.clone();
        for decl in stub.opaques() {
            self.check_opaque_decl(&mut result, decl)?
        }
        Ok(result)
    }
}

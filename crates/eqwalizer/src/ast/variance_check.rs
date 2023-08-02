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

use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use fxhash::FxHashMap;

use super::db::EqwalizerASTDatabase;
use super::form::InvalidForm;
use super::form::InvalidTypeDecl;
use super::form::TypeDecl;
use super::invalid_diagnostics::AliasWithNonCovariantParam;
use super::invalid_diagnostics::Invalid;
use super::stub::ModuleStub;
use super::subst::Subst;
use super::types::AnyArityFunType;
use super::types::DictMap;
use super::types::FunType;
use super::types::ListType;
use super::types::OpaqueType;
use super::types::OptProp;
use super::types::Prop;
use super::types::RefinedRecordType;
use super::types::RemoteType;
use super::types::ReqProp;
use super::types::ShapeMap;
use super::types::TupleType;
use super::types::Type;
use super::types::UnionType;
use super::types::VarType;
use super::Id;
use super::RemoteId;
use super::VarianceCheckError;

pub struct VarianceChecker<'d> {
    db: &'d dyn EqwalizerASTDatabase,
    project_id: ProjectId,
}

impl VarianceChecker<'_> {
    pub fn new<'d>(db: &'d dyn EqwalizerASTDatabase, project_id: ProjectId) -> VarianceChecker<'d> {
        return VarianceChecker { db, project_id };
    }

    fn check_opaque_decl(
        &self,
        stub: &mut ModuleStub,
        t: &TypeDecl,
    ) -> Result<(), VarianceCheckError> {
        if let Some((ty_var, expansion)) = self.expands_to_contravariant(t)? {
            let invalid = self.to_invalid(t, &ty_var, expansion);
            stub.invalid_forms.push(invalid);
            stub.private_opaques.remove(&t.id);
        }
        Ok(())
    }

    fn expands_to_contravariant(
        &self,
        decl: &TypeDecl,
    ) -> Result<Option<(VarType, Vec<Type>)>, VarianceCheckError> {
        for tv in &decl.params {
            let expansion = self.find_contravariant_expansion(&decl.body, tv, true, &vec![])?;
            if expansion.len() > 0 {
                return Ok(Some((tv.clone(), expansion)));
            }
        }
        return Ok(None);
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
            if expansion.len() > 0 {
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

    fn find_contravariant_expansion_in_props<I>(
        &self,
        props: &mut I,
        tv: &VarType,
        positive: bool,
        history: &Vec<&RemoteType>,
    ) -> Result<Vec<Vec<Prop>>, VarianceCheckError>
    where
        I: Iterator<Item = Prop>,
    {
        if let Some(prop) = props.next() {
            let expansion = self.find_contravariant_expansion(prop.tp(), tv, positive, history)?;
            if expansion.len() > 0 {
                Ok(expansion
                    .into_iter()
                    .map(|tp| {
                        let new_prop = match &prop {
                            Prop::OptProp(op) => Prop::OptProp(OptProp {
                                key: op.key.clone(),
                                tp,
                            }),
                            Prop::ReqProp(rp) => Prop::ReqProp(ReqProp {
                                key: rp.key.clone(),
                                tp,
                            }),
                        };
                        let mut props2: Vec<Prop> = props.collect();
                        props2.insert(0, new_prop);
                        props2
                    })
                    .collect())
            } else {
                Ok(self
                    .find_contravariant_expansion_in_props(props, tv, positive, history)?
                    .into_iter()
                    .map(|mut exp| {
                        exp.insert(0, prop.clone());
                        exp
                    })
                    .collect())
            }
        } else {
            Ok(vec![])
        }
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
                                new_fields.insert(name.clone(), field_ty);
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
            Type::DictMap(dt) => {
                let k_exps =
                    self.find_contravariant_expansion(&dt.k_type, tv, positive, history)?;
                if k_exps.is_empty() {
                    let v_exps =
                        self.find_contravariant_expansion(&dt.v_type, tv, positive, history)?;
                    Ok(v_exps
                        .into_iter()
                        .map(|v_type| {
                            Type::DictMap(DictMap {
                                k_type: dt.k_type.clone(),
                                v_type: Box::new(v_type),
                            })
                        })
                        .collect())
                } else {
                    Ok(k_exps
                        .into_iter()
                        .map(|k_type| {
                            Type::DictMap(DictMap {
                                k_type: Box::new(k_type),
                                v_type: dt.v_type.clone(),
                            })
                        })
                        .collect())
                }
            }
            Type::ShapeMap(mt) => Ok(self
                .find_contravariant_expansion_in_props(
                    &mut mt.props.clone().into_iter(),
                    tv,
                    positive,
                    history,
                )?
                .into_iter()
                .map(|props| Type::ShapeMap(ShapeMap { props }))
                .collect()),
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
            | Type::NumberType => Ok(vec![]),
        }
    }

    fn to_invalid(&self, t: &TypeDecl, ty_var: &VarType, expansion: Vec<Type>) -> InvalidForm {
        let diagnostics = Invalid::AliasWithNonCovariantParam(AliasWithNonCovariantParam {
            type_var: ty_var.name.clone(),
            location: t.location.clone(),
            name: t.id.to_string().into(),
            exps: expansion,
        });
        InvalidForm::InvalidTypeDecl(InvalidTypeDecl {
            location: t.location.clone(),
            id: t.id.clone(),
            te: diagnostics,
        })
    }

    fn type_decl_body(
        &self,
        id: &RemoteId,
        args: &Vec<Type>,
    ) -> Result<Option<Type>, VarianceCheckError> {
        let local_id = Id {
            name: id.name.clone(),
            arity: id.arity,
        };
        let stub = self
            .db
            .contractive_stub(self.project_id, ModuleName::new(id.module.as_str()))
            .map_err(|_| VarianceCheckError::UnexpectedID(id.clone()))?;
        fn subst(decl: &TypeDecl, args: &Vec<Type>) -> Type {
            let sub: FxHashMap<u32, &Type> =
                decl.params.iter().map(|v| v.n).zip(args.iter()).collect();
            Subst { sub }.apply(decl.body.clone())
        }
        Ok(stub.types.get(&local_id).map(|t| subst(t, args)))
    }

    pub fn check(&self, stub: &ModuleStub) -> Result<ModuleStub, VarianceCheckError> {
        let mut stub_result = stub.clone();
        stub.private_opaques
            .iter()
            .map(|(_, decl)| self.check_opaque_decl(&mut stub_result, decl))
            .collect::<Result<Vec<()>, _>>()?;
        Ok(stub_result)
    }
}

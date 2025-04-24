/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This module contains the first phase of stubs validation
//!
//! It ensures that type ids point to types that exist, and expand local ids
//! to remote ids by adding the module they are defined in.
//!
//! It also performs several checks on types, such as ensuring that the same
//! type variable does not appear twice in the parameters of a type.

use std::collections::BTreeSet;
use std::sync::Arc;

use elp_base_db::ModuleName;
use elp_base_db::ProjectId;
use elp_types_db::StringId;
use elp_types_db::eqwalizer::ext_types::AnyArityFunExtType;
use elp_types_db::eqwalizer::ext_types::ConstrainedFunType;
use elp_types_db::eqwalizer::ext_types::ExtProp;
use elp_types_db::eqwalizer::ext_types::ExtType;
use elp_types_db::eqwalizer::ext_types::FunExtType;
use elp_types_db::eqwalizer::ext_types::ListExtType;
use elp_types_db::eqwalizer::ext_types::MapExtType;
use elp_types_db::eqwalizer::ext_types::OptExtProp;
use elp_types_db::eqwalizer::ext_types::RecordRefinedExtType;
use elp_types_db::eqwalizer::ext_types::RefinedField;
use elp_types_db::eqwalizer::ext_types::RemoteExtType;
use elp_types_db::eqwalizer::ext_types::ReqExtProp;
use elp_types_db::eqwalizer::ext_types::TupleExtType;
use elp_types_db::eqwalizer::ext_types::UnionExtType;
use elp_types_db::eqwalizer::ext_types::VarExtType;
use elp_types_db::eqwalizer::form::ExternalCallback;
use elp_types_db::eqwalizer::form::ExternalForm;
use elp_types_db::eqwalizer::form::ExternalFunSpec;
use elp_types_db::eqwalizer::form::ExternalOpaqueDecl;
use elp_types_db::eqwalizer::form::ExternalRecDecl;
use elp_types_db::eqwalizer::form::ExternalRecField;
use elp_types_db::eqwalizer::form::ExternalTypeDecl;
use elp_types_db::eqwalizer::form::TypeDecl;
use elp_types_db::eqwalizer::invalid_diagnostics::BadMapKey;
use elp_types_db::eqwalizer::invalid_diagnostics::Invalid;
use elp_types_db::eqwalizer::invalid_diagnostics::NonExportedId;
use elp_types_db::eqwalizer::invalid_diagnostics::RecursiveConstraint;
use elp_types_db::eqwalizer::invalid_diagnostics::RefinedRecordInTypeCast;
use elp_types_db::eqwalizer::invalid_diagnostics::RepeatedTyVarInTyDecl;
use elp_types_db::eqwalizer::invalid_diagnostics::TyVarWithMultipleConstraints;
use elp_types_db::eqwalizer::invalid_diagnostics::UnboundTyVarInTyDecl;
use elp_types_db::eqwalizer::invalid_diagnostics::UnknownId;
use elp_types_db::eqwalizer::invalid_diagnostics::VariablesInTypeCast;
use elp_types_db::eqwalizer::types::Type;
use fxhash::FxHashMap;
use fxhash::FxHashSet;

use super::AST;
use super::Id;
use super::RemoteId;
use super::TypeConversionError;
use super::convert_types::TypeConverter;
use super::stub::ModuleStub;
use crate::ast;
use crate::ast::Visibility;
use crate::db::EqwalizerDiagnosticsDatabase;

struct Expander<'d> {
    module: StringId,
    project_id: ProjectId,
    invalids: Vec<Invalid>,
    db: &'d dyn EqwalizerDiagnosticsDatabase,
}

impl Expander<'_> {
    fn expand_fun_spec(&mut self, fun_spec: ExternalFunSpec) -> Result<ExternalFunSpec, Invalid> {
        let types = self.expand_cfts(fun_spec.types)?;
        Ok(ExternalFunSpec { types, ..fun_spec })
    }

    fn expand_cfts(
        &mut self,
        cfts: Vec<ConstrainedFunType>,
    ) -> Result<Vec<ConstrainedFunType>, Invalid> {
        cfts.into_iter().map(|cft| self.expand_cft(cft)).collect()
    }

    fn expand_cft(&mut self, cft: ConstrainedFunType) -> Result<ConstrainedFunType, Invalid> {
        let ft = {
            if cft.constraints.is_empty() {
                cft.ty
            } else {
                self.check_multiply_constrained_type_var(&cft)?;
                let subst: FxHashMap<StringId, ExtType> = cft
                    .constraints
                    .into_iter()
                    .map(|c| (c.t_var, c.ty))
                    .collect();
                let arg_tys = cft
                    .ty
                    .arg_tys
                    .into_iter()
                    .map(|ty| self.expand_constraints(ty, &subst, &FxHashSet::default()))
                    .collect::<Result<Vec<_>, _>>()?;
                let res_ty =
                    self.expand_constraints(*cft.ty.res_ty, &subst, &FxHashSet::default())?;
                FunExtType {
                    arg_tys,
                    res_ty: Box::new(res_ty),
                    pos: cft.ty.pos,
                }
            }
        };
        let arg_tys = self.expand_types(ft.arg_tys)?;
        let res_ty = Box::new(self.expand_type(*ft.res_ty)?);
        let exp_ft = FunExtType {
            arg_tys,
            res_ty,
            pos: ft.pos,
        };
        Ok(ConstrainedFunType {
            pos: cft.pos,
            ty: exp_ft,
            constraints: vec![],
        })
    }

    fn expand_callback(&mut self, cb: ExternalCallback) -> Result<ExternalCallback, Invalid> {
        let types = self.expand_cfts(cb.types)?;
        Ok(ExternalCallback { types, ..cb })
    }

    fn expand_type_decl(&mut self, decl: ExternalTypeDecl) -> Result<ExternalTypeDecl, Invalid> {
        let body = self
            .validate_type_vars(&decl.pos, &decl.body, &decl.params)
            .and_then(|()| self.expand_type(decl.body))?;
        Ok(ExternalTypeDecl { body, ..decl })
    }

    fn expand_opaque_decl(
        &mut self,
        decl: ExternalOpaqueDecl,
    ) -> Result<ExternalOpaqueDecl, Invalid> {
        let body = self
            .validate_type_vars(&decl.pos, &decl.body, &decl.params)
            .and_then(|()| self.expand_type(decl.body))?;
        Ok(ExternalOpaqueDecl { body, ..decl })
    }

    fn validate_type_vars(
        &mut self,
        pos: &ast::Pos,
        body: &ExtType,
        params: &[StringId],
    ) -> Result<(), Invalid> {
        self.check_repeated_type_param(pos, params)?;
        body.traverse(&mut |ty| match ty {
            ExtType::VarExtType(ty_var) => self.check_unbound_type_var(pos, params, ty_var),
            _ => Ok(()),
        })?;
        Ok(())
    }

    fn check_repeated_type_param(
        &mut self,
        pos: &ast::Pos,
        params: &[StringId],
    ) -> Result<(), Invalid> {
        let mut names = FxHashSet::default();
        for name in params {
            if names.contains(name) {
                return Err(Invalid::RepeatedTyVarInTyDecl(RepeatedTyVarInTyDecl {
                    pos: pos.clone(),
                    name: name.into(),
                }));
            }
            names.insert(name);
        }
        Ok(())
    }

    fn check_multiply_constrained_type_var(
        &mut self,
        cft: &ConstrainedFunType,
    ) -> Result<(), Invalid> {
        let mut names = FxHashSet::default();
        let vars: Vec<StringId> = cft.constraints.iter().map(|c| c.t_var).collect();
        for name in vars {
            if names.contains(&name) {
                return Err(Invalid::TyVarWithMultipleConstraints(
                    TyVarWithMultipleConstraints {
                        pos: cft.pos.clone(),
                        n: name.into(),
                    },
                ));
            }
            names.insert(name);
        }
        Ok(())
    }

    fn check_unbound_type_var(
        &mut self,
        pos: &ast::Pos,
        params: &[StringId],
        ty_var: &VarExtType,
    ) -> Result<(), Invalid> {
        if !params.contains(&ty_var.name) {
            Err(Invalid::UnboundTyVarInTyDecl(UnboundTyVarInTyDecl {
                pos: pos.clone(),
                name: ty_var.name.into(),
            }))
        } else {
            Ok(())
        }
    }

    fn expand_rec_decl(&mut self, decl: ExternalRecDecl) -> Result<ExternalRecDecl, Invalid> {
        let fields = decl
            .fields
            .into_iter()
            .map(|field| self.expand_rec_field(field))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(ExternalRecDecl { fields, ..decl })
    }

    fn expand_types(&mut self, ts: Vec<ExtType>) -> Result<Vec<ExtType>, Invalid> {
        ts.into_iter().map(|t| self.expand_type(t)).collect()
    }

    fn expand_type(&mut self, t: ExtType) -> Result<ExtType, Invalid> {
        match t {
            ExtType::LocalExtType(ty) => {
                let id = RemoteId {
                    module: self.module,
                    name: ty.id.name,
                    arity: ty.id.arity,
                };
                let expanded_params = self.expand_types(ty.args)?;
                Ok(ExtType::RemoteExtType(RemoteExtType {
                    id,
                    args: expanded_params,
                    pos: ty.pos,
                }))
            }
            ExtType::RemoteExtType(ty) => {
                let local_id = Id {
                    name: ty.id.name,
                    arity: ty.id.arity,
                };
                let module = ModuleName::new(&ty.id.module);
                if let Some(visibility) = self
                    .db
                    .type_ids(self.project_id, module)
                    .ok()
                    .and_then(|ids| ids.get(&local_id).copied())
                {
                    if visibility == Visibility::Public || ty.id.module == self.module {
                        let expanded_params = self.expand_types(ty.args)?;
                        Ok(ExtType::RemoteExtType(RemoteExtType {
                            id: ty.id,
                            args: expanded_params,
                            pos: ty.pos,
                        }))
                    } else {
                        Err(Invalid::NonExportedId(NonExportedId {
                            pos: ty.pos,
                            id: ty.id,
                        }))
                    }
                } else {
                    Err(Invalid::UnknownId(UnknownId {
                        pos: ty.pos,
                        id: ty.id,
                    }))
                }
            }
            ExtType::FunExtType(ty) => Ok(ExtType::FunExtType(FunExtType {
                pos: ty.pos,
                arg_tys: self.expand_types(ty.arg_tys)?,
                res_ty: Box::new(self.expand_type(*ty.res_ty)?),
            })),
            ExtType::AnyArityFunExtType(ty) => {
                Ok(ExtType::AnyArityFunExtType(AnyArityFunExtType {
                    pos: ty.pos,
                    res_ty: Box::new(self.expand_type(*ty.res_ty)?),
                }))
            }
            ExtType::TupleExtType(ty) => Ok(ExtType::TupleExtType(TupleExtType {
                pos: ty.pos,
                arg_tys: self.expand_types(ty.arg_tys)?,
            })),
            ExtType::ListExtType(ty) => Ok(ExtType::ListExtType(ListExtType {
                pos: ty.pos,
                t: Box::new(self.expand_type(*ty.t)?),
            })),
            ExtType::AnyListExtType(ty) => Ok(ExtType::ListExtType(ListExtType {
                pos: ty.pos.clone(),
                t: Box::new(ExtType::dynamic_ext_type(ty.pos)),
            })),
            ExtType::UnionExtType(ty) => Ok(ExtType::UnionExtType(UnionExtType {
                pos: ty.pos,
                tys: self.expand_types(ty.tys)?,
            })),
            ExtType::MapExtType(ty) => {
                if let Some(invalid_prop) = ty.props.iter().find(|prop| !prop.is_ok()) {
                    let pos = invalid_prop.pos().clone();
                    let invalid = Invalid::BadMapKey(BadMapKey {
                        pos,
                        required: invalid_prop.required(),
                    });
                    self.invalids.push(invalid);
                    Ok(ExtType::MapExtType(MapExtType {
                        pos: ty.pos.clone(),
                        props: vec![ExtProp::OptExtProp(OptExtProp {
                            pos: ty.pos.clone(),
                            key: ExtType::dynamic_ext_type(ty.pos.clone()),
                            tp: ExtType::dynamic_ext_type(ty.pos),
                        })],
                    }))
                } else {
                    let props = ty
                        .props
                        .into_iter()
                        .map(|prop| self.expand_prop(prop))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(ExtType::MapExtType(MapExtType { pos: ty.pos, props }))
                }
            }
            ExtType::AnyMapExtType(ty) => Ok(ExtType::MapExtType(MapExtType {
                pos: ty.pos.clone(),
                props: vec![ExtProp::OptExtProp(OptExtProp {
                    pos: ty.pos.clone(),
                    key: ExtType::dynamic_ext_type(ty.pos.clone()),
                    tp: ExtType::dynamic_ext_type(ty.pos),
                })],
            })),
            ExtType::RecordRefinedExtType(ty) => {
                Ok(ExtType::RecordRefinedExtType(RecordRefinedExtType {
                    pos: ty.pos,
                    name: ty.name,
                    refined_fields: ty
                        .refined_fields
                        .into_iter()
                        .map(|field| self.expand_refined_record_field(field))
                        .collect::<Result<Vec<_>, _>>()?,
                }))
            }
            ExtType::VarExtType(_)
            | ExtType::BuiltinExtType(_)
            | ExtType::IntLitExtType(_)
            | ExtType::AtomLitExtType(_)
            | ExtType::RecordExtType(_)
            | ExtType::UnOpType(_)
            | ExtType::BinOpType(_) => Ok(t),
        }
    }

    fn expand_prop(&mut self, prop: ExtProp) -> Result<ExtProp, Invalid> {
        match prop {
            ExtProp::ReqExtProp(prop) => Ok(ExtProp::ReqExtProp(ReqExtProp {
                pos: prop.pos,
                key: self.expand_type(prop.key)?,
                tp: self.expand_type(prop.tp)?,
            })),
            ExtProp::OptExtProp(prop) => Ok(ExtProp::OptExtProp(OptExtProp {
                pos: prop.pos,
                key: self.expand_type(prop.key)?,
                tp: self.expand_type(prop.tp)?,
            })),
            prop => Err(Invalid::BadMapKey(BadMapKey {
                pos: prop.pos().clone(),
                required: prop.required(),
            })),
        }
    }

    fn expand_refined_record_field(
        &mut self,
        field: RefinedField,
    ) -> Result<RefinedField, Invalid> {
        Ok(RefinedField {
            label: field.label,
            ty: self.expand_type(field.ty)?,
        })
    }

    fn expand_all_constraints(
        &mut self,
        ts: Vec<ExtType>,
        sub: &FxHashMap<StringId, ExtType>,
        stack: &FxHashSet<StringId>,
    ) -> Result<Vec<ExtType>, Invalid> {
        ts.into_iter()
            .map(|t| self.expand_constraints(t, sub, stack))
            .collect()
    }

    fn expand_constraints(
        &mut self,
        t: ExtType,
        sub: &FxHashMap<StringId, ExtType>,
        stack: &FxHashSet<StringId>,
    ) -> Result<ExtType, Invalid> {
        match t {
            ExtType::LocalExtType(ty) => {
                let id = RemoteId {
                    module: self.module,
                    name: ty.id.name,
                    arity: ty.id.arity,
                };
                let expanded_params = self.expand_all_constraints(ty.args, sub, stack)?;
                Ok(ExtType::RemoteExtType(RemoteExtType {
                    id,
                    args: expanded_params,
                    pos: ty.pos,
                }))
            }
            ExtType::RemoteExtType(ty) => Ok(ExtType::RemoteExtType(RemoteExtType {
                pos: ty.pos,
                id: ty.id,
                args: self.expand_all_constraints(ty.args, sub, stack)?,
            })),
            ExtType::FunExtType(ty) => Ok(ExtType::FunExtType(FunExtType {
                pos: ty.pos,
                arg_tys: self.expand_all_constraints(ty.arg_tys, sub, stack)?,
                res_ty: Box::new(self.expand_constraints(*ty.res_ty, sub, stack)?),
            })),
            ExtType::AnyArityFunExtType(ty) => {
                Ok(ExtType::AnyArityFunExtType(AnyArityFunExtType {
                    pos: ty.pos,
                    res_ty: Box::new(self.expand_constraints(*ty.res_ty, sub, stack)?),
                }))
            }
            ExtType::TupleExtType(ty) => Ok(ExtType::TupleExtType(TupleExtType {
                pos: ty.pos,
                arg_tys: self.expand_all_constraints(ty.arg_tys, sub, stack)?,
            })),
            ExtType::ListExtType(ty) => Ok(ExtType::ListExtType(ListExtType {
                pos: ty.pos,
                t: Box::new(self.expand_constraints(*ty.t, sub, stack)?),
            })),
            ExtType::UnionExtType(ty) => Ok(ExtType::UnionExtType(UnionExtType {
                pos: ty.pos,
                tys: self.expand_all_constraints(ty.tys, sub, stack)?,
            })),
            ExtType::MapExtType(ty) => {
                if let Some(invalid_prop) = ty.props.iter().find(|prop| !prop.is_ok()) {
                    let pos = invalid_prop.pos().clone();
                    let invalid = Invalid::BadMapKey(BadMapKey {
                        pos,
                        required: invalid_prop.required(),
                    });
                    self.invalids.push(invalid);
                    Ok(ExtType::MapExtType(MapExtType {
                        pos: ty.pos.clone(),
                        props: vec![ExtProp::OptExtProp(OptExtProp {
                            pos: ty.pos.clone(),
                            key: ExtType::dynamic_ext_type(ty.pos.clone()),
                            tp: ExtType::dynamic_ext_type(ty.pos),
                        })],
                    }))
                } else {
                    let props = ty
                        .props
                        .into_iter()
                        .map(|prop| self.expand_prop_constraint(prop, sub, stack))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(ExtType::MapExtType(MapExtType { pos: ty.pos, props }))
                }
            }
            ExtType::RecordRefinedExtType(ty) => {
                Ok(ExtType::RecordRefinedExtType(RecordRefinedExtType {
                    pos: ty.pos,
                    name: ty.name,
                    refined_fields: ty
                        .refined_fields
                        .into_iter()
                        .map(|field| self.expand_refined_record_field_constraint(field, sub, stack))
                        .collect::<Result<Vec<_>, _>>()?,
                }))
            }
            ExtType::VarExtType(ty) => {
                if stack.contains(&ty.name) {
                    Err(Invalid::RecursiveConstraint(RecursiveConstraint {
                        pos: ty.pos,
                        n: ty.name.into(),
                    }))
                } else if let Some(tp) = sub.get(&ty.name) {
                    let mut stack2 = stack.clone();
                    stack2.insert(ty.name);
                    self.expand_constraints(tp.clone(), sub, &stack2)
                } else {
                    Ok(ExtType::VarExtType(ty))
                }
            }
            ExtType::BuiltinExtType(_)
            | ExtType::IntLitExtType(_)
            | ExtType::AtomLitExtType(_)
            | ExtType::RecordExtType(_)
            | ExtType::AnyMapExtType(_)
            | ExtType::UnOpType(_)
            | ExtType::BinOpType(_)
            | ExtType::AnyListExtType(_) => Ok(t),
        }
    }

    fn expand_prop_constraint(
        &mut self,
        prop: ExtProp,
        sub: &FxHashMap<StringId, ExtType>,
        stack: &FxHashSet<StringId>,
    ) -> Result<ExtProp, Invalid> {
        match prop {
            ExtProp::ReqExtProp(prop) => Ok(ExtProp::ReqExtProp(ReqExtProp {
                pos: prop.pos,
                key: self.expand_constraints(prop.key, sub, stack)?,
                tp: self.expand_constraints(prop.tp, sub, stack)?,
            })),
            ExtProp::OptExtProp(prop) => Ok(ExtProp::OptExtProp(OptExtProp {
                pos: prop.pos,
                key: self.expand_constraints(prop.key, sub, stack)?,
                tp: self.expand_constraints(prop.tp, sub, stack)?,
            })),
            prop => Err(Invalid::BadMapKey(BadMapKey {
                pos: prop.pos().clone(),
                required: prop.required(),
            })),
        }
    }

    fn expand_refined_record_field_constraint(
        &mut self,
        field: RefinedField,
        sub: &FxHashMap<StringId, ExtType>,
        stack: &FxHashSet<StringId>,
    ) -> Result<RefinedField, Invalid> {
        Ok(RefinedField {
            label: field.label,
            ty: self.expand_constraints(field.ty, sub, stack)?,
        })
    }

    fn expand_rec_field(&mut self, field: ExternalRecField) -> Result<ExternalRecField, Invalid> {
        let tp = {
            if let Some(tp) = field.tp {
                Some(self.expand_type(tp)?)
            } else {
                None
            }
        };
        Ok(ExternalRecField { tp, ..field })
    }
}

pub struct StubExpander<'d> {
    expander: Expander<'d>,
    type_converter: TypeConverter,
    pub stub: ModuleStub,
    from_beam: bool,
    module_file: StringId,
    current_file: StringId,
}

impl StubExpander<'_> {
    pub fn new<'d>(
        db: &'d dyn EqwalizerDiagnosticsDatabase,
        project_id: ProjectId,
        module: StringId,
        ast: &AST,
    ) -> StubExpander<'d> {
        let expander = Expander {
            module,
            invalids: vec![],
            db,
            project_id,
        };
        let type_converter = TypeConverter::new(module);
        let stub = ModuleStub {
            module,
            ..ModuleStub::default()
        };
        let module_file = ast
            .forms
            .iter()
            .find_map(|form| match form {
                ExternalForm::File(f) => Some(f.file),
                _ => None,
            })
            .unwrap();
        StubExpander {
            from_beam: ast.from_beam,
            expander,
            type_converter,
            stub,
            module_file,
            current_file: module_file,
        }
    }

    fn add_type_decl(&mut self, t: ExternalTypeDecl) -> Result<(), TypeConversionError> {
        match self.expander.expand_type_decl(t) {
            Ok(decl) => {
                let decl = self.type_converter.convert_type_decl(decl)?;
                self.stub.types.insert(decl.id.clone(), Arc::new(decl));
            }
            Err(invalid) => {
                if self.current_file == self.module_file {
                    self.stub.invalids.push(invalid);
                }
            }
        }
        Ok(())
    }

    fn add_opaque_decl(&mut self, t: ExternalOpaqueDecl) -> Result<(), TypeConversionError> {
        match self.expander.expand_opaque_decl(t) {
            Ok(decl) => {
                let opaque_decl = self.type_converter.convert_opaque_private(decl)?;
                self.stub
                    .opaques
                    .insert(opaque_decl.id.clone(), Arc::new(opaque_decl));
            }
            Err(invalid) => {
                if self.current_file == self.module_file {
                    self.stub.invalids.push(invalid);
                }
            }
        }
        Ok(())
    }

    fn add_record_decl(&mut self, t: ExternalRecDecl) -> Result<(), TypeConversionError> {
        match self.expander.expand_rec_decl(t) {
            Ok(decl) => match self.type_converter.convert_rec_decl(decl)? {
                Ok(decl) => {
                    self.stub.records.insert(decl.name, Arc::new(decl));
                }
                Err(invalid) => {
                    if self.current_file == self.module_file {
                        self.stub.invalids.push(invalid);
                    }
                }
            },
            Err(invalid) => {
                if self.current_file == self.module_file {
                    self.stub.invalids.push(invalid);
                }
            }
        }
        Ok(())
    }

    fn add_spec(&mut self, t: ExternalFunSpec) -> Result<(), TypeConversionError> {
        match self.expander.expand_fun_spec(t) {
            Ok(decl) => {
                if decl.types.len() == 1 {
                    let spec = self.type_converter.convert_spec(decl)?;
                    self.stub.specs.insert(spec.id.clone(), Arc::new(spec));
                } else {
                    let spec = self.type_converter.convert_overloaded_spec(decl)?;
                    self.stub
                        .overloaded_specs
                        .insert(spec.id.clone(), Arc::new(spec));
                }
            }
            Err(invalid) => {
                if self.current_file == self.module_file {
                    self.stub.invalids.push(invalid);
                }
            }
        }
        Ok(())
    }

    fn add_callback(&mut self, cb: ExternalCallback) -> Result<(), TypeConversionError> {
        match self.expander.expand_callback(cb) {
            Ok(cb) => {
                let cb = self.type_converter.convert_callback(cb)?;
                self.stub.callbacks.push(cb);
            }
            Err(invalid) => {
                if self.current_file == self.module_file {
                    self.stub.invalids.push(invalid);
                }
            }
        }
        Ok(())
    }

    fn add_extra_types(&mut self) {
        if let "erlang" = self.stub.module.as_str() {
            let pos: ast::Pos = {
                if self.from_beam {
                    elp_types_db::eqwalizer::LineAndColumn::fake().into()
                } else {
                    elp_types_db::eqwalizer::TextRange::fake().into()
                }
            };
            Type::builtin_type_aliases("erlang").for_each(|name| {
                let body = Type::builtin_type_alias_body(&name).unwrap();
                let id = ast::Id { name, arity: 0 };
                let decl = TypeDecl {
                    pos: pos.clone(),
                    id: id.clone(),
                    params: vec![],
                    body,
                };
                self.stub.types.insert(id, Arc::new(decl));
            })
        }
    }

    pub fn expand(&mut self, forms: &[ExternalForm]) -> Result<(), TypeConversionError> {
        for form in forms {
            match form {
                ExternalForm::File(f) => {
                    self.current_file = f.file;
                }
                ExternalForm::Export(e) => {
                    self.stub.exports.extend(e.funs.iter().cloned());
                }
                ExternalForm::Import(i) => {
                    self.stub
                        .imports
                        .extend(i.funs.iter().map(|id| (id.clone(), i.module)));
                }
                ExternalForm::ExportType(e) => {
                    self.stub.export_types.extend(e.types.iter().cloned());
                }
                ExternalForm::ExternalOptionalCallbacks(ocb) => {
                    self.stub.optional_callbacks.extend(ocb.ids.iter().cloned());
                }
                ExternalForm::ExternalTypeDecl(d) => self.add_type_decl(d.clone())?,
                ExternalForm::ExternalOpaqueDecl(d) => self.add_opaque_decl(d.clone())?,
                ExternalForm::ExternalFunSpec(s) => self.add_spec(s.clone())?,
                ExternalForm::ExternalRecDecl(r) => self.add_record_decl(r.clone())?,
                ExternalForm::ExternalCallback(cb) => self.add_callback(cb.clone())?,
                ExternalForm::Module(_)
                | ExternalForm::Behaviour(_)
                | ExternalForm::CompileExportAll(_)
                | ExternalForm::FunDecl(_)
                | ExternalForm::ElpMetadata(_)
                | ExternalForm::EqwalizerUnlimitedRefinement(_)
                | ExternalForm::EqwalizerNowarnFunction(_)
                | ExternalForm::TypingAttribute(_) => (),
            }
        }
        self.add_extra_types();
        self.stub.invalids.append(&mut self.expander.invalids);
        Ok(())
    }
}

pub struct CastExpander<'d> {
    expander: Expander<'d>,
    type_converter: TypeConverter,
}
impl CastExpander<'_> {
    pub fn new(
        db: &dyn EqwalizerDiagnosticsDatabase,
        project_id: ProjectId,
        module: StringId,
    ) -> CastExpander<'_> {
        let expander = Expander {
            module,
            invalids: vec![],
            db,
            project_id,
        };
        let type_converter = TypeConverter::new(module);
        CastExpander {
            expander,
            type_converter,
        }
    }

    fn refined_records(ty: &ExtType) -> BTreeSet<StringId> {
        let mut records = BTreeSet::default();
        let _ = ty.traverse::<()>(&mut |t| match t {
            ExtType::RecordRefinedExtType(r) => {
                records.insert(r.name);
                Ok(())
            }
            _ => Ok(()),
        });
        records
    }

    pub fn expand(&mut self, ty: ExtType) -> Result<Result<Type, Invalid>, TypeConversionError> {
        let result = self.expander.expand_type(ty);
        if let Some(invalid) = self.expander.invalids.first() {
            return Ok(Err(invalid.to_owned()));
        }
        match result {
            Ok(expanded_ty) => {
                let type_vars = expanded_ty.vars();
                if !type_vars.is_empty() {
                    let variables = type_vars.iter().map(|v| v.into()).collect();
                    let diag = Invalid::VariablesInTypeCast(VariablesInTypeCast::new(
                        expanded_ty.pos().to_owned(),
                        variables,
                    ));
                    return Ok(Err(diag));
                }
                let refined_records = Self::refined_records(&expanded_ty);
                if let Some(name) = refined_records.first() {
                    let diag = Invalid::RefinedRecordInTypeCast(RefinedRecordInTypeCast {
                        pos: expanded_ty.pos().to_owned(),
                        name: name.into(),
                    });
                    return Ok(Err(diag));
                }
                self.type_converter
                    .convert_type(&Default::default(), expanded_ty)
            }
            Err(invalid) => Ok(Err(invalid)),
        }
    }
}

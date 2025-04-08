/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;

use elp_types_db::eqwalizer::ext_types::ConstrainedFunType;
use elp_types_db::eqwalizer::ext_types::ExtProp;
use elp_types_db::eqwalizer::ext_types::ExtType;
use elp_types_db::eqwalizer::ext_types::FunExtType;
use elp_types_db::eqwalizer::form::Callback;
use elp_types_db::eqwalizer::form::ExternalCallback;
use elp_types_db::eqwalizer::form::ExternalFunSpec;
use elp_types_db::eqwalizer::form::ExternalOpaqueDecl;
use elp_types_db::eqwalizer::form::ExternalRecDecl;
use elp_types_db::eqwalizer::form::ExternalRecField;
use elp_types_db::eqwalizer::form::ExternalTypeDecl;
use elp_types_db::eqwalizer::form::FunSpec;
use elp_types_db::eqwalizer::form::OverloadedFunSpec;
use elp_types_db::eqwalizer::form::RecDecl;
use elp_types_db::eqwalizer::form::RecField;
use elp_types_db::eqwalizer::form::TypeDecl;
use elp_types_db::eqwalizer::invalid_diagnostics::Invalid;
use elp_types_db::eqwalizer::invalid_diagnostics::TypeVarInRecordField;
use elp_types_db::eqwalizer::types::AnyArityFunType;
use elp_types_db::eqwalizer::types::AtomLitType;
use elp_types_db::eqwalizer::types::FunType;
use elp_types_db::eqwalizer::types::Key;
use elp_types_db::eqwalizer::types::ListType;
use elp_types_db::eqwalizer::types::MapType;
use elp_types_db::eqwalizer::types::Prop;
use elp_types_db::eqwalizer::types::RecordType;
use elp_types_db::eqwalizer::types::RefinedRecordType;
use elp_types_db::eqwalizer::types::RemoteType;
use elp_types_db::eqwalizer::types::TupleType;
use elp_types_db::eqwalizer::types::Type;
use elp_types_db::eqwalizer::types::UnionType;
use elp_types_db::eqwalizer::types::VarType;
use elp_types_db::StringId;
use fxhash::FxHashMap;

use super::TypeConversionError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConverter {
    module: StringId,
    in_rec_decl: bool,
}

impl TypeConverter {
    pub fn new(module: StringId) -> Self {
        TypeConverter {
            module,
            in_rec_decl: false,
        }
    }

    fn enter_rec_decl(&self) -> Self {
        TypeConverter {
            module: self.module,
            in_rec_decl: true,
        }
    }

    pub fn convert_spec(&self, spec: ExternalFunSpec) -> Result<FunSpec, TypeConversionError> {
        let ty = self.convert_cft(spec.types.into_iter().next().unwrap())?;
        Ok(FunSpec {
            pos: spec.pos,
            id: spec.id,
            ty,
        })
    }

    pub fn convert_overloaded_spec(
        &self,
        spec: ExternalFunSpec,
    ) -> Result<OverloadedFunSpec, TypeConversionError> {
        let tys = spec
            .types
            .into_iter()
            .map(|ty| self.convert_cft(ty))
            .collect::<Result<_, _>>()?;
        Ok(OverloadedFunSpec {
            pos: spec.pos,
            id: spec.id,
            tys,
        })
    }

    pub fn convert_callback(&self, cb: ExternalCallback) -> Result<Callback, TypeConversionError> {
        let tys = cb
            .types
            .into_iter()
            .map(|ty| self.convert_cft(ty))
            .collect::<Result<_, _>>()?;
        Ok(Callback {
            pos: cb.pos,
            id: cb.id,
            tys,
        })
    }

    fn convert_cft(&self, cft: ConstrainedFunType) -> Result<FunType, TypeConversionError> {
        let var_names = self.collect_var_names_in_fun_type(&cft.ty);
        let subst = self.collect_substitution(&var_names);
        // Invalid declarations should only appear when converting record declarations
        let ft = self
            .convert_fun_type(&subst, cft.ty)?
            .map_err(TypeConversionError::ErrorInFunType)?;
        Ok(FunType {
            forall: subst.into_values().collect(),
            ..ft
        })
    }

    pub fn convert_rec_decl(
        &self,
        decl: ExternalRecDecl,
    ) -> Result<Result<RecDecl, Invalid>, TypeConversionError> {
        let new_context = self.enter_rec_decl();
        let result = decl
            .fields
            .into_iter()
            .map(|field| new_context.convert_rec_field(field))
            .collect::<Result<Result<Vec<_>, _>, _>>()?;
        match result {
            Ok(fields) => {
                let refinable = fields.iter().any(|field| field.refinable);
                Ok(Ok(RecDecl {
                    name: decl.name,
                    fields,
                    refinable,
                    pos: decl.pos,
                }))
            }
            Err(e) => Ok(Err(e)),
        }
    }

    fn is_refinable(&self, field: &ExternalRecField) -> bool {
        if let Some(ExtType::RemoteExtType(rt)) = &field.tp {
            rt.id.module == "eqwalizer" && rt.id.name == "refinable" && rt.id.arity == 1
        } else {
            false
        }
    }

    fn convert_rec_field(
        &self,
        field: ExternalRecField,
    ) -> Result<Result<RecField, Invalid>, TypeConversionError> {
        let refinable = self.is_refinable(&field);
        let tp = {
            if let Some(typ) = field.tp {
                match self.convert_type(&FxHashMap::default(), typ)? {
                    Ok(ty) => ty,
                    Err(invalid) => return Ok(Err(invalid)),
                }
            } else {
                Type::DynamicType
            }
        };
        Ok(Ok(RecField {
            name: field.name,
            tp,
            default_value: field.default_value,
            refinable,
        }))
    }

    pub fn convert_type_decl(
        &self,
        decl: ExternalTypeDecl,
    ) -> Result<TypeDecl, TypeConversionError> {
        let sub = self.collect_substitution(&decl.params);
        let params = self.collect_vars(&decl.params);
        let result = self.convert_type(&sub, decl.body)?;
        // Invalid declarations should only appear when converting record declarations
        let body = result.map_err(TypeConversionError::ErrorInTypeDecl)?;
        Ok(TypeDecl {
            id: decl.id,
            params,
            body,
            pos: decl.pos,
        })
    }

    pub fn convert_opaque_private(
        &self,
        decl: ExternalOpaqueDecl,
    ) -> Result<TypeDecl, TypeConversionError> {
        let sub = self.collect_substitution(&decl.params);
        let params = self.collect_vars(&decl.params);
        let result = self.convert_type(&sub, decl.body)?;
        // Invalid declarations should only appear when converting record declarations
        let body = result.map_err(TypeConversionError::ErrorInTypeDecl)?;
        Ok(TypeDecl {
            id: decl.id,
            params,
            body,
            pos: decl.pos,
        })
    }

    fn collect_substitution(&self, vars: &[StringId]) -> FxHashMap<StringId, u32> {
        vars.iter()
            .enumerate()
            .map(|(n, name)| (*name, n as u32))
            .collect()
    }

    fn collect_vars(&self, vars: &[StringId]) -> Vec<VarType> {
        vars.iter()
            .enumerate()
            .map(|(n, name)| VarType {
                n: n as u32,
                name: *name,
            })
            .collect()
    }

    fn convert_fun_type(
        &self,
        sub: &FxHashMap<StringId, u32>,
        ty: FunExtType,
    ) -> Result<Result<FunType, Invalid>, TypeConversionError> {
        let args_conv = self.convert_types(sub, ty.arg_tys)?;
        let res_conv = self.convert_type(sub, *ty.res_ty)?;
        Ok(args_conv.and_then(|arg_tys| {
            res_conv.map(|res_ty| FunType {
                forall: vec![],
                arg_tys,
                res_ty: Box::new(res_ty),
            })
        }))
    }

    fn convert_types(
        &self,
        sub: &FxHashMap<StringId, u32>,
        tys: Vec<ExtType>,
    ) -> Result<Result<Vec<Type>, Invalid>, TypeConversionError> {
        tys.into_iter()
            .map(|ty| self.convert_type(sub, ty))
            .collect::<Result<Result<Vec<_>, _>, _>>()
    }

    fn convert_type(
        &self,
        sub: &FxHashMap<StringId, u32>,
        ty: ExtType,
    ) -> Result<Result<Type, Invalid>, TypeConversionError> {
        match ty {
            ExtType::AtomLitExtType(atom) => {
                Ok(Ok(Type::AtomLitType(AtomLitType { atom: atom.atom })))
            }
            ExtType::FunExtType(ft) => self
                .convert_fun_type(sub, ft)
                .map(|res| res.map(Type::FunType)),
            ExtType::AnyArityFunExtType(ft) => {
                Ok(self.convert_type(sub, *ft.res_ty)?.map(|res_ty| {
                    Type::AnyArityFunType(AnyArityFunType {
                        res_ty: Box::new(res_ty),
                    })
                }))
            }
            ExtType::TupleExtType(tys) => Ok(self
                .convert_types(sub, tys.arg_tys)?
                .map(|arg_tys| Type::TupleType(TupleType { arg_tys }))),
            ExtType::ListExtType(ty) => Ok(self
                .convert_type(sub, *ty.t)?
                .map(|t| Type::ListType(ListType { t: Box::new(t) }))),
            ExtType::UnionExtType(tys) => Ok(self
                .convert_types(sub, tys.tys)?
                .map(|tys| Type::UnionType(UnionType { tys }))),
            ExtType::RemoteExtType(ty)
                if ty.id.module == "eqwalizer"
                    && ty.id.name == "refinable"
                    && ty.id.arity == 1
                    && ty.args.len() == 1 =>
            {
                self.convert_type(sub, ty.args.into_iter().next().unwrap())
            }
            ExtType::RemoteExtType(ty) => Ok(self
                .convert_types(sub, ty.args)?
                .map(|arg_tys| Type::RemoteType(RemoteType { id: ty.id, arg_tys }))),
            ExtType::VarExtType(var) => match sub.get(&var.name) {
                Some(id) => Ok(Ok(Type::VarType(VarType {
                    n: *id,
                    name: var.name,
                }))),
                None if self.in_rec_decl => {
                    Ok(Err(Invalid::TypeVarInRecordField(TypeVarInRecordField {
                        pos: var.pos,
                        name: var.name.into(),
                    })))
                }
                None => Err(TypeConversionError::UnexpectedVariable(var.name.into())),
            },
            ExtType::RecordExtType(ty) => Ok(Ok(Type::RecordType(RecordType {
                name: ty.name,
                module: self.module,
            }))),
            ExtType::RecordRefinedExtType(ty) => {
                let rec_type = RecordType {
                    name: ty.name,
                    module: self.module,
                };
                let fields = ty
                    .refined_fields
                    .into_iter()
                    .map(|field| {
                        self.convert_type(sub, field.ty)
                            .map(|res| res.map(|map| (field.label, map)))
                    })
                    .collect::<Result<Result<_, _>, _>>()?;
                Ok(fields
                    .map(|fields| Type::RefinedRecordType(RefinedRecordType { rec_type, fields })))
            }
            ExtType::MapExtType(ty) => {
                let mut props: BTreeMap<Key, Prop> = BTreeMap::default();
                let mut k_type = Type::NoneType;
                let mut v_type = Type::NoneType;
                // .rev() so that the leftmost association takes precedence in the props list
                for prop in ty.props.into_iter().rev() {
                    if prop.key().is_key() {
                        match self.to_shape_prop(sub, prop)? {
                            Ok((key, tp)) => props.insert(key, tp),
                            Err(e) => return Ok(Err(e)),
                        };
                    } else {
                        let (k_prop, v_prop) = prop.to_pair();
                        match self.convert_type(sub, k_prop)? {
                            Ok(t) => k_type = t,
                            Err(e) => return Ok(Err(e)),
                        }
                        match self.convert_type(sub, v_prop)? {
                            Ok(t) => v_type = t,
                            Err(e) => return Ok(Err(e)),
                        }
                    }
                }
                Ok(Ok(Type::MapType(MapType {
                    props,
                    k_type: Box::new(k_type),
                    v_type: Box::new(v_type),
                })))
            }
            ExtType::BuiltinExtType(ty) => Ok(Ok(Type::builtin_type(ty.name.as_str())
                .ok_or(TypeConversionError::UnknownBuiltin(ty.name.into(), 0))?)),
            ExtType::IntLitExtType(_) => Ok(Ok(Type::NumberType)),
            ExtType::UnOpType(_) | ExtType::BinOpType(_) => Ok(Ok(Type::NumberType)),
            ExtType::LocalExtType(_) | ExtType::AnyMapExtType(_) | ExtType::AnyListExtType(_) => {
                Err(TypeConversionError::UnexpectedType)
            }
        }
    }

    fn to_shape_prop(
        &self,
        sub: &FxHashMap<StringId, u32>,
        prop: ExtProp,
    ) -> Result<Result<(Key, Prop), Invalid>, TypeConversionError> {
        let req = prop.required();
        let (key, tp) = prop.to_pair();
        let converted_key = match self.convert_type(sub, key)?.map(Key::from_type) {
            Ok(Some(k)) => k,
            Ok(None) => return Err(TypeConversionError::UnexpectedShapeProp),
            Err(invalid) => return Ok(Err(invalid)),
        };
        let converted_tp = match self.convert_type(sub, tp)? {
            Ok(tp) => tp,
            Err(invalid) => return Ok(Err(invalid)),
        };
        Ok(Ok((
            converted_key,
            Prop {
                req,
                tp: converted_tp,
            },
        )))
    }

    fn collect_var_names_in_fun_type(&self, ty: &FunExtType) -> Vec<StringId> {
        [
            self.collect_var_names(&ty.res_ty),
            self.collect_all_var_names(&ty.arg_tys),
        ]
        .concat()
    }

    fn collect_var_names(&self, ty: &ExtType) -> Vec<StringId> {
        match ty {
            ExtType::VarExtType(var) => vec![var.name],
            ExtType::FunExtType(ft) => self.collect_var_names_in_fun_type(ft),
            ExtType::AnyArityFunExtType(ft) => self.collect_var_names(&ft.res_ty),
            ExtType::TupleExtType(ty) => self.collect_all_var_names(ty.arg_tys.as_ref()),
            ExtType::UnionExtType(ty) => self.collect_all_var_names(ty.tys.as_ref()),
            ExtType::LocalExtType(ty) => self.collect_all_var_names(ty.args.as_ref()),
            ExtType::RemoteExtType(ty) => self.collect_all_var_names(ty.args.as_ref()),
            ExtType::RecordRefinedExtType(ty) => ty
                .refined_fields
                .iter()
                .flat_map(|field| self.collect_var_names(&field.ty))
                .collect(),
            ExtType::MapExtType(ty) => ty
                .props
                .iter()
                .flat_map(|prop| {
                    [
                        self.collect_var_names(prop.key()),
                        self.collect_var_names(prop.tp()),
                    ]
                    .concat()
                })
                .collect(),
            ExtType::ListExtType(ty) => self.collect_var_names(&ty.t),
            ExtType::AtomLitExtType(_)
            | ExtType::RecordExtType(_)
            | ExtType::BuiltinExtType(_)
            | ExtType::IntLitExtType(_)
            | ExtType::AnyMapExtType(_)
            | ExtType::UnOpType(_)
            | ExtType::BinOpType(_)
            | ExtType::AnyListExtType(_) => vec![],
        }
    }

    fn collect_all_var_names(&self, tys: &[ExtType]) -> Vec<StringId> {
        tys.iter()
            .flat_map(|ty| self.collect_var_names(ty))
            .collect()
    }
}

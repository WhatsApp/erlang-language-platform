/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use serde::Deserialize;
use serde::Serialize;

use crate::eqwalizer;
use crate::StringId;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum ExtType {
    AtomLitExtType(AtomLitExtType),
    FunExtType(FunExtType),
    AnyArityFunExtType(AnyArityFunExtType),
    TupleExtType(TupleExtType),
    ListExtType(ListExtType),
    AnyListExtType(AnyListExtType),
    UnionExtType(UnionExtType),
    LocalExtType(LocalExtType),
    RemoteExtType(RemoteExtType),
    BuiltinExtType(BuiltinExtType),
    IntLitExtType(IntLitExtType),
    UnOpType(UnOpType),
    BinOpType(BinOpType),
    VarExtType(VarExtType),
    RecordExtType(RecordExtType),
    RecordRefinedExtType(RecordRefinedExtType),
    MapExtType(MapExtType),
    AnyMapExtType(AnyMapExtType),
}
impl ExtType {
    pub fn int_ext_type(location: eqwalizer::Pos) -> ExtType {
        ExtType::BuiltinExtType(BuiltinExtType {
            location,
            name: "integer".into(),
        })
    }

    pub fn any_ext_type(location: eqwalizer::Pos) -> ExtType {
        ExtType::BuiltinExtType(BuiltinExtType {
            location,
            name: "any".into(),
        })
    }

    pub fn char_ext_type(location: eqwalizer::Pos) -> ExtType {
        ExtType::BuiltinExtType(BuiltinExtType {
            location,
            name: "char".into(),
        })
    }

    pub fn tuple_ext_type(location: eqwalizer::Pos) -> ExtType {
        ExtType::BuiltinExtType(BuiltinExtType {
            location,
            name: "tuple".into(),
        })
    }

    pub fn binary_ext_type(location: eqwalizer::Pos) -> ExtType {
        ExtType::BuiltinExtType(BuiltinExtType {
            location,
            name: "binary".into(),
        })
    }

    pub fn dynamic_ext_type(location: eqwalizer::Pos) -> ExtType {
        ExtType::BuiltinExtType(BuiltinExtType {
            location,
            name: "dynamic".into(),
        })
    }

    pub fn walk<'a, T>(&'a self, f: &mut dyn FnMut(&'a ExtType) -> Result<(), T>) -> Result<(), T> {
        match self {
            ExtType::FunExtType(ty) => {
                f(&ty.res_ty).and_then(|()| ty.arg_tys.iter().try_for_each(f))
            }
            ExtType::AnyArityFunExtType(ty) => f(&ty.res_ty),
            ExtType::TupleExtType(ty) => ty.arg_tys.iter().try_for_each(f),
            ExtType::UnionExtType(ty) => ty.tys.iter().try_for_each(f),
            ExtType::MapExtType(ty) => ty
                .props
                .iter()
                .try_for_each(|prop| f(prop.key()).and_then(|()| f(prop.tp()))),
            ExtType::ListExtType(ty) => f(&ty.t),
            ExtType::RecordRefinedExtType(ty) => {
                ty.refined_fields.iter().try_for_each(|field| f(&field.ty))
            }
            ExtType::RemoteExtType(ty) => ty.args.iter().try_for_each(f),
            ExtType::AtomLitExtType(_)
            | ExtType::VarExtType(_)
            | ExtType::RecordExtType(_)
            | ExtType::AnyMapExtType(_)
            | ExtType::LocalExtType(_)
            | ExtType::BuiltinExtType(_)
            | ExtType::IntLitExtType(_)
            | ExtType::UnOpType(_)
            | ExtType::BinOpType(_)
            | ExtType::AnyListExtType(_) => Ok(()),
        }
    }

    pub fn traverse<T>(&self, f: &mut dyn FnMut(&ExtType) -> Result<(), T>) -> Result<(), T> {
        f(self)?;
        self.walk(&mut |ty| ty.traverse(f))
    }

    pub fn is_key(&self) -> bool {
        match self {
            ExtType::AtomLitExtType(_) => true,
            ExtType::TupleExtType(TupleExtType { arg_tys, .. }) => {
                arg_tys.iter().all(|t| t.is_key())
            }
            _ => false,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AtomLitExtType {
    pub location: eqwalizer::Pos,
    pub atom: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FunExtType {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub arg_tys: Vec<ExtType>,
    pub res_ty: Box<ExtType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AnyArityFunExtType {
    pub location: eqwalizer::Pos,
    pub res_ty: Box<ExtType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TupleExtType {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub arg_tys: Vec<ExtType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ListExtType {
    pub location: eqwalizer::Pos,
    pub t: Box<ExtType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AnyListExtType {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UnionExtType {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub tys: Vec<ExtType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct LocalExtType {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub args: Vec<ExtType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RemoteExtType {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::RemoteId,
    #[serde(default)]
    pub args: Vec<ExtType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BuiltinExtType {
    pub location: eqwalizer::Pos,
    pub name: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct IntLitExtType {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UnOpType {
    pub location: eqwalizer::Pos,
    pub op: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BinOpType {
    pub location: eqwalizer::Pos,
    pub op: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct VarExtType {
    pub location: eqwalizer::Pos,
    pub name: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordExtType {
    pub location: eqwalizer::Pos,
    pub name: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordRefinedExtType {
    pub location: eqwalizer::Pos,
    pub name: StringId,
    #[serde(default)]
    pub refined_fields: Vec<RefinedField>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MapExtType {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub props: Vec<ExtProp>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AnyMapExtType {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ConstrainedFunType {
    pub location: eqwalizer::Pos,
    pub ty: FunExtType,
    #[serde(default)]
    pub constraints: Vec<Constraint>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Constraint {
    pub location: eqwalizer::Pos,
    pub t_var: StringId,
    pub ty: ExtType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RefinedField {
    pub label: StringId,
    pub ty: ExtType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum ExtProp {
    ReqExtProp(ReqExtProp),
    ReqBadExtProp(ReqBadExtProp),
    OptExtProp(OptExtProp),
    OptBadExtProp(OptBadExtProp),
}
impl ExtProp {
    pub fn key(&self) -> &ExtType {
        match self {
            ExtProp::ReqExtProp(p) => &p.key,
            ExtProp::ReqBadExtProp(p) => &p.key,
            ExtProp::OptExtProp(p) => &p.key,
            ExtProp::OptBadExtProp(p) => &p.key,
        }
    }

    pub fn tp(&self) -> &ExtType {
        match self {
            ExtProp::ReqExtProp(p) => &p.tp,
            ExtProp::ReqBadExtProp(p) => &p.tp,
            ExtProp::OptExtProp(p) => &p.tp,
            ExtProp::OptBadExtProp(p) => &p.tp,
        }
    }

    pub fn to_pair(self) -> (ExtType, ExtType) {
        match self {
            ExtProp::ReqExtProp(p) => (p.key, p.tp),
            ExtProp::ReqBadExtProp(p) => (p.key, p.tp),
            ExtProp::OptExtProp(p) => (p.key, p.tp),
            ExtProp::OptBadExtProp(p) => (p.key, p.tp),
        }
    }

    pub fn is_ok(&self) -> bool {
        match self {
            ExtProp::ReqExtProp(_) | ExtProp::OptExtProp(_) => true,
            ExtProp::ReqBadExtProp(_) | ExtProp::OptBadExtProp(_) => false,
        }
    }

    pub fn location(&self) -> &eqwalizer::Pos {
        match self {
            ExtProp::ReqExtProp(p) => &p.location,
            ExtProp::ReqBadExtProp(p) => &p.location,
            ExtProp::OptExtProp(p) => &p.location,
            ExtProp::OptBadExtProp(p) => &p.location,
        }
    }

    pub fn required(&self) -> bool {
        match self {
            ExtProp::ReqExtProp(_) | ExtProp::ReqBadExtProp(_) => true,
            ExtProp::OptExtProp(_) | ExtProp::OptBadExtProp(_) => false,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ReqExtProp {
    pub location: eqwalizer::Pos,
    pub key: ExtType,
    pub tp: ExtType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ReqBadExtProp {
    pub location: eqwalizer::Pos,
    pub key: ExtType,
    pub tp: ExtType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct OptExtProp {
    pub location: eqwalizer::Pos,
    pub key: ExtType,
    pub tp: ExtType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct OptBadExtProp {
    pub location: eqwalizer::Pos,
    pub key: ExtType,
    pub tp: ExtType,
}

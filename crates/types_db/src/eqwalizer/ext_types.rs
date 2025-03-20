/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_syntax::SmolStr;
use serde::Deserialize;
use serde::Serialize;

use crate::eqwalizer;

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
    pub fn int_ext_type(pos: eqwalizer::Pos) -> ExtType {
        ExtType::BuiltinExtType(BuiltinExtType {
            pos,
            name: "integer".into(),
        })
    }

    pub fn any_ext_type(pos: eqwalizer::Pos) -> ExtType {
        ExtType::BuiltinExtType(BuiltinExtType {
            pos,
            name: "any".into(),
        })
    }

    pub fn char_ext_type(pos: eqwalizer::Pos) -> ExtType {
        ExtType::BuiltinExtType(BuiltinExtType {
            pos,
            name: "char".into(),
        })
    }

    pub fn tuple_ext_type(pos: eqwalizer::Pos) -> ExtType {
        ExtType::BuiltinExtType(BuiltinExtType {
            pos,
            name: "tuple".into(),
        })
    }

    pub fn binary_ext_type(pos: eqwalizer::Pos) -> ExtType {
        ExtType::BuiltinExtType(BuiltinExtType {
            pos,
            name: "binary".into(),
        })
    }

    pub fn dynamic_ext_type(pos: eqwalizer::Pos) -> ExtType {
        ExtType::BuiltinExtType(BuiltinExtType {
            pos,
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
    pub pos: eqwalizer::Pos,
    pub atom: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FunExtType {
    pub pos: eqwalizer::Pos,
    #[serde(default)]
    pub arg_tys: Vec<ExtType>,
    pub res_ty: Box<ExtType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AnyArityFunExtType {
    pub pos: eqwalizer::Pos,
    pub res_ty: Box<ExtType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TupleExtType {
    pub pos: eqwalizer::Pos,
    #[serde(default)]
    pub arg_tys: Vec<ExtType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ListExtType {
    pub pos: eqwalizer::Pos,
    pub t: Box<ExtType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AnyListExtType {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UnionExtType {
    pub pos: eqwalizer::Pos,
    #[serde(default)]
    pub tys: Vec<ExtType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct LocalExtType {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub args: Vec<ExtType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RemoteExtType {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::RemoteId,
    #[serde(default)]
    pub args: Vec<ExtType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BuiltinExtType {
    pub pos: eqwalizer::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct IntLitExtType {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UnOpType {
    pub pos: eqwalizer::Pos,
    pub op: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BinOpType {
    pub pos: eqwalizer::Pos,
    pub op: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct VarExtType {
    pub pos: eqwalizer::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordExtType {
    pub pos: eqwalizer::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordRefinedExtType {
    pub pos: eqwalizer::Pos,
    pub name: SmolStr,
    #[serde(default)]
    pub refined_fields: Vec<RefinedField>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MapExtType {
    pub pos: eqwalizer::Pos,
    #[serde(default)]
    pub props: Vec<ExtProp>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AnyMapExtType {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ConstrainedFunType {
    pub pos: eqwalizer::Pos,
    pub ty: FunExtType,
    #[serde(default)]
    pub constraints: Vec<Constraint>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Constraint {
    pub pos: eqwalizer::Pos,
    pub t_var: SmolStr,
    pub ty: ExtType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RefinedField {
    pub label: SmolStr,
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

    pub fn pos(&self) -> &eqwalizer::Pos {
        match self {
            ExtProp::ReqExtProp(p) => &p.pos,
            ExtProp::ReqBadExtProp(p) => &p.pos,
            ExtProp::OptExtProp(p) => &p.pos,
            ExtProp::OptBadExtProp(p) => &p.pos,
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
    pub pos: eqwalizer::Pos,
    pub key: ExtType,
    pub tp: ExtType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ReqBadExtProp {
    pub pos: eqwalizer::Pos,
    pub key: ExtType,
    pub tp: ExtType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct OptExtProp {
    pub pos: eqwalizer::Pos,
    pub key: ExtType,
    pub tp: ExtType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct OptBadExtProp {
    pub pos: eqwalizer::Pos,
    pub key: ExtType,
    pub tp: ExtType,
}

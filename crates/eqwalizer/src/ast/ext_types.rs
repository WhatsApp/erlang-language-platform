/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_syntax::SmolStr;
use serde::Serialize;

use super::RemoteId;
use crate::ast;

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
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
    pub fn int_ext_type(location: ast::Pos) -> ExtType {
        return ExtType::BuiltinExtType(BuiltinExtType {
            location,
            name: "integer".into(),
        });
    }

    pub fn any_ext_type(location: ast::Pos) -> ExtType {
        return ExtType::BuiltinExtType(BuiltinExtType {
            location,
            name: "any".into(),
        });
    }

    pub fn char_ext_type(location: ast::Pos) -> ExtType {
        return ExtType::BuiltinExtType(BuiltinExtType {
            location,
            name: "char".into(),
        });
    }

    pub fn tuple_ext_type(location: ast::Pos) -> ExtType {
        return ExtType::BuiltinExtType(BuiltinExtType {
            location,
            name: "tuple".into(),
        });
    }

    pub fn binary_ext_type(location: ast::Pos) -> ExtType {
        return ExtType::BuiltinExtType(BuiltinExtType {
            location,
            name: "binary".into(),
        });
    }

    pub fn eqwalizer_dynamic(location: ast::Pos) -> ExtType {
        let id = RemoteId {
            module: "eqwalizer".into(),
            name: "dynamic".into(),
            arity: 0,
        };
        return ExtType::RemoteExtType(RemoteExtType {
            location,
            id,
            args: vec![],
        });
    }

    pub fn visit<T>(&self, f: &dyn Fn(&ExtType) -> Result<(), T>) -> Result<(), T> {
        f(self)?;
        match self {
            ExtType::FunExtType(ty) => ty
                .res_ty
                .visit(f)
                .and_then(|()| ty.arg_tys.iter().map(|ty| ty.visit(f)).collect()),
            ExtType::AnyArityFunExtType(ty) => ty.res_ty.visit(f),
            ExtType::TupleExtType(ty) => ty.arg_tys.iter().map(|ty| ty.visit(f)).collect(),
            ExtType::UnionExtType(ty) => ty.tys.iter().map(|ty| ty.visit(f)).collect(),
            ExtType::MapExtType(ty) => ty
                .props
                .iter()
                .map(|prop| prop.key().visit(f).and_then(|()| prop.tp().visit(f)))
                .collect(),
            ExtType::ListExtType(ty) => ty.t.visit(f),
            ExtType::RecordRefinedExtType(ty) => ty
                .refined_fields
                .iter()
                .map(|field| field.ty.visit(f))
                .collect(),
            ExtType::RemoteExtType(ty) => ty.args.iter().map(|ty| ty.visit(f)).collect(),
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
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct AtomLitExtType {
    pub location: ast::Pos,
    pub atom: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct FunExtType {
    pub location: ast::Pos,
    pub arg_tys: Vec<ExtType>,
    pub res_ty: Box<ExtType>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct AnyArityFunExtType {
    pub location: ast::Pos,
    pub res_ty: Box<ExtType>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TupleExtType {
    pub location: ast::Pos,
    pub arg_tys: Vec<ExtType>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ListExtType {
    pub location: ast::Pos,
    pub t: Box<ExtType>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct AnyListExtType {
    pub location: ast::Pos,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct UnionExtType {
    pub location: ast::Pos,
    pub tys: Vec<ExtType>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct LocalExtType {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub args: Vec<ExtType>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RemoteExtType {
    pub location: ast::Pos,
    pub id: ast::RemoteId,
    pub args: Vec<ExtType>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct BuiltinExtType {
    pub location: ast::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct IntLitExtType {
    pub location: ast::Pos,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct UnOpType {
    pub location: ast::Pos,
    pub op: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct BinOpType {
    pub location: ast::Pos,
    pub op: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct VarExtType {
    pub location: ast::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordExtType {
    pub location: ast::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordRefinedExtType {
    pub location: ast::Pos,
    pub name: SmolStr,
    pub refined_fields: Vec<RefinedField>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct MapExtType {
    pub location: ast::Pos,
    pub props: Vec<ExtProp>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct AnyMapExtType {
    pub location: ast::Pos,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ConstrainedFunType {
    pub location: ast::Pos,
    pub ty: FunExtType,
    pub constraints: Vec<Constraint>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Constraint {
    pub location: ast::Pos,
    pub t_var: SmolStr,
    pub ty: ExtType,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RefinedField {
    pub label: SmolStr,
    pub ty: ExtType,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
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
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ReqExtProp {
    pub location: ast::Pos,
    pub key: ExtType,
    pub tp: ExtType,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ReqBadExtProp {
    pub location: ast::Pos,
    pub key: ExtType,
    pub tp: ExtType,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct OptExtProp {
    pub location: ast::Pos,
    pub key: ExtType,
    pub tp: ExtType,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct OptBadExtProp {
    pub location: ast::Pos,
    pub key: ExtType,
    pub tp: ExtType,
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_syntax::SmolStr;
use fxhash::FxHashMap;
use serde::Serialize;

use crate::ast;

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub enum Type {
    AtomLitType(AtomLitType),
    AnyFunType,
    FunType(FunType),
    AnyArityFunType(AnyArityFunType),
    AnyTupleType,
    TupleType(TupleType),
    NilType,
    ListType(ListType),
    UnionType(UnionType),
    RemoteType(RemoteType),
    OpaqueType(OpaqueType),
    VarType(VarType),
    RecordType(RecordType),
    RefinedRecordType(RefinedRecordType),
    DictMap(DictMap),
    ShapeMap(ShapeMap),
    BinaryType,
    AnyType,
    AtomType,
    DynamicType,
    NoneType,
    PidType,
    PortType,
    ReferenceType,
    NumberType,
}
impl Type {
    pub const fn atom_lit_type(lit: SmolStr) -> Type {
        return Type::AtomLitType(AtomLitType { atom: lit });
    }

    pub const FALSE_TYPE: Type = Type::atom_lit_type(SmolStr::new_inline("false"));

    pub const TRUE_TYPE: Type = Type::atom_lit_type(SmolStr::new_inline("true"));

    pub const CHAR_TYPE: Type = Type::NumberType;

    pub const BYTE_TYPE: Type = Type::NumberType;

    pub const FLOAT_TYPE: Type = Type::NumberType;

    pub const UNDEFINED: Type = Type::atom_lit_type(SmolStr::new_inline("undefined"));

    pub fn exn_class_type() -> Type {
        return Type::UnionType(UnionType {
            tys: vec![
                Type::atom_lit_type(SmolStr::new_inline("error")),
                Type::atom_lit_type(SmolStr::new_inline("exit")),
                Type::atom_lit_type(SmolStr::new_inline("throw")),
            ],
        });
    }

    pub fn cls_exn_stack_type() -> Type {
        return Type::UnionType(UnionType {
            tys: vec![
                Type::exn_class_type(),
                Type::AnyType,
                Type::ListType(ListType {
                    t: Box::new(Type::AnyType),
                }),
            ],
        });
    }

    pub fn cls_exn_stack_type_dynamic() -> Type {
        return Type::UnionType(UnionType {
            tys: vec![
                Type::exn_class_type(),
                Type::DynamicType,
                Type::ListType(ListType {
                    t: Box::new(Type::DynamicType),
                }),
            ],
        });
    }

    pub fn builtin_type_aliases(module: &str) -> Vec<SmolStr> {
        match module {
            "erlang" => vec![
                "string".into(),
                "boolean".into(),
                "timeout".into(),
                "identifier".into(),
                "mfa".into(),
                "iolist".into(),
                "iodata".into(),
            ],
            _ => vec![],
        }
    }

    pub fn builtin_type_alias(name: &str) -> Option<RemoteType> {
        match name {
            "string" | "boolean" | "timeout" | "identifier" | "mfa" | "iolist" | "iodata" => {
                let id = ast::RemoteId {
                    module: "erlang".into(),
                    name: name.into(),
                    arity: 0,
                };
                return Some(RemoteType {
                    id,
                    arg_tys: vec![],
                });
            }
            _ => {
                return None;
            }
        }
    }

    pub fn builtin_type_alias_body(name: &str) -> Option<Type> {
        match name {
            "string" => Some(Type::ListType(ListType {
                t: Box::new(Type::CHAR_TYPE),
            })),
            "boolean" => Some(Type::UnionType(UnionType {
                tys: vec![Type::FALSE_TYPE, Type::TRUE_TYPE],
            })),
            "timeout" => Some(Type::UnionType(UnionType {
                tys: vec![
                    Type::AtomLitType(AtomLitType {
                        atom: "infinity".into(),
                    }),
                    Type::NumberType,
                ],
            })),
            "identifier" => Some(Type::UnionType(UnionType {
                tys: vec![Type::PidType, Type::PortType, Type::ReferenceType],
            })),
            "mfa" => Some(Type::TupleType(TupleType {
                arg_tys: vec![Type::AtomType, Type::AtomType, Type::NumberType],
            })),
            "iolist" => Some(Type::ListType(ListType {
                t: Box::new(Type::UnionType(UnionType {
                    tys: vec![
                        Type::BYTE_TYPE,
                        Type::BinaryType,
                        Type::RemoteType(Type::builtin_type_alias("iolist").unwrap()),
                    ],
                })),
            })),
            "iodata" => Some(Type::UnionType(UnionType {
                tys: vec![
                    Type::RemoteType(Type::builtin_type_alias("iolist").unwrap()),
                    Type::BinaryType,
                ],
            })),
            _ => None,
        }
    }

    pub fn string_type() -> Type {
        return Type::RemoteType(Type::builtin_type_alias("string").unwrap());
    }

    pub fn boolean_type() -> Type {
        return Type::RemoteType(Type::builtin_type_alias("boolean").unwrap());
    }

    pub fn builtin_type(name: &str) -> Option<Type> {
        match name {
            "any" | "term" => {
                return Some(Type::AnyType);
            }
            "atom" | "module" | "node" => {
                return Some(Type::AtomType);
            }
            "binary" | "bitstring" | "nonempty_binary" | "nonempty_bitstring" => {
                return Some(Type::BinaryType);
            }
            "byte" => {
                return Some(Type::BYTE_TYPE);
            }
            "char" => {
                return Some(Type::CHAR_TYPE);
            }
            "float" => {
                return Some(Type::FLOAT_TYPE);
            }
            "fun" | "function" => {
                return Some(Type::AnyFunType);
            }
            "maybe_improper_list" | "nonempty_maybe_improper_list" => {
                return Some(Type::ListType(ListType {
                    t: Box::new(Type::AnyType),
                }));
            }
            "pos_integer" | "neg_integer" | "non_neg_integer" | "integer" | "number" | "arity" => {
                return Some(Type::NumberType);
            }
            "nil" => {
                return Some(Type::NilType);
            }
            "none" | "no_return" => {
                return Some(Type::NoneType);
            }
            "pid" => {
                return Some(Type::PidType);
            }
            "port" => {
                return Some(Type::PortType);
            }
            "reference" => {
                return Some(Type::ReferenceType);
            }
            "tuple" => {
                return Some(Type::AnyTupleType);
            }
            "nonempty_string" => {
                return Some(Type::string_type());
            }
            "dynamic" => {
                return Some(Type::DynamicType);
            }
            _ => {
                return Type::builtin_type_alias(name).map(|rt| Type::RemoteType(rt));
            }
        }
    }

    pub fn visit_children<T>(&self, f: &mut dyn FnMut(&Type) -> Result<(), T>) -> Result<(), T> {
        match self {
            Type::FunType(ty) => {
                f(&ty.res_ty).and_then(|()| ty.arg_tys.iter().map(|ty| f(ty)).collect())
            }
            Type::AnyArityFunType(ty) => f(&ty.res_ty),
            Type::TupleType(ty) => ty.arg_tys.iter().map(|ty| f(ty)).collect(),
            Type::UnionType(ty) => ty.tys.iter().map(|ty| f(ty)).collect(),
            Type::RemoteType(ty) => ty.arg_tys.iter().map(|ty| f(ty)).collect(),
            Type::OpaqueType(ty) => ty.arg_tys.iter().map(|ty| f(ty)).collect(),
            Type::ShapeMap(ty) => ty.props.iter().map(|prop| f(prop.tp())).collect(),
            Type::DictMap(ty) => f(&ty.v_type).and_then(|()| f(&ty.k_type)),
            Type::ListType(ty) => f(&ty.t),
            Type::RefinedRecordType(ty) => ty.fields.iter().map(|(_, ty)| f(ty)).collect(),
            Type::AtomLitType(_)
            | Type::AnyType
            | Type::AnyFunType
            | Type::AnyTupleType
            | Type::AtomType
            | Type::NilType
            | Type::RecordType(_)
            | Type::VarType(_)
            | Type::BinaryType
            | Type::NoneType
            | Type::DynamicType
            | Type::PidType
            | Type::PortType
            | Type::ReferenceType
            | Type::NumberType => Ok(()),
        }
    }

    pub fn traverse<T>(&self, f: &mut dyn FnMut(&Type) -> Result<(), T>) -> Result<(), T> {
        f(self)?;
        self.visit_children(&mut |ty| ty.traverse(f))
    }
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct AtomLitType {
    pub atom: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct FunType {
    pub forall: Vec<u32>,
    pub arg_tys: Vec<Type>,
    pub res_ty: Box<Type>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct AnyArityFunType {
    pub res_ty: Box<Type>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TupleType {
    pub arg_tys: Vec<Type>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ListType {
    pub t: Box<Type>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct UnionType {
    pub tys: Vec<Type>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RemoteType {
    pub id: ast::RemoteId,
    pub arg_tys: Vec<Type>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct OpaqueType {
    pub id: ast::RemoteId,
    pub arg_tys: Vec<Type>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct VarType {
    pub n: u32,
    pub name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordType {
    pub name: SmolStr,
    pub module: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RefinedRecordType {
    pub rec_type: RecordType,
    pub fields: FxHashMap<SmolStr, Type>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct DictMap {
    pub k_type: Box<Type>,
    pub v_type: Box<Type>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ShapeMap {
    pub props: Vec<Prop>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub enum Prop {
    ReqProp(ReqProp),
    OptProp(OptProp),
}
impl Prop {
    pub fn key(&self) -> &SmolStr {
        match self {
            Prop::ReqProp(req) => &req.key,
            Prop::OptProp(opt) => &opt.key,
        }
    }

    pub fn tp(&self) -> &Type {
        match self {
            Prop::ReqProp(req) => &req.tp,
            Prop::OptProp(opt) => &opt.tp,
        }
    }
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ReqProp {
    pub key: SmolStr,
    pub tp: Type,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct OptProp {
    pub key: SmolStr,
    pub tp: Type,
}

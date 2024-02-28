/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Types specific to Eqwalizer.

use std::fmt;

use elp_syntax::SmolStr;
use elp_syntax::TextRange;
use fxhash::FxHashMap;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Deserialize, PartialEq, Eq, Clone)]
#[serde(rename_all = "camelCase")]
pub struct EqwalizerDiagnostic {
    #[serde(deserialize_with = "deserialize_text_range")]
    pub range: TextRange,
    pub message: String,
    pub uri: String,
    pub code: String,
    #[serde(rename(deserialize = "expressionOrNull"))]
    pub expression: Option<String>,
    #[serde(rename(deserialize = "explanationOrNull"))]
    pub explanation: Option<String>,
}

impl EqwalizerDiagnostic {
    pub fn expr_string(&self) -> String {
        match &self.expression {
            Some(s) => format!("`{}`.\n", s),
            None => "".to_string(),
        }
    }
}

fn deserialize_text_range<'de, D>(deserializer: D) -> Result<TextRange, D::Error>
where
    D: serde::Deserializer<'de>,
{
    #[derive(Deserialize)]
    struct RawTextRange {
        start: u32,
        end: u32,
    }

    let range = RawTextRange::deserialize(deserializer)?;
    Ok(TextRange::new(range.start.into(), range.end.into()))
}

// ---------------------------------------------------------------------

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
pub struct RemoteId {
    pub module: SmolStr,
    pub name: SmolStr,
    pub arity: u32,
}

impl fmt::Display for RemoteId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}/{}", self.module, self.name, self.arity)
    }
}

// ---------------------------------------------------------------------

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
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
    BoundedDynamicType(BoundedDynamicType),
    NoneType,
    PidType,
    PortType,
    ReferenceType,
    NumberType,
}
impl Type {
    pub const fn atom_lit_type(lit: SmolStr) -> Type {
        Type::AtomLitType(AtomLitType { atom: lit })
    }

    pub const FALSE_TYPE: Type = Type::atom_lit_type(SmolStr::new_inline("false"));

    pub const TRUE_TYPE: Type = Type::atom_lit_type(SmolStr::new_inline("true"));

    pub const CHAR_TYPE: Type = Type::NumberType;

    pub const BYTE_TYPE: Type = Type::NumberType;

    pub const FLOAT_TYPE: Type = Type::NumberType;

    pub const UNDEFINED: Type = Type::atom_lit_type(SmolStr::new_inline("undefined"));

    pub fn exn_class_type() -> Type {
        Type::UnionType(UnionType {
            tys: vec![
                Type::atom_lit_type(SmolStr::new_inline("error")),
                Type::atom_lit_type(SmolStr::new_inline("exit")),
                Type::atom_lit_type(SmolStr::new_inline("throw")),
            ],
        })
    }

    pub fn cls_exn_stack_type() -> Type {
        Type::UnionType(UnionType {
            tys: vec![
                Type::exn_class_type(),
                Type::AnyType,
                Type::ListType(ListType {
                    t: Box::new(Type::AnyType),
                }),
            ],
        })
    }

    pub fn cls_exn_stack_type_dynamic() -> Type {
        Type::UnionType(UnionType {
            tys: vec![
                Type::exn_class_type(),
                Type::DynamicType,
                Type::ListType(ListType {
                    t: Box::new(Type::DynamicType),
                }),
            ],
        })
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
                let id = RemoteId {
                    module: "erlang".into(),
                    name: name.into(),
                    arity: 0,
                };
                Some(RemoteType {
                    id,
                    arg_tys: vec![],
                })
            }
            _ => None,
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
        Type::RemoteType(Type::builtin_type_alias("string").unwrap())
    }

    pub fn boolean_type() -> Type {
        Type::RemoteType(Type::builtin_type_alias("boolean").unwrap())
    }

    pub fn builtin_type(name: &str) -> Option<Type> {
        match name {
            "any" | "term" => Some(Type::AnyType),
            "atom" | "module" | "node" => Some(Type::AtomType),
            "binary" | "bitstring" | "nonempty_binary" | "nonempty_bitstring" => {
                Some(Type::BinaryType)
            }
            "byte" => Some(Type::BYTE_TYPE),
            "char" => Some(Type::CHAR_TYPE),
            "float" => Some(Type::FLOAT_TYPE),
            "fun" | "function" => Some(Type::AnyFunType),
            "maybe_improper_list" | "nonempty_maybe_improper_list" => {
                Some(Type::ListType(ListType {
                    t: Box::new(Type::AnyType),
                }))
            }
            "pos_integer" | "neg_integer" | "non_neg_integer" | "integer" | "number" | "arity" => {
                Some(Type::NumberType)
            }
            "nil" => Some(Type::NilType),
            "none" | "no_return" => Some(Type::NoneType),
            "pid" => Some(Type::PidType),
            "port" => Some(Type::PortType),
            "reference" => Some(Type::ReferenceType),
            "tuple" => Some(Type::AnyTupleType),
            "nonempty_string" => Some(Type::string_type()),
            "dynamic" => Some(Type::DynamicType),
            _ => Type::builtin_type_alias(name).map(Type::RemoteType),
        }
    }

    pub fn walk<'a, T>(&'a self, f: &mut dyn FnMut(&'a Type) -> Result<(), T>) -> Result<(), T> {
        match self {
            Type::FunType(ty) => {
                f(&ty.res_ty).and_then(|()| ty.arg_tys.iter().try_for_each(|ty| f(ty)))
            }
            Type::AnyArityFunType(ty) => f(&ty.res_ty),
            Type::TupleType(ty) => ty.arg_tys.iter().try_for_each(|ty| f(ty)),
            Type::UnionType(ty) => ty.tys.iter().try_for_each(|ty| f(ty)),
            Type::RemoteType(ty) => ty.arg_tys.iter().try_for_each(|ty| f(ty)),
            Type::OpaqueType(ty) => ty.arg_tys.iter().try_for_each(|ty| f(ty)),
            Type::ShapeMap(ty) => ty.props.iter().try_for_each(|prop| f(prop.tp())),
            Type::DictMap(ty) => f(&ty.v_type).and_then(|()| f(&ty.k_type)),
            Type::ListType(ty) => f(&ty.t),
            Type::RefinedRecordType(ty) => ty.fields.iter().try_for_each(|(_, ty)| f(ty)),
            Type::BoundedDynamicType(ty) => f(&ty.bound),
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
        self.walk(&mut |ty| ty.traverse(f))
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::AtomLitType(a) => write!(f, "{}", a.atom.as_str()),
            Type::AnyType => write!(f, "term()"),
            Type::AnyFunType => write!(f, "fun()"),
            Type::AnyTupleType => write!(f, "tuple()"),
            Type::AtomType => write!(f, "atom()"),
            Type::NilType => write!(f, "[]"),
            Type::RecordType(rec) => write!(f, "#{}{{}}", rec.name.as_str()),
            Type::VarType(v) => write!(f, "{}", v.name.as_str()),
            Type::BinaryType => write!(f, "binary()"),
            Type::NoneType => write!(f, "none()"),
            Type::DynamicType => write!(f, "dynamic()"),
            Type::PidType => write!(f, "pid()"),
            Type::PortType => write!(f, "port()"),
            Type::ReferenceType => write!(f, "reference()"),
            Type::NumberType => write!(f, "number()"),
            Type::FunType(ty) => write!(
                f,
                "fun(({}) -> {})",
                ty.arg_tys
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
                ty.res_ty
            ),
            Type::AnyArityFunType(ty) => write!(f, "fun((...) -> {})", ty.res_ty),
            Type::TupleType(ty) => write!(
                f,
                "{{{}}}",
                ty.arg_tys
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::UnionType(ty) => {
                write!(
                    f,
                    "{}",
                    ty.tys
                        .iter()
                        .map(|ty| ty.to_string())
                        .collect::<Vec<_>>()
                        .join(" | ")
                )
            }
            Type::RemoteType(ty) => write!(
                f,
                "{}:{}({})",
                ty.id.module,
                ty.id.name,
                ty.arg_tys
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::OpaqueType(ty) => write!(
                f,
                "{}:{}({})",
                ty.id.module,
                ty.id.name,
                ty.arg_tys
                    .iter()
                    .map(|ty| ty.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::ShapeMap(ty) => write!(
                f,
                "#S{{{}}}",
                ty.props
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::DictMap(ty) => write!(f, "#D{{{} => {}}}", ty.k_type, ty.v_type),
            Type::ListType(ty) => write!(f, "[{}]", ty.t),
            Type::RefinedRecordType(ty) => write!(f, "#{}{{}}", ty.rec_type.name.as_str()),
            Type::BoundedDynamicType(ty) => write!(f, "dynamic({})", ty.bound),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AtomLitType {
    pub atom: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FunType {
    #[serde(default)]
    pub forall: Vec<u32>,
    #[serde(default)]
    pub arg_tys: Vec<Type>,
    pub res_ty: Box<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AnyArityFunType {
    pub res_ty: Box<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TupleType {
    #[serde(default)]
    pub arg_tys: Vec<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ListType {
    pub t: Box<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UnionType {
    #[serde(default)]
    pub tys: Vec<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RemoteType {
    pub id: RemoteId,
    #[serde(default)]
    pub arg_tys: Vec<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct OpaqueType {
    pub id: RemoteId,
    #[serde(default)]
    pub arg_tys: Vec<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct VarType {
    pub n: u32,
    pub name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordType {
    pub name: SmolStr,
    pub module: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RefinedRecordType {
    pub rec_type: RecordType,
    #[serde(default)]
    pub fields: FxHashMap<SmolStr, Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct DictMap {
    pub k_type: Box<Type>,
    pub v_type: Box<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ShapeMap {
    #[serde(default)]
    pub props: Vec<Prop>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
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

impl fmt::Display for Prop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prop::ReqProp(req) => write!(f, "{} := {}", req.key, req.tp),
            Prop::OptProp(opt) => write!(f, "{} => {}", opt.key, opt.tp),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ReqProp {
    pub key: SmolStr,
    pub tp: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct OptProp {
    pub key: SmolStr,
    pub tp: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BoundedDynamicType {
    pub bound: Box<Type>,
}

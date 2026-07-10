/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::BTreeMap;
use std::fmt;
use std::sync::LazyLock;
use std::vec;

use serde::Deserialize;
use serde::Serialize;
use serde_with::serde_as;

use crate::StringId;
use crate::eqwalizer::RemoteId;

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variance {
    Covariant,
    Contravariant,
    Invariant,
    Constant,
}

impl Variance {
    pub fn combine(self, other: Variance) -> Variance {
        match (self, other) {
            (v, Variance::Constant) => v,
            (Variance::Constant, v) => v,
            (v1, v2) if v1 == v2 => v1,
            _ => Variance::Invariant,
        }
    }
}

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
    ConsType(ConsType),
    UnionType(UnionType),
    RemoteType(RemoteType),
    BoundVarType(BoundVarType),
    FreeVarType(FreeVarType),
    RecordType(RecordType),
    RefinedRecordType(RefinedRecordType),
    NativeRecordType(NativeRecordType),
    AnyNativeRecordType,
    MapType(MapType),
    BinaryType,
    AnyType,
    AtomType,
    DynamicType,
    BoundedDynamicType(BoundedDynamicType),
    NoneType,
    PidType,
    PortType,
    ReferenceType,
    IntegerType,
    FloatType,
}
impl Type {
    pub const fn atom_lit_type(lit: StringId) -> Type {
        Type::AtomLitType(AtomLitType { atom: lit })
    }

    pub fn false_type() -> Type {
        static TYPE: LazyLock<Type> =
            LazyLock::new(|| Type::atom_lit_type(StringId::from("false")));
        TYPE.clone()
    }

    pub fn true_type() -> Type {
        static TYPE: LazyLock<Type> = LazyLock::new(|| Type::atom_lit_type(StringId::from("true")));
        TYPE.clone()
    }

    pub const CHAR_TYPE: Type = Type::IntegerType;

    pub const BYTE_TYPE: Type = Type::IntegerType;

    pub fn undefined() -> Type {
        static TYPE: LazyLock<Type> =
            LazyLock::new(|| Type::atom_lit_type(StringId::from("undefined")));
        TYPE.clone()
    }

    pub fn exn_class_type() -> Type {
        static TYPE: LazyLock<Type> = LazyLock::new(|| {
            Type::UnionType(UnionType {
                tys: vec![
                    Type::atom_lit_type(StringId::from("error")),
                    Type::atom_lit_type(StringId::from("exit")),
                    Type::atom_lit_type(StringId::from("throw")),
                ],
            })
        });
        TYPE.clone()
    }

    pub fn cls_exn_stack_type() -> Type {
        static TYPE: LazyLock<Type> = LazyLock::new(|| {
            Type::UnionType(UnionType {
                tys: vec![
                    Type::exn_class_type(),
                    Type::AnyType,
                    Type::ListType(ListType {
                        t: Box::new(Type::AnyType),
                    }),
                ],
            })
        });
        TYPE.clone()
    }

    pub fn cls_exn_stack_type_dynamic() -> Type {
        static TYPE: LazyLock<Type> = LazyLock::new(|| {
            Type::UnionType(UnionType {
                tys: vec![
                    Type::exn_class_type(),
                    Type::DynamicType,
                    Type::ListType(ListType {
                        t: Box::new(Type::DynamicType),
                    }),
                ],
            })
        });
        TYPE.clone()
    }

    pub fn builtin_type_aliases(module: &str) -> impl Iterator<Item = StringId> + use<> {
        static ERLANG_ALIASES: LazyLock<Vec<StringId>> = LazyLock::new(|| {
            vec![
                "string".into(),
                "boolean".into(),
                "timeout".into(),
                "identifier".into(),
                "mfa".into(),
                "iolist".into(),
                "iodata".into(),
                "number".into(),
            ]
        });
        static EMPTY: LazyLock<Vec<StringId>> = LazyLock::new(Vec::new);
        match module {
            "erlang" => ERLANG_ALIASES.iter().copied(),
            _ => EMPTY.iter().copied(),
        }
    }

    pub fn builtin_type_alias(name: &str) -> Option<RemoteType> {
        match name {
            "string" | "boolean" | "timeout" | "identifier" | "mfa" | "iolist" | "iodata"
            | "number" => {
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
                tys: vec![Type::false_type(), Type::true_type()],
            })),
            "timeout" => Some(Type::UnionType(UnionType {
                tys: vec![
                    Type::AtomLitType(AtomLitType {
                        atom: "infinity".into(),
                    }),
                    Type::IntegerType,
                ],
            })),
            "identifier" => Some(Type::UnionType(UnionType {
                tys: vec![Type::PidType, Type::PortType, Type::ReferenceType],
            })),
            "mfa" => Some(Type::TupleType(TupleType {
                arg_tys: vec![Type::AtomType, Type::AtomType, Type::IntegerType],
            })),
            "number" => Some(Type::UnionType(UnionType {
                tys: vec![Type::IntegerType, Type::FloatType],
            })),
            "iolist" => Some(Type::ListType(ListType {
                t: Box::new(Type::UnionType(UnionType {
                    tys: vec![
                        Type::BYTE_TYPE,
                        Type::BinaryType,
                        Type::RemoteType(
                            Type::builtin_type_alias("iolist").expect("iolist should be a builtin"),
                        ),
                    ],
                })),
            })),
            "iodata" => Some(Type::UnionType(UnionType {
                tys: vec![
                    Type::RemoteType(
                        Type::builtin_type_alias("iolist").expect("iolist should be a builtin"),
                    ),
                    Type::BinaryType,
                ],
            })),
            _ => None,
        }
    }

    pub fn string_type() -> Type {
        Type::RemoteType(Type::builtin_type_alias("string").expect("string should be a builtin"))
    }

    pub fn boolean_type() -> Type {
        Type::RemoteType(Type::builtin_type_alias("boolean").expect("boolean should be a builtin"))
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
            "float" => Some(Type::FloatType),
            "fun" | "function" => Some(Type::AnyFunType),
            "maybe_improper_list" | "nonempty_maybe_improper_list" => {
                Some(Type::ListType(ListType {
                    t: Box::new(Type::AnyType),
                }))
            }
            "pos_integer" | "neg_integer" | "non_neg_integer" | "integer" | "arity" => {
                Some(Type::IntegerType)
            }
            "nil" => Some(Type::NilType),
            "none" | "no_return" => Some(Type::NoneType),
            "pid" => Some(Type::PidType),
            "port" => Some(Type::PortType),
            "reference" => Some(Type::ReferenceType),
            "tuple" => Some(Type::AnyTupleType),
            "record" => Some(Type::AnyNativeRecordType),
            "nonempty_string" => Some(Type::string_type()),
            "dynamic" => Some(Type::DynamicType),
            _ => Type::builtin_type_alias(name).map(Type::RemoteType),
        }
    }

    pub fn walk<'a, T>(&'a self, f: &mut dyn FnMut(&'a Type) -> Result<(), T>) -> Result<(), T> {
        match self {
            Type::FunType(ty) => f(&ty.res_ty).and_then(|()| ty.arg_tys.iter().try_for_each(f)),
            Type::AnyArityFunType(ty) => f(&ty.res_ty),
            Type::TupleType(ty) => ty.arg_tys.iter().try_for_each(f),
            Type::UnionType(ty) => ty.tys.iter().try_for_each(f),
            Type::RemoteType(ty) => ty.arg_tys.iter().try_for_each(f),
            Type::MapType(ty) => f(&ty.k_type)
                .and_then(|()| f(&ty.v_type))
                .and_then(|()| ty.props.iter().try_for_each(|(_, prop)| f(&prop.tp))),
            Type::ListType(ty) => f(&ty.t),
            Type::ConsType(ty) => f(&ty.head_t).and_then(|()| f(&ty.tail_t)),
            Type::RefinedRecordType(ty) => ty.fields.iter().try_for_each(|(_, ty)| f(ty)),
            Type::BoundedDynamicType(ty) => f(&ty.bound),
            Type::AtomLitType(_)
            | Type::AnyType
            | Type::AnyFunType
            | Type::AnyTupleType
            | Type::AtomType
            | Type::NilType
            | Type::RecordType(_)
            | Type::NativeRecordType(_)
            | Type::AnyNativeRecordType
            | Type::BoundVarType(_)
            | Type::FreeVarType(_)
            | Type::BinaryType
            | Type::NoneType
            | Type::DynamicType
            | Type::PidType
            | Type::PortType
            | Type::ReferenceType
            | Type::IntegerType
            | Type::FloatType => Ok(()),
        }
    }

    pub fn traverse<T>(&self, f: &mut dyn FnMut(&Type) -> Result<(), T>) -> Result<(), T> {
        f(self)?;
        self.walk(&mut |ty| ty.traverse(f))
    }

    pub fn is_dynamic(&self) -> bool {
        self == &Type::DynamicType
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
            Type::BoundVarType(v) => write!(f, "{}", v.name.as_str()),
            Type::FreeVarType(v) => write!(f, "{}", v.name.as_str()),
            Type::BinaryType => write!(f, "binary()"),
            Type::NoneType => write!(f, "none()"),
            Type::DynamicType => write!(f, "dynamic()"),
            Type::PidType => write!(f, "pid()"),
            Type::PortType => write!(f, "port()"),
            Type::ReferenceType => write!(f, "reference()"),
            Type::IntegerType => write!(f, "integer()"),
            Type::FloatType => write!(f, "float()"),
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
            Type::MapType(ty) if ty.props.is_empty() => {
                write!(f, "#{{{} => {}}}", ty.k_type, ty.v_type)
            }
            Type::MapType(ty) if *ty.k_type == Self::NoneType => write!(
                f,
                "#{{{}}}",
                ty.props
                    .iter()
                    .map(|(k, p)| format!("{k} {p}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::MapType(ty) => write!(
                f,
                "#{{{}, {} => {}}}",
                ty.props
                    .iter()
                    .map(|(k, p)| format!("{k} {p}"))
                    .collect::<Vec<_>>()
                    .join(", "),
                ty.k_type,
                ty.v_type
            ),
            Type::ListType(ty) => write!(f, "[{}]", ty.t),
            Type::ConsType(_) => write!(f, "{}", show_cons(self)),
            Type::RefinedRecordType(ty) => write!(f, "#{}{{}}", ty.rec_type.name.as_str()),
            Type::AnyNativeRecordType => write!(f, "record()"),
            Type::NativeRecordType(ty) => {
                write!(f, "#{}:{}{{}}", ty.id.module, ty.id.name)
            }
            Type::BoundedDynamicType(ty) => write!(f, "dynamic({})", ty.bound),
        }
    }
}

/// Render a ConsType chain, flattening contiguous cons cells into Erlang's
/// `[h1, h2, ... / tail]` syntax. Mirrors `Show.showCons` on the Scala side.
fn show_cons(t: &Type) -> String {
    let mut heads: Vec<&Type> = Vec::new();
    let mut cur = t;
    while let Type::ConsType(c) = cur {
        heads.push(&c.head_t);
        cur = &c.tail_t;
    }
    let head_str = heads
        .iter()
        .map(|h| h.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    match cur {
        Type::NilType => format!("[{} / []]", head_str),
        Type::ListType(lt) => {
            if heads.iter().all(|h| *h == lt.t.as_ref()) {
                format!("[{}, ...]", lt.t)
            } else {
                format!("[{} / [{}]]", head_str, lt.t)
            }
        }
        other => format!("[{} / {}]", head_str, other),
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AtomLitType {
    pub atom: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FunType {
    #[serde(default)]
    pub forall: u32,
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
pub struct ConsType {
    pub head_t: Box<Type>,
    pub tail_t: Box<Type>,
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

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BoundVarType {
    pub lvl: u32,
    pub name: StringId,
}

impl PartialEq for BoundVarType {
    fn eq(&self, other: &Self) -> bool {
        self.lvl == other.lvl
    }
}

impl Eq for BoundVarType {}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct FreeVarType {
    pub n: u32,
    pub name: StringId,
}

impl PartialEq for FreeVarType {
    fn eq(&self, other: &Self) -> bool {
        self.n == other.n
    }
}

impl Eq for FreeVarType {}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct RecordType {
    pub name: StringId,
    pub module: StringId,
}

impl PartialEq for RecordType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for RecordType {}

#[serde_as]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RefinedRecordType {
    pub rec_type: RecordType,
    #[serde_as(as = "Vec<(_, _)>")]
    #[serde(default)]
    pub fields: BTreeMap<StringId, Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct NativeRecordType {
    pub id: RemoteId,
}

#[serde_as]
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MapType {
    #[serde_as(as = "Vec<(_, _)>")]
    #[serde(default)]
    pub props: BTreeMap<Key, MapProp>,
    pub k_type: Box<Type>,
    pub v_type: Box<Type>,
}

#[derive(
    Serialize,
    Deserialize,
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    Ord,
    PartialOrd
)]
pub enum Key {
    TupleKey(TupleKey),
    AtomKey(AtomKey),
}

#[derive(
    Serialize,
    Deserialize,
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    Ord,
    PartialOrd
)]
pub struct TupleKey {
    #[serde(default)]
    pub keys: Vec<Key>,
}

#[derive(
    Serialize,
    Deserialize,
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    Ord,
    PartialOrd
)]
pub struct AtomKey {
    pub name: StringId,
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Key::TupleKey(ty) => write!(
                f,
                "{{{}}}",
                ty.keys
                    .iter()
                    .map(|k| k.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Key::AtomKey(a) => write!(f, "{}", a.name.as_str()),
        }
    }
}

impl Key {
    pub fn from_type(ty: Type) -> Option<Self> {
        match ty {
            Type::TupleType(ty) => ty
                .arg_tys
                .into_iter()
                .map(Key::from_type)
                .collect::<Option<Vec<_>>>()
                .map(|keys| Key::TupleKey(TupleKey { keys })),
            Type::AtomLitType(a) => Some(Key::AtomKey(AtomKey { name: a.atom })),
            _ => None,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MapProp {
    pub req: bool,
    pub tp: Type,
}

impl fmt::Display for MapProp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.req {
            write!(f, ":= {}", self.tp)
        } else {
            write!(f, "=> {}", self.tp)
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BoundedDynamicType {
    pub bound: Box<Type>,
}

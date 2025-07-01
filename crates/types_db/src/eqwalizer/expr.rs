/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use serde::Deserialize;
use serde::Serialize;

use crate::StringId;
use crate::eqwalizer;
use crate::eqwalizer::Pos;
use crate::eqwalizer::binary_specifier;
use crate::eqwalizer::ext_types::ExtType;
use crate::eqwalizer::guard;
use crate::eqwalizer::pat;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Var(Var),
    AtomLit(AtomLit),
    IntLit(IntLit),
    FloatLit(FloatLit),
    Block(Block),
    Match(Match),
    Tuple(Tuple),
    StringLit(StringLit),
    NilLit(NilLit),
    Cons(Cons),
    Case(Case),
    If(If),
    LocalCall(LocalCall),
    DynCall(DynCall),
    RemoteCall(RemoteCall),
    LocalFun(LocalFun),
    RemoteFun(RemoteFun),
    DynRemoteFun(DynRemoteFun),
    DynRemoteFunArity(DynRemoteFunArity),
    Lambda(Lambda),
    UnOp(UnOp),
    BinOp(BinOp),
    LComprehension(LComprehension),
    BComprehension(BComprehension),
    MComprehension(MComprehension),
    Binary(Binary),
    Catch(Catch),
    TryCatchExpr(TryCatchExpr),
    TryOfCatchExpr(TryOfCatchExpr),
    Receive(Receive),
    ReceiveWithTimeout(ReceiveWithTimeout),
    RecordCreate(RecordCreate),
    RecordUpdate(RecordUpdate),
    RecordSelect(RecordSelect),
    RecordIndex(RecordIndex),
    MapCreate(MapCreate),
    MapUpdate(MapUpdate),
    Maybe(Maybe),
    MaybeElse(MaybeElse),
    MaybeMatch(MaybeMatch),
    TypeCast(TypeCast),
}

impl Expr {
    pub fn atom_true(pos: Pos) -> Self {
        Expr::AtomLit(AtomLit {
            pos,
            s: StringId::from("true"),
        })
    }

    pub fn atom_false(pos: Pos) -> Self {
        Expr::AtomLit(AtomLit {
            pos,
            s: StringId::from("false"),
        })
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Var {
    pub pos: Pos,
    pub n: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AtomLit {
    pub pos: Pos,
    pub s: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct IntLit {
    pub pos: Pos,
    pub value: Option<i32>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FloatLit {
    pub pos: Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub pos: Pos,
    pub body: Body,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Body {
    #[serde(default)]
    pub exprs: Vec<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Match {
    pub pos: Pos,
    pub pat: pat::Pat,
    pub expr: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    pub pos: Pos,
    #[serde(default)]
    pub elems: Vec<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct StringLit {
    pub pos: Pos,
    pub empty: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct NilLit {
    pub pos: Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Cons {
    pub pos: Pos,
    pub h: Box<Expr>,
    pub t: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Case {
    pub pos: Pos,
    pub expr: Box<Expr>,
    #[serde(default)]
    pub clauses: Vec<Clause>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct If {
    pub pos: Pos,
    #[serde(default)]
    pub clauses: Vec<Clause>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct LocalCall {
    pub pos: Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub args: Vec<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct DynCall {
    pub pos: Pos,
    pub f: Box<Expr>,
    #[serde(default)]
    pub args: Vec<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RemoteCall {
    pub pos: Pos,
    pub id: eqwalizer::RemoteId,
    #[serde(default)]
    pub args: Vec<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct LocalFun {
    pub pos: Pos,
    pub id: eqwalizer::Id,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RemoteFun {
    pub pos: Pos,
    pub id: eqwalizer::RemoteId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct DynRemoteFun {
    pub pos: Pos,
    pub module: Box<Expr>,
    pub name: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct DynRemoteFunArity {
    pub pos: Pos,
    pub module: Box<Expr>,
    pub name: Box<Expr>,
    pub arity: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Lambda {
    pub pos: Pos,
    #[serde(default)]
    pub clauses: Vec<Clause>,
    pub name: Option<StringId>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UnOp {
    pub pos: Pos,
    pub op: StringId,
    pub arg: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BinOp {
    pub pos: Pos,
    pub op: StringId,
    pub arg_1: Box<Expr>,
    pub arg_2: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct LComprehension {
    pub pos: Pos,
    pub template: Box<Expr>,
    #[serde(default)]
    pub qualifiers: Vec<Qualifier>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BComprehension {
    pub pos: Pos,
    pub template: Box<Expr>,
    #[serde(default)]
    pub qualifiers: Vec<Qualifier>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MComprehension {
    pub pos: Pos,
    pub k_template: Box<Expr>,
    pub v_template: Box<Expr>,
    #[serde(default)]
    pub qualifiers: Vec<Qualifier>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Binary {
    pub pos: Pos,
    #[serde(default)]
    pub elems: Vec<BinaryElem>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Catch {
    pub pos: Pos,
    pub expr: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TryCatchExpr {
    pub pos: Pos,
    pub try_body: Body,
    #[serde(default)]
    pub catch_clauses: Vec<Clause>,
    pub after_body: Option<Body>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TryOfCatchExpr {
    pub pos: Pos,
    pub try_body: Body,
    #[serde(default)]
    pub try_clauses: Vec<Clause>,
    #[serde(default)]
    pub catch_clauses: Vec<Clause>,
    pub after_body: Option<Body>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Receive {
    pub pos: Pos,
    #[serde(default)]
    pub clauses: Vec<Clause>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ReceiveWithTimeout {
    pub pos: Pos,
    #[serde(default)]
    pub clauses: Vec<Clause>,
    pub timeout: Box<Expr>,
    pub timeout_body: Body,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordCreate {
    pub pos: Pos,
    pub rec_name: StringId,
    #[serde(default)]
    pub fields: Vec<RecordField>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordUpdate {
    pub pos: Pos,
    pub expr: Box<Expr>,
    pub rec_name: StringId,
    #[serde(default)]
    pub fields: Vec<RecordFieldNamed>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordSelect {
    pub pos: Pos,
    pub expr: Box<Expr>,
    pub rec_name: StringId,
    pub field_name: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordIndex {
    pub pos: Pos,
    pub rec_name: StringId,
    pub field_name: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MapCreate {
    pub pos: Pos,
    #[serde(default)]
    pub kvs: Vec<(Expr, Expr)>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MapUpdate {
    pub pos: Pos,
    pub map: Box<Expr>,
    #[serde(default)]
    pub kvs: Vec<(Expr, Expr)>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Clause {
    pub pos: Pos,
    #[serde(default)]
    pub pats: Vec<pat::Pat>,
    #[serde(default)]
    pub guards: Vec<guard::Guard>,
    pub body: Body,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BinaryElem {
    pub pos: Pos,
    pub expr: Expr,
    pub size: Option<Expr>,
    pub specifier: binary_specifier::Specifier,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum RecordField {
    RecordFieldNamed(RecordFieldNamed),
    RecordFieldGen(RecordFieldGen),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordFieldNamed {
    pub name: StringId,
    pub value: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordFieldGen {
    pub value: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Qualifier {
    LGenerate(LGenerate),
    LGenerateStrict(LGenerateStrict),
    BGenerate(BGenerate),
    BGenerateStrict(BGenerateStrict),
    MGenerate(MGenerate),
    MGenerateStrict(MGenerateStrict),
    Zip(Zip),
    Filter(Filter),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct LGenerate {
    pub pat: pat::Pat,
    pub expr: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct LGenerateStrict {
    pub pat: pat::Pat,
    pub expr: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BGenerate {
    pub pat: pat::Pat,
    pub expr: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BGenerateStrict {
    pub pat: pat::Pat,
    pub expr: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MGenerate {
    pub k_pat: pat::Pat,
    pub v_pat: pat::Pat,
    pub expr: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MGenerateStrict {
    pub k_pat: pat::Pat,
    pub v_pat: pat::Pat,
    pub expr: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Zip {
    pub generators: Vec<Qualifier>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Filter {
    pub expr: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Maybe {
    pub pos: Pos,
    pub body: Body,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MaybeElse {
    pub pos: Pos,
    pub body: Body,
    #[serde(default)]
    pub else_clauses: Vec<Clause>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MaybeMatch {
    pub pos: Pos,
    pub pat: pat::Pat,
    pub arg: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TypeCast {
    pub pos: Pos,
    pub expr: Box<Expr>,
    pub ty: ExtType,
    pub checked: bool,
}

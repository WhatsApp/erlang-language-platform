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
use crate::eqwalizer::binary_specifier;
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
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Var {
    pub location: eqwalizer::Pos,
    pub n: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AtomLit {
    pub location: eqwalizer::Pos,
    pub s: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct IntLit {
    pub location: eqwalizer::Pos,
    pub value: Option<i32>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FloatLit {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub location: eqwalizer::Pos,
    pub body: Body,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Body {
    #[serde(default)]
    pub exprs: Vec<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Match {
    pub location: eqwalizer::Pos,
    pub pat: pat::Pat,
    pub expr: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub elems: Vec<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct StringLit {
    pub location: eqwalizer::Pos,
    pub empty: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct NilLit {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Cons {
    pub location: eqwalizer::Pos,
    pub h: Box<Expr>,
    pub t: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Case {
    pub location: eqwalizer::Pos,
    pub expr: Box<Expr>,
    #[serde(default)]
    pub clauses: Vec<Clause>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct If {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub clauses: Vec<Clause>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct LocalCall {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub args: Vec<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct DynCall {
    pub location: eqwalizer::Pos,
    pub f: Box<Expr>,
    #[serde(default)]
    pub args: Vec<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RemoteCall {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::RemoteId,
    #[serde(default)]
    pub args: Vec<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct LocalFun {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RemoteFun {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::RemoteId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct DynRemoteFun {
    pub location: eqwalizer::Pos,
    pub module: Box<Expr>,
    pub name: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct DynRemoteFunArity {
    pub location: eqwalizer::Pos,
    pub module: Box<Expr>,
    pub name: Box<Expr>,
    pub arity: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Lambda {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub clauses: Vec<Clause>,
    pub name: Option<SmolStr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UnOp {
    pub location: eqwalizer::Pos,
    pub op: SmolStr,
    pub arg: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BinOp {
    pub location: eqwalizer::Pos,
    pub op: SmolStr,
    pub arg_1: Box<Expr>,
    pub arg_2: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct LComprehension {
    pub location: eqwalizer::Pos,
    pub template: Box<Expr>,
    #[serde(default)]
    pub qualifiers: Vec<Qualifier>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BComprehension {
    pub location: eqwalizer::Pos,
    pub template: Box<Expr>,
    #[serde(default)]
    pub qualifiers: Vec<Qualifier>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MComprehension {
    pub location: eqwalizer::Pos,
    pub k_template: Box<Expr>,
    pub v_template: Box<Expr>,
    #[serde(default)]
    pub qualifiers: Vec<Qualifier>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Binary {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub elems: Vec<BinaryElem>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Catch {
    pub location: eqwalizer::Pos,
    pub expr: Box<Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TryCatchExpr {
    pub location: eqwalizer::Pos,
    pub try_body: Body,
    #[serde(default)]
    pub catch_clauses: Vec<Clause>,
    pub after_body: Option<Body>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TryOfCatchExpr {
    pub location: eqwalizer::Pos,
    pub try_body: Body,
    #[serde(default)]
    pub try_clauses: Vec<Clause>,
    #[serde(default)]
    pub catch_clauses: Vec<Clause>,
    pub after_body: Option<Body>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Receive {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub clauses: Vec<Clause>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ReceiveWithTimeout {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub clauses: Vec<Clause>,
    pub timeout: Box<Expr>,
    pub timeout_body: Body,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordCreate {
    pub location: eqwalizer::Pos,
    pub rec_name: SmolStr,
    #[serde(default)]
    pub fields: Vec<RecordField>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordUpdate {
    pub location: eqwalizer::Pos,
    pub expr: Box<Expr>,
    pub rec_name: SmolStr,
    #[serde(default)]
    pub fields: Vec<RecordFieldNamed>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordSelect {
    pub location: eqwalizer::Pos,
    pub expr: Box<Expr>,
    pub rec_name: SmolStr,
    pub field_name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordIndex {
    pub location: eqwalizer::Pos,
    pub rec_name: SmolStr,
    pub field_name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MapCreate {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub kvs: Vec<(Expr, Expr)>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MapUpdate {
    pub location: eqwalizer::Pos,
    pub map: Box<Expr>,
    #[serde(default)]
    pub kvs: Vec<(Expr, Expr)>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Clause {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub pats: Vec<pat::Pat>,
    #[serde(default)]
    pub guards: Vec<guard::Guard>,
    pub body: Body,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BinaryElem {
    pub location: eqwalizer::Pos,
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
    pub name: SmolStr,
    pub value: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordFieldGen {
    pub value: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Qualifier {
    LGenerate(LGenerate),
    BGenerate(BGenerate),
    MGenerate(MGenerate),
    Filter(Filter),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct LGenerate {
    pub pat: pat::Pat,
    pub expr: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BGenerate {
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
pub struct Filter {
    pub expr: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Maybe {
    pub location: eqwalizer::Pos,
    pub body: Body,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MaybeElse {
    pub location: eqwalizer::Pos,
    pub body: Body,
    #[serde(default)]
    pub else_clauses: Vec<Clause>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MaybeMatch {
    pub location: eqwalizer::Pos,
    pub pat: pat::Pat,
    pub arg: Box<Expr>,
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use ast::binary_specifier;
use ast::guard;
use ast::pat;
use elp_syntax::SmolStr;
use serde::Serialize;

use crate::ast;

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
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
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Var {
    pub location: ast::Pos,
    pub n: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct AtomLit {
    pub location: ast::Pos,
    pub s: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct IntLit {
    pub location: ast::Pos,
    pub value: Option<i32>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct FloatLit {
    pub location: ast::Pos,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub location: ast::Pos,
    pub body: Body,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Body {
    pub exprs: Vec<Expr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Match {
    pub location: ast::Pos,
    pub pat: pat::Pat,
    pub expr: Box<Expr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Tuple {
    pub location: ast::Pos,
    pub elems: Vec<Expr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct StringLit {
    pub location: ast::Pos,
    pub empty: bool,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct NilLit {
    pub location: ast::Pos,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Cons {
    pub location: ast::Pos,
    pub h: Box<Expr>,
    pub t: Box<Expr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Case {
    pub location: ast::Pos,
    pub expr: Box<Expr>,
    pub clauses: Vec<Clause>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct If {
    pub location: ast::Pos,
    pub clauses: Vec<Clause>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct LocalCall {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub args: Vec<Expr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct DynCall {
    pub location: ast::Pos,
    pub f: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RemoteCall {
    pub location: ast::Pos,
    pub id: ast::RemoteId,
    pub args: Vec<Expr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct LocalFun {
    pub location: ast::Pos,
    pub id: ast::Id,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RemoteFun {
    pub location: ast::Pos,
    pub id: ast::RemoteId,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct DynRemoteFun {
    pub location: ast::Pos,
    pub module: Box<Expr>,
    pub name: Box<Expr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct DynRemoteFunArity {
    pub location: ast::Pos,
    pub module: Box<Expr>,
    pub name: Box<Expr>,
    pub arity: Box<Expr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Lambda {
    pub location: ast::Pos,
    pub clauses: Vec<Clause>,
    pub name: Option<SmolStr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct UnOp {
    pub location: ast::Pos,
    pub op: SmolStr,
    pub arg: Box<Expr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct BinOp {
    pub location: ast::Pos,
    pub op: SmolStr,
    pub arg_1: Box<Expr>,
    pub arg_2: Box<Expr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct LComprehension {
    pub location: ast::Pos,
    pub template: Box<Expr>,
    pub qualifiers: Vec<Qualifier>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct BComprehension {
    pub location: ast::Pos,
    pub template: Box<Expr>,
    pub qualifiers: Vec<Qualifier>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct MComprehension {
    pub location: ast::Pos,
    pub k_template: Box<Expr>,
    pub v_template: Box<Expr>,
    pub qualifiers: Vec<Qualifier>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Binary {
    pub location: ast::Pos,
    pub elems: Vec<BinaryElem>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Catch {
    pub location: ast::Pos,
    pub expr: Box<Expr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TryCatchExpr {
    pub location: ast::Pos,
    pub try_body: Body,
    pub catch_clauses: Vec<Clause>,
    pub after_body: Option<Body>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TryOfCatchExpr {
    pub location: ast::Pos,
    pub try_body: Body,
    pub try_clauses: Vec<Clause>,
    pub catch_clauses: Vec<Clause>,
    pub after_body: Option<Body>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Receive {
    pub location: ast::Pos,
    pub clauses: Vec<Clause>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ReceiveWithTimeout {
    pub location: ast::Pos,
    pub clauses: Vec<Clause>,
    pub timeout: Box<Expr>,
    pub timeout_body: Body,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordCreate {
    pub location: ast::Pos,
    pub rec_name: SmolStr,
    pub fields: Vec<RecordField>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordUpdate {
    pub location: ast::Pos,
    pub expr: Box<Expr>,
    pub rec_name: SmolStr,
    pub fields: Vec<RecordFieldNamed>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordSelect {
    pub location: ast::Pos,
    pub expr: Box<Expr>,
    pub rec_name: SmolStr,
    pub field_name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordIndex {
    pub location: ast::Pos,
    pub rec_name: SmolStr,
    pub field_name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct MapCreate {
    pub location: ast::Pos,
    pub kvs: Vec<(Expr, Expr)>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct MapUpdate {
    pub location: ast::Pos,
    pub map: Box<Expr>,
    pub kvs: Vec<(Expr, Expr)>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Clause {
    pub location: ast::Pos,
    pub pats: Vec<pat::Pat>,
    pub guards: Vec<guard::Guard>,
    pub body: Body,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct BinaryElem {
    pub location: ast::Pos,
    pub expr: Expr,
    pub size: Option<Expr>,
    pub specifier: binary_specifier::Specifier,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub enum RecordField {
    RecordFieldNamed(RecordFieldNamed),
    RecordFieldGen(RecordFieldGen),
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordFieldNamed {
    pub name: SmolStr,
    pub value: Expr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordFieldGen {
    pub value: Expr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub enum Qualifier {
    LGenerate(LGenerate),
    BGenerate(BGenerate),
    MGenerate(MGenerate),
    Filter(Filter),
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct LGenerate {
    pub pat: pat::Pat,
    pub expr: Expr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct BGenerate {
    pub pat: pat::Pat,
    pub expr: Expr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct MGenerate {
    pub k_pat: pat::Pat,
    pub v_pat: pat::Pat,
    pub expr: Expr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Filter {
    pub expr: Expr,
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use ast::binary_specifier;
use ast::expr;
use ast::guard;
use elp_syntax::SmolStr;
use serde::Serialize;

use crate::ast;

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    PatWild(PatWild),
    PatMatch(PatMatch),
    PatTuple(PatTuple),
    PatString(PatString),
    PatNil(PatNil),
    PatCons(PatCons),
    PatInt(PatInt),
    PatNumber(PatNumber),
    PatAtom(PatAtom),
    PatVar(PatVar),
    PatRecord(PatRecord),
    PatRecordIndex(PatRecordIndex),
    PatUnOp(PatUnOp),
    PatBinOp(PatBinOp),
    PatBinary(PatBinary),
    PatMap(PatMap),
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatWild {
    pub location: ast::Pos,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatMatch {
    pub location: ast::Pos,
    pub pat: Box<Pat>,
    pub arg: Box<Pat>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatTuple {
    pub location: ast::Pos,
    pub elems: Vec<Pat>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatString {
    pub location: ast::Pos,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatNil {
    pub location: ast::Pos,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatCons {
    pub location: ast::Pos,
    pub h: Box<Pat>,
    pub t: Box<Pat>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatInt {
    pub location: ast::Pos,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatNumber {
    pub location: ast::Pos,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatAtom {
    pub location: ast::Pos,
    pub s: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatVar {
    pub location: ast::Pos,
    pub n: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatRecord {
    pub location: ast::Pos,
    pub rec_name: SmolStr,
    pub fields: Vec<PatRecordFieldNamed>,
    pub gen: Option<Box<Pat>>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatRecordIndex {
    pub location: ast::Pos,
    pub rec_name: SmolStr,
    pub field_name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatUnOp {
    pub location: ast::Pos,
    pub op: SmolStr,
    pub arg: Box<Pat>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatBinOp {
    pub location: ast::Pos,
    pub op: SmolStr,
    pub arg_1: Box<Pat>,
    pub arg_2: Box<Pat>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatBinary {
    pub location: ast::Pos,
    pub elems: Vec<PatBinaryElem>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatBinaryElem {
    pub location: ast::Pos,
    pub pat: Pat,
    pub size: Option<expr::Expr>,
    pub specifier: binary_specifier::Specifier,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatRecordFieldNamed {
    pub name: SmolStr,
    pub pat: Pat,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct PatMap {
    pub location: ast::Pos,
    pub kvs: Vec<(guard::Test, Pat)>,
}

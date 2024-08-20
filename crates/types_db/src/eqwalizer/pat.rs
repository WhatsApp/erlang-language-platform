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
use crate::eqwalizer::expr;
use crate::eqwalizer::guard;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
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

impl Pat {
    pub fn pat_var(location: eqwalizer::Pos, n: SmolStr) -> Self {
        Pat::PatVar(PatVar { location, n })
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatWild {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatMatch {
    pub location: eqwalizer::Pos,
    pub pat: Box<Pat>,
    pub arg: Box<Pat>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatTuple {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub elems: Vec<Pat>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatString {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatNil {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatCons {
    pub location: eqwalizer::Pos,
    pub h: Box<Pat>,
    pub t: Box<Pat>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatInt {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatNumber {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatAtom {
    pub location: eqwalizer::Pos,
    pub s: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatVar {
    pub location: eqwalizer::Pos,
    pub n: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatRecord {
    pub location: eqwalizer::Pos,
    pub rec_name: SmolStr,
    #[serde(default)]
    pub fields: Vec<PatRecordFieldNamed>,
    pub gen: Option<Box<Pat>>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatRecordIndex {
    pub location: eqwalizer::Pos,
    pub rec_name: SmolStr,
    pub field_name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatUnOp {
    pub location: eqwalizer::Pos,
    pub op: SmolStr,
    pub arg: Box<Pat>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatBinOp {
    pub location: eqwalizer::Pos,
    pub op: SmolStr,
    pub arg_1: Box<Pat>,
    pub arg_2: Box<Pat>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatBinary {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub elems: Vec<PatBinaryElem>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatBinaryElem {
    pub location: eqwalizer::Pos,
    pub pat: Pat,
    pub size: Option<expr::Expr>,
    pub specifier: binary_specifier::Specifier,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatRecordFieldNamed {
    pub name: SmolStr,
    pub pat: Pat,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatMap {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub kvs: Vec<(guard::Test, Pat)>,
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use serde::Deserialize;
use serde::Serialize;

use crate::StringId;
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
    pub fn pat_var(location: eqwalizer::Pos, n: StringId) -> Self {
        Pat::PatVar(PatVar { pos: location, n })
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatWild {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatMatch {
    pub pos: eqwalizer::Pos,
    pub pat: Box<Pat>,
    pub arg: Box<Pat>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatTuple {
    pub pos: eqwalizer::Pos,
    #[serde(default)]
    pub elems: Vec<Pat>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatString {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatNil {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatCons {
    pub pos: eqwalizer::Pos,
    pub h: Box<Pat>,
    pub t: Box<Pat>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatInt {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatNumber {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatAtom {
    pub pos: eqwalizer::Pos,
    pub s: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatVar {
    pub pos: eqwalizer::Pos,
    pub n: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatRecord {
    pub pos: eqwalizer::Pos,
    pub rec_name: StringId,
    #[serde(default)]
    pub fields: Vec<PatRecordFieldNamed>,
    pub gen: Option<Box<Pat>>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatRecordIndex {
    pub pos: eqwalizer::Pos,
    pub rec_name: StringId,
    pub field_name: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatUnOp {
    pub pos: eqwalizer::Pos,
    pub op: StringId,
    pub arg: Box<Pat>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatBinOp {
    pub pos: eqwalizer::Pos,
    pub op: StringId,
    pub arg_1: Box<Pat>,
    pub arg_2: Box<Pat>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatBinary {
    pub pos: eqwalizer::Pos,
    #[serde(default)]
    pub elems: Vec<PatBinaryElem>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatBinaryElem {
    pub pos: eqwalizer::Pos,
    pub pat: Pat,
    pub size: Option<expr::Expr>,
    pub specifier: binary_specifier::Specifier,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatRecordFieldNamed {
    pub name: StringId,
    pub pat: Pat,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PatMap {
    pub pos: eqwalizer::Pos,
    #[serde(default)]
    pub kvs: Vec<(guard::Test, Pat)>,
}

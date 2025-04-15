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

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Guard {
    #[serde(default)]
    pub tests: Vec<Test>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Test {
    TestVar(TestVar),
    TestAtom(TestAtom),
    TestNumber(TestNumber),
    TestTuple(TestTuple),
    TestString(TestString),
    TestNil(TestNil),
    TestCons(TestCons),
    TestCall(TestCall),
    TestRecordCreate(TestRecordCreate),
    TestRecordSelect(TestRecordSelect),
    TestRecordIndex(TestRecordIndex),
    TestMapCreate(TestMapCreate),
    TestMapUpdate(TestMapUpdate),
    TestUnOp(TestUnOp),
    TestBinOp(TestBinOp),
    TestBinaryLit(TestBinaryLit),
}

impl Test {
    pub fn test_var(pos: eqwalizer::Pos, v: StringId) -> Self {
        Test::TestVar(TestVar { pos, v })
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestVar {
    pub pos: eqwalizer::Pos,
    pub v: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestAtom {
    pub pos: eqwalizer::Pos,
    pub s: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestNumber {
    pub pos: eqwalizer::Pos,
    pub lit: Option<i32>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestTuple {
    pub pos: eqwalizer::Pos,
    #[serde(default)]
    pub elems: Vec<Test>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestString {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestNil {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestCons {
    pub pos: eqwalizer::Pos,
    pub h: Box<Test>,
    pub t: Box<Test>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestCall {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub args: Vec<Test>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestRecordCreate {
    pub pos: eqwalizer::Pos,
    pub rec_name: StringId,
    #[serde(default)]
    pub fields: Vec<TestRecordField>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestRecordSelect {
    pub pos: eqwalizer::Pos,
    pub rec: Box<Test>,
    pub rec_name: StringId,
    pub field_name: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestRecordIndex {
    pub pos: eqwalizer::Pos,
    pub rec_name: StringId,
    pub field_name: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestMapCreate {
    pub pos: eqwalizer::Pos,
    #[serde(default)]
    pub kvs: Vec<(Test, Test)>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestMapUpdate {
    pub pos: eqwalizer::Pos,
    pub map: Box<Test>,
    #[serde(default)]
    pub kvs: Vec<(Test, Test)>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestUnOp {
    pub pos: eqwalizer::Pos,
    pub op: StringId,
    pub arg: Box<Test>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestBinOp {
    pub pos: eqwalizer::Pos,
    pub op: StringId,
    pub arg_1: Box<Test>,
    pub arg_2: Box<Test>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestBinaryLit {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum TestRecordField {
    TestRecordFieldNamed(TestRecordFieldNamed),
    TestRecordFieldGen(TestRecordFieldGen),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestRecordFieldNamed {
    pub name: StringId,
    pub value: Test,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestRecordFieldGen {
    pub value: Test,
}

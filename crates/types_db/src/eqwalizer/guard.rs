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

use crate::eqwalizer;
use crate::StringId;

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
    pub fn test_var(location: eqwalizer::Pos, v: StringId) -> Self {
        Test::TestVar(TestVar { location, v })
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestVar {
    pub location: eqwalizer::Pos,
    pub v: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestAtom {
    pub location: eqwalizer::Pos,
    pub s: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestNumber {
    pub location: eqwalizer::Pos,
    pub lit: Option<i32>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestTuple {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub elems: Vec<Test>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestString {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestNil {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestCons {
    pub location: eqwalizer::Pos,
    pub h: Box<Test>,
    pub t: Box<Test>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestCall {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub args: Vec<Test>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestRecordCreate {
    pub location: eqwalizer::Pos,
    pub rec_name: StringId,
    #[serde(default)]
    pub fields: Vec<TestRecordField>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestRecordSelect {
    pub location: eqwalizer::Pos,
    pub rec: Box<Test>,
    pub rec_name: StringId,
    pub field_name: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestRecordIndex {
    pub location: eqwalizer::Pos,
    pub rec_name: StringId,
    pub field_name: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestMapCreate {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub kvs: Vec<(Test, Test)>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestMapUpdate {
    pub location: eqwalizer::Pos,
    pub map: Box<Test>,
    #[serde(default)]
    pub kvs: Vec<(Test, Test)>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestUnOp {
    pub location: eqwalizer::Pos,
    pub op: StringId,
    pub arg: Box<Test>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestBinOp {
    pub location: eqwalizer::Pos,
    pub op: StringId,
    pub arg_1: Box<Test>,
    pub arg_2: Box<Test>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TestBinaryLit {
    pub location: eqwalizer::Pos,
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

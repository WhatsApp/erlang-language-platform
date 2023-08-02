/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_syntax::SmolStr;
use serde::Serialize;

use crate::ast;

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Guard {
    pub tests: Vec<Test>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
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

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestVar {
    pub location: ast::Pos,
    pub v: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestAtom {
    pub location: ast::Pos,
    pub s: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestNumber {
    pub location: ast::Pos,
    pub lit: Option<i32>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestTuple {
    pub location: ast::Pos,
    pub elems: Vec<Test>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestString {
    pub location: ast::Pos,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestNil {
    pub location: ast::Pos,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestCons {
    pub location: ast::Pos,
    pub h: Box<Test>,
    pub t: Box<Test>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestCall {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub args: Vec<Test>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestRecordCreate {
    pub location: ast::Pos,
    pub rec_name: SmolStr,
    pub fields: Vec<TestRecordField>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestRecordSelect {
    pub location: ast::Pos,
    pub rec: Box<Test>,
    pub rec_name: SmolStr,
    pub field_name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestRecordIndex {
    pub location: ast::Pos,
    pub rec_name: SmolStr,
    pub field_name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestMapCreate {
    pub location: ast::Pos,
    pub kvs: Vec<(Test, Test)>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestMapUpdate {
    pub location: ast::Pos,
    pub map: Box<Test>,
    pub kvs: Vec<(Test, Test)>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestUnOp {
    pub location: ast::Pos,
    pub op: SmolStr,
    pub arg: Box<Test>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestBinOp {
    pub location: ast::Pos,
    pub op: SmolStr,
    pub arg_1: Box<Test>,
    pub arg_2: Box<Test>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestBinaryLit {
    pub location: ast::Pos,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub enum TestRecordField {
    TestRecordFieldNamed(TestRecordFieldNamed),
    TestRecordFieldGen(TestRecordFieldGen),
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestRecordFieldNamed {
    pub name: SmolStr,
    pub value: Test,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TestRecordFieldGen {
    pub value: Test,
}

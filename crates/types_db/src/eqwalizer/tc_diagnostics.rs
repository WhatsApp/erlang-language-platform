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
use crate::eqwalizer::expr::Expr;
use crate::eqwalizer::types::Type;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    ExpectedSubtype(ExpectedSubtype),
    ExpectedFunType(ExpectedFunType),
    NoDynamicRemoteFun(NoDynamicRemoteFun),
    NoSpecialType(NoSpecialType),
    LambdaArityMismatch(LambdaArityMismatch),
    IndexOutOfBounds(IndexOutOfBounds),
    NotSupportedLambdaInOverloadedCall(NotSupportedLambdaInOverloadedCall),
    UndefinedField(UndefinedField),
    UnboundVar(UnboundVar),
    UnboundRecord(UnboundRecord),
    NonexistentBehaviour(NonexistentBehaviour),
    MissingCallback(MissingCallback),
    IncorrectCallbackReturn(IncorrectCallbackReturn),
    IncorrectCallbackParams(IncorrectCallbackParams),
    RevealTypeHint(RevealTypeHint),
    RedundantFixme(RedundantFixme),
    RedundantNowarnFunction(RedundantNowarnFunction),
    RedundantGuard(RedundantGuard),
    AmbiguousUnion(AmbiguousUnion),
    ClauseNotCovered(ClauseNotCovered),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExpectedSubtype {
    pub location: eqwalizer::Pos,
    pub expr: Expr,
    pub expected: Type,
    pub got: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExpectedFunType {
    pub location: eqwalizer::Pos,
    pub expr: Expr,
    pub expected_arity: u32,
    pub got: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct NoDynamicRemoteFun {
    pub location: eqwalizer::Pos,
    pub expr: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct NoSpecialType {
    pub location: eqwalizer::Pos,
    pub expr: Expr,
    #[serde(default)]
    pub arg_tys: Vec<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct LambdaArityMismatch {
    pub location: eqwalizer::Pos,
    pub expr: Expr,
    pub lambda_arity: u32,
    pub args_arity: u32,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct IndexOutOfBounds {
    pub location: eqwalizer::Pos,
    pub expr: Expr,
    pub index: u32,
    pub tuple_arity: u32,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct NotSupportedLambdaInOverloadedCall {
    pub location: eqwalizer::Pos,
    pub expr: Expr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UndefinedField {
    pub location: eqwalizer::Pos,
    pub rec_name: SmolStr,
    pub field_name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UnboundVar {
    pub location: eqwalizer::Pos,
    pub n: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UnboundRecord {
    pub location: eqwalizer::Pos,
    pub rec: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct NonexistentBehaviour {
    pub location: eqwalizer::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MissingCallback {
    pub location: eqwalizer::Pos,
    pub behaviour_name: SmolStr,
    pub callback: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct IncorrectCallbackReturn {
    pub location: eqwalizer::Pos,
    pub behaviour_name: SmolStr,
    pub callback: SmolStr,
    pub expected: Type,
    pub got: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct IncorrectCallbackParams {
    pub location: eqwalizer::Pos,
    pub behaviour_name: SmolStr,
    pub callback: SmolStr,
    pub param_index: u32,
    pub expected: Type,
    pub got: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RevealTypeHint {
    pub location: eqwalizer::Pos,
    pub t: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RedundantFixme {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RedundantNowarnFunction {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RedundantGuard {
    pub location: eqwalizer::Pos,
    pub variable: SmolStr,
    pub test: Type,
    pub got: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AmbiguousUnion {
    pub location: eqwalizer::Pos,
    pub expr: Expr,
    pub expected: Type,
    pub got: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ClauseNotCovered {
    pub location: eqwalizer::Pos,
}

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
use crate::eqwalizer::types::Type;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    ExpectedSubtype(ExpectedSubtype),
    ExpectedFunType(ExpectedFunType),
    NoSpecialType(NoSpecialType),
    LambdaArityMismatch(LambdaArityMismatch),
    IndexOutOfBounds(IndexOutOfBounds),
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
    AmbiguousUnion(AmbiguousUnion),
    ClauseNotCovered(ClauseNotCovered),
    DynamicLambda(DynamicLambda),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExpectedSubtype {
    pub pos: eqwalizer::Pos,
    pub expected: Type,
    pub got: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExpectedFunType {
    pub pos: eqwalizer::Pos,
    pub expected_arity: u32,
    pub got: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct NoSpecialType {
    pub pos: eqwalizer::Pos,
    #[serde(default)]
    pub arg_tys: Vec<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct DynamicLambda {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct LambdaArityMismatch {
    pub pos: eqwalizer::Pos,
    pub lambda_arity: u32,
    pub args_arity: u32,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct IndexOutOfBounds {
    pub pos: eqwalizer::Pos,
    pub index: u32,
    pub tuple_arity: u32,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UndefinedField {
    pub pos: eqwalizer::Pos,
    pub rec_name: SmolStr,
    pub field_name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UnboundVar {
    pub pos: eqwalizer::Pos,
    pub n: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UnboundRecord {
    pub pos: eqwalizer::Pos,
    pub rec: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct NonexistentBehaviour {
    pub pos: eqwalizer::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct MissingCallback {
    pub pos: eqwalizer::Pos,
    pub behaviour_name: SmolStr,
    pub callback: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct IncorrectCallbackReturn {
    pub pos: eqwalizer::Pos,
    pub behaviour_name: SmolStr,
    pub callback: SmolStr,
    pub expected: Type,
    pub got: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct IncorrectCallbackParams {
    pub pos: eqwalizer::Pos,
    pub behaviour_name: SmolStr,
    pub callback: SmolStr,
    pub param_index: u32,
    pub expected: Type,
    pub got: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RevealTypeHint {
    pub pos: eqwalizer::Pos,
    pub t: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RedundantFixme {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RedundantNowarnFunction {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AmbiguousUnion {
    pub pos: eqwalizer::Pos,
    pub expected: Type,
    pub got: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ClauseNotCovered {
    pub pos: eqwalizer::Pos,
}

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
pub enum Invalid {
    UnknownId(UnknownId),
    NonExportedId(NonExportedId),
    RecursiveConstraint(RecursiveConstraint),
    TyVarWithMultipleConstraints(TyVarWithMultipleConstraints),
    TypeVarInRecordField(TypeVarInRecordField),
    UnboundTyVarInTyDecl(UnboundTyVarInTyDecl),
    RepeatedTyVarInTyDecl(RepeatedTyVarInTyDecl),
    NonProductiveRecursiveTypeAlias(NonProductiveRecursiveTypeAlias),
    TransitiveInvalid(TransitiveInvalid),
    AliasWithNonCovariantParam(AliasWithNonCovariantParam),
    BadMapKey(BadMapKey),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UnknownId {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::RemoteId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct NonExportedId {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::RemoteId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecursiveConstraint {
    pub pos: eqwalizer::Pos,
    pub n: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TyVarWithMultipleConstraints {
    pub pos: eqwalizer::Pos,
    pub n: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TypeVarInRecordField {
    pub pos: eqwalizer::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct UnboundTyVarInTyDecl {
    pub pos: eqwalizer::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RepeatedTyVarInTyDecl {
    pub pos: eqwalizer::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct NonProductiveRecursiveTypeAlias {
    pub pos: eqwalizer::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TransitiveInvalid {
    pos: eqwalizer::Pos,
    name: SmolStr,
    #[serde(default)]
    references: Vec<SmolStr>,
}

impl TransitiveInvalid {
    pub fn new(location: eqwalizer::Pos, name: SmolStr, mut references: Vec<SmolStr>) -> Self {
        references.sort_unstable();
        Self {
            pos: location,
            name,
            references,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AliasWithNonCovariantParam {
    pub pos: eqwalizer::Pos,
    pub name: SmolStr,
    pub type_var: SmolStr,
    #[serde(default)]
    pub exps: Vec<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BadMapKey {
    pub pos: eqwalizer::Pos,
    pub required: bool,
}

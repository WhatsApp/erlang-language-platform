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

use super::types::Type;
use crate::ast;

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub enum Invalid {
    UnknownId(UnknownId),
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

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct UnknownId {
    pub location: ast::Pos,
    pub id: ast::RemoteId,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RecursiveConstraint {
    pub location: ast::Pos,
    pub n: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TyVarWithMultipleConstraints {
    pub location: ast::Pos,
    pub n: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TypeVarInRecordField {
    pub location: ast::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct UnboundTyVarInTyDecl {
    pub location: ast::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RepeatedTyVarInTyDecl {
    pub location: ast::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct NonProductiveRecursiveTypeAlias {
    pub location: ast::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TransitiveInvalid {
    pub location: ast::Pos,
    pub name: SmolStr,
    pub references: Vec<SmolStr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct AliasWithNonCovariantParam {
    pub location: ast::Pos,
    pub name: SmolStr,
    pub type_var: SmolStr,
    pub exps: Vec<Type>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct BadMapKey {
    pub location: ast::Pos,
}

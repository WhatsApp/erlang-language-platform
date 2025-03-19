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
use crate::eqwalizer::expr;
use crate::eqwalizer::ext_types;
use crate::eqwalizer::invalid_diagnostics::Invalid;
use crate::eqwalizer::types::FunType;
use crate::eqwalizer::types::Type;
use crate::eqwalizer::types::VarType;
use crate::eqwalizer::TextRange;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum ExternalForm {
    Module(ModuleAttr),
    CompileExportAll(CompileExportAllAttr),
    Export(ExportAttr),
    Import(ImportAttr),
    ExportType(ExportTypeAttr),
    FunDecl(FunDecl),
    File(FileAttr),
    ElpMetadata(ElpMetadataAttr),
    Behaviour(BehaviourAttr),
    EqwalizerNowarnFunction(EqwalizerNowarnFunctionAttr),
    EqwalizerUnlimitedRefinement(EqwalizerUnlimitedRefinementAttr),
    TypingAttribute(TypingAttribute),
    ExternalTypeDecl(ExternalTypeDecl),
    ExternalOpaqueDecl(ExternalOpaqueDecl),
    ExternalFunSpec(ExternalFunSpec),
    ExternalCallback(ExternalCallback),
    ExternalOptionalCallbacks(ExternalOptionalCallbacks),
    ExternalRecDecl(ExternalRecDecl),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum InvalidForm {
    InvalidTypeDecl(InvalidTypeDecl),
    InvalidFunSpec(InvalidFunSpec),
    InvalidRecDecl(InvalidRecDecl),
    InvalidConvertTypeInRecDecl(InvalidConvertTypeInRecDecl),
    InvalidMapType(InvalidMapType),
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ModuleAttr {
    pub location: eqwalizer::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExportAttr {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub funs: Vec<eqwalizer::Id>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ImportAttr {
    pub location: eqwalizer::Pos,
    pub module: SmolStr,
    #[serde(default)]
    pub funs: Vec<eqwalizer::Id>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExportTypeAttr {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub types: Vec<eqwalizer::Id>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FunDecl {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub clauses: Vec<expr::Clause>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FileAttr {
    pub location: eqwalizer::Pos,
    pub file: SmolStr,
    pub start: u32,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ElpMetadataAttr {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub fixmes: Vec<Fixme>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Fixme {
    pub comment: TextRange,
    pub suppression: TextRange,
    pub is_ignore: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BehaviourAttr {
    pub location: eqwalizer::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct EqwalizerNowarnFunctionAttr {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct EqwalizerUnlimitedRefinementAttr {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FunSpec {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    pub ty: FunType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct OverloadedFunSpec {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub tys: Vec<FunType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Callback {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub tys: Vec<FunType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecDecl {
    pub location: eqwalizer::Pos,
    pub name: SmolStr,
    #[serde(default)]
    pub fields: Vec<RecField>,
    pub refinable: bool,
    pub file: Option<SmolStr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecField {
    pub name: SmolStr,
    pub tp: Option<Type>,
    pub default_value: Option<expr::Expr>,
    pub refinable: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TypeDecl {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub params: Vec<VarType>,
    pub body: Type,
    pub file: Option<SmolStr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct CompileExportAllAttr {
    pub location: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TypingAttribute {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub names: Vec<SmolStr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalTypeDecl {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub params: Vec<SmolStr>,
    pub body: ext_types::ExtType,
    pub file: Option<SmolStr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalOpaqueDecl {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub params: Vec<SmolStr>,
    pub body: ext_types::ExtType,
    pub file: Option<SmolStr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalFunSpec {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub types: Vec<ext_types::ConstrainedFunType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalCallback {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub types: Vec<ext_types::ConstrainedFunType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalOptionalCallbacks {
    pub location: eqwalizer::Pos,
    #[serde(default)]
    pub ids: Vec<eqwalizer::Id>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalRecDecl {
    pub location: eqwalizer::Pos,
    pub name: SmolStr,
    #[serde(default)]
    pub fields: Vec<ExternalRecField>,
    pub file: Option<SmolStr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalRecField {
    pub name: SmolStr,
    pub tp: Option<ext_types::ExtType>,
    pub default_value: Option<expr::Expr>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct InvalidTypeDecl {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    pub te: Invalid,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct InvalidFunSpec {
    pub location: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    pub te: Invalid,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct InvalidRecDecl {
    pub location: eqwalizer::Pos,
    pub name: SmolStr,
    pub te: Invalid,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct InvalidConvertTypeInRecDecl {
    pub location: eqwalizer::Pos,
    pub name: SmolStr,
    pub te: Invalid,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct InvalidMapType {
    pub location: eqwalizer::Pos,
    pub te: Invalid,
}

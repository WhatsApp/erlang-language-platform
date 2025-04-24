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
use crate::eqwalizer::TextRange;
use crate::eqwalizer::expr;
use crate::eqwalizer::ext_types;
use crate::eqwalizer::types::FunType;
use crate::eqwalizer::types::Type;
use crate::eqwalizer::types::VarType;

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
pub struct ModuleAttr {
    pub pos: eqwalizer::Pos,
    pub name: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExportAttr {
    pub pos: eqwalizer::Pos,
    #[serde(default)]
    pub funs: Vec<eqwalizer::Id>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ImportAttr {
    pub pos: eqwalizer::Pos,
    pub module: StringId,
    #[serde(default)]
    pub funs: Vec<eqwalizer::Id>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExportTypeAttr {
    pub pos: eqwalizer::Pos,
    #[serde(default)]
    pub types: Vec<eqwalizer::Id>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FunDecl {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub clauses: Vec<expr::Clause>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FileAttr {
    pub pos: eqwalizer::Pos,
    pub file: StringId,
    pub start: u32,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ElpMetadataAttr {
    pub pos: eqwalizer::Pos,
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
    pub pos: eqwalizer::Pos,
    pub name: StringId,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct EqwalizerNowarnFunctionAttr {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::Id,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct EqwalizerUnlimitedRefinementAttr {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::Id,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FunSpec {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    pub ty: FunType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct OverloadedFunSpec {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub tys: Vec<FunType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Callback {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub tys: Vec<FunType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecDecl {
    pub pos: eqwalizer::Pos,
    pub name: StringId,
    #[serde(default)]
    pub fields: Vec<RecField>,
    pub refinable: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecField {
    pub name: StringId,
    pub tp: Type,
    pub default_value: Option<expr::Expr>,
    pub refinable: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TypeDecl {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub params: Vec<VarType>,
    pub body: Type,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct CompileExportAllAttr {
    pub pos: eqwalizer::Pos,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TypingAttribute {
    pub pos: eqwalizer::Pos,
    #[serde(default)]
    pub names: Vec<StringId>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalTypeDecl {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub params: Vec<StringId>,
    pub body: ext_types::ExtType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalOpaqueDecl {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub params: Vec<StringId>,
    pub body: ext_types::ExtType,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalFunSpec {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub types: Vec<ext_types::ConstrainedFunType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalCallback {
    pub pos: eqwalizer::Pos,
    pub id: eqwalizer::Id,
    #[serde(default)]
    pub types: Vec<ext_types::ConstrainedFunType>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalOptionalCallbacks {
    pub pos: eqwalizer::Pos,
    #[serde(default)]
    pub ids: Vec<eqwalizer::Id>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalRecDecl {
    pub pos: eqwalizer::Pos,
    pub name: StringId,
    #[serde(default)]
    pub fields: Vec<ExternalRecField>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalRecField {
    pub name: StringId,
    pub tp: Option<ext_types::ExtType>,
    pub default_value: Option<expr::Expr>,
}

impl TypeDecl {
    pub fn to_bytes(&self) -> Vec<u8> {
        serde_json::to_vec(self).unwrap()
    }
}

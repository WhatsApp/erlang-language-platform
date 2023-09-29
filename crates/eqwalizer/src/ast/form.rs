/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use ast::expr;
use ast::ext_types;
use ast::types;
use elp_syntax::SmolStr;
use serde::Serialize;

use super::invalid_diagnostics::Invalid;
use crate::ast;

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub enum InternalForm {
    Module(ModuleAttr),
    Export(ExportAttr),
    Import(ImportAttr),
    ExportType(ExportTypeAttr),
    FunDecl(FunDecl),
    File(FileAttr),
    ElpMetadata(ElpMetadataAttr),
    Behaviour(BehaviourAttr),
    EqwalizerNowarnFunction(EqwalizerNowarnFunctionAttr),
    EqwalizerUnlimitedRefinement(EqwalizerUnlimitedRefinementAttr),
    FunSpec(FunSpec),
    OverloadedFunSpec(OverloadedFunSpec),
    Callback(Callback),
    RecDecl(RecDecl),
    OpaqueTypeDecl(OpaqueTypeDecl),
    TypeDecl(TypeDecl),
    InvalidForm(InvalidForm),
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
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

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub enum InvalidForm {
    InvalidTypeDecl(InvalidTypeDecl),
    InvalidFunSpec(InvalidFunSpec),
    InvalidRecDecl(InvalidRecDecl),
    InvalidConvertTypeInRecDecl(InvalidConvertTypeInRecDecl),
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ModuleAttr {
    pub location: ast::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ExportAttr {
    pub location: ast::Pos,
    pub funs: Vec<ast::Id>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ImportAttr {
    pub location: ast::Pos,
    pub module: SmolStr,
    pub funs: Vec<ast::Id>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ExportTypeAttr {
    pub location: ast::Pos,
    pub types: Vec<ast::Id>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct FunDecl {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub clauses: Vec<expr::Clause>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct FileAttr {
    pub location: ast::Pos,
    pub file: SmolStr,
    pub start: u32,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ElpMetadataAttr {
    pub location: ast::Pos,
    pub fixmes: Vec<Fixme>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Fixme {
    pub comment: ast::TextRange,
    pub suppression: ast::TextRange,
    pub is_ignore: bool,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct BehaviourAttr {
    pub location: ast::Pos,
    pub name: SmolStr,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct EqwalizerNowarnFunctionAttr {
    pub location: ast::Pos,
    pub id: ast::Id,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct EqwalizerUnlimitedRefinementAttr {
    pub location: ast::Pos,
    pub id: ast::Id,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct FunSpec {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub ty: types::FunType,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct OverloadedFunSpec {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub tys: Vec<types::FunType>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct Callback {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub tys: Vec<types::FunType>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RecDecl {
    pub location: ast::Pos,
    pub name: SmolStr,
    pub fields: Vec<RecField>,
    pub refinable: bool,
    pub file: Option<SmolStr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct RecField {
    pub name: SmolStr,
    pub tp: Option<types::Type>,
    pub default_value: Option<expr::Expr>,
    pub refinable: bool,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct OpaqueTypeDecl {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub file: Option<SmolStr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TypeDecl {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub params: Vec<types::VarType>,
    pub body: types::Type,
    pub file: Option<SmolStr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct CompileExportAllAttr {
    pub location: ast::Pos,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct TypingAttribute {
    pub location: ast::Pos,
    pub names: Vec<SmolStr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalTypeDecl {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub params: Vec<SmolStr>,
    pub body: ext_types::ExtType,
    pub file: Option<SmolStr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalOpaqueDecl {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub params: Vec<SmolStr>,
    pub body: ext_types::ExtType,
    pub file: Option<SmolStr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalFunSpec {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub types: Vec<ext_types::ConstrainedFunType>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalCallback {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub types: Vec<ext_types::ConstrainedFunType>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalOptionalCallbacks {
    pub location: ast::Pos,
    pub ids: Vec<ast::Id>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalRecDecl {
    pub location: ast::Pos,
    pub name: SmolStr,
    pub fields: Vec<ExternalRecField>,
    pub file: Option<SmolStr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct ExternalRecField {
    pub name: SmolStr,
    pub tp: Option<ext_types::ExtType>,
    pub default_value: Option<expr::Expr>,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct InvalidTypeDecl {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub te: Invalid,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct InvalidFunSpec {
    pub location: ast::Pos,
    pub id: ast::Id,
    pub te: Invalid,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct InvalidRecDecl {
    pub location: ast::Pos,
    pub name: SmolStr,
    pub te: Invalid,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
pub struct InvalidConvertTypeInRecDecl {
    pub location: ast::Pos,
    pub name: SmolStr,
    pub te: Invalid,
}

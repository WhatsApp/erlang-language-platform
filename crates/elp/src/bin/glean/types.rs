/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide::TextRange;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use serde::Serialize;

#[derive(Serialize, Debug, Eq, Hash, PartialEq, Clone)]
pub(crate) struct GleanFileId(u32);

impl From<GleanFileId> for FileId {
    fn from(val: GleanFileId) -> Self {
        FileId::from_raw(val.0 - 1)
    }
}

impl From<FileId> for GleanFileId {
    fn from(value: FileId) -> Self {
        GleanFileId(value.index() + 1)
    }
}

#[derive(Serialize, Debug)]
pub(crate) struct FileFact {
    #[serde(rename = "id")]
    pub(crate) file_id: GleanFileId,
    #[serde(rename = "key")]
    pub(crate) file_path: String,
}

impl FileFact {
    pub(crate) fn new(file_id: FileId, file_path: String) -> Self {
        Self {
            file_id: file_id.into(),
            file_path,
        }
    }
}

#[derive(Serialize, Debug)]
pub(crate) struct FileLinesFact {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) lengths: Vec<u32>,
    #[serde(rename = "endsInNewline")]
    pub(crate) ends_with_new_line: bool,
    #[serde(rename = "hasUnicodeOrTabs")]
    unicode_or_tabs: bool,
}

impl FileLinesFact {
    pub(crate) fn new(file_id: FileId, lengths: Vec<u32>, ends_with_new_line: bool) -> Self {
        Self {
            file_id: file_id.into(),
            lengths,
            ends_with_new_line,
            unicode_or_tabs: true,
        }
    }
}

#[derive(Serialize, Debug)]
pub(crate) struct ModuleFact {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) oncall: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) exports: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) behaviours: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) module_doc: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) exdoc_link: Option<String>,
}

impl ModuleFact {
    pub(crate) fn new(
        file_id: FileId,
        name: String,
        oncall: Option<String>,
        exports: Option<Vec<String>>,
        behaviours: Option<Vec<String>>,
        module_doc: Option<String>,
        exdoc_link: Option<String>,
    ) -> Self {
        Self {
            file_id: file_id.into(),
            name,
            oncall,
            exports,
            behaviours,
            module_doc,
            exdoc_link,
        }
    }
}

#[derive(Serialize, Debug)]
pub(crate) struct FunctionDeclarationFact {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) fqn: MFA,
    pub(crate) span: Location,
}

#[derive(Serialize, Debug)]
pub(crate) struct XRefFact {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) xrefs: Vec<XRefFactVal>,
}

#[derive(Serialize, Debug)]
pub(crate) struct XRefFactVal {
    pub(crate) source: Location,
    pub(crate) target: MFA,
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
pub(crate) struct MFA {
    pub(crate) module: String,
    pub(crate) name: String,
    pub(crate) arity: u32,
    #[serde(skip_serializing)]
    pub(crate) file_id: GleanFileId,
}

#[derive(Serialize, Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct Location {
    pub(crate) start: u32,
    pub(crate) length: u32,
}

impl From<Location> for TextRange {
    fn from(val: Location) -> Self {
        TextRange::at(val.start.into(), val.length.into())
    }
}

#[derive(Serialize, Debug)]
#[serde(tag = "predicate")]
pub(crate) enum Fact {
    #[serde(rename = "src.File")]
    File { facts: Vec<FileFact> },
    #[serde(rename = "src.FileLines")]
    FileLine { facts: Vec<Key<FileLinesFact>> },
    #[serde(rename = "erlang.FunctionDeclaration")]
    FunctionDeclaration {
        facts: Vec<Key<FunctionDeclarationFact>>,
    },
    #[serde(rename = "erlang.XRefsViaFqnByFile")]
    XRef { facts: Vec<Key<XRefFact>> },
    #[serde(rename = "erlang.DeclarationComment")]
    DeclarationComment { facts: Vec<Key<CommentFact>> },
    #[serde(rename = "erlang.Module")]
    Module { facts: Vec<Key<ModuleFact>> },
}

#[derive(Serialize, Debug)]
pub(crate) struct XRefFile {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) xrefs: Vec<XRef>,
}

#[derive(Serialize, Debug)]
pub(crate) struct XRef {
    pub(crate) source: Location,
    pub(crate) target: XRefTarget,
}

#[derive(Serialize, Debug)]
pub(crate) enum XRefTarget {
    #[serde(rename = "func")]
    Function(Key<FunctionTarget>),
    #[serde(rename = "macro")]
    Macro(Key<MacroTarget>),
    #[serde(rename = "header")]
    Header(Key<HeaderTarget>),
    #[serde(rename = "record")]
    Record(Key<RecordTarget>),
    #[serde(rename = "ttype")]
    Type(Key<TypeTarget>),
    #[serde(rename = "varr")]
    Var(Key<VarTarget>),
}

impl XRefTarget {
    pub(crate) fn file_id(&self) -> &GleanFileId {
        match self {
            XRefTarget::Function(xref) => &xref.key.file_id,
            XRefTarget::Macro(xref) => &xref.key.file_id,
            XRefTarget::Header(xref) => &xref.key.file_id,
            XRefTarget::Record(xref) => &xref.key.file_id,
            XRefTarget::Type(xref) => &xref.key.file_id,
            XRefTarget::Var(xref) => &xref.key.file_id,
        }
    }
}

#[derive(Serialize, Debug)]
pub(crate) struct FunctionTarget {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) name: String,
    pub(crate) arity: u32,
}

#[derive(Serialize, Debug)]
pub(crate) struct TaggedUrl {
    pub tag: String,
    pub display_name: String,
    pub url: String,
}

#[derive(Serialize, Debug)]
pub(crate) struct MacroTarget {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) arity: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) expansion: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub(crate) tagged_urls: Vec<TaggedUrl>,
}

#[derive(Serialize, Debug)]
pub(crate) struct HeaderTarget {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) name: String,
}

#[derive(Serialize, Debug)]
pub(crate) struct RecordTarget {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) name: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub(crate) tagged_urls: Vec<TaggedUrl>,
}

#[derive(Serialize, Debug)]
pub(crate) struct TypeTarget {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) name: String,
    pub(crate) arity: u32,
}

#[derive(Serialize, Debug)]
pub(crate) struct VarTarget {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) name: String,
}

#[derive(Serialize, Debug)]
pub(crate) struct FileDeclaration {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) declarations: Vec<Declaration>,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct Key<T> {
    pub(crate) key: T,
}

impl<T> From<T> for Key<T> {
    fn from(item: T) -> Self {
        Key { key: item }
    }
}

#[derive(Serialize, Debug)]
pub(crate) struct CommentFact {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) declaration: Key<FunctionDeclarationFact>,
    pub(crate) span: Location,
    pub(crate) text: String,
}

#[derive(Serialize, Debug, Clone)]
#[allow(clippy::enum_variant_names)]
pub(crate) enum Declaration {
    #[serde(rename = "func")]
    FunctionDeclaration(Key<FuncDecl>),
    #[serde(rename = "macro")]
    MacroDeclaration(Key<MacroDecl>),
    #[serde(rename = "ttype")]
    TypeDeclaration(Key<TypeDecl>),
    #[serde(rename = "record")]
    RecordDeclaration(Key<RecordDecl>),
    #[serde(rename = "varr")]
    VarDeclaration(Key<VarDecl>),
    #[serde(rename = "header")]
    HeaderDeclaration(Key<HeaderDecl>),
    #[serde(rename = "doc")]
    DocDeclaration(Key<DocDecl>),
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct FuncDecl {
    pub(crate) name: String,
    pub(crate) arity: u32,
    pub(crate) span: Location,
    pub(crate) exported: bool,
    pub(crate) deprecated: bool,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct MacroDecl {
    pub(crate) name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) arity: Option<u32>,
    pub(crate) span: Location,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct TypeDecl {
    pub(crate) name: String,
    pub(crate) arity: u32,
    pub(crate) span: Location,
    pub(crate) exported: bool,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct RecordDecl {
    pub(crate) name: String,
    pub(crate) span: Location,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct VarDecl {
    pub(crate) name: String,
    pub(crate) doc: String,
    pub(crate) span: Location,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct HeaderDecl {
    pub(crate) name: String,
    pub(crate) span: Location,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct DocDecl {
    pub(crate) target: Box<Declaration>,
    pub(crate) span: Location,
    pub(crate) text: String,
}

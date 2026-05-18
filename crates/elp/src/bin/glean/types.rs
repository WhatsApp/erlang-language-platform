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
use serde::Serializer;

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

#[derive(Serialize, Debug, Clone)]
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

    pub(crate) fn new_from_glean_id(file_id: GleanFileId, file_path: String) -> Self {
        Self { file_id, file_path }
    }
}

#[derive(Serialize, Debug, Clone)]
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

#[derive(Serialize, Debug, Clone)]
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
    pub(crate) module_doc: Option<ModuleDocComment>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) exdoc_link: Option<String>,
    #[serde(skip)]
    pub(crate) module_span: Option<Location>,
    #[serde(skip)]
    pub(crate) callbacks: Vec<CallbackInfo>,
    #[serde(skip)]
    pub(crate) compile_options: Vec<String>,
    #[serde(skip)]
    pub(crate) on_load_fns: Vec<String>,
    #[serde(skip)]
    pub(crate) nif_fns: Vec<(String, u32)>,
    #[serde(skip)]
    pub(crate) included_files: Vec<(GleanFileId, String)>,
    #[serde(skip)]
    pub(crate) record_fields: Vec<RecordFieldInfo>,
    #[serde(skip)]
    pub(crate) record_def_texts: Vec<RecordDefText>,
    #[serde(skip)]
    pub(crate) behaviour_callback_stubs: Vec<BehaviourCallbackStub>,
}

#[derive(Debug, Clone)]
pub(crate) struct BehaviourCallbackStub {
    pub(crate) callback_name: String,
    pub(crate) callback_arity: u32,
    pub(crate) behaviour_module: String,
    pub(crate) behaviour_app: String,
}

#[derive(Debug, Clone)]
pub(crate) struct RecordDefText {
    pub(crate) name: String,
    pub(crate) definition_text: String,
}

#[derive(Debug, Clone)]
pub(crate) struct RecordFieldInfo {
    pub(crate) record_name: String,
    pub(crate) field_name: String,
    pub(crate) span: Location,
}

#[derive(Debug, Clone)]
pub(crate) struct CallbackInfo {
    pub(crate) name: String,
    pub(crate) arity: u32,
    pub(crate) optional: bool,
    pub(crate) span: Location,
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

#[derive(Debug, Clone)]
pub(crate) struct ModuleDocComment {
    pub(crate) text: String,
    pub(crate) span: Location,
}

impl Serialize for ModuleDocComment {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.text.serialize(serializer)
    }
}

#[derive(Serialize, Debug)]
#[serde(tag = "predicate")]
pub(crate) enum Fact {
    #[serde(rename = "src.File")]
    File { facts: Vec<FileFact> },
    #[serde(rename = "src.FileLines")]
    FileLine { facts: Vec<Key<FileLinesFact>> },
    #[serde(rename = "erlang.FunctionDeclaration.2")]
    FuncDecl2 { facts: Vec<Key<Schema2FuncDecl>> },
    #[serde(rename = "erlang.MacroDeclaration.2")]
    MacroDecl2 { facts: Vec<Key<Schema2MacroDecl>> },
    #[serde(rename = "erlang.RecordDeclaration.2")]
    RecordDecl2 { facts: Vec<Key<Schema2RecordDecl>> },
    #[serde(rename = "erlang.TypeDeclaration.2")]
    TypeDecl2 { facts: Vec<Key<Schema2TypeDecl>> },
    #[serde(rename = "erlang.HeaderDeclaration.2")]
    HeaderDecl2 { facts: Vec<Key<Schema2HeaderDecl>> },
    #[serde(rename = "erlang.CallbackDeclaration.2")]
    CallbackDecl2 {
        facts: Vec<Key<Schema2CallbackDecl>>,
    },
    #[serde(rename = "erlang.RecordFieldDeclaration.2")]
    RecordFieldDecl2 {
        facts: Vec<Key<Schema2RecordFieldDecl>>,
    },
    #[serde(rename = "erlang.ModuleDeclaration.2")]
    Module2 { facts: Vec<Key<Schema2ModuleDecl>> },
    #[serde(rename = "erlang.MacroUsage.2")]
    MacroUsage2 { facts: Vec<Key<Schema2MacroUsage>> },
    #[serde(rename = "erlang.VarDeclaration.2")]
    VarDecl2 { facts: Vec<Key<Schema2VarDecl>> },
    #[serde(rename = "erlang.FunctionDefinition.2")]
    FuncDef2 { facts: Vec<Key<Schema2FuncDef>> },
    #[serde(rename = "erlang.MacroDefinition.2")]
    MacroDef2 { facts: Vec<Key<Schema2MacroDef>> },
    #[serde(rename = "erlang.RecordDefinition.2")]
    RecordDef2 { facts: Vec<Key<Schema2RecordDef>> },
    #[serde(rename = "erlang.TypeDefinition.2")]
    TypeDef2 { facts: Vec<Key<Schema2TypeDef>> },
    #[serde(rename = "erlang.CallbackDefinition.2")]
    CallbackDef2 { facts: Vec<Key<Schema2CallbackDef>> },
    #[serde(rename = "erlang.ModuleDefinition.2")]
    ModuleDef2 { facts: Vec<Key<Schema2ModuleDef>> },
    #[serde(rename = "erlang.DeclarationLocation.2")]
    DeclLocation2 {
        facts: Vec<Key<Schema2DeclLocation>>,
    },
    #[serde(rename = "erlang.MacroUsageLocation.2")]
    MacroUsageLocation2 {
        facts: Vec<Key<Schema2MacroUsageLocation>>,
    },
    #[serde(rename = "erlang.VarLocation.2")]
    VarLocation2 { facts: Vec<Key<Schema2VarLocation>> },
    #[serde(rename = "erlang.XRefsByFile.2")]
    TypedXRefs2 { facts: Vec<Key<Schema2XRefsByFile>> },
    #[serde(rename = "erlang.VarXRefsByFile.2")]
    VarXRefs2 {
        facts: Vec<Key<Schema2VarXRefsByFile>>,
    },
    #[serde(rename = "erlang.FileDeclarations.2")]
    FileDecls2 {
        facts: Vec<Key<Schema2FileDeclarations>>,
    },
    #[serde(rename = "erlang.DeclarationComment.2")]
    DeclComment2 { facts: Vec<Key<Schema2CommentFact>> },
    #[serde(rename = "erlang.FileIncludes.2")]
    FileIncludes2 {
        facts: Vec<Key<Schema2FileIncludes>>,
    },
    #[serde(rename = "erlang.DeclarationTarget.2")]
    DeclTarget2 {
        facts: Vec<Key<Schema2DeclarationTarget>>,
    },
    #[serde(rename = "erlang.AppInfo.2")]
    AppInfo2 { facts: Vec<Key<Schema2AppInfo>> },
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct Schema2AppInfo {
    pub(crate) name: String,
    pub(crate) type_: Schema2AppType,
}

/// Mirrors the `enum { FirstParty | Otp | ThirdParty }` field on
/// `erlang.AppInfo.2`. Keep the order in sync with the
/// schema in `fbcode/glean/schema/source/erlang.angle`.
#[repr(u32)]
#[derive(Debug, Clone, Copy)]
pub(crate) enum Schema2AppType {
    FirstParty = 0,
    Otp = 1,
    ThirdParty = 2,
}

impl Serialize for Schema2AppType {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        (*self as u32).serialize(serializer)
    }
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct XRefFile {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) xrefs: Vec<XRef>,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct XRef {
    pub(crate) source: Location,
    pub(crate) target: XRefTarget,
    #[serde(skip)]
    pub(crate) caller: Option<(GleanFileId, String, u32)>,
}

#[derive(Serialize, Debug, Clone)]
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
    #[serde(skip)]
    RecordField(Key<RecordFieldTarget>),
    #[serde(skip)]
    Callback(Key<CallbackTarget>),
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct RecordFieldTarget {
    pub(crate) file_id: GleanFileId,
    pub(crate) record_name: String,
    pub(crate) field_name: String,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct CallbackTarget {
    pub(crate) file_id: GleanFileId,
    pub(crate) name: String,
    pub(crate) arity: u32,
    pub(crate) span: Location,
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
            XRefTarget::RecordField(xref) => &xref.key.file_id,
            XRefTarget::Callback(xref) => &xref.key.file_id,
        }
    }
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct FunctionTarget {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) name: String,
    pub(crate) arity: u32,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct TaggedUrl {
    pub tag: String,
    pub display_name: String,
    pub url: String,
}

#[derive(Serialize, Debug, Clone)]
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

#[derive(Serialize, Debug, Clone)]
pub(crate) struct HeaderTarget {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) name: String,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct RecordTarget {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) name: String,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub(crate) tagged_urls: Vec<TaggedUrl>,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct TypeTarget {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) name: String,
    pub(crate) arity: u32,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct VarTarget {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) name: String,
    #[serde(skip)]
    pub(crate) decl_span_start: Option<u32>,
}

#[derive(Serialize, Debug, Clone)]
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
    #[serde(skip)]
    pub(crate) deprecated_desc: Option<String>,
    #[serde(skip)]
    pub(crate) spec_text: Option<String>,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct MacroDecl {
    pub(crate) name: String,
    pub(crate) arity: Option<u32>,
    pub(crate) span: Location,
    #[serde(skip)]
    pub(crate) definition_text: Option<String>,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct TypeDecl {
    pub(crate) name: String,
    pub(crate) arity: u32,
    pub(crate) span: Location,
    pub(crate) exported: bool,
    #[serde(skip)]
    pub(crate) opaque: bool,
    #[serde(skip)]
    pub(crate) definition_text: Option<String>,
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
    pub(crate) type_text: Option<String>,
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

// ── erlang.2 output types ──────────────────────────────────────────

#[derive(Serialize, Debug, Clone)]
pub(crate) enum Schema2Declaration {
    #[serde(rename = "func")]
    Func(Key<Schema2FuncDecl>),
    #[serde(rename = "macro_")]
    Macro(Key<Schema2MacroDecl>),
    #[serde(rename = "record_")]
    Record(Key<Schema2RecordDecl>),
    #[serde(rename = "type_")]
    Type(Key<Schema2TypeDecl>),
    #[serde(rename = "header_")]
    Header(Key<Schema2HeaderDecl>),
    #[serde(rename = "callback_")]
    Callback(Key<Schema2CallbackDecl>),
    #[serde(rename = "record_field")]
    RecordField(Key<Schema2RecordFieldDecl>),
    #[serde(rename = "module_")]
    Module(Key<Schema2ModuleDecl>),
}

// ── Declaration identity types ──

#[derive(Serialize, Debug, Clone)]
pub(crate) struct Schema2Fqn {
    pub(crate) module: String,
    pub(crate) name: String,
    pub(crate) arity: u32,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct Schema2FuncDecl {
    pub(crate) fqn: Schema2Fqn,
    pub(crate) app: String,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct Schema2MacroDecl {
    pub(crate) name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) arity: Option<u32>,
    pub(crate) module: String,
    pub(crate) app: String,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct Schema2RecordDecl {
    pub(crate) name: String,
    pub(crate) module: String,
    pub(crate) app: String,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct Schema2TypeDecl {
    pub(crate) name: String,
    pub(crate) arity: u32,
    pub(crate) module: String,
    pub(crate) app: String,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct Schema2HeaderDecl {
    pub(crate) name: String,
    pub(crate) app: String,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct Schema2CallbackDecl {
    pub(crate) name: String,
    pub(crate) arity: u32,
    pub(crate) module: String,
    pub(crate) app: String,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct Schema2RecordFieldDecl {
    pub(crate) record_name: String,
    pub(crate) field_name: String,
    pub(crate) module: String,
    pub(crate) app: String,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct Schema2ModuleDecl {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) name: String,
    pub(crate) app: String,
}

// ── Definition types ──

#[derive(Serialize, Debug)]
pub(crate) struct Schema2FuncDef {
    pub(crate) declaration: Key<Schema2FuncDecl>,
    pub(crate) exported: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) deprecated: Option<String>,
    pub(crate) is_on_load: bool,
    pub(crate) is_nif: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) spec_text: Option<String>,
}

#[derive(Serialize, Debug)]
pub(crate) struct Schema2MacroDef {
    pub(crate) declaration: Key<Schema2MacroDecl>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) definition_text: Option<String>,
}

#[derive(Serialize, Debug)]
pub(crate) struct Schema2RecordDef {
    pub(crate) declaration: Key<Schema2RecordDecl>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) definition_text: Option<String>,
}

#[derive(Serialize, Debug)]
pub(crate) struct Schema2TypeDef {
    pub(crate) declaration: Key<Schema2TypeDecl>,
    pub(crate) exported: bool,
    pub(crate) opaque: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) definition_text: Option<String>,
}

#[derive(Serialize, Debug)]
pub(crate) struct Schema2CallbackDef {
    pub(crate) declaration: Key<Schema2CallbackDecl>,
    pub(crate) optional_: bool,
}

#[derive(Serialize, Debug)]
pub(crate) struct Schema2ModuleDef {
    pub(crate) declaration: Key<Schema2ModuleDecl>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) oncall: Option<String>,
    pub(crate) exports: Vec<Key<Schema2FuncDecl>>,
    pub(crate) behaviours: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) exdoc_link: Option<String>,
    pub(crate) compile_options: Vec<String>,
}

// ── File-level types ──

#[derive(Serialize, Debug)]
pub(crate) struct Schema2FileIncludes {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) included: GleanFileId,
}

#[derive(Serialize, Debug)]
pub(crate) struct Schema2DeclarationTarget {
    pub(crate) source: Schema2Declaration,
    pub(crate) target: Schema2Declaration,
}

// ── Location types ──

#[derive(Serialize, Debug)]
pub(crate) struct Schema2DeclLocation {
    pub(crate) declaration: Schema2Declaration,
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) span: Location,
}

#[derive(Serialize, Debug)]
pub(crate) struct Schema2MacroUsageLocation {
    pub(crate) usage: Key<Schema2MacroUsage>,
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) span: Location,
}

#[derive(Serialize, Debug)]
pub(crate) struct Schema2VarLocation {
    pub(crate) var_: Key<Schema2VarDecl>,
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) span: Location,
}

// ── MacroUsage + VarDeclaration ──

#[derive(Serialize, Debug, Clone)]
pub(crate) struct Schema2MacroUsage {
    pub(crate) name: String,
    pub(crate) module: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) arity: Option<u32>,
    pub(crate) app: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) expansion: Option<String>,
    pub(crate) links: Vec<String>,
    pub(crate) content_hash: String,
}

#[derive(Serialize, Debug, Clone)]
pub(crate) struct Schema2VarDecl {
    pub(crate) name: String,
    pub(crate) module: String,
    pub(crate) app: String,
    pub(crate) span_start: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) type_text: Option<String>,
}

// ── XRef types ──

#[derive(Serialize, Debug)]
pub(crate) struct Schema2XRef {
    pub(crate) target: Schema2Declaration,
    pub(crate) source: Location,
}

#[derive(Serialize, Debug)]
pub(crate) struct Schema2XRefsByFile {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) xrefs: Vec<Schema2XRef>,
}

// ── Variable xrefs ──

#[derive(Serialize, Debug)]
pub(crate) struct Schema2VarXRef {
    pub(crate) target_name: String,
    pub(crate) target_span_start: u32,
    pub(crate) sources: Vec<Location>,
}

#[derive(Serialize, Debug)]
pub(crate) struct Schema2VarXRefsByFile {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) xrefs: Vec<Schema2VarXRef>,
}

// ── Bulk file index ──

#[derive(Serialize, Debug)]
pub(crate) struct Schema2FileDeclarations {
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) declarations: Vec<Schema2Declaration>,
}

// ── Comment ──

#[derive(Serialize, Debug)]
pub(crate) struct Schema2CommentFact {
    pub(crate) declaration: Schema2Declaration,
    #[serde(rename = "file")]
    pub(crate) file_id: GleanFileId,
    pub(crate) span: Location,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) text: Option<String>,
}

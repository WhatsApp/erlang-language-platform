/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A simplified AST that only contains forms.
//!
//! This is the primary IR used throughout `hir`.
//!
//! `FormList`s are built per `FileId`, from the syntax tree of the parsed file. This means that
//! they are location-independent: they don't know which macros are active or which module or header
//! they belong to, since those concepts don't exist at this level (a single `FormList` might be part
//! of multiple modules, or might be included into the same module twice via `-include`).
//!
//! One important purpose of this layer is to provide an "invalidation barrier" for incremental
//! computations: when typing inside a form body, the `FormList` of the modified file is typically
//! unaffected, so we don't have to recompute name resolution results or form data (see `data.rs`).
//!
//! TODO: The `FormList` for the currently open file can be displayed by using the VS Code command
//! "ELP: Debug FormList".
//!
//! Compared to erlc's architecture, `FormList` has properties represented both
//! in EEP (like `-define`), Erlang Abstract Forms (like functions),
//! but also logically extracted from wildcard attributes (like `-on_load`).
//! Many syntax-level Erlang features are already desugared to simpler forms in the `FormList`,
//! but name resolution has not yet been performed. `FormList`s are per-file, while Abstract Forms are
//! per module, because we are interested in incrementally computing it.
//!
//! The representation of items in the `FormList` should generally mirror the surface syntax: it is
//! usually a bad idea to desugar a syntax-level construct to something that is structurally
//! different here. Name resolution needs to be able to process attributes and expand macros
//! and having a 1-to-1 mapping between syntax and the `FormList` avoids introducing subtle bugs.
//!
//! In general, any item in the `FormList` stores its `FormId`, which allows mapping it back to its
//! surface syntax.

// Note:
// We use `FormId` to form a stable reference to a form in a file from
// other files. e.g. for types, remote calls, etc.  But we also use
// the form list for navigation when working on the file itself.  The
// crucial difference in these two cases is that for the latter we can
// rely on the `SyntaxNode` of a form being the same, as it is the same
// file.  This allows us to speed up operations by caching a map from
// the form `SyntaxNode` to the `Form` itself.

use std::fmt;
use std::ops::Deref;
use std::ops::Index;
use std::sync::Arc;

use elp_base_db::FileId;
use elp_syntax::ast;
use elp_syntax::AstPtr;
use elp_syntax::SmolStr;
use fxhash::FxHashMap;
use la_arena::Arena;
use la_arena::Idx;
use la_arena::IdxRange;
use profile::Count;

use crate::db::MinDefDatabase;
use crate::Diagnostic;
use crate::MacroName;
use crate::Name;
use crate::NameArity;

mod form_id;
mod lower;
mod pretty;
#[cfg(test)]
mod tests;

pub use form_id::FormId;

#[derive(Debug, Eq, PartialEq)]
pub struct FormList {
    _c: Count<Self>,
    data: Box<FormListData>,
    forms: Vec<FormIdx>,
    diagnostics: Vec<Diagnostic>,
    // Map from the range of a form to its index
    map_back: FxHashMap<AstPtr<ast::Form>, FormIdx>,
    define_id_map: FxHashMap<DefineId, FormIdx>,
}

impl FormList {
    pub(crate) fn file_form_list_query(db: &dyn MinDefDatabase, file_id: FileId) -> Arc<FormList> {
        let _p = profile::span("file_form_list_query").detail(|| format!("{:?}", file_id));
        let syntax = db.parse(file_id).tree();
        let ctx = lower::Ctx::new(db, &syntax);
        Arc::new(ctx.lower_forms())
    }

    pub fn forms(&self) -> &[FormIdx] {
        &self.forms
    }

    pub(crate) fn data(&self) -> &FormListData {
        &self.data
    }

    pub fn includes(&self) -> impl Iterator<Item = (IncludeAttributeId, &IncludeAttribute)> {
        self.data.includes.iter()
    }

    pub fn functions(&self) -> impl Iterator<Item = (FunctionId, &Function)> {
        self.data.functions.iter()
    }

    pub fn exports(&self) -> impl Iterator<Item = (ExportId, &Export)> {
        self.data.exports.iter()
    }

    pub fn specs(&self) -> impl Iterator<Item = (SpecId, &Spec)> {
        self.data.specs.iter()
    }

    pub fn attributes(&self) -> impl Iterator<Item = (AttributeId, &Attribute)> {
        self.data.attributes.iter()
    }

    pub fn pp_stack(&self) -> &Arena<PPDirective> {
        &self.data.pp_directives
    }

    /// Returns the first -module attribute in the file
    pub fn module_attribute(&self) -> Option<&ModuleAttribute> {
        self.data
            .module_attribute
            .iter()
            .next()
            .map(|(_idx, attr)| attr)
    }

    /// Returns the -define attributes in the file
    pub fn define_attributes(&self) -> impl Iterator<Item = (DefineId, &Define)> {
        self.data.defines.iter()
    }

    /// Returns the -behaviour attributes in the file
    pub fn behaviour_attributes(&self) -> impl Iterator<Item = (BehaviourId, &Behaviour)> {
        self.data.behaviours.iter()
    }

    /// Returns the -callback attribute in the file
    pub fn callback_attributes(&self) -> impl Iterator<Item = (CallbackId, &Callback)> {
        self.data.callbacks.iter()
    }

    /// Returns an iterator over the -compile attributes in the file
    pub fn compile_attributes(&self) -> impl Iterator<Item = (CompileOptionId, &CompileOption)> {
        self.data.compile_options.iter()
    }

    pub fn find_form(&self, form: &ast::Form) -> Option<FormIdx> {
        self.map_back.get(&AstPtr::new(form)).copied()
    }

    pub fn find_define_form(&self, define: &DefineId) -> Option<FormIdx> {
        self.define_id_map.get(define).copied()
    }

    pub fn pretty_print(&self) -> String {
        pretty::print(self)
    }
}

#[derive(Debug, Default, Eq, PartialEq)]
pub(crate) struct FormListData {
    // Even though only one is allowed, in the syntax we might
    // have many due to errors or conditional compilation
    module_attribute: Arena<ModuleAttribute>,
    includes: Arena<IncludeAttribute>,
    functions: Arena<Function>,
    pub defines: Arena<Define>,
    pub pp_directives: Arena<PPDirective>,
    pp_conditions: Arena<PPCondition>,
    exports: Arena<Export>,
    imports: Arena<Import>,
    type_exports: Arena<TypeExport>,
    behaviours: Arena<Behaviour>,
    type_aliases: Arena<TypeAlias>,
    specs: Arena<Spec>,
    callbacks: Arena<Callback>,
    optional_callbacks: Arena<OptionalCallbacks>,
    records: Arena<Record>,
    attributes: Arena<Attribute>,
    compile_options: Arena<CompileOption>,
    record_fields: Arena<RecordField>,
    fa_entries: Arena<FaEntry>,
    deprecates: Arena<DeprecatedAttribute>,
}

impl FormListData {
    fn shrink_to_fit(&mut self) {
        // Exhaustive match to require handling new fields.
        let FormListData {
            module_attribute,
            includes,
            functions,
            defines,
            pp_directives,
            pp_conditions,
            exports,
            imports,
            type_exports,
            behaviours,
            type_aliases,
            specs,
            callbacks,
            optional_callbacks,
            records,
            attributes,
            compile_options,
            record_fields,
            fa_entries,
            deprecates,
        } = self;
        module_attribute.shrink_to_fit();
        includes.shrink_to_fit();
        functions.shrink_to_fit();
        defines.shrink_to_fit();
        pp_directives.shrink_to_fit();
        pp_conditions.shrink_to_fit();
        exports.shrink_to_fit();
        imports.shrink_to_fit();
        type_exports.shrink_to_fit();
        type_aliases.shrink_to_fit();
        behaviours.shrink_to_fit();
        specs.shrink_to_fit();
        callbacks.shrink_to_fit();
        optional_callbacks.shrink_to_fit();
        records.shrink_to_fit();
        compile_options.shrink_to_fit();
        attributes.shrink_to_fit();
        record_fields.shrink_to_fit();
        fa_entries.shrink_to_fit();
        deprecates.shrink_to_fit();
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FormIdx {
    ModuleAttribute(ModuleAttributeId),
    Function(FunctionId),
    PPDirective(PPDirectiveId),
    PPCondition(PPConditionId),
    Export(ExportId),
    Import(ImportId),
    TypeExport(TypeExportId),
    Behaviour(BehaviourId),
    TypeAlias(TypeAliasId),
    Spec(SpecId),
    Callback(CallbackId),
    OptionalCallbacks(OptionalCallbacksId),
    Record(RecordId),
    Attribute(AttributeId),
    CompileOption(CompileOptionId),
    DeprecatedAttribute(DeprecatedAttributeId),
}

pub type ModuleAttributeId = Idx<ModuleAttribute>;
pub type IncludeAttributeId = Idx<IncludeAttribute>;
pub type FunctionId = Idx<Function>;
pub type DefineId = Idx<Define>;
pub type PPDirectiveId = Idx<PPDirective>;
pub type PPConditionId = Idx<PPCondition>;
pub type ExportId = Idx<Export>;
pub type ImportId = Idx<Import>;
pub type TypeExportId = Idx<TypeExport>;
pub type BehaviourId = Idx<Behaviour>;
pub type TypeAliasId = Idx<TypeAlias>;
pub type SpecId = Idx<Spec>;
pub type CallbackId = Idx<Callback>;
pub type OptionalCallbacksId = Idx<OptionalCallbacks>;
pub type RecordId = Idx<Record>;
pub type AttributeId = Idx<Attribute>;
pub type CompileOptionId = Idx<CompileOption>;
pub type RecordFieldId = Idx<RecordField>;
pub type FaEntryId = Idx<FaEntry>;
pub type DeprecatedAttributeId = Idx<DeprecatedAttribute>;

impl Index<ModuleAttributeId> for FormList {
    type Output = ModuleAttribute;

    fn index(&self, index: ModuleAttributeId) -> &Self::Output {
        &self.data.module_attribute[index]
    }
}

impl Index<IncludeAttributeId> for FormList {
    type Output = IncludeAttribute;

    fn index(&self, index: IncludeAttributeId) -> &Self::Output {
        &self.data.includes[index]
    }
}

impl Index<FunctionId> for FormList {
    type Output = Function;

    fn index(&self, index: FunctionId) -> &Self::Output {
        &self.data.functions[index]
    }
}

impl Index<DefineId> for FormList {
    type Output = Define;

    fn index(&self, index: DefineId) -> &Self::Output {
        &self.data.defines[index]
    }
}

impl Index<PPDirectiveId> for FormList {
    type Output = PPDirective;

    fn index(&self, index: PPDirectiveId) -> &Self::Output {
        &self.data.pp_directives[index]
    }
}

impl Index<PPConditionId> for FormList {
    type Output = PPCondition;

    fn index(&self, index: PPConditionId) -> &Self::Output {
        &self.data.pp_conditions[index]
    }
}

impl Index<ExportId> for FormList {
    type Output = Export;

    fn index(&self, index: ExportId) -> &Self::Output {
        &self.data.exports[index]
    }
}

impl Index<ImportId> for FormList {
    type Output = Import;

    fn index(&self, index: ImportId) -> &Self::Output {
        &self.data.imports[index]
    }
}

impl Index<TypeExportId> for FormList {
    type Output = TypeExport;

    fn index(&self, index: TypeExportId) -> &Self::Output {
        &self.data.type_exports[index]
    }
}

impl Index<BehaviourId> for FormList {
    type Output = Behaviour;

    fn index(&self, index: BehaviourId) -> &Self::Output {
        &self.data.behaviours[index]
    }
}

impl Index<TypeAliasId> for FormList {
    type Output = TypeAlias;

    fn index(&self, index: TypeAliasId) -> &Self::Output {
        &self.data.type_aliases[index]
    }
}

impl Index<SpecId> for FormList {
    type Output = Spec;

    fn index(&self, index: SpecId) -> &Self::Output {
        &self.data.specs[index]
    }
}

impl Index<CallbackId> for FormList {
    type Output = Callback;

    fn index(&self, index: CallbackId) -> &Self::Output {
        &self.data.callbacks[index]
    }
}

impl Index<OptionalCallbacksId> for FormList {
    type Output = OptionalCallbacks;

    fn index(&self, index: OptionalCallbacksId) -> &Self::Output {
        &self.data.optional_callbacks[index]
    }
}

impl Index<RecordId> for FormList {
    type Output = Record;

    fn index(&self, index: RecordId) -> &Self::Output {
        &self.data.records[index]
    }
}

impl Index<AttributeId> for FormList {
    type Output = Attribute;

    fn index(&self, index: AttributeId) -> &Self::Output {
        &self.data.attributes[index]
    }
}

impl Index<CompileOptionId> for FormList {
    type Output = CompileOption;

    fn index(&self, index: CompileOptionId) -> &Self::Output {
        &self.data.compile_options[index]
    }
}

impl Index<RecordFieldId> for FormList {
    type Output = RecordField;

    fn index(&self, index: RecordFieldId) -> &Self::Output {
        &self.data.record_fields[index]
    }
}

impl Index<FaEntryId> for FormList {
    type Output = FaEntry;

    fn index(&self, index: FaEntryId) -> &Self::Output {
        &self.data.fa_entries[index]
    }
}

impl Index<DeprecatedAttributeId> for FormList {
    type Output = DeprecatedAttribute;

    fn index(&self, index: DeprecatedAttributeId) -> &Self::Output {
        &self.data.deprecates[index]
    }
}

/// -module
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ModuleAttribute {
    pub name: Name,
    pub cond: Option<PPConditionId>,
    pub form_id: FormId<ast::ModuleAttribute>,
}

/// -include and -include_lib
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum IncludeAttribute {
    Include {
        path: SmolStr,
        cond: Option<PPConditionId>,
        form_id: FormId<ast::PpInclude>,
    },
    IncludeLib {
        path: SmolStr,
        cond: Option<PPConditionId>,
        form_id: FormId<ast::PpIncludeLib>,
    },
}

impl IncludeAttribute {
    pub fn form_id(&self) -> FormId<ast::Form> {
        match self {
            IncludeAttribute::Include { form_id, .. } => form_id.upcast(),
            IncludeAttribute::IncludeLib { form_id, .. } => form_id.upcast(),
        }
    }
}

/// -deprecated
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DeprecatedAttribute {
    Module {
        cond: Option<PPConditionId>,
        form_id: FormId<ast::DeprecatedAttribute>,
    },
    Fa {
        fa: DeprecatedFa,
        cond: Option<PPConditionId>,
        form_id: FormId<ast::DeprecatedAttribute>,
    },
    Fas {
        fas: Vec<DeprecatedFa>,
        cond: Option<PPConditionId>,
        form_id: FormId<ast::DeprecatedAttribute>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DeprecatedFa {
    pub name: Name,
    pub arity: Option<u32>,
    pub desc: Option<DeprecatedDesc>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum DeprecatedDesc {
    Str(SmolStr),
    Atom(SmolStr),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParamName {
    Name(Name),
    Default(Name),
}

impl fmt::Display for ParamName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParamName::Name(name) => fmt::Display::fmt(name, f),
            ParamName::Default(name) => fmt::Display::fmt(name, f),
        }
    }
}

impl Deref for ParamName {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            ParamName::Name(name) => name,
            ParamName::Default(name) => name,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
    pub name: NameArity,
    pub param_names: Vec<ParamName>,
    pub cond: Option<PPConditionId>,
    pub form_id: FormId<ast::FunDecl>,
}

/// -define, -undef, -include, and -include_lib
///
/// These form a stack we can use for macro resolution
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PPDirective {
    Define(DefineId),
    Undef {
        name: Name,
        cond: Option<PPConditionId>,
        form_id: FormId<ast::PpUndef>,
    },
    Include(IncludeAttributeId),
}

impl PPDirective {
    pub fn as_include(&self) -> Option<IncludeAttributeId> {
        match self {
            PPDirective::Define(_) => None,
            PPDirective::Undef { .. } => None,
            PPDirective::Include(idx) => Some(*idx),
        }
    }

    pub fn as_define(&self) -> Option<DefineId> {
        match self {
            PPDirective::Define(idx) => Some(*idx),
            PPDirective::Undef { .. } => None,
            PPDirective::Include(_) => None,
        }
    }

    pub fn form_id(&self, form_list: &FormList) -> FormId<ast::Form> {
        match self {
            PPDirective::Define(define) => form_list[*define].form_id.upcast(),
            PPDirective::Undef { form_id, .. } => form_id.upcast(),
            PPDirective::Include(include) => form_list[*include].form_id(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Define {
    pub name: MacroName,
    pub cond: Option<PPConditionId>,
    pub form_id: FormId<ast::PpDefine>,
}

/// -ifdef, -ifndef, -elsif, -end, and -endif
///
/// Every form has an index into a pre-processor condition
/// it was defined in. This can be evaluated to understand if
/// the form is "active" or not.
/// -elif, -else, and -endif conditions refer to the previous
/// -elif, -if, -ifdef, or -ifndef. Evaluating the entire preceding chain
/// can answer, if this condition is "active" or not.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PPCondition {
    Ifdef {
        cond: Option<PPConditionId>,
        name: Name,
        form_id: FormId<ast::PpIfdef>,
    },
    Ifndef {
        cond: Option<PPConditionId>,
        name: Name,
        form_id: FormId<ast::PpIfndef>,
    },
    If {
        cond: Option<PPConditionId>,
        form_id: FormId<ast::PpIf>,
    },
    Else {
        prev: PPConditionId,
        form_id: FormId<ast::PpElse>,
    },
    Elif {
        prev: PPConditionId,
        form_id: FormId<ast::PpElif>,
    },
    Endif {
        prev: PPConditionId,
        form_id: FormId<ast::PpEndif>,
    },
}

impl PPCondition {
    pub fn form_id(&self) -> FormId<ast::Form> {
        match self {
            PPCondition::Ifdef { form_id, .. } => form_id.upcast(),
            PPCondition::Ifndef { form_id, .. } => form_id.upcast(),
            PPCondition::If { form_id, .. } => form_id.upcast(),
            PPCondition::Else { form_id, .. } => form_id.upcast(),
            PPCondition::Elif { form_id, .. } => form_id.upcast(),
            PPCondition::Endif { form_id, .. } => form_id.upcast(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Export {
    pub entries: IdxRange<FaEntry>,
    pub cond: Option<PPConditionId>,
    pub form_id: FormId<ast::ExportAttribute>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Import {
    pub from: Name,
    pub entries: IdxRange<FaEntry>,
    pub cond: Option<PPConditionId>,
    pub form_id: FormId<ast::ImportAttribute>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeExport {
    pub entries: IdxRange<FaEntry>,
    pub cond: Option<PPConditionId>,
    pub form_id: FormId<ast::ExportTypeAttribute>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Behaviour {
    pub name: Name,
    pub cond: Option<PPConditionId>,
    pub form_id: FormId<ast::BehaviourAttribute>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeAlias {
    Regular {
        name: NameArity,
        cond: Option<PPConditionId>,
        form_id: FormId<ast::TypeAlias>,
    },
    Opaque {
        name: NameArity,
        cond: Option<PPConditionId>,
        form_id: FormId<ast::Opaque>,
    },
}

impl TypeAlias {
    pub fn form_id(&self) -> FormId<ast::Form> {
        match self {
            TypeAlias::Regular { form_id, .. } => form_id.upcast(),
            TypeAlias::Opaque { form_id, .. } => form_id.upcast(),
        }
    }

    pub fn name(&self) -> &NameArity {
        match self {
            TypeAlias::Regular { name, .. } => name,
            TypeAlias::Opaque { name, .. } => name,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Spec {
    pub name: NameArity,
    pub cond: Option<PPConditionId>,
    pub form_id: FormId<ast::Spec>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Callback {
    pub name: NameArity,
    pub cond: Option<PPConditionId>,
    pub form_id: FormId<ast::Callback>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OptionalCallbacks {
    pub entries: IdxRange<FaEntry>,
    pub cond: Option<PPConditionId>,
    pub form_id: FormId<ast::OptionalCallbacksAttribute>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Record {
    pub name: Name,
    pub fields: IdxRange<RecordField>,
    pub cond: Option<PPConditionId>,
    pub form_id: FormId<ast::RecordDecl>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CompileOption {
    pub cond: Option<PPConditionId>,
    pub form_id: FormId<ast::CompileOptionsAttribute>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Attribute {
    pub name: Name,
    pub cond: Option<PPConditionId>,
    pub form_id: FormId<ast::WildAttribute>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RecordField {
    pub name: Name,
    pub idx: u32,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FaEntry {
    pub name: NameArity,
    pub idx: u32,
}

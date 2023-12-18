/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_base_db::FileId;
use elp_base_db::SourceDatabase;
use elp_syntax::ast;

mod body;
pub mod db;
mod def_map;
mod diagnostics;
pub mod edoc;
mod expr;
pub mod fold;
mod form_list;
mod include;
mod intern;
mod macro_exp;
mod module_data;
mod name;
pub mod resolver;
pub mod sema;
mod test_db;

pub use body::AnyAttribute;
pub use body::AttributeBody;
pub use body::Body;
pub use body::BodySourceMap;
pub use body::DefineBody;
pub use body::ExprSource;
pub use body::FunctionBody;
pub use body::FunctionClauseBody;
pub use body::InFileAstPtr;
pub use body::MacroSource;
pub use body::RecordBody;
pub use body::SpecBody;
pub use body::SpecOrCallback;
pub use body::TypeBody;
pub use def_map::DefMap;
pub use def_map::FunctionDefId;
pub use diagnostics::Diagnostic;
pub use diagnostics::DiagnosticMessage;
pub use expr::AnyExpr;
pub use expr::AnyExprId;
pub use expr::AnyExprRef;
pub use expr::BinarySeg;
pub use expr::CRClause;
pub use expr::CallTarget;
pub use expr::CatchClause;
pub use expr::Clause;
pub use expr::ClauseId;
pub use expr::ComprehensionBuilder;
pub use expr::ComprehensionExpr;
pub use expr::Expr;
pub use expr::ExprId;
pub use expr::FunType;
pub use expr::IfClause;
pub use expr::ListType;
pub use expr::Literal;
pub use expr::MapOp;
pub use expr::Pat;
pub use expr::PatId;
pub use expr::ReceiveAfter;
pub use expr::RecordFieldBody;
pub use expr::SpecSig;
pub use expr::Term;
pub use expr::TermId;
pub use expr::TypeExpr;
pub use expr::TypeExprId;
pub use fold::FoldCtx;
pub use fold::On;
pub use fold::Strategy;
pub use form_list::Attribute;
pub use form_list::AttributeId;
pub use form_list::Behaviour;
pub use form_list::BehaviourId;
pub use form_list::Callback;
pub use form_list::CallbackId;
pub use form_list::CompileOption;
pub use form_list::CompileOptionId;
pub use form_list::Define;
pub use form_list::DefineId;
pub use form_list::Export;
pub use form_list::ExportId;
pub use form_list::FaEntry;
pub use form_list::FaEntryId;
pub use form_list::FormId;
pub use form_list::FormIdx;
pub use form_list::FormList;
pub use form_list::FunctionClause;
pub use form_list::FunctionClauseId;
pub use form_list::Import;
pub use form_list::ImportId;
pub use form_list::IncludeAttribute;
pub use form_list::IncludeAttributeId;
pub use form_list::ModuleAttribute;
pub use form_list::ModuleAttributeId;
pub use form_list::OptionalCallbacks;
pub use form_list::OptionalCallbacksId;
pub use form_list::PPCondition;
pub use form_list::PPConditionId;
pub use form_list::PPDirective;
pub use form_list::PPDirectiveId;
pub use form_list::ParamName;
pub use form_list::Record;
pub use form_list::RecordField;
pub use form_list::RecordFieldId;
pub use form_list::RecordId;
pub use form_list::Spec;
pub use form_list::SpecId;
pub use form_list::TypeAlias;
pub use form_list::TypeAliasId;
pub use form_list::TypeExport;
pub use form_list::TypeExportId;
pub use intern::Atom;
pub use intern::Var;
pub use macro_exp::ResolvedMacro;
pub use module_data::CallbackDef;
pub use module_data::DefineDef;
pub use module_data::File;
pub use module_data::FunctionClauseDef;
pub use module_data::FunctionDef;
pub use module_data::Module;
pub use module_data::RecordDef;
pub use module_data::RecordFieldDef;
pub use module_data::SpecDef;
pub use module_data::SpecdFunctionDef;
pub use module_data::TypeAliasDef;
pub use module_data::TypeAliasSource;
pub use module_data::VarDef;
pub use name::known;
// @fb-only: pub use name::meta_only;
pub use name::MacroName;
pub use name::Name;
pub use name::NameArity;
pub use sema::CallDef;
pub use sema::DefinitionOrReference;
pub use sema::FaDef;
pub use sema::InFunctionBody;
pub use sema::InFunctionClauseBody;
pub use sema::MacroCallDef;
pub use sema::ScopeAnalysis;
pub use sema::Semantic;

/// `InFile<T>` stores a value of `T` inside a particular file.
///
/// Typical usages are:
///
/// * `InFile<SyntaxNode>` -- syntax node in a file
/// * `InFile<ast::FnDef>` -- ast node in a file
/// * `InFile<TextSize>` -- offset in a file
/// * `InFile<IncludeAttributeId>` -- `-include` in a file
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct InFile<T> {
    pub file_id: FileId,
    pub value: T,
}

impl<T> InFile<T> {
    pub fn new(file_id: FileId, value: T) -> InFile<T> {
        InFile { file_id, value }
    }

    pub fn with_value<U>(&self, value: U) -> InFile<U> {
        InFile::new(self.file_id, value)
    }

    pub fn map<F: FnOnce(T) -> U, U>(self, f: F) -> InFile<U> {
        InFile::new(self.file_id, f(self.value))
    }

    pub fn as_ref(&self) -> InFile<&T> {
        self.with_value(&self.value)
    }

    pub fn file_syntax(&self, db: &dyn SourceDatabase) -> ast::SourceFile {
        db.parse(self.file_id).tree()
    }
}

impl<T: Clone> InFile<&T> {
    pub fn cloned(&self) -> InFile<T> {
        self.with_value(self.value.clone())
    }
}

impl<T> InFile<Option<T>> {
    pub fn transpose(self) -> Option<InFile<T>> {
        self.value.map(|value| InFile::new(self.file_id, value))
    }
}

// ---------------------------------------------------------------------

/// HIR index.  Uniquely identifies any specific HIR item in a file
/// file. Use globally as `InFile<HirIdx>`.
#[derive(Debug, Clone, Copy)]
pub struct HirIdx {
    pub form_id: FormIdx,
    pub idx: AnyExprId,
}

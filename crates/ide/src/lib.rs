/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::error::Error;
use std::sync::Arc;

use anyhow::Result;
use call_hierarchy::CallItem;
use diagnostics::Diagnostic;
use diagnostics::DiagnosticsConfig;
use diagnostics::LabeledDiagnostics;
use diagnostics_collection::DiagnosticCollection;
use elp_ide_assists::Assist;
use elp_ide_assists::AssistConfig;
use elp_ide_assists::AssistId;
use elp_ide_assists::AssistKind;
use elp_ide_assists::AssistResolveStrategy;
use elp_ide_completion::Completion;
use elp_ide_db::assists::AssistContextDiagnostic;
use elp_ide_db::assists::AssistUserInput;
use elp_ide_db::common_test::CommonTestInfo;
use elp_ide_db::docs::Doc;
use elp_ide_db::elp_base_db::salsa;
use elp_ide_db::elp_base_db::salsa::ParallelDatabase;
use elp_ide_db::elp_base_db::Change;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::elp_base_db::ModuleIndex;
use elp_ide_db::elp_base_db::ModuleName;
use elp_ide_db::elp_base_db::ProjectData;
use elp_ide_db::elp_base_db::ProjectId;
use elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide_db::elp_base_db::SourceDatabaseExt;
use elp_ide_db::eqwalizer::type_references;
use elp_ide_db::erlang_service::ParseResult;
use elp_ide_db::rename::RenameError;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::Eqwalizer;
use elp_ide_db::EqwalizerDatabase;
use elp_ide_db::EqwalizerDiagnostics;
use elp_ide_db::ErlAstDatabase;
use elp_ide_db::Includes;
use elp_ide_db::LineIndex;
use elp_ide_db::LineIndexDatabase;
use elp_ide_db::RootDatabase;
use elp_project_model::AppName;
use elp_project_model::AppType;
use elp_syntax::algo::ancestors_at_offset;
use elp_syntax::ast;
use elp_syntax::label::Label;
use elp_syntax::AstNode;
use elp_syntax::SmolStr;
use elp_types_db::eqwalizer;
use erlang_service::CompileOption;
use expand_macro::ExpandedMacro;
use handlers::get_docs;
use handlers::goto_definition;
use handlers::references;
use hir::db::DefDatabase;
use hir::DefMap;
use hir::File;
use hir::FormList;
use hir::Module;
use hir::Semantic;
use navigation_target::ToNav;

mod annotations;
mod call_hierarchy;
mod codemod_helpers;
mod common_test;
mod doc_links;
mod document_symbols;
mod expand_macro;
mod extend_selection;
mod folding_ranges;
mod handlers;
mod hover;
mod inlay_hints;
mod navigation_target;
mod rename;
mod runnables;
mod signature_help;
mod syntax_highlighting;

#[cfg(test)]
mod fixture;
#[cfg(test)]
mod tests;

pub mod diagnostics;
pub mod diagnostics_collection;
pub mod diff;
mod highlight_related;
// @fb-only: mod meta_only;

pub use annotations::Annotation;
pub use annotations::AnnotationKind;
pub use codemod_helpers::FunctionMatch;
pub use codemod_helpers::MFA;
pub use common_test::GroupName;
pub use doc_links::DocLink;
pub use document_symbols::DocumentSymbol;
pub use elp_ide_assists;
pub use elp_ide_completion;
pub use elp_ide_db;
pub use elp_ide_db::erlang_service;
pub use elp_syntax::TextRange;
pub use elp_syntax::TextSize;
pub use folding_ranges::Fold;
pub use folding_ranges::FoldKind;
pub use handlers::references::ReferenceSearchResult;
pub use highlight_related::HighlightedRange;
pub use hover::HoverAction;
pub use hover::HoverActionsConfig;
pub use inlay_hints::InlayHint;
pub use inlay_hints::InlayHintLabel;
pub use inlay_hints::InlayHintLabelPart;
pub use inlay_hints::InlayHintsConfig;
pub use inlay_hints::InlayKind;
pub use inlay_hints::InlayTooltip;
pub use navigation_target::NavigationTarget;
pub use runnables::Runnable;
pub use runnables::RunnableKind;
pub use signature_help::SignatureHelp;
pub use syntax_highlighting::tags::Highlight;
pub use syntax_highlighting::tags::HlMod;
pub use syntax_highlighting::tags::HlMods;
pub use syntax_highlighting::tags::HlTag;
pub use syntax_highlighting::HighlightConfig;
pub use syntax_highlighting::HlRange;

pub type Cancellable<T> = Result<T, salsa::Cancelled>;

/// Info associated with a text range.
#[derive(Debug)]
pub struct RangeInfo<T> {
    pub range: TextRange,
    pub info: T,
}

impl<T> RangeInfo<T> {
    pub fn new(range: TextRange, info: T) -> RangeInfo<T> {
        RangeInfo { range, info }
    }
}

/// `AnalysisHost` stores the current state of the world.
#[derive(Debug, Default)]
pub struct AnalysisHost {
    db: RootDatabase,
}

impl AnalysisHost {
    /// Returns a snapshot of the current state, which you can query for
    /// semantic information.
    pub fn analysis(&self) -> Analysis {
        Analysis {
            db: self.db.snapshot(),
        }
    }

    /// Trigger cancellations on all Analysis forked from the current database
    pub fn request_cancellation(&mut self) {
        self.db.request_cancellation();
    }

    pub fn raw_database(&self) -> &RootDatabase {
        &self.db
    }
    pub fn raw_database_mut(&mut self) -> &mut RootDatabase {
        &mut self.db
    }

    /// Applies changes to the current state of the world. If there are
    /// outstanding snapshots, they will be canceled.
    pub fn apply_change(&mut self, change: Change) {
        self.db.apply_change(change)
    }
}

/// Analysis is a snapshot of a world state at a moment in time. It is the main
/// entry point for asking semantic information about the world. When the world
/// state is advanced using `AnalysisHost::apply_change` method, all existing
/// `Analysis` are canceled (most method return `Err(Canceled)`).
#[derive(Debug)]
pub struct Analysis {
    db: salsa::Snapshot<RootDatabase>,
}

// As a general design guideline, `Analysis` API are intended to be independent
// from the language server protocol. That is, when exposing some functionality
// we should think in terms of "what API makes most sense" and not in terms of
// "what types LSP uses". We have at least 2 consumers of the API - LSP and CLI
impl Analysis {
    /// Gets the file's `LineIndex`: data structure to convert between absolute
    /// offsets and line/column representation.
    pub fn line_index(&self, file_id: FileId) -> Cancellable<Arc<LineIndex>> {
        self.with_db(|db| db.file_line_index(file_id))
    }

    /// Computes the set of ELP-native diagnostics for the given file.
    pub fn native_diagnostics(
        &self,
        config: &DiagnosticsConfig,
        file_id: FileId,
    ) -> Cancellable<LabeledDiagnostics> {
        self.with_db(|db| diagnostics::native_diagnostics(db, config, file_id))
    }

    /// Computes the set of eqwalizer diagnostics for the given files,
    /// including checking for disabled. Returns standard diagnostics.
    pub fn eqwalizer_diagnostics_for_file(
        &self,
        file_id: FileId,
        include_generated: bool,
    ) -> Cancellable<Option<Vec<Diagnostic>>> {
        self.with_db(|db| diagnostics::eqwalizer_diagnostics(db, file_id, include_generated))
    }

    /// Computes the set of eqwalizer diagnostics for the given files,
    /// without checking if disabled. Returns EqwalizerDiagnostics
    pub fn eqwalizer_diagnostics(
        &self,
        project_id: ProjectId,
        file_ids: Vec<FileId>,
    ) -> Cancellable<Arc<EqwalizerDiagnostics>> {
        self.with_db(|db| {
            elp_ide_db::eqwalizer::eqwalizer_diagnostics_by_project(db, project_id, file_ids)
        })
    }

    pub fn eqwalizer_stats(
        &self,
        project_id: ProjectId,
        file_id: FileId,
    ) -> Cancellable<Option<Vec<Diagnostic>>> {
        self.with_db(|db| diagnostics::eqwalizer_stats(db, project_id, file_id))
    }

    pub fn type_at_position(
        &self,
        project_id: ProjectId,
        range: FileRange,
    ) -> Cancellable<Option<Arc<(eqwalizer::types::Type, FileRange)>>> {
        self.with_db(|db| db.type_at_position(project_id, range))
    }

    pub fn type_references(
        &self,
        project_id: ProjectId,
        ty: &eqwalizer::types::Type,
    ) -> Cancellable<Vec<(SmolStr, FileRange)>> {
        self.with_db(|db| type_references(db, project_id, ty))
    }

    /// Computes the set of EDoc diagnostics for the given file.
    pub fn edoc_diagnostics(&self, file_id: FileId) -> Cancellable<Vec<(FileId, Vec<Diagnostic>)>> {
        self.with_db(|db| diagnostics::edoc_diagnostics(db, file_id))
    }

    /// Computes Common Test info for the given file.
    pub fn ct_info(&self, file_id: FileId) -> Cancellable<Arc<CommonTestInfo>> {
        self.with_db(|db| diagnostics::ct_info(db, file_id))
    }

    /// Computes Common Test diagnostics for the given file.
    pub fn ct_diagnostics(&self, file_id: FileId) -> Cancellable<Vec<Diagnostic>> {
        self.with_db(|db| diagnostics::ct_diagnostics(db, file_id))
    }

    /// Computes the set of parse server diagnostics for the given file.
    pub fn erlang_service_diagnostics(
        &self,
        file_id: FileId,
        config: &DiagnosticsConfig,
    ) -> Cancellable<Vec<(FileId, LabeledDiagnostics)>> {
        self.with_db(|db| diagnostics::erlang_service_diagnostics(db, file_id, config))
    }

    /// Low-level access to eqwalizer
    pub fn eqwalizer(&self) -> &Eqwalizer {
        self.db.eqwalizer()
    }

    /// eqwalizer is enabled if:
    /// - the app (the module belongs to) has `.eqwalizer` marker in the roof
    /// - or the module has `-typing([eqwalizer]).` pragma
    /// - or the whole project has `enable_all=true` in its `.elp.toml` file
    pub fn is_eqwalizer_enabled(
        &self,
        file_id: FileId,
        include_generated: bool,
    ) -> Cancellable<bool> {
        self.with_db(|db| db.is_eqwalizer_enabled(file_id, include_generated))
    }

    /// ETF for the module's abstract forms
    pub fn module_ast(
        &self,
        file_id: FileId,
        format: erlang_service::Format,
        compile_options: Vec<CompileOption>,
    ) -> Cancellable<Arc<ParseResult>> {
        self.with_db(|db| db.module_ast(file_id, format, compile_options))
    }

    pub fn project_id(&self, file_id: FileId) -> Cancellable<Option<ProjectId>> {
        // Context for T171541590
        let _ = stdx::panic_context::enter(format!("\nproject_id: {:?}", file_id));
        self.with_db(|db| Some(db.app_data(db.file_source_root(file_id))?.project_id))
    }

    pub fn project_data(&self, file_id: FileId) -> Cancellable<Option<Arc<ProjectData>>> {
        self.with_db(|db| {
            // Context for T171541590
            let _ = stdx::panic_context::enter(format!("\nproject_data: {:?}", file_id));
            Some(db.project_data(db.app_data(db.file_source_root(file_id))?.project_id))
        })
    }

    /// Returns module name
    pub fn module_name(&self, file_id: FileId) -> Cancellable<Option<ModuleName>> {
        self.with_db(|db| {
            // Context for T171541590
            let _ = stdx::panic_context::enter(format!("\nmodule_name: {:?}", file_id));
            let app_data = db.app_data(db.file_source_root(file_id))?;
            db.module_index(app_data.project_id)
                .module_for_file(file_id)
                .cloned()
        })
    }

    pub fn module_index(&self, project_id: ProjectId) -> Cancellable<Arc<ModuleIndex>> {
        self.with_db(|db| db.module_index(project_id))
    }

    pub fn module_file_id(
        &self,
        project_id: ProjectId,
        module: &str,
    ) -> Cancellable<Option<FileId>> {
        self.with_db(|db| db.module_index(project_id).file_for_module(module))
    }

    pub fn expand_macro(&self, position: FilePosition) -> Cancellable<Option<ExpandedMacro>> {
        self.with_db(|db| expand_macro::expand_macro(db, position))
    }

    /// Selects the next syntactic nodes encompassing the range.
    pub fn extend_selection(&self, frange: FileRange) -> Cancellable<TextRange> {
        self.with_db(|db| extend_selection::extend_selection(db, frange))
    }

    /// Returns a list of symbols in the file. Useful to draw a
    /// file outline.
    pub fn document_symbols(&self, file_id: FileId) -> Cancellable<Vec<DocumentSymbol>> {
        self.with_db(|db| document_symbols::document_symbols(db, file_id))
    }

    /// Returns the contents of a file
    pub fn file_text(&self, file_id: FileId) -> Cancellable<Arc<str>> {
        self.with_db(|db| db.file_text(file_id))
    }

    /// Returns the app_type for a file
    pub fn file_app_name(&self, file_id: FileId) -> Cancellable<Option<AppName>> {
        self.with_db(|db| db.file_app_name(file_id))
    }

    /// Returns the app_type for a file
    pub fn file_app_type(&self, file_id: FileId) -> Cancellable<Option<AppType>> {
        self.with_db(|db| db.file_app_type(file_id))
    }

    /// Returns the FileKind for a file
    pub fn file_kind(&self, file_id: FileId) -> Cancellable<FileKind> {
        self.with_db(|db| db.file_kind(file_id))
    }

    /// When we get a range from the client, limit it to what is in the source file
    pub fn clamp_range(&self, file_id: FileId, range: TextRange) -> Cancellable<TextRange> {
        self.with_db(|db| db.clamp_range(file_id, range))
    }

    /// When we get an offset from the client, limit it to what is in the source file
    pub fn clamp_offset(&self, file_id: FileId, offset: TextSize) -> Cancellable<TextSize> {
        self.with_db(|db| db.clamp_offset(file_id, offset))
    }

    /// Convenience function to return assists + quick fixes for diagnostics
    pub fn assists_with_fixes(
        &self,
        assist_config: &AssistConfig,
        diagnostics_config: &DiagnosticsConfig,
        resolve: AssistResolveStrategy,
        frange: FileRange,
        context_diagnostics: &[AssistContextDiagnostic],
        // Note: These diagnostics are from a prior snapshot.
        // We use them as a way to access the ones updated on save.
        // Eventually we should be able to access them directly
        diagnostics_collection: &DiagnosticCollection,
        user_input: Option<AssistUserInput>,
    ) -> Cancellable<Vec<Assist>> {
        let include_fixes = match &assist_config.allowed {
            Some(it) => it
                .iter()
                .any(|&it| it == AssistKind::None || it == AssistKind::QuickFix),
            None => true,
        };

        self.with_db(|db| {
            let diagnostic_assists = if include_fixes {
                diagnostics::native_diagnostics(db, diagnostics_config, frange.file_id)
                    .iter()
                    .filter_map(|it| it.fixes.clone())
                    .flatten()
                    .filter(|it| it.target.intersect(frange.range).is_some())
                    .collect()
            } else {
                Vec::new()
            };
            let eqwalizer_assists = if include_fixes {
                diagnostics_collection
                    .eqwalizer
                    .get(&frange.file_id)
                    .iter()
                    .map(|x| x.iter().filter_map(|it| it.fixes.clone()).flatten())
                    .flatten()
                    .filter(|it| it.target.intersect(frange.range).is_some())
                    .collect()
            } else {
                Vec::new()
            };
            let assists = elp_ide_assists::assists(
                db,
                assist_config,
                resolve,
                frange,
                context_diagnostics,
                user_input,
            );

            let mut res = diagnostic_assists;
            res.extend(assists);
            res.extend(eqwalizer_assists);

            res
        })
    }

    pub fn is_generated(&self, file_id: FileId) -> Cancellable<bool> {
        self.with_db(|db| db.is_generated(file_id))
    }

    pub fn is_test_suite_or_test_helper(&self, file_id: FileId) -> Cancellable<Option<bool>> {
        self.with_db(|db| db.is_test_suite_or_test_helper(file_id))
    }

    /// Search symbols. Only module names are currently supported.
    pub fn symbol_search(
        &self,
        project_id: ProjectId,
        query: &str,
    ) -> Cancellable<Vec<NavigationTarget>> {
        const LIMIT: i32 = 128;
        self.with_db(|db| {
            let module_index = self.module_index(project_id).unwrap();
            let mut total = 0;
            module_index
                .all_modules()
                .iter()
                .filter_map(|name: &ModuleName| {
                    if total <= LIMIT && name.as_str().contains(query) {
                        let file_id = module_index.file_for_module(name)?;
                        let module = Module {
                            file: File { file_id },
                        };
                        total += 1;
                        Some(module.to_nav(db))
                    } else {
                        None
                    }
                })
                .collect()
        })
    }

    pub fn goto_definition(
        &self,
        position: FilePosition,
    ) -> Cancellable<Option<RangeInfo<Vec<NavigationTarget>>>> {
        self.with_db(|db| goto_definition::goto_definition(db, position))
    }

    /// Returns the docs for the symbol at the given position
    pub fn get_docs_at_position(
        &self,
        position: FilePosition,
    ) -> Cancellable<Option<(Doc, Option<FileRange>)>> {
        self.with_db(|db| get_docs::get_doc_at_position(db, position))
    }

    /// Returns available hover actions (rendered as buttons)
    pub fn hover_actions(
        &self,
        position: FilePosition,
        config: &HoverActionsConfig,
    ) -> Cancellable<Vec<HoverAction>> {
        self.with_db(|db| hover::actions(db, position, config))
    }

    /// Finds all usages of the reference at point.
    pub fn find_all_refs(
        &self,
        position: FilePosition,
    ) -> Cancellable<Option<Vec<ReferenceSearchResult>>> {
        self.with_db(|db| references::find_all_refs(&Semantic::new(db), position))
    }

    pub fn completions(
        &self,
        position: FilePosition,
        trigger_character: Option<char>,
    ) -> Cancellable<Vec<Completion>> {
        self.with_db(|db| elp_ide_completion::completions(db, position, trigger_character))
    }

    pub fn resolved_includes(&self, file_id: FileId) -> Cancellable<Option<Includes>> {
        self.with_db(|db| db.resolved_includes(file_id))
    }

    /// Returns the edit required to rename the thing at the position to the new
    /// name.
    pub fn rename(
        &self,
        position: FilePosition,
        new_name: &str,
    ) -> Cancellable<Result<SourceChange, RenameError>> {
        self.with_db(|db| rename::rename(db, position, new_name))
    }

    /// Returns the set of folding ranges.
    pub fn folding_ranges(&self, file_id: FileId) -> Cancellable<Vec<Fold>> {
        self.with_db(|db| folding_ranges::folding_ranges(db, file_id))
    }

    /// Computes call hierarchy candidates for the given file position.
    pub fn call_hierarchy_prepare(
        &self,
        position: FilePosition,
    ) -> Cancellable<Option<RangeInfo<Vec<NavigationTarget>>>> {
        self.with_db(|db| call_hierarchy::call_hierarchy_prepare(db, position))
    }

    /// Computes incoming calls for the given file position.
    pub fn incoming_calls(&self, position: FilePosition) -> Cancellable<Option<Vec<CallItem>>> {
        self.with_db(|db| call_hierarchy::incoming_calls(db, position))
    }

    /// Computes outgoing calls for the given file position.
    pub fn outgoing_calls(&self, position: FilePosition) -> Cancellable<Option<Vec<CallItem>>> {
        self.with_db(|db| call_hierarchy::outgoing_calls(db, position))
    }

    /// Computes parameter information at the given position.
    pub fn signature_help(
        &self,
        position: FilePosition,
    ) -> Cancellable<Option<(Vec<SignatureHelp>, Option<usize>)>> {
        self.with_db(|db| signature_help::signature_help(db, position))
    }

    /// Returns a list of the places in the file where type hints can be displayed.
    pub fn inlay_hints(
        &self,
        config: &InlayHintsConfig,
        file_id: FileId,
        range: Option<TextRange>,
    ) -> Cancellable<Vec<InlayHint>> {
        self.with_db(|db| inlay_hints::inlay_hints(db, file_id, range, config))
    }

    /// Computes syntax highlighting for the given file
    pub fn highlight(&self, file_id: FileId) -> Cancellable<Vec<HlRange>> {
        self.with_db(|db| syntax_highlighting::highlight(db, file_id, None))
    }

    /// Computes all ranges to highlight for a given item in a file.
    pub fn highlight_related(
        &self,
        position: FilePosition,
    ) -> Cancellable<Option<Vec<HighlightedRange>>> {
        self.with_db(|db| highlight_related::highlight_related(&Semantic::new(db), position))
    }

    /// Computes syntax highlighting for the given file range.
    pub fn highlight_range(&self, frange: FileRange) -> Cancellable<Vec<HlRange>> {
        self.with_db(|db| syntax_highlighting::highlight(db, frange.file_id, Some(frange.range)))
    }

    pub fn annotations(&self, file_id: FileId) -> Cancellable<Vec<Annotation>> {
        self.with_db(|db| match &*diagnostics::ct_info(db, file_id) {
            CommonTestInfo::Result { all, groups } => {
                annotations::ct_annotations(db, file_id, all.clone(), groups.clone())
            }
            _ => annotations::annotations(db, file_id),
        })
    }

    pub fn runnables(&self, file_id: FileId) -> Cancellable<Vec<Runnable>> {
        self.with_db(|db| match &*diagnostics::ct_info(db, file_id) {
            CommonTestInfo::Result { all, groups } => {
                runnables::runnables(db, file_id, all.clone(), groups.clone())
            }
            _ => Vec::new(),
        })
    }

    /// Return URL(s) for the documentation of the symbol under the cursor.
    pub fn external_docs(&self, position: FilePosition) -> Cancellable<Option<Vec<DocLink>>> {
        self.with_db(|db| doc_links::external_docs(db, &position))
    }

    /// Return the form enclosing the given position
    pub fn enclosing_form(&self, position: FilePosition) -> Cancellable<Option<ast::Form>> {
        self.with_db(|db| {
            let sema = Semantic::new(db);
            let source = sema.parse(position.file_id);
            let syntax = source.value.syntax();
            let form = ancestors_at_offset(syntax, position.offset)
                .and_then(|mut ns| ns.find_map(ast::Form::cast))?;
            Some(form)
        })
    }

    pub fn def_map(&self, file_id: FileId) -> Cancellable<Arc<DefMap>> {
        self.with_db(|db| db.def_map(file_id))
    }

    pub fn form_list(&self, file_id: FileId) -> Cancellable<Arc<FormList>> {
        self.with_db(|db| db.file_form_list(file_id))
    }

    /// Performs an operation on the database that may be canceled.
    ///
    /// ELP needs to be able to answer semantic questions about the
    /// code while the code is being modified. A common problem is that a
    /// long-running query is being calculated when a new change arrives.
    ///
    /// We can't just apply the change immediately: this will cause the pending
    /// query to see inconsistent state (it will observe an absence of
    /// repeatable read). So what we do is we **cancel** all pending queries
    /// before applying the change.
    ///
    /// Salsa implements cancellation by unwinding with a special value and
    /// catching it on the API boundary.
    pub fn with_db<F, T>(&self, f: F) -> Cancellable<T>
    where
        F: FnOnce(&RootDatabase) -> T + std::panic::UnwindSafe,
    {
        salsa::Cancelled::catch(|| f(&self.db))
    }
}

impl Clone for Analysis {
    fn clone(&self) -> Self {
        Analysis {
            db: self.db.snapshot(),
        }
    }
}

pub fn is_cancelled(e: &(dyn Error + 'static)) -> bool {
    e.downcast_ref::<salsa::Cancelled>().is_some()
}

// ---------------------------------------------------------------------

fn fix(id: &'static str, label: &str, source_change: SourceChange, target: TextRange) -> Assist {
    let mut res = unresolved_fix(id, label, target);
    res.source_change = Some(source_change);
    res
}

fn unresolved_fix(id: &'static str, label: &str, target: TextRange) -> Assist {
    assert!(!id.contains(' '));
    Assist {
        id: AssistId(id, AssistKind::QuickFix),
        label: Label::new(label),
        group: None,
        target,
        source_change: None,
        user_input: None,
    }
}

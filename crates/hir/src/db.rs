/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use elp_base_db::AppDataId;
use elp_base_db::FileId;
use elp_base_db::InternedFileId;
use elp_base_db::RootQueryDb;
use elp_base_db::Upcast;
use elp_base_db::eetf;
use elp_base_db::salsa;
use elp_syntax::ast;
use elp_types_db::TypedSemantic;
use fxhash::FxHashMap;

use crate::AttributeBody;
use crate::AttributeId;
use crate::BodySourceMap;
use crate::CallbackId;
use crate::CompileOptionId;
use crate::DefMap;
use crate::DefineId;
use crate::FormList;
use crate::FunctionBody;
use crate::FunctionClauseBody;
use crate::FunctionClauseId;
use crate::InFile;
use crate::InFileAstPtr;
use crate::IncludeAttributeId;
use crate::MacroName;
use crate::Name;
use crate::PPConditionId;
use crate::RecordBody;
use crate::RecordId;
use crate::ResolvedMacro;
use crate::SpecBody;
use crate::SpecId;
use crate::SsrBody;
use crate::SsrSource;
use crate::TypeAliasId;
use crate::TypeBody;
use crate::body::ConditionBody;
use crate::body::DefineBody;
use crate::body::scope::ExprScopes;
use crate::body::scope::FunctionScopes;
use crate::def_map::FunctionDefId;
use crate::edoc;
use crate::edoc::EdocHeader;
use crate::include;
pub use crate::intern::InternDatabase;
use crate::macro_exp;
use crate::macro_exp::MacroResolution;
use crate::preprocessor;
use crate::preprocessor::ConditionDiagnosticsMap;
use crate::preprocessor::MacroEnvironment;
use crate::preprocessor::PreprocessorAnalysis;

#[ra_ap_query_group_macro::query_group]
pub trait DefDatabase:
    InternDatabase + Upcast<dyn InternDatabase> + RootQueryDb + Upcast<dyn RootQueryDb> + TypedSemantic
{
    #[salsa::transparent]
    #[salsa::invoke(FormList::file_form_list_dispatch)]
    fn file_form_list(&self, file_id: FileId) -> Arc<FormList>;

    #[salsa::invoke(FormList::file_form_list_inner)]
    fn file_form_list_interned(&self, fid: InternedFileId) -> Arc<FormList>;

    #[salsa::transparent]
    #[salsa::invoke(FunctionBody::function_body_with_source_dispatch)]
    fn function_body_with_source(
        &self,
        function_id: InFile<FunctionDefId>,
    ) -> (Arc<FunctionBody>, Vec<Arc<BodySourceMap>>);

    #[salsa::invoke(FunctionBody::function_body_with_source_inner)]
    fn function_body_with_source_interned(
        &self,
        fid: crate::InternedInFileFunctionDef,
    ) -> (Arc<FunctionBody>, Vec<Arc<BodySourceMap>>);

    #[salsa::transparent]
    #[salsa::invoke(FunctionClauseBody::function_clause_body_with_source_dispatch)]
    fn function_clause_body_with_source(
        &self,
        function_clause_id: InFile<FunctionClauseId>,
    ) -> (Arc<FunctionClauseBody>, Arc<BodySourceMap>);

    #[salsa::invoke(FunctionClauseBody::function_clause_body_with_source_inner)]
    fn function_clause_body_with_source_interned(
        &self,
        fid: crate::InternedInFileFunctionClause,
    ) -> (Arc<FunctionClauseBody>, Arc<BodySourceMap>);

    #[salsa::invoke_interned(RecordBody::record_body_with_source_query)]
    fn record_body_with_source(
        &self,
        record_id: InFile<RecordId>,
    ) -> (Arc<RecordBody>, Arc<BodySourceMap>);

    #[salsa::invoke_interned(SpecBody::spec_body_with_source_query)]
    fn spec_body_with_source(&self, spec_id: InFile<SpecId>)
    -> (Arc<SpecBody>, Arc<BodySourceMap>);

    #[salsa::invoke_interned(SpecBody::callback_body_with_source_query)]
    fn callback_body_with_source(
        &self,
        callback_id: InFile<CallbackId>,
    ) -> (Arc<SpecBody>, Arc<BodySourceMap>);

    #[salsa::invoke_interned(TypeBody::type_body_with_source_query)]
    fn type_body_with_source(
        &self,
        type_alias_id: InFile<TypeAliasId>,
    ) -> (Arc<TypeBody>, Arc<BodySourceMap>);

    #[salsa::invoke_interned(AttributeBody::attribute_body_with_source_query)]
    fn attribute_body_with_source(
        &self,
        attribute_id: InFile<AttributeId>,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>);

    #[salsa::invoke_interned(AttributeBody::compile_body_with_source_query)]
    fn compile_body_with_source(
        &self,
        attribute_id: InFile<CompileOptionId>,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>);

    #[salsa::invoke_interned(DefineBody::define_body_with_source_query)]
    fn define_body_with_source(
        &self,
        define_id: InFile<DefineId>,
    ) -> (Arc<DefineBody>, Arc<BodySourceMap>);

    #[salsa::invoke_interned(ConditionBody::condition_body_with_source_query)]
    fn condition_body_with_source(
        &self,
        cond_id: InFile<PPConditionId>,
    ) -> Option<(Arc<ConditionBody>, Arc<BodySourceMap>)>;

    // SsrSource is already a #[salsa::interned] struct, so use invoke (not invoke_interned)
    #[salsa::invoke(SsrBody::ssr_body_with_source_query)]
    fn ssr_body_with_source(
        &self,
        ssr_source: SsrSource,
    ) -> Option<(Arc<SsrBody>, Arc<BodySourceMap>)>;

    // Projection queries to stop recomputation if structure didn't change, even if positions did
    #[salsa::transparent]
    #[salsa::invoke(function_body_dispatch)]
    fn function_body(&self, function_id: InFile<FunctionDefId>) -> Arc<FunctionBody>;

    #[salsa::invoke(function_body_inner)]
    fn function_body_interned(&self, fid: crate::InternedInFileFunctionDef) -> Arc<FunctionBody>;

    #[salsa::transparent]
    #[salsa::invoke(function_clause_body_dispatch)]
    fn function_clause_body(
        &self,
        function_clause_id: InFile<FunctionClauseId>,
    ) -> Arc<FunctionClauseBody>;

    #[salsa::invoke(function_clause_body_inner)]
    fn function_clause_body_interned(
        &self,
        fid: crate::InternedInFileFunctionClause,
    ) -> Arc<FunctionClauseBody>;
    #[salsa::invoke_interned(type_body)]
    fn type_body(&self, type_alias_id: InFile<TypeAliasId>) -> Arc<TypeBody>;
    #[salsa::invoke_interned(spec_body)]
    fn spec_body(&self, spec_id: InFile<SpecId>) -> Arc<SpecBody>;
    #[salsa::invoke_interned(callback_body)]
    fn callback_body(&self, callback_id: InFile<CallbackId>) -> Arc<SpecBody>;
    #[salsa::invoke_interned(record_body)]
    fn record_body(&self, record_id: InFile<RecordId>) -> Arc<RecordBody>;
    #[salsa::invoke_interned(attribute_body)]
    fn attribute_body(&self, attribute_id: InFile<AttributeId>) -> Arc<AttributeBody>;
    #[salsa::invoke_interned(compile_body)]
    fn compile_body(&self, attribute_id: InFile<CompileOptionId>) -> Arc<AttributeBody>;
    #[salsa::invoke_interned(define_body)]
    fn define_body(&self, define_id: InFile<DefineId>) -> Arc<DefineBody>;
    // SsrSource is already a #[salsa::interned] struct, so use invoke (not invoke_interned)
    #[salsa::invoke(ssr_body)]
    fn ssr_body(&self, ssr_source: SsrSource) -> Option<Arc<SsrBody>>;

    #[salsa::transparent]
    #[salsa::invoke(FunctionScopes::function_scopes_dispatch)]
    fn function_scopes(&self, fun: InFile<FunctionDefId>) -> Arc<FunctionScopes>;

    #[salsa::invoke(FunctionScopes::function_scopes_inner)]
    fn function_scopes_interned(
        &self,
        fid: crate::InternedInFileFunctionDef,
    ) -> Arc<FunctionScopes>;

    #[salsa::transparent]
    #[salsa::invoke(FunctionScopes::function_clause_scopes_dispatch)]
    fn function_clause_scopes(&self, clause: InFile<FunctionClauseId>) -> Arc<ExprScopes>;

    #[salsa::invoke(FunctionScopes::function_clause_scopes_inner)]
    fn function_clause_scopes_interned(
        &self,
        fid: crate::InternedInFileFunctionClause,
    ) -> Arc<ExprScopes>;

    #[salsa::transparent]
    #[salsa::invoke(include::resolve_dispatch)]
    fn resolve_include(
        &self,
        orig_app: Option<AppDataId>,
        include_id: InFile<IncludeAttributeId>,
    ) -> Option<FileId>;

    #[salsa::invoke(include::resolve_inner)]
    fn resolve_include_interned(&self, key: crate::InternedResolveInclude) -> Option<FileId>;

    #[salsa::transparent]
    #[salsa::invoke(macro_exp::resolve_dispatch)]
    fn resolve_macro(&self, file_id: FileId, name: MacroName) -> Option<ResolvedMacro>;

    #[salsa::invoke(macro_exp::resolve_inner)]
    fn resolve_macro_interned(&self, fid: InternedFileId, name: MacroName)
    -> Option<ResolvedMacro>;

    #[salsa::transparent]
    #[salsa::invoke(edoc::file_edoc_comments_dispatch)]
    fn file_edoc_comments(
        &self,
        file_id: FileId,
    ) -> Option<Arc<FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>>>;

    #[salsa::invoke(edoc::file_edoc_comments_inner)]
    fn file_edoc_comments_interned(
        &self,
        fid: InternedFileId,
    ) -> Option<Arc<FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>>>;

    #[salsa::transparent]
    #[salsa::invoke(macro_exp::project_macro_environment_dispatch)]
    fn project_macro_environment(&self, file_id: FileId) -> Arc<MacroEnvironment>;

    #[salsa::invoke(macro_exp::project_macro_environment_inner)]
    fn project_macro_environment_interned(&self, fid: InternedFileId) -> Arc<MacroEnvironment>;

    #[salsa::transparent]
    #[salsa::invoke(preprocessor::file_preprocessor_analysis_with_diagnostics_dispatch)]
    fn file_preprocessor_analysis_with_diagnostics(
        &self,
        file_id: FileId,
        env: Arc<MacroEnvironment>,
    ) -> (Arc<PreprocessorAnalysis>, Arc<ConditionDiagnosticsMap>);

    #[salsa::cycle(cycle_result = preprocessor::recover_cycle_with_diagnostics)]
    #[salsa::invoke(preprocessor::file_preprocessor_analysis_with_diagnostics_inner)]
    fn file_preprocessor_analysis_with_diagnostics_interned(
        &self,
        fid: InternedFileId,
        env: Arc<MacroEnvironment>,
    ) -> (Arc<PreprocessorAnalysis>, Arc<ConditionDiagnosticsMap>);

    // Projection query - just returns analysis without diagnostics
    #[salsa::transparent]
    #[salsa::invoke(preprocessor::file_preprocessor_analysis_dispatch)]
    fn file_preprocessor_analysis(
        &self,
        file_id: FileId,
        env: Arc<MacroEnvironment>,
    ) -> Arc<PreprocessorAnalysis>;

    #[salsa::cycle(cycle_result = preprocessor::recover_cycle)]
    #[salsa::invoke(preprocessor::file_preprocessor_analysis_inner)]
    fn file_preprocessor_analysis_interned(
        &self,
        fid: InternedFileId,
        env: Arc<MacroEnvironment>,
    ) -> Arc<PreprocessorAnalysis>;

    // Helper query to run the recursive resolution algorithm
    #[salsa::transparent]
    #[salsa::invoke(macro_exp::local_resolve_dispatch)]
    fn local_resolve_macro(&self, file_id: FileId, name: MacroName) -> MacroResolution;

    #[salsa::cycle(cycle_result = macro_exp::recover_cycle)]
    #[salsa::invoke(macro_exp::local_resolve_inner)]
    fn local_resolve_macro_interned(&self, fid: InternedFileId, name: MacroName)
    -> MacroResolution;

    #[salsa::transparent]
    #[salsa::invoke(DefMap::def_map_dispatch)]
    fn def_map(&self, file_id: FileId) -> Arc<DefMap>;

    #[salsa::cycle(cycle_result = DefMap::recover_cycle)]
    #[salsa::invoke(DefMap::def_map_inner)]
    fn def_map_interned(&self, fid: InternedFileId) -> Arc<DefMap>;

    #[salsa::transparent]
    #[salsa::invoke(DefMap::def_map_with_env_dispatch)]
    fn def_map_with_env(&self, file_id: FileId, env: Arc<MacroEnvironment>) -> Arc<DefMap>;

    #[salsa::cycle(cycle_result = DefMap::recover_cycle_with_env)]
    #[salsa::invoke(DefMap::def_map_with_env_inner)]
    fn def_map_with_env_interned(
        &self,
        fid: InternedFileId,
        env: Arc<MacroEnvironment>,
    ) -> Arc<DefMap>;

    // Helper query to compute only local data, avoids recomputation of header data,
    // if only local information changed
    #[salsa::transparent]
    #[salsa::invoke(DefMap::def_map_local_dispatch)]
    fn def_map_local(&self, file_id: FileId) -> Arc<DefMap>;

    #[salsa::invoke(DefMap::def_map_local_inner)]
    fn def_map_local_interned(&self, fid: InternedFileId) -> Arc<DefMap>;

    /// Returns the macro names defined externally (e.g., via `-D` command line flags)
    /// for a given file, based on its application's configuration.
    #[salsa::transparent]
    #[salsa::invoke(file_external_defines_dispatch)]
    fn file_external_defines(&self, file_id: FileId) -> Arc<Vec<Name>>;

    #[salsa::invoke(file_external_defines_inner)]
    fn file_external_defines_interned(&self, fid: InternedFileId) -> Arc<Vec<Name>>;
}

fn function_body_dispatch(
    db: &dyn DefDatabase,
    function_id: InFile<FunctionDefId>,
) -> Arc<FunctionBody> {
    db.function_body_interned(crate::InternedInFileFunctionDef::new(db, function_id))
}

fn function_body_inner(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileFunctionDef,
) -> Arc<FunctionBody> {
    let function_id = fid.value(db);
    db.function_body_with_source(function_id).0
}

fn function_clause_body_dispatch(
    db: &dyn DefDatabase,
    function_clause_id: InFile<FunctionClauseId>,
) -> Arc<FunctionClauseBody> {
    db.function_clause_body_interned(crate::InternedInFileFunctionClause::new(
        db,
        function_clause_id,
    ))
}

fn function_clause_body_inner(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileFunctionClause,
) -> Arc<FunctionClauseBody> {
    let function_clause_id = fid.value(db);
    db.function_clause_body_with_source(function_clause_id).0
}

fn type_body(db: &dyn DefDatabase, type_alias_id: InFile<TypeAliasId>) -> Arc<TypeBody> {
    db.type_body_with_source(type_alias_id).0
}

fn spec_body(db: &dyn DefDatabase, spec_id: InFile<SpecId>) -> Arc<SpecBody> {
    db.spec_body_with_source(spec_id).0
}

fn callback_body(db: &dyn DefDatabase, callback_id: InFile<CallbackId>) -> Arc<SpecBody> {
    db.callback_body_with_source(callback_id).0
}

fn record_body(db: &dyn DefDatabase, record_id: InFile<RecordId>) -> Arc<RecordBody> {
    db.record_body_with_source(record_id).0
}

fn attribute_body(db: &dyn DefDatabase, attribute_id: InFile<AttributeId>) -> Arc<AttributeBody> {
    db.attribute_body_with_source(attribute_id).0
}

fn compile_body(db: &dyn DefDatabase, attribute_id: InFile<CompileOptionId>) -> Arc<AttributeBody> {
    db.compile_body_with_source(attribute_id).0
}

fn define_body(db: &dyn DefDatabase, define_id: InFile<DefineId>) -> Arc<DefineBody> {
    db.define_body_with_source(define_id).0
}

fn ssr_body(db: &dyn DefDatabase, ssr_source: SsrSource) -> Option<Arc<SsrBody>> {
    db.ssr_body_with_source(ssr_source)
        .map(|(body, _source)| body)
}

// `file_preprocessor_analysis_dispatch` and `_inner` live in `preprocessor.rs`
// alongside the with-diagnostics dispatch / inner / cycle recovery siblings.

fn file_external_defines_dispatch(db: &dyn DefDatabase, file_id: FileId) -> Arc<Vec<Name>> {
    db.file_external_defines_interned(InternedFileId::new(db, file_id))
}

/// Extract macro names from application data for external defines.
/// Macros in AppData.macros are stored as eetf::Term and can be:
/// - An Atom (just the macro name, with implicit value true)
/// - A Tuple of (Atom key, value) for {MacroName, Value}
fn file_external_defines_inner(db: &dyn DefDatabase, fid: InternedFileId) -> Arc<Vec<Name>> {
    let file_id = fid.file_id(db);
    // Define ELP_ERLANG_SERVICE to match the Erlang service behavior.
    // Headers like assert.hrl use -ifdef(ELP_ERLANG_SERVICE) to select
    // simplified macro expansions that work without parse transforms.
    let mut names: Vec<Name> = vec![Name::from_erlang_service("ELP_ERLANG_SERVICE")];

    if let Some(app_data) = db.file_app_data(file_id) {
        names.extend(app_data.macros.iter().filter_map(|term| {
            match term {
                // Just an atom means -D<NAME>
                eetf::Term::Atom(atom) => Some(Name::from_erlang_service(&atom.name)),
                // A tuple {Name, Value} means -D<NAME>=<VALUE>
                eetf::Term::Tuple(tuple) => {
                    if let Some(eetf::Term::Atom(key)) = tuple.elements.first() {
                        Some(Name::from_erlang_service(&key.name))
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }));
    }

    Arc::new(names)
}

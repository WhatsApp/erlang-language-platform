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

use elp_base_db::App;
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
use crate::ImplicitCall;
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
use crate::preprocessor::PreprocessorMacroDefs;

#[salsa::db]
pub trait DefDatabase:
    InternDatabase + Upcast<dyn InternDatabase> + RootQueryDb + Upcast<dyn RootQueryDb> + TypedSemantic
{
    fn file_form_list(&self, file_id: FileId) -> Arc<FormList>;

    fn file_form_list_interned(&self, fid: InternedFileId) -> Arc<FormList>;

    fn function_body_with_source(
        &self,
        function_id: InFile<FunctionDefId>,
    ) -> (Arc<FunctionBody>, Vec<Arc<BodySourceMap>>);

    fn function_body_with_source_interned(
        &self,
        fid: crate::InternedInFileFunctionDef,
    ) -> (Arc<FunctionBody>, Vec<Arc<BodySourceMap>>);

    fn function_clause_body_with_source(
        &self,
        function_clause_id: InFile<FunctionClauseId>,
    ) -> (Arc<FunctionClauseBody>, Arc<BodySourceMap>);

    fn function_clause_body_with_source_interned(
        &self,
        fid: crate::InternedInFileFunctionClause,
    ) -> (Arc<FunctionClauseBody>, Arc<BodySourceMap>);

    fn record_body_with_source(
        &self,
        record_id: InFile<RecordId>,
    ) -> (Arc<RecordBody>, Arc<BodySourceMap>);

    fn record_body_with_source_interned(
        &self,
        fid: crate::InternedInFileRecord,
    ) -> (Arc<RecordBody>, Arc<BodySourceMap>);

    fn spec_body_with_source(&self, spec_id: InFile<SpecId>)
    -> (Arc<SpecBody>, Arc<BodySourceMap>);

    fn spec_body_with_source_interned(
        &self,
        fid: crate::InternedInFileSpec,
    ) -> (Arc<SpecBody>, Arc<BodySourceMap>);

    fn callback_body_with_source(
        &self,
        callback_id: InFile<CallbackId>,
    ) -> (Arc<SpecBody>, Arc<BodySourceMap>);

    fn callback_body_with_source_interned(
        &self,
        fid: crate::InternedInFileCallback,
    ) -> (Arc<SpecBody>, Arc<BodySourceMap>);

    fn type_body_with_source(
        &self,
        type_alias_id: InFile<TypeAliasId>,
    ) -> (Arc<TypeBody>, Arc<BodySourceMap>);

    fn type_body_with_source_interned(
        &self,
        fid: crate::InternedInFileTypeAlias,
    ) -> (Arc<TypeBody>, Arc<BodySourceMap>);

    fn attribute_body_with_source(
        &self,
        attribute_id: InFile<AttributeId>,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>);

    fn attribute_body_with_source_interned(
        &self,
        fid: crate::InternedInFileAttribute,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>);

    fn compile_body_with_source(
        &self,
        attribute_id: InFile<CompileOptionId>,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>);

    fn compile_body_with_source_interned(
        &self,
        fid: crate::InternedInFileCompileOption,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>);

    fn define_body_with_source(
        &self,
        define_id: InFile<DefineId>,
    ) -> (Arc<DefineBody>, Arc<BodySourceMap>);

    fn define_body_with_source_interned(
        &self,
        fid: crate::InternedInFileDefine,
    ) -> (Arc<DefineBody>, Arc<BodySourceMap>);

    fn condition_body_with_source(
        &self,
        cond_id: InFile<PPConditionId>,
    ) -> Option<(Arc<ConditionBody>, Arc<BodySourceMap>)>;

    fn condition_body_with_source_interned(
        &self,
        fid: crate::InternedInFilePPCondition,
    ) -> Option<(Arc<ConditionBody>, Arc<BodySourceMap>)>;

    fn ssr_body_with_source(
        &self,
        ssr_source: SsrSource,
    ) -> Option<(Arc<SsrBody>, Arc<BodySourceMap>)>;

    // Projection queries to stop recomputation if structure didn't change, even if positions did
    fn function_body(&self, function_id: InFile<FunctionDefId>) -> Arc<FunctionBody>;

    fn function_body_interned(&self, fid: crate::InternedInFileFunctionDef) -> Arc<FunctionBody>;

    fn function_clause_body(
        &self,
        function_clause_id: InFile<FunctionClauseId>,
    ) -> Arc<FunctionClauseBody>;

    fn function_clause_body_interned(
        &self,
        fid: crate::InternedInFileFunctionClause,
    ) -> Arc<FunctionClauseBody>;
    fn type_body(&self, type_alias_id: InFile<TypeAliasId>) -> Arc<TypeBody>;
    fn type_body_interned(&self, fid: crate::InternedInFileTypeAlias) -> Arc<TypeBody>;

    fn spec_body(&self, spec_id: InFile<SpecId>) -> Arc<SpecBody>;
    fn spec_body_interned(&self, fid: crate::InternedInFileSpec) -> Arc<SpecBody>;

    fn callback_body(&self, callback_id: InFile<CallbackId>) -> Arc<SpecBody>;
    fn callback_body_interned(&self, fid: crate::InternedInFileCallback) -> Arc<SpecBody>;

    fn record_body(&self, record_id: InFile<RecordId>) -> Arc<RecordBody>;
    fn record_body_interned(&self, fid: crate::InternedInFileRecord) -> Arc<RecordBody>;

    fn attribute_body(&self, attribute_id: InFile<AttributeId>) -> Arc<AttributeBody>;
    fn attribute_body_interned(&self, fid: crate::InternedInFileAttribute) -> Arc<AttributeBody>;

    fn compile_body(&self, attribute_id: InFile<CompileOptionId>) -> Arc<AttributeBody>;
    fn compile_body_interned(&self, fid: crate::InternedInFileCompileOption) -> Arc<AttributeBody>;

    fn define_body(&self, define_id: InFile<DefineId>) -> Arc<DefineBody>;
    fn define_body_interned(&self, fid: crate::InternedInFileDefine) -> Arc<DefineBody>;
    fn ssr_body(&self, ssr_source: SsrSource) -> Option<Arc<SsrBody>>;

    fn function_clause_implicit_calls(
        &self,
        clause: InFile<FunctionClauseId>,
    ) -> Arc<Vec<ImplicitCall>>;

    fn function_clause_implicit_calls_interned(
        &self,
        fid: crate::InternedInFileFunctionClause,
    ) -> Arc<Vec<ImplicitCall>>;

    fn function_scopes(&self, fun: InFile<FunctionDefId>) -> Arc<FunctionScopes>;

    fn function_scopes_interned(
        &self,
        fid: crate::InternedInFileFunctionDef,
    ) -> Arc<FunctionScopes>;

    fn function_clause_scopes(&self, clause: InFile<FunctionClauseId>) -> Arc<ExprScopes>;

    fn function_clause_scopes_interned(
        &self,
        fid: crate::InternedInFileFunctionClause,
    ) -> Arc<ExprScopes>;

    fn resolve_include(
        &self,
        orig_app: Option<App>,
        include_id: InFile<IncludeAttributeId>,
    ) -> Option<FileId>;

    fn resolve_include_interned(&self, key: crate::InternedResolveInclude) -> Option<FileId>;

    fn resolve_macro(&self, file_id: FileId, name: MacroName) -> Option<ResolvedMacro>;

    fn resolve_macro_interned(&self, fid: InternedFileId, name: MacroName)
    -> Option<ResolvedMacro>;

    fn file_edoc_comments(
        &self,
        file_id: FileId,
    ) -> Option<Arc<FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>>>;

    fn file_edoc_comments_interned(
        &self,
        fid: InternedFileId,
    ) -> Option<Arc<FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>>>;

    fn project_macro_environment(&self, file_id: FileId) -> Arc<MacroEnvironment>;

    fn project_macro_environment_interned(&self, fid: InternedFileId) -> Arc<MacroEnvironment>;

    fn file_preprocessor_analysis_with_diagnostics(
        &self,
        file_id: FileId,
        env: Arc<MacroEnvironment>,
    ) -> (Arc<PreprocessorAnalysis>, Arc<ConditionDiagnosticsMap>);

    fn file_preprocessor_analysis_with_diagnostics_interned(
        &self,
        fid: InternedFileId,
        env: Arc<MacroEnvironment>,
    ) -> (Arc<PreprocessorAnalysis>, Arc<ConditionDiagnosticsMap>);

    // Projection query - just returns analysis without diagnostics
    fn file_preprocessor_analysis(
        &self,
        file_id: FileId,
        env: Arc<MacroEnvironment>,
    ) -> Arc<PreprocessorAnalysis>;

    fn file_preprocessor_analysis_interned(
        &self,
        fid: InternedFileId,
        env: Arc<MacroEnvironment>,
    ) -> Arc<PreprocessorAnalysis>;

    /// Point-in-time macro definition snapshots for a file, keyed on its macro
    /// environment. Collects the `env_macro_defs` and `condition_macro_defs`
    /// maps by running the preprocessor pass with `compute_macro_defs = true`.
    fn file_macro_defs(
        &self,
        file_id: FileId,
        env: Arc<MacroEnvironment>,
    ) -> Arc<PreprocessorMacroDefs>;

    fn file_macro_defs_interned(
        &self,
        fid: InternedFileId,
        env: Arc<MacroEnvironment>,
    ) -> Arc<PreprocessorMacroDefs>;

    // Helper query to run the recursive resolution algorithm
    fn local_resolve_macro(&self, file_id: FileId, name: MacroName) -> MacroResolution;

    fn local_resolve_macro_interned(&self, fid: InternedFileId, name: MacroName)
    -> MacroResolution;

    fn def_map(&self, file_id: FileId) -> Arc<DefMap>;

    fn def_map_interned(&self, fid: InternedFileId) -> Arc<DefMap>;

    fn def_map_with_env(&self, file_id: FileId, env: Arc<MacroEnvironment>) -> Arc<DefMap>;

    fn def_map_with_env_interned(
        &self,
        fid: InternedFileId,
        env: Arc<MacroEnvironment>,
    ) -> Arc<DefMap>;

    // Helper query to compute only local data, avoids recomputation of header data,
    // if only local information changed
    fn def_map_local(&self, file_id: FileId) -> Arc<DefMap>;

    fn def_map_local_interned(&self, fid: InternedFileId) -> Arc<DefMap>;

    /// Returns the macro names defined externally (e.g., via `-D` command line flags)
    /// for a given file, based on its application's configuration.
    fn file_external_defines(&self, file_id: FileId) -> Arc<Vec<Name>>;

    fn file_external_defines_interned(&self, fid: InternedFileId) -> Arc<Vec<Name>>;
}

#[salsa::db]
impl<DB> DefDatabase for DB
where
    DB: InternDatabase
        + Upcast<dyn InternDatabase>
        + RootQueryDb
        + Upcast<dyn RootQueryDb>
        + TypedSemantic,
{
    fn file_form_list(&self, file_id: FileId) -> Arc<FormList> {
        FormList::file_form_list_dispatch(self, file_id)
    }

    fn file_form_list_interned(&self, fid: InternedFileId) -> Arc<FormList> {
        file_form_list_interned_tracked(self, fid)
    }

    fn function_body_with_source(
        &self,
        function_id: InFile<FunctionDefId>,
    ) -> (Arc<FunctionBody>, Vec<Arc<BodySourceMap>>) {
        FunctionBody::function_body_with_source_dispatch(self, function_id)
    }

    fn function_body_with_source_interned(
        &self,
        fid: crate::InternedInFileFunctionDef,
    ) -> (Arc<FunctionBody>, Vec<Arc<BodySourceMap>>) {
        function_body_with_source_interned_tracked(self, fid)
    }

    fn function_clause_body_with_source(
        &self,
        function_clause_id: InFile<FunctionClauseId>,
    ) -> (Arc<FunctionClauseBody>, Arc<BodySourceMap>) {
        FunctionClauseBody::function_clause_body_with_source_dispatch(self, function_clause_id)
    }

    fn function_clause_body_with_source_interned(
        &self,
        fid: crate::InternedInFileFunctionClause,
    ) -> (Arc<FunctionClauseBody>, Arc<BodySourceMap>) {
        function_clause_body_with_source_interned_tracked(self, fid)
    }

    fn record_body_with_source(
        &self,
        record_id: InFile<RecordId>,
    ) -> (Arc<RecordBody>, Arc<BodySourceMap>) {
        RecordBody::record_body_with_source_dispatch(self, record_id)
    }

    fn record_body_with_source_interned(
        &self,
        fid: crate::InternedInFileRecord,
    ) -> (Arc<RecordBody>, Arc<BodySourceMap>) {
        record_body_with_source_interned_tracked(self, fid)
    }

    fn spec_body_with_source(
        &self,
        spec_id: InFile<SpecId>,
    ) -> (Arc<SpecBody>, Arc<BodySourceMap>) {
        SpecBody::spec_body_with_source_dispatch(self, spec_id)
    }

    fn spec_body_with_source_interned(
        &self,
        fid: crate::InternedInFileSpec,
    ) -> (Arc<SpecBody>, Arc<BodySourceMap>) {
        spec_body_with_source_interned_tracked(self, fid)
    }

    fn callback_body_with_source(
        &self,
        callback_id: InFile<CallbackId>,
    ) -> (Arc<SpecBody>, Arc<BodySourceMap>) {
        SpecBody::callback_body_with_source_dispatch(self, callback_id)
    }

    fn callback_body_with_source_interned(
        &self,
        fid: crate::InternedInFileCallback,
    ) -> (Arc<SpecBody>, Arc<BodySourceMap>) {
        callback_body_with_source_interned_tracked(self, fid)
    }

    fn type_body_with_source(
        &self,
        type_alias_id: InFile<TypeAliasId>,
    ) -> (Arc<TypeBody>, Arc<BodySourceMap>) {
        TypeBody::type_body_with_source_dispatch(self, type_alias_id)
    }

    fn type_body_with_source_interned(
        &self,
        fid: crate::InternedInFileTypeAlias,
    ) -> (Arc<TypeBody>, Arc<BodySourceMap>) {
        type_body_with_source_interned_tracked(self, fid)
    }

    fn attribute_body_with_source(
        &self,
        attribute_id: InFile<AttributeId>,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>) {
        AttributeBody::attribute_body_with_source_dispatch(self, attribute_id)
    }

    fn attribute_body_with_source_interned(
        &self,
        fid: crate::InternedInFileAttribute,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>) {
        attribute_body_with_source_interned_tracked(self, fid)
    }

    fn compile_body_with_source(
        &self,
        attribute_id: InFile<CompileOptionId>,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>) {
        AttributeBody::compile_body_with_source_dispatch(self, attribute_id)
    }

    fn compile_body_with_source_interned(
        &self,
        fid: crate::InternedInFileCompileOption,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>) {
        compile_body_with_source_interned_tracked(self, fid)
    }

    fn define_body_with_source(
        &self,
        define_id: InFile<DefineId>,
    ) -> (Arc<DefineBody>, Arc<BodySourceMap>) {
        DefineBody::define_body_with_source_dispatch(self, define_id)
    }

    fn define_body_with_source_interned(
        &self,
        fid: crate::InternedInFileDefine,
    ) -> (Arc<DefineBody>, Arc<BodySourceMap>) {
        define_body_with_source_interned_tracked(self, fid)
    }

    fn condition_body_with_source(
        &self,
        cond_id: InFile<PPConditionId>,
    ) -> Option<(Arc<ConditionBody>, Arc<BodySourceMap>)> {
        ConditionBody::condition_body_with_source_dispatch(self, cond_id)
    }

    fn condition_body_with_source_interned(
        &self,
        fid: crate::InternedInFilePPCondition,
    ) -> Option<(Arc<ConditionBody>, Arc<BodySourceMap>)> {
        condition_body_with_source_interned_tracked(self, fid)
    }

    fn ssr_body_with_source(
        &self,
        ssr_source: SsrSource,
    ) -> Option<(Arc<SsrBody>, Arc<BodySourceMap>)> {
        ssr_body_with_source_tracked(self, ssr_source)
    }

    fn function_body(&self, function_id: InFile<FunctionDefId>) -> Arc<FunctionBody> {
        function_body_dispatch(self, function_id)
    }

    fn function_body_interned(&self, fid: crate::InternedInFileFunctionDef) -> Arc<FunctionBody> {
        function_body_inner(self, fid)
    }

    fn function_clause_body(
        &self,
        function_clause_id: InFile<FunctionClauseId>,
    ) -> Arc<FunctionClauseBody> {
        function_clause_body_dispatch(self, function_clause_id)
    }

    fn function_clause_body_interned(
        &self,
        fid: crate::InternedInFileFunctionClause,
    ) -> Arc<FunctionClauseBody> {
        function_clause_body_inner(self, fid)
    }

    fn type_body(&self, type_alias_id: InFile<TypeAliasId>) -> Arc<TypeBody> {
        type_body_dispatch(self, type_alias_id)
    }

    fn type_body_interned(&self, fid: crate::InternedInFileTypeAlias) -> Arc<TypeBody> {
        type_body_inner(self, fid)
    }

    fn spec_body(&self, spec_id: InFile<SpecId>) -> Arc<SpecBody> {
        spec_body_dispatch(self, spec_id)
    }

    fn spec_body_interned(&self, fid: crate::InternedInFileSpec) -> Arc<SpecBody> {
        spec_body_inner(self, fid)
    }

    fn callback_body(&self, callback_id: InFile<CallbackId>) -> Arc<SpecBody> {
        callback_body_dispatch(self, callback_id)
    }

    fn callback_body_interned(&self, fid: crate::InternedInFileCallback) -> Arc<SpecBody> {
        callback_body_inner(self, fid)
    }

    fn record_body(&self, record_id: InFile<RecordId>) -> Arc<RecordBody> {
        record_body_dispatch(self, record_id)
    }

    fn record_body_interned(&self, fid: crate::InternedInFileRecord) -> Arc<RecordBody> {
        record_body_inner(self, fid)
    }

    fn attribute_body(&self, attribute_id: InFile<AttributeId>) -> Arc<AttributeBody> {
        attribute_body_dispatch(self, attribute_id)
    }

    fn attribute_body_interned(&self, fid: crate::InternedInFileAttribute) -> Arc<AttributeBody> {
        attribute_body_inner(self, fid)
    }

    fn compile_body(&self, attribute_id: InFile<CompileOptionId>) -> Arc<AttributeBody> {
        compile_body_dispatch(self, attribute_id)
    }

    fn compile_body_interned(&self, fid: crate::InternedInFileCompileOption) -> Arc<AttributeBody> {
        compile_body_inner(self, fid)
    }

    fn define_body(&self, define_id: InFile<DefineId>) -> Arc<DefineBody> {
        define_body_dispatch(self, define_id)
    }

    fn define_body_interned(&self, fid: crate::InternedInFileDefine) -> Arc<DefineBody> {
        define_body_inner(self, fid)
    }

    fn ssr_body(&self, ssr_source: SsrSource) -> Option<Arc<SsrBody>> {
        ssr_body(self, ssr_source)
    }

    fn function_clause_implicit_calls(
        &self,
        clause: InFile<FunctionClauseId>,
    ) -> Arc<Vec<ImplicitCall>> {
        crate::sema::implicit_calls::function_clause_implicit_calls_dispatch(self, clause)
    }

    fn function_clause_implicit_calls_interned(
        &self,
        fid: crate::InternedInFileFunctionClause,
    ) -> Arc<Vec<ImplicitCall>> {
        crate::sema::implicit_calls::function_clause_implicit_calls_inner(self, fid)
    }

    fn function_scopes(&self, fun: InFile<FunctionDefId>) -> Arc<FunctionScopes> {
        FunctionScopes::function_scopes_dispatch(self, fun)
    }

    fn function_scopes_interned(
        &self,
        fid: crate::InternedInFileFunctionDef,
    ) -> Arc<FunctionScopes> {
        function_scopes_interned_tracked(self, fid)
    }

    fn function_clause_scopes(&self, clause: InFile<FunctionClauseId>) -> Arc<ExprScopes> {
        FunctionScopes::function_clause_scopes_dispatch(self, clause)
    }

    fn function_clause_scopes_interned(
        &self,
        fid: crate::InternedInFileFunctionClause,
    ) -> Arc<ExprScopes> {
        function_clause_scopes_interned_tracked(self, fid)
    }

    fn resolve_include(
        &self,
        orig_app: Option<App>,
        include_id: InFile<IncludeAttributeId>,
    ) -> Option<FileId> {
        include::resolve_dispatch(self, orig_app, include_id)
    }

    fn resolve_include_interned(&self, key: crate::InternedResolveInclude) -> Option<FileId> {
        include::resolve_inner(self, key)
    }

    fn resolve_macro(&self, file_id: FileId, name: MacroName) -> Option<ResolvedMacro> {
        macro_exp::resolve_dispatch(self, file_id, name)
    }

    fn resolve_macro_interned(
        &self,
        fid: InternedFileId,
        name: MacroName,
    ) -> Option<ResolvedMacro> {
        macro_exp::resolve_inner(self, fid, name)
    }

    fn file_edoc_comments(
        &self,
        file_id: FileId,
    ) -> Option<Arc<FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>>> {
        edoc::file_edoc_comments_dispatch(self, file_id)
    }

    fn file_edoc_comments_interned(
        &self,
        fid: InternedFileId,
    ) -> Option<Arc<FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>>> {
        edoc::file_edoc_comments_inner(self, fid)
    }

    fn project_macro_environment(&self, file_id: FileId) -> Arc<MacroEnvironment> {
        macro_exp::project_macro_environment_dispatch(self, file_id)
    }

    fn project_macro_environment_interned(&self, fid: InternedFileId) -> Arc<MacroEnvironment> {
        macro_exp::project_macro_environment_inner(self, fid)
    }

    fn file_preprocessor_analysis_with_diagnostics(
        &self,
        file_id: FileId,
        env: Arc<MacroEnvironment>,
    ) -> (Arc<PreprocessorAnalysis>, Arc<ConditionDiagnosticsMap>) {
        preprocessor::file_preprocessor_analysis_with_diagnostics_dispatch(self, file_id, env)
    }

    fn file_preprocessor_analysis_with_diagnostics_interned(
        &self,
        fid: InternedFileId,
        env: Arc<MacroEnvironment>,
    ) -> (Arc<PreprocessorAnalysis>, Arc<ConditionDiagnosticsMap>) {
        file_preprocessor_analysis_with_diagnostics_interned_tracked(self, fid, env)
    }

    fn file_preprocessor_analysis(
        &self,
        file_id: FileId,
        env: Arc<MacroEnvironment>,
    ) -> Arc<PreprocessorAnalysis> {
        preprocessor::file_preprocessor_analysis_dispatch(self, file_id, env)
    }

    fn file_preprocessor_analysis_interned(
        &self,
        fid: InternedFileId,
        env: Arc<MacroEnvironment>,
    ) -> Arc<PreprocessorAnalysis> {
        file_preprocessor_analysis_interned_tracked(self, fid, env)
    }

    fn file_macro_defs(
        &self,
        file_id: FileId,
        env: Arc<MacroEnvironment>,
    ) -> Arc<PreprocessorMacroDefs> {
        preprocessor::file_macro_defs_dispatch(self, file_id, env)
    }

    fn file_macro_defs_interned(
        &self,
        fid: InternedFileId,
        env: Arc<MacroEnvironment>,
    ) -> Arc<PreprocessorMacroDefs> {
        file_macro_defs_interned_tracked(self, fid, env)
    }

    fn local_resolve_macro(&self, file_id: FileId, name: MacroName) -> MacroResolution {
        macro_exp::local_resolve_dispatch(self, file_id, name)
    }

    fn local_resolve_macro_interned(
        &self,
        fid: InternedFileId,
        name: MacroName,
    ) -> MacroResolution {
        local_resolve_macro_interned_tracked(self, fid, name)
    }

    fn def_map(&self, file_id: FileId) -> Arc<DefMap> {
        DefMap::def_map_dispatch(self, file_id)
    }

    fn def_map_interned(&self, fid: InternedFileId) -> Arc<DefMap> {
        def_map_interned_tracked(self, fid)
    }

    fn def_map_with_env(&self, file_id: FileId, env: Arc<MacroEnvironment>) -> Arc<DefMap> {
        DefMap::def_map_with_env_dispatch(self, file_id, env)
    }

    fn def_map_with_env_interned(
        &self,
        fid: InternedFileId,
        env: Arc<MacroEnvironment>,
    ) -> Arc<DefMap> {
        def_map_with_env_interned_tracked(self, fid, env)
    }

    fn def_map_local(&self, file_id: FileId) -> Arc<DefMap> {
        DefMap::def_map_local_dispatch(self, file_id)
    }

    fn def_map_local_interned(&self, fid: InternedFileId) -> Arc<DefMap> {
        def_map_local_interned_tracked(self, fid)
    }

    fn file_external_defines(&self, file_id: FileId) -> Arc<Vec<Name>> {
        file_external_defines_dispatch(self, file_id)
    }

    fn file_external_defines_interned(&self, fid: InternedFileId) -> Arc<Vec<Name>> {
        file_external_defines_inner(self, fid)
    }
}

#[salsa::tracked(returns(clone))]
fn file_form_list_interned_tracked(db: &dyn DefDatabase, fid: InternedFileId) -> Arc<FormList> {
    FormList::file_form_list_inner(db, fid)
}

#[salsa::tracked(returns(clone))]
fn function_body_with_source_interned_tracked(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileFunctionDef,
) -> (Arc<FunctionBody>, Vec<Arc<BodySourceMap>>) {
    FunctionBody::function_body_with_source_inner(db, fid)
}

#[salsa::tracked(returns(clone))]
fn function_clause_body_with_source_interned_tracked(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileFunctionClause,
) -> (Arc<FunctionClauseBody>, Arc<BodySourceMap>) {
    FunctionClauseBody::function_clause_body_with_source_inner(db, fid)
}

#[salsa::tracked(returns(clone))]
fn record_body_with_source_interned_tracked(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileRecord,
) -> (Arc<RecordBody>, Arc<BodySourceMap>) {
    RecordBody::record_body_with_source_inner(db, fid)
}

#[salsa::tracked(returns(clone))]
fn spec_body_with_source_interned_tracked(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileSpec,
) -> (Arc<SpecBody>, Arc<BodySourceMap>) {
    SpecBody::spec_body_with_source_inner(db, fid)
}

#[salsa::tracked(returns(clone))]
fn callback_body_with_source_interned_tracked(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileCallback,
) -> (Arc<SpecBody>, Arc<BodySourceMap>) {
    SpecBody::callback_body_with_source_inner(db, fid)
}

#[salsa::tracked(returns(clone))]
fn type_body_with_source_interned_tracked(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileTypeAlias,
) -> (Arc<TypeBody>, Arc<BodySourceMap>) {
    TypeBody::type_body_with_source_inner(db, fid)
}

#[salsa::tracked(returns(clone))]
fn attribute_body_with_source_interned_tracked(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileAttribute,
) -> (Arc<AttributeBody>, Arc<BodySourceMap>) {
    AttributeBody::attribute_body_with_source_inner(db, fid)
}

#[salsa::tracked(returns(clone))]
fn compile_body_with_source_interned_tracked(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileCompileOption,
) -> (Arc<AttributeBody>, Arc<BodySourceMap>) {
    AttributeBody::compile_body_with_source_inner(db, fid)
}

#[salsa::tracked(returns(clone))]
fn define_body_with_source_interned_tracked(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileDefine,
) -> (Arc<DefineBody>, Arc<BodySourceMap>) {
    DefineBody::define_body_with_source_inner(db, fid)
}

#[salsa::tracked(returns(clone))]
fn condition_body_with_source_interned_tracked(
    db: &dyn DefDatabase,
    fid: crate::InternedInFilePPCondition,
) -> Option<(Arc<ConditionBody>, Arc<BodySourceMap>)> {
    ConditionBody::condition_body_with_source_inner(db, fid)
}

#[salsa::tracked(returns(clone))]
fn ssr_body_with_source_tracked(
    db: &dyn DefDatabase,
    ssr_source: SsrSource,
) -> Option<(Arc<SsrBody>, Arc<BodySourceMap>)> {
    SsrBody::ssr_body_with_source_query(db, ssr_source)
}

#[salsa::tracked(returns(clone))]
fn function_scopes_interned_tracked(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileFunctionDef,
) -> Arc<FunctionScopes> {
    FunctionScopes::function_scopes_inner(db, fid)
}

#[salsa::tracked(returns(clone))]
fn function_clause_scopes_interned_tracked(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileFunctionClause,
) -> Arc<ExprScopes> {
    FunctionScopes::function_clause_scopes_inner(db, fid)
}

#[salsa::tracked(returns(clone))]
fn def_map_local_interned_tracked(db: &dyn DefDatabase, fid: InternedFileId) -> Arc<DefMap> {
    DefMap::def_map_local_inner(db, fid)
}

#[salsa::tracked(returns(clone), cycle_result = preprocessor::recover_cycle_with_diagnostics)]
fn file_preprocessor_analysis_with_diagnostics_interned_tracked(
    db: &dyn DefDatabase,
    fid: InternedFileId,
    env: Arc<MacroEnvironment>,
) -> (Arc<PreprocessorAnalysis>, Arc<ConditionDiagnosticsMap>) {
    preprocessor::file_preprocessor_analysis_with_diagnostics_inner(db, fid, env)
}

#[salsa::tracked(returns(clone), cycle_result = preprocessor::recover_cycle)]
fn file_preprocessor_analysis_interned_tracked(
    db: &dyn DefDatabase,
    fid: InternedFileId,
    env: Arc<MacroEnvironment>,
) -> Arc<PreprocessorAnalysis> {
    preprocessor::file_preprocessor_analysis_inner(db, fid, env)
}

#[salsa::tracked(returns(clone), cycle_result = preprocessor::recover_cycle_macro_defs)]
fn file_macro_defs_interned_tracked(
    db: &dyn DefDatabase,
    fid: InternedFileId,
    env: Arc<MacroEnvironment>,
) -> Arc<PreprocessorMacroDefs> {
    preprocessor::file_macro_defs_inner(db, fid, env)
}

#[salsa::tracked(returns(clone), cycle_result = macro_exp::recover_cycle)]
fn local_resolve_macro_interned_tracked(
    db: &dyn DefDatabase,
    fid: InternedFileId,
    name: MacroName,
) -> MacroResolution {
    macro_exp::local_resolve_inner(db, fid, name)
}

#[salsa::tracked(returns(clone), cycle_result = DefMap::recover_cycle)]
fn def_map_interned_tracked(db: &dyn DefDatabase, fid: InternedFileId) -> Arc<DefMap> {
    DefMap::def_map_inner(db, fid)
}

#[salsa::tracked(returns(clone), cycle_result = DefMap::recover_cycle_with_env)]
fn def_map_with_env_interned_tracked(
    db: &dyn DefDatabase,
    fid: InternedFileId,
    env: Arc<MacroEnvironment>,
) -> Arc<DefMap> {
    DefMap::def_map_with_env_inner(db, fid, env)
}

fn function_body_dispatch(
    db: &dyn DefDatabase,
    function_id: InFile<FunctionDefId>,
) -> Arc<FunctionBody> {
    db.function_body_interned(crate::InternedInFileFunctionDef::new(db, function_id))
}

#[salsa::tracked(returns(clone))]
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

#[salsa::tracked(returns(clone))]
fn function_clause_body_inner(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileFunctionClause,
) -> Arc<FunctionClauseBody> {
    let function_clause_id = fid.value(db);
    db.function_clause_body_with_source(function_clause_id).0
}

fn type_body_dispatch(db: &dyn DefDatabase, type_alias_id: InFile<TypeAliasId>) -> Arc<TypeBody> {
    db.type_body_interned(crate::InternedInFileTypeAlias::new(db, type_alias_id))
}
#[salsa::tracked(returns(clone))]
fn type_body_inner(db: &dyn DefDatabase, fid: crate::InternedInFileTypeAlias) -> Arc<TypeBody> {
    db.type_body_with_source(fid.value(db)).0
}

fn spec_body_dispatch(db: &dyn DefDatabase, spec_id: InFile<SpecId>) -> Arc<SpecBody> {
    db.spec_body_interned(crate::InternedInFileSpec::new(db, spec_id))
}
#[salsa::tracked(returns(clone))]
fn spec_body_inner(db: &dyn DefDatabase, fid: crate::InternedInFileSpec) -> Arc<SpecBody> {
    db.spec_body_with_source(fid.value(db)).0
}

fn callback_body_dispatch(db: &dyn DefDatabase, callback_id: InFile<CallbackId>) -> Arc<SpecBody> {
    db.callback_body_interned(crate::InternedInFileCallback::new(db, callback_id))
}
#[salsa::tracked(returns(clone))]
fn callback_body_inner(db: &dyn DefDatabase, fid: crate::InternedInFileCallback) -> Arc<SpecBody> {
    db.callback_body_with_source(fid.value(db)).0
}

fn record_body_dispatch(db: &dyn DefDatabase, record_id: InFile<RecordId>) -> Arc<RecordBody> {
    db.record_body_interned(crate::InternedInFileRecord::new(db, record_id))
}
#[salsa::tracked(returns(clone))]
fn record_body_inner(db: &dyn DefDatabase, fid: crate::InternedInFileRecord) -> Arc<RecordBody> {
    db.record_body_with_source(fid.value(db)).0
}

fn attribute_body_dispatch(
    db: &dyn DefDatabase,
    attribute_id: InFile<AttributeId>,
) -> Arc<AttributeBody> {
    db.attribute_body_interned(crate::InternedInFileAttribute::new(db, attribute_id))
}
#[salsa::tracked(returns(clone))]
fn attribute_body_inner(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileAttribute,
) -> Arc<AttributeBody> {
    db.attribute_body_with_source(fid.value(db)).0
}

fn compile_body_dispatch(
    db: &dyn DefDatabase,
    attribute_id: InFile<CompileOptionId>,
) -> Arc<AttributeBody> {
    db.compile_body_interned(crate::InternedInFileCompileOption::new(db, attribute_id))
}
#[salsa::tracked(returns(clone))]
fn compile_body_inner(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileCompileOption,
) -> Arc<AttributeBody> {
    db.compile_body_with_source(fid.value(db)).0
}

fn define_body_dispatch(db: &dyn DefDatabase, define_id: InFile<DefineId>) -> Arc<DefineBody> {
    db.define_body_interned(crate::InternedInFileDefine::new(db, define_id))
}
#[salsa::tracked(returns(clone))]
fn define_body_inner(db: &dyn DefDatabase, fid: crate::InternedInFileDefine) -> Arc<DefineBody> {
    db.define_body_with_source(fid.value(db)).0
}

#[salsa::tracked(returns(clone))]
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
/// An app's macros are stored as eetf::Term and can be:
/// - An Atom (just the macro name, with implicit value true)
/// - A Tuple of (Atom key, value) for {MacroName, Value}
#[salsa::tracked(returns(clone))]
fn file_external_defines_inner(db: &dyn DefDatabase, fid: InternedFileId) -> Arc<Vec<Name>> {
    let file_id = fid.file_id(db);
    // Define ELP_ERLANG_SERVICE to match the Erlang service behavior.
    // Headers like assert.hrl use -ifdef(ELP_ERLANG_SERVICE) to select
    // simplified macro expansions that work without parse transforms.
    let mut names: Vec<Name> = vec![Name::from_erlang_service("ELP_ERLANG_SERVICE")];

    if let Some(app) = db.file_app(file_id) {
        names.extend(app.macros(db).iter().filter_map(|term| {
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

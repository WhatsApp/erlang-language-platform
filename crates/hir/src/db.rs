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

use elp_base_db::FileId;
use elp_base_db::RootQueryDb;
use elp_base_db::Upcast;
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
use crate::RecordBody;
use crate::RecordId;
use crate::ResolvedMacro;
use crate::SpecBody;
use crate::SpecId;
use crate::SsrBody;
use crate::SsrSource;
use crate::TypeAliasId;
use crate::TypeBody;
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

#[ra_ap_query_group_macro::query_group]
pub trait DefDatabase:
    InternDatabase + Upcast<dyn InternDatabase> + RootQueryDb + Upcast<dyn RootQueryDb> + TypedSemantic
{
    #[salsa::invoke(FormList::file_form_list_query)]
    fn file_form_list(&self, file_id: FileId) -> Arc<FormList>;

    #[salsa::invoke(FunctionBody::function_body_with_source_query)]
    fn function_body_with_source(
        &self,
        function_id: InFile<FunctionDefId>,
    ) -> (Arc<FunctionBody>, Vec<Arc<BodySourceMap>>);

    #[salsa::invoke(FunctionClauseBody::function_clause_body_with_source_query)]
    fn function_clause_body_with_source(
        &self,
        function_clause_id: InFile<FunctionClauseId>,
    ) -> (Arc<FunctionClauseBody>, Arc<BodySourceMap>);

    #[salsa::invoke(RecordBody::record_body_with_source_query)]
    fn record_body_with_source(
        &self,
        record_id: InFile<RecordId>,
    ) -> (Arc<RecordBody>, Arc<BodySourceMap>);

    #[salsa::invoke(SpecBody::spec_body_with_source_query)]
    fn spec_body_with_source(&self, spec_id: InFile<SpecId>)
    -> (Arc<SpecBody>, Arc<BodySourceMap>);

    #[salsa::invoke(SpecBody::callback_body_with_source_query)]
    fn callback_body_with_source(
        &self,
        callback_id: InFile<CallbackId>,
    ) -> (Arc<SpecBody>, Arc<BodySourceMap>);

    #[salsa::invoke(TypeBody::type_body_with_source_query)]
    fn type_body_with_source(
        &self,
        type_alias_id: InFile<TypeAliasId>,
    ) -> (Arc<TypeBody>, Arc<BodySourceMap>);

    #[salsa::invoke(AttributeBody::attribute_body_with_source_query)]
    fn attribute_body_with_source(
        &self,
        attribute_id: InFile<AttributeId>,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>);

    #[salsa::invoke(AttributeBody::compile_body_with_source_query)]
    fn compile_body_with_source(
        &self,
        attribute_id: InFile<CompileOptionId>,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>);

    #[salsa::invoke(DefineBody::define_body_with_source_query)]
    fn define_body_with_source(
        &self,
        define_id: InFile<DefineId>,
    ) -> (Arc<DefineBody>, Arc<BodySourceMap>);

    #[salsa::invoke(SsrBody::ssr_body_with_source_query)]
    fn ssr_body_with_source(
        &self,
        ssr_source: SsrSource,
    ) -> Option<(Arc<SsrBody>, Arc<BodySourceMap>)>;

    // Projection queries to stop recomputation if structure didn't change, even if positions did
    fn function_body(&self, function_id: InFile<FunctionDefId>) -> Arc<FunctionBody>;
    fn function_clause_body(
        &self,
        function_clause_id: InFile<FunctionClauseId>,
    ) -> Arc<FunctionClauseBody>;
    fn type_body(&self, type_alias_id: InFile<TypeAliasId>) -> Arc<TypeBody>;
    fn spec_body(&self, spec_id: InFile<SpecId>) -> Arc<SpecBody>;
    fn callback_body(&self, callback_id: InFile<CallbackId>) -> Arc<SpecBody>;
    fn record_body(&self, record_id: InFile<RecordId>) -> Arc<RecordBody>;
    fn attribute_body(&self, attribute_id: InFile<AttributeId>) -> Arc<AttributeBody>;
    fn compile_body(&self, attribute_id: InFile<CompileOptionId>) -> Arc<AttributeBody>;
    fn define_body(&self, define_id: InFile<DefineId>) -> Arc<DefineBody>;
    fn ssr_body(&self, ssr_source: SsrSource) -> Option<Arc<SsrBody>>;

    #[salsa::invoke(FunctionScopes::function_scopes_query)]
    fn function_scopes(&self, fun: InFile<FunctionDefId>) -> Arc<FunctionScopes>;

    #[salsa::invoke(FunctionScopes::function_clause_scopes_query)]
    fn function_clause_scopes(&self, clause: InFile<FunctionClauseId>) -> Arc<ExprScopes>;

    #[salsa::invoke(include::resolve)]
    fn resolve_include(&self, include_id: InFile<IncludeAttributeId>) -> Option<FileId>;

    #[salsa::invoke(macro_exp::resolve_query)]
    fn resolve_macro(&self, file_id: FileId, name: MacroName) -> Option<ResolvedMacro>;

    #[salsa::invoke(edoc::file_edoc_comments_query)]
    fn file_edoc_comments(
        &self,
        file_id: FileId,
    ) -> Option<Arc<FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>>>;

    // Helper query to run the recursive resolution algorithm
    #[salsa::cycle(macro_exp::recover_cycle)]
    #[salsa::invoke(macro_exp::local_resolve_query)]
    fn local_resolve_macro(&self, file_id: FileId, name: MacroName) -> MacroResolution;

    #[salsa::cycle(DefMap::recover_cycle)]
    #[salsa::invoke(DefMap::def_map_query)]
    fn def_map(&self, file_id: FileId) -> Arc<DefMap>;

    // Helper query to compute only local data, avoids recomputation of header data,
    // if only local information changed
    #[salsa::invoke(DefMap::def_map_local_query)]
    fn def_map_local(&self, file_id: FileId) -> Arc<DefMap>;
}

fn function_body(db: &dyn DefDatabase, function_id: InFile<FunctionDefId>) -> Arc<FunctionBody> {
    db.function_body_with_source(function_id).0
}

fn function_clause_body(
    db: &dyn DefDatabase,
    function_clause_id: InFile<FunctionClauseId>,
) -> Arc<FunctionClauseBody> {
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

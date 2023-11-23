/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use elp_base_db::salsa;
use elp_base_db::FileId;
use elp_base_db::SourceDatabase;
use elp_base_db::Upcast;
use elp_syntax::ast;
use fxhash::FxHashMap;

use crate::body::scope::ExprScopes;
use crate::body::scope::FunctionScopes;
use crate::body::DefineBody;
use crate::def_map::FunctionDefId;
use crate::edoc;
use crate::edoc::EdocHeader;
use crate::include;
pub use crate::intern::MinInternDatabase;
pub use crate::intern::MinInternDatabaseStorage;
use crate::macro_exp;
use crate::macro_exp::MacroResolution;
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
use crate::FunctionId;
use crate::InFile;
use crate::InFileAstPtr;
use crate::IncludeAttributeId;
use crate::MacroName;
use crate::RecordBody;
use crate::RecordId;
use crate::ResolvedMacro;
use crate::SpecBody;
use crate::SpecId;
use crate::TypeAliasId;
use crate::TypeBody;

#[salsa::query_group(MinDefDatabaseStorage)]
pub trait MinDefDatabase:
    MinInternDatabase + Upcast<dyn MinInternDatabase> + SourceDatabase + Upcast<dyn SourceDatabase>
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
        function_id: InFile<FunctionId>,
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
    ) -> Option<(Arc<DefineBody>, Arc<BodySourceMap>)>;

    // Projection queries to stop recomputation if structure didn't change, even if positions did
    fn function_body(&self, function_id: InFile<FunctionDefId>) -> Arc<FunctionBody>;
    fn function_clause_body(&self, function_id: InFile<FunctionId>) -> Arc<FunctionClauseBody>;
    fn type_body(&self, type_alias_id: InFile<TypeAliasId>) -> Arc<TypeBody>;
    fn spec_body(&self, spec_id: InFile<SpecId>) -> Arc<SpecBody>;
    fn callback_body(&self, callback_id: InFile<CallbackId>) -> Arc<SpecBody>;
    fn record_body(&self, record_id: InFile<RecordId>) -> Arc<RecordBody>;
    fn attribute_body(&self, attribute_id: InFile<AttributeId>) -> Arc<AttributeBody>;
    fn compile_body(&self, attribute_id: InFile<CompileOptionId>) -> Arc<AttributeBody>;
    fn define_body(&self, define_id: InFile<DefineId>) -> Option<Arc<DefineBody>>;

    #[salsa::invoke(FunctionScopes::function_scopes_query)]
    fn function_scopes(&self, fun: InFile<FunctionDefId>) -> Arc<FunctionScopes>;

    #[salsa::invoke(FunctionScopes::function_clause_scopes_query)]
    fn function_clause_scopes(&self, fun: InFile<FunctionId>) -> Arc<ExprScopes>;

    #[salsa::invoke(include::resolve)]
    fn resolve_include(&self, include_id: InFile<IncludeAttributeId>) -> Option<FileId>;

    #[salsa::invoke(macro_exp::resolve_query)]
    fn resolve_macro(&self, file_id: FileId, name: MacroName) -> Option<ResolvedMacro>;

    #[salsa::invoke(edoc::file_edoc_comments_query)]
    fn file_edoc_comments(
        &self,
        file_id: FileId,
    ) -> Option<FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>>;

    // Helper query to run the recursive resolution algorithm
    #[salsa::cycle(macro_exp::recover_cycle)]
    #[salsa::invoke(macro_exp::local_resolve_query)]
    fn local_resolve_macro(&self, file_id: FileId, name: MacroName) -> MacroResolution;

    #[salsa::cycle(DefMap::recover_cycle)]
    #[salsa::invoke(DefMap::def_map_query)]
    fn def_map(&self, file_id: FileId) -> Arc<DefMap>;

    // Helper query to compute only local data, avoids recomputation of header data,
    // if only local information changed
    #[salsa::invoke(DefMap::local_def_map_query)]
    fn local_def_map(&self, file_id: FileId) -> Arc<DefMap>;
}

fn function_body(db: &dyn MinDefDatabase, function_id: InFile<FunctionDefId>) -> Arc<FunctionBody> {
    db.function_body_with_source(function_id).0
}

fn function_clause_body(
    db: &dyn MinDefDatabase,
    function_id: InFile<FunctionId>,
) -> Arc<FunctionClauseBody> {
    db.function_clause_body_with_source(function_id).0
}

fn type_body(db: &dyn MinDefDatabase, type_alias_id: InFile<TypeAliasId>) -> Arc<TypeBody> {
    db.type_body_with_source(type_alias_id).0
}

fn spec_body(db: &dyn MinDefDatabase, spec_id: InFile<SpecId>) -> Arc<SpecBody> {
    db.spec_body_with_source(spec_id).0
}

fn callback_body(db: &dyn MinDefDatabase, callback_id: InFile<CallbackId>) -> Arc<SpecBody> {
    db.callback_body_with_source(callback_id).0
}

fn record_body(db: &dyn MinDefDatabase, record_id: InFile<RecordId>) -> Arc<RecordBody> {
    db.record_body_with_source(record_id).0
}

fn attribute_body(
    db: &dyn MinDefDatabase,
    attribute_id: InFile<AttributeId>,
) -> Arc<AttributeBody> {
    db.attribute_body_with_source(attribute_id).0
}

fn compile_body(
    db: &dyn MinDefDatabase,
    attribute_id: InFile<CompileOptionId>,
) -> Arc<AttributeBody> {
    db.compile_body_with_source(attribute_id).0
}

fn define_body(db: &dyn MinDefDatabase, define_id: InFile<DefineId>) -> Option<Arc<DefineBody>> {
    db.define_body_with_source(define_id)
        .map(|(body, _source)| body)
}

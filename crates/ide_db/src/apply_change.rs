/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Applies changes to the IDE state transactionally.

use elp_base_db::Change;
use elp_base_db::FileId;
use elp_base_db::salsa::Query;
use elp_base_db::salsa::QueryTable;
use elp_base_db::salsa::debug::DebugQueryTable;
use elp_base_db::salsa::debug::TableEntry;
use paths::AbsPathBuf;
use profile::Bytes;
use profile::memory_usage;

use crate::RootDatabase;
use crate::common_test;
use crate::docs;
use crate::erl_ast;

impl RootDatabase {
    pub fn apply_change(
        &mut self,
        change: Change,
        resolve_file_id: &impl Fn(&AbsPathBuf) -> Option<FileId>,
    ) {
        let _p = tracing::info_span!("RootDatabase::apply_change").entered();
        self.request_cancellation();
        log::info!("apply_change {change:?}");
        change.apply(self, resolve_file_id);
    }

    pub fn per_query_memory_usage(&mut self) -> Vec<(String, Bytes, usize)> {
        let mut acc: Vec<(String, Bytes, usize)> = vec![];

        fn collect_query_count<'q, Q>(table: &QueryTable<'q, Q>) -> usize
        where
            QueryTable<'q, Q>: DebugQueryTable,
            Q: Query,
            <Q as Query>::Storage: 'q,
        {
            struct EntryCounter(usize);
            impl<K, V> FromIterator<TableEntry<K, V>> for EntryCounter {
                fn from_iter<T>(iter: T) -> EntryCounter
                where
                    T: IntoIterator<Item = TableEntry<K, V>>,
                {
                    EntryCounter(iter.into_iter().count())
                }
            }
            table.entries::<EntryCounter>().0
        }

        macro_rules! purge_each_query {
            ($($q:path)*) => {$(
                let before = memory_usage().allocated;
                let table = $q.in_db(self);
                let count = collect_query_count(&table);
                table.purge();
                let after = memory_usage().allocated;
                let q: $q = Default::default();
                let name = format!("{:?}", q);
                acc.push((name, before - after, count));
            )*}
        }
        // Grep for salsa::query_group in the codebase to find relevant queries
        // Simply convert the function name to camel case and add Query to the end
        purge_each_query![
            // SourceDatabase
            elp_base_db::ParseQuery
            elp_base_db::ModuleIndexQuery
            elp_base_db::IncludeFileIndexQuery
            elp_base_db::MappedIncludeFileQuery
            elp_base_db::FileAppDataQuery
            elp_base_db::AppDataQuery
            elp_base_db::AppIndexQuery

            // SourceDatabaseExt
            elp_base_db::FileTextQuery

            // DefDatabase
            hir::db::FileFormListQuery
            hir::db::DefMapQuery
            hir::db::DefMapLocalQuery
            hir::db::DefineBodyWithSourceQuery
            hir::db::CompileBodyWithSourceQuery
            hir::db::AttributeBodyWithSourceQuery
            hir::db::TypeBodyWithSourceQuery
            hir::db::CallbackBodyWithSourceQuery
            hir::db::SpecBodyWithSourceQuery
            hir::db::RecordBodyWithSourceQuery
            hir::db::FunctionBodyWithSourceQuery
            hir::db::FunctionClauseBodyWithSourceQuery
            hir::db::SsrBodyWithSourceQuery
            hir::db::LocalResolveMacroQuery
            hir::db::ResolveMacroQuery
            hir::db::FileEdocCommentsQuery
            hir::db::ResolveIncludeQuery
            hir::db::FunctionScopesQuery
            hir::db::FunctionClauseScopesQuery

            // CommonTest
            common_test::CtInfoQuery

            // Doc
            docs::FileDocQuery
            docs::FileSpecsQuery

            // ErlAST
            erl_ast::ModuleAstQuery
            erl_ast::ElpMetadataQuery

            // Eqwalizer
            elp_eqwalizer::db::ModuleDiagnosticsQuery
        ];

        acc.sort_by_key(|it| std::cmp::Reverse(it.1));
        acc
    }
}

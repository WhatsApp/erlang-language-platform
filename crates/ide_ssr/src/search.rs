/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Searching for matches.

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::SearchScope;
use fxhash::FxHashSet;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::AnyExprId;
use hir::BodyOrigin;
use hir::FormIdx;
use hir::SsrIdx;
use hir::Strategy;

use crate::matching;
use crate::matching::Match;
use crate::MatchFinder;
use crate::SsrPattern;

impl MatchFinder<'_> {
    /// Adds all matches for `rule` to `matches_out`. Matches may
    /// overlap in ways that make replacement impossible, so further
    /// processing is required in order to properly nest matches and
    /// remove overlapping matches. This is done in the `nesting`
    /// module.
    pub(crate) fn find_matches_for_rule(&self, rule: &SsrPattern, matches_out: &mut Vec<Match>) {
        self.search_files_do(|file_id| {
            let code = SsrIdx::WholeFile(file_id);
            self.slow_scan_node(&code, rule, &None, matches_out);
        })
    }

    fn search_files_do(&self, mut callback: impl FnMut(FileId)) {
        if self.restrict_ranges.is_empty() {
            // AZ: Look at using SymbolDefinition::search_scope() implementation
            let file_id = self.file_id;
            match self
                .sema
                .db
                .app_data(self.sema.db.file_source_root(file_id))
            {
                Some(app_data) => {
                    let search_scope = SearchScope::project(self.sema.db, app_data.project_id);
                    for (file_id, _) in search_scope.into_iter() {
                        callback(file_id);
                    }
                }
                None => callback(file_id),
            };
        } else {
            // Search is restricted, deduplicate file IDs (generally only one).
            let mut files = FxHashSet::default();
            for range in &self.restrict_ranges {
                if files.insert(range.file_id) {
                    callback(range.file_id);
                }
            }
        }
    }

    fn slow_scan_node(
        &self,
        code: &SsrIdx,
        rule: &SsrPattern,
        restrict_range: &Option<FileRange>,
        matches_out: &mut Vec<Match>,
    ) {
        // - Fold over the code.
        // - For each HIR AST node, check if there is a match
        //   Most will fail fast because the initial node
        //   does not match.

        code.fold(
            &self.sema,
            Strategy {
                macros: MacroStrategy::ExpandButIncludeMacroCall,
                parens: ParenStrategy::InvisibleParens,
            },
            (),
            &mut |_acc, ctx| {
                self.try_add_match(
                    rule,
                    &ctx.body_origin,
                    &ctx.item_id,
                    restrict_range,
                    matches_out,
                );
            },
            &mut |_acc, _on, _form_id: FormIdx| {},
        );
    }

    // Check for a match originating at this point in the `code` AST.
    fn try_add_match(
        &self,
        rule: &SsrPattern,
        code_body_origin: &BodyOrigin,
        code: &AnyExprId,
        restrict_range: &Option<FileRange>,
        matches_out: &mut Vec<Match>,
    ) {
        if let Ok(m) = matching::get_match(
            self.debug_print,
            rule,
            code_body_origin,
            code,
            restrict_range,
            &self.sema,
        ) {
            matches_out.push(m);
        }
    }
}

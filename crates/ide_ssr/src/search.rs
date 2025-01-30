/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Searching for matches.

use elp_ide_db::elp_base_db::FileRange;
use hir::fold::fold_body;
use hir::AnyExprId;
use hir::BodyOrigin;
use hir::FoldBody;
use hir::FormIdx;

use crate::matching;
use crate::matching::Match;
use crate::MatchFinder;
use crate::SsrPattern;
use crate::SsrSearchScope;

impl MatchFinder<'_> {
    /// Adds all matches for `rule` to `matches_out`. Matches may
    /// overlap in ways that make replacement impossible, so further
    /// processing is required in order to properly nest matches and
    /// remove overlapping matches. This is done in the `nesting`
    /// module.
    pub(crate) fn find_matches_for_rule(&self, rule: &SsrPattern, matches_out: &mut Vec<Match>) {
        let pattern_body = rule.get_body(self.sema).expect("Cannot get pattern_body");
        let pattern_body = fold_body(self.strategy, &pattern_body);
        let code = SsrSearchScope::WholeFile(self.file_id);
        self.slow_scan_node(&code, rule, &None, matches_out, &pattern_body);
    }

    fn slow_scan_node(
        &self,
        code: &SsrSearchScope,
        rule: &SsrPattern,
        restrict_range: &Option<FileRange>,
        matches_out: &mut Vec<Match>,
        pattern_body: &FoldBody,
    ) {
        // - Fold over the code.
        // - For each HIR AST node, check if there is a match
        //   Most will fail fast because the initial node
        //   does not match.

        code.fold(
            &self.sema,
            self.strategy,
            (),
            &mut |_acc, ctx| {
                let code_body = &ctx
                    .body_origin
                    .get_body(self.sema)
                    .expect("Could not get code Body");
                let code_body = fold_body(self.strategy, &code_body);
                self.try_add_match(
                    rule,
                    &ctx.body_origin,
                    &ctx.item_id,
                    restrict_range,
                    matches_out,
                    &code_body,
                    pattern_body,
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
        code_body: &FoldBody,
        pattern_body: &FoldBody,
    ) {
        if let Ok(m) = matching::get_match(
            self.debug_print,
            rule,
            code_body_origin,
            code,
            restrict_range,
            &self.sema,
            code_body,
            pattern_body,
        ) {
            matches_out.push(m);
        }
    }
}

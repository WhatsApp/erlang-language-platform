/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Structural Search and Replace
//!
//! Allows searching the AST for code that matches one or more
//! patterns and then replacing that code based on a template.

// Note: not all parts of the feature are currently implemented, but
// this is what we are aiming for.

// Feature: Structural Search and Replace
//
// Search and replace with named wildcards that will match any
// expression, type, path, pattern or item.  The syntax for a
// structural search replace command is `ssr: <search_pattern> ==>>
// <replace_pattern>.`.  A `_@<name>` placeholder in the search pattern
// will match any AST node and `_@<name>` will reference it in the
// replacement.  Within a macro call, a placeholder will match up
// until whatever token follows the placeholder.
//
// The scope of the search / replace will be restricted to the current
// selection if any, otherwise it will apply to the whole workspace.
//
// Available via the command `elp.ssr`.
//
// ```erlang
// // Using structural search replace command [ssr: foo(_@A, _@B) ==>> foo(_@B, _@A)]
//
// // BEFORE
// foo(Y + 5, Z)
//
// // AFTER
// foo(Z, Y + 5)
// ```
//
// |===
// | Editor  | Action Name
//
// | VS Code | **elp: Structural Search Replace**
// |===
//
// Also available as an assist, by writing a comment containing the structural
// search and replace rule. You will only see the assist if the comment can
// be parsed as a valid structural search and replace rule.
//
// ```erlang
// %% Place the cursor on the line below to see the assist 💡.
// %% ssr: foo(_@A, _@B) ==>> foo(_@B, _@A).
// ```

use std::sync::Arc;

use elp_ide_db::RootDatabase;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::elp_base_db::SourceDatabase;
use elp_syntax::AstNode;
use elp_syntax::SyntaxNode;
use elp_syntax::TextRange;
use elp_syntax::ast;
use elp_syntax::ast::CompOp;
use fxhash::FxHashMap;
use hir::AnyExprId;
use hir::AnyExprRef;
use hir::Atom;
use hir::Body;
use hir::Expr;
use hir::ExprId;
use hir::FoldBody;
use hir::FormIdx;
use hir::InFile;
use hir::InSsr;
use hir::Literal;
use hir::Name;
use hir::On;
use hir::Pat;
use hir::Semantic;
use hir::SsrBody;
use hir::SsrPatternIds;
use hir::SsrPlaceholder;
use hir::SsrSource;
use hir::db::DefDatabase;
use hir::db::InternDatabase;
use hir::fold::AnyCallBack;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::fold_body;
use hir::fold::fold_file;
use hir::fold::fold_file_functions;

#[macro_use]
mod errors;

mod matching;
mod nester;
mod search;
#[cfg(test)]
mod tests;

pub use errors::SsrError;
use hir::Strategy;
use hir::StringVariant;
use hir::Var;
pub use matching::Match;
pub use matching::MatchFailureReason;
pub use matching::PlaceholderMatch;
pub use matching::SubId;

// ---------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SsrSearchScope {
    WholeFile(FileId),
    FunctionsOnly(FileId),
}

impl SsrSearchScope {
    pub fn fold<'a, T>(
        &self,
        sema: &Semantic,
        strategy: Strategy,
        initial: T,
        callback: AnyCallBack<'a, T>,
        form_callback: &'a mut dyn FnMut(T, On, FormIdx) -> T,
    ) -> T {
        match &self {
            SsrSearchScope::WholeFile(file_id) => {
                fold_file(sema, strategy, *file_id, initial, callback, form_callback)
            }
            SsrSearchScope::FunctionsOnly(file_id) => {
                fold_file_functions(sema, strategy, *file_id, initial, callback)
            }
        }
    }
}

pub fn match_pattern_in_file(
    sema: &Semantic,
    strategy: Strategy,
    file_id: FileId,
    pattern: &str,
) -> SsrMatches {
    let pattern = SsrRule::parse_str(sema.db, pattern).expect("could not parse SSR pattern");
    let mut match_finder =
        MatchFinder::in_context(sema, strategy, SsrSearchScope::WholeFile(file_id));
    match_finder.debug_print = false;
    match_finder.add_search_pattern(pattern);
    let matches: SsrMatches = match_finder.matches().flattened();
    matches
}

pub fn match_pattern_in_file_functions(
    sema: &Semantic,
    strategy: Strategy,
    file_id: FileId,
    pattern: &str,
) -> SsrMatches {
    let pattern = SsrRule::parse_str(sema.db, pattern).expect("could not parse SSR pattern");
    let mut match_finder =
        MatchFinder::in_context(sema, strategy, SsrSearchScope::FunctionsOnly(file_id));
    match_finder.debug_print = false;
    match_finder.add_search_pattern(pattern);
    let matches: SsrMatches = match_finder.matches().flattened();
    matches
}

pub fn is_placeholder_a_var_from_body(body: &Body, m: &PlaceholderMatch) -> bool {
    match m.code_id {
        SubId::AnyExprId(AnyExprId::Expr(expr_id)) => match body.exprs[expr_id] {
            Expr::Var(_) => true,
            _ => false,
        },
        SubId::AnyExprId(AnyExprId::Pat(pat_id)) => match body.pats[pat_id] {
            Pat::Var(_) => true,
            _ => false,
        },
        SubId::Var(_) => true,
        _ => false,
    }
}

pub fn is_placeholder_a_var_from_sema_and_match(
    sema: &Semantic,
    overall_match: &Match,
    m: &PlaceholderMatch,
) -> bool {
    overall_match
        .matched_node_body
        .get_body(sema)
        .is_some_and(|body_arc| {
            let body = body_arc.as_ref();
            is_placeholder_a_var_from_body(body, m)
        })
}

// ---------------------------------------------------------------------

// A structured search replace rule. Create by calling `parse` on a str.

#[derive(Debug)]
pub struct SsrRule {
    parsed_rule: Arc<SsrBody>,
    conditions: FxHashMap<SsrPlaceholder, Condition>,
}

/// A possible condition extracted from the ssr rule `when` clause
#[derive(Debug)]
pub enum Condition {
    Literal(GuardLiteral),
    Not(Box<Condition>),
}

impl SsrRule {
    #[allow(unused)] // Used in tests
    pub(crate) fn tree_print(&self, db: &dyn InternDatabase) -> String {
        self.parsed_rule.tree_print(db)
    }

    fn parse_ssr_source(db: &dyn DefDatabase, ssr_source: SsrSource) -> Result<SsrRule, SsrError> {
        if let Some((ssr_body, _)) = db.ssr_body_with_source(ssr_source) {
            // Using a `FoldBody` with invisible parens is fine for
            // conditions, because either way we only check a
            // placeholder when we actually see one, and we do not
            // want to worry about parens in evaluating the
            // conditions.
            let body = fold_body(
                Strategy {
                    macros: MacroStrategy::Expand,
                    parens: ParenStrategy::InvisibleParens,
                },
                &ssr_body.body,
            );
            let conditions = SsrRule::make_conditions(&ssr_body, &body)?;
            Ok(SsrRule {
                parsed_rule: ssr_body.clone(),
                conditions,
            })
        } else {
            Err(SsrError("Could not lower rule".to_string()))
        }
    }

    pub fn parse_str(db: &dyn DefDatabase, pattern_str: &str) -> Result<SsrRule, SsrError> {
        let ssr_source = db.ssr(Arc::from(pattern_str));
        Self::parse_ssr_source(db, ssr_source)
    }

    /// The `when` clause is lowered as HIR guards.
    /// Process these and turn them into something we can easily check
    /// when matching.
    /// Initially we only support conditions which check that a
    /// placeholder is a literal, such as
    ///
    /// ```erlang
    /// ssr: _@X when _@X == foo.
    /// ```
    fn make_conditions(
        ssr_body: &SsrBody,
        body: &FoldBody,
    ) -> Result<FxHashMap<SsrPlaceholder, Condition>, SsrError> {
        let mut conditions: FxHashMap<SsrPlaceholder, Condition> = FxHashMap::default();
        let mut error = None;
        if let Some(w) = ssr_body.when.as_ref() {
            w.iter().for_each(|conds| {
                conds.iter().for_each(|cond| {
                    extract_condition(body, cond, &mut conditions, &mut error);
                });
            })
        }
        if let Some(error) = error {
            Err(error)
        } else {
            Ok(conditions)
        }
    }
}

fn extract_condition(
    body: &FoldBody,
    cond: &ExprId,
    conditions: &mut FxHashMap<SsrPlaceholder, Condition>,
    error: &mut Option<SsrError>,
) {
    match body[*cond] {
        Expr::BinaryOp { lhs, rhs, op } => {
            if let Expr::SsrPlaceholder(ssr_placeholder) = &body[lhs] {
                // We have a condition on the current placeholder, store it if valid
                match op {
                    ast::BinaryOp::CompOp(CompOp::Eq { strict: _, negated }) => {
                        if let Some(lit_rhs) =
                            get_literal_subid(body, &SubId::AnyExprId(AnyExprId::Expr(rhs)))
                        {
                            if negated {
                                conditions.insert(
                                    ssr_placeholder.clone(),
                                    Condition::Not(Box::new(Condition::Literal(lit_rhs.clone()))),
                                );
                            } else {
                                conditions.insert(
                                    ssr_placeholder.clone(),
                                    Condition::Literal(lit_rhs.clone()),
                                );
                            }
                        } else {
                            *error = Some(SsrError::new("Invalid `when` RHS, expecting a literal"));
                        }
                    }
                    _ => {
                        *error = Some(SsrError::new(
                            "Invalid `when` condition, must use `==`, `/=`, `=:=` or `=/=`",
                        ))
                    }
                }
            }
        }
        _ => {
            *error = Some(SsrError::new("Invalid `when` condition"));
        }
    }
}

#[derive(Debug)]
pub(crate) struct SsrPattern {
    pub(crate) ssr_source: SsrSource,
    pub(crate) conditions: FxHashMap<SsrPlaceholder, Condition>,
    pub(crate) pattern_node: SsrPatternIds,
    pub(crate) index: usize,
}

impl SsrPattern {
    pub(crate) fn new(rule: SsrRule, index: usize) -> SsrPattern {
        SsrPattern {
            ssr_source: rule.parsed_rule.ssr_source,
            conditions: rule.conditions,
            pattern_node: rule.parsed_rule.pattern.clone(),
            index,
        }
    }

    pub(crate) fn get_body(&self, sema: &Semantic) -> Option<Arc<Body>> {
        let (body, _body_map) = sema.db.ssr_body_with_source(self.ssr_source)?;
        Some(body.body.clone())
    }

    /// The pattern is lowered both as a HIR Pat and an Expr.
    /// Choose which of these to use for matching the given `code` node.
    pub(crate) fn pattern_sub_id_for_code(
        &self,
        pattern_body: &FoldBody,
        code: &AnyExprId,
    ) -> SubId {
        match code {
            AnyExprId::Expr(_) => SubId::AnyExprId(AnyExprId::Expr(self.pattern_node.expr)),
            AnyExprId::Pat(_) => match pattern_body[self.pattern_node.pat] {
                Pat::Missing => SubId::Constant("Cannot match pattern Pat::Missing".to_string()),
                _ => SubId::AnyExprId(AnyExprId::Pat(self.pattern_node.pat)),
            },
            _ => SubId::Constant("not implemented yet".to_string()),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct SsrMatches {
    pub matches: Vec<Match>,
}

/// Searches a file for pattern matches and possibly replaces them
/// with something else.
pub struct MatchFinder<'a> {
    /// Our source of information about the user's code.
    sema: &'a Semantic<'a>,
    rules: Vec<SsrPattern>,
    scope: SsrSearchScope,
    pub debug_print: bool,
    strategy: Strategy,
}

impl<'a> MatchFinder<'a> {
    /// Constructs a new instance where names will be looked up as if
    /// they appeared at `lookup_context`.
    pub fn in_context(
        sema: &'a Semantic<'a>,
        strategy: Strategy,
        scope: SsrSearchScope,
    ) -> MatchFinder<'a> {
        MatchFinder {
            sema,
            rules: Vec::new(),
            scope,
            debug_print: false,
            strategy,
        }
    }

    /// Adds a search pattern.
    pub fn add_search_pattern(&mut self, rule: SsrRule) {
        if self.debug_print {
            println!(
                "MatchFinder: adding pattern:\n{}",
                rule.tree_print(self.sema.db.upcast())
            );
        }
        self.rules.push(SsrPattern::new(rule, self.rules.len()));
    }

    /// Returns matches for all added rules.
    pub fn matches(&self) -> SsrMatches {
        let mut matches: Vec<Match> = Vec::new();
        for rule in &self.rules {
            self.find_matches_for_rule(rule, &mut matches);
        }
        nester::nest_and_remove_collisions(matches, self.sema)
    }

    /// Finds all nodes in `file_id` whose text is exactly equal to
    /// `snippet` and attempts to match them, while recording reasons
    /// why they don't match. This API is useful for command
    /// line-based debugging where providing a range is difficult.
    pub fn debug_where_text_equal(&self, file_id: FileId, snippet: &str) -> Vec<MatchDebugInfo> {
        let file = self.sema.parse(file_id);
        let mut res = Vec::new();
        let file_text = self.sema.db.file_text(file_id).text(self.sema.db);
        let mut remaining_text = &*file_text;
        let mut base = 0;
        let len = snippet.len() as u32;
        if len == 0 {
            return vec![];
        }
        while let Some(offset) = remaining_text.find(snippet) {
            let start = base + offset as u32;
            let end = start + len;
            self.output_debug_for_nodes_at_range(
                file.value.syntax(),
                FileRange {
                    file_id,
                    range: TextRange::new(start.into(), end.into()),
                },
                &None,
                &mut res,
            );
            remaining_text = &remaining_text[offset + snippet.len()..];
            base = end;
        }
        res
    }

    fn output_debug_for_nodes_at_range(
        &self,
        node: &SyntaxNode,
        range: FileRange,
        restrict_range: &Option<FileRange>,
        out: &mut Vec<MatchDebugInfo>,
    ) {
        for node in node.children() {
            let node_range = node.text_range();
            if !node_range.contains_range(range.range) {
                continue;
            }
            if node_range == range.range {
                if let Some(expr) = ast::Expr::cast(node.clone()) {
                    if let Some(in_clause_expr) =
                        self.sema.to_expr(InFile::new(range.file_id, &expr))
                    {
                        for rule in &self.rules {
                            let any_expr_id = AnyExprId::Expr(in_clause_expr.value);
                            let body_origin = in_clause_expr.body().origin;
                            let pattern_body =
                                rule.get_body(self.sema).expect("Cannot get pattern_body");
                            let pattern_body = fold_body(self.strategy, &pattern_body);
                            let code_body = &body_origin
                                .get_body(self.sema)
                                .expect("Could not get code Body");
                            let code_body = fold_body(self.strategy, code_body);
                            out.push(MatchDebugInfo {
                                matched: matching::get_match(
                                    true,
                                    rule,
                                    &rule.pattern_sub_id_for_code(&pattern_body, &any_expr_id),
                                    &body_origin,
                                    &any_expr_id,
                                    restrict_range,
                                    self.sema,
                                    &code_body,
                                    &pattern_body,
                                )
                                .map_err(|e| MatchFailureReason {
                                    reason: e.reason.unwrap_or_else(|| {
                                        "Match failed, but no reason was given".to_owned()
                                    }),
                                }),
                                pattern: InSsr::new(rule.ssr_source, rule.pattern_node.clone()),
                                node: node.clone(),
                            });
                        }
                    }
                }
            }
            self.output_debug_for_nodes_at_range(&node, range, restrict_range, out);
        }
    }
}

impl SsrMatches {
    /// Returns `self` with any nested matches removed and made into
    /// top-level matches.
    pub fn flattened(self) -> SsrMatches {
        let mut out = SsrMatches::default();
        self.flatten_into(&mut out);
        out
    }

    fn flatten_into(self, out: &mut SsrMatches) {
        for mut m in self.matches {
            for p in m.placeholder_values.values_mut() {
                std::mem::take(&mut p.inner_matches).flatten_into(out);
            }
            out.matches.push(m);
        }
    }
}

impl Match {
    pub fn matched_text(&self, db: &RootDatabase) -> String {
        let file_text = SourceDatabase::file_text(db, self.range.file_id).text(db);
        file_text[self.range.range.start().into()..self.range.range.end().into()].to_string()
    }

    pub fn get_placeholder_matches(
        &self,
        sema: &Semantic,
        placeholder_name: &str,
    ) -> Option<Vec<PlaceholderMatch>> {
        let var = sema.db.var(Name::from_erlang_service(placeholder_name));
        let subids = self.placeholders_by_var.get(&var)?;
        Some(
            subids
                .iter()
                .filter_map(|sub_id| self.placeholder_values.get(sub_id).cloned())
                .collect(),
        )
    }
    pub fn get_placeholder_match(
        &self,
        sema: &Semantic,
        placeholder_name: &str,
    ) -> Option<PlaceholderMatch> {
        let var = sema.db.var(Name::from_erlang_service(placeholder_name));
        let subids = self.placeholders_by_var.get(&var)?;
        if subids.len() == 1 {
            self.placeholder_values
                .get(&subids.iter().next().cloned()?)
                .cloned()
        } else {
            // We panic here because this should be used when doing
            // development only, give feedback to the dev.
            panic!(
                "expecting a single match for '{}', got multiple",
                placeholder_name
            );
        }
    }

    pub fn placeholder_texts(
        &self,
        sema: &Semantic,
        placeholder_name: &str,
    ) -> Option<Vec<String>> {
        let placeholder_matches = self.get_placeholder_matches(sema, placeholder_name)?;
        let body = self.matched_node_body.get_body(sema)?;
        Some(
            placeholder_matches
                .iter()
                .flat_map(|pm| pm.text(sema, &body))
                .collect(),
        )
    }

    pub fn placeholder_text(&self, sema: &Semantic, placeholder_name: &str) -> Option<String> {
        let placeholder_match = self.get_placeholder_match(sema, placeholder_name)?;
        let body = self.matched_node_body.get_body(sema)?;
        placeholder_match.text(sema, &body)
    }

    pub fn placeholder_ranges(
        &self,
        sema: &Semantic,
        placeholder_name: &str,
    ) -> Option<Vec<TextRange>> {
        let placeholder_matches = self.get_placeholder_matches(sema, placeholder_name)?;
        Some(placeholder_matches.iter().map(|pm| pm.range()).collect())
    }

    pub fn placeholder_range(&self, sema: &Semantic, placeholder_name: &str) -> Option<TextRange> {
        let placeholder_match = self.get_placeholder_match(sema, placeholder_name)?;
        Some(placeholder_match.range())
    }

    pub fn placeholder_is_string(
        &self,
        sema: &Semantic,
        placeholder_name: &str,
    ) -> Option<StringVariant> {
        let placeholder_match = self.get_placeholder_match(sema, placeholder_name)?;
        let body = self.matched_node_body.get_body(sema)?;
        placeholder_match.is_string(&body)
    }

    pub fn placeholder_is_atom(&self, sema: &Semantic, placeholder_name: &str) -> Option<Atom> {
        let placeholder_match = self.get_placeholder_match(sema, placeholder_name)?;
        let body = self.matched_node_body.get_body(sema)?;
        placeholder_match.is_atom(&body)
    }

    pub fn placeholder_is_var(&self, sema: &Semantic, placeholder_name: &str) -> Option<Var> {
        let placeholder_match = self.get_placeholder_match(sema, placeholder_name)?;
        let body = self.matched_node_body.get_body(sema)?;
        placeholder_match.is_var(&body)
    }
}

pub struct MatchDebugInfo {
    node: SyntaxNode,
    /// Our search pattern
    pattern: InSsr<SsrPatternIds>,
    matched: Result<Match, MatchFailureReason>,
}

impl std::fmt::Debug for MatchDebugInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.matched {
            Ok(_) => writeln!(f, "Node matched")?,
            Err(reason) => writeln!(f, "Node failed to match because: {}", reason.reason)?,
        }
        writeln!(
            f,
            "============ AST ===========\n\
            {:#?}",
            self.node
        )?;
        writeln!(f, "========= PATTERN ==========")?;
        writeln!(f, "{:#?}", self.pattern)?;
        writeln!(f, "============================")?;
        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum GuardLiteral {
    Literal(Literal),
    Var(Var),
}

fn get_literal_subid<'a>(body: &'a FoldBody, code: &'a SubId) -> Option<GuardLiteral> {
    match code {
        SubId::AnyExprId(any_expr_id) => match body.get_any(*any_expr_id) {
            AnyExprRef::Expr(expr) => match expr {
                Expr::Literal(literal) => Some(GuardLiteral::Literal(literal.clone())),
                Expr::Var(var) => Some(GuardLiteral::Var(*var)),
                _ => None,
            },
            AnyExprRef::Pat(pat) => match pat {
                Pat::Literal(literal) => Some(GuardLiteral::Literal(literal.clone())),
                Pat::Var(var) => Some(GuardLiteral::Var(*var)),
                _ => None,
            },
            AnyExprRef::TypeExpr(_) => None,
            AnyExprRef::Term(_) => None,
        },
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use elp_ide_db::RootDatabase;
    use elp_ide_db::elp_base_db::fixture::WithFixture;
    use expect_test::expect;
    use hir::Semantic;
    use hir::Strategy;
    use hir::fold::MacroStrategy;
    use hir::fold::ParenStrategy;

    use crate::match_pattern_in_file;
    use crate::match_pattern_in_file_functions;

    #[test]
    fn test_match_pattern_in_file() {
        let fixture = r#"fn() -> {foo, a}."#;

        let (db, file_id) = RootDatabase::with_single_file(fixture);
        let sema = Semantic::new(&db);

        let m = match_pattern_in_file(
            &sema,
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            file_id,
            "ssr: {foo, _@A}.",
        );
        expect![[r#"
            SsrMatches {
                matches: [
                    Match {
                        range: FileRange {
                            file_id: FileId(
                                0,
                            ),
                            range: 8..16,
                        },
                        matched_node_body: FormIdx {
                            file_id: FileId(
                                0,
                            ),
                            form_id: FunctionClause(
                                Idx::<FunctionClause>(0),
                            ),
                        },
                        matched_node: AnyExprId(
                            Expr(
                                Idx::<Expr>(3),
                            ),
                        ),
                        placeholder_values: {
                            AnyExprId(
                                Expr(
                                    Idx::<Expr>(1),
                                ),
                            ): PlaceholderMatch {
                                range: FileRange {
                                    file_id: FileId(
                                        0,
                                    ),
                                    range: 14..15,
                                },
                                code_id: AnyExprId(
                                    Expr(
                                        Idx::<Expr>(2),
                                    ),
                                ),
                                inner_matches: SsrMatches {
                                    matches: [],
                                },
                            },
                        },
                        placeholders_by_var: {
                            Var(
                                Id(3c00),
                            ): {
                                AnyExprId(
                                    Expr(
                                        Idx::<Expr>(1),
                                    ),
                                ),
                            },
                        },
                        rule_index: 0,
                        depth: 0,
                    },
                ],
            }
        "#]]
        .assert_debug_eq(&m);
    }

    #[test]
    fn test_match_pattern_in_file_functions() {
        // Note: currently we do not match types, so the spec will not
        // match anyway. But show that we DO still match.
        let fixture = r#"
               -spec fn() -> {foo, a}.
               fn() -> {foo, a}.
                "#;

        let (db, file_id) = RootDatabase::with_single_file(fixture);
        let sema = Semantic::new(&db);

        let m = match_pattern_in_file_functions(
            &sema,
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            file_id,
            "ssr: {foo, _@A}.",
        );
        expect![[r#"
            SsrMatches {
                matches: [
                    Match {
                        range: FileRange {
                            file_id: FileId(
                                0,
                            ),
                            range: 32..40,
                        },
                        matched_node_body: FormIdx {
                            file_id: FileId(
                                0,
                            ),
                            form_id: FunctionClause(
                                Idx::<FunctionClause>(0),
                            ),
                        },
                        matched_node: AnyExprId(
                            Expr(
                                Idx::<Expr>(3),
                            ),
                        ),
                        placeholder_values: {
                            AnyExprId(
                                Expr(
                                    Idx::<Expr>(1),
                                ),
                            ): PlaceholderMatch {
                                range: FileRange {
                                    file_id: FileId(
                                        0,
                                    ),
                                    range: 38..39,
                                },
                                code_id: AnyExprId(
                                    Expr(
                                        Idx::<Expr>(2),
                                    ),
                                ),
                                inner_matches: SsrMatches {
                                    matches: [],
                                },
                            },
                        },
                        placeholders_by_var: {
                            Var(
                                Id(3c00),
                            ): {
                                AnyExprId(
                                    Expr(
                                        Idx::<Expr>(1),
                                    ),
                                ),
                            },
                        },
                        rule_index: 0,
                        depth: 0,
                    },
                ],
            }
        "#]]
        .assert_debug_eq(&m);
    }

    #[test]
    fn test_match_source_text() {
        let fixture = r#"fn() -> {foo, a + 1}."#;

        let (db, file_id) = RootDatabase::with_single_file(fixture);
        let sema = Semantic::new(&db);

        let m = match_pattern_in_file(
            &sema,
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            file_id,
            "ssr: {foo, _@A}.",
        );
        expect![[r#"
            Some(
                "a + 1",
            )
        "#]]
        .assert_debug_eq(&m.matches[0].placeholder_text(&sema, "_@A"));

        expect![[r#"
            Some(
                14..19,
            )
        "#]]
        .assert_debug_eq(&m.matches[0].placeholder_range(&sema, "_@A"));
    }
}

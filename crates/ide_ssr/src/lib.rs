/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::elp_base_db::SourceDatabaseExt;
use elp_ide_db::RootDatabase;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::SyntaxNode;
use elp_syntax::TextRange;
use hir::db::DefDatabase;
use hir::db::InternDatabase;
use hir::AnyExprId;
use hir::Body;
use hir::InFile;
use hir::InSsr;
use hir::Semantic;
use hir::SsrBody;
use hir::SsrPatternIds;
use hir::SsrSource;

#[macro_use]
mod errors;

mod matching;
mod nester;
mod search;
#[cfg(test)]
mod tests;

pub use errors::SsrError;
pub use matching::Match;
pub use matching::MatchFailureReason;
pub use matching::SubId;

// ---------------------------------------------------------------------

pub fn match_pattern_in_file(sema: &Semantic, file_id: FileId, pattern: &str) -> SsrMatches {
    let pattern = SsrRule::parse_str(sema.db, pattern).expect("could not parse SSR pattern");
    let restrict_ranges = vec![];
    let mut match_finder = MatchFinder::in_context(&sema, file_id, restrict_ranges);
    match_finder.debug_print = false;
    match_finder.add_search_pattern(pattern);
    let matches: SsrMatches = match_finder.matches().flattened();
    matches
}

// ---------------------------------------------------------------------

// A structured search replace rule. Create by calling `parse` on a str.

#[derive(Debug)]
pub struct SsrRule {
    parsed_rule: Arc<SsrBody>,
}

impl SsrRule {
    #[allow(unused)] // Used in tests
    pub(crate) fn tree_print(&self, db: &dyn InternDatabase) -> String {
        self.parsed_rule.tree_print(db)
    }

    fn parse_ssr_source(db: &dyn DefDatabase, ssr_source: SsrSource) -> Result<SsrRule, SsrError> {
        if let Some((body, _)) = db.ssr_body_with_source(ssr_source) {
            Ok(SsrRule {
                parsed_rule: body.clone(),
            })
        } else {
            Err(SsrError("Could not lower rule".to_string()))
        }
    }

    pub fn parse_str(db: &dyn DefDatabase, pattern_str: &str) -> Result<SsrRule, SsrError> {
        let ssr_source = db.ssr(Arc::from(pattern_str));
        Self::parse_ssr_source(db, ssr_source)
    }
}

#[derive(Debug)]
pub(crate) struct SsrPattern {
    pub(crate) ssr_source: SsrSource,
    pub(crate) pattern_node: SsrPatternIds,
    pub(crate) index: usize,
}

impl SsrPattern {
    pub(crate) fn new(rule: Arc<SsrBody>, index: usize) -> SsrPattern {
        SsrPattern {
            ssr_source: rule.ssr_source,
            pattern_node: rule.pattern.clone(),
            index,
        }
    }

    pub(crate) fn get_body(&self, sema: &Semantic) -> Option<Arc<Body>> {
        let (body, _body_map) = sema.db.ssr_body_with_source(self.ssr_source)?;
        Some(body.body.clone())
    }

    /// The pattern is lowered both as a HIR Pat and an Expr.
    /// Choose which of these to use for matching the given `code` node.
    pub(crate) fn pattern_sub_id_for_code(&self, code: &SubId) -> SubId {
        match code {
            SubId::AnyExprId(AnyExprId::Expr(_)) => {
                SubId::AnyExprId(AnyExprId::Expr(self.pattern_node.expr.clone()))
            }
            SubId::AnyExprId(AnyExprId::Pat(_)) => {
                SubId::AnyExprId(AnyExprId::Pat(self.pattern_node.pat.clone()))
            }
            _ => SubId::Constant("not implemented yet".to_string()),
        }
    }
}

#[derive(Debug, Default)]
pub struct SsrMatches {
    pub matches: Vec<Match>,
}

/// Searches a file for pattern matches and possibly replaces them
/// with something else.
pub struct MatchFinder<'a> {
    /// Our source of information about the user's code.
    sema: &'a Semantic<'a>,
    rules: Vec<SsrPattern>,
    file_id: FileId,
    restrict_ranges: Vec<FileRange>,
    pub debug_print: bool,
}

impl<'a> MatchFinder<'a> {
    /// Constructs a new instance where names will be looked up as if
    /// they appeared at `lookup_context`.
    pub fn in_context(
        sema: &'a Semantic<'a>,
        file_id: FileId,
        mut restrict_ranges: Vec<FileRange>,
    ) -> MatchFinder<'a> {
        restrict_ranges.retain(|range| !range.range.is_empty());
        MatchFinder {
            sema,
            rules: Vec::new(),
            file_id,
            restrict_ranges,
            debug_print: false,
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
        self.rules
            .push(SsrPattern::new(rule.parsed_rule, self.rules.len()));
    }

    /// Returns matches for all added rules.
    pub fn matches(&self) -> SsrMatches {
        let mut matches: Vec<Match> = Vec::new();
        for rule in &self.rules {
            self.find_matches_for_rule(rule, &mut matches);
        }
        nester::nest_and_remove_collisions(matches, &self.sema)
    }

    /// Finds all nodes in `file_id` whose text is exactly equal to
    /// `snippet` and attempts to match them, while recording reasons
    /// why they don't match. This API is useful for command
    /// line-based debugging where providing a range is difficult.
    pub fn debug_where_text_equal(&self, file_id: FileId, snippet: &str) -> Vec<MatchDebugInfo> {
        let file = self.sema.parse(file_id);
        let mut res = Vec::new();
        let file_text = self.sema.db.file_text(file_id);
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
                            out.push(MatchDebugInfo {
                                matched: matching::get_match(
                                    true,
                                    rule,
                                    &body_origin,
                                    &any_expr_id,
                                    restrict_range,
                                    &self.sema,
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
        let file_text = db.file_text(self.range.file_id);
        file_text[self.range.range.start().into()..self.range.range.end().into()].to_string()
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

#[cfg(test)]
mod test {
    use elp_ide_db::elp_base_db::fixture::WithFixture;
    use elp_ide_db::RootDatabase;
    use expect_test::expect;
    use hir::Semantic;

    use crate::match_pattern_in_file;

    #[test]
    fn test_match_pattern_in_file() {
        let fixture = r#"fn() -> {foo, a}."#;

        let (db, file_id) = RootDatabase::with_single_file(fixture);
        let sema = Semantic::new(&db);

        let m = match_pattern_in_file(&sema, file_id, "ssr: {foo, _@A}.");
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
                            Var(
                                0,
                            ): PlaceholderMatch {
                                range: FileRange {
                                    file_id: FileId(
                                        0,
                                    ),
                                    range: 14..15,
                                },
                                node: AnyExprId(
                                    Expr(
                                        Idx::<Expr>(2),
                                    ),
                                ),
                                inner_matches: SsrMatches {
                                    matches: [],
                                },
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
}

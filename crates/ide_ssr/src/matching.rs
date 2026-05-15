/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! This module is responsible for matching a search pattern against a
//! node in the HIR AST. In the process of matching, placeholder
//! values are recorded.

use std::borrow::Cow;
use std::cell::Cell;
use std::iter;

use elp_ide_db::elp_base_db::FileRange;
use elp_syntax::TextRange;
use elp_syntax::ast;
use elp_syntax::ast::ArithOp;
use elp_syntax::ast::BinaryOp;
use elp_syntax::ast::CompOp;
use elp_syntax::ast::ListOp;
use elp_syntax::ast::LogicOp;
use elp_syntax::ast::Ordering;
use elp_syntax::ast::UnaryOp;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::AnyExprId;
use hir::AnyExprRef;
use hir::Atom;
use hir::BinarySeg;
use hir::Body;
use hir::BodyOrigin;
use hir::BodySourceMap;
use hir::CRClause;
use hir::CallTarget;
use hir::ComprehensionBuilder;
use hir::ComprehensionExpr;
use hir::Expr;
use hir::ExprId;
use hir::FoldBody;
use hir::IfClause;
use hir::InFileAstPtr;
use hir::Literal;
use hir::MacroCallName;
use hir::MapOp;
use hir::MaybeExpr;
use hir::Name;
use hir::NativeRecordName;
use hir::Pat;
use hir::PatId;
use hir::Semantic;
use hir::Strategy;
use hir::StringVariant;
use hir::Term;
use hir::TypeExpr;
use hir::Var;
use hir::fold::FoldCtx;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::ParentId;

use crate::Condition;
use crate::Conjunction;
use crate::NodeKind;
use crate::SsrMatches;
use crate::SsrPattern;
use crate::get_literal_subid;

/// An SSR placeholder detected from a Var with the `_@` prefix.
/// This type is local to the SSR crate since it's only meaningful in SSR context.
#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct SsrPlaceholder {
    pub var: Var,
    pub kind: PlaceholderKind,
}

/// Distinguishes single-element placeholders (`_@Name`) from glob
/// placeholders (`_@@Name`). Glob placeholders bind a contiguous slice
/// of sibling HIR nodes within a sequence (à la Erlang Merl).
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum PlaceholderKind {
    Single,
    Glob,
}

impl SsrPlaceholder {
    /// Create from a Var, if it's an SSR placeholder.
    pub fn from_var(var: Var) -> Option<Self> {
        if var.is_ssr_glob_placeholder() {
            Some(SsrPlaceholder {
                var,
                kind: PlaceholderKind::Glob,
            })
        } else if var.is_ssr_placeholder() {
            Some(SsrPlaceholder {
                var,
                kind: PlaceholderKind::Single,
            })
        } else {
            None
        }
    }

    pub fn is_glob(&self) -> bool {
        matches!(self.kind, PlaceholderKind::Glob)
    }
}

/// Pre-computed placeholder information for a pattern body.
/// This avoids repeated Salsa intern lookups (`is_ssr_placeholder(db)`)
/// during matching, which was a major source of RwLock contention
/// when many threads were matching patterns simultaneously.
pub struct PlaceholderCache {
    /// Maps each pattern AnyExprId that is a placeholder to its SsrPlaceholder.
    placeholders: FxHashMap<AnyExprId, SsrPlaceholder>,
}

impl PlaceholderCache {
    /// Build a placeholder cache by scanning all nodes in the pattern body.
    /// This does the Salsa intern lookups once upfront, so the matching
    /// hot path never needs to access the database for placeholder checks.
    pub fn build(pattern_body: &FoldBody) -> Self {
        let mut placeholders = FxHashMap::default();
        // Use FoldBody indexing (pattern_body[id]) instead of raw body iteration
        // so that InvisibleParens strategy is respected: paren ExprIds transparently
        // resolve to their inner expression, allowing paren-wrapped placeholders
        // to be found in the cache during matching.
        for (expr_id, _expr) in pattern_body.body.exprs.iter() {
            if let Expr::Var(var) = &pattern_body[expr_id]
                && let Some(placeholder) = SsrPlaceholder::from_var(*var)
            {
                placeholders.insert(AnyExprId::Expr(expr_id), placeholder);
            }
        }
        for (pat_id, _pat) in pattern_body.body.pats.iter() {
            if let Pat::Var(var) = &pattern_body[pat_id]
                && let Some(placeholder) = SsrPlaceholder::from_var(*var)
            {
                placeholders.insert(AnyExprId::Pat(pat_id), placeholder);
            }
        }
        for (type_id, _type_expr) in pattern_body.body.type_exprs.iter() {
            if let TypeExpr::Var(var) = &pattern_body[type_id]
                && let Some(placeholder) = SsrPlaceholder::from_var(*var)
            {
                placeholders.insert(AnyExprId::TypeExpr(type_id), placeholder);
            }
        }
        PlaceholderCache { placeholders }
    }

    /// Check if a pattern AnyExprId is a placeholder.
    fn is_placeholder(&self, id: &AnyExprId) -> bool {
        self.placeholders.contains_key(id)
    }

    /// Get the placeholder for a pattern AnyExprId, if it is one.
    fn get_placeholder(&self, id: &AnyExprId) -> Option<&SsrPlaceholder> {
        self.placeholders.get(id)
    }

    /// Returns true if the given pattern AnyExprId is a glob placeholder
    /// (i.e. a placeholder with the `_@@` prefix).
    pub(crate) fn is_glob(&self, id: &AnyExprId) -> bool {
        self.placeholders
            .get(id)
            .is_some_and(SsrPlaceholder::is_glob)
    }
}

// Creates a match error.
macro_rules! match_error {
    ($e:expr) => {{
            MatchFailed {
                reason: if recording_match_fail_reasons() {
                    Some(format!("{}", $e))
                } else {
                    None
                }
            }
    }};
    ($fmt:expr, $($arg:tt)+) => {{
        MatchFailed {
            reason: if recording_match_fail_reasons() {
                Some(format!($fmt, $($arg)+))
            } else {
                None
            }
        }
    }};
}

// Fails the current match attempt, recording the supplied reason if
// we're recording match fail reasons.
macro_rules! fail_match {
    ($($args:tt)*) => {return Err(match_error!($($args)*))};
}

/// Information about a match that was found.
#[derive(Debug, Clone)]
pub struct Match {
    pub range: FileRange,
    pub matched_node_body: BodyOrigin,
    pub matched_node: SubId,
    // A placeholder originates as a `SubId`, and carries a `hir::Var`.
    // The `SubId` is unique, but we need to ensure that each
    // `hir::Var` in a match corresponds to the `same` code fragment.
    pub placeholder_values: FxHashMap<SubId, PlaceholderMatch>,
    pub(crate) placeholders_by_var: FxHashMap<Var, FxHashSet<SubId>>,
    /// Which rule matched
    pub rule_index: usize,
    /// The depth of matched_node.
    pub depth: usize,
}

impl Match {
    pub fn range(&self) -> TextRange {
        self.range.range
    }

    /// Return any comments on the match
    pub fn comments(&self, sema: &Semantic) -> Option<Vec<ast::Comment>> {
        let (_body, body_map) = self.matched_node_body.get_body_and_map(sema)?;
        match self.matched_node {
            SubId::AnyExprId(any_expr_id) => sema.any_expr_comments(&body_map, any_expr_id),
            _ => None,
        }
    }

    /// Every binding for every placeholder, as `(name, source text, source
    /// range)` tuples. One entry per occurrence — repeated placeholders
    /// (e.g. `{_@A, _@A}`) yield multiple entries. Bindings whose source
    /// text cannot be resolved are skipped (text and range stay paired by
    /// originating from the same `PlaceholderMatch`, never zipped).
    pub fn placeholder_bindings<'a>(
        &'a self,
        sema: &'a Semantic,
    ) -> impl Iterator<Item = (Name, String, TextRange)> + 'a {
        let body = self.matched_node_body.get_body(sema);
        self.placeholders_by_var
            .iter()
            .flat_map(move |(var, sub_ids)| {
                let name = var.as_name();
                let body = body.clone();
                sub_ids.iter().filter_map(move |sub_id| {
                    let pm = self.placeholder_values.get(sub_id)?;
                    let body = body.as_ref()?;
                    let text = pm.text(sema, body)?;
                    Some((name.clone(), text, pm.range()))
                })
            })
    }
}

/// Information about a placeholder bound in a match.
///
/// `Single` corresponds to a `_@Name` placeholder bound to one HIR node.
/// `Glob` corresponds to a `_@@Name` glob placeholder bound to a (possibly
/// empty) contiguous slice of sibling HIR nodes within a sequence — added
/// in support of Erlang-Merl-style sequence matching. Only `Single` is
/// constructed today; `Glob` is introduced by later commits.
#[derive(Debug, Clone)]
pub enum PlaceholderMatch {
    Single {
        range: FileRange,
        /// The code node that matched the placeholder.
        code_id: SubId,
        /// More matches, found within `code_id`.
        inner_matches: SsrMatches,
    },
    Glob {
        /// Range spanning the matched slice. Empty range positioned at
        /// the insertion point when the glob matched zero elements.
        range: FileRange,
        /// The code nodes that matched the glob, in source order.
        code_ids: Vec<SubId>,
        /// More matches, found within the glob's bound elements.
        inner_matches: SsrMatches,
    },
}

impl PlaceholderMatch {
    pub(crate) fn single(range: FileRange, code_id: SubId) -> Self {
        Self::Single {
            range,
            code_id,
            inner_matches: SsrMatches::default(),
        }
    }

    /// The file range covered by this placeholder.
    pub fn file_range(&self) -> FileRange {
        match self {
            Self::Single { range, .. } | Self::Glob { range, .. } => *range,
        }
    }

    pub fn range(&self) -> TextRange {
        self.file_range().range
    }

    /// The single code node bound to this placeholder, if it is a
    /// `Single` placeholder. Returns `None` for `Glob` — use
    /// [`Self::code_ids`] there.
    pub fn code_id(&self) -> Option<&SubId> {
        match self {
            Self::Single { code_id, .. } => Some(code_id),
            Self::Glob { .. } => None,
        }
    }

    /// The slice of code nodes bound to this placeholder, if it is a
    /// `Glob` placeholder. Returns `None` for `Single`.
    pub fn code_ids(&self) -> Option<&[SubId]> {
        match self {
            Self::Single { .. } => None,
            Self::Glob { code_ids, .. } => Some(code_ids),
        }
    }

    /// Returns true if this placeholder is a glob (zero-or-more) match.
    pub fn is_glob(&self) -> bool {
        matches!(self, Self::Glob { .. })
    }

    pub fn inner_matches(&self) -> &SsrMatches {
        match self {
            Self::Single { inner_matches, .. } | Self::Glob { inner_matches, .. } => inner_matches,
        }
    }

    pub(crate) fn take_inner_matches(&mut self) -> SsrMatches {
        match self {
            Self::Single { inner_matches, .. } | Self::Glob { inner_matches, .. } => {
                std::mem::take(inner_matches)
            }
        }
    }

    pub fn text(&self, sema: &Semantic, body: &Body) -> Option<String> {
        let placeholder_match_id = self.code_id()?.any_expr_id()?;
        let placeholder_match_src: InFileAstPtr<ast::Expr> =
            body.get_body_map(sema)?.any(placeholder_match_id)?;

        Some(
            placeholder_match_src
                .to_node(&sema.parse(body.origin.file_id()))?
                .to_string(),
        )
    }

    pub fn is_string(&self, body: &Body) -> Option<StringVariant> {
        match self.code_id()? {
            SubId::AnyExprId(AnyExprId::Expr(expr_id)) => match &body[*expr_id] {
                Expr::Literal(Literal::String(s)) => Some(s.clone()),
                _ => None,
            },
            SubId::AnyExprId(AnyExprId::Pat(pat_id)) => match &body[*pat_id] {
                Pat::Literal(Literal::String(s)) => Some(s.clone()),
                _ => None,
            },
            SubId::AnyExprId(AnyExprId::TypeExpr(type_expr_id)) => match &body[*type_expr_id] {
                TypeExpr::Literal(Literal::String(s)) => Some(s.clone()),
                _ => None,
            },
            SubId::AnyExprId(AnyExprId::Term(term_id)) => match &body[*term_id] {
                Term::Literal(Literal::String(s)) => Some(s.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn is_atom(&self, body: &Body) -> Option<Atom> {
        match self.code_id()? {
            SubId::AnyExprId(AnyExprId::Expr(expr_id)) => match &body[*expr_id] {
                Expr::Literal(Literal::Atom(a)) => Some(*a),
                _ => None,
            },
            SubId::AnyExprId(AnyExprId::Pat(pat_id)) => match &body[*pat_id] {
                Pat::Literal(Literal::Atom(a)) => Some(*a),
                _ => None,
            },
            SubId::AnyExprId(AnyExprId::TypeExpr(type_expr_id)) => match &body[*type_expr_id] {
                TypeExpr::Literal(Literal::Atom(a)) => Some(*a),
                _ => None,
            },
            SubId::AnyExprId(AnyExprId::Term(term_id)) => match &body[*term_id] {
                Term::Literal(Literal::Atom(a)) => Some(*a),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn is_var(&self, body: &Body) -> Option<Var> {
        match self.code_id()? {
            SubId::AnyExprId(AnyExprId::Expr(expr_id)) => match &body[*expr_id] {
                Expr::Var(v) => Some(*v),
                _ => None,
            },
            SubId::AnyExprId(AnyExprId::Pat(pat_id)) => match &body[*pat_id] {
                Pat::Var(v) => Some(*v),
                _ => None,
            },
            SubId::AnyExprId(AnyExprId::TypeExpr(type_expr_id)) => match &body[*type_expr_id] {
                TypeExpr::Var(v) => Some(*v),
                _ => None,
            },
            _ => None,
        }
    }

    // N.B. that if the body was not indexed with macros included, you definitely will not find any!
    // As such, if you want to find macros you will need to make sure you used `MacroStrategy::DoNotExpand`
    // or `MacroStrategy::ExpandButIncludeMacroCall` when constructing the `Body` given as an argument here.
    pub fn is_macro(&self, body: &FoldBody) -> Option<bool> {
        // Glob matches bind a list of nodes, not a single macro; report `Some(false)`.
        let Some(code_id) = self.code_id() else {
            return Some(false);
        };
        match code_id {
            SubId::AnyExprId(AnyExprId::Expr(expr_id)) => match &body[*expr_id] {
                Expr::MacroCall { .. } => Some(true),
                _ => Some(false),
            },
            SubId::AnyExprId(AnyExprId::Pat(pat_id)) => match &body[*pat_id] {
                Pat::MacroCall { .. } => Some(true),
                _ => Some(false),
            },
            SubId::AnyExprId(AnyExprId::TypeExpr(type_expr_id)) => match &body[*type_expr_id] {
                TypeExpr::MacroCall { .. } => Some(true),
                _ => Some(false),
            },
            SubId::AnyExprId(AnyExprId::Term(term_id)) => match &body[*term_id] {
                Term::MacroCall { .. } => Some(true),
                _ => Some(false),
            },
            _ => Some(false),
        }
    }

    /// Return any comments on the matched item.
    pub fn comments(&self, sema: &Semantic, body_map: &BodySourceMap) -> Option<Vec<ast::Comment>> {
        match self.code_id()? {
            SubId::AnyExprId(any_expr_id) => sema.any_expr_comments(body_map, *any_expr_id),
            _ => None,
        }
    }

    // Check if our `code_id` and the `other` represent equivalent
    // code fragments. Glob equivalence (list-vs-list) is handled in a
    // later commit; today only `Single` is constructed.
    fn equivalent(
        &self,
        sema: &Semantic,
        rule: &SsrPattern,
        body: &FoldBody,
        other: &SubId,
    ) -> bool {
        let debug_print = false;
        let Some(SubId::AnyExprId(code_id)) = self.code_id() else {
            return false;
        };
        // The "pattern body" here is the code body itself (equivalence check).
        // Regular code bodies have no SSR placeholders, so the cache is empty.
        let placeholder_cache = PlaceholderCache::build(body);
        get_match(
            debug_print,
            rule,
            other,
            &body.body.origin,
            code_id,
            &None,
            sema,
            body,
            body,
            &placeholder_cache,
        )
        .is_ok()
    }
}

#[derive(Debug)]
pub struct MatchFailureReason {
    pub reason: String,
}

/// An "error" indicating that matching failed. Use the fail_match!
/// macro to create and return this.
#[derive(Clone, Debug)]
pub(crate) struct MatchFailed {
    /// The reason why we failed to match. Only present when
    /// debug_active true in call to `get_match`.
    pub(crate) reason: Option<String>,
}

pub(crate) fn record_match_fails_reasons_scope<F, T>(debug_active: bool, f: F) -> T
where
    F: Fn() -> T,
{
    RECORDING_MATCH_FAIL_REASONS.with(|c| c.set(debug_active));
    let res = f();
    RECORDING_MATCH_FAIL_REASONS.with(|c| c.set(false));
    res
}

// For performance reasons, we don't want to record the reason why
// every match fails, only the bit of code that the user indicated
// they thought would match. We use a thread local to indicate when we
// are trying to match that bit of code. This saves us having to pass
// a boolean into all the bits of code that can make the decision to
// not match.
thread_local! {
    pub static RECORDING_MATCH_FAIL_REASONS: Cell<bool> = const { Cell::new(false) };
}

fn recording_match_fail_reasons() -> bool {
    RECORDING_MATCH_FAIL_REASONS.with(|c| c.get())
}

/// Checks if `code` matches the search pattern found in
/// `search_scope`, returning information about the match, if it
/// does. Since we only do matching in this module and searching is
/// done by the parent module, we don't populate nested matches.
#[allow(clippy::too_many_arguments)]
pub(crate) fn get_match(
    debug_active: bool,
    rule: &SsrPattern,
    pattern: &SubId,
    code_body_origin: &BodyOrigin,
    code: &AnyExprId,
    restrict_range: &Option<FileRange>,
    sema: &Semantic,
    code_body: &FoldBody,
    pattern_body: &FoldBody,
    placeholder_cache: &PlaceholderCache,
) -> Result<Match, MatchFailed> {
    record_match_fails_reasons_scope(debug_active, || {
        Matcher::new(
            sema,
            rule,
            *restrict_range,
            pattern,
            code_body_origin,
            pattern_body,
            code_body,
            placeholder_cache,
        )
        .try_match(debug_active, &SubId::AnyExprId(*code))
    })
}

/// Which phase of matching we're currently performing. We do two
/// phases because most attempted matches will fail and it means we
/// can defer more expensive checks to the second phase.
#[derive(Debug)]
enum Phase<'a> {
    /// On the first phase, we perform cheap checks. No state is
    /// mutated and nothing is recorded.
    First,
    /// On the second phase, we construct the `Match`. Things like
    /// what placeholders bind to is recorded.
    Second(&'a mut Match),
}

/// Checks if our search pattern matches a particular node of the AST.
struct Matcher<'a> {
    sema: &'a Semantic<'a>,
    rule: &'a SsrPattern,
    pattern: &'a SubId,
    /// If any placeholders come from anywhere outside of this range,
    /// then the match will be rejected.
    restrict_range: Option<FileRange>,
    pattern_body: &'a FoldBody<'a>,
    code_body: &'a FoldBody<'a>,
    code_body_origin: &'a BodyOrigin,
    /// Pre-computed placeholder information for the pattern body.
    /// Avoids Salsa intern lookups in the matching hot path.
    placeholder_cache: &'a PlaceholderCache,
}

impl<'a> Matcher<'a> {
    #[allow(clippy::too_many_arguments)]
    fn new(
        sema: &'a Semantic<'a>,
        rule: &'a SsrPattern,
        restrict_range: Option<FileRange>,
        pattern: &'a SubId,
        code_body_origin: &'a BodyOrigin,
        pattern_body: &'a FoldBody<'a>,
        code_body: &'a FoldBody<'a>,
        placeholder_cache: &'a PlaceholderCache,
    ) -> Matcher<'a> {
        Matcher {
            sema,
            rule,
            restrict_range,
            pattern,
            pattern_body,
            code_body,
            code_body_origin,
            placeholder_cache,
        }
    }

    fn try_match(&self, debug_active: bool, code: &SubId) -> Result<Match, MatchFailed> {
        if debug_active && let SubId::AnyExprId(any_expr_id) = code {
            println!(
                "Matcher::try_match:code:---------------\n{}----------------\n",
                self.code_body.body.tree_print_any_expr(*any_expr_id)
            );
        }

        // First pass at matching, where we check that node types and idents match.
        self.attempt_match_node(&mut Phase::First, self.pattern, code)?;

        let code_range = self.get_code_range(code).expect("cannot get code range");
        self.validate_range(&code_range)?;
        let mut the_match = Match {
            range: code_range,
            matched_node_body: *self.code_body_origin,
            matched_node: code.clone(),
            placeholder_values: FxHashMap::default(),
            placeholders_by_var: FxHashMap::default(),
            rule_index: self.rule.index,
            depth: 0,
        };

        // Second matching pass, where we record placeholder matches,
        // ignored comments and maybe do any other more expensive
        // checks that we didn't want to do on the first pass.
        self.attempt_match_node(&mut Phase::Second(&mut the_match), self.pattern, code)?;

        // Evaluate the `when` clause now that every placeholder has a
        // binding. Must run after Phase::Second because OR (`;`)
        // semantics can only be decided once all conjuncts in *some*
        // alternative are knowable.
        self.check_when_disjunction(&the_match)?;

        Ok(the_match)
    }

    /// Evaluate the rule's `when` clause against the recorded bindings.
    /// The clause is in DNF: `Vec<Conjunction>` outer is `;` (OR),
    /// inner-map AND-groups per placeholder. Match succeeds if the rule
    /// has no `when` clause, or if at least one alternative passes all
    /// its conjuncts. Last failure reason is returned for debugging.
    fn check_when_disjunction(&self, the_match: &Match) -> Result<(), MatchFailed> {
        if self.rule.conditions.is_empty() {
            return Ok(());
        }
        let mut last_err: Option<MatchFailed> = None;
        for conjunction in &self.rule.conditions {
            match self.check_conjunction(the_match, conjunction) {
                Ok(()) => return Ok(()),
                Err(e) => last_err = Some(e),
            }
        }
        Err(last_err.unwrap_or_else(|| match_error!("when clause: no alternative matched")))
    }

    fn check_conjunction(
        &self,
        the_match: &Match,
        conjunction: &Conjunction,
    ) -> Result<(), MatchFailed> {
        for (placeholder, conditions) in conjunction {
            // Resolve all SubIds the pattern bound under this
            // placeholder's name. A placeholder used N times in the
            // pattern has N occurrences; equivalence among them was
            // already enforced during binding, so checking each is
            // redundant but cheap and keeps error messages precise.
            let Some(subids) = the_match.placeholders_by_var.get(&placeholder.var) else {
                // The condition refers to a placeholder name that
                // doesn't appear in the pattern. Match-time silence
                // mirrors the lowering convention used for `_@X == lit`
                // on a non-pattern placeholder — see `condition_from_expr`.
                continue;
            };
            for subid in subids {
                let Some(pm) = the_match.placeholder_values.get(subid) else {
                    continue;
                };
                // `when`-clause conditions target single-element
                // placeholders today; globs are rejected during pattern
                // compilation by a later commit, so this branch is
                // unreachable for well-formed rules.
                let Some(code_id) = pm.code_id() else {
                    continue;
                };
                for condition in conditions {
                    self.check_condition(code_id, condition)?;
                }
            }
        }
        Ok(())
    }

    /// Checks that `range` is within the permitted range if any. This
    /// is applicable when we're processing a macro expansion and we
    /// want to fail the match if we're working with a node that
    /// didn't originate from the location of the macro call.
    fn validate_range(&self, range: &FileRange) -> Result<(), MatchFailed> {
        if let Some(restrict_range) = &self.restrict_range
            && (restrict_range.file_id != range.file_id
                || !restrict_range.range.contains_range(range.range))
        {
            fail_match!("Node originated from a macro");
        }
        Ok(())
    }

    fn attempt_match_node(
        &self,
        phase: &mut Phase<'_>,
        pattern: &SubId,
        code: &SubId,
    ) -> Result<(), MatchFailed> {
        let code_node_type = self.get_code_str(code);
        let pattern_node_type = self.get_pattern_str(pattern);

        if self.attempt_match_placeholder(pattern, code, phase)? {
            // This means the pattern is for a placeholder, and the match succeeded.
            // If it is a placeholder and the match fails, the `?` above will
            // abort the matching with an appropriate `MatchFailed` result.
            return Ok(());
        }

        // Not matching a placeholder, compare trees
        if pattern_node_type != code_node_type {
            fail_match!(
                "Pattern had `{}`, code had `{}`",
                pattern_node_type,
                code_node_type,
            );
        }
        if self.is_code_leaf(code) {
            self.attempt_match_leaf(phase, pattern, code)
        } else {
            // The current node types are the same, descend into the children of each.
            self.attempt_match_node_children(phase, pattern, code)
        }
    }

    /// If the given `code` is a placeholder, attempt a match.
    /// The return values are a bit subtle
    /// - If the pattern is not a placeholder,
    ///   return Ok(false)
    /// - If the pattern is a placeholder, and the code is a placeholder,
    ///   return Ok(true) or Err(MatchFailed) depending on the condition
    fn attempt_match_placeholder(
        &self,
        pattern: &SubId,
        code: &SubId,
        phase: &mut Phase<'_>,
    ) -> Result<bool, MatchFailed> {
        if let Some(placeholder) = self.get_placeholder_for_node(pattern) {
            // Glob placeholders bind a contiguous slice of sibling nodes
            // and are only meaningful in list-walking
            // (`attempt_match_pattern_lists`). Reaching this point with a
            // glob means the surrounding context is not a sequence — fall
            // through so the normal variant comparison rejects the match.
            if placeholder.is_glob() {
                return Ok(false);
            }
            if let Phase::Second(matches_out) = phase {
                // `when`-clause conditions are evaluated after the whole
                // pattern binds, in `check_when_disjunction`, because OR
                // (`;`) semantics need every binding in hand before any
                // alternative can be ruled in.
                if let Some(original_range) = self.get_code_range(code) {
                    // We validated the range for the node
                    // when we started the match, so the
                    // placeholder probably can't fail range
                    // validation, but just to be safe...
                    self.validate_range(&original_range)?;

                    // If there is already a match for the same
                    // placeholder.var, fail if they are not compatible.
                    if let Some(match_ids) = matches_out.placeholders_by_var.get(&placeholder.var) {
                        for match_id in match_ids.iter() {
                            if let Some(m) = matches_out.placeholder_values.get(match_id)
                                && !m.equivalent(self.sema, self.rule, self.code_body, code)
                            {
                                fail_match!(
                                    "placeholder match failed: different occurrences do not match:\n---------------\n{:?}\n-----------",
                                    &m
                                );
                            }
                        }
                        // })
                    }

                    matches_out.placeholder_values.insert(
                        pattern.clone(),
                        PlaceholderMatch::single(original_range, code.clone()),
                    );
                    matches_out
                        .placeholders_by_var
                        .entry(placeholder.var)
                        .and_modify(|s| {
                            s.insert(pattern.clone());
                        })
                        .or_insert(FxHashSet::from_iter(vec![pattern.clone()]));
                }
            }
            return Ok(true);
        }
        Ok(false)
    }

    fn check_condition(&self, code: &SubId, condition: &Condition) -> Result<(), MatchFailed> {
        match condition {
            Condition::Literal(literal) => {
                if let Some(code_literal) = get_literal_subid(self.code_body, code) {
                    if &code_literal != literal {
                        fail_match!("literal match condition failed: literals different");
                    }
                } else {
                    fail_match!("literal match condition failed: placeholder not a literal");
                }
            }
            Condition::Not(condition) => {
                if self.check_condition(code, condition).is_ok() {
                    fail_match!("condition matched when it was expected not to");
                }
            }
            Condition::Kind(expected) => {
                let actual = node_kind(self.code_body, code);
                if actual != Some(*expected) {
                    fail_match!(
                        "kind constraint failed: expected {:?}, got {:?}",
                        expected,
                        actual
                    );
                }
            }
        }
        Ok(())
    }

    fn attempt_match_node_children(
        &self,
        phase: &mut Phase<'_>,
        pattern: &SubId,
        code: &SubId,
    ) -> Result<(), MatchFailed> {
        let pi = self.pattern_iterator(pattern);
        let ci = self.code_iterator(code);
        match (pi, ci) {
            (PatternIterator::List(pl), PatternIterator::List(cl)) => {
                self.attempt_match_pattern_lists(phase, pl, cl)
            }
            (PatternIterator::Map(pm), PatternIterator::Map(cm)) => {
                self.attempt_match_pattern_maps(phase, pm, cm)
            }
            _ => fail_match!("Expecting two PatternMaps or two PatternLists"),
        }
    }

    /// Walk the elements of each list matching each one.  Succeed if
    /// they both end at the same time having matched all elements.
    fn attempt_match_pattern_lists(
        &self,
        phase: &mut Phase<'_>,
        pattern_it: PatternList,
        mut code_it: PatternList,
    ) -> Result<(), MatchFailed> {
        let mut recursion_limit = 100;
        let mut pattern_it = pattern_it.peekable();
        loop {
            recursion_limit -= 1;
            if recursion_limit <= 0 {
                fail_match!("Internal error: attempt_match_pattern_lists recursion limit");
            }
            match code_it.next() {
                None => {
                    if let Some(p) = pattern_it.next() {
                        fail_match!("Part of the pattern was unmatched: {:?}", p);
                    }
                    return Ok(());
                }
                Some(c) => match pattern_it.next() {
                    Some(p) => {
                        if self.is_code_leaf(&c) {
                            self.attempt_match_leaf(phase, &p, &c)?;
                        } else {
                            self.attempt_match_node(phase, &p, &c)?;
                        }
                    }
                    None => {
                        fail_match!("Pattern reached end, code has {:?}", &c)
                    }
                },
            }
        }
    }

    fn attempt_match_pattern_maps(
        &self,
        phase: &mut Phase<'_>,
        pattern_it: PatternMap,
        code_it: PatternMap,
    ) -> Result<(), MatchFailed> {
        self.attempt_match_pattern_lists(phase, pattern_it.prefix, code_it.prefix)?;
        // We have a map of fields that can map in any order.  Each is
        // a key, with possible values

        // Some of the keys might be placeholders.  So first see what
        // non-placeholders we can match.

        // Note: Perhaps preprocess the pattern map when constructing
        // it, to avoid rework on every match.  But make it work
        // first.

        // What do we do if we have more than one possible match?
        // e.g. two placeholder field names, with placeholder RHS?
        // Pathological, ignore for now.
        #[allow(clippy::type_complexity)]
        let (placeholders, non_placeholders): (
            Vec<(&SubId, &Vec<SubId>)>,
            Vec<(&SubId, &Vec<SubId>)>,
        ) = pattern_it
            .children
            .iter()
            .partition(|(k, _v)| self.is_placeholder_expr(k));

        // We are likely to have best results matching in order
        // (heuristic).  So pulling something out of the set is not
        // ideal, but we need to be able to loop through, attempting,
        // and then remove if matched. Optimize later.
        let mut code_keys: Vec<(&SubId, &Vec<SubId>)> = code_it.children.iter().collect();
        // Note: this is potentially n^2 in the number of fields
        for p in non_placeholders {
            if let Ok(index) = self.attempt_match_vec(phase, &p, &code_keys) {
                code_keys.remove(index);
            } else {
                fail_match!("no non-placeholder match in child vector");
            };
        }
        // We have dealt with the non-placeholders, which give an explicit match.
        // Now we assign the placeholders in the order they appear,
        // with the fields in the order they appear.
        for p in placeholders {
            if let Ok(index) = self.attempt_match_vec(phase, &p, &code_keys) {
                code_keys.remove(index);
            } else {
                fail_match!("no placeholder match in child vector");
            };
        }
        if !code_keys.is_empty() {
            fail_match!("unmatched fields");
        }
        Ok(())
    }

    fn attempt_match_vec(
        &self,
        phase: &mut Phase<'_>,
        pattern: &(&SubId, &Vec<SubId>),
        codes: &[(&SubId, &Vec<SubId>)],
    ) -> Result<usize, MatchFailed> {
        if let Some((idx, rhs)) = codes.iter().enumerate().find_map(|(idx, (code, rhs))| {
            if (if self.is_code_leaf(code) {
                self.attempt_match_leaf(phase, pattern.0, code)
            } else {
                self.attempt_match_node(phase, pattern.0, code)
            })
            .is_ok()
            {
                Some((idx, *rhs))
            } else {
                None
            }
        }) {
            // Now match the RHS too
            self.attempt_match_pattern_lists(phase, pattern.1.clone().into(), rhs.clone().into())?;
            Ok(idx)
        } else {
            fail_match!("nope");
        }
    }

    fn attempt_match_leaf(
        &self,
        phase: &mut Phase<'_>,
        pattern: &SubId,
        code: &SubId,
    ) -> Result<(), MatchFailed> {
        let code_expr = &code.sub_id_ref(self.code_body);
        let code_node_type = code.variant_str(self.code_body);
        let pattern_expr = pattern.sub_id_ref(self.pattern_body);
        let pattern_node_type = pattern.variant_str(self.pattern_body);

        if self.attempt_match_placeholder(pattern, code, phase)? {
            return Ok(());
        }
        // Check literals
        match (pattern_expr, code_expr) {
            (
                SubIdRef::AnyExprRef(AnyExprRef::Expr(Expr::Literal(pat_lit))),
                SubIdRef::AnyExprRef(AnyExprRef::Expr(Expr::Literal(code_lit))),
            ) => {
                // Compare Literal enums directly — they derive PartialEq,
                // and for Literal::Atom this compares interned IDs without
                // Salsa lookups. Only render to strings for error messages.
                if pat_lit == code_lit {
                    return Ok(());
                } else {
                    fail_match!(
                        "Pattern had `{}`, code had `{}`",
                        render_str(pat_lit),
                        render_str(code_lit)
                    );
                }
            }
            (
                SubIdRef::AnyExprRef(AnyExprRef::Pat(Pat::Literal(pat_lit))),
                SubIdRef::AnyExprRef(AnyExprRef::Pat(Pat::Literal(code_lit))),
            ) => {
                if pat_lit == code_lit {
                    return Ok(());
                } else {
                    fail_match!(
                        "Pattern had `{}`, code had `{}`",
                        render_str(pat_lit),
                        render_str(code_lit)
                    );
                }
            }
            (
                SubIdRef::AnyExprRef(AnyExprRef::Expr(Expr::Var(code_var))),
                SubIdRef::AnyExprRef(AnyExprRef::Expr(Expr::Var(pat_var))),
            ) => {
                self.attempt_match_var(code_var, pat_var)?;
            }
            (
                SubIdRef::AnyExprRef(AnyExprRef::Pat(Pat::Var(code_var))),
                SubIdRef::AnyExprRef(AnyExprRef::Pat(Pat::Var(pat_var))),
            ) => {
                self.attempt_match_var(code_var, pat_var)?;
            }
            (SubIdRef::Atom(pat_atom), SubIdRef::Atom(code_atom)) => {
                // Compare interned IDs directly — both are from the same
                // Salsa database, so equal IDs mean equal names. This avoids
                // two Salsa intern lookups per atom comparison.
                if pat_atom == *code_atom {
                    return Ok(());
                } else {
                    fail_match!(
                        "Pattern had atom `{}`, code had atom `{}`",
                        pat_atom.as_name(),
                        code_atom.as_name(),
                    );
                }
            }
            (SubIdRef::Var(pat_var), SubIdRef::Var(code_var)) => {
                self.attempt_match_var(code_var, &pat_var)?;
            }
            // UnaryOp and BinaryOp can be fully distinguished by variant_str.
            _ => {}
        }
        if pattern_node_type != code_node_type {
            fail_match!(
                "Pattern had `{}`, code had `{}`",
                pattern_node_type,
                code_node_type,
            );
        } else {
            Ok(())
        }
    }

    fn attempt_match_var(&self, code_var: &Var, pat_var: &Var) -> Result<(), MatchFailed> {
        // Compare interned IDs directly — avoids Salsa intern lookups.
        if pat_var == code_var {
            Ok(())
        } else {
            fail_match!(
                "Pattern had var `{}`, code had var `{}`",
                pat_var.as_name(),
                code_var.as_name(),
            );
        }
    }

    fn get_placeholder_for_node(&self, id: &SubId) -> Option<&SsrPlaceholder> {
        match id {
            SubId::AnyExprId(any_expr) => self.placeholder_cache.get_placeholder(any_expr),
            _ => None,
        }
    }

    /// A leaf node has no children.
    fn is_code_leaf(&self, c: &SubId) -> bool {
        match c {
            SubId::AnyExprId(c) => {
                PatternIterator::new_any_expr(&self.code_body.get_any(*c)).is_empty()
            }
            _ => true,
        }
    }

    fn get_code_range(&self, code: &SubId) -> Option<FileRange> {
        match code {
            SubId::AnyExprId(code) => self.code_body.body.range_for_any(self.sema, *code),
            _ => None,
        }
    }

    fn pattern_iterator(&self, pattern: &SubId) -> PatternIterator {
        match pattern {
            SubId::AnyExprId(pattern) => {
                PatternIterator::new_any_expr(&self.pattern_body.get_any(*pattern))
            }
            _ => PatternIterator::default(),
        }
    }

    fn code_iterator(&self, code: &SubId) -> PatternIterator {
        match code {
            SubId::AnyExprId(code) => PatternIterator::new_any_expr(&self.code_body.get_any(*code)),
            _ => PatternIterator::default(),
        }
    }

    fn get_code_str(&self, id: &SubId) -> Cow<'static, str> {
        id.variant_str(self.code_body)
    }

    fn get_pattern_str(&self, id: &SubId) -> Cow<'static, str> {
        id.variant_str(self.pattern_body)
    }

    fn is_placeholder_expr(&self, id: &SubId) -> bool {
        match id {
            SubId::AnyExprId(any_expr_id) => match any_expr_id {
                AnyExprId::Expr(_) | AnyExprId::Pat(_) => {
                    self.placeholder_cache.is_placeholder(any_expr_id)
                }
                _ => false,
            },
            _ => false,
        }
    }
}

/// Resolve a bound HIR node to its `NodeKind`. Position-agnostic: a tuple
/// is `Tuple` whether it appears in expression or pattern position.
/// Returns `None` for HIR variants outside the constraint vocabulary.
pub(crate) fn node_kind(body: &FoldBody, code: &SubId) -> Option<NodeKind> {
    let SubId::AnyExprId(any_expr_id) = code else {
        return None;
    };
    match body.get_any(*any_expr_id) {
        AnyExprRef::Expr(expr) => match expr {
            Expr::Literal(lit) => literal_kind(lit),
            Expr::Var(_) => Some(NodeKind::Var),
            Expr::Tuple { .. } => Some(NodeKind::Tuple),
            Expr::List { .. } => Some(NodeKind::List),
            Expr::Map { .. } => Some(NodeKind::Map),
            Expr::Binary { .. } => Some(NodeKind::Binary),
            Expr::Call { .. } => Some(NodeKind::Call),
            Expr::Closure { .. } | Expr::CaptureFun { .. } => Some(NodeKind::Fun),
            _ => None,
        },
        AnyExprRef::Pat(pat) => match pat {
            Pat::Literal(lit) => literal_kind(lit),
            Pat::Var(_) => Some(NodeKind::Var),
            Pat::Tuple { .. } => Some(NodeKind::Tuple),
            Pat::List { .. } => Some(NodeKind::List),
            Pat::Map { .. } => Some(NodeKind::Map),
            Pat::Binary { .. } => Some(NodeKind::Binary),
            _ => None,
        },
        AnyExprRef::TypeExpr(_) | AnyExprRef::Term(_) => None,
    }
}

fn literal_kind(lit: &Literal) -> Option<NodeKind> {
    match lit {
        Literal::Atom(_) => Some(NodeKind::Atom),
        Literal::Integer(_) => Some(NodeKind::Integer),
        _ => None,
    }
}

fn native_record_name_to_subids(name: &NativeRecordName) -> Vec<SubId> {
    match name {
        NativeRecordName::Anon => vec![SubId::Constant("#_".to_string())],
        NativeRecordName::Qualified { module, name } => {
            vec![(*module).into(), (*name).into()]
        }
    }
}

fn render_str(lit: &Literal) -> String {
    match lit {
        Literal::String(s) => s.as_string(),
        Literal::Char(c) => format!("{c}"),
        Literal::Atom(a) => a.as_string(),
        Literal::Integer(i) => format!("{}", i.value),
        Literal::Float(f) => format!("{f}"),
    }
}

/// We need to be able to walk the HIR AST, and match what is in it,
/// comparing a template and the current code.  The HIR AST is not
/// uniform, however, and some nodes in it may not bottom out in
/// something with an AnyExprId, but we still need to check that the
/// nodes match.
/// So we introduce the `SubId` enum to enumerate the possible things
/// in the HIR AST that we can match.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SubId {
    AnyExprId(AnyExprId),
    Atom(Atom),
    Var(Var),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    MapOp(MapOp),
    // Used to mark specific syntax such as a `|` in a list, to
    // clearly separate the optional tail
    Constant(String),
}

impl SubId {
    pub fn any_expr_id(&self) -> Option<AnyExprId> {
        match self {
            SubId::AnyExprId(any_expr_id) => Some(*any_expr_id),
            _ => None,
        }
    }

    pub fn variant_str(&self, body: &FoldBody) -> Cow<'static, str> {
        match self {
            SubId::AnyExprId(e) => Cow::Borrowed(body.get_any(*e).variant_str()),
            SubId::Atom(_) => Cow::Borrowed("Atom"),
            SubId::Var(_) => Cow::Borrowed("Var"),
            SubId::UnaryOp(op) => Cow::Borrowed(match op {
                UnaryOp::Plus => "UnaryOp::Plus",
                UnaryOp::Minus => "UnaryOp::Minus",
                UnaryOp::Bnot => "UnaryOp::Bnot",
                UnaryOp::Not => "UnaryOp::Not",
            }),
            SubId::BinaryOp(op) => Cow::Borrowed(match op {
                BinaryOp::LogicOp(op) => match op {
                    LogicOp::And { lazy } => {
                        if *lazy {
                            "LogicOp::And{true}"
                        } else {
                            "LogicOp::And{false}"
                        }
                    }
                    LogicOp::Or { lazy } => {
                        if *lazy {
                            "LogicOp::Or{true}"
                        } else {
                            "LogicOp::Or{false}"
                        }
                    }
                    LogicOp::Xor => "LogicOp::Xor",
                },
                BinaryOp::ArithOp(op) => match op {
                    ArithOp::Add => "ArithOp::Add",
                    ArithOp::Mul => "ArithOp::Mul",
                    ArithOp::Sub => "ArithOp::Sub",
                    ArithOp::FloatDiv => "ArithOp::FloatDiv",
                    ArithOp::Div => "ArithOp::Div",
                    ArithOp::Rem => "ArithOp::Rem",
                    ArithOp::Band => "ArithOp::Band",
                    ArithOp::Bor => "ArithOp::Bor",
                    ArithOp::Bxor => "ArithOp::Bxor",
                    ArithOp::Bsr => "ArithOp::Bsr",
                    ArithOp::Bsl => "ArithOp::Bsl",
                },
                BinaryOp::ListOp(op) => match op {
                    ListOp::Append => "ListOp::Append",
                    ListOp::Subtract => "ListOp::Subtract",
                },
                BinaryOp::CompOp(op) => match op {
                    CompOp::Eq {
                        strict: true,
                        negated: true,
                    } => "CompOp::Eq{true,true}",
                    CompOp::Eq {
                        strict: true,
                        negated: false,
                    } => "CompOp::Eq{true,false}",
                    CompOp::Eq {
                        strict: false,
                        negated: true,
                    } => "CompOp::Eq{false,true}",
                    CompOp::Eq {
                        strict: false,
                        negated: false,
                    } => "CompOp::Eq{false,false}",
                    CompOp::Ord {
                        ordering: Ordering::Greater,
                        strict: true,
                    } => "CompOp::Ord{>,true}",
                    CompOp::Ord {
                        ordering: Ordering::Greater,
                        strict: false,
                    } => "CompOp::Ord{>,false}",
                    CompOp::Ord {
                        ordering: Ordering::Less,
                        strict: true,
                    } => "CompOp::Ord{<,true}",
                    CompOp::Ord {
                        ordering: Ordering::Less,
                        strict: false,
                    } => "CompOp::Ord{<,false}",
                },
                BinaryOp::Send => "BinaryOp::Send",
            }),
            SubId::MapOp(op) => Cow::Borrowed(match op {
                MapOp::Assoc => "MapOp::Assoc",
                MapOp::Exact => "MapOp::Exact",
            }),
            SubId::Constant(v) => Cow::Owned(v.clone()),
        }
    }

    pub fn sub_id_ref<'a>(&'a self, body: &'a FoldBody) -> SubIdRef<'a> {
        match self {
            SubId::AnyExprId(code) => SubIdRef::AnyExprRef(body.get_any(*code)),
            SubId::Atom(a) => SubIdRef::Atom(*a),
            SubId::Var(a) => SubIdRef::Var(*a),
            SubId::UnaryOp(op) => SubIdRef::UnaryOp(*op),
            SubId::BinaryOp(op) => SubIdRef::BinaryOp(*op),
            SubId::MapOp(op) => SubIdRef::MapOp(*op),
            SubId::Constant(v) => SubIdRef::Constant(v.clone()),
        }
    }
}

impl From<PatId> for SubId {
    fn from(value: PatId) -> Self {
        SubId::AnyExprId(value.into())
    }
}

impl From<ExprId> for SubId {
    fn from(value: ExprId) -> Self {
        SubId::AnyExprId(value.into())
    }
}

impl From<Atom> for SubId {
    fn from(value: Atom) -> Self {
        SubId::Atom(value)
    }
}

impl From<Var> for SubId {
    fn from(value: Var) -> Self {
        SubId::Var(value)
    }
}

impl From<&str> for SubId {
    fn from(value: &str) -> Self {
        SubId::Constant(value.to_string())
    }
}

impl From<UnaryOp> for SubId {
    fn from(value: UnaryOp) -> Self {
        SubId::UnaryOp(value)
    }
}

impl From<BinaryOp> for SubId {
    fn from(value: BinaryOp) -> Self {
        SubId::BinaryOp(value)
    }
}

impl From<MapOp> for SubId {
    fn from(value: MapOp) -> Self {
        SubId::MapOp(value)
    }
}

impl From<&MacroCallName> for SubId {
    fn from(value: &MacroCallName) -> Self {
        match value {
            MacroCallName::Var(var) => SubId::Var(*var),
            MacroCallName::Atom(atom) => SubId::Atom(*atom),
            MacroCallName::Missing => "missing".into(),
        }
    }
}

impl From<&bool> for SubId {
    fn from(value: &bool) -> Self {
        if *value {
            SubId::Constant("true".to_string())
        } else {
            SubId::Constant("false".to_string())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SubIdRef<'a> {
    AnyExprRef(AnyExprRef<'a>),
    Atom(Atom),
    Var(Var),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    MapOp(MapOp),
    Constant(String),
}

#[derive(Debug, Clone)]
#[derive(Default)]
pub enum PatternIterator {
    List(PatternList),
    // For records and fields, we record the name and value separately
    Map(PatternMap),
    #[default]
    Leaf,
}

#[derive(Debug, Clone, Default)]
pub struct PatternList {
    children: Vec<SubId>,
    idx: usize,
}

#[derive(Debug, Clone, Default)]
pub struct PatternMap {
    prefix: PatternList,
    children: FxHashMap<SubId, Vec<SubId>>,
}

impl From<Vec<SubId>> for PatternList {
    fn from(children: Vec<SubId>) -> Self {
        PatternList { children, idx: 0 }
    }
}

impl Iterator for PatternList {
    type Item = SubId;

    fn next(&mut self) -> Option<Self::Item> {
        let r = self.children.get(self.idx);
        self.idx += 1;
        r.cloned()
    }
}

impl PatternIterator {
    pub fn new_any_expr(parent: &AnyExprRef) -> PatternIterator {
        match parent {
            AnyExprRef::Expr(it) => match it {
                Expr::Missing => PatternIterator::Leaf,
                Expr::Literal(_) => PatternIterator::Leaf,
                Expr::Var(var) => PatternIterator::as_pattern_list(vec![(*var).into()]),
                Expr::Match { lhs, rhs } => {
                    PatternIterator::as_pattern_list(vec![(*lhs).into(), (*rhs).into()])
                }
                Expr::Tuple { exprs } => {
                    PatternIterator::as_pattern_list(exprs.iter().map(|id| (*id).into()).collect())
                }
                Expr::List { exprs, tail } => PatternIterator::as_pattern_list(
                    exprs
                        .iter()
                        .map(|id| (*id).into())
                        .chain(iter::once("|".into()))
                        .chain(tail.iter().map(|id| (*id).into()))
                        .collect(),
                ),
                Expr::Binary { segs } => PatternIterator::as_pattern_list(
                    segs.iter().flat_map(iterate_binary_seg).collect(),
                ),
                Expr::UnaryOp { expr, op } => {
                    PatternIterator::as_pattern_list(vec![(*op).into(), (*expr).into()])
                }
                Expr::BinaryOp { lhs, rhs, op } => PatternIterator::as_pattern_list(vec![
                    (*op).into(),
                    (*lhs).into(),
                    (*rhs).into(),
                ]),
                Expr::Record { name, fields } => {
                    let children: FxHashMap<SubId, Vec<SubId>> = fields
                        .iter()
                        .map(|(name, val)| ((*name).into(), vec![(*val).into()]))
                        .collect();
                    PatternIterator::as_pattern_map(vec![(*name).into()], children)
                }
                Expr::RecordUpdate { expr, name, fields } => PatternIterator::as_pattern_list(
                    vec![(*name).into(), (*expr).into()]
                        .into_iter()
                        .chain(
                            fields
                                .iter()
                                .flat_map(|(name, val)| vec![(*name).into(), (*val).into()]),
                        )
                        .collect(),
                ),
                Expr::RecordIndex { name, field } => {
                    PatternIterator::as_pattern_list(vec![(*name).into(), (*field).into()])
                }
                Expr::RecordField { expr, name, field } => PatternIterator::as_pattern_list(vec![
                    (*name).into(),
                    (*field).into(),
                    (*expr).into(),
                ]),
                Expr::NativeRecord { name, fields } => {
                    let children: FxHashMap<SubId, Vec<SubId>> = fields
                        .iter()
                        .map(|(name, val)| ((*name).into(), vec![(*val).into()]))
                        .collect();
                    PatternIterator::as_pattern_map(native_record_name_to_subids(name), children)
                }
                Expr::NativeRecordUpdate { expr, name, fields } => {
                    let mut prefix = native_record_name_to_subids(name);
                    prefix.push((*expr).into());
                    PatternIterator::as_pattern_list(
                        prefix
                            .into_iter()
                            .chain(
                                fields
                                    .iter()
                                    .flat_map(|(name, val)| vec![(*name).into(), (*val).into()]),
                            )
                            .collect(),
                    )
                }
                Expr::NativeRecordField { expr, name, field } => {
                    let mut prefix = native_record_name_to_subids(name);
                    prefix.push((*field).into());
                    prefix.push((*expr).into());
                    PatternIterator::as_pattern_list(prefix)
                }
                Expr::Map { fields } => {
                    let children: FxHashMap<SubId, Vec<SubId>> = fields
                        .iter()
                        .map(|(name, val)| ((*name).into(), vec![(*val).into()]))
                        .collect();
                    PatternIterator::as_pattern_map(vec![], children)
                }
                Expr::MapUpdate { expr, fields } => {
                    let children: FxHashMap<SubId, Vec<SubId>> = fields
                        .iter()
                        .map(|(name, op, val)| ((*name).into(), vec![(*op).into(), (*val).into()]))
                        .collect();
                    PatternIterator::as_pattern_map(vec![(*expr).into()], children)
                }
                Expr::Catch { expr } => PatternIterator::as_pattern_list(vec![(*expr).into()]),
                Expr::MacroCall {
                    expansion: _,
                    args,
                    macro_def: _,
                    macro_name,
                } => PatternIterator::as_pattern_list(
                    iter::once(macro_name.into())
                        .chain(args.iter().map(|id| (*id).into()))
                        .collect(),
                ),
                Expr::Call { target, args } => PatternIterator::as_pattern_list({
                    let mut res = Vec::default();
                    match target {
                        CallTarget::Local { name } => res.push((*name).into()),
                        CallTarget::Remote { module, name, .. } => {
                            res.push((*module).into());
                            res.push((*name).into());
                        }
                    }
                    args.iter().for_each(|arg| res.push((*arg).into()));
                    res
                }),
                Expr::Comprehension { builder, exprs } => {
                    let bs: Vec<SubId> = match builder {
                        ComprehensionBuilder::List(exprs) => {
                            exprs.iter().map(|e| (*e).into()).collect()
                        }
                        ComprehensionBuilder::Binary(e) => vec![(*e).into()],
                        ComprehensionBuilder::Map(fields) => fields
                            .iter()
                            .flat_map(|(k, v)| [(*k).into(), (*v).into()])
                            .collect(),
                    };
                    PatternIterator::as_pattern_list(
                        bs.into_iter()
                            .chain(iter::once("||".into()))
                            .chain(
                                exprs
                                    .iter()
                                    .flat_map(PatternIterator::for_comprehension_expr),
                            )
                            .collect(),
                    )
                }
                Expr::Block { exprs } => {
                    PatternIterator::as_pattern_list(exprs.iter().map(|id| (*id).into()).collect())
                }
                Expr::If { clauses } => PatternIterator::as_pattern_list(
                    clauses.iter().flat_map(if_clause_iter).collect(),
                ),
                Expr::Case { expr, clauses } => PatternIterator::as_pattern_list(
                    iter::once((*expr).into())
                        .chain(clauses.iter().flat_map(cr_clause_iter))
                        .collect(),
                ),
                Expr::Receive { clauses, after } => PatternIterator::as_pattern_list(
                    clauses
                        .iter()
                        .flat_map(cr_clause_iter)
                        .chain(iter::once("after".into()))
                        .chain(after.iter().flat_map(|a| {
                            iter::once(a.timeout.into()).chain(a.exprs.iter().map(|e| (*e).into()))
                        }))
                        .collect(),
                ),
                Expr::Try {
                    exprs,
                    of_clauses,
                    catch_clauses,
                    after,
                } => PatternIterator::as_pattern_list({
                    let mut res = Vec::default();
                    exprs.iter().for_each(|e| res.push((*e).into()));
                    res.push("of".into());
                    of_clauses
                        .iter()
                        .flat_map(cr_clause_iter)
                        .for_each(|e| res.push(e));
                    res.push("catch".into());
                    catch_clauses.iter().for_each(|cc| {
                        if let Some(p) = cc.class {
                            res.push(p.into())
                        }
                        res.push(cc.reason.into());
                        if let Some(p) = cc.stack {
                            res.push(p.into())
                        }
                        cc.guards
                            .iter()
                            .for_each(|g| g.iter().for_each(|e| res.push((*e).into())));
                        cc.exprs.iter().for_each(|e| res.push((*e).into()));
                    });
                    res.push("after".into());
                    after.iter().for_each(|e| res.push((*e).into()));
                    res
                }),
                Expr::CaptureFun { target, arity } => PatternIterator::as_pattern_list({
                    let mut res = Vec::default();
                    match target {
                        CallTarget::Local { name } => res.push((*name).into()),
                        CallTarget::Remote { module, name, .. } => {
                            res.push((*module).into());
                            res.push((*name).into());
                        }
                    }
                    res.push((*arity).into());
                    res
                }),
                Expr::Closure { clauses, name } => PatternIterator::as_pattern_list(
                    name.iter()
                        .map(|n| (*n).into())
                        .chain(iter::once("clauses".into()))
                        .chain(clauses.iter().flat_map(|clause| {
                            iter::once("pats".into()).chain(
                                clause
                                    .pats
                                    .iter()
                                    .map(|p| (*p).into())
                                    .chain(iter::once("guards".into()))
                                    .chain(
                                        clause
                                            .guards
                                            .iter()
                                            .flat_map(|guard| guard.iter().map(|g| (*g).into())),
                                    )
                                    .chain(iter::once("exprs".into()))
                                    .chain(clause.exprs.iter().map(|e| (*e).into())),
                            )
                        }))
                        .collect(),
                ),
                Expr::Maybe {
                    exprs,
                    else_clauses,
                } => PatternIterator::as_pattern_list(
                    exprs
                        .iter()
                        .flat_map(|maybe_expr| match maybe_expr {
                            MaybeExpr::Cond { lhs, rhs } => vec![(*lhs).into(), (*rhs).into()],
                            MaybeExpr::Expr(expr) => vec![(*expr).into()],
                        })
                        .chain(else_clauses.iter().flat_map(cr_clause_iter))
                        .collect(),
                ),
                Expr::Paren { expr } => PatternIterator::as_pattern_list(vec![(*expr).into()]),
            },
            AnyExprRef::Pat(it) => match it {
                Pat::Missing => PatternIterator::Leaf,
                Pat::Literal(_) => PatternIterator::Leaf,
                Pat::Var(var) => PatternIterator::as_pattern_list(vec![(*var).into()]),
                Pat::Match { lhs, rhs } => {
                    PatternIterator::as_pattern_list(vec![(*lhs).into(), (*rhs).into()])
                }
                Pat::Tuple { pats } => {
                    PatternIterator::as_pattern_list(pats.iter().map(|p| (*p).into()).collect())
                }
                Pat::List { pats, tail } => PatternIterator::as_pattern_list(
                    pats.iter()
                        .chain(tail.iter())
                        .map(|id| (*id).into())
                        .collect(),
                ),
                Pat::Binary { segs } => PatternIterator::as_pattern_list(
                    segs.iter().flat_map(iterate_binary_seg).collect(),
                ),
                Pat::UnaryOp { pat, op } => {
                    PatternIterator::as_pattern_list(vec![(*op).into(), (*pat).into()])
                }
                Pat::BinaryOp { lhs, rhs, op } => PatternIterator::as_pattern_list(vec![
                    (*op).into(),
                    (*lhs).into(),
                    (*rhs).into(),
                ]), // match op first to fail fast
                Pat::Record { name, fields } => {
                    let children: FxHashMap<SubId, Vec<SubId>> = fields
                        .iter()
                        .map(|(name, val)| ((*name).into(), vec![(*val).into()]))
                        .collect();
                    PatternIterator::as_pattern_map(vec![(*name).into()], children)
                }
                Pat::RecordIndex { name, field } => {
                    PatternIterator::as_pattern_list(vec![(*name).into(), (*field).into()])
                }
                Pat::NativeRecord { name, fields } => {
                    let children: FxHashMap<SubId, Vec<SubId>> = fields
                        .iter()
                        .map(|(name, val)| ((*name).into(), vec![(*val).into()]))
                        .collect();
                    PatternIterator::as_pattern_map(native_record_name_to_subids(name), children)
                }
                Pat::Map { fields } => {
                    let children: FxHashMap<SubId, Vec<SubId>> = fields
                        .iter()
                        .map(|(name, val)| ((*name).into(), vec![(*val).into()]))
                        .collect();
                    PatternIterator::as_pattern_map(vec![], children)
                }
                Pat::MacroCall {
                    expansion: _,
                    args,
                    macro_def: _,
                    macro_name,
                } => PatternIterator::as_pattern_list(
                    iter::once(macro_name.into())
                        .chain(args.iter().map(|id| (*id).into()))
                        .collect(),
                ),
                Pat::Paren { pat } => PatternIterator::as_pattern_list(vec![(*pat).into()]),
            },
            AnyExprRef::TypeExpr(_) => {
                panic!("PatternIterator not implemented for TypeExpr")
            }
            AnyExprRef::Term(_) => {
                panic!("PatternIterator not implemented for Term")
            }
        }
    }

    fn as_pattern_list(children: Vec<SubId>) -> PatternIterator {
        PatternIterator::List(PatternList { children, idx: 0 })
    }

    fn as_pattern_map(
        prefix: Vec<SubId>,
        children: FxHashMap<SubId, Vec<SubId>>,
    ) -> PatternIterator {
        PatternIterator::Map(PatternMap {
            prefix: PatternList {
                children: prefix,
                idx: 0,
            },
            children,
        })
    }

    pub fn is_empty(&self) -> bool {
        match self {
            PatternIterator::Leaf => true,
            _ => false,
        }
    }

    fn for_comprehension_expr(cb: &ComprehensionExpr) -> Vec<SubId> {
        match cb {
            ComprehensionExpr::BinGenerator { pat, expr, strict } => {
                vec!["CBB".into(), strict.into(), (*pat).into(), (*expr).into()]
            }
            ComprehensionExpr::ListGenerator { pat, expr, strict } => {
                vec!["CBL".into(), strict.into(), (*pat).into(), (*expr).into()]
            }
            ComprehensionExpr::MapGenerator {
                key,
                value,
                expr,
                strict,
            } => {
                vec![
                    "CBM".into(),
                    strict.into(),
                    (*key).into(),
                    (*value).into(),
                    (*expr).into(),
                ]
            }
            ComprehensionExpr::Expr(expr) => vec![(*expr).into()],
            ComprehensionExpr::Zip(exprs) => exprs
                .iter()
                .flat_map(|id| {
                    iter::once("ZIP".into()).chain(PatternIterator::for_comprehension_expr(id))
                })
                .collect(),
        }
    }
}

fn if_clause_iter(ifc: &IfClause) -> Vec<SubId> {
    ifc.guards
        .iter()
        .flat_map(|g| g.iter().map(|e| (*e).into()))
        .chain(iter::once("exprs".into()))
        .chain(ifc.exprs.iter().map(|e| (*e).into()))
        .collect()
}

fn cr_clause_iter(cr: &CRClause) -> Vec<SubId> {
    iter::once(cr.pat.into())
        .chain(iter::once("guards".into()))
        .chain(cr.guards.iter().flat_map(|g| g.iter().map(|e| (*e).into())))
        .chain(iter::once("exprs".into()))
        .chain(cr.exprs.iter().map(|e| (*e).into()))
        .collect()
}

fn iterate_binary_seg<Id>(s: &BinarySeg<Id>) -> Vec<SubId>
where
    SubId: From<Id>,
    Id: Copy,
{
    iter::once(s.elem.into())
        .chain(s.size.into_iter().map(|id| id.into()))
        .collect()
}

/// Validate the usage of glob placeholders (`_@@Name`) in an SSR rule.
///
/// Globs are only allowed as direct elements of a "sequence position":
/// `Tuple.exprs`, `List.exprs`, `Block.exprs`, and `Call.args` (and the
/// corresponding `Pat` variants). At most one glob per sequence is
/// permitted, mirroring Erlang Merl. Globs are forbidden inside `when`
/// clauses entirely. Any violation surfaces as an `SsrError` so the
/// rule fails to parse rather than silently producing wrong matches.
pub(crate) fn validate_glob_usage(
    ssr_body: &hir::SsrBody,
    pattern_body: &FoldBody,
    cache: &PlaceholderCache,
) -> Result<(), crate::SsrError> {
    let strategy = Strategy {
        macros: MacroStrategy::Expand,
        parens: ParenStrategy::InvisibleParens,
    };
    let body = pattern_body.body;
    let pattern = &ssr_body.pattern;

    // Walk the pattern's expr and pat trees, collecting each glob with
    // its immediate parent. `ctx.parents` ends with the current item, so
    // the parent is the second-to-last entry (when present).
    let mut globs: Vec<(AnyExprId, Option<AnyExprId>)> = Vec::new();
    let mut collect = |ctx: &hir::fold::AnyCallBackCtx| {
        if cache.is_glob(&ctx.item_id) {
            let parent_id = if ctx.parents.len() >= 2 {
                match ctx.parents[ctx.parents.len() - 2] {
                    ParentId::HirIdx(idx) => Some(idx.idx),
                    _ => None,
                }
            } else {
                None
            };
            globs.push((ctx.item_id, parent_id));
        }
    };
    FoldCtx::fold_expr(strategy, body, pattern.expr, (), &mut |(), ctx| {
        collect(&ctx);
    });
    FoldCtx::fold_pat(strategy, body, pattern.pat, (), &mut |(), ctx| {
        collect(&ctx);
    });

    // Validate placements and tally per-parent glob counts.
    let mut seq_glob_count: FxHashMap<AnyExprId, usize> = FxHashMap::default();
    for (glob_id, parent_id) in &globs {
        let Some(parent_id) = parent_id else {
            return Err(crate::SsrError::new(
                "glob placeholder must appear inside a sequence \
                 (tuple, list, block, or call args), not at the top level"
                    .to_string(),
            ));
        };
        match glob_role(body, *parent_id, *glob_id) {
            GlobRole::Sequence => {
                *seq_glob_count.entry(*parent_id).or_insert(0) += 1;
            }
            GlobRole::Forbidden => {
                return Err(crate::SsrError::new(
                    "glob placeholder not allowed in this position; \
                     globs are only allowed in tuple/list/block elements \
                     and call arguments"
                        .to_string(),
                ));
            }
        }
    }
    if let Some((_, count)) = seq_glob_count.iter().find(|(_, c)| **c > 1) {
        return Err(crate::SsrError::new(format!(
            "at most one glob placeholder allowed per sequence (found {count})"
        )));
    }

    // Reject any glob anywhere in `where` clauses.
    if let Some(when) = &ssr_body.when {
        for conjunction in when {
            for guard_expr_id in conjunction {
                let found =
                    FoldCtx::fold_expr(strategy, body, *guard_expr_id, false, &mut |acc, ctx| {
                        acc || cache.is_glob(&ctx.item_id)
                    });
                if found {
                    return Err(crate::SsrError::new(
                        "glob placeholder not allowed in `where` clause".to_string(),
                    ));
                }
            }
        }
    }

    Ok(())
}

/// Role a child plays in its parent HIR node, from the perspective of
/// glob-placement validation.
enum GlobRole {
    /// Direct element of a sequence position (tuple/list/block/call args).
    Sequence,
    /// Any other slot — globs are forbidden here.
    Forbidden,
}

fn glob_role(body: &Body, parent_id: AnyExprId, glob_id: AnyExprId) -> GlobRole {
    match (parent_id, glob_id) {
        (AnyExprId::Expr(p), AnyExprId::Expr(g)) => match &body.exprs[p] {
            Expr::Tuple { exprs } if exprs.contains(&g) => GlobRole::Sequence,
            Expr::List { exprs, .. } if exprs.contains(&g) => GlobRole::Sequence,
            Expr::Block { exprs } if exprs.contains(&g) => GlobRole::Sequence,
            Expr::Call { args, .. } if args.contains(&g) => GlobRole::Sequence,
            _ => GlobRole::Forbidden,
        },
        (AnyExprId::Pat(p), AnyExprId::Pat(g)) => match &body.pats[p] {
            Pat::Tuple { pats } if pats.contains(&g) => GlobRole::Sequence,
            Pat::List { pats, .. } if pats.contains(&g) => GlobRole::Sequence,
            _ => GlobRole::Forbidden,
        },
        _ => GlobRole::Forbidden,
    }
}

#[cfg(test)]
mod tests {
    use elp_ide_db::elp_base_db::FileId;
    use elp_ide_db::elp_base_db::FileRange;
    use elp_syntax::TextRange;

    use super::*;

    fn dummy_range() -> FileRange {
        FileRange {
            file_id: FileId::from_raw(0u32),
            range: TextRange::new(0.into(), 1.into()),
        }
    }

    #[test]
    fn placeholder_match_single_accessors() {
        let code_id = SubId::Constant("a".to_string());
        let m = PlaceholderMatch::single(dummy_range(), code_id.clone());

        assert!(!m.is_glob());
        assert_eq!(m.code_id(), Some(&code_id));
        assert_eq!(m.code_ids(), None);
        assert_eq!(m.file_range(), dummy_range());
        assert_eq!(m.range(), dummy_range().range);
        assert_eq!(m.inner_matches().matches.len(), 0);
    }

    #[test]
    fn placeholder_match_glob_accessors() {
        // Pre-construct a Glob to lock the enum surface, even though
        // the matching engine does not produce Glob bindings yet.
        let code_ids = vec![
            SubId::Constant("a".to_string()),
            SubId::Constant("b".to_string()),
        ];
        let m = PlaceholderMatch::Glob {
            range: dummy_range(),
            code_ids: code_ids.clone(),
            inner_matches: SsrMatches::default(),
        };

        assert!(m.is_glob());
        assert_eq!(m.code_id(), None);
        assert_eq!(m.code_ids(), Some(code_ids.as_slice()));
        assert_eq!(m.file_range(), dummy_range());
    }

    #[test]
    fn placeholder_match_take_inner_matches() {
        let mut m = PlaceholderMatch::single(dummy_range(), SubId::Constant("a".to_string()));
        let inner = m.take_inner_matches();
        assert_eq!(inner.matches.len(), 0);
        // Re-takeable, returns a fresh empty SsrMatches.
        assert_eq!(m.take_inner_matches().matches.len(), 0);
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This module is responsible for matching a search pattern against a
//! node in the HIR AST. In the process of matching, placeholder
//! values are recorded.

use std::cell::Cell;
use std::iter;

use elp_ide_db::elp_base_db::FileRange;
use elp_syntax::ast;
use elp_syntax::ast::ArithOp;
use elp_syntax::ast::BinaryOp;
use elp_syntax::ast::CompOp;
use elp_syntax::ast::ListOp;
use elp_syntax::ast::LogicOp;
use elp_syntax::ast::Ordering;
use elp_syntax::ast::UnaryOp;
use elp_syntax::TextRange;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::AnyExprId;
use hir::AnyExprRef;
use hir::Atom;
use hir::BinarySeg;
use hir::Body;
use hir::BodyOrigin;
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
use hir::Pat;
use hir::PatId;
use hir::Semantic;
use hir::SsrPlaceholder;
use hir::TypeExpr;
use hir::Var;

use crate::get_literal_subid;
use crate::Condition;
use crate::SsrMatches;
use crate::SsrPattern;

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
}

/// Information about a placeholder bound in a match.
#[derive(Debug, Clone)]
pub struct PlaceholderMatch {
    pub range: FileRange,
    /// The code node that matched the placeholder
    pub code_id: SubId,
    /// More matches, found within `node`.
    pub inner_matches: SsrMatches,
}

impl PlaceholderMatch {
    fn new(range: FileRange, code_id: SubId) -> Self {
        Self {
            range,
            code_id,
            inner_matches: SsrMatches::default(),
        }
    }

    pub fn range(&self) -> TextRange {
        self.range.range
    }

    pub fn text(&self, sema: &Semantic, body: &Body) -> Option<String> {
        let placeholder_match_id = self.code_id.any_expr_id()?;
        let placeholder_match_src: InFileAstPtr<ast::Expr> =
            body.get_body_map(sema)?.any(placeholder_match_id)?;

        Some(
            placeholder_match_src
                .to_node(&sema.parse(body.origin.file_id()))?
                .to_string(),
        )
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
    pub static RECORDING_MATCH_FAIL_REASONS: Cell<bool> = Cell::new(false);
}

fn recording_match_fail_reasons() -> bool {
    RECORDING_MATCH_FAIL_REASONS.with(|c| c.get())
}

/// Checks if `code` matches the search pattern found in
/// `search_scope`, returning information about the match, if it
/// does. Since we only do matching in this module and searching is
/// done by the parent module, we don't populate nested matches.
pub(crate) fn get_match(
    debug_active: bool,
    rule: &SsrPattern,
    code_body_origin: &BodyOrigin,
    code: &AnyExprId,
    restrict_range: &Option<FileRange>,
    sema: &Semantic,
    code_body: &FoldBody,
    pattern_body: &FoldBody,
) -> Result<Match, MatchFailed> {
    record_match_fails_reasons_scope(debug_active, || {
        Matcher::new(
            sema,
            *restrict_range,
            rule,
            code_body_origin,
            &pattern_body,
            &code_body,
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
    /// If any placeholders come from anywhere outside of this range,
    /// then the match will be rejected.
    restrict_range: Option<FileRange>,
    pattern_body: &'a FoldBody<'a>,
    code_body: &'a FoldBody<'a>,
    code_body_origin: &'a BodyOrigin,
}

impl<'a> Matcher<'a> {
    fn new(
        sema: &'a Semantic<'a>,
        restrict_range: Option<FileRange>,
        pattern: &'a SsrPattern,
        code_body_origin: &'a BodyOrigin,
        pattern_body: &'a FoldBody<'a>,
        code_body: &'a FoldBody<'a>,
    ) -> Matcher<'a> {
        Matcher {
            sema,
            rule: pattern,
            restrict_range,
            pattern_body,
            code_body,
            code_body_origin,
        }
    }

    fn try_match(&self, debug_active: bool, code: &SubId) -> Result<Match, MatchFailed> {
        if debug_active {
            match code {
                SubId::AnyExprId(any_expr_id) => {
                    println!(
                        "Matcher::try_match:code:---------------\n{}----------------\n",
                        self.code_body
                            .body
                            .tree_print_any_expr(self.sema.db.upcast(), *any_expr_id)
                    );
                }
                _ => {}
            }
        }

        // First pass at matching, where we check that node types and idents match.
        self.attempt_match_node(
            &mut Phase::First,
            &self.rule.pattern_sub_id_for_code(code),
            code,
        )?;

        let range = self.get_code_range(code).expect("cannot get code range");
        let code_range = FileRange {
            file_id: self.code_body.body.origin.file_id(),
            range,
        };
        self.validate_range(&code_range)?;
        let mut the_match = Match {
            range: code_range,
            matched_node_body: self.code_body_origin.clone(),
            matched_node: code.clone(),
            placeholder_values: FxHashMap::default(),
            placeholders_by_var: FxHashMap::default(),
            rule_index: self.rule.index,
            depth: 0,
        };

        // Second matching pass, where we record placeholder matches,
        // ignored comments and maybe do any other more expensive
        // checks that we didn't want to do on the first pass.
        self.attempt_match_node(
            &mut Phase::Second(&mut the_match),
            &self.rule.pattern_sub_id_for_code(code),
            code,
        )?;

        Ok(the_match)
    }

    /// Checks that `range` is within the permitted range if any. This
    /// is applicable when we're processing a macro expansion and we
    /// want to fail the match if we're working with a node that
    /// didn't originate from the location of the macro call.
    fn validate_range(&self, range: &FileRange) -> Result<(), MatchFailed> {
        if let Some(restrict_range) = &self.restrict_range {
            if restrict_range.file_id != range.file_id
                || !restrict_range.range.contains_range(range.range)
            {
                fail_match!("Node originated from a macro");
            }
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
        // The current node types are the same, descend into the children of each.
        self.attempt_match_node_children(phase, pattern, code)
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
        if self.is_placeholder(pattern) {
            if let Some(placeholder) = self.get_placeholder_for_node(pattern) {
                if let Phase::Second(matches_out) = phase {
                    // check constraints
                    if let Some(condition) = self.rule.conditions.get(placeholder) {
                        self.check_condition(code, condition)?;
                    }
                    if let Some(range) = self.get_code_range(code) {
                        let file_id = self.code_body.body.origin.file_id();
                        let original_range = FileRange { file_id, range };
                        // We validated the range for the node
                        // when we started the match, so the
                        // placeholder probably can't fail range
                        // validation, but just to be safe...
                        self.validate_range(&original_range)?;
                        matches_out.placeholder_values.insert(
                            pattern.clone(),
                            PlaceholderMatch::new(original_range, code.clone()),
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
        }
        return Ok(false);
    }

    fn check_condition(&self, code: &SubId, condition: &Condition) -> Result<(), MatchFailed> {
        match condition {
            Condition::Literal(literal) => {
                if let Some(code_literal) = get_literal_subid(&self.code_body, code) {
                    if code_literal != literal {
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

        let non_placeholders: Vec<(&SubId, &Vec<SubId>)> = pattern_it
            .children
            .iter()
            .filter(|(k, _v)| !self.is_placeholder(k))
            .collect();

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
                fail_match!("no match in child vector");
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
            if let Ok(_) = if self.is_code_leaf(code) {
                self.attempt_match_leaf(phase, pattern.0, code)
            } else {
                self.attempt_match_node(phase, pattern.0, code)
            } {
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
        let code_expr = &code.sub_id_ref(&self.code_body);
        let code_node_type = code.variant_str(&self.code_body);
        let pattern_expr = pattern.sub_id_ref(&self.pattern_body);
        let pattern_node_type = pattern.variant_str(&self.pattern_body);

        if self.attempt_match_placeholder(pattern, code, phase)? {
            return Ok(());
        }
        // Check literals
        match (pattern_expr, code_expr) {
            (
                SubIdRef::AnyExprRef(AnyExprRef::Expr(Expr::Literal(pat_lit))),
                SubIdRef::AnyExprRef(AnyExprRef::Expr(Expr::Literal(code_lit))),
            ) => {
                let pat_lit_str = render_str(self.sema, pat_lit);
                let code_lit_str = render_str(self.sema, code_lit);
                if pat_lit_str == code_lit_str {
                    return Ok(());
                } else {
                    fail_match!(
                        "Pattern had `{}`, code had `{}`",
                        &pat_lit_str,
                        &code_lit_str
                    );
                }
            }
            (SubIdRef::Atom(pat_atom), SubIdRef::Atom(code_atom)) => {
                if self.sema.db.lookup_atom(pat_atom) == self.sema.db.lookup_atom(*code_atom) {
                    return Ok(());
                } else {
                    fail_match!(
                        "Pattern had atom `{}`, code had atom `{}`",
                        self.sema.db.lookup_atom(pat_atom),
                        self.sema.db.lookup_atom(*code_atom),
                    );
                }
            }
            (SubIdRef::Var(pat_var), SubIdRef::Var(code_var)) => {
                if self.sema.db.lookup_var(pat_var) == self.sema.db.lookup_var(*code_var) {
                    return Ok(());
                } else {
                    fail_match!(
                        "Pattern had var `{}`, code had var `{}`",
                        self.sema.db.lookup_var(pat_var),
                        self.sema.db.lookup_var(*code_var),
                    );
                }
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

    fn get_placeholder_for_node(&self, id: &SubId) -> Option<&SsrPlaceholder> {
        match id {
            SubId::AnyExprId(any_expr) => self.get_placeholder(any_expr),
            _ => None,
        }
    }

    fn get_placeholder(&self, pattern_expr: &AnyExprId) -> Option<&SsrPlaceholder> {
        match self.pattern_body.get_any(*pattern_expr) {
            AnyExprRef::Expr(expr) => match expr {
                Expr::SsrPlaceholder(placeholder) => Some(placeholder),
                _ => None,
            },
            AnyExprRef::Pat(pat) => match pat {
                Pat::SsrPlaceholder(placeholder) => Some(placeholder),
                _ => None,
            },
            AnyExprRef::TypeExpr(type_expr) => match type_expr {
                TypeExpr::SsrPlaceholder(placeholder) => Some(placeholder),
                _ => None,
            },
            AnyExprRef::Term(_) => None,
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

    fn get_code_range(&self, code: &SubId) -> Option<TextRange> {
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

    fn get_code_str(&self, id: &SubId) -> String {
        id.variant_str(&self.code_body).to_string()
    }

    fn get_pattern_str(&self, id: &SubId) -> String {
        id.variant_str(&self.pattern_body).to_string()
    }

    /// Check if the pattern_body SubId is a placeholder
    fn is_placeholder(&self, id: &SubId) -> bool {
        let pattern_str = self.get_pattern_str(id);

        pattern_str == "Pat::SsrPlaceholder"
            || pattern_str == "Expr::SsrPlaceholder"
            || pattern_str == "TypeExpr::SsrPlaceholder"
            || pattern_str == "Term::SsrPlaceholder"
    }
}

fn render_str(sema: &Semantic, lit: &Literal) -> String {
    match lit {
        Literal::String(s) => s.as_string(),
        Literal::Char(c) => format!("{c}"),
        Literal::Atom(a) => a.as_string(sema.db.upcast()),
        Literal::Integer(i) => format!("{i}"),
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

    pub fn variant_str<'a>(&'a self, body: &'a FoldBody) -> &'a str {
        match self {
            SubId::AnyExprId(e) => body.get_any(*e).variant_str(),
            SubId::Atom(_) => "Atom",
            SubId::Var(_) => "Var",
            SubId::UnaryOp(op) => match op {
                UnaryOp::Plus => "UnaryOp::Plus",
                UnaryOp::Minus => "UnaryOp::Minus",
                UnaryOp::Bnot => "UnaryOp::Bnot",
                UnaryOp::Not => "UnaryOp::Not",
            },
            SubId::BinaryOp(op) => match op {
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
            },
            SubId::MapOp(op) => match op {
                MapOp::Assoc => "MapOp::Assoc",
                MapOp::Exact => "MapOp::Exact",
            },
            SubId::Constant(v) => v,
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
pub enum PatternIterator {
    List(PatternList),
    // For records and fields, we record the name and value separately
    Map(PatternMap),
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

impl Default for PatternIterator {
    fn default() -> Self {
        PatternIterator::List(PatternList::default())
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
                Expr::Missing => PatternIterator::as_pattern_list(vec![]),
                Expr::Literal(_) => PatternIterator::as_pattern_list(vec![]),
                Expr::Var(_) => PatternIterator::as_pattern_list(vec![]),
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
                    segs.iter().flat_map(|s| iterate_binary_seg(s)).collect(),
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
                        ComprehensionBuilder::List(e) => vec![(*e).into()],
                        ComprehensionBuilder::Binary(e) => vec![(*e).into()],
                        ComprehensionBuilder::Map(k, v) => vec![(*k).into(), (*v).into()],
                    };
                    PatternIterator::as_pattern_list(
                        bs.into_iter()
                            .chain(exprs.iter().flat_map(|cb| match cb {
                                ComprehensionExpr::BinGenerator { pat, expr } => {
                                    vec!["CBB".into(), (*pat).into(), (*expr).into()]
                                }
                                ComprehensionExpr::ListGenerator { pat, expr } => {
                                    vec!["CBL".into(), (*pat).into(), (*expr).into()]
                                }
                                ComprehensionExpr::MapGenerator { key, value, expr } => {
                                    vec![
                                        "CBM".into(),
                                        (*key).into(),
                                        (*value).into(),
                                        (*expr).into(),
                                    ]
                                }
                                ComprehensionExpr::Expr(expr) => vec![(*expr).into()],
                            }))
                            .collect(),
                    )
                }
                Expr::Block { exprs } => {
                    PatternIterator::as_pattern_list(exprs.iter().map(|id| (*id).into()).collect())
                }
                Expr::If { clauses } => PatternIterator::as_pattern_list(
                    clauses.iter().flat_map(|cr| if_clause_iter(cr)).collect(),
                ),
                Expr::Case { expr, clauses } => PatternIterator::as_pattern_list(
                    iter::once((*expr).into())
                        .chain(clauses.iter().flat_map(|cr| cr_clause_iter(cr)))
                        .collect(),
                ),
                Expr::Receive { clauses, after } => PatternIterator::as_pattern_list(
                    clauses
                        .iter()
                        .flat_map(|cr| cr_clause_iter(cr))
                        .chain(iter::once("after".into()))
                        .chain(
                            after
                                .iter()
                                .flat_map(|a| a.exprs.iter().map(|e| (*e).into())),
                        )
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
                        .flat_map(|cr| cr_clause_iter(cr))
                        .for_each(|e| res.push(e));
                    res.push("catch".into());
                    catch_clauses.iter().for_each(|cc| {
                        cc.class.map(|p| res.push(p.into()));
                        res.push(cc.reason.into());
                        cc.stack.map(|p| res.push(p.into()));
                        cc.guards
                            .iter()
                            .for_each(|g| g.iter().for_each(|e| res.push((*e).into())));
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
                        .chain(else_clauses.iter().flat_map(|cr| cr_clause_iter(cr)))
                        .collect(),
                ),
                Expr::Paren { expr } => PatternIterator::as_pattern_list(vec![(*expr).into()]),
                Expr::SsrPlaceholder(_) => PatternIterator::as_pattern_list(vec![]),
            },
            AnyExprRef::Pat(it) => match &*it {
                Pat::Missing => PatternIterator::as_pattern_list(vec![]),
                Pat::Literal(_) => PatternIterator::as_pattern_list(vec![]),
                Pat::Var(_) => PatternIterator::as_pattern_list(vec![]),
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
                    segs.iter().flat_map(|s| iterate_binary_seg(s)).collect(),
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
                Pat::SsrPlaceholder(_) => PatternIterator::as_pattern_list(vec![]),
            },
            AnyExprRef::TypeExpr(_) => todo!(),
            AnyExprRef::Term(_) => todo!(),
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
            PatternIterator::List(l) => l.children.is_empty(),
            PatternIterator::Map(m) => m.prefix.children.is_empty() && m.children.is_empty(),
        }
    }
}

fn if_clause_iter(ifc: &IfClause) -> Vec<SubId> {
    ifc.guards
        .iter()
        .flat_map(|g| g.into_iter().map(|e| (*e).into()))
        .chain(iter::once("exprs".into()))
        .chain(ifc.exprs.iter().map(|e| (*e).into()))
        .collect()
}

fn cr_clause_iter(cr: &CRClause) -> Vec<SubId> {
    iter::once(cr.pat.into())
        .chain(iter::once("guards".into()))
        .chain(
            cr.guards
                .iter()
                .flat_map(|g| g.into_iter().map(|e| (*e).into())),
        )
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

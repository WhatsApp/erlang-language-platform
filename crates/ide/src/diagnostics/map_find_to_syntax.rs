/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: map_find_to_syntax
//!
//! Offer to rewrite basic `maps:find/2` calls whose result is immediately
//! inspected to the equivalent map query syntax, which avoids allocating an
//! intermediate `{ok, V}` tuple. Two shapes are handled.
//!
//! The `case` shape:
//!
//! ```ignore
//! case maps:find(K, M) of
//!     {ok, V} -> A;   %% Unnecessary allocation of result tuple
//!     error -> B
//! end
//! ```
//!
//! becomes `case M of #{K := V} -> A; #{} -> B end`, and the match shape:
//!
//! ```ignore
//! {ok, V} = maps:find(K, M)
//! ```
//!
//! becomes `#{K := V} = M`.
//!
//! This is a bespoke HIR matcher (rather than a set of fixed SSR patterns),
//! allowing it to handle multi-statement clause bodies, an arbitrary number
//! of `{ok, _}` clauses, and any value pattern (record/map/nested), etc.

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_db::text_edit::TextRange;
use elp_syntax::AstNode;
use elp_syntax::NodeOrToken;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxToken;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::CRClause;
use hir::CallTarget;
use hir::Expr;
use hir::ExprId;
use hir::FunctionDef;
use hir::InFunctionClauseBody;
use hir::Literal;
use hir::Name;
use hir::Pat;
use hir::PatId;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::known;

use crate::Assist;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::fix;

pub(crate) struct MapFindToSyntaxLinter;

impl Linter for MapFindToSyntaxLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::MapsFindFunctionRatherThanSyntax
    }

    fn description(&self) -> &'static str {
        "Unnecessary allocation of result tuple when the key is found."
    }
}

/// A set of disjoint `(range, replacement)` edits to apply to the source.
type Edits = Vec<(TextRange, String)>;

#[derive(Debug, Clone)]
pub(crate) struct Context {
    edits: Edits,
}

impl GenericLinter for MapFindToSyntaxLinter {
    type Context = Context;

    fn matches(&self, ctx: &LinterContext) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let mut res = Vec::new();
        sema.for_each_function(file_id, |def| check_function(&mut res, sema, file_id, def));
        Some(res)
    }

    fn fixes(
        &self,
        context: &Context,
        range: TextRange,
        ctx: &LinterContext,
    ) -> Option<Vec<Assist>> {
        let mut builder = SourceChangeBuilder::new(ctx.file_id);
        for (edit_range, replacement) in &context.edits {
            builder.replace(*edit_range, replacement.clone());
        }
        Some(vec![fix(
            "maps_find_rather_than_syntax",
            "Rewrite to use map query syntax",
            builder.finish(),
            range,
        )])
    }
}

pub(crate) static LINTER: MapFindToSyntaxLinter = MapFindToSyntaxLinter;

#[derive(Clone, Copy)]
enum ClauseShape {
    Ok {
        /// The whole `{ok, V}` pattern, rewritten in place to `#{K := V}`.
        pat_span: TextRange,
        value_span: TextRange,
        /// Whether the value pattern matches every present value -- i.e. a fresh
        /// variable or `_`. A bound-variable value (`{ok, Expected}`) is an
        /// equality test, not a catch-all, so it does NOT count.
        value_is_catch_all: bool,
        guard_span: Option<TextRange>,
        body_span: TextRange,
    },
    /// The `error` atom clause or a bare `_` wildcard clause.
    Terminal {
        /// The `error`/`_` pattern, rewritten in place to `#{}`.
        pat_span: TextRange,
        body_span: TextRange,
        is_wildcard: bool,
    },
}

fn check_function(
    res: &mut Vec<GenericLinterMatchContext<Context>>,
    sema: &Semantic,
    file_id: FileId,
    def: &FunctionDef,
) {
    let def_fb = def.in_function_body(sema, def);
    def_fb.clone().fold_function(
        Strategy {
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        },
        (),
        &mut |_acc, clause_id, cb| {
            if cb.in_macro.is_some() {
                return;
            }
            let rewrite = match &cb.item {
                AnyExpr::Expr(Expr::Case { expr, clauses }) => build_case_rewrite(
                    sema,
                    def_fb.in_clause(clause_id),
                    file_id,
                    *expr,
                    clauses,
                    cb.item_id,
                ),
                AnyExpr::Expr(Expr::Match { lhs, rhs }) => build_match_rewrite(
                    sema,
                    def_fb.in_clause(clause_id),
                    file_id,
                    *lhs,
                    *rhs,
                    cb.item_id,
                ),
                _ => None,
            };
            if let Some((diagnostic_range, edits)) = rewrite {
                res.push(GenericLinterMatchContext {
                    range: FileRange {
                        file_id,
                        range: diagnostic_range,
                    },
                    context: Context { edits },
                });
            }
        },
    );
}

fn build_case_rewrite(
    sema: &Semantic,
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    file_id: FileId,
    scrutinee: ExprId,
    clauses: &[CRClause],
    case_id: AnyExprId,
) -> Option<(TextRange, Edits)> {
    let (key_id, map_id) = maps_find_args(in_clause, scrutinee)?;
    if !is_safe_key(in_clause, key_id) {
        return None;
    }

    // Classify every clause; an unrecognised clause declines the whole rewrite.
    let mut shapes = Vec::with_capacity(clauses.len());
    for clause in clauses {
        shapes.push(classify_clause(
            sema,
            in_clause,
            file_id,
            clause.pat,
            &clause.guards,
            &clause.exprs,
        )?);
    }

    // There must be exactly one terminal (`error` or `_`) clause; locate it.
    let mut terminal_idx = None;
    for (idx, shape) in shapes.iter().enumerate() {
        if matches!(shape, ClauseShape::Terminal { .. }) {
            if terminal_idx.is_some() {
                return None;
            }
            terminal_idx = Some(idx);
        }
    }
    let terminal_idx = terminal_idx?;
    // At least one ok clause is required alongside the single terminal clause.
    if shapes.len() < 2 {
        return None;
    }

    // Correctness guard (see module docs): an `error` fallback only becomes
    // `#{}` safely when some ok clause matches every present value.
    let is_wildcard = matches!(
        shapes[terminal_idx],
        ClauseShape::Terminal {
            is_wildcard: true,
            ..
        }
    );
    let exhaustive = shapes.iter().any(|shape| {
        matches!(
            shape,
            ClauseShape::Ok {
                value_is_catch_all: true,
                guard_span: None,
                ..
            }
        )
    });
    if !is_wildcard && !exhaustive {
        return None;
    }

    let full = in_clause.range_for_any(case_id)?;
    if full.file_id != file_id {
        return None;
    }
    let full_range = full.range;

    let key_text = text_of_expr(sema, in_clause, file_id, key_id)?;
    let map = in_clause.range_for_expr(map_id)?;
    if map.file_id != file_id {
        return None;
    }
    let map_range = map.range;
    let map_text = slice(sema, file_id, map_range);

    // Preferred path: rewrite only the scrutinee and each clause pattern in
    // place, leaving every body, guard, separator, and comment byte-for-byte.
    // The rewritten `#{}` pattern matches any map, so it must remain the final
    // clause -- this is only sound when the terminal clause is already last.
    if terminal_idx == shapes.len() - 1
        && let Some(edits) = precise_case_edits(
            sema, in_clause, file_id, scrutinee, map_range, &map_text, &shapes, &key_text,
        )
    {
        return Some((full_range, edits));
    }

    // Fallback: the terminal clause is not last, so the clauses must be
    // reordered, which means regenerating the whole skeleton -- and that cannot
    // preserve comments, so it declines when the `case` contains one.
    let regenerated = regenerate_case(sema, file_id, full_range, &shapes, &key_text, &map_text)?;
    Some((full_range, vec![(full_range, regenerated)]))
}

/// Rewrite the scrutinee (`maps:find(K, M)` -> `M`) and each clause pattern
/// (`{ok, V}` -> `#{K := V}`, `error`/`_` -> `#{}`) in place, one edit per span,
/// so all surrounding trivia survives untouched.
///
/// Each edit re-emits some source verbatim -- the map argument as the new
/// subject, and each ok value pattern into its map pattern -- while discarding
/// the structural syntax around it (`maps:find(...)`, `{ok, ...}`). A comment
/// inside a re-emitted part is preserved; one in the discarded syntax would be
/// dropped, so we decline in that case and let the fallback path decide.
fn precise_case_edits(
    sema: &Semantic,
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    file_id: FileId,
    scrutinee: ExprId,
    map_range: TextRange,
    map_text: &str,
    shapes: &[ClauseShape],
    key_text: &str,
) -> Option<Edits> {
    let scrutinee_range = in_clause.range_for_expr(scrutinee)?;
    if scrutinee_range.file_id != file_id {
        return None;
    }
    // Only the map argument is carried over verbatim (as the new subject).
    if drops_comment(sema, file_id, scrutinee_range.range, &[map_range]) {
        return None;
    }

    let mut edits = Vec::with_capacity(shapes.len() + 1);
    edits.push((scrutinee_range.range, map_text.to_string()));
    for shape in shapes {
        match shape {
            ClauseShape::Ok {
                pat_span,
                value_span,
                ..
            } => {
                // Only the value pattern is carried over verbatim.
                if drops_comment(sema, file_id, *pat_span, &[*value_span]) {
                    return None;
                }
                let value_text = slice(sema, file_id, *value_span);
                edits.push((*pat_span, format!("#{{{key_text} := {value_text}}}")));
            }
            ClauseShape::Terminal { pat_span, .. } => {
                // `error`/`_` is a single token with no interior, so no comment
                // can hide in it.
                edits.push((*pat_span, "#{}".to_string()));
            }
        }
    }

    Some(edits)
}

/// Whether `replaced` contains an Erlang comment (`%`) that lies outside every
/// `kept` sub-range. The `kept` ranges are spliced verbatim into the rewrite,
/// so a comment inside them survives; a comment anywhere else in `replaced`
/// would be dropped, so the caller must decline.
fn drops_comment(
    sema: &Semantic,
    file_id: FileId,
    replaced: TextRange,
    kept: &[TextRange],
) -> bool {
    let mut kept: Vec<TextRange> = kept
        .iter()
        .copied()
        .filter(|k| replaced.contains_range(*k))
        .collect();
    kept.sort_by_key(|k| k.start());
    let mut cursor = replaced.start();
    for k in kept {
        if cursor < k.start() && contains_comment(sema, file_id, TextRange::new(cursor, k.start()))
        {
            return true;
        }
        cursor = cursor.max(k.end());
    }
    cursor < replaced.end()
        && contains_comment(sema, file_id, TextRange::new(cursor, replaced.end()))
}

fn contains_comment(sema: &Semantic, file_id: FileId, range: TextRange) -> bool {
    let source_file = sema.parse(file_id);
    let syntax = source_file.value.syntax();
    match syntax.covering_element(range) {
        NodeOrToken::Node(node) => node
            .descendants_with_tokens()
            .filter_map(|element| element.into_token())
            .any(|token| is_comment_token(&token, range)),
        NodeOrToken::Token(token) => is_comment_token(&token, range),
    }
}

fn is_comment_token(token: &SyntaxToken, range: TextRange) -> bool {
    token.kind() == SyntaxKind::COMMENT && range.intersect(token.text_range()).is_some()
}

/// Regenerate the whole `case ... of ... end`, moving the terminal clause last
/// (`#{}` matches any map, so it must come last). Declines when the `case`
/// contains a comment, which this whole-skeleton rewrite cannot preserve.
fn regenerate_case(
    sema: &Semantic,
    file_id: FileId,
    full_range: TextRange,
    shapes: &[ClauseShape],
    key_text: &str,
    map_text: &str,
) -> Option<String> {
    if contains_comment(sema, file_id, full_range) {
        return None;
    }
    let file_text = sema.db.file_text(file_id).text(sema.db);
    let start: usize = full_range.start().into();
    let base = line_indent(&file_text, start);

    let mut out = String::new();
    out.push_str("case ");
    out.push_str(map_text);
    out.push_str(" of\n");
    let mut term_body = None;
    for shape in shapes {
        match shape {
            ClauseShape::Ok {
                value_span,
                guard_span,
                body_span,
                ..
            } => {
                out.push_str(&base);
                out.push_str("    #{");
                out.push_str(key_text);
                out.push_str(" := ");
                out.push_str(&slice(sema, file_id, *value_span));
                out.push('}');
                if let Some(guard) = guard_span {
                    out.push_str(" when ");
                    out.push_str(&slice(sema, file_id, *guard));
                }
                out.push_str(" ->\n");
                out.push_str(&base);
                out.push_str("        ");
                out.push_str(&slice(sema, file_id, *body_span));
                out.push_str(";\n");
            }
            ClauseShape::Terminal { body_span, .. } => {
                term_body = Some(*body_span);
            }
        }
    }
    let term_body = term_body?;
    out.push_str(&base);
    out.push_str("    #{} ->\n");
    out.push_str(&base);
    out.push_str("        ");
    out.push_str(&slice(sema, file_id, term_body));
    out.push('\n');
    out.push_str(&base);
    out.push_str("end");

    Some(out)
}

/// Rewrite the match/assignment idiom `{ok, V} = maps:find(K, M)` into
/// `#{K := V} = M`. Behaviour-preserving for any value pattern (both raise
/// `badmatch` on a miss), so no exhaustiveness reasoning is needed here.
fn build_match_rewrite(
    sema: &Semantic,
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    file_id: FileId,
    lhs: PatId,
    rhs: ExprId,
    match_id: AnyExprId,
) -> Option<(TextRange, Edits)> {
    let value_pat = ok_tuple_value(in_clause, lhs)?;
    // A `Pat = Pat` alias value doesn't splice cleanly into a map value slot.
    if matches!(&in_clause[value_pat], Pat::Match { .. }) {
        return None;
    }
    let (key_id, map_id) = maps_find_args(in_clause, rhs)?;
    if !is_safe_key(in_clause, key_id) {
        return None;
    }

    let full = in_clause.range_for_any(match_id)?;
    if full.file_id != file_id {
        return None;
    }
    let full_range = full.range;

    let lhs_range = in_clause.range_for_pat(lhs)?;
    if lhs_range.file_id != file_id {
        return None;
    }
    let rhs_range = in_clause.range_for_expr(rhs)?;
    if rhs_range.file_id != file_id {
        return None;
    }
    let value_range = in_clause.range_for_pat(value_pat)?;
    if value_range.file_id != file_id {
        return None;
    }
    let map = in_clause.range_for_expr(map_id)?;
    if map.file_id != file_id {
        return None;
    }

    // Rewrite the `{ok, V}` pattern and the `maps:find(...)` call in place so
    // any comment around them survives. Only the value pattern (in the lhs) and
    // the map argument (in the rhs) are re-emitted verbatim; decline if a
    // comment hides in the discarded `{ok, ...}` or `maps:find(...)` syntax.
    if drops_comment(sema, file_id, lhs_range.range, &[value_range.range])
        || drops_comment(sema, file_id, rhs_range.range, &[map.range])
    {
        return None;
    }

    let key_text = text_of_expr(sema, in_clause, file_id, key_id)?;
    let value_text = slice(sema, file_id, value_range.range);
    let map_text = slice(sema, file_id, map.range);
    let edits = vec![
        (lhs_range.range, format!("#{{{key_text} := {value_text}}}")),
        (rhs_range.range, map_text),
    ];
    Some((full_range, edits))
}

/// If `pat` is `{ok, ValuePat}`, return the `ValuePat` id.
fn ok_tuple_value(in_clause: &InFunctionClauseBody<&FunctionDef>, pat: PatId) -> Option<PatId> {
    if let Pat::Tuple { pats } = &in_clause[pat]
        && pats.len() == 2
        && matches!(&in_clause[pats[0]], Pat::Literal(Literal::Atom(atom)) if atom.as_name() == known::ok)
    {
        Some(pats[1])
    } else {
        None
    }
}

fn maps_find_args(
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    scrutinee: ExprId,
) -> Option<(ExprId, ExprId)> {
    if let Expr::Call {
        target: CallTarget::Remote { module, name, .. },
        args,
    } = &in_clause[scrutinee]
        && args.len() == 2
        && atom_named(in_clause, *module, &known::maps)
        && atom_named(in_clause, *name, &known::find)
    {
        Some((args[0], args[1]))
    } else {
        None
    }
}

fn atom_named(in_clause: &InFunctionClauseBody<&FunctionDef>, id: ExprId, name: &Name) -> bool {
    matches!(&in_clause[id], Expr::Literal(Literal::Atom(atom)) if &atom.as_name() == name)
}

/// A map-pattern key must be a legal guard expression that cannot raise. We
/// accept a conservative subset (vars, literals, and tuples/lists/records of
/// those); calls, binaries with variable segments, arithmetic, etc. are
/// rejected because they either don't compile as a key or silently fall
/// through instead of crashing.
fn is_safe_key(in_clause: &InFunctionClauseBody<&FunctionDef>, id: ExprId) -> bool {
    match &in_clause[id] {
        Expr::Var(_) | Expr::Literal(_) => true,
        Expr::Tuple { exprs } => exprs.iter().all(|elem| is_safe_key(in_clause, *elem)),
        Expr::List { exprs, tail } => {
            exprs.iter().all(|elem| is_safe_key(in_clause, *elem))
                && tail.is_none_or(|t| is_safe_key(in_clause, t))
        }
        Expr::Record {
            fields,
            default_field,
            ..
        } => {
            fields.iter().all(|(_, val)| is_safe_key(in_clause, *val))
                && default_field.is_none_or(|d| is_safe_key(in_clause, d))
        }
        _ => false,
    }
}

/// Whether `pat` is a `Pat::Var` that binds a fresh variable in this clause (a
/// brand-new name, or the wildcard `_`), rather than a use of an already-bound
/// variable. A fresh `{ok, V}` matches every present value and so makes the ok
/// clauses exhaustive; a bound `{ok, Expected}` is really an equality test and
/// does not. Mirrors the resolution the `bound_variable` (W0060) lint uses.
fn pat_var_is_fresh_binding(in_clause: &InFunctionClauseBody<&FunctionDef>, pat: PatId) -> bool {
    match &in_clause[pat] {
        Pat::Var(var) => {
            let var = *var;
            match in_clause.resolver().resolve_pat_id(&var, pat) {
                // `_` never resolves; it always matches, never an equality test.
                None => true,
                // Fresh iff this pattern is among the variable's binding sites.
                Some(defs) => defs.contains(&pat),
            }
        }
        _ => false,
    }
}

fn classify_clause(
    sema: &Semantic,
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    file_id: FileId,
    pat: PatId,
    guards: &[Vec<ExprId>],
    body: &[ExprId],
) -> Option<ClauseShape> {
    let body_span = span_of_exprs(in_clause, file_id, body)?;
    let pat_range = in_clause.range_for_pat(pat)?;
    if pat_range.file_id != file_id {
        return None;
    }
    let pat_span = pat_range.range;
    match &in_clause[pat] {
        Pat::Tuple { pats } if pats.len() == 2 => match &in_clause[pats[0]] {
            Pat::Literal(Literal::Atom(atom)) if atom.as_name() == known::ok => {
                let value_pat = pats[1];
                // A `Pat = Pat` alias value doesn't splice cleanly into a map
                // value position; leave those to a future enhancement.
                if matches!(&in_clause[value_pat], Pat::Match { .. }) {
                    return None;
                }
                let value_is_catch_all = pat_var_is_fresh_binding(in_clause, value_pat);
                let value_range = in_clause.range_for_pat(value_pat)?;
                if value_range.file_id != file_id {
                    return None;
                }
                let guard_span = if guards.is_empty() {
                    None
                } else {
                    Some(span_of_guards(in_clause, file_id, guards)?)
                };
                Some(ClauseShape::Ok {
                    pat_span,
                    value_span: value_range.range,
                    value_is_catch_all,
                    guard_span,
                    body_span,
                })
            }
            _ => None,
        },
        Pat::Literal(Literal::Atom(atom))
            if atom.as_name() == known::error && guards.is_empty() =>
        {
            Some(ClauseShape::Terminal {
                pat_span,
                body_span,
                is_wildcard: false,
            })
        }
        Pat::Var(_) if guards.is_empty() => {
            // Only a bare `_` is safe to collapse into `#{}`; a named variable
            // catch-all binds the result and would lose that binding.
            if slice(sema, file_id, pat_span) == "_" {
                Some(ClauseShape::Terminal {
                    pat_span,
                    body_span,
                    is_wildcard: true,
                })
            } else {
                None
            }
        }
        _ => None,
    }
}

fn span_of_exprs(
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    file_id: FileId,
    exprs: &[ExprId],
) -> Option<TextRange> {
    let mut acc: Option<TextRange> = None;
    for expr in exprs {
        let range = in_clause.range_for_expr(*expr)?;
        if range.file_id != file_id {
            return None;
        }
        acc = Some(match acc {
            Some(prev) => prev.cover(range.range),
            None => range.range,
        });
    }
    acc
}

fn span_of_guards(
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    file_id: FileId,
    guards: &[Vec<ExprId>],
) -> Option<TextRange> {
    let mut acc: Option<TextRange> = None;
    for alt in guards {
        for guard in alt {
            let range = in_clause.range_for_expr(*guard)?;
            if range.file_id != file_id {
                return None;
            }
            acc = Some(match acc {
                Some(prev) => prev.cover(range.range),
                None => range.range,
            });
        }
    }
    acc
}

fn text_of_expr(
    sema: &Semantic,
    in_clause: &InFunctionClauseBody<&FunctionDef>,
    file_id: FileId,
    id: ExprId,
) -> Option<String> {
    let range = in_clause.range_for_expr(id)?;
    if range.file_id != file_id {
        return None;
    }
    Some(slice(sema, file_id, range.range))
}

fn slice(sema: &Semantic, file_id: FileId, range: TextRange) -> String {
    let file_text = sema.db.file_text(file_id).text(sema.db);
    file_text[range.start().into()..range.end().into()].to_string()
}

/// The whitespace column of `offset`, used as the base indentation for the
/// regenerated `case` so mid-line expressions align under the `case` keyword.
fn line_indent(text: &str, offset: usize) -> String {
    let line_start = text[..offset].rfind('\n').map(|i| i + 1).unwrap_or(0);
    text[line_start..offset]
        .chars()
        .map(|c| if c == '\t' { '\t' } else { ' ' })
        .collect()
}

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::MapsFindFunctionRatherThanSyntax
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        tests::check_filtered_diagnostics(fixture, &filter)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        tests::check_fix(fixture_before, fixture_after)
    }

    #[test]
    fn rewrites_map_find_preserving_comment_after_of() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(k, M) of % keep me
                {ok, V} ->
                    {found, V};
                error ->
                    not_found
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case M of % keep me
                #{k := V} ->
                    {found, V};
                #{} ->
                    not_found
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_preserving_comment_after_ok_arrow() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(k, M) of
                {ok, V} -> % keep me
                    {found, V};
                error ->
                    not_found
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{k := V} -> % keep me
                    {found, V};
                #{} ->
                    not_found
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_preserving_comment_after_body() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(k, M) of
                {ok, V} ->
                    {found, V}; % keep me
                error ->
                    not_found
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{k := V} ->
                    {found, V}; % keep me
                #{} ->
                    not_found
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_preserving_comment_on_error_arm() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(k, M) of
                {ok, V} ->
                    {found, V};
                error -> % keep me
                    not_found
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{k := V} ->
                    {found, V};
                #{} -> % keep me
                    not_found
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_preserving_comment_before_fallback_body() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(k, M) of
                {ok, V} ->
                    {found, V};
                error ->
                    % keep me
                    not_found
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{k := V} ->
                    {found, V};
                #{} ->
                    % keep me
                    not_found
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_preserving_comment_inside_value_pattern() {
        // The comment is inside the value pattern, which is spliced verbatim
        // into the map pattern, so it survives and the rewrite still fires.
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(k, M) of
                {ok, [A, % keep me
                      B]} ->
                    {A, B};
                _ ->
                    none
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{k := [A, % keep me
                      B]} ->
                    {A, B};
                #{} ->
                    none
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_match_idiom_preserving_comment_inside_value_pattern() {
        check_fix(
            r#"
         //- /src/mfm.erl
         -module(mfm).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            {ok, [A, % keep me
                  B]} = maps:fi~nd(k, M),
            {A, B}.
            "#,
            expect![[r#"
         -module(mfm).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            #{k := [A, % keep me
                  B]} = M,
            {A, B}.
            "#]],
        )
    }

    #[test]
    fn ignores_map_find_with_comment_inside_replaced_pattern() {
        // The comment sits inside the `{ok, V}` pattern we rewrite in place, so
        // re-emitting it as `#{k := V}` would drop the comment. Decline.
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of
                {ok, % keep me
                 V} ->
                    {found, V};
                error ->
                    not_found
            end.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_with_comment_inside_scrutinee() {
        // The comment sits inside the `maps:find(...)` call we replace with `M`,
        // so the rewrite would drop it. Decline.
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, % keep me
                           M) of
                {ok, V} ->
                    {found, V};
                error ->
                    not_found
            end.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_with_comment_when_reordering_needed() {
        // `error` is not last, so the clauses must be reordered, which requires
        // regenerating the skeleton and would drop the comment. Decline.
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of
                error -> % keep me
                    not_found;
                {ok, V} ->
                    {found, V}
            end.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_where_the_key_is_not_a_simple_pattern_call() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         get_key() -> [a,b].

         fn(K, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(get_key(), M) of {ok, V} -> Found; error -> NotFound end.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_where_the_key_is_not_a_simple_pattern_map() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(#{}, M) of {ok, V} -> Found; error -> NotFound end.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_where_the_key_is_not_a_simple_pattern_tuple() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         get_key() -> [a,b].

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find({tag,get_key()}, M) of {ok, V} -> Found; error -> NotFound end.
            "#,
        )
    }

    #[test]
    fn detects_map_find_with_simple_literal_key() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(my_key,M) of {ok, V} -> Found; error -> NotFound end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 warning: W0032: Unnecessary allocation of result tuple when the key is found.
            "#,
        )
    }

    #[test]
    fn detects_map_find_with_tuple_literal_key() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find({key_a,key_b},M) of {ok, V} -> Found; error -> NotFound end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 warning: W0032: Unnecessary allocation of result tuple when the key is found.
            "#,
        )
    }

    #[test]
    fn detects_map_find_with_immediate_pattern_match_using_error_atom() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(K, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(K,M) of {ok, V} -> Found; error -> NotFound end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 warning: W0032: Unnecessary allocation of result tuple when the key is found.
            "#,
        )
    }

    #[test]
    fn detects_map_find_with_immediate_pattern_match_using_underscore() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(K, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(K,M) of {ok, V} -> Found; _ -> NotFound end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 warning: W0032: Unnecessary allocation of result tuple when the key is found.
            "#,
        )
    }

    #[test]
    fn detects_map_find_with_multi_statement_body() {
        check_diagnostics(
            r#"
         //- /src/mfc.erl
         -module(mfc).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of {ok, V} -> X = g(V), {ok, X}; error -> none end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 warning: W0032: Unnecessary allocation of result tuple when the key is found.

         g(X) -> X.
            "#,
        )
    }

    #[test]
    fn detects_map_find_with_three_ok_clauses() {
        check_diagnostics(
            r#"
         //- /src/mfc.erl
         -module(mfc).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of {ok, a} -> 1; {ok, b} -> 2; {ok, _} -> 3; error -> 0 end.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^💡 warning: W0032: Unnecessary allocation of result tuple when the key is found.
            "#,
        )
    }

    #[test]
    fn detects_map_find_match_idiom() {
        check_diagnostics(
            r#"
         //- /src/mfc.erl
         -module(mfc).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            {ok, V} = maps:find(k, M), V.
         %% ^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0032: Unnecessary allocation of result tuple when the key is found.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_with_literal_value_and_error() {
        // Correctness guard: `{ok, a}` + `error` crashes on a present non-`a`
        // value, but `#{} -> ...` would not. So we decline.
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of {ok, a} -> Found; error -> NotFound end.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_with_guard_and_error() {
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of {ok, V} when is_binary(V) -> Found; error -> NotFound end.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_with_two_ok_cases_and_error() {
        // `{ok, a}; {ok, b}; error` crashes on any present value other than
        // `a` or `b`; `#{} -> ...` would not, so the rewrite is declined.
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(key, M) of {ok, a} -> FoundA; {ok, b} -> FoundB; error -> NotFound end.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_with_record_value_and_error() {
        // Correctness guard (the real deleter_SUITE shape): `{ok, #rec{}}`
        // + `error` crashes when the key is present but the value is not a
        // `#rec{}`, whereas `#{} -> ...` would swallow it. So we decline.
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         -record(rec, {f}).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of {ok, #rec{f = Found}} -> Found; error -> NotFound end.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_with_bound_value_var_and_error() {
        // `Expected` is a bound parameter, so `{ok, Expected}` is an equality
        // test, not a catch-all. With an `error` fallback the original crashes
        // on a present value other than `Expected`, but `#{} -> ...` would
        // swallow it -- so the ok clauses are NOT exhaustive and we decline.
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Expected, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of
                {ok, Expected} ->
                    yes;
                error ->
                    no
            end.
            "#,
        )
    }

    #[test]
    fn rewrites_map_find_with_bound_value_var_and_wildcard() {
        // A bound value var is fine when the fallback is `_`: both the original
        // `_` and the rewritten `#{}` catch a present value that isn't
        // `Expected`, so no exhaustiveness reasoning is needed.
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Expected, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(k, M) of
                {ok, Expected} ->
                    yes;
                _ ->
                    no
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(Expected, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{k := Expected} ->
                    yes;
                #{} ->
                    no
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_bound_var_rescued_by_fresh_var() {
        // The bound `{ok, Expected}` clause isn't a catch-all, but the later
        // unguarded `{ok, V}` clause matches every present value, so the ok
        // clauses are exhaustive and the `error` fallback rewrite is safe.
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Expected, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(k, M) of
                {ok, Expected} ->
                    Expected;
                {ok, V} ->
                    V;
                error ->
                    none
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(Expected, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{k := Expected} ->
                    Expected;
                #{k := V} ->
                    V;
                #{} ->
                    none
            end.
            "#]],
        )
    }

    #[test]
    fn ignores_map_find_with_named_var_terminal() {
        // A named catch-all binds the whole `maps:find` result; `#{}` would not,
        // so the binding would be lost. Decline.
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of
                {ok, V} ->
                    V;
                Other ->
                    Other
            end.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_with_alias_value() {
        // A `V = Pat` alias value doesn't splice cleanly into a map value slot,
        // even with a `_` fallback (so exhaustiveness is not the reason).
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of
                {ok, V = {a, B}} ->
                    {V, B};
                _ ->
                    none
            end.
            "#,
        )
    }

    #[test]
    fn ignores_match_idiom_with_alias_value() {
        check_diagnostics(
            r#"
         //- /src/mfm.erl
         -module(mfm).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            {ok, V = {a, B}} = maps:find(k, M),
            {V, B}.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_with_no_fallback_clause() {
        // No `error`/`_` clause means there is nothing to become the `#{}` arm.
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of
                {ok, a} -> 1;
                {ok, b} -> 2
            end.
            "#,
        )
    }

    #[test]
    fn ignores_map_find_with_two_terminal_clauses() {
        // Two fallbacks (`error` and `_`) can't both collapse into one `#{}`.
        check_diagnostics(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:find(k, M) of
                {ok, V} -> V;
                error -> a;
                _ -> b
            end.
            "#,
        )
    }

    #[test]
    fn rewrites_match_idiom_with_bound_value_var() {
        // Unlike the `case` shape, the match idiom is safe with a bound value
        // var: both `{ok, Expected} = ...` and `#{k := Expected} = M` raise
        // `badmatch` on a mismatch, so no exhaustiveness reasoning is needed.
        check_fix(
            r#"
         //- /src/mfm.erl
         -module(mfm).

         fn(Expected, M) ->
            % elp:ignore W0017 (undefined_function)
            {ok, Expected} = maps:fi~nd(k, M),
            Expected.
            "#,
            expect![[r#"
         -module(mfm).

         fn(Expected, M) ->
            % elp:ignore W0017 (undefined_function)
            #{k := Expected} = M,
            Expected.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_list_key() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd([a, b], M) of
                {ok, V} ->
                    V;
                error ->
                    none
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{[a, b] := V} ->
                    V;
                #{} ->
                    none
            end.
            "#]],
        )
    }

    #[test]
    fn ignores_match_idiom_with_call_key() {
        check_diagnostics(
            r#"
         //- /src/mfc.erl
         -module(mfc).

         get_key() -> k.

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            {ok, V} = maps:find(get_key(), M), V.
            "#,
        )
    }

    #[test]
    fn rewrites_map_find_with_immediate_pattern_match_using_error_atom_to_syntax() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(K, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(K,M) of
                {ok, V} ->
                    Found;
                error ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(K, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{K := V} ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_immediate_pattern_match_using_underscore_to_syntax() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(K, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(K,M) of
                {ok, V} ->
                    Found;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(K, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{K := V} ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_simple_literal_key() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(my_key,M) of
                {ok, V} ->
                    Found;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{my_key := V} ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_literal_tuple_key() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd({key_a,key_b},M) of
                {ok, V} ->
                    Found;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{{key_a,key_b} := V} ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_tuple_pat_key() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(KB, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd({key_a,KB},M) of
                {ok, V} ->
                    Found;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(KB, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{{key_a,KB} := V} ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_where_error_is_handled_first() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(my_key,M) of
                error -> NotFound;
                {ok, V} -> Found
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{my_key := V} ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_mid_line_map_find_where_error_is_handled_first() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            X = case maps:fi~nd(k, M) of
                error -> none;
                {ok, V} -> V
            end,
            X.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            X = case M of
                    #{k := V} ->
                        V;
                    #{} ->
                        none
                end,
            X.
            "#]],
        )
    }

    #[test]
    fn rewrites_reordered_case_with_percent_literals() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd({"%", '%', $%}, M) of
                error ->
                    {"%", '%', $%};
                {ok, V} ->
                    V
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{{"%", '%', $%} := V} ->
                    V;
                #{} ->
                    {"%", '%', $%}
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_two_ok_cases_and_underscore() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(KB, FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(key,M) of
                {ok, a} ->
                    FoundA;
                {ok, b} ->
                    FoundB;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(KB, FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{key := a} ->
                    FoundA;
                #{key := b} ->
                    FoundB;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_two_ok_cases_guard_and_underscore() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(KB, FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(key,M) of
                {ok, A} when is_binary(A) ->
                    FoundA;
                {ok, B} ->
                    FoundB;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(KB, FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{key := A} when is_binary(A) ->
                    FoundA;
                #{key := B} ->
                    FoundB;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_two_ok_cases_guard_and_error() {
        // Safe despite the `error` fallback: the unguarded `{ok, B}` clause
        // matches every present value, so the ok clauses are exhaustive.
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(KB, FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(key,M) of
                {ok, A} when is_binary(A) ->
                    FoundA;
                {ok, B} ->
                    FoundB;
                error ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(KB, FoundA, FoundB, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{key := A} when is_binary(A) ->
                    FoundA;
                #{key := B} ->
                    FoundB;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_guard() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(KB, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(key,M) of
                {ok, V} when is_binary(V) ->
                    Found;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(KB, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{key := V} when is_binary(V) ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_compound_guard() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(KB, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(key,M) of
                {ok, V} when is_binary(V) orelse is_atom(V) ->
                    Found;
                _ ->
                    NotFound
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(KB, Found, NotFound, M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{key := V} when is_binary(V) orelse is_atom(V) ->
                    Found;
                #{} ->
                    NotFound
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_record_value() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         -record(rec, {f}).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(k, M) of
                {ok, #rec{f = V}} ->
                    V;
                _ ->
                    none
            end.
            "#,
            expect![[r#"
         -module(map_find_function).

         -record(rec, {f}).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{k := #rec{f = V}} ->
                    V;
                #{} ->
                    none
            end.
            "#]],
        )
    }

    #[test]
    fn rewrites_map_find_with_multi_statement_body() {
        check_fix(
            r#"
         //- /src/map_find_function.erl
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case maps:fi~nd(k, M) of
                {ok, V} ->
                    X = g(V),
                    {ok, X};
                error ->
                    none
            end.

         g(X) -> X.
            "#,
            expect![[r#"
         -module(map_find_function).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            case M of
                #{k := V} ->
                    X = g(V),
                    {ok, X};
                #{} ->
                    none
            end.

         g(X) -> X.
            "#]],
        )
    }

    #[test]
    fn rewrites_match_idiom() {
        check_fix(
            r#"
         //- /src/mfm.erl
         -module(mfm).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            {ok, V} = maps:fi~nd(k, M), V.
            "#,
            expect![[r#"
         -module(mfm).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            #{k := V} = M, V.
            "#]],
        )
    }

    #[test]
    fn rewrites_match_idiom_with_percent_literals_in_key() {
        check_fix(
            r#"
         //- /src/mfm.erl
         -module(mfm).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            {ok, V} = maps:fi~nd({"%", '%', $%}, M), V.
            "#,
            expect![[r#"
         -module(mfm).

         fn(M) ->
            % elp:ignore W0017 (undefined_function)
            #{{"%", '%', $%} := V} = M, V.
            "#]],
        )
    }
}

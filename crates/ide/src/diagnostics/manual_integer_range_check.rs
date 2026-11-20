/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: manual_integer_range_check
//!
//! Warn on a conjunction (a guard, or an `andalso` chain) that checks
//! `is_integer(X)` together with a lower and an upper bound on `X`, such as
//!
//! ```erlang
//! f(X) when is_integer(X), X > 0, X < 10 -> ok.
//! ```
//!
//! and suggest the `is_integer/3` BIF, available from OTP 29, which does the
//! same in one call: `is_integer(X, 1, 9)`.
//!
//! Both bounds must be integers, otherwise `is_integer/3` raises `badarg`
//! where the comparisons would not. A bound qualifies when it is built from
//! integer or character literals, macros expanding to them, integer arithmetic
//! and variables checked by `is_integer/1` in the same conjunction.
//!
//! In an `andalso` chain, `is_integer/3` evaluates both bounds even where the
//! original chain would have stopped at an earlier `false`, so there a `div`
//! or `rem` in a bound must have a non-zero literal divisor, and a `bsl` or
//! `bsr` a literal shift count of magnitude at most `MAX_SHIFT`.
//!
//! Constant bounds that give an empty range are not reported: the check is
//! always false, and the compiler rejects such an `is_integer/3` call.

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::text_edit::TextRange;
use elp_project_model::otp::Otp;
use elp_syntax::ast::ArithOp;
use elp_syntax::ast::BinaryOp;
use elp_syntax::ast::CompOp;
use elp_syntax::ast::LogicOp;
use elp_syntax::ast::Ordering;
use elp_syntax::ast::UnaryOp;
use fxhash::FxHashSet;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::BasedInteger;
use hir::CallTarget;
use hir::Expr;
use hir::ExprId;
use hir::FoldBody;
use hir::InFile;
use hir::Literal;
use hir::Name;
use hir::Semantic;
use hir::Strategy;
use hir::Var;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::ParentId;
use hir::known;

use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;

pub(crate) struct ManualIntegerRangeCheckLinter;

impl Linter for ManualIntegerRangeCheckLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::ManualIntegerRangeCheck
    }

    fn description(&self) -> &'static str {
        "Integer range check can be written with `is_integer/3`."
    }

    fn should_process_file_id(&self, _sema: &Semantic, _file_id: FileId) -> bool {
        Otp::supports_is_integer_3()
    }
}

impl GenericLinter for ManualIntegerRangeCheckLinter {
    type Context = ();

    fn matches(
        &self,
        ctx: &LinterContext,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let mut res = Vec::new();

        sema.def_map_local(file_id)
            .get_functions()
            .for_each(|(_, fun_def)| {
                let def_fb = fun_def.in_function_body(sema, fun_def);
                let fun_range = fun_def.range(sema.db);
                for (_, clause) in def_fb.clauses() {
                    let body = clause.body.index_with_strategy(STRATEGY);
                    let mut conjunctions: Vec<Conjunction> = clause
                        .clause
                        .guards
                        .iter()
                        .map(|guard| Conjunction::guard(guard))
                        .collect();
                    let roots = clause
                        .clause
                        .guards
                        .iter()
                        .flatten()
                        .chain(clause.clause.exprs.iter())
                        .copied();
                    collect_conjunctions(&body, roots, &mut conjunctions);
                    res.extend(conjunctions.iter().filter_map(|conjunction| {
                        check_conjunction(sema, file_id, &body, fun_range, conjunction)
                    }));
                }
            });

        sema.form_list(file_id)
            .define_attributes()
            .for_each(|(define_id, _)| {
                let define_body = sema.db.define_body(InFile::new(file_id, define_id));
                let body = define_body.body.index_with_strategy(STRATEGY);
                let mut conjunctions = Vec::new();
                collect_conjunctions(&body, [define_body.expr].into_iter(), &mut conjunctions);
                res.extend(conjunctions.iter().filter_map(|conjunction| {
                    check_conjunction(sema, file_id, &body, None, conjunction)
                }));
            });

        Some(res)
    }
}

pub(crate) static LINTER: ManualIntegerRangeCheckLinter = ManualIntegerRangeCheckLinter;

const STRATEGY: Strategy = Strategy {
    macros: MacroStrategy::DoNotExpand,
    parens: ParenStrategy::VisibleParens,
};

#[derive(Debug)]
struct Conjunction {
    /// In source order.
    conjuncts: Vec<ExprId>,
    /// Whether the conjuncts are evaluated left to right, stopping at the
    /// first `false`: true for an `andalso` chain, false for a guard.
    short_circuit: bool,
}

impl Conjunction {
    fn guard(conjuncts: &[ExprId]) -> Conjunction {
        Conjunction {
            conjuncts: conjuncts.to_vec(),
            short_circuit: false,
        }
    }
}

/// Collect the guards of every clause under `roots`, and every maximal
/// `andalso` chain.
fn collect_conjunctions(
    body: &FoldBody,
    roots: impl Iterator<Item = ExprId>,
    conjunctions: &mut Vec<Conjunction>,
) {
    roots.for_each(|root| {
        body.body.fold_expr(STRATEGY, root, (), &mut |(), ctx| {
            let (AnyExpr::Expr(expr), AnyExprId::Expr(expr_id)) = (&ctx.item, ctx.item_id) else {
                return;
            };
            conjunctions.extend(
                clause_guards(expr)
                    .into_iter()
                    .map(|guard| Conjunction::guard(guard)),
            );
            if is_andalso(body, expr_id) && !is_andalso_operand(body, ctx.parent()) {
                let mut chain = Vec::new();
                flatten_andalso(body, expr_id, &mut chain);
                conjunctions.push(Conjunction {
                    conjuncts: chain,
                    short_circuit: true,
                });
            }
        });
    });
}

/// The guards of the clauses of `expr`, one entry per guard.
fn clause_guards(expr: &Expr) -> Vec<&Vec<ExprId>> {
    match expr {
        Expr::If { clauses } => clauses.iter().flat_map(|clause| &clause.guards).collect(),
        Expr::Case { clauses, .. }
        | Expr::Receive { clauses, .. }
        | Expr::Maybe {
            else_clauses: clauses,
            ..
        } => clauses.iter().flat_map(|clause| &clause.guards).collect(),
        Expr::Try {
            of_clauses,
            catch_clauses,
            ..
        } => of_clauses
            .iter()
            .flat_map(|clause| &clause.guards)
            .chain(catch_clauses.iter().flat_map(|clause| &clause.guards))
            .collect(),
        Expr::Closure { clauses, .. } => clauses.iter().flat_map(|clause| &clause.guards).collect(),
        _ => Vec::new(),
    }
}

fn is_andalso_operand(body: &FoldBody, parent: ParentId) -> bool {
    let ParentId::HirIdx(parent) = parent else {
        return false;
    };
    let AnyExprId::Expr(parent_id) = parent.idx else {
        return false;
    };
    is_andalso(body, parent_id)
}

fn is_andalso(body: &FoldBody, expr_id: ExprId) -> bool {
    matches!(
        body[expr_id],
        Expr::BinaryOp {
            op: BinaryOp::LogicOp(LogicOp::And { lazy: true }),
            ..
        }
    )
}

/// Operands in parentheses stay whole, so that removing one never leaves an
/// unbalanced parenthesis behind.
fn flatten_andalso(body: &FoldBody, expr_id: ExprId, chain: &mut Vec<ExprId>) {
    match &body[expr_id] {
        Expr::BinaryOp {
            lhs,
            rhs,
            op: BinaryOp::LogicOp(LogicOp::And { lazy: true }),
        } => {
            flatten_andalso(body, *lhs, chain);
            flatten_andalso(body, *rhs, chain);
        }
        _ => chain.push(expr_id),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Side {
    Lower,
    Upper,
}

#[derive(Debug, Clone, Copy)]
struct Bound {
    conjunct: usize,
    var: Var,
    side: Side,
    strict: bool,
    expr: ExprId,
}

fn check_conjunction(
    sema: &Semantic,
    file_id: FileId,
    body: &FoldBody,
    enclosing: Option<TextRange>,
    conjunction: &Conjunction,
) -> Option<GenericLinterMatchContext<()>> {
    let conjuncts = conjunction.conjuncts.as_slice();
    if conjuncts.len() < 3 {
        return None;
    }
    let checked: Vec<(usize, Var)> = conjuncts
        .iter()
        .enumerate()
        .filter_map(|(idx, conjunct)| is_integer_test(body, *conjunct).map(|var| (idx, var)))
        .collect();
    if checked.is_empty() {
        return None;
    }
    let bounds: Vec<Bound> = conjuncts
        .iter()
        .enumerate()
        .flat_map(|(idx, conjunct)| comparison_bounds(body, idx, *conjunct))
        .collect();

    let (checked_idx, lower, upper) = checked.iter().find_map(|(idx, var)| {
        // `is_integer/3` evaluates its bounds where `is_integer/1` was, so in
        // an `andalso` chain a variable bound must be checked before that.
        let integer_vars: FxHashSet<Var> = checked
            .iter()
            .filter(|(other, _)| !conjunction.short_circuit || other < idx)
            .map(|(_, var)| *var)
            .collect();
        let bound_ctx = BoundContext {
            integer_vars: &integer_vars,
            checked_var: *var,
            allow_any_right_operand: !conjunction.short_circuit,
        };
        let find = |side| {
            bounds.iter().find(|bound| {
                bound.var == *var
                    && bound.side == side
                    && is_integer_valued(body, bound.expr, &bound_ctx)
            })
        };
        let lower = find(Side::Lower)?;
        let upper = find(Side::Upper)?;
        if let (Some(lo), Some(hi)) = (inclusive_value(body, lower), inclusive_value(body, upper))
            && may_be_empty(lo, hi)
        {
            return None;
        }
        Some((*idx, lower.conjunct, upper.conjunct))
    })?;

    let ranges = conjuncts
        .iter()
        .map(|conjunct| {
            let range = body.body.range_for_any(sema, AnyExprId::Expr(*conjunct))?;
            (range.file_id == file_id).then_some(range.range)
        })
        .collect::<Option<Vec<_>>>()?;
    // Conjuncts spliced in from a macro definition map to the definition.
    if ranges
        .windows(2)
        .any(|pair| pair[0].end() > pair[1].start())
        || enclosing
            .is_some_and(|enclosing| ranges.iter().any(|range| !enclosing.contains_range(*range)))
    {
        return None;
    }
    let involved = [checked_idx, lower, upper];
    let start = involved.iter().map(|idx| ranges[*idx].start()).min()?;
    let end = involved.iter().map(|idx| ranges[*idx].end()).max()?;
    Some(GenericLinterMatchContext {
        range: FileRange {
            file_id,
            range: TextRange::new(start, end),
        },
        context: (),
    })
}

fn peel_parens(body: &FoldBody, mut expr_id: ExprId) -> ExprId {
    while let Expr::Paren { expr } = &body[expr_id] {
        expr_id = *expr;
    }
    expr_id
}

fn as_var(body: &FoldBody, expr_id: ExprId) -> Option<Var> {
    match &body[peel_parens(body, expr_id)] {
        Expr::Var(var) => Some(*var),
        _ => None,
    }
}

/// The variable checked by `is_integer(Var)` or `erlang:is_integer(Var)`.
fn is_integer_test(body: &FoldBody, expr_id: ExprId) -> Option<Var> {
    let Expr::Call { target, args } = &body[peel_parens(body, expr_id)] else {
        return None;
    };
    let [arg] = args.as_slice() else {
        return None;
    };
    let name = match target {
        CallTarget::Local { name } => name,
        CallTarget::Remote { module, name, .. } => {
            if atom_name(body, *module)? != *known::erlang {
                return None;
            }
            name
        }
    };
    if atom_name(body, *name)? != *known::is_integer {
        return None;
    }
    as_var(body, *arg)
}

fn atom_name(body: &FoldBody, expr_id: ExprId) -> Option<Name> {
    match &body[expr_id] {
        Expr::Literal(Literal::Atom(atom)) => Some(atom.as_name()),
        _ => None,
    }
}

/// The bounds a comparison such as `X < Hi` or `Lo =< X` places on a variable.
/// A comparison between two variables bounds each of them.
fn comparison_bounds(body: &FoldBody, conjunct: usize, expr_id: ExprId) -> Vec<Bound> {
    let Expr::BinaryOp {
        lhs,
        rhs,
        op: BinaryOp::CompOp(CompOp::Ord { ordering, strict }),
    } = &body[peel_parens(body, expr_id)]
    else {
        return Vec::new();
    };
    let (lhs_side, rhs_side) = match ordering {
        Ordering::Greater => (Side::Lower, Side::Upper),
        Ordering::Less => (Side::Upper, Side::Lower),
    };
    [(*lhs, *rhs, lhs_side), (*rhs, *lhs, rhs_side)]
        .into_iter()
        .filter_map(|(var_expr, bound_expr, side)| {
            Some(Bound {
                conjunct,
                var: as_var(body, var_expr)?,
                side,
                strict: *strict,
                expr: bound_expr,
            })
        })
        .collect()
}

/// The largest shift count allowed in a bound in an `andalso` chain, far below
/// the counts that raise `system_limit`.
const MAX_SHIFT: i128 = 1024;

struct BoundContext<'a> {
    /// Variables known to be integers where `is_integer/3` evaluates its bounds.
    integer_vars: &'a FxHashSet<Var>,
    /// The variable whose range is checked, which cannot bound itself.
    checked_var: Var,
    /// Whether a bound may use `div` or `rem` with any divisor and `bsl` or
    /// `bsr` with any shift count: only where an exception has the same effect
    /// as `false`, that is, outside `andalso` chains.
    allow_any_right_operand: bool,
}

fn is_integer_valued(body: &FoldBody, expr_id: ExprId, ctx: &BoundContext) -> bool {
    match &body[expr_id] {
        Expr::Paren { expr }
        | Expr::MacroCall {
            expansion: expr, ..
        }
        | Expr::UnaryOp {
            expr,
            op: UnaryOp::Plus | UnaryOp::Minus | UnaryOp::Bnot,
        } => is_integer_valued(body, *expr, ctx),
        Expr::Literal(Literal::Integer(_) | Literal::Char(_)) => true,
        Expr::Var(var) => *var != ctx.checked_var && ctx.integer_vars.contains(var),
        Expr::BinaryOp {
            lhs,
            rhs,
            op: BinaryOp::ArithOp(op),
        } => {
            let allowed = match op {
                ArithOp::FloatDiv => false,
                ArithOp::Div | ArithOp::Rem => {
                    ctx.allow_any_right_operand
                        || integer_literal(body, *rhs).is_some_and(|v| v != 0)
                }
                ArithOp::Bsl | ArithOp::Bsr => {
                    ctx.allow_any_right_operand
                        || integer_literal(body, *rhs)
                            .is_some_and(|count| count.unsigned_abs() <= MAX_SHIFT as u128)
                }
                _ => true,
            };
            allowed && is_integer_valued(body, *lhs, ctx) && is_integer_valued(body, *rhs, ctx)
        }
        _ => false,
    }
}

/// The value of an integer literal, through parentheses and macros.
fn integer_literal(body: &FoldBody, expr_id: ExprId) -> Option<i128> {
    match &body[expr_id] {
        Expr::Paren { expr }
        | Expr::MacroCall {
            expansion: expr, ..
        } => integer_literal(body, *expr),
        Expr::Literal(Literal::Integer(BasedInteger { value, .. })) => Some(*value),
        _ => None,
    }
}

/// The value of a bound built only from integer and character literals.
#[derive(Debug, Clone, Copy)]
enum Constant {
    Int(i128),
    AboveI128,
    BelowI128,
    /// Arithmetic on a value outside `i128`, or a division by zero.
    Unknown,
}

impl Constant {
    fn checked(value: Option<i128>, positive_on_overflow: bool) -> Constant {
        match value {
            Some(value) => Constant::Int(value),
            None if positive_on_overflow => Constant::AboveI128,
            None => Constant::BelowI128,
        }
    }
}

/// The value of `expr_id`, or `None` if it is not built only from integer and
/// character literals.
fn constant(body: &FoldBody, expr_id: ExprId) -> Option<Constant> {
    match &body[expr_id] {
        Expr::Paren { expr }
        | Expr::MacroCall {
            expansion: expr, ..
        }
        | Expr::UnaryOp {
            expr,
            op: UnaryOp::Plus,
        } => constant(body, *expr),
        Expr::UnaryOp {
            expr,
            op: UnaryOp::Minus,
        } => Some(match constant(body, *expr)? {
            Constant::Int(value) => Constant::checked(value.checked_neg(), true),
            Constant::AboveI128 => Constant::BelowI128,
            Constant::BelowI128 => Constant::AboveI128,
            Constant::Unknown => Constant::Unknown,
        }),
        Expr::UnaryOp {
            expr,
            op: UnaryOp::Bnot,
        } => Some(match constant(body, *expr)? {
            Constant::Int(value) => Constant::Int(!value),
            _ => Constant::Unknown,
        }),
        Expr::Literal(Literal::Integer(BasedInteger { value, .. })) => Some(Constant::Int(*value)),
        Expr::Literal(Literal::Char(c)) => Some(Constant::Int(i128::from(u32::from(*c)))),
        Expr::BinaryOp {
            lhs,
            rhs,
            op: BinaryOp::ArithOp(op),
        } => {
            let (Constant::Int(lhs), Constant::Int(rhs)) =
                (constant(body, *lhs)?, constant(body, *rhs)?)
            else {
                return Some(Constant::Unknown);
            };
            Some(match op {
                ArithOp::Add => Constant::checked(lhs.checked_add(rhs), lhs > 0),
                ArithOp::Sub => Constant::checked(lhs.checked_sub(rhs), lhs >= 0),
                ArithOp::Mul => Constant::checked(lhs.checked_mul(rhs), (lhs < 0) == (rhs < 0)),
                ArithOp::Div | ArithOp::Rem if rhs == 0 => Constant::Unknown,
                ArithOp::Div => Constant::checked(lhs.checked_div(rhs), true),
                ArithOp::Rem => Constant::Int(lhs.wrapping_rem(rhs)),
                ArithOp::Band => Constant::Int(lhs & rhs),
                ArithOp::Bor => Constant::Int(lhs | rhs),
                ArithOp::Bxor => Constant::Int(lhs ^ rhs),
                ArithOp::Bsl => shift_left(lhs, rhs),
                ArithOp::Bsr => shift_left(lhs, rhs.saturating_neg()),
                ArithOp::FloatDiv => Constant::Unknown,
            })
        }
        _ => None,
    }
}

/// `value bsl count`, where a negative count shifts right.
fn shift_left(value: i128, count: i128) -> Constant {
    if value == 0 {
        return Constant::Int(0);
    }
    if count < 0 {
        // Shifting right by 127 or more leaves only the sign: 0 or -1.
        let count = u32::try_from(count.unsigned_abs().min(127)).expect("should be at most 127");
        return Constant::Int(value >> count);
    }
    match u32::try_from(count) {
        Ok(count) if count < 128 && (value << count) >> count == value => {
            Constant::Int(value << count)
        }
        _ => Constant::checked(None, value > 0),
    }
}

/// The value of `bound` made inclusive, or `None` if it is not constant.
fn inclusive_value(body: &FoldBody, bound: &Bound) -> Option<Constant> {
    let value = constant(body, bound.expr)?;
    Some(match (value, bound.strict, bound.side) {
        (Constant::Int(value), true, Side::Lower) => Constant::checked(value.checked_add(1), true),
        (Constant::Int(value), true, Side::Upper) => Constant::checked(value.checked_sub(1), false),
        // Made inclusive, a value outside `i128` stays outside on its side.
        _ => value,
    })
}

/// Whether inclusive constant bounds give, or may give, an empty range.
fn may_be_empty(lower: Constant, upper: Constant) -> bool {
    match (lower, upper) {
        (Constant::Int(lower), Constant::Int(upper)) => lower > upper,
        (Constant::BelowI128, Constant::Int(_) | Constant::AboveI128)
        | (Constant::Int(_), Constant::AboveI128) => false,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use elp_ide_db::DiagnosticCode;
    use elp_project_model::otp::Otp;

    use crate::diagnostics::Diagnostic;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::ManualIntegerRangeCheck
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        if Otp::supports_is_integer_3() {
            tests::check_filtered_diagnostics(fixture, &filter)
        }
    }

    #[test]
    fn guard_with_exclusive_bounds() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/1]).
            f(X) when is_integer(X), X > 0, X < 10 -> X.
            %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                   | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn reports_inclusive_and_reversed_bounds() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            f(X) when 0 =< X, is_integer(X), 10 >= X -> X.
            %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                     | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn reports_with_other_guard_tests() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            f(X, Y) when is_atom(Y), X >= -5, is_integer(X), Y =/= ok, X =< 5 -> X.
            %%                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                                              | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn reports_for_one_guard_of_several() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            f(X) when is_integer(X), X > 0, X < 10; X =:= none -> X.
            %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                   | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn reports_andalso_chain() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            f(X) -> is_integer(X) andalso X > 0 andalso X =< 100.
            %%      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                                 | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn reports_andalso_chain_in_guard() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            f(X) when is_integer(X) andalso X >= 1 andalso X < 3 -> X.
            %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                                 | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn reports_case_clause_guard() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            f(X) ->
                case X of
                    Y when is_integer(Y), Y > 0, Y < 256 -> Y;
            %%             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                         | 💡 <suppression>
                    _ -> 0
                end.
            "#,
        );
    }

    #[test]
    fn reports_macro_and_expression_bounds() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -define(BITS, 8).
            f(X) when is_integer(X), X > 0, X < (1 bsl ?BITS) -> X.
            %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                              | 💡 <suppression>
            "#,
        );
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -define(MAX, 720).
            f(X) when is_integer(X), X > ?MAX div 2, X < 1 bsl ?MAX -> X.
            %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                                    | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn reports_variable_bound_checked_later_in_guard() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            f(X, N) when is_integer(X), X >= 0, X < N, is_integer(N) -> X.
            %%           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                      | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn reports_variable_bound_checked_earlier_in_andalso() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            f(X, N) -> is_integer(N) andalso is_integer(X) andalso X >= 0 andalso X < N.
            %%                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                                                        | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn no_diagnostic_for_division_bound_in_andalso() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/2, g/1]).
            -define(MAX, 720).
            f(X, Y) -> is_integer(Y) andalso is_integer(X) andalso X >= 0 andalso X < (10 div Y).
            g(X) -> is_integer(X) andalso X > ?MAX rem (7 - 7) andalso X < 10.
            "#,
        )
    }

    #[test]
    fn no_diagnostic_for_variable_shift_bound_in_andalso() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/2, g/2]).
            f(X, N) -> is_integer(N) andalso is_integer(X) andalso X >= 0 andalso X < (1 bsl N).
            g(X, N) -> is_integer(N) andalso is_integer(X) andalso X > (1024 bsr (N - 1)) andalso X < 10.
            "#,
        )
    }

    #[test]
    fn reports_literal_shift_bound_in_andalso() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -define(BITS, 8).
            f(X) -> is_integer(X) andalso X >= 0 andalso X < (1 bsl ?BITS).
            %%      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                                           | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn reports_literal_divisor_bound_in_andalso() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -define(MAX, 720).
            -define(PARTS, 4).
            f(X) -> is_integer(X) andalso X >= 0 andalso X < ?MAX div ?PARTS.
            %%      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                                             | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn no_diagnostic_for_large_shift_bound_in_andalso() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/1]).
            f(X) -> is_integer(X) andalso X >= 0 andalso X < (1 bsl 100000000).
            "#,
        )
    }

    #[test]
    fn no_diagnostic_for_variable_bound_checked_later_in_andalso() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/2]).
            f(X, N) -> is_integer(X) andalso X >= 0 andalso X < N andalso is_integer(N).
            "#,
        )
    }

    #[test]
    fn reports_char_bounds() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            f(C) when is_integer(C), C >= $a, C =< $z -> C.
            %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                      | 💡 <suppression>
            "#,
        );
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            f(C) when is_integer(C), C > $/, C < $: -> C.
            %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                    | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn reports_in_macro_definition() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -define(MAX, 720).
            -define(IS_PX(Px), (is_integer(Px) andalso Px > 0 andalso Px =< ?MAX)).
            %%                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                                                 | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn no_diagnostic_without_is_integer() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/1, g/1]).
            f(X) when is_number(X), X > 0, X < 10 -> X.
            g(X) when X > 0, X < 10 -> X.
            "#,
        )
    }

    #[test]
    fn no_diagnostic_with_one_bound() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/1]).
            f(X) when is_integer(X), X > 0, X =/= 10 -> X.
            "#,
        )
    }

    #[test]
    fn no_diagnostic_for_bounds_that_may_not_be_integers() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/1, g/3, h/1]).
            f(X) when is_integer(X), X > 0, X < 10.5 -> X.
            g(X, Lo, Hi) when is_integer(X), X >= Lo, X =< Hi -> X.
            h(X) when is_integer(X), X > 0, X < 10 / 2 -> X.
            "#,
        )
    }

    #[test]
    fn no_diagnostic_for_inverted_bounds() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/1, g/1, h/1]).
            -define(MAX, 720).
            f(X) when is_integer(X), X > 0, X < 1 -> X.
            g(X) when is_integer(X), X >= $b, X < $b -> X.
            h(X) -> is_integer(X) andalso X > ?MAX andalso X =< ?MAX div 2.
            "#,
        )
    }

    #[test]
    fn no_diagnostic_for_inverted_bounds_outside_i128() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/1, g/1, h/1, i/1]).
            f(X) when is_integer(X), X > (1 bsl 200), X < 10 -> X.
            g(X) when is_integer(X), X > 10, X < -(1 bsl 200) -> X.
            h(X) when is_integer(X), X >= (1 bsl 200), X =< (1 bsl 199) -> X.
            i(X) when is_integer(X), X > 0, X < 1 div 0 -> X.
            "#,
        )
    }

    #[test]
    fn reports_bounds_outside_i128() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            f(X) when is_integer(X), X >= -1 bsl 200, X =< 1 bsl 200 -> X.
            %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0097: Integer range check can be written with `is_integer/3`.
            %%                                                     | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn no_diagnostic_across_guards_or_orelse() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/1, g/1]).
            f(X) when is_integer(X), X > 0; X < 10 -> X.
            g(X) -> is_integer(X) andalso (X > 0 orelse X < 10).
            "#,
        )
    }

    #[test]
    fn no_diagnostic_for_guard_from_macro() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -export([f/1]).
            -define(IN_RANGE(X), is_integer(X), X > 0, X < 10).
            f(X) when ?IN_RANGE(X) -> X.
            "#,
        )
    }
}

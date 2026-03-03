/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Condition expression types and evaluation for `-if`/`-elif` preprocessor directives.
//!
//! This module provides a simplified expression representation (`ConditionExpr`) that can
//! be evaluated with a set of defined macro names to produce a boolean result. This enables
//! ELP to determine which preprocessor branches are active or inactive.

use std::collections::BTreeSet;

use elp_base_db::FileId;
use elp_syntax::ast;
use elp_syntax::ast::ArithOp;
use elp_syntax::ast::BinaryOp;
use elp_syntax::ast::CompOp;
use elp_syntax::ast::LogicOp;
use elp_syntax::ast::Ordering;
use elp_syntax::ast::UnaryOp;

use crate::Body;
use crate::CallTarget;
use crate::Expr;
use crate::ExprId;
use crate::Literal;
use crate::MacroName;
use crate::Name;
use crate::PPConditionId;
use crate::body::lower_condition_body;
use crate::db::DefDatabase;
use crate::known;

/// A simplified expression type for evaluating `-if`/`-elif` preprocessor conditions.
///
/// This representation captures the subset of Erlang expressions that are valid
/// in preprocessor conditions and can be evaluated at compile time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConditionExpr {
    /// `defined(MACRO)` - checks if a macro is defined
    Defined(Name),
    /// `not Expr` - logical negation
    Not(Box<ConditionExpr>),
    /// `Expr andalso Expr` - short-circuit logical and
    AndAlso(Box<ConditionExpr>, Box<ConditionExpr>),
    /// `Expr and Expr` - strict logical and (evaluates both sides)
    And(Box<ConditionExpr>, Box<ConditionExpr>),
    /// `Expr orelse Expr` - short-circuit logical or
    OrElse(Box<ConditionExpr>, Box<ConditionExpr>),
    /// `Expr or Expr` - strict logical or (evaluates both sides)
    Or(Box<ConditionExpr>, Box<ConditionExpr>),
    /// `true` or `false` atoms
    LiteralBool(bool),
    /// Integer literal
    Integer(i128),
    /// Atom (other than true/false)
    Atom(Name),
    /// String literal
    String(String),
    /// Comparison expression
    Compare {
        op: CompOp,
        left: Box<ConditionExpr>,
        right: Box<ConditionExpr>,
    },
    /// Binary arithmetic expression
    Arithmetic {
        op: ArithOp,
        left: Box<ConditionExpr>,
        right: Box<ConditionExpr>,
    },
    /// Unary arithmetic expression (-, +, bnot)
    UnaryArithmetic {
        op: UnaryOp,
        operand: Box<ConditionExpr>,
    },
    /// Unparseable or unsupported expression
    Invalid,
}

/// The result of evaluating a condition expression to a value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConditionValue {
    Bool(bool),
    Integer(i128),
    Atom(Name),
    String(String),
}

// ============================================================================
// Diagnostic-aware condition lowering
// ============================================================================

/// A diagnostic message generated during condition lowering.
///
/// This captures information about unsupported or invalid constructs
/// encountered when lowering an AST condition expression to `ConditionExpr`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConditionDiagnostic {
    /// The diagnostic message describing the issue
    pub message: String,
}

impl ConditionDiagnostic {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

/// The result of lowering a condition expression with diagnostic tracking.
///
/// This bundles the resulting `ConditionExpr` with any diagnostics
/// generated during the lowering process.
#[derive(Debug, Clone)]
pub struct ConditionLowerResult {
    /// The lowered condition expression
    pub expr: ConditionExpr,
    /// Diagnostics generated during lowering
    pub diagnostics: Vec<ConditionDiagnostic>,
}

impl ConditionExpr {
    /// Evaluate the condition expression to a boolean result.
    ///
    /// Returns `Some(true)` if the condition is definitely true,
    /// `Some(false)` if definitely false, or `None` if the result
    /// cannot be determined (e.g., due to an invalid expression or
    /// undefined variables).
    pub fn evaluate(&self, defined_macros: &BTreeSet<MacroName>) -> Option<bool> {
        match self {
            // Short-circuit logical operations need special handling
            ConditionExpr::Not(expr) => {
                let value = evaluate_to_value(expr, defined_macros)?;
                match value {
                    ConditionValue::Bool(b) => Some(!b),
                    // Type error: 'not' requires boolean operand
                    _ => None,
                }
            }

            ConditionExpr::AndAlso(left, right) => {
                // Short-circuit: if left is false, result is false
                let left_value = evaluate_to_value(left, defined_macros)?;
                match left_value {
                    ConditionValue::Bool(false) => Some(false),
                    ConditionValue::Bool(true) => {
                        let right_value = evaluate_to_value(right, defined_macros)?;
                        match right_value {
                            ConditionValue::Bool(b) => Some(b),
                            // Type error: 'andalso' requires boolean operand
                            _ => None,
                        }
                    }
                    // Type error: 'andalso' requires boolean operand
                    _ => None,
                }
            }

            ConditionExpr::And(left, right) => {
                // Strict 'and': always evaluate both sides
                let left_value = evaluate_to_value(left, defined_macros)?;
                let right_value = evaluate_to_value(right, defined_macros)?;
                match (left_value, right_value) {
                    (ConditionValue::Bool(l), ConditionValue::Bool(r)) => Some(l && r),
                    // Type error: 'and' requires boolean operands
                    _ => None,
                }
            }

            ConditionExpr::OrElse(left, right) => {
                // Short-circuit: if left is true, result is true
                let left_value = evaluate_to_value(left, defined_macros)?;
                match left_value {
                    ConditionValue::Bool(true) => Some(true),
                    ConditionValue::Bool(false) => {
                        let right_value = evaluate_to_value(right, defined_macros)?;
                        match right_value {
                            ConditionValue::Bool(b) => Some(b),
                            // Type error: 'orelse' requires boolean operand
                            _ => None,
                        }
                    }
                    // Type error: 'orelse' requires boolean operand
                    _ => None,
                }
            }

            ConditionExpr::Or(left, right) => {
                // Strict 'or': always evaluate both sides
                let left_value = evaluate_to_value(left, defined_macros)?;
                let right_value = evaluate_to_value(right, defined_macros)?;
                match (left_value, right_value) {
                    (ConditionValue::Bool(l), ConditionValue::Bool(r)) => Some(l || r),
                    // Type error: 'or' requires boolean operands
                    _ => None,
                }
            }

            // All other cases: evaluate to value and convert to bool
            _ => {
                let value = evaluate_to_value(self, defined_macros)?;
                value_to_bool(&value)
            }
        }
    }
}

/// Convert a condition value to a boolean for use as a condition result.
///
/// In Erlang, only `true` and `false` atoms are valid boolean values.
/// Integers, strings, and other atoms are not valid condition results.
fn value_to_bool(value: &ConditionValue) -> Option<bool> {
    match value {
        ConditionValue::Bool(b) => Some(*b),
        // Integers are type errors so falsy in Erlang conditions
        ConditionValue::Integer(_) => Some(false),
        // Strings are type errors so falsy in Erlang conditions
        ConditionValue::String(_) => Some(false),
        // Non-boolean atoms are type errors so falsy in Erlang conditions
        ConditionValue::Atom(_) => Some(false),
    }
}

/// Evaluate a sub-expression to a value (for use in comparisons and arithmetic).
fn evaluate_to_value(
    expr: &ConditionExpr,
    defined_macros: &BTreeSet<MacroName>,
) -> Option<ConditionValue> {
    match expr {
        ConditionExpr::LiteralBool(b) => Some(ConditionValue::Bool(*b)),
        ConditionExpr::Integer(i) => Some(ConditionValue::Integer(*i)),
        ConditionExpr::Atom(name) => {
            if name == &known::true_name {
                Some(ConditionValue::Bool(true))
            } else if name == &known::false_name {
                Some(ConditionValue::Bool(false))
            } else {
                Some(ConditionValue::Atom(name.clone()))
            }
        }
        ConditionExpr::String(s) => Some(ConditionValue::String(s.clone())),

        ConditionExpr::Defined(name) => {
            let macro_name = MacroName::new(name.clone(), None);
            Some(ConditionValue::Bool(
                defined_macros.contains(&macro_name)
                    || crate::macro_exp::BuiltInMacro::is_built_in_name(name),
            ))
        }

        ConditionExpr::Not(inner) => {
            let value = evaluate_to_value(inner, defined_macros)?;
            match value {
                ConditionValue::Bool(b) => Some(ConditionValue::Bool(!b)),
                // Type error: 'not' requires boolean operand
                _ => None,
            }
        }

        ConditionExpr::AndAlso(left, right) => {
            let left_value = evaluate_to_value(left, defined_macros)?;
            match left_value {
                ConditionValue::Bool(false) => Some(ConditionValue::Bool(false)),
                ConditionValue::Bool(true) => {
                    let right_value = evaluate_to_value(right, defined_macros)?;
                    match right_value {
                        ConditionValue::Bool(b) => Some(ConditionValue::Bool(b)),
                        // Type error: 'andalso' requires boolean operand
                        _ => None,
                    }
                }
                // Type error: 'andalso' requires boolean operand
                _ => None,
            }
        }

        ConditionExpr::And(left, right) => {
            let left_value = evaluate_to_value(left, defined_macros)?;
            let right_value = evaluate_to_value(right, defined_macros)?;
            match (left_value, right_value) {
                (ConditionValue::Bool(l), ConditionValue::Bool(r)) => {
                    Some(ConditionValue::Bool(l && r))
                }
                // Type error: 'and' requires boolean operands
                _ => None,
            }
        }

        ConditionExpr::OrElse(left, right) => {
            let left_value = evaluate_to_value(left, defined_macros)?;
            match left_value {
                ConditionValue::Bool(true) => Some(ConditionValue::Bool(true)),
                ConditionValue::Bool(false) => {
                    let right_value = evaluate_to_value(right, defined_macros)?;
                    match right_value {
                        ConditionValue::Bool(b) => Some(ConditionValue::Bool(b)),
                        // Type error: 'orelse' requires boolean operand
                        _ => None,
                    }
                }
                // Type error: 'orelse' requires boolean operand
                _ => None,
            }
        }

        ConditionExpr::Or(left, right) => {
            let left_value = evaluate_to_value(left, defined_macros)?;
            let right_value = evaluate_to_value(right, defined_macros)?;
            match (left_value, right_value) {
                (ConditionValue::Bool(l), ConditionValue::Bool(r)) => {
                    Some(ConditionValue::Bool(l || r))
                }
                // Type error: 'or' requires boolean operands
                _ => None,
            }
        }

        ConditionExpr::Compare { op, left, right } => {
            let left_val = evaluate_to_value(left, defined_macros)?;
            let right_val = evaluate_to_value(right, defined_macros)?;
            let result = compare_values(*op, &left_val, &right_val)?;
            Some(ConditionValue::Bool(result))
        }

        ConditionExpr::Arithmetic { op, left, right } => {
            let left_val = evaluate_to_value(left, defined_macros)?;
            let right_val = evaluate_to_value(right, defined_macros)?;

            let left_int = match left_val {
                ConditionValue::Integer(i) => i,
                _ => return None,
            };
            let right_int = match right_val {
                ConditionValue::Integer(i) => i,
                _ => return None,
            };

            let result = apply_arithmetic(*op, left_int, right_int)?;
            Some(ConditionValue::Integer(result))
        }

        ConditionExpr::UnaryArithmetic { op, operand } => {
            let val = evaluate_to_value(operand, defined_macros)?;
            let int_val = match val {
                ConditionValue::Integer(i) => i,
                _ => return None,
            };

            let result = apply_unary_arithmetic(*op, int_val)?;
            Some(ConditionValue::Integer(result))
        }

        ConditionExpr::Invalid => None,
    }
}

/// Compare two condition values using the given comparison operator.
fn compare_values(op: CompOp, left: &ConditionValue, right: &ConditionValue) -> Option<bool> {
    match op {
        CompOp::Eq { strict, negated } => {
            let eq = if strict {
                // Exact equality (=:=): types must match exactly
                values_exactly_equal(left, right)
            } else {
                // Loose equality (==): allows numeric coercion
                values_equal(left, right)
            };
            let eq = eq?;
            Some(if negated { !eq } else { eq })
        }
        CompOp::Ord { ordering, strict } => {
            let cmp = compare_values_ord(left, right)?;
            let result = match ordering {
                Ordering::Less => {
                    if strict {
                        cmp == std::cmp::Ordering::Less
                    } else {
                        cmp != std::cmp::Ordering::Greater
                    }
                }
                Ordering::Greater => {
                    if strict {
                        cmp == std::cmp::Ordering::Greater
                    } else {
                        cmp != std::cmp::Ordering::Less
                    }
                }
            };
            Some(result)
        }
    }
}

/// Check if two values are exactly equal (=:=).
fn values_exactly_equal(left: &ConditionValue, right: &ConditionValue) -> Option<bool> {
    match (left, right) {
        (ConditionValue::Bool(a), ConditionValue::Bool(b)) => Some(a == b),
        (ConditionValue::Integer(a), ConditionValue::Integer(b)) => Some(a == b),
        (ConditionValue::Atom(a), ConditionValue::Atom(b)) => Some(a == b),
        (ConditionValue::String(a), ConditionValue::String(b)) => Some(a == b),
        // Different types are never exactly equal
        _ => Some(false),
    }
}

/// Check if two values are loosely equal (==).
fn values_equal(left: &ConditionValue, right: &ConditionValue) -> Option<bool> {
    match (left, right) {
        (ConditionValue::Bool(a), ConditionValue::Bool(b)) => Some(a == b),
        (ConditionValue::Integer(a), ConditionValue::Integer(b)) => Some(a == b),
        (ConditionValue::Atom(a), ConditionValue::Atom(b)) => Some(a == b),
        (ConditionValue::String(a), ConditionValue::String(b)) => Some(a == b),
        // For now, we treat loose equality same as strict for these types
        // In Erlang, only numbers (int/float) have loose equality differences
        _ => Some(false),
    }
}

/// Compare two values for ordering.
fn compare_values_ord(left: &ConditionValue, right: &ConditionValue) -> Option<std::cmp::Ordering> {
    match (left, right) {
        (ConditionValue::Integer(a), ConditionValue::Integer(b)) => Some(a.cmp(b)),
        (ConditionValue::Atom(a), ConditionValue::Atom(b)) => Some(a.cmp(b)),
        (ConditionValue::String(a), ConditionValue::String(b)) => Some(a.cmp(b)),
        (ConditionValue::Bool(a), ConditionValue::Bool(b)) => {
            // In Erlang: false < true
            Some(a.cmp(b))
        }
        // Cross-type comparison follows Erlang term ordering:
        // number < atom < reference < fun < port < pid < tuple < map < nil < list < bitstring
        // For our purposes, we handle the common cases:
        (ConditionValue::Integer(_), ConditionValue::Atom(_)) => Some(std::cmp::Ordering::Less),
        (ConditionValue::Atom(_), ConditionValue::Integer(_)) => Some(std::cmp::Ordering::Greater),
        (ConditionValue::Integer(_), ConditionValue::Bool(_)) => {
            // Bools are atoms in Erlang, so number < atom
            Some(std::cmp::Ordering::Less)
        }
        (ConditionValue::Bool(_), ConditionValue::Integer(_)) => Some(std::cmp::Ordering::Greater),
        _ => None,
    }
}

/// Apply a binary arithmetic operation to two integers.
fn apply_arithmetic(op: ArithOp, left: i128, right: i128) -> Option<i128> {
    match op {
        ArithOp::Add => left.checked_add(right),
        ArithOp::Sub => left.checked_sub(right),
        ArithOp::Mul => left.checked_mul(right),
        ArithOp::Div => {
            if right == 0 {
                None
            } else {
                Some(left / right)
            }
        }
        ArithOp::Rem => {
            if right == 0 {
                None
            } else {
                Some(left % right)
            }
        }
        ArithOp::Band => Some(left & right),
        ArithOp::Bor => Some(left | right),
        ArithOp::Bxor => Some(left ^ right),
        ArithOp::Bsl => {
            // Shift left - limit shift amount to prevent overflow
            let shift = right.try_into().ok()?;
            if shift >= 128 {
                None
            } else {
                left.checked_shl(shift)
            }
        }
        ArithOp::Bsr => {
            // Shift right - limit shift amount
            let shift = right.try_into().ok()?;
            if shift >= 128 {
                Some(if left < 0 { -1 } else { 0 })
            } else {
                left.checked_shr(shift)
            }
        }
        ArithOp::FloatDiv => {
            // Float division - not supported in integer context
            None
        }
    }
}

/// Apply a unary arithmetic operation to an integer.
fn apply_unary_arithmetic(op: UnaryOp, operand: i128) -> Option<i128> {
    match op {
        UnaryOp::Plus => Some(operand),
        UnaryOp::Minus => operand.checked_neg(),
        UnaryOp::Bnot => Some(!operand),
        UnaryOp::Not => {
            // Logical not - not applicable to integers in this context
            None
        }
    }
}

// ============================================================================
// HIR to ConditionExpr lowering
// ============================================================================

/// Lower an AST expression to a ConditionExpr with diagnostic tracking and optional module name override.
///
/// This function creates a HIR body from the AST expression (with macro expansion),
/// then converts the HIR to a `ConditionExpr` that can be evaluated. It also tracks
/// diagnostics for unsupported constructs encountered during lowering.
///
/// The `module_name_override` allows specifying a module name to use for ?FILE, ?MODULE, etc.
/// when evaluating conditions in included header files.
pub fn lower_condition_expr(
    db: &dyn DefDatabase,
    file_id: FileId,
    cond_id: PPConditionId,
    ast_expr: &ast::Expr,
    module_name_override: Option<Name>,
) -> ConditionLowerResult {
    // Create a body with macro expansion
    let (body, _source_map, root_id) =
        lower_condition_body(db, file_id, cond_id, ast_expr, module_name_override.clone());
    let mut diagnostics = Vec::new();
    let expr = hir_to_condition_expr(db, &body, root_id, &mut diagnostics);
    ConditionLowerResult { expr, diagnostics }
}

/// Convert a HIR expression to a ConditionExpr, tracking diagnostics for unsupported constructs.
fn hir_to_condition_expr(
    db: &dyn DefDatabase,
    body: &Body,
    expr_id: ExprId,
    diagnostics: &mut Vec<ConditionDiagnostic>,
) -> ConditionExpr {
    // Access the raw expression without looking through macros/parens
    // so we can handle MacroCall and Paren explicitly
    let expr = &body.exprs[expr_id];

    match expr {
        Expr::Literal(Literal::Atom(atom)) => {
            let name_str = db.lookup_atom(*atom);
            match name_str.as_str() {
                "true" => ConditionExpr::LiteralBool(true),
                "false" => ConditionExpr::LiteralBool(false),
                _ => ConditionExpr::Atom(name_str),
            }
        }

        Expr::Literal(Literal::Integer(i)) => ConditionExpr::Integer(i.value),

        Expr::Literal(Literal::String(s)) => ConditionExpr::String(s.as_string()),

        Expr::UnaryOp { expr, op } => match op {
            UnaryOp::Not => ConditionExpr::Not(Box::new(hir_to_condition_expr(
                db,
                body,
                *expr,
                diagnostics,
            ))),
            UnaryOp::Minus | UnaryOp::Plus | UnaryOp::Bnot => ConditionExpr::UnaryArithmetic {
                op: *op,
                operand: Box::new(hir_to_condition_expr(db, body, *expr, diagnostics)),
            },
        },

        Expr::BinaryOp { lhs, rhs, op } => match op {
            BinaryOp::LogicOp(LogicOp::And { lazy: true }) => ConditionExpr::AndAlso(
                Box::new(hir_to_condition_expr(db, body, *lhs, diagnostics)),
                Box::new(hir_to_condition_expr(db, body, *rhs, diagnostics)),
            ),
            BinaryOp::LogicOp(LogicOp::Or { lazy: true }) => ConditionExpr::OrElse(
                Box::new(hir_to_condition_expr(db, body, *lhs, diagnostics)),
                Box::new(hir_to_condition_expr(db, body, *rhs, diagnostics)),
            ),
            BinaryOp::LogicOp(LogicOp::And { lazy: false }) => {
                // Non-lazy 'and' - evaluate both sides and combine
                ConditionExpr::And(
                    Box::new(hir_to_condition_expr(db, body, *lhs, diagnostics)),
                    Box::new(hir_to_condition_expr(db, body, *rhs, diagnostics)),
                )
            }
            BinaryOp::LogicOp(LogicOp::Or { lazy: false }) => {
                // Non-lazy 'or' - evaluate both sides and combine
                ConditionExpr::Or(
                    Box::new(hir_to_condition_expr(db, body, *lhs, diagnostics)),
                    Box::new(hir_to_condition_expr(db, body, *rhs, diagnostics)),
                )
            }
            BinaryOp::LogicOp(LogicOp::Xor) => {
                // xor is not directly supported, emit a diagnostic
                diagnostics.push(ConditionDiagnostic::new(
                    "the 'xor' operator is not supported in preprocessor conditions",
                ));
                ConditionExpr::Invalid
            }
            BinaryOp::CompOp(comp_op) => ConditionExpr::Compare {
                op: *comp_op,
                left: Box::new(hir_to_condition_expr(db, body, *lhs, diagnostics)),
                right: Box::new(hir_to_condition_expr(db, body, *rhs, diagnostics)),
            },
            BinaryOp::ArithOp(arith_op) => ConditionExpr::Arithmetic {
                op: *arith_op,
                left: Box::new(hir_to_condition_expr(db, body, *lhs, diagnostics)),
                right: Box::new(hir_to_condition_expr(db, body, *rhs, diagnostics)),
            },
            BinaryOp::ListOp(_) | BinaryOp::Send => {
                // List operations and send are not supported in conditions
                diagnostics.push(ConditionDiagnostic::new(
                    "list operations and send are not supported in preprocessor conditions",
                ));
                ConditionExpr::Invalid
            }
        },

        Expr::Call { target, args } => {
            if is_defined_call(db, target, body) {
                extract_defined_macro_name(db, args, body, diagnostics)
            } else {
                // Unsupported function call
                let call_name = get_call_name(db, target, body);
                diagnostics.push(ConditionDiagnostic::new(format!(
                    "function call '{}' is not supported in preprocessor conditions",
                    call_name
                )));
                ConditionExpr::Invalid
            }
        }

        Expr::MacroCall {
            expansion,
            macro_name,
            ..
        } => {
            // Check if the macro was resolved - for undefined macros, the expansion is Missing.
            // Note: built-in macros like ?FILE have macro_def=None but still have valid expansions.
            // TODO: Consider a predefined macro for built-in macros like ?FILE.
            if matches!(body.exprs[*expansion], Expr::Missing) {
                // Undefined macro
                let name = macro_name.as_name(db);
                diagnostics.push(ConditionDiagnostic::new(format!(
                    "undefined macro '{}' in preprocessor condition",
                    name
                )));
                ConditionExpr::Invalid
            } else {
                // Follow macro expansion
                hir_to_condition_expr(db, body, *expansion, diagnostics)
            }
        }

        Expr::Paren { expr } => hir_to_condition_expr(db, body, *expr, diagnostics),

        // All other expression types are not supported in conditions
        _ => {
            diagnostics.push(ConditionDiagnostic::new(
                "unsupported expression in preprocessor condition",
            ));
            ConditionExpr::Invalid
        }
    }
}

/// Get the name of a call for diagnostic messages.
fn get_call_name(db: &dyn DefDatabase, target: &CallTarget<ExprId>, body: &Body) -> String {
    match target {
        CallTarget::Local { name } => {
            if let Expr::Literal(Literal::Atom(atom)) = &body.exprs[*name] {
                return db.lookup_atom(*atom).to_string();
            }
            if let Expr::MacroCall { expansion, .. } = &body.exprs[*name]
                && let Expr::Literal(Literal::Atom(atom)) = &body.exprs[*expansion]
            {
                return db.lookup_atom(*atom).to_string();
            }
            "<unknown>".to_string()
        }
        CallTarget::Remote { module, name, .. } => {
            let module_name = if let Expr::Literal(Literal::Atom(atom)) = &body.exprs[*module] {
                db.lookup_atom(*atom).to_string()
            } else {
                "<unknown>".to_string()
            };
            let func_name = if let Expr::Literal(Literal::Atom(atom)) = &body.exprs[*name] {
                db.lookup_atom(*atom).to_string()
            } else {
                "<unknown>".to_string()
            };
            format!("{}:{}", module_name, func_name)
        }
    }
}

/// Check if a call target is a call to `defined/1`.
fn is_defined_call(db: &dyn DefDatabase, target: &CallTarget<ExprId>, body: &Body) -> bool {
    match target {
        CallTarget::Local { name } => {
            // Access the raw expression
            if let Expr::Literal(Literal::Atom(atom)) = &body.exprs[*name] {
                return db.lookup_atom(*atom) == known::defined;
            }
            // Check through macro expansion
            if let Expr::MacroCall { expansion, .. } = &body.exprs[*name]
                && let Expr::Literal(Literal::Atom(atom)) = &body.exprs[*expansion]
            {
                return db.lookup_atom(*atom) == known::defined;
            }
            false
        }
        CallTarget::Remote { .. } => false,
    }
}

/// Extract the macro name from a `defined(MACRO)` call with diagnostic tracking.
fn extract_defined_macro_name(
    db: &dyn DefDatabase,
    args: &[ExprId],
    body: &Body,
    diagnostics: &mut Vec<ConditionDiagnostic>,
) -> ConditionExpr {
    if args.len() != 1 {
        diagnostics.push(ConditionDiagnostic::new(
            "defined() requires exactly one argument",
        ));
        return ConditionExpr::Invalid;
    }
    // This corresponds to the code evaluate_builtins/2 in elp_epp.erl
    let arg = &body[args[0]];
    match arg {
        Expr::Literal(Literal::Atom(atom)) => ConditionExpr::Defined(db.lookup_atom(*atom)),
        // Handle uppercase identifiers (like FOO) which are parsed as variables
        Expr::Var(var) => ConditionExpr::Defined(db.lookup_var(*var)),
        _ => {
            diagnostics.push(ConditionDiagnostic::new(
                "defined() argument must be an atom or macro name",
            ));
            ConditionExpr::Invalid
        }
    }
}

#[cfg(test)]
mod tests {

    use std::collections::BTreeSet;

    use elp_base_db::FileId;
    use elp_base_db::FileLoader;
    use elp_base_db::SourceDatabase;
    use elp_base_db::fixture::ChangeFixture;
    use elp_base_db::fixture::WithFixture;
    use elp_syntax::TextSize;
    use elp_syntax::ast::ArithOp;
    use elp_syntax::ast::CompOp;
    use elp_syntax::ast::Ordering;
    use elp_syntax::ast::UnaryOp;
    use expect_test::Expect;
    use expect_test::expect;

    use crate::InFile;
    use crate::MacroName;
    use crate::Name;
    use crate::condition_expr::ConditionExpr;
    use crate::db::DefDatabase;
    use crate::form_list::FormIdx;
    use crate::form_list::FormList;
    use crate::form_list::FormPPContext;
    use crate::form_list::PPCondition;
    use crate::form_list::PPConditionId;
    use crate::form_list::PPConditionResult;
    use crate::test_db::TestDB;

    fn name(s: &str) -> Name {
        Name::from_erlang_service(s)
    }

    fn make_defined_set(names: &[&str]) -> BTreeSet<MacroName> {
        names
            .iter()
            .map(|s| MacroName::new(name(s), None))
            .collect()
    }

    #[test]
    fn test_defined() {
        let defined = make_defined_set(&["FOO", "BAR"]);

        let expr = ConditionExpr::Defined(name("FOO"));
        assert_eq!(expr.evaluate(&defined), Some(true));

        let expr = ConditionExpr::Defined(name("BAZ"));
        assert_eq!(expr.evaluate(&defined), Some(false));
    }

    #[test]
    fn test_not() {
        let defined = make_defined_set(&["FOO"]);

        let expr = ConditionExpr::Not(Box::new(ConditionExpr::Defined(name("FOO"))));
        assert_eq!(expr.evaluate(&defined), Some(false));

        let expr = ConditionExpr::Not(Box::new(ConditionExpr::Defined(name("BAR"))));
        assert_eq!(expr.evaluate(&defined), Some(true));

        let expr = ConditionExpr::Not(Box::new(ConditionExpr::LiteralBool(true)));
        assert_eq!(expr.evaluate(&defined), Some(false));
    }

    #[test]
    fn test_andalso() {
        let defined = make_defined_set(&["FOO"]);

        // true andalso true -> true
        let expr = ConditionExpr::AndAlso(
            Box::new(ConditionExpr::LiteralBool(true)),
            Box::new(ConditionExpr::LiteralBool(true)),
        );
        assert_eq!(expr.evaluate(&defined), Some(true));

        // true andalso false -> false
        let expr = ConditionExpr::AndAlso(
            Box::new(ConditionExpr::LiteralBool(true)),
            Box::new(ConditionExpr::LiteralBool(false)),
        );
        assert_eq!(expr.evaluate(&defined), Some(false));

        // false andalso X -> false (short-circuit)
        let expr = ConditionExpr::AndAlso(
            Box::new(ConditionExpr::LiteralBool(false)),
            Box::new(ConditionExpr::Invalid),
        );
        assert_eq!(expr.evaluate(&defined), Some(false));

        // defined(FOO) andalso defined(BAR) -> false
        let expr = ConditionExpr::AndAlso(
            Box::new(ConditionExpr::Defined(name("FOO"))),
            Box::new(ConditionExpr::Defined(name("BAR"))),
        );
        assert_eq!(expr.evaluate(&defined), Some(false));
    }

    #[test]
    fn test_orelse() {
        let defined = make_defined_set(&["FOO"]);

        // false orelse false -> false
        let expr = ConditionExpr::OrElse(
            Box::new(ConditionExpr::LiteralBool(false)),
            Box::new(ConditionExpr::LiteralBool(false)),
        );
        assert_eq!(expr.evaluate(&defined), Some(false));

        // false orelse true -> true
        let expr = ConditionExpr::OrElse(
            Box::new(ConditionExpr::LiteralBool(false)),
            Box::new(ConditionExpr::LiteralBool(true)),
        );
        assert_eq!(expr.evaluate(&defined), Some(true));

        // true orelse X -> true (short-circuit)
        let expr = ConditionExpr::OrElse(
            Box::new(ConditionExpr::LiteralBool(true)),
            Box::new(ConditionExpr::Invalid),
        );
        assert_eq!(expr.evaluate(&defined), Some(true));

        // defined(FOO) orelse defined(BAR) -> true
        let expr = ConditionExpr::OrElse(
            Box::new(ConditionExpr::Defined(name("FOO"))),
            Box::new(ConditionExpr::Defined(name("BAR"))),
        );
        assert_eq!(expr.evaluate(&defined), Some(true));
    }

    #[test]
    fn test_and() {
        let defined = make_defined_set(&["FOO"]);

        // true and true -> true
        let expr = ConditionExpr::And(
            Box::new(ConditionExpr::LiteralBool(true)),
            Box::new(ConditionExpr::LiteralBool(true)),
        );
        assert_eq!(expr.evaluate(&defined), Some(true));

        // true and false -> false
        let expr = ConditionExpr::And(
            Box::new(ConditionExpr::LiteralBool(true)),
            Box::new(ConditionExpr::LiteralBool(false)),
        );
        assert_eq!(expr.evaluate(&defined), Some(false));

        // false and Invalid -> None (both sides evaluated, right fails)
        let expr = ConditionExpr::And(
            Box::new(ConditionExpr::LiteralBool(false)),
            Box::new(ConditionExpr::Invalid),
        );
        assert_eq!(expr.evaluate(&defined), None);

        // defined(FOO) and defined(BAR) -> false
        let expr = ConditionExpr::And(
            Box::new(ConditionExpr::Defined(name("FOO"))),
            Box::new(ConditionExpr::Defined(name("BAR"))),
        );
        assert_eq!(expr.evaluate(&defined), Some(false));
    }

    #[test]
    fn test_or() {
        let defined = make_defined_set(&["FOO"]);

        // false or false -> false
        let expr = ConditionExpr::Or(
            Box::new(ConditionExpr::LiteralBool(false)),
            Box::new(ConditionExpr::LiteralBool(false)),
        );
        assert_eq!(expr.evaluate(&defined), Some(false));

        // false or true -> true
        let expr = ConditionExpr::Or(
            Box::new(ConditionExpr::LiteralBool(false)),
            Box::new(ConditionExpr::LiteralBool(true)),
        );
        assert_eq!(expr.evaluate(&defined), Some(true));

        // true or Invalid -> None (both sides evaluated, right fails)
        let expr = ConditionExpr::Or(
            Box::new(ConditionExpr::LiteralBool(true)),
            Box::new(ConditionExpr::Invalid),
        );
        assert_eq!(expr.evaluate(&defined), None);

        // defined(FOO) or defined(BAR) -> true
        let expr = ConditionExpr::Or(
            Box::new(ConditionExpr::Defined(name("FOO"))),
            Box::new(ConditionExpr::Defined(name("BAR"))),
        );
        assert_eq!(expr.evaluate(&defined), Some(true));
    }

    #[test]
    fn test_compare_integers() {
        let defined = BTreeSet::default();

        // 25 >= 25 -> true
        let expr = ConditionExpr::Compare {
            op: CompOp::Ord {
                ordering: Ordering::Greater,
                strict: false,
            },
            left: Box::new(ConditionExpr::Integer(25)),
            right: Box::new(ConditionExpr::Integer(25)),
        };
        assert_eq!(expr.evaluate(&defined), Some(true));

        // 25 > 25 -> false
        let expr = ConditionExpr::Compare {
            op: CompOp::Ord {
                ordering: Ordering::Greater,
                strict: true,
            },
            left: Box::new(ConditionExpr::Integer(25)),
            right: Box::new(ConditionExpr::Integer(25)),
        };
        assert_eq!(expr.evaluate(&defined), Some(false));

        // 26 > 25 -> true
        let expr = ConditionExpr::Compare {
            op: CompOp::Ord {
                ordering: Ordering::Greater,
                strict: true,
            },
            left: Box::new(ConditionExpr::Integer(26)),
            right: Box::new(ConditionExpr::Integer(25)),
        };
        assert_eq!(expr.evaluate(&defined), Some(true));

        // 24 < 25 -> true
        let expr = ConditionExpr::Compare {
            op: CompOp::Ord {
                ordering: Ordering::Less,
                strict: true,
            },
            left: Box::new(ConditionExpr::Integer(24)),
            right: Box::new(ConditionExpr::Integer(25)),
        };
        assert_eq!(expr.evaluate(&defined), Some(true));
    }

    #[test]
    fn test_compare_equality() {
        let defined = BTreeSet::default();

        // 25 == 25 -> true
        let expr = ConditionExpr::Compare {
            op: CompOp::Eq {
                strict: false,
                negated: false,
            },
            left: Box::new(ConditionExpr::Integer(25)),
            right: Box::new(ConditionExpr::Integer(25)),
        };
        assert_eq!(expr.evaluate(&defined), Some(true));

        // 25 /= 26 -> true
        let expr = ConditionExpr::Compare {
            op: CompOp::Eq {
                strict: false,
                negated: true,
            },
            left: Box::new(ConditionExpr::Integer(25)),
            right: Box::new(ConditionExpr::Integer(26)),
        };
        assert_eq!(expr.evaluate(&defined), Some(true));

        // foo =:= foo -> true
        let expr = ConditionExpr::Compare {
            op: CompOp::Eq {
                strict: true,
                negated: false,
            },
            left: Box::new(ConditionExpr::Atom(name("foo"))),
            right: Box::new(ConditionExpr::Atom(name("foo"))),
        };
        assert_eq!(expr.evaluate(&defined), Some(true));

        // foo =/= bar -> true
        let expr = ConditionExpr::Compare {
            op: CompOp::Eq {
                strict: true,
                negated: true,
            },
            left: Box::new(ConditionExpr::Atom(name("foo"))),
            right: Box::new(ConditionExpr::Atom(name("bar"))),
        };
        assert_eq!(expr.evaluate(&defined), Some(true));
    }

    #[test]
    fn test_arithmetic() {
        let defined = BTreeSet::default();

        // 10 + 5 = 15, integer result is falsy
        let expr = ConditionExpr::Arithmetic {
            op: ArithOp::Add,
            left: Box::new(ConditionExpr::Integer(10)),
            right: Box::new(ConditionExpr::Integer(5)),
        };
        assert_eq!(expr.evaluate(&defined), Some(false));

        // 5 - 5 = 0, which is falsy
        let expr = ConditionExpr::Arithmetic {
            op: ArithOp::Sub,
            left: Box::new(ConditionExpr::Integer(5)),
            right: Box::new(ConditionExpr::Integer(5)),
        };
        assert_eq!(expr.evaluate(&defined), Some(false));

        // 10 div 3 = 3, integer result is falsy
        let expr = ConditionExpr::Arithmetic {
            op: ArithOp::Div,
            left: Box::new(ConditionExpr::Integer(10)),
            right: Box::new(ConditionExpr::Integer(3)),
        };
        assert_eq!(expr.evaluate(&defined), Some(false));

        // Division by zero returns None
        let expr = ConditionExpr::Arithmetic {
            op: ArithOp::Div,
            left: Box::new(ConditionExpr::Integer(10)),
            right: Box::new(ConditionExpr::Integer(0)),
        };
        assert_eq!(expr.evaluate(&defined), None);

        // Bitwise operations
        let expr = ConditionExpr::Arithmetic {
            op: ArithOp::Band,
            left: Box::new(ConditionExpr::Integer(0b1100)),
            right: Box::new(ConditionExpr::Integer(0b1010)),
        };
        // 0b1100 band 0b1010 = 0b1000 = 8, integer result is falsy
        assert_eq!(expr.evaluate(&defined), Some(false));
    }

    #[test]
    fn test_unary_arithmetic() {
        // All numerical values are falsy
        let defined = BTreeSet::default();

        // -5 is falsy
        let expr = ConditionExpr::UnaryArithmetic {
            op: UnaryOp::Minus,
            operand: Box::new(ConditionExpr::Integer(5)),
        };
        assert_eq!(expr.evaluate(&defined), Some(false));

        // -0 = 0 is falsy
        let expr = ConditionExpr::UnaryArithmetic {
            op: UnaryOp::Minus,
            operand: Box::new(ConditionExpr::Integer(0)),
        };
        assert_eq!(expr.evaluate(&defined), Some(false));

        // +5 is falsy
        let expr = ConditionExpr::UnaryArithmetic {
            op: UnaryOp::Plus,
            operand: Box::new(ConditionExpr::Integer(5)),
        };
        assert_eq!(expr.evaluate(&defined), Some(false));

        // bnot 0 = -1, falsy
        let expr = ConditionExpr::UnaryArithmetic {
            op: UnaryOp::Bnot,
            operand: Box::new(ConditionExpr::Integer(0)),
        };
        assert_eq!(expr.evaluate(&defined), Some(false));
    }

    #[test]
    fn test_invalid() {
        let defined = BTreeSet::default();

        let expr = ConditionExpr::Invalid;
        assert_eq!(expr.evaluate(&defined), None);
    }

    #[test]
    fn test_complex_expression() {
        // Test: defined(DEBUG) orelse (?OTP_RELEASE >= 25)
        // With DEBUG not defined and OTP_RELEASE = 26
        let defined = BTreeSet::default();

        let expr = ConditionExpr::OrElse(
            Box::new(ConditionExpr::Defined(name("DEBUG"))),
            Box::new(ConditionExpr::Compare {
                op: CompOp::Ord {
                    ordering: Ordering::Greater,
                    strict: false,
                },
                left: Box::new(ConditionExpr::Integer(26)),
                right: Box::new(ConditionExpr::Integer(25)),
            }),
        );
        assert_eq!(expr.evaluate(&defined), Some(true));
    }

    #[test]
    fn test_string_comparison() {
        let defined = BTreeSet::default();

        // "abc" == "abc" -> true
        let expr = ConditionExpr::Compare {
            op: CompOp::Eq {
                strict: false,
                negated: false,
            },
            left: Box::new(ConditionExpr::String("abc".to_string())),
            right: Box::new(ConditionExpr::String("abc".to_string())),
        };
        assert_eq!(expr.evaluate(&defined), Some(true));

        // "abc" < "abd" -> true
        let expr = ConditionExpr::Compare {
            op: CompOp::Ord {
                ordering: Ordering::Less,
                strict: true,
            },
            left: Box::new(ConditionExpr::String("abc".to_string())),
            right: Box::new(ConditionExpr::String("abd".to_string())),
        };
        assert_eq!(expr.evaluate(&defined), Some(true));
    }

    /// These tests use a declarative format to test preprocessor condition
    /// evaluation. The output format shows:
    /// - `+|` for active (included) lines
    /// - `-|` for inactive (excluded) lines
    /// - `?|` for unknown (cannot be determined) lines
    /// - `>>> reason` when entering an inactive/unknown block
    /// - `>>> defined: [...]` showing final macro definitions
    ///
    /// Line status for rendering: Active, Inactive, or Unknown
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum LineStatus {
        Active,
        Inactive,
        Unknown,
    }

    /// Collect all active defined macros from define_attributes plus external defines.
    /// This walks through all defines and checks if they are active, including
    /// defines from included files.
    ///
    /// Note: For header files with conditions like `-if(?FILE == "mod1.erl")`, we pass
    /// the including file's module name so that ?FILE and ?MODULE resolve correctly.
    fn collect_all_defines(
        db: &TestDB,
        file_id: elp_base_db::FileId,
        form_list: &FormList,
    ) -> Vec<String> {
        collect_all_defines_with_override(db, file_id, form_list, None)
    }

    /// Same as `collect_all_defines` but with an optional module name override.
    fn collect_all_defines_with_override(
        db: &TestDB,
        file_id: elp_base_db::FileId,
        form_list: &FormList,
        module_name_override: Option<Name>,
    ) -> Vec<String> {
        let mut defines = Vec::new();
        // Add external defines (from fixture macros:[])
        for name in db.file_external_defines(file_id).iter() {
            defines.push(name.to_string());
        }
        // Add internal defines from this file that are active or unknown
        // (Unknown means we can't evaluate the condition, typically because
        // of missing context like ?FILE or ?MODULE in headers)
        for (_, define) in form_list.define_attributes() {
            let result =
                form_list.is_form_active(db, file_id, &define.pp_ctx, module_name_override.clone());
            if result != PPConditionResult::Inactive {
                defines.push(define.name.to_string());
            }
        }
        // Determine the module name for included files
        // If we don't have an override, try to get it from this file's module attribute
        let module_name_for_includes = module_name_override.clone().or_else(|| {
            form_list
                .module_attribute()
                .map(|mod_attr| mod_attr.name.clone())
        });
        // Collect defines from included files
        for (idx, _include) in form_list.includes() {
            if let Some(included_file_id) =
                db.resolve_include(db.app_data_id_by_file(file_id), InFile::new(file_id, idx))
                && included_file_id != file_id
            {
                // Guard against cycles
                let included_form_list = db.file_form_list(included_file_id);
                let included_defines = collect_all_defines_with_override(
                    db,
                    included_file_id,
                    &included_form_list,
                    module_name_for_includes.clone(),
                );
                defines.extend(included_defines);
            }
        }
        defines.sort();
        defines.dedup();
        defines
    }

    /// Compute line start offsets for the given source text.
    fn line_starts(source: &str) -> Vec<TextSize> {
        let mut starts = vec![TextSize::from(0)];
        for (offset, ch) in source.char_indices() {
            if ch == '\n' {
                starts.push(TextSize::from((offset + 1) as u32));
            }
        }
        starts
    }

    /// Convert a text offset to a 0-indexed line number.
    fn offset_to_line(offset: TextSize, line_starts: &[TextSize]) -> usize {
        match line_starts.binary_search(&offset) {
            Ok(line) => line,
            Err(line) => line.saturating_sub(1),
        }
    }

    /// Get the pp_ctx for a form (extracting from each variant).
    fn get_form_pp_ctx(form_list: &FormList, form_idx: FormIdx) -> Option<&FormPPContext> {
        form_list.get(form_idx).pp_ctx(form_list)
    }

    /// Generate the reason string for why a condition is inactive or unknown.
    fn get_condition_reason(
        db: &TestDB,
        file_id: FileId,
        form_list: &FormList,
        cond_id: PPConditionId,
        result: PPConditionResult,
    ) -> String {
        match result {
            PPConditionResult::Active => String::new(),
            PPConditionResult::Inactive => match &form_list[cond_id] {
                PPCondition::Ifdef { name, .. } => format!("{} is not defined", name),
                PPCondition::Ifndef { name, .. } => format!("{} is defined", name),
                PPCondition::If { .. } => "-if condition is false".to_string(),
                PPCondition::Elif { prev, .. } => {
                    // Check if previous branch was taken
                    if was_previous_branch_taken(db, file_id, form_list, *prev) {
                        "previous branch was taken".to_string()
                    } else {
                        "-elif condition is false".to_string()
                    }
                }
                PPCondition::Else { .. } => "previous branch was taken".to_string(),
                PPCondition::Endif { .. } => "endif".to_string(),
            },
            PPConditionResult::Unknown => match &form_list[cond_id] {
                PPCondition::If { .. } => "-if condition is unknown".to_string(),
                PPCondition::Elif { prev, .. } => {
                    // Check if previous branch was taken
                    if was_previous_branch_taken(db, file_id, form_list, *prev) {
                        "previous branch was taken".to_string()
                    } else {
                        "-elif condition is unknown".to_string()
                    }
                }
                _ => "condition is unknown".to_string(),
            },
        }
    }

    /// Check if any previous branch in the conditional chain was taken (active).
    /// This checks if the CONTENT inside the branch would be active.
    fn was_previous_branch_taken(
        db: &TestDB,
        file_id: FileId,
        form_list: &FormList,
        cond_id: PPConditionId,
    ) -> bool {
        match &form_list[cond_id] {
            PPCondition::If { pp_ctx, .. }
            | PPCondition::Ifdef { pp_ctx, .. }
            | PPCondition::Ifndef { pp_ctx, .. } => {
                // To check if the branch was taken, we need to check if content INSIDE
                // this block would be active. That means creating a pp_ctx with this
                // condition as the enclosing condition.
                let content_pp_ctx = FormPPContext {
                    condition: Some(cond_id),
                    env: pp_ctx.env,
                };
                form_list.is_form_active(db, file_id, &content_pp_ctx, None)
                    == PPConditionResult::Active
            }
            PPCondition::Elif { prev, .. } => {
                // For elif, we need to check the whole chain before it
                // Recursively check the previous condition
                was_previous_branch_taken(db, file_id, form_list, *prev)
            }
            PPCondition::Else { .. } | PPCondition::Endif { .. } => false,
        }
    }

    /// Get the text range for a form index.
    fn get_form_range(
        db: &TestDB,
        file_id: FileId,
        form_list: &FormList,
        form_idx: FormIdx,
    ) -> elp_syntax::TextRange {
        form_list.form_range(form_idx, db, file_id)
    }

    /// Render the preprocessor state as annotated source.
    ///
    /// Format:
    /// - Active lines:   `+| source line`
    /// - Inactive lines: `-| source line`
    /// - Unknown lines:  `?| source line`
    /// - Reason annotation at start of inactive/unknown block: `>>> reason`
    pub fn render_preprocessor_state(db: &TestDB, file_id: FileId) -> String {
        let source = db.file_text(file_id);
        let form_list = db.file_form_list(file_id);
        let line_starts_vec = line_starts(&source);
        let lines: Vec<&str> = source.lines().collect();

        // Build a map: line_number -> (status, reason)
        // Default: all lines are active
        let mut line_status: Vec<(LineStatus, Option<String>)> =
            vec![(LineStatus::Active, None); lines.len()];

        // Process each form to mark lines as inactive or unknown
        for &form_idx in form_list.forms() {
            if let Some(pp_ctx) = get_form_pp_ctx(&form_list, form_idx) {
                let result = form_list.is_form_active(db, file_id, pp_ctx, None);
                if result != PPConditionResult::Active {
                    let range = get_form_range(db, file_id, &form_list, form_idx);
                    let start_line = offset_to_line(range.start(), &line_starts_vec);
                    let end_line = offset_to_line(range.end(), &line_starts_vec);

                    // Get the reason from the condition
                    let reason = pp_ctx.condition.map(|cond_id| {
                        get_condition_reason(db, file_id, &form_list, cond_id, result)
                    });

                    let status = match result {
                        PPConditionResult::Inactive => LineStatus::Inactive,
                        PPConditionResult::Unknown => LineStatus::Unknown,
                        PPConditionResult::Active => LineStatus::Active,
                    };

                    // Mark all lines covered by this form
                    for item in line_status
                        .iter_mut()
                        .take(end_line.min(lines.len().saturating_sub(1)) + 1)
                        .skip(start_line)
                    {
                        *item = (status, reason.clone());
                    }
                }
            }
        }

        // Render with prefixes
        let mut result = String::new();
        let mut prev_status = LineStatus::Active;

        for (i, line) in lines.iter().enumerate() {
            let (status, ref reason) = line_status[i];

            // Insert reason annotation at the start of an inactive/unknown block
            if status != LineStatus::Active
                && prev_status == LineStatus::Active
                && let Some(reason_text) = reason
            {
                result.push_str(&format!(">>> {}\n", reason_text));
            }

            // Render the line with appropriate prefix
            let prefix = match status {
                LineStatus::Active => "+|",
                LineStatus::Inactive => "-|",
                LineStatus::Unknown => "?|",
            };
            if line.is_empty() {
                result.push_str(&format!("{}\n", prefix));
            } else {
                result.push_str(&format!("{} {}\n", prefix, line));
            }

            prev_status = status;
        }

        // Append defined macros annotation at the end
        let defined_macros = collect_all_defines(db, file_id, &form_list);
        result.push_str(&format!(">>> defined: [{}]\n", defined_macros.join(", ")));

        result
    }

    /// Check preprocessor analysis with declarative expected output.
    /// Macros are specified in the fixture metadata using `macros:[A,B]` syntax.
    fn check_preprocessor(fixture: &str, expect: Expect) {
        let (db, files, _) = TestDB::with_many_files(fixture);
        let file_id = files[0];
        let rendered = render_preprocessor_state(&db, file_id);
        expect.assert_eq(&rendered);
    }

    /// Check preprocessor analysis with includes.
    /// This is similar to `check_preprocessor` but uses `with_fixture` to get
    /// access to annotations for future diagnostic verification.
    fn check_preprocessor_with_diagnostics(fixture: &str, expect: Expect) {
        let (db, fixture_data): (TestDB, ChangeFixture) = TestDB::with_fixture(fixture);
        let file_id = fixture_data.files[0];
        let rendered = render_preprocessor_state(&db, file_id);
        expect.assert_eq(&rendered);
    }

    #[test]
    fn test_pp_inactive_reason_ifdef() {
        // The reason should mention DEBUG in the annotation
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-ifdef(DEBUG).
debug_only() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -ifdef(DEBUG).
                >>> DEBUG is not defined
                -| debug_only() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn test_pp_inactive_reason_ifndef() {
        // With FEATURE defined, -ifndef(FEATURE) block is inactive
        check_preprocessor(
            r#"
//- /src/test.erl macros:[FEATURE]
-module(test).
-ifndef(FEATURE).
no_feature() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -ifndef(FEATURE).
                >>> FEATURE is defined
                -| no_feature() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE, FEATURE]
            "#]],
        );
    }

    #[test]
    fn test_pp_define_affects_ifdef() {
        // The -define makes FEATURE available for subsequent -ifdef
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-define(FEATURE, ok).
-ifdef(FEATURE).
feature_enabled() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -define(FEATURE, ok).
                +| -ifdef(FEATURE).
                +| feature_enabled() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE, FEATURE]
            "#]],
        );
    }

    #[test]
    fn test_pp_ifdef_else_release_path() {
        // With DEBUG defined, the else branch is inactive
        check_preprocessor(
            r#"
//- /src/test.erl macros:[DEBUG]
-module(test).
-ifdef(DEBUG).
debug_foo() -> ok.
-else.
release_foo() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -ifdef(DEBUG).
                +| debug_foo() -> ok.
                +| -else.
                >>> previous branch was taken
                -| release_foo() -> ok.
                +| -endif.
                >>> defined: [DEBUG, ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn test_pp_inactive_forms_collection() {
        // Without DEBUG: debug_fn is inactive, release_fn and always_active are active
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-ifdef(DEBUG).
debug_fn() -> ok.
-else.
release_fn() -> ok.
-endif.
always_active() -> ok.
"#,
            expect![[r#"
                +| -module(test).
                +| -ifdef(DEBUG).
                >>> DEBUG is not defined
                -| debug_fn() -> ok.
                +| -else.
                +| release_fn() -> ok.
                +| -endif.
                +| always_active() -> ok.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    // =========================================================================
    // Guard semantics tests - type errors
    // =========================================================================
    //
    // In Erlang guards, operators like 'not', 'andalso', and 'orelse' require
    // boolean operands. Using a non-boolean is a type error that causes the
    // entire guard to fail (returning None).

    #[test]
    fn test_not_requires_boolean() {
        let defined = BTreeSet::default();

        // not("string") -> None (type error)
        let expr = ConditionExpr::Not(Box::new(ConditionExpr::String("hello".to_string())));
        assert_eq!(expr.evaluate(&defined), None);

        // not(42) -> None (type error)
        let expr = ConditionExpr::Not(Box::new(ConditionExpr::Integer(42)));
        assert_eq!(expr.evaluate(&defined), None);

        // not(some_atom) -> None (type error)
        let expr = ConditionExpr::Not(Box::new(ConditionExpr::Atom(name("foo"))));
        assert_eq!(expr.evaluate(&defined), None);

        // not(true) -> Some(false) (valid)
        let expr = ConditionExpr::Not(Box::new(ConditionExpr::LiteralBool(true)));
        assert_eq!(expr.evaluate(&defined), Some(false));

        // not(false) -> Some(true) (valid)
        let expr = ConditionExpr::Not(Box::new(ConditionExpr::LiteralBool(false)));
        assert_eq!(expr.evaluate(&defined), Some(true));
    }

    #[test]
    fn test_type_error_propagates_through_comparison() {
        let defined = BTreeSet::default();

        // not("string") == false -> None (type error propagates)
        let expr = ConditionExpr::Compare {
            op: CompOp::Eq {
                strict: false,
                negated: false,
            },
            left: Box::new(ConditionExpr::Not(Box::new(ConditionExpr::String(
                "hello".to_string(),
            )))),
            right: Box::new(ConditionExpr::LiteralBool(false)),
        };
        assert_eq!(expr.evaluate(&defined), None);

        // not("string") == true -> None (type error propagates)
        let expr = ConditionExpr::Compare {
            op: CompOp::Eq {
                strict: false,
                negated: false,
            },
            left: Box::new(ConditionExpr::Not(Box::new(ConditionExpr::String(
                "hello".to_string(),
            )))),
            right: Box::new(ConditionExpr::LiteralBool(true)),
        };
        assert_eq!(expr.evaluate(&defined), None);
    }

    #[test]
    fn test_andalso_requires_boolean() {
        let defined = BTreeSet::default();

        // "string" andalso true -> None (type error on left)
        let expr = ConditionExpr::AndAlso(
            Box::new(ConditionExpr::String("hello".to_string())),
            Box::new(ConditionExpr::LiteralBool(true)),
        );
        assert_eq!(expr.evaluate(&defined), None);

        // true andalso "string" -> None (type error on right)
        let expr = ConditionExpr::AndAlso(
            Box::new(ConditionExpr::LiteralBool(true)),
            Box::new(ConditionExpr::String("hello".to_string())),
        );
        assert_eq!(expr.evaluate(&defined), None);

        // false andalso "string" -> Some(false) (short-circuit, right not evaluated)
        let expr = ConditionExpr::AndAlso(
            Box::new(ConditionExpr::LiteralBool(false)),
            Box::new(ConditionExpr::String("hello".to_string())),
        );
        assert_eq!(expr.evaluate(&defined), Some(false));

        // true andalso true -> Some(true) (valid)
        let expr = ConditionExpr::AndAlso(
            Box::new(ConditionExpr::LiteralBool(true)),
            Box::new(ConditionExpr::LiteralBool(true)),
        );
        assert_eq!(expr.evaluate(&defined), Some(true));

        // true andalso false -> Some(false) (valid)
        let expr = ConditionExpr::AndAlso(
            Box::new(ConditionExpr::LiteralBool(true)),
            Box::new(ConditionExpr::LiteralBool(false)),
        );
        assert_eq!(expr.evaluate(&defined), Some(false));
    }

    #[test]
    fn test_orelse_requires_boolean() {
        let defined = BTreeSet::default();

        // "string" orelse false -> None (type error on left)
        let expr = ConditionExpr::OrElse(
            Box::new(ConditionExpr::String("hello".to_string())),
            Box::new(ConditionExpr::LiteralBool(false)),
        );
        assert_eq!(expr.evaluate(&defined), None);

        // false orelse "string" -> None (type error on right)
        let expr = ConditionExpr::OrElse(
            Box::new(ConditionExpr::LiteralBool(false)),
            Box::new(ConditionExpr::String("hello".to_string())),
        );
        assert_eq!(expr.evaluate(&defined), None);

        // true orelse "string" -> Some(true) (short-circuit, right not evaluated)
        let expr = ConditionExpr::OrElse(
            Box::new(ConditionExpr::LiteralBool(true)),
            Box::new(ConditionExpr::String("hello".to_string())),
        );
        assert_eq!(expr.evaluate(&defined), Some(true));

        // false orelse true -> Some(true) (valid)
        let expr = ConditionExpr::OrElse(
            Box::new(ConditionExpr::LiteralBool(false)),
            Box::new(ConditionExpr::LiteralBool(true)),
        );
        assert_eq!(expr.evaluate(&defined), Some(true));

        // false orelse false -> Some(false) (valid)
        let expr = ConditionExpr::OrElse(
            Box::new(ConditionExpr::LiteralBool(false)),
            Box::new(ConditionExpr::LiteralBool(false)),
        );
        assert_eq!(expr.evaluate(&defined), Some(false));
    }

    #[test]
    fn test_and_requires_boolean() {
        let defined = BTreeSet::default();

        // "string" and true -> None (type error on left)
        let expr = ConditionExpr::And(
            Box::new(ConditionExpr::String("hello".to_string())),
            Box::new(ConditionExpr::LiteralBool(true)),
        );
        assert_eq!(expr.evaluate(&defined), None);

        // true and "string" -> None (type error on right)
        let expr = ConditionExpr::And(
            Box::new(ConditionExpr::LiteralBool(true)),
            Box::new(ConditionExpr::String("hello".to_string())),
        );
        assert_eq!(expr.evaluate(&defined), None);

        // false and "string" -> None (no short-circuit, both evaluated)
        let expr = ConditionExpr::And(
            Box::new(ConditionExpr::LiteralBool(false)),
            Box::new(ConditionExpr::String("hello".to_string())),
        );
        assert_eq!(expr.evaluate(&defined), None);
    }

    #[test]
    fn test_or_requires_boolean() {
        let defined = BTreeSet::default();

        // "string" or false -> None (type error on left)
        let expr = ConditionExpr::Or(
            Box::new(ConditionExpr::String("hello".to_string())),
            Box::new(ConditionExpr::LiteralBool(false)),
        );
        assert_eq!(expr.evaluate(&defined), None);

        // false or "string" -> None (type error on right)
        let expr = ConditionExpr::Or(
            Box::new(ConditionExpr::LiteralBool(false)),
            Box::new(ConditionExpr::String("hello".to_string())),
        );
        assert_eq!(expr.evaluate(&defined), None);

        // true or "string" -> None (no short-circuit, both evaluated)
        let expr = ConditionExpr::Or(
            Box::new(ConditionExpr::LiteralBool(true)),
            Box::new(ConditionExpr::String("hello".to_string())),
        );
        assert_eq!(expr.evaluate(&defined), None);
    }

    // =========================================================================
    // Tests for strict boolean lowering from hir::Expr to ConditionExpr
    // =========================================================================

    /// Helper to lower an expression from source and return the ConditionExpr.
    fn lower_expr_from_source(source: &str) -> ConditionExpr {
        let fixture = format!(
            r#"
//- /src/test.erl
-module(test).
-if({}).
foo() -> ok.
-endif.
"#,
            source
        );
        let (db, files, _) = TestDB::with_many_files(&fixture);
        let file_id = files[0];
        let form_list = db.file_form_list(file_id);

        // Find the -if condition and get its expression
        for &form_idx in form_list.forms() {
            if let FormIdx::PPCondition(cond_id) = form_idx
                && let PPCondition::If { form_id, .. } = &form_list[cond_id]
            {
                let pp_if = form_id.get_ast(&db, file_id);
                if let Some(expr) = pp_if.cond() {
                    let result = crate::condition_expr::lower_condition_expr(
                        &db, file_id, cond_id, &expr, None,
                    );
                    return result.expr;
                }
            }
        }
        panic!("Could not find -if condition in source");
    }

    /// Helper to check if a ConditionExpr is And variant (not AndAlso)
    fn is_and_variant(expr: &ConditionExpr) -> bool {
        matches!(expr, ConditionExpr::And(_, _))
    }

    /// Helper to check if a ConditionExpr is Or variant (not OrElse)
    fn is_or_variant(expr: &ConditionExpr) -> bool {
        matches!(expr, ConditionExpr::Or(_, _))
    }

    /// Helper to check if a ConditionExpr is AndAlso variant
    fn is_andalso_variant(expr: &ConditionExpr) -> bool {
        matches!(expr, ConditionExpr::AndAlso(_, _))
    }

    /// Helper to check if a ConditionExpr is OrElse variant
    fn is_orelse_variant(expr: &ConditionExpr) -> bool {
        matches!(expr, ConditionExpr::OrElse(_, _))
    }

    #[test]
    fn test_lower_strict_and() {
        // `and` (strict) should lower to ConditionExpr::And
        let expr = lower_expr_from_source("true and false");
        assert!(
            is_and_variant(&expr),
            "Expected And variant, got {:?}",
            expr
        );
    }

    #[test]
    fn test_lower_strict_or() {
        // `or` (strict) should lower to ConditionExpr::Or
        let expr = lower_expr_from_source("true or false");
        assert!(is_or_variant(&expr), "Expected Or variant, got {:?}", expr);
    }

    #[test]
    fn test_lower_lazy_andalso() {
        // `andalso` (lazy) should lower to ConditionExpr::AndAlso
        let expr = lower_expr_from_source("true andalso false");
        assert!(
            is_andalso_variant(&expr),
            "Expected AndAlso variant, got {:?}",
            expr
        );
    }

    #[test]
    fn test_lower_lazy_orelse() {
        // `orelse` (lazy) should lower to ConditionExpr::OrElse
        let expr = lower_expr_from_source("true orelse false");
        assert!(
            is_orelse_variant(&expr),
            "Expected OrElse variant, got {:?}",
            expr
        );
    }

    #[test]
    fn test_lower_strict_and_evaluates_both_sides() {
        // Strict 'and' should evaluate both sides and fail if right side is invalid
        let defined = make_defined_set(&["A"]);

        // Lower `defined(A) and defined(B)` - should be And variant
        let expr = lower_expr_from_source("defined(A) and defined(B)");
        assert!(is_and_variant(&expr), "Expected And variant");

        // Evaluate: true and false = false
        assert_eq!(expr.evaluate(&defined), Some(false));
    }

    #[test]
    fn test_lower_strict_or_evaluates_both_sides() {
        // Strict 'or' should evaluate both sides
        let defined = make_defined_set(&["A"]);

        // Lower `defined(A) or defined(B)` - should be Or variant
        let expr = lower_expr_from_source("defined(A) or defined(B)");
        assert!(is_or_variant(&expr), "Expected Or variant");

        // Evaluate: true or false = true
        assert_eq!(expr.evaluate(&defined), Some(true));
    }

    #[test]
    fn test_lower_lazy_andalso_short_circuits() {
        // Lazy 'andalso' should short-circuit when left is false
        let defined = BTreeSet::default();

        // Lower `false andalso invalid_expr` - should be AndAlso variant
        let expr = lower_expr_from_source("false andalso true");
        assert!(is_andalso_variant(&expr), "Expected AndAlso variant");

        // Evaluate: false andalso anything = false (short-circuit)
        assert_eq!(expr.evaluate(&defined), Some(false));
    }

    #[test]
    fn test_lower_lazy_orelse_short_circuits() {
        // Lazy 'orelse' should short-circuit when left is true
        let defined = BTreeSet::default();

        // Lower `true orelse invalid_expr` - should be OrElse variant
        let expr = lower_expr_from_source("true orelse false");
        assert!(is_orelse_variant(&expr), "Expected OrElse variant");

        // Evaluate: true orelse anything = true (short-circuit)
        assert_eq!(expr.evaluate(&defined), Some(true));
    }

    // =========================================================================
    // -if Expression Evaluation Tests (with db access)
    // =========================================================================

    #[test]
    fn if_true_is_active() {
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-if(true).
in_if_true() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(true).
                +| in_if_true() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn if_false_is_inactive() {
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-if(false).
in_if_false() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(false).
                >>> -if condition is false
                -| in_if_false() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn if_defined_macro_active() {
        check_preprocessor(
            r#"
//- /src/test.erl
-define(FOO, 1).
-if(defined(FOO)).
in_if_defined() -> ok.
-endif.
"#,
            expect![[r#"
                +| -define(FOO, 1).
                +| -if(defined(FOO)).
                +| in_if_defined() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE, FOO]
            "#]],
        );
    }

    #[test]
    fn if_defined_macro_inactive() {
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-if(defined(FOO)).
in_if_defined() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(defined(FOO)).
                >>> -if condition is false
                -| in_if_defined() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn if_comparison_active() {
        check_preprocessor(
            r#"
//- /src/test.erl
-if(25 >= 25).
in_if_compare() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(25 >= 25).
                +| in_if_compare() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn if_comparison_inactive() {
        check_preprocessor(
            r#"
//- /src/test.erl
-if(24 >= 25).
in_if_compare() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(24 >= 25).
                >>> -if condition is false
                -| in_if_compare() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn if_not_expression() {
        check_preprocessor(
            r#"
//- /src/test.erl
-if(not false).
in_if_not() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(not false).
                +| in_if_not() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn if_andalso_expression() {
        check_preprocessor(
            r#"
//- /src/test.erl
-if(true andalso true).
in_if_andalso() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(true andalso true).
                +| in_if_andalso() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn if_orelse_expression() {
        check_preprocessor(
            r#"
//- /src/test.erl
-if(false orelse true).
in_if_orelse() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(false orelse true).
                +| in_if_orelse() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn if_else_with_true() {
        check_preprocessor(
            r#"
//- /src/test.erl
-if(true).
in_if() -> ok.
-else.
in_else() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(true).
                +| in_if() -> ok.
                +| -else.
                >>> previous branch was taken
                -| in_else() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn if_else_with_false() {
        check_preprocessor(
            r#"
//- /src/test.erl
-if(false).
in_if() -> ok.
-else.
in_else() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(false).
                >>> -if condition is false
                -| in_if() -> ok.
                +| -else.
                +| in_else() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn elif_first_branch_active() {
        check_preprocessor(
            r#"
//- /src/test.erl
-if(true).
in_if() -> ok.
-elif(true).
in_elif() -> ok.
-else.
in_else() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(true).
                +| in_if() -> ok.
                +| -elif(true).
                >>> previous branch was taken
                -| in_elif() -> ok.
                +| -else.
                >>> previous branch was taken
                -| in_else() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn elif_second_branch_active() {
        check_preprocessor(
            r#"
//- /src/test.erl
-if(false).
in_if() -> ok.
-elif(true).
in_elif() -> ok.
-else.
in_else() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(false).
                >>> -if condition is false
                -| in_if() -> ok.
                +| -elif(true).
                +| in_elif() -> ok.
                +| -else.
                >>> previous branch was taken
                -| in_else() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn elif_else_branch_active() {
        check_preprocessor(
            r#"
//- /src/test.erl
-if(false).
in_if() -> ok.
-elif(false).
in_elif() -> ok.
-else.
in_else() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(false).
                >>> -if condition is false
                -| in_if() -> ok.
                +| -elif(false).
                >>> -elif condition is false
                -| in_elif() -> ok.
                +| -else.
                +| in_else() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn if_with_arithmetic() {
        check_preprocessor(
            r#"
//- /src/test.erl
-if(10 + 5 > 12).
in_if() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(10 + 5 > 12).
                +| in_if() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn nested_if_conditions() {
        check_preprocessor(
            r#"
//- /src/test.erl
-if(true).
-if(false).
nested_both() -> ok.
-endif.
nested_outer() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(true).
                +| -if(false).
                >>> -if condition is false
                -| nested_both() -> ok.
                +| -endif.
                +| nested_outer() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn if_unknown_expression() {
        check_preprocessor(
            r#"
//- /src/test.erl
-if(?UNDEFINED_MACRO > 0).
in_if() -> ok.
-endif.
"#,
            expect![[r#"
                +| -if(?UNDEFINED_MACRO > 0).
                >>> -if condition is unknown
                ?| in_if() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    // =========================================================================

    #[test]
    fn test_pp_if_defined_expression_with_feature() {
        // Test -if(defined(FEATURE))
        // With FEATURE: feature_code should be active
        check_preprocessor(
            r#"
//- /src/test.erl macros:[FEATURE]
-module(test).
-if(defined(FEATURE)).
feature_code() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -if(defined(FEATURE)).
                +| feature_code() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE, FEATURE]
            "#]],
        );
    }

    #[test]
    fn test_pp_if_macro_expansion_true() {
        // Test -if(?FEATURE) where FEATURE is defined as true
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-define(FEATURE, true).
-if(?FEATURE).
feature_code() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -define(FEATURE, true).
                +| -if(?FEATURE).
                +| feature_code() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE, FEATURE]
            "#]],
        );
    }

    #[test]
    fn test_pp_if_macro_expansion_false() {
        // Test -if(?FEATURE) where FEATURE is defined as false
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-define(FEATURE, false).
-if(?FEATURE).
feature_code() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -define(FEATURE, false).
                +| -if(?FEATURE).
                >>> -if condition is false
                -| feature_code() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE, FEATURE]
            "#]],
        );
    }

    #[test]
    fn test_pp_include_macro_visible_after_include() {
        // Include resolution now works: the macro defined in the header
        // is visible after the include, so the ifdef is correctly active.
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-include("header.hrl").
-ifdef(HEADER_MACRO).
header_code() -> ok.
-endif.
//- /src/header.hrl
-define(HEADER_MACRO, true).
"#,
            expect![[r#"
                +| -module(test).
                +| -include("header.hrl").
                +| -ifdef(HEADER_MACRO).
                +| header_code() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE, HEADER_MACRO]
            "#]],
        );
    }

    #[test]
    fn test_if_elif_else_with_module_comparison_1() {
        // Test -if/-elif with ?MODULE comparisons and -else with -error
        // Note: In test context, ?MODULE evaluation may produce Unknown results
        // since the module context isn't fully available during condition evaluation.
        check_preprocessor(
            r#"
          //- /src/mod1.erl macros:[]
          -module(mod1).
          -if(?MODULE == mod1).
          -define(MSG1, "mod1 message").
          -elif((?MODULE == mod2) orelse (?MODULE == mod3)).
          -define(MSG2, "mod2 or mod3 message").
          -else.
          -error("an error").
          -endif.
          "#,
            expect![[r#"
                +| -module(mod1).
                +| -if(?MODULE == mod1).
                +| -define(MSG1, "mod1 message").
                +| -elif((?MODULE == mod2) orelse (?MODULE == mod3)).
                >>> previous branch was taken
                -| -define(MSG2, "mod2 or mod3 message").
                +| -else.
                >>> previous branch was taken
                -| -error("an error").
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE, MSG1]
            "#]],
        );
    }

    #[test]
    fn test_if_elif_else_with_module_comparison_2() {
        // Test -if/-elif with ?MODULE comparisons and -else with -error
        // Note: In test context, ?MODULE evaluation may produce Unknown results.
        check_preprocessor(
            r#"
          //- /src/mod2.erl macros:[]
          -module(mod2).
          -if(?MODULE == mod1).
          -define(MSG1, "mod1 message").
          -elif((?MODULE == mod2) orelse (?MODULE == mod3)).
          -define(MSG2, "mod2 or mod3 message").
          -else.
          -error("an error").
          -endif.
          "#,
            expect![[r#"
                +| -module(mod2).
                +| -if(?MODULE == mod1).
                >>> -if condition is false
                -| -define(MSG1, "mod1 message").
                +| -elif((?MODULE == mod2) orelse (?MODULE == mod3)).
                +| -define(MSG2, "mod2 or mod3 message").
                +| -else.
                >>> previous branch was taken
                -| -error("an error").
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE, MSG2]
            "#]],
        );
    }

    #[test]
    fn test_if_elif_else_with_module_comparison_3() {
        // Test -if/-elif with ?MODULE comparisons and -else with -error
        // Note: In test context, ?MODULE evaluation may produce Unknown results.
        check_preprocessor(
            r#"
          //- /src/mod4.erl macros:[]
          -module(mod4).
          -if(?MODULE == mod1).
          -define(MSG1, "mod1 message").
          -elif((?MODULE == mod2) orelse (?MODULE == mod3)).
          -define(MSG2, "mod2 or mod3 message").
          -else.
          -error("an error").
          -endif.
          "#,
            expect![[r#"
                +| -module(mod4).
                +| -if(?MODULE == mod1).
                >>> -if condition is false
                -| -define(MSG1, "mod1 message").
                +| -elif((?MODULE == mod2) orelse (?MODULE == mod3)).
                >>> -elif condition is false
                -| -define(MSG2, "mod2 or mod3 message").
                +| -else.
                +| -error("an error").
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn test_module_comparison_in_header_mod1() {
        // Test module-specific conditional code from header - mod1 case
        // When the header is analyzed independently (for diagnostics), ?MODULE
        // is not available so the macro expansion produces Missing expressions.
        // The main file analysis uses the preprocessor result from the main file
        // context where ?MODULE would be available during actual inclusion.
        check_preprocessor_with_diagnostics(
            r#"
//- /src/mod1.erl macros:[]
-module(mod1).
-include("conditional.hrl").
-ifdef(MSG1).
foo() -> ?MSG1.
-elif(defined(MSG2)).
foo() -> ?MSG2.
-else.
foo() -> "default message".
-endif.
//- /src/conditional.hrl
-if(?MODULE == mod1).
%% ^^^^^^^^^^^^^^^^^ warning: W0061: undefined macro 'MODULE' in preprocessor condition
-define(MSG1, "mod1 message").
-elif((?MODULE == mod2) orelse (?MODULE == mod3)).
%%   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0061: undefined macro 'MODULE' in preprocessor condition
-define(MSG2, "mod2 or mod3 message").
-else.
-error("an error").
-endif.
"#,
            expect![[r#"
                +| -module(mod1).
                +| -include("conditional.hrl").
                +| -ifdef(MSG1).
                +| foo() -> ?MSG1.
                +| -elif(defined(MSG2)).
                >>> previous branch was taken
                -| foo() -> ?MSG2.
                +| -else.
                >>> previous branch was taken
                -| foo() -> "default message".
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE, MSG1]
            "#]],
        );
    }

    #[test]
    fn test_module_comparison_in_header_mod2() {
        // Test module-specific conditional code from header - mod2 case
        // When the header is included from mod2, ?MODULE resolves to mod2.
        // The elif condition (?MODULE == mod2) orelse (?MODULE == mod3) is true.
        // When the header is analyzed independently (for diagnostics), ?MODULE
        // is not available so the macro expansion produces Missing expressions.
        check_preprocessor_with_diagnostics(
            r#"
//- /src/mod2.erl macros:[]
-module(mod2).
-include("conditional.hrl").
//- /src/conditional.hrl
-if(?MODULE == mod1).
%% ^^^^^^^^^^^^^^^^^ warning: W0061: undefined macro 'MODULE' in preprocessor condition
-define(MSG, "mod1 message").
-elif((?MODULE == mod2) orelse (?MODULE == mod3)).
%%   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0061: undefined macro 'MODULE' in preprocessor condition
-define(MSG, "mod2 or mod3 message").
-else.
-error("an error").
-endif.
"#,
            expect![[r#"
                +| -module(mod2).
                +| -include("conditional.hrl").
                >>> defined: [ELP_ERLANG_SERVICE, MSG]
            "#]],
        );
    }

    #[test]
    fn test_module_comparison_in_header_mod3() {
        // Test module-specific conditional code from header - mod3 case
        // When the header is included from mod3, ?MODULE resolves to mod3.
        // The elif condition (?MODULE == mod2) orelse (?MODULE == mod3) is true.
        // When the header is analyzed independently (for diagnostics), ?MODULE
        // is not available so the macro expansion produces Missing expressions.
        check_preprocessor_with_diagnostics(
            r#"
//- /src/mod3.erl macros:[]
-module(mod3).
-include("conditional.hrl").
//- /src/conditional.hrl
-if(?MODULE == mod1).
%% ^^^^^^^^^^^^^^^^^ warning: W0061: undefined macro 'MODULE' in preprocessor condition
-define(MSG1, "mod1 message").
-elif((?MODULE == mod2) orelse (?MODULE == mod3)).
%%   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0061: undefined macro 'MODULE' in preprocessor condition
-define(MSG2, "mod2 or mod3 message").
-else.
-error("an error").
-endif.
"#,
            expect![[r#"
                +| -module(mod3).
                +| -include("conditional.hrl").
                >>> defined: [ELP_ERLANG_SERVICE, MSG2]
            "#]],
        );
    }

    #[test]
    fn test_module_string_comparison_in_header_mod1() {
        // Test ?MODULE_STRING in header - mod1 case
        // When the header is included from mod1, ?MODULE_STRING resolves to "mod1".
        // Different macros are defined in each branch to verify the correct path is taken.
        check_preprocessor_with_diagnostics(
            r#"
//- /src/mod1.erl macros:[]
-module(mod1).
-include("conditional.hrl").
//- /src/conditional.hrl
-if(?MODULE_STRING == "mod1").
%% ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0061: undefined macro 'MODULE_STRING' in preprocessor condition
-define(MOD1_STRING_MSG, "mod1 string").
-else.
-define(OTHER_STRING_MSG, "other string").
-endif.
"#,
            expect![[r#"
                +| -module(mod1).
                +| -include("conditional.hrl").
                >>> defined: [ELP_ERLANG_SERVICE, MOD1_STRING_MSG]
            "#]],
        );
    }

    #[test]
    fn test_module_string_comparison_in_header_mod2() {
        // Test ?MODULE_STRING in header - mod2 case
        // When the header is included from mod2, ?MODULE_STRING resolves to "mod2".
        // Different macros are defined in each branch to verify the correct path is taken.
        check_preprocessor_with_diagnostics(
            r#"
//- /src/mod2.erl macros:[]
-module(mod2).
-include("conditional.hrl").

//- /src/conditional.hrl
-if(?MODULE_STRING == "mod1").
%% ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0061: undefined macro 'MODULE_STRING' in preprocessor condition
-define(MOD1_STRING_MSG, "mod1 string").
-elif(?MODULE_STRING == "mod2").
%%   ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0061: undefined macro 'MODULE_STRING' in preprocessor condition
-define(MOD2_STRING_MSG, "mod2 string").
-else.
-define(OTHER_STRING_MSG, "other string").
-endif.
"#,
            expect![[r#"
                +| -module(mod2).
                +| -include("conditional.hrl").
                +|
                >>> defined: [ELP_ERLANG_SERVICE, MOD2_STRING_MSG]
            "#]],
        );
    }

    #[test]
    fn test_file_comparison_in_header_mod1() {
        // Test ?FILE in header - mod1 case
        // When the header is included from mod1, ?FILE resolves to "mod1.erl".
        // Different macros are defined in each branch to verify the correct path is taken.
        check_preprocessor_with_diagnostics(
            r#"
//- /src/mod1.erl macros:[]
-module(mod1).
-include("conditional.hrl").

//- /src/conditional.hrl
-if(?FILE == "mod1.erl").
%% ^^^^^^^^^^^^^^^^^^^^^ warning: W0061: undefined macro 'FILE' in preprocessor condition
-define(MOD1_FILE_MSG, "mod1 file").
-else.
-define(OTHER_FILE_MSG, "other file").
-endif.
"#,
            expect![[r#"
                +| -module(mod1).
                +| -include("conditional.hrl").
                +|
                >>> defined: [ELP_ERLANG_SERVICE, MOD1_FILE_MSG]
            "#]],
        );
    }

    #[test]
    fn test_file_comparison_in_header_mod2() {
        // Test ?FILE in header - mod2 case
        // When the header is included from mod2, ?FILE resolves to "mod2.erl".
        // Different macros are defined in each branch to verify the correct path is taken.
        check_preprocessor_with_diagnostics(
            r#"
//- /src/mod2.erl macros:[]
-module(mod2).
-include("conditional.hrl").

//- /src/conditional.hrl
-if(?FILE == "mod1.erl").
-define(MOD1_FILE_MSG, "mod1 file").
-elif(?FILE == "mod2.erl").
-define(MOD2_FILE_MSG, "mod2 file").
-else.
-define(OTHER_FILE_MSG, "other file").
-endif.
"#,
            expect![[r#"
                +| -module(mod2).
                +| -include("conditional.hrl").
                +|
                >>> defined: [ELP_ERLANG_SERVICE, MOD2_FILE_MSG]
            "#]],
        );
    }

    #[test]
    fn test_pp_ifdef_builtin_otp_release() {
        // OTP_RELEASE is a built-in macro, so -ifdef(?OTP_RELEASE) should
        // always take the ifdef branch even without an explicit define.
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-ifdef(OTP_RELEASE).
new_otp_code() -> ok.
-else.
old_otp_code() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -ifdef(OTP_RELEASE).
                +| new_otp_code() -> ok.
                +| -else.
                >>> previous branch was taken
                -| old_otp_code() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn test_pp_ifndef_builtin_otp_release() {
        // OTP_RELEASE is always defined, so -ifndef(?OTP_RELEASE) should
        // take the else branch.
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-ifndef(OTP_RELEASE).
old_otp_code() -> ok.
-else.
new_otp_code() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -ifndef(OTP_RELEASE).
                >>> OTP_RELEASE is defined
                -| old_otp_code() -> ok.
                +| -else.
                +| new_otp_code() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn test_pp_if_defined_builtin_otp_release() {
        // -if(defined(OTP_RELEASE)) should evaluate to true for a built-in.
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-if(defined(OTP_RELEASE)).
otp_code() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -if(defined(OTP_RELEASE)).
                +| otp_code() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn test_pp_ifdef_builtin_file() {
        // FILE is a built-in macro, so -ifdef(?FILE) should be active.
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-ifdef(FILE).
has_file() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -ifdef(FILE).
                +| has_file() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn test_pp_ifdef_builtin_module() {
        // MODULE is a built-in macro, so -ifdef(?MODULE) should be active.
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-ifdef(MODULE).
has_module() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -ifdef(MODULE).
                +| has_module() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn test_pp_ifdef_builtin_line() {
        // LINE is a built-in macro, so -ifdef(?LINE) should be active.
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-ifdef(LINE).
has_line() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -ifdef(LINE).
                +| has_line() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn test_pp_ifndef_builtin_module_takes_else() {
        // MODULE is always defined, so -ifndef(?MODULE) should skip to -else.
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-ifndef(MODULE).
no_module() -> ok.
-else.
has_module() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -ifndef(MODULE).
                >>> MODULE is defined
                -| no_module() -> ok.
                +| -else.
                +| has_module() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn test_pp_ifdef_elp_erlang_service() {
        // ELP_ERLANG_SERVICE is always injected into external defines,
        // so -ifdef(ELP_ERLANG_SERVICE) should take the ifdef branch.
        // This matches the Erlang service, which defines the macro when
        // compiling files.
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-ifdef(ELP_ERLANG_SERVICE).
simplified_expansion() -> ok.
-else.
complex_expansion() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -ifdef(ELP_ERLANG_SERVICE).
                +| simplified_expansion() -> ok.
                +| -else.
                >>> previous branch was taken
                -| complex_expansion() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn test_pp_ifndef_elp_erlang_service() {
        // ELP_ERLANG_SERVICE is always defined, so -ifndef should be inactive.
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-ifndef(ELP_ERLANG_SERVICE).
unsupported_path() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -ifndef(ELP_ERLANG_SERVICE).
                >>> ELP_ERLANG_SERVICE is defined
                -| unsupported_path() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn test_pp_if_defined_elp_erlang_service() {
        // -if(defined(ELP_ERLANG_SERVICE)) should evaluate to true.
        check_preprocessor(
            r#"
//- /src/test.erl macros:[]
-module(test).
-if(defined(ELP_ERLANG_SERVICE)).
elp_path() -> ok.
-endif.
"#,
            expect![[r#"
                +| -module(test).
                +| -if(defined(ELP_ERLANG_SERVICE)).
                +| elp_path() -> ok.
                +| -endif.
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn test_pp_cross_app_transitive_include_macro() {
        // A macro defined in a transitively-included header from another
        // app should be visible for ifdef evaluation.  app_a depends on
        // app_b and app_c; app_b does NOT depend on app_c.  app_a's
        // source includes app_b's header, which includes app_c's header
        // that defines the macro.  Without orig_app propagation the
        // nested include_lib would fail because app_b lacks the dep on
        // app_c.
        check_preprocessor(
            r#"
//- /a_real/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_b,app_c
-module(main).
-include_lib("app_b/include/bridge.hrl").
-ifdef(FROM_APP_C).
guarded() -> from_app_c.
-endif.

//- /b_real/include/bridge.hrl app:app_b buck_target:cell//app_b:lib
-include_lib("app_c/include/defs.hrl").

//- /c_real/include/defs.hrl app:app_c buck_target:cell//app_c:lib
-define(FROM_APP_C, true).
"#,
            expect![[r#"
                +| -module(main).
                +| -include_lib("app_b/include/bridge.hrl").
                +| -ifdef(FROM_APP_C).
                +| guarded() -> from_app_c.
                +| -endif.
                +|
                >>> defined: [ELP_ERLANG_SERVICE]
            "#]],
        );
    }

    #[test]
    fn test_pp_cross_app_transitive_include_macro_reverse_dep() {
        // Reverse scenario: app_b has the dep on app_c, but app_a only
        // depends on app_b.  The nested include from app_b's header
        // should still resolve because the intermediate app's deps are
        // also consulted.
        check_preprocessor(
            r#"
//- /a_real/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_b
-module(main).
-include_lib("app_b/include/bridge.hrl").
-ifdef(FROM_APP_C).
guarded() -> from_app_c.
-endif.

//- /b_real/include/bridge.hrl app:app_b buck_target:cell//app_b:lib deps:app_c
-include_lib("app_c/include/defs.hrl").

//- /c_real/include/defs.hrl app:app_c buck_target:cell//app_c:lib
-define(FROM_APP_C, true).
"#,
            expect![[r#"
                +| -module(main).
                +| -include_lib("app_b/include/bridge.hrl").
                +| -ifdef(FROM_APP_C).
                +| guarded() -> from_app_c.
                +| -endif.
                +|
                >>> defined: [ELP_ERLANG_SERVICE, FROM_APP_C]
            "#]],
        );
    }
}

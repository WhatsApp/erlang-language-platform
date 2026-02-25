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
            Some(ConditionValue::Bool(defined_macros.contains(&macro_name)))
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

    use elp_syntax::ast::ArithOp;
    use elp_syntax::ast::CompOp;
    use elp_syntax::ast::Ordering;
    use elp_syntax::ast::UnaryOp;

    use crate::MacroName;
    use crate::Name;
    use crate::condition_expr::ConditionExpr;

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
}

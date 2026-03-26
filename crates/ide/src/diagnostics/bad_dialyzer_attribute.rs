/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: bad_dialyzer_attribute
//
// Validates the contents of `-dialyzer()` attributes, checking for:
// - Invalid dialyzer options (with fuzzy-match suggestions)
// - References to non-existent functions
// - Malformed attribute values

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_syntax::SyntaxKind;
use elp_syntax::ast;
use elp_syntax::ast::AstNode;
use hir::AsName;
use hir::Name;
use hir::NameArity;
use hir::Semantic;
use hir::known;

use super::DiagnosticCode;
use super::GenericLinter;
use super::GenericLinterMatchContext;
use super::Linter;
use super::Severity;
use crate::Assist;
use crate::TextRange;
use crate::fix;

/// Valid dialyzer options that can be used at module level (bare atom).
/// These are also valid when paired with function references.
const MODULE_LEVEL_OPTIONS: &[Name] = &[
    known::error_handling,
    known::extra_return,
    known::missing_return,
    known::no_behaviours,
    known::no_contracts,
    known::no_extra_return,
    known::no_fail_call,
    known::no_fun_app,
    known::no_improper_lists,
    known::no_match,
    known::no_missing_calls,
    known::no_missing_return,
    known::no_opaque,
    known::no_opaque_union,
    known::no_return,
    known::no_undefined_callbacks,
    known::no_underspecs,
    known::no_unknown,
    known::no_unused,
    known::opaque_union,
    known::overspecs,
    known::race_conditions,
    known::specdiffs,
    known::underspecs,
    known::unknown,
    known::unmatched_returns,
];

/// Options that are only valid when paired with function references.
const FUNCTION_ONLY_OPTIONS: &[Name] = &[known::nowarn_function];

fn is_valid_function_option(name: &Name) -> bool {
    MODULE_LEVEL_OPTIONS.contains(name) || FUNCTION_ONLY_OPTIONS.contains(name)
}

fn is_valid_module_option(name: &Name) -> bool {
    MODULE_LEVEL_OPTIONS.contains(name)
}

fn all_options() -> impl Iterator<Item = &'static str> {
    MODULE_LEVEL_OPTIONS
        .iter()
        .chain(FUNCTION_ONLY_OPTIONS.iter())
        .map(|n| n.as_str())
}

fn suggest_option(name: &str) -> Option<&'static str> {
    let mut suggestions: Vec<(&str, f64)> = all_options()
        .filter(|&opt| opt != name)
        .filter(|&opt| {
            let close_enough: usize = (name.len() / 3).clamp(1, 3);
            triple_accel::levenshtein::rdamerau(name.as_bytes(), opt.as_bytes())
                <= u32::try_from(close_enough).expect("valid u32 conversion")
        })
        .map(|opt| (opt, strsim::jaro_winkler(name, opt)))
        .collect();
    suggestions.sort_by(|a, b| {
        b.1.partial_cmp(&a.1)
            .expect("similarity scores should be comparable")
    });
    suggestions.first().map(|(s, _)| *s)
}

pub(crate) struct BadDialyzerAttributeLinter;

impl Linter for BadDialyzerAttributeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::BadDialyzerAttribute
    }

    fn description(&self) -> &'static str {
        "invalid dialyzer attribute"
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::Error
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Context {
    kind: ContextKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
enum ContextKind {
    BadOption {
        option_text: String,
        suggested: Option<String>,
    },
    UndefinedFunction {
        fa_text: String,
    },
    #[default]
    MalformedAttribute,
}

impl GenericLinter for BadDialyzerAttributeLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let form_list = sema.db.file_form_list(file_id);
        let def_map = sema.def_map(file_id);
        let mut res = Vec::new();

        for (_id, attr) in form_list.attributes() {
            if attr.name != known::dialyzer {
                continue;
            }
            let wild_attr = attr.form_id.get_ast(sema.db, file_id);
            if let Some(value) = wild_attr.value() {
                validate_dialyzer_value(&value, file_id, &def_map, &mut res);
            }
        }

        if res.is_empty() { None } else { Some(res) }
    }

    fn match_description(&self, context: &Self::Context) -> std::borrow::Cow<'_, str> {
        match &context.kind {
            ContextKind::BadOption {
                option_text,
                suggested: Some(suggestion),
            } => format!(
                "invalid dialyzer option '{}', did you mean '{}'?",
                option_text, suggestion
            )
            .into(),
            ContextKind::BadOption {
                option_text,
                suggested: None,
            } => format!("invalid dialyzer option '{}'", option_text).into(),
            ContextKind::UndefinedFunction { fa_text } => {
                format!("function {} is not defined in this module", fa_text).into()
            }
            ContextKind::MalformedAttribute => "malformed dialyzer attribute value".into(),
        }
    }

    fn fixes(
        &self,
        context: &Self::Context,
        range: TextRange,
        _sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        if let ContextKind::BadOption {
            suggested: Some(suggestion),
            ..
        } = &context.kind
        {
            let edit = TextEdit::replace(range, suggestion.clone());
            let msg = format!("Change to '{}'", suggestion);
            Some(vec![fix(
                "fix_bad_dialyzer_option",
                &msg,
                SourceChange::from_text_edit(file_id, edit),
                range,
            )])
        } else {
            None
        }
    }
}

fn validate_dialyzer_value(
    expr: &ast::Expr,
    file_id: FileId,
    def_map: &hir::DefMap,
    res: &mut Vec<GenericLinterMatchContext<Context>>,
) {
    match expr {
        // Bare atom: -dialyzer(no_return).
        ast::Expr::ExprMax(ast::ExprMax::Atom(atom)) => {
            let name = atom.as_name();
            if !is_valid_module_option(&name) {
                let name_str = name.to_string();
                let suggested = suggest_option(&name_str);
                res.push(GenericLinterMatchContext {
                    range: FileRange {
                        file_id,
                        range: atom.syntax().text_range(),
                    },
                    context: Context {
                        kind: ContextKind::BadOption {
                            option_text: name_str,
                            suggested: suggested.map(|s| s.to_string()),
                        },
                    },
                });
            }
        }
        // Tuple: {Option, FA} or {Option, [FA, ...]} or {[Options], FA} etc.
        ast::Expr::ExprMax(ast::ExprMax::Tuple(tuple)) => {
            validate_dialyzer_tuple(tuple, file_id, def_map, res);
        }
        // List: [Item, ...]
        ast::Expr::ExprMax(ast::ExprMax::List(list)) => {
            for item in list.exprs() {
                validate_dialyzer_value(&item, file_id, def_map, res);
            }
        }
        // Paren: unwrap
        ast::Expr::ExprMax(ast::ExprMax::ParenExpr(paren)) => {
            if let Some(inner) = paren.expr() {
                validate_dialyzer_value(&inner, file_id, def_map, res);
            }
        }
        // Anything else is malformed
        _ => {
            res.push(GenericLinterMatchContext {
                range: FileRange {
                    file_id,
                    range: expr.syntax().text_range(),
                },
                context: Context {
                    kind: ContextKind::MalformedAttribute,
                },
            });
        }
    }
}

fn validate_dialyzer_tuple(
    tuple: &ast::Tuple,
    file_id: FileId,
    def_map: &hir::DefMap,
    res: &mut Vec<GenericLinterMatchContext<Context>>,
) {
    let elems: Vec<ast::Expr> = tuple.expr().collect();
    if elems.len() != 2 {
        // Dialyzer tuples must be {Option, FA} or {Options, FAs}
        res.push(GenericLinterMatchContext {
            range: FileRange {
                file_id,
                range: tuple.syntax().text_range(),
            },
            context: Context {
                kind: ContextKind::MalformedAttribute,
            },
        });
        return;
    }

    let option_expr = &elems[0];
    let fa_expr = &elems[1];

    // Validate option(s)
    validate_option_part(option_expr, file_id, res);

    // Validate function reference(s)
    validate_fa_part(fa_expr, file_id, def_map, res);
}

/// Validate the option part of a {Option, FA} tuple.
/// Can be a single atom or a list of atoms.
fn validate_option_part(
    expr: &ast::Expr,
    file_id: FileId,
    res: &mut Vec<GenericLinterMatchContext<Context>>,
) {
    match expr {
        ast::Expr::ExprMax(ast::ExprMax::Atom(atom)) => {
            let name = atom.as_name();
            if !is_valid_function_option(&name) {
                let name_str = name.to_string();
                let suggested = suggest_option(&name_str);
                res.push(GenericLinterMatchContext {
                    range: FileRange {
                        file_id,
                        range: atom.syntax().text_range(),
                    },
                    context: Context {
                        kind: ContextKind::BadOption {
                            option_text: name_str,
                            suggested: suggested.map(|s| s.to_string()),
                        },
                    },
                });
            }
        }
        ast::Expr::ExprMax(ast::ExprMax::List(list)) => {
            for item in list.exprs() {
                validate_option_part(&item, file_id, res);
            }
        }
        _ => {
            res.push(GenericLinterMatchContext {
                range: FileRange {
                    file_id,
                    range: expr.syntax().text_range(),
                },
                context: Context {
                    kind: ContextKind::MalformedAttribute,
                },
            });
        }
    }
}

/// Validate the function-reference part of a {Option, FA} tuple.
/// Can be a single FA (Name/Arity) or a list of FAs.
fn validate_fa_part(
    expr: &ast::Expr,
    file_id: FileId,
    def_map: &hir::DefMap,
    res: &mut Vec<GenericLinterMatchContext<Context>>,
) {
    match expr {
        // FA as Name/Arity (binary op with /)
        ast::Expr::BinaryOpExpr(bin_op) => {
            validate_single_fa(bin_op, file_id, def_map, res);
        }
        // List of FAs
        ast::Expr::ExprMax(ast::ExprMax::List(list)) => {
            for item in list.exprs() {
                validate_fa_part(&item, file_id, def_map, res);
            }
        }
        _ => {
            // Not a recognized FA form
            res.push(GenericLinterMatchContext {
                range: FileRange {
                    file_id,
                    range: expr.syntax().text_range(),
                },
                context: Context {
                    kind: ContextKind::MalformedAttribute,
                },
            });
        }
    }
}

/// Validate a single function/arity reference like `foo/0`.
fn validate_single_fa(
    bin_op: &ast::BinaryOpExpr,
    file_id: FileId,
    def_map: &hir::DefMap,
    res: &mut Vec<GenericLinterMatchContext<Context>>,
) {
    // Check that the operator is `/`
    let has_slash = bin_op
        .syntax()
        .children_with_tokens()
        .any(|child| child.kind() == SyntaxKind::ANON_SLASH);

    if !has_slash {
        res.push(GenericLinterMatchContext {
            range: FileRange {
                file_id,
                range: bin_op.syntax().text_range(),
            },
            context: Context {
                kind: ContextKind::MalformedAttribute,
            },
        });
        return;
    }

    let (fun_name, arity) = match (bin_op.lhs(), bin_op.rhs()) {
        (
            Some(ast::Expr::ExprMax(ast::ExprMax::Atom(name_atom))),
            Some(ast::Expr::ExprMax(ast::ExprMax::Integer(arity_int))),
        ) => {
            let name: Name = name_atom.as_name();
            let arity_text = arity_int.text().to_string();
            match arity_text.parse::<u32>() {
                Ok(a) => (name, a),
                Err(_) => {
                    res.push(GenericLinterMatchContext {
                        range: FileRange {
                            file_id,
                            range: bin_op.syntax().text_range(),
                        },
                        context: Context {
                            kind: ContextKind::MalformedAttribute,
                        },
                    });
                    return;
                }
            }
        }
        _ => {
            res.push(GenericLinterMatchContext {
                range: FileRange {
                    file_id,
                    range: bin_op.syntax().text_range(),
                },
                context: Context {
                    kind: ContextKind::MalformedAttribute,
                },
            });
            return;
        }
    };

    let na = NameArity::new(fun_name, arity);
    if def_map.get_function(&na).is_none() {
        let fa_text = na.to_string();
        res.push(GenericLinterMatchContext {
            range: FileRange {
                file_id,
                range: bin_op.syntax().text_range(),
            },
            context: Context {
                kind: ContextKind::UndefinedFunction { fa_text },
            },
        });
    }
}

pub static LINTER: BadDialyzerAttributeLinter = BadDialyzerAttributeLinter;

#[cfg(test)]
mod tests {
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix_with_config;

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::NoDialyzerAttribute);
        check_diagnostics_with_config(config, fixture);
    }

    #[track_caller]
    fn check_fix(fixture: &str, expected: expect_test::Expect) {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::NoDialyzerAttribute);
        check_fix_with_config(config, fixture, expected);
    }

    // -------------------------------------------------------------------
    // Valid cases - no diagnostic expected
    // -------------------------------------------------------------------

    #[test]
    fn valid_bare_option() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer(no_return).
            "#,
        );
    }

    #[test]
    fn valid_nowarn_function_with_existing_fun() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer({nowarn_function, foo/0}).
            foo() -> ok.
            "#,
        );
    }

    #[test]
    fn valid_option_with_existing_fun() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer({no_return, foo/0}).
            foo() -> ok.
            "#,
        );
    }

    #[test]
    fn valid_option_list_with_existing_fun() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer({[no_return, no_match], [foo/0]}).
            foo() -> ok.
            "#,
        );
    }

    #[test]
    fn valid_list_of_options() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer([no_return, no_match]).
            "#,
        );
    }

    #[test]
    fn valid_list_of_mixed_forms() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer([no_return, {nowarn_function, foo/0}]).
            foo() -> ok.
            "#,
        );
    }

    // -------------------------------------------------------------------
    // Invalid option
    // -------------------------------------------------------------------

    #[test]
    fn invalid_option_bare() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer(noworn_funkshun).
            %%        ^^^^^^^^^^^^^^^ 💡 error: W0076: invalid dialyzer option 'noworn_funkshun'
            "#,
        );
    }

    #[test]
    fn invalid_option_with_suggestion() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer(no_retrn).
            %%        ^^^^^^^^ 💡 error: W0076: invalid dialyzer option 'no_retrn', did you mean 'no_return'?
            "#,
        );
    }

    #[test]
    fn invalid_option_in_list() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer([no_return, bad_option]).
            %%                    ^^^^^^^^^^ 💡 error: W0076: invalid dialyzer option 'bad_option'
            "#,
        );
    }

    #[test]
    fn invalid_option_in_tuple() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer({no_retrn, foo/0}).
            %%         ^^^^^^^^ 💡 error: W0076: invalid dialyzer option 'no_retrn', did you mean 'no_return'?
            foo() -> ok.
            "#,
        );
    }

    #[test]
    fn nowarn_function_bare_is_invalid() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer(nowarn_function).
            %%        ^^^^^^^^^^^^^^^ 💡 error: W0076: invalid dialyzer option 'nowarn_function'
            "#,
        );
    }

    // -------------------------------------------------------------------
    // Undefined function
    // -------------------------------------------------------------------

    #[test]
    fn undefined_function_reference() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer({nowarn_function, nonexistent/0}).
            %%                          ^^^^^^^^^^^^^ 💡 error: W0076: function nonexistent/0 is not defined in this module
            "#,
        );
    }

    #[test]
    fn undefined_function_in_list() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer({nowarn_function, [foo/0, missing/1]}).
            %%                                  ^^^^^^^^^ 💡 error: W0076: function missing/1 is not defined in this module
            foo() -> ok.
            "#,
        );
    }

    // -------------------------------------------------------------------
    // Malformed attribute
    // -------------------------------------------------------------------

    #[test]
    fn malformed_integer_value() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer(42).
            %%        ^^ 💡 error: W0076: malformed dialyzer attribute value
            "#,
        );
    }

    // -------------------------------------------------------------------
    // Fix tests
    // -------------------------------------------------------------------

    #[test]
    fn fix_bad_option() {
        check_fix(
            r#"
            //- /src/main.erl
            -module(main).
            -dialyzer(no_ret~rn).
            "#,
            expect_test::expect![[r#"
            -module(main).
            -dialyzer(no_return).
            "#]],
        );
    }
}

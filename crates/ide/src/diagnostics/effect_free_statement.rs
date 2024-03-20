/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Lint/fix: effect_free_statement
//!
//! Return a diagnostic if a statement is just a literal or a variable, and
//! offer to remove the statement as a fix.

use std::iter;

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::SyntaxElement;
use elp_syntax::SyntaxKind;
use hir::AnyExprId;
use hir::Expr;
use hir::ExprId;
use hir::FunctionDef;
use hir::InFunctionClauseBody;
use hir::Semantic;
use hir::Strategy;
use text_edit::TextEdit;

use super::Category;
use super::Diagnostic;
use super::Severity;
use crate::codemod_helpers::statement_range;
use crate::diagnostics::DiagnosticCode;
use crate::fix;

pub(crate) fn effect_free_statement(diags: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
    if sema.db.is_generated(file_id) {
        // No point asking for changes to generated files
        return;
    }
    sema.def_map(file_id).get_functions().for_each(|(_, def)| {
        if def.file.file_id == file_id {
            let source_file = sema.parse(file_id);

            let def_fb = def.in_function_body(sema, def);
            def_fb.fold_function(
                Strategy::InvisibleMacros,
                (),
                &mut |_acc, clause_id, ctx| {
                    if let AnyExprId::Expr(expr_id) = ctx.item_id {
                        let body_map = def_fb.get_body_map(clause_id);
                        let in_clause = def_fb.in_clause(clause_id);
                        if let Some(in_file_ast_ptr) = body_map.expr(expr_id) {
                            if let Some(expr_ast) = in_file_ast_ptr.to_node(&source_file) {
                                if is_statement(&expr_ast)
                                    && !is_macro_usage(&expr_ast)
                                    && has_no_effect(in_clause, &expr_id)
                                    && is_followed_by(SyntaxKind::ANON_COMMA, &expr_ast)
                                {
                                    diags.push(make_diagnostic(file_id, &expr_ast));
                                }
                            }
                        }
                    }
                },
            );
        }
    });
}

fn has_no_effect(def_fb: &InFunctionClauseBody<&FunctionDef>, expr_id: &ExprId) -> bool {
    let expr = &def_fb[*expr_id];
    match expr {
        Expr::Missing => false,

        Expr::Literal(_) => true,
        Expr::Binary { .. } => true,
        Expr::Var(_) => true,
        Expr::CaptureFun { .. } => true,
        Expr::Closure { .. } => true,

        Expr::Tuple { exprs } => exprs
            .iter()
            .all(|list_elem| has_no_effect(def_fb, list_elem)),

        Expr::List { exprs, .. } => exprs
            .iter()
            .all(|list_elem| has_no_effect(def_fb, list_elem)),

        Expr::Map { fields } => fields
            .iter()
            .all(|(k, v)| has_no_effect(def_fb, k) && has_no_effect(def_fb, v)),
        Expr::MapUpdate { .. } => {
            // Side-effect: may throw if not a map
            false
        }
        Expr::Comprehension { .. } => {
            // Side-effect: may throw due to generators types
            false
        }

        Expr::Record { fields, .. } => fields
            .iter()
            .all(|(_key, value)| has_no_effect(def_fb, value)),
        Expr::RecordUpdate { .. } | Expr::RecordField { .. } => {
            // Side-effect: may throw (e.g. expr not a record)
            false
        }
        Expr::RecordIndex { .. } => true,

        Expr::UnaryOp { .. } | Expr::BinaryOp { .. } => {
            // Side-effect: may throw
            false
        }
        Expr::MacroCall {
            expansion,
            args: _,
            macro_def: _,
        } => has_no_effect(def_fb, expansion),
        Expr::Call { .. } | Expr::Receive { .. } => false,

        Expr::Block { exprs } => exprs.iter().all(|stmt| has_no_effect(def_fb, stmt)),
        Expr::Catch { expr } => has_no_effect(def_fb, expr),
        Expr::Try { of_clauses, .. } if !of_clauses.is_empty() => {
            // Side-effect: may fail to match
            false
        }
        Expr::Try { exprs, after, .. } => {
            // NB. if exprs has no effect, nothing can throw
            // so there can be no match errors in catch_clauses
            exprs.iter().all(|stmt| has_no_effect(def_fb, stmt))
                && after.iter().all(|stmt| has_no_effect(def_fb, stmt))
        }
        Expr::Match { .. } | Expr::If { .. } | Expr::Case { .. } | Expr::Maybe { .. } => {
            // Side-effects:
            //   - binding of variables
            //   - may fail to match
            false
        }
    }
}

#[allow(clippy::match_like_matches_macro)]
fn is_statement(expr: &ast::Expr) -> bool {
    let syntax = expr.syntax();
    match syntax.parent() {
        Some(parent) => match parent.kind() {
            SyntaxKind::CLAUSE_BODY => true,
            SyntaxKind::BLOCK_EXPR => true,
            SyntaxKind::TRY_EXPR => true,
            SyntaxKind::CATCH_EXPR => true,
            SyntaxKind::TRY_AFTER => true,
            _ => false,
        },
        _ => false,
    }
}

fn is_macro_usage(expr: &ast::Expr) -> bool {
    let syntax = expr.syntax();
    syntax.kind() == SyntaxKind::MACRO_CALL_EXPR
}

fn is_followed_by(expected_kind: SyntaxKind, expr: &ast::Expr) -> bool {
    let node = expr.syntax();
    let elements = iter::successors(node.next_sibling_or_token(), |n| {
        (*n).next_sibling_or_token()
    });
    for element in elements {
        if let Some(t) = &SyntaxElement::into_token(element) {
            let kind = t.kind();
            if kind != SyntaxKind::WHITESPACE {
                return kind == expected_kind;
            }
        }
    }
    false
}

fn remove_statement(expr: &ast::Expr) -> Option<TextEdit> {
    let range = statement_range(expr);

    let mut edit_builder = TextEdit::builder();
    edit_builder.delete(range);
    Some(edit_builder.finish())
}

fn make_diagnostic(file_id: FileId, expr: &ast::Expr) -> Diagnostic {
    let node = expr.syntax();
    let range = node.text_range();
    let diag = Diagnostic::new(
        DiagnosticCode::StatementHasNoEffect,
        "this statement has no effect",
        range,
    )
    .with_severity(Severity::Warning)
    .add_categories([Category::SimplificationRule]);

    if let Some(statement_removal) = remove_statement(expr) {
        diag.with_fixes(Some(vec![fix(
            "remove_statement",
            "Remove redundant statement",
            SourceChange::from_text_edit(file_id, statement_removal),
            range,
        )]))
    } else {
        diag
    }
}

#[cfg(test)]
mod tests {

    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn check_removes_comma_and_other_stuff() {
        check_fix(
            r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                ~ok,
                do_something_else(),
                ok.
            do_something() -> ok.
            "#,
            r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                do_something_else(),
                ok.
            do_something() -> ok.
            "#,
        );
    }

    #[test]
    fn remove_useless_atom() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                ok,
            %%% ^^ 💡 warning: this statement has no effect
                do_something_else(),
                bar,
            %%% ^^^ 💡 warning: this statement has no effect
                ok.
            do_something() -> ok.
            "#,
        );
    }

    #[test]
    fn remove_useless_var() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                X = 42,
                X,
            %%% ^ 💡 warning: this statement has no effect
                ok.
            "#,
        );
    }

    #[test]
    fn remove_useless_literals() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                42,
            %%% ^^ 💡 warning: this statement has no effect
                41.9999,
            %%% ^^^^^^^ 💡 warning: this statement has no effect
                do_something_else(),
                "foo",
            %%% ^^^^^ 💡 warning: this statement has no effect
                <<"foo">>,
            %%% ^^^^^^^^^ 💡 warning: this statement has no effect
                'A',
            %%% ^^^ 💡 warning: this statement has no effect
                ok.
            do_something() -> 42.
            "#,
        );
    }

    #[test]
    fn remove_useless_lambda() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                fun() -> do_something() end,
            %%% ^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: this statement has no effect
                F = fun() -> do_something() end,
                F(),
                fun do_something/0,
            %%% ^^^^^^^^^^^^^^^^^^ 💡 warning: this statement has no effect
                fun erlang:length/1,
            %%% ^^^^^^^^^^^^^^^^^^^ 💡 warning: this statement has no effect
                ok.
            do_something() -> 42.
            "#,
        );
    }

    #[test]
    fn remove_under_parens() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                (do_something()),
                (blah),
            %%% ^^^^^^ 💡 warning: this statement has no effect
                ok.
            do_something() -> (42).
            "#,
        );
    }

    #[test]
    fn remove_useless_blocks() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                begin 42, blah, ("foo") end,
            %%% ^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: this statement has no effect
            %%%       ^^ 💡 warning: this statement has no effect
            %%%           ^^^^ 💡 warning: this statement has no effect
                begin
                  do_something(),
                  blah,
              %%% ^^^^ 💡 warning: this statement has no effect
                  ok
                end,
                ok.
            do_something() -> (42).
            "#,
        );
    }

    #[test]
    fn remove_useless_lists() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                [42, blah, ("foo")],
            %%% ^^^^^^^^^^^^^^^^^^^ 💡 warning: this statement has no effect
                [42, do_something(), blah],
                [],
            %%% ^^ 💡 warning: this statement has no effect
                ok.
            do_something() -> [].
            "#,
        );
    }

    #[test]
    fn remove_useless_tuples() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_Config) ->
                do_something(),
                {42, [blah], {"foo"}},
            %%% ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: this statement has no effect
                {42, do_something(), blah},
                {},
            %%% ^^ 💡 warning: this statement has no effect
                ok.
            do_something() -> [].
            "#,
        );
    }

    #[test]
    fn remove_useless_record_operations() {
        check_diagnostics(
            r#"
            -module(main).
            -record(person, {name, age}).
            test_foo(P) ->
                do_something(),
                #person{name="Bob", age=42},
            %%% ^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: this statement has no effect
                #person{name=get_name(), age=42},
                P#person{name="Alice"},
                #person.name,
            %%% ^^^^^^^^^^^^ 💡 warning: this statement has no effect
                P#person.name,
                ok.
            get_name() -> "bob".
            "#,
        );
    }

    #[test]
    fn remove_useless_map_operations() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(P) ->
                do_something(),
                #{name => "Bob", age => 42},
            %%% ^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: this statement has no effect
                #{name => get_name(), age => 42},
                #{get_key() => "Bob", age => 42},
                P#{name=>"Alice"},
                ok.
            get_name() -> "bob".
            get_key() -> name.
            "#,
        );
    }

    #[test]
    fn remove_useless_try_catch_operations() {
        check_diagnostics(
            r#"
            -module(main).
            test_foo(_P) ->
                catch do_something(),
                catch ok,
            %%% ^^^^^^^^ 💡 warning: this statement has no effect
                try does, nothing catch _ -> do_stuff() end,
            %%% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: this statement has no effect
                %%% ^^^^ 💡 warning: this statement has no effect
                try
                    does_nothing
                of _ -> ok
                catch _ -> not_ok
                end,
                try
                    do_something()
                after
                    ok
                end,
                try does, nothing after blah, ok end,
            %%% ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: this statement has no effect
                %%% ^^^^ 💡 warning: this statement has no effect
                                    %%% ^^^^ 💡 warning: this statement has no effect
                try
                    does, nothing
                %%% ^^^^ 💡 warning: this statement has no effect
                of _ -> foo, bar
                    %%% ^^^ 💡 warning: this statement has no effect
                catch
                  _ -> 42, not_ok
                   %%% ^^ 💡 warning: this statement has no effect
                after
                  [1,2,3],
              %%% ^^^^^^^ 💡 warning: this statement has no effect
                  ok
                end,
                ok.
            "#,
        );
    }

    #[test]
    fn ignore_stuff_introduced_from_macros() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
            -define(included_noop(X), noop).

//- /src/foo.erl
            -module(foo).
            -include("foo.hrl").
            -define(noop(X), another_noop).
            -define(also_noop, yet_another_noop).

            blah() ->
                 noop,
             %%% ^^^^ 💡 warning: this statement has no effect
                 do_something(),
                 ?included_noop(42),
                 do_something(),
                 ?noop(42),
                 ?also_noop,
                 ok.
        "#,
        )
    }
}

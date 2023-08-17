/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistUserInput;
use elp_ide_db::assists::AssistUserInputType;
use elp_syntax::algo;
use elp_syntax::ast;
use elp_syntax::ast::edit::IndentLevel;
use elp_syntax::ast::AstChildren;
use elp_syntax::AstNode;
use elp_syntax::Direction;
use elp_syntax::NodeOrToken;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use elp_syntax::SyntaxToken;
use elp_syntax::TextRange;
use fxhash::FxHashSet;
use hir::resolver::Resolution;
use hir::ScopeAnalysis;
use hir::Var;
use itertools::Itertools;
use stdx::format_to;

use crate::assist_context::AssistContext;
use crate::assist_context::Assists;
use crate::helpers::change_indent;
use crate::helpers::freshen_function_name;
use crate::helpers::DEFAULT_INDENT_STEP;

// Assist: extract_function
//
// Extracts selected statements and comments into new function.
//
// ```
// main() ->
//     N = 1,
//     ~M = N + 2,
//     // calculate
//     K = M + N,~
//     G = 3.
// }
// ```
// ->
// ```
// main() ->
//     N = 1,
//     fun_name(N),
//     G = 3.
// }
//
// $0fun_name(N) ->
//     M = N + 2,
//     // calculate
//     K = M + N.
// }
// ```
pub(crate) fn extract_function(acc: &mut Assists, ctx: &AssistContext<'_>) -> Option<()> {
    let range = ctx.selection_trimmed();
    if range.is_empty() {
        return None;
    }

    let node = ctx.covering_element();
    if node.kind() == SyntaxKind::COMMENT {
        return None;
    }

    let node = match node {
        NodeOrToken::Node(n) => n,
        NodeOrToken::Token(t) => t.parent()?,
    };

    let body = extraction_target(ctx, &node, range)?;
    let insert_after = node_to_insert_after(&body)?;
    let target_range = body.text_range();

    acc.add(
        AssistId("extract_function", crate::AssistKind::RefactorExtract),
        "Extract into function",
        target_range,
        Some(AssistUserInput {
            input_type: AssistUserInputType::Atom,
            value: make_function_name(ctx), // Maybe just a constant, this is expensive
        }),
        move |builder| {
            let locals = body.analyze(ctx);
            let outliving_locals = body.ret_values(ctx, &locals.bound);
            let params = body.extracted_function_params(locals.free);
            let name = freshen_function_name(
                ctx,
                ctx.user_input_or(|| make_function_name(ctx)),
                params.len() as u32,
            );
            let fun = Function {
                name,
                params,
                body,
                outliving_locals,
            };

            let new_indent = IndentLevel::from_node(&insert_after);
            let old_indent = fun.body.indent_level();

            builder.replace(target_range, make_call(ctx, &fun));
            let fn_def = fun.format(ctx, old_indent, new_indent);
            let insert_offset = insert_after.text_range().end();

            match ctx.config.snippet_cap {
                Some(cap) => builder.insert_snippet(cap, insert_offset, fn_def),
                None => builder.insert(insert_offset, fn_def),
            };
        },
    )
}

fn make_function_name(ctx: &AssistContext<'_>) -> String {
    let def_map = ctx.sema.def_map(ctx.file_id());
    let names_in_scope: FxHashSet<_> = def_map
        .get_functions()
        .keys()
        .chain(def_map.get_imports().iter().map(|(na, _)| na))
        .map(|n| n.name().as_str().to_string())
        .collect();
    let default_name = "fun_name";

    let mut name = default_name.to_string();
    let mut counter = 0;
    while names_in_scope.contains(&name) {
        counter += 1;
        name = format!("{}{}", &default_name, counter)
    }

    name
}

/// Try to guess what user wants to extract
///
/// We have basically have two cases:
/// * We want whole node,
///   Then we can use `ast::Expr`
/// * We want a few statements for a block. E.g.
///   ```erlang,no_run
///   foo() ->
///     M = 1,
///     ~
///     N = 2,
///     K = 3,
///     K + N.
///     ~
///   }
///   ```
///
fn extraction_target(
    ctx: &AssistContext,
    node: &SyntaxNode,
    selection_range: TextRange,
) -> Option<FunctionBody> {
    // Covering element returned the parent block of one or multiple
    // expressions that have been selected
    if let Some(expr_list) = ast::ClauseBody::cast(node.clone()) {
        // Extract the full expressions.
        return Some(FunctionBody::from_range_clause_body(
            expr_list,
            selection_range,
        ));
    }
    if let Some(fun_decl) = ast::FunDecl::cast(node.clone()) {
        // Extract the full expressions.
        // There can only be one clause, else the FunDecl would not be its covering element.
        if let Some(clause) = fun_decl.clauses().next() {
            match clause {
                ast::FunctionOrMacroClause::FunctionClause(_fun_clause) => {
                    return Some(FunctionBody::from_range_fun_decl(fun_decl, selection_range));
                }
                ast::FunctionOrMacroClause::MacroCallExpr(_) => {}
            };
        }
    }

    let expr = ast::Expr::cast(node.clone())?;
    // A node got selected fully
    if node.text_range() == selection_range {
        return FunctionBody::from_expr(ctx, expr);
    }

    node.ancestors()
        .find_map(ast::Expr::cast)
        .and_then(|expr| FunctionBody::from_expr(ctx, expr))
}

#[derive(Debug)]
struct Function {
    name: String,
    params: Vec<Param>,
    body: FunctionBody,
    outliving_locals: Vec<Var>,
}

#[derive(Debug)]
struct Param {
    var: Var,
}

#[derive(Debug)]
enum FunctionBody {
    Expr(ast::Expr),
    Span {
        parent: SpanParent,
        text_range: TextRange,
    },
}

#[derive(Debug)]
enum SpanParent {
    ClauseBody(ast::ClauseBody),
    FunDecl(ast::FunDecl),
}

impl SpanParent {
    fn syntax(&self) -> &SyntaxNode {
        match self {
            SpanParent::ClauseBody(it) => it.syntax(),
            SpanParent::FunDecl(it) => it.syntax(),
        }
    }

    fn exprs(&self) -> Option<AstChildren<ast::Expr>> {
        match self {
            SpanParent::ClauseBody(it) => Some(it.exprs()),
            SpanParent::FunDecl(it) => match it.clauses().next()? {
                ast::FunctionOrMacroClause::FunctionClause(it) => Some(it.body()?.exprs()),
                ast::FunctionOrMacroClause::MacroCallExpr(_) => None,
            },
        }
    }
}

impl FunctionBody {
    fn from_expr(ctx: &AssistContext, expr: ast::Expr) -> Option<Self> {
        if !valid_extraction(expr.syntax()) {
            return None;
        }
        ctx.sema
            .find_enclosing_function(ctx.file_id(), expr.syntax())
            .map(|_| Self::Expr(expr))
    }

    fn from_range_clause_body(parent: ast::ClauseBody, selected: TextRange) -> FunctionBody {
        let full_body = parent.syntax().children_with_tokens();

        let text_range = full_body
            // Harvest commonality
            .filter(|it| ast::Expr::can_cast(it.kind()) || it.kind() == SyntaxKind::COMMENT)
            .map(|element| element.text_range())
            .filter(|&range| overlaps(&selected, &range))
            .reduce(|acc, item| acc.cover(item));

        Self::Span {
            parent: SpanParent::ClauseBody(parent),
            text_range: text_range.unwrap_or(selected),
        }
    }

    fn from_range_fun_decl(parent: ast::FunDecl, selected: TextRange) -> FunctionBody {
        let parent = SpanParent::FunDecl(parent);
        let text_range = FunctionBody::nodes_in_span_parent(&parent, &selected)
            .iter()
            // Harvest commonality
            .map(|element| element.text_range())
            .filter(|&range| overlaps(&selected, &range))
            .reduce(|acc, item| acc.cover(item));

        Self::Span {
            parent,
            text_range: text_range.unwrap_or(selected),
        }
    }

    fn nodes_in_span_parent(parent: &SpanParent, selected: &TextRange) -> Vec<NodeOrToken> {
        match parent {
            SpanParent::ClauseBody(clause) => clause.syntax().children_with_tokens().collect_vec(),
            SpanParent::FunDecl(fun_decl) => {
                let full_body = fun_decl.syntax().children_with_tokens();

                // We need the full FunDecl for comments between the end of
                // the FunctionClause and the end of the FunDecl. But we must
                // process the Exprs in the FunctionClause. Hence need an expansion step
                full_body
                    .flat_map(|it| match &it {
                        NodeOrToken::Node(node) => {
                            if let Some(function_clause) = ast::FunctionClause::cast(node.clone()) {
                                match function_clause.body() {
                                    Some(clause_body) => {
                                        clause_body.syntax().children_with_tokens().collect_vec()
                                    }
                                    _ => vec![it],
                                }
                            } else {
                                vec![it]
                            }
                        }
                        NodeOrToken::Token(_) => vec![it],
                    })
                    // Harvest commonality
                    .filter(|it| ast::Expr::can_cast(it.kind()) || it.kind() == SyntaxKind::COMMENT)
                    .filter(|it| overlaps(selected, &it.text_range()))
                    .collect_vec()
            }
        }
    }

    fn node(&self) -> &SyntaxNode {
        match self {
            FunctionBody::Expr(e) => e.syntax(),
            FunctionBody::Span { parent, .. } => parent.syntax(),
        }
    }

    fn indent_level(&self) -> IndentLevel {
        match &self {
            FunctionBody::Expr(expr) => IndentLevel::from_node(expr.syntax()),
            FunctionBody::Span { parent, text_range } => {
                if let Some(element) = FunctionBody::nodes_in_span_parent(parent, text_range)
                    .iter()
                    .find(|it| text_range.contains_range(it.text_range()))
                {
                    IndentLevel::from_element(element)
                } else {
                    IndentLevel(1)
                }
            }
        }
    }

    fn walk_expr(&self, ctx: &AssistContext, analyzer: &mut ScopeAnalysis) {
        match self {
            FunctionBody::Expr(expr) => {
                analyzer.walk_ast_expr(&ctx.sema, ctx.file_id(), expr.clone())
            }
            FunctionBody::Span { parent, text_range } => {
                if let Some(exprs) = parent.exprs() {
                    exprs
                        .filter(|expr| text_range.contains_range(expr.syntax().text_range()))
                        .for_each(|expr: ast::Expr| {
                            analyzer.walk_ast_expr(&ctx.sema, ctx.file_id(), expr)
                        });
                }
            }
        }
    }

    fn text_range(&self) -> TextRange {
        match self {
            FunctionBody::Expr(expr) => expr.syntax().text_range(),
            &FunctionBody::Span { text_range, .. } => text_range,
        }
    }

    /// Analyzes a function body, returning the used local variables
    /// that are referenced in it.
    fn analyze(&self, ctx: &AssistContext) -> ScopeAnalysis {
        let mut analyzer = ScopeAnalysis::new();
        self.walk_expr(ctx, &mut analyzer);
        analyzer
    }

    /// Local variables defined inside `body` that are accessed outside of it
    fn ret_values<'a>(
        &self,
        ctx: &'a AssistContext<'_>,
        locals_bound_in_body: &'a FxHashSet<Resolution>,
    ) -> Vec<Var> {
        match &self {
            FunctionBody::Expr(expr) => {
                let parent = expr
                    .syntax()
                    .ancestors()
                    .find_map(ast::ClauseBody::cast)
                    .map(SpanParent::ClauseBody);
                if let Some(parent) = parent {
                    calculate_ret_values(
                        &parent,
                        &expr.syntax().text_range(),
                        ctx,
                        locals_bound_in_body,
                    )
                } else {
                    vec![]
                }
            }
            FunctionBody::Span { parent, text_range } => {
                calculate_ret_values(parent, text_range, ctx, locals_bound_in_body)
            }
        }
    }

    /// find variables that should be extracted as params
    ///
    /// Computes additional info that affects param type and mutability
    fn extracted_function_params(&self, free: FxHashSet<Resolution>) -> Vec<Param> {
        free.into_iter().map(|(var, _)| Param { var }).collect()
    }
}

fn calculate_ret_values(
    parent: &SpanParent,
    text_range: &TextRange,
    ctx: &AssistContext,
    locals_bound_in_body: &FxHashSet<Resolution>,
) -> Vec<Var> {
    let trailing: Vec<_> = parent
        .syntax()
        .children_with_tokens()
        .filter(|it| it.text_range().start() > text_range.end())
        .filter_map(|node_or_token| match &node_or_token {
            NodeOrToken::Node(node) => ast::Expr::cast(node.clone()),
            _ => None,
        })
        .collect::<Vec<ast::Expr>>();
    let mut analyzer = ScopeAnalysis::new();
    trailing.into_iter().for_each(|expr: ast::Expr| {
        analyzer.walk_ast_expr(&ctx.sema, ctx.file_id(), expr);
    });
    locals_bound_in_body
        .iter()
        .filter_map(|local| {
            if analyzer.free.contains(local) || analyzer.bound.contains(local) {
                Some(local.0)
            } else {
                None
            }
        })
        .collect()
}

/// Check whether the node is a valid expression which can be
/// extracted to a function.  In general that's true for any
/// expression, but in some cases that would produce invalid code.
fn valid_extraction(node: &SyntaxNode) -> bool {
    if let Some(n) = node.parent() {
        n.kind() != SyntaxKind::RECORD_FIELD
    } else {
        false
    }
}
/// Check if there is any part in common of the two `TextRange`s
fn overlaps(range_a: &TextRange, range_b: &TextRange) -> bool {
    range_a
        .intersect(*range_b)
        .filter(|it| !it.is_empty())
        .is_some()
}

/// find where to put extracted function definition
///
/// Function should be put right after returned node
fn node_to_insert_after(body: &FunctionBody) -> Option<SyntaxNode> {
    let node = body.node();
    let mut last_ancestor = None;
    for next_ancestor in node.ancestors().peekable() {
        if next_ancestor.kind() == SyntaxKind::SOURCE_FILE {
            break;
        }
        last_ancestor = Some(next_ancestor);
    }
    last_ancestor
}

fn make_call(ctx: &AssistContext<'_>, fun: &Function) -> String {
    let args = fun
        .params
        .iter()
        .map(|param| format!("{}", ctx.db().lookup_var(param.var)))
        .collect::<Vec<_>>()
        .join(",");

    let mut buf = String::default();

    match fun.outliving_locals.as_slice() {
        [] => {}
        [local] => {
            format_to!(buf, "{} = ", local.as_string(ctx.db().upcast()))
        }
        vars => {
            buf.push('{');
            let bindings = vars.iter().format_with(", ", |local, f| {
                f(&format_args!("{}", local.as_string(ctx.db().upcast())))
            });
            format_to!(buf, "{}", bindings);
            buf.push_str("} = ");
        }
    }

    format_to!(buf, "{}", fun.name);
    format_to!(buf, "({})", args);

    // Check if the selected range ended with a comment, which
    // occurred after a trailing comma in the original context
    let insert_comma = ends_with_comma_then_trivia(fun.body.node(), fun.body.text_range());
    if insert_comma.is_some() {
        buf.push(',');
    }

    buf
}

fn ends_with_comma_then_trivia(node: &SyntaxNode, range: TextRange) -> Option<TextRange> {
    // Temporary for T153426323
    let _pctx = stdx::panic_context::enter("\nends_with_comma_then_trivia".to_string());
    let end_tok = node.token_at_offset(range.end());
    let end_tok = end_tok
        .left_biased()
        .and_then(|t| algo::skip_trivia_token(t, Direction::Prev));
    end_tok.and_then(|it| {
        if it.kind() == SyntaxKind::ANON_COMMA {
            Some(it.text_range())
        } else {
            None
        }
    })
}

impl Function {
    fn make_param_list(&self, ctx: &AssistContext<'_>) -> String {
        self.params
            .iter()
            .map(|param| format!("{}", ctx.db().lookup_var(param.var)))
            .collect::<Vec<_>>()
            .join(",")
    }

    fn format(
        &self,
        ctx: &AssistContext<'_>,
        old_indent: IndentLevel,
        new_indent: IndentLevel,
    ) -> String {
        let mut fn_def = String::new();
        let params = self.make_param_list(ctx);
        let ret_ty = self.make_ret_ty(ctx);
        let (body, ends_with_comment) = self.make_body(old_indent);
        match ctx.config.snippet_cap {
            Some(_) => format_to!(fn_def, "\n\n{}$0{}({}) ->", new_indent, self.name, params),
            None => format_to!(fn_def, "\n\n{}{}({}) ->", new_indent, self.name, params),
        }
        fn_def.push_str(&body);

        if let Some(ret_ty) = ret_ty {
            format_to!(fn_def, ",\n    {}", ret_ty);
            fn_def.push('.');
        } else {
            // If the body ends with a comment, put the final `.` on a new
            // line.
            if ends_with_comment {
                fn_def.push_str("\n    .");
            } else {
                fn_def.push('.');
            }
        }
        fn_def
    }

    fn make_ret_ty(&self, ctx: &AssistContext<'_>) -> Option<String> {
        match self.outliving_locals.as_slice() {
            [] => None,
            [local] => Some(local.as_string(ctx.db().upcast())),
            vars => Some(format!(
                "{{{}}}", // open and closing braces, around vars
                vars.iter()
                    .map(|v| v.as_string(ctx.db().upcast()))
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
        }
    }

    fn make_body(&self, old_indent: IndentLevel) -> (String, bool) {
        let mut fun_str = String::default();
        let delta_indent = DEFAULT_INDENT_STEP - old_indent.0 as i8;

        let mut last_tok = None;
        let block = match &self.body {
            FunctionBody::Expr(expr) => {
                change_indent(DEFAULT_INDENT_STEP, format!("\n{}", expr.syntax()))
            }
            FunctionBody::Span { parent, text_range } => {
                let mut parts = " ".repeat(old_indent.0 as usize);
                parts = "\n".to_owned() + &parts;

                let has_comma = ends_with_comma_then_trivia(self.body.node(), *text_range);

                tokens(parent.syntax())
                    .filter(|it| {
                        text_range.contains_range(it.text_range())
                            // Remove comma before comment, if it exists we have its range.
                            && has_comma.map_or(true, |comma_range| comma_range != it.text_range())
                    })
                    .for_each(|it| {
                        parts.push_str(it.text());
                        last_tok = Some(it);
                    });
                change_indent(delta_indent, parts)
            }
        };

        fun_str.push_str(&block);
        (
            fun_str,
            last_tok.map_or(false, |t| t.kind() == SyntaxKind::COMMENT),
        )
    }
}

fn tokens(node: &SyntaxNode) -> impl Iterator<Item = SyntaxToken> {
    node.descendants_with_tokens()
        .filter_map(|element| element.into_token())
}

// ---------------------------------------------------------------------
#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::tests::check_assist;
    use crate::tests::check_assist_not_applicable;
    use crate::tests::check_assist_with_user_input;

    #[test]
    fn no_args_from_binary_expr() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
foo() ->
    foo(~1 + 1~).
"#,
            expect![[r#"
                foo() ->
                    foo(fun_name_edited()).

                $0fun_name_edited() ->
                    1 + 1.
            "#]],
        );
    }

    #[test]
    fn no_args_last_expr() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
    foo() ->
        K = 1,
        ~M = 1,
        M + 1~.
    "#,
            expect![[r#"
                foo() ->
                    K = 1,
                    fun_name_edited().

                $0fun_name_edited() ->
                    M = 1,
                    M + 1.
            "#]],
        );
    }

    #[test]
    fn no_args_not_last_expr() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
    foo() ->
        K = 1,
        ~foo(),
        baz:bar()~,
        K + 1.
    "#,
            expect![[r#"
                foo() ->
                    K = 1,
                    fun_name_edited(),
                    K + 1.

                $0fun_name_edited() ->
                    foo(),
                    baz:bar().
            "#]],
        );
    }

    #[test]
    fn extract_with_args_no_return() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
    foo() ->
        K = 1,
        M = 1,
        ~M + 1~,
        ok.
    "#,
            expect![[r#"
                foo() ->
                    K = 1,
                    M = 1,
                    fun_name_edited(M),
                    ok.

                $0fun_name_edited(M) ->
                    M + 1.
            "#]],
        );
    }

    #[test]
    fn extract_with_args_return_single() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
    foo() ->
        N = 1,
        V = ~N * N,~
        V + 1.
    "#,
            expect![[r#"
                foo() ->
                    N = 1,
                    V = fun_name_edited(N),
                    V + 1.

                $0fun_name_edited(N) ->
                    V = N * N,
                    V.
            "#]],
        );
    }

    #[test]
    fn extract_with_args_return_multiple() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
    foo() ->
        N = 1,
        ~V = N * N,
        J = 1~,
        V + J.
    "#,
            expect![[r#"
                foo() ->
                    N = 1,
                    {V, J} = fun_name_edited(N),
                    V + J.

                $0fun_name_edited(N) ->
                    V = N * N,
                    J = 1,
                    {V, J}.
            "#]],
        );
    }

    #[test]
    fn extract_with_args_return_what_is_needed() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
    foo() ->
        N = 1,
        ~V = N * N,
        J = 1~,
        V + 3.
    "#,
            expect![[r#"
                foo() ->
                    N = 1,
                    V = fun_name_edited(N),
                    V + 3.

                $0fun_name_edited(N) ->
                    V = N * N,
                    J = 1,
                    V.
            "#]],
        );
    }

    #[test]
    fn extract_does_not_clash_name1() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
    foo() ->
        ~N = 1~.

    fun_name() -> ok.
    "#,
            expect![[r#"
                foo() ->
                    fun_name1_edited().

                $0fun_name1_edited() ->
                    N = 1.

                fun_name() -> ok.
            "#]],
        );
    }

    #[test]
    fn extract_does_not_clash_name2() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
    -import(bar, [fun_name/0]).
    foo() ->
        ~N = 1~.
    "#,
            expect![[r#"
                -import(bar, [fun_name/0]).
                foo() ->
                    fun_name1_edited().

                $0fun_name1_edited() ->
                    N = 1.
            "#]],
        );
    }

    #[test]
    fn extract_preserves_internal_comments() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
    foo() ->
        N = 1,
        ~V = N * N,
        %% A comment
        J = 1~,
        V + 3.
    "#,
            expect![[r#"
                foo() ->
                    N = 1,
                    V = fun_name_edited(N),
                    V + 3.

                $0fun_name_edited(N) ->
                    V = N * N,
                    %% A comment
                    J = 1,
                    V.
            "#]],
        );
    }

    #[test]
    fn in_comment_is_not_applicable() {
        check_assist_not_applicable(
            extract_function,
            r#"main() -> 1 + %% ~comment~
                1."#,
        );
    }

    #[test]
    fn extract_does_not_tear_comments_apart1() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
    foo() ->
        %% ~ comment1
        foo(),
        foo(),
        %% ~ comment2
        bar().
    "#,
            expect![[r#"
                foo() ->
                    fun_name_edited(),
                    bar().

                $0fun_name_edited() ->
                    %%  comment1
                    foo(),
                    foo()
                    %%  comment2
                    .
            "#]],
        );
    }

    #[test]
    fn extract_does_not_tear_comments_apart2() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
    foo() ->
        %% ~ comment1
        foo(),
        foo()
        %% ~ comment2
        .
    "#,
            expect![[r#"
                foo() ->
                    fun_name_edited()
                    .

                $0fun_name_edited() ->
                    %%  comment1
                    foo(),
                    foo()
                    %%  comment2
                    .
            "#]],
        );
    }

    #[test]
    fn extract_function_copies_comment_at_start() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
   func()->
       I = 0,
       ~%% comment here!
       X = 0.~
   "#,
            expect![[r#"
                func()->
                    I = 0,
                    fun_name_edited().

                $0fun_name_edited() ->
                    %% comment here!
                    X = 0.
            "#]],
        );
    }

    #[test]
    fn extract_function_copies_comment_at_end() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
       func() ->
           I = 0,
           ~X = 0,
           %% comment here!~
           foo().
       "#,
            expect![[r#"
                func() ->
                    I = 0,
                    fun_name_edited(),
                    foo().

                $0fun_name_edited() ->
                    X = 0
                    %% comment here!
                    .
            "#]],
        );
    }
    #[test]
    fn extract_function_copies_comment_indented() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
       func() ->
           I = 0,
           ~X = 0,
           begin
               %% comment here!
               A = 3
           end~.
       "#,
            expect![[r#"
                func() ->
                    I = 0,
                    fun_name_edited().

                $0fun_name_edited() ->
                    X = 0,
                    begin
                        %% comment here!
                        A = 3
                    end.
            "#]],
        );
    }

    #[test]
    fn extract_subject_of_case() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
    foo() ->
        X = 1,
        case ~X + 3~ of
          _ -> ok
        end,
        foo().
    "#,
            expect![[r#"
                foo() ->
                    X = 1,
                    case fun_name_edited(X) of
                      _ -> ok
                    end,
                    foo().

                $0fun_name_edited(X) ->
                    X + 3.
            "#]],
        );
    }

    #[test]
    fn extract_already_complex_return() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
    foo() ->
        X = 1,
        ~{ok, Bar} = case X of
                      _ -> {ok, X+2}
                    end,
        J = Bar + X + 2,~
        J + Bar.
    "#,
            expect![[r#"
                foo() ->
                    X = 1,
                    {J, Bar} = fun_name_edited(X),
                    J + Bar.

                $0fun_name_edited(X) ->
                    {ok, Bar} = case X of
                                  _ -> {ok, X+2}
                                end,
                    J = Bar + X + 2,
                    {J, Bar}.
            "#]],
        );
    }

    #[test]
    fn extract_expression_in_case_arm() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
    foo() ->
        X = 1,
        {ok, Bar} = case X of
                      _ -> {ok, ~X+2~}
                    end,
        J = Bar + X + 2,
        J + Bar.
    "#,
            expect![[r#"
                foo() ->
                    X = 1,
                    {ok, Bar} = case X of
                                  _ -> {ok, fun_name_edited(X)}
                                end,
                    J = Bar + X + 2,
                    J + Bar.

                $0fun_name_edited(X) ->
                    X+2.
            "#]],
        );
    }

    #[test]
    fn extract_partial_block() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
       foo() ->
           M = 2,
           N = 1,
           V = M ~* N,
           W = 3,~
           V + W.
       "#,
            expect![[r#"
                foo() ->
                    M = 2,
                    N = 1,
                    {V, W} = fun_name_edited(N,M),
                    V + W.

                $0fun_name_edited(N,M) ->
                    V = M * N,
                    W = 3,
                    {V, W}.
            "#]],
        );
    }

    #[test]
    fn extract_in_macro_def_rhs() {
        check_assist_not_applicable(
            extract_function,
            r#"
       -define(FOO(X), ~X+3~).
       "#,
        );
    }

    #[test]
    fn extract_record_field_1() {
        check_assist_not_applicable(
            extract_function,
            r#"
             foo(X) ->
                 #?REQ_ARGS_STRUCT_NAME{
                    ~request = Normal#?REQ_STRUCT_NAME{
                         field_name = undefined
                     }~
                 }.
             "#,
        );
    }

    #[test]
    fn extract_record_field_2() {
        check_assist_not_applicable(
            extract_function,
            r#"
             foo(X) ->
                 #?REQ_ARGS_STRUCT_NAME{
                    ~request~ = Normal#?REQ_STRUCT_NAME{
                         field_name = undefined
                     }
                 }.
             "#,
        );
    }

    #[test]
    fn user_input_fun_name() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
foo() ->
    foo(~1 + 1~).
"#,
            expect![[r#"
                foo() ->
                    foo(fun_name_edited()).

                $0fun_name_edited() ->
                    1 + 1.
            "#]],
        );
    }

    #[test]
    fn check_new_name_is_safe() {
        check_assist_with_user_input(
            extract_function,
            "Extract into function",
            "foo",
            r#"
             foo() ->
                 foo(~1 + 1~).
             "#,
            expect![[r#"
                foo() ->
                    foo(foo_0()).

                $0foo_0() ->
                    1 + 1.
            "#]],
        );
    }

    #[test]
    fn check_new_name_is_safe_checks_arity() {
        check_assist_with_user_input(
            extract_function,
            "Extract into function",
            "foo",
            r#"
             foo(X) ->
                 bar(~1 + 1~).
             "#,
            expect![[r#"
                foo(X) ->
                    bar(foo()).

                $0foo() ->
                    1 + 1.
            "#]],
        );
    }

    #[test]
    fn check_new_name_is_safe_checks_erlang_auto() {
        check_assist_with_user_input(
            extract_function,
            "Extract into function",
            "date",
            r#"
             foo(X) ->
                 bar(~1 + 1~).
             "#,
            expect![[r#"
                foo(X) ->
                    bar(date_0()).

                $0date_0() ->
                    1 + 1.
            "#]],
        );
    }

    #[test]
    fn underscores() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
             f() ->
                 _ = 42,
                 ~{ok, _} = {ok, 42}~.
             "#,
            expect![[r#"
                f() ->
                    _ = 42,
                    fun_name_edited().

                $0fun_name_edited() ->
                    {ok, _} = {ok, 42}.
            "#]],
        );
    }

    #[test]
    // T147302206
    fn trailing_comma() {
        check_assist(
            extract_function,
            "Extract into function",
            r#"
               version(Application) ->
                   ~{ok, Version} = application:get_key(Application, vsn)~,
                   Version.
             "#,
            expect![[r#"
                version(Application) ->
                    Version = fun_name_edited(Application),
                    Version.

                $0fun_name_edited(Application) ->
                    {ok, Version} = application:get_key(Application, vsn),
                    Version.
            "#]],
        );
    }
}

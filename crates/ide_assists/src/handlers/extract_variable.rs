/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_ide_db::assists::AssistUserInput;
use elp_ide_db::assists::AssistUserInputType;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::NodeOrToken;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use hir::InFile;
use stdx::format_to;

use crate::assist_context::AssistContext;
use crate::assist_context::Assists;
use crate::helpers::freshen_variable_name;
use crate::helpers::suggest_name_for_variable;

// Assist: extract_variable
//
// Extracts subexpression into a variable.
//
// ```
// foo() ->
//     $0(1 + 2)$0 * 4.
// ```
// ->
// ```
// foo() ->
//     $0VarName = (1 + 2),
//     VarName * 4.
// ```
// Note: $0 is the snippet language encoding of cursor ranges and positions.
pub(crate) fn extract_variable(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    if ctx.has_empty_selection() {
        return None;
    }

    let node = match ctx.covering_element() {
        NodeOrToken::Node(it) => it,
        NodeOrToken::Token(it) if it.kind() == SyntaxKind::COMMENT => {
            return None;
        }
        NodeOrToken::Token(it) => it.parent()?,
    };
    let node = node
        .ancestors()
        .take_while(|anc| anc.text_range() == node.text_range())
        .last()?;
    if !valid_extraction(&node) {
        return None;
    }
    let to_extract = node
        .descendants()
        .take_while(|it| ctx.selection_trimmed().contains_range(it.text_range()))
        .find_map(valid_target_expr)?;

    let anchor = Anchor::from(&to_extract)?;
    let target = to_extract.syntax().text_range();
    let indent = anchor.syntax().prev_sibling_or_token()?.as_token()?.clone();
    acc.add(
        AssistId("extract_variable", AssistKind::RefactorExtract),
        "Extract into variable",
        target,
        Some(AssistUserInput {
            input_type: AssistUserInputType::Variable,
            value: suggest_name_for_variable(&to_extract, &ctx.sema),
        }),
        move |edit| {
            let vars_in_clause = ctx
                .sema
                .find_vars_in_clause_ast(&InFile::new(ctx.file_id(), &to_extract));
            let var_name = freshen_variable_name(
                &ctx.sema,
                ctx.user_input_or(|| suggest_name_for_variable(&to_extract, &ctx.sema)),
                &vars_in_clause,
            );
            let expr_range = to_extract.syntax().text_range();

            let mut buf = String::new();
            format_to!(buf, "{} = {}", var_name, to_extract.syntax());

            buf.push(',');

            // We want to maintain the indent level,
            // but we do not want to duplicate possible
            // extra newlines in the indent block
            let text = indent.text();
            if text.starts_with('\n') {
                buf.push('\n');
                buf.push_str(text.trim_start_matches('\n'));
            } else {
                buf.push_str(text);
            }

            edit.replace(expr_range, var_name.clone());
            let offset = anchor.syntax().text_range().start();
            match ctx.config.snippet_cap {
                Some(cap) => {
                    let snip = buf.replace(&var_name.to_string(), &format!("$0{}", var_name));
                    edit.insert_snippet(cap, offset, snip)
                }
                None => edit.insert(offset, buf),
            }
        },
    )
}

/// Check whether the node is a valid expression which can be
/// extracted to a variable.  In general that's true for any
/// expression, but in some cases that would produce invalid code.
fn valid_target_expr(node: SyntaxNode) -> Option<ast::Expr> {
    ast::Expr::cast(node)
}

/// Check whether the node is a valid expression which can be
/// extracted to a variable.  In general that's true for any
/// expression, but in some cases that would produce invalid code.
fn valid_extraction(node: &SyntaxNode) -> bool {
    node.kind() != SyntaxKind::RECORD_FIELD
}

#[derive(Debug)]
struct Anchor(SyntaxNode);

impl Anchor {
    fn from(to_extract: &ast::Expr) -> Option<Anchor> {
        to_extract
            .syntax()
            .ancestors()
            // Do not ascend beyond the current declaration
            .take_while(|it| {
                !ast::ClauseBody::can_cast(it.kind()) || ast::MacroCallExpr::can_cast(it.kind())
            })
            .find_map(|node| {
                if ast::MacroCallExpr::can_cast(node.kind()) {
                    return None;
                }

                if let Some(_expr) = node.parent().and_then(ast::ClauseBody::cast) {
                    return Some(Anchor(node));
                }

                None
            })
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::tests::check_assist;
    use crate::tests::check_assist_not_applicable;
    use crate::tests::check_assist_with_user_input;

    #[test]
    fn test_extract_var_simple() {
        check_assist(
            extract_variable,
            "Extract into variable",
            r#"
foo() ->
  ~(1 + 2)~ * 4.
"#,
            expect![[r#"
                foo() ->
                  $0VarNameEdited = (1 + 2),
                  VarNameEdited * 4.
            "#]],
        );
    }

    #[test]
    fn test_extract_var_case_rhs() {
        check_assist(
            extract_variable,
            "Extract into variable",
            r#"
foo(X) ->
  case X of
    1 -> ~3 + X~;
    _ -> X
  end.
"#,
            expect![[r#"
                foo(X) ->
                  case X of
                    1 -> $0VarNameEdited = 3 + X, VarNameEdited;
                    _ -> X
                  end.
            "#]],
        );
    }

    #[test]
    fn test_extract_var_case_expr() {
        check_assist(
            extract_variable,
            "Extract into variable",
            r#"
foo(X) ->
  case ~X + 2~ of
    1 -> 3 + X;
    _ -> X
  end.
"#,
            expect![[r#"
                foo(X) ->
                  $0VarNameEdited = X + 2,
                  case VarNameEdited of
                    1 -> 3 + X;
                    _ -> X
                  end.
            "#]],
        );
    }

    #[test]
    fn test_extract_var_in_record_1() {
        check_assist_not_applicable(
            extract_variable,
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
    fn test_extract_var_in_record_2() {
        check_assist(
            extract_variable,
            "Extract into variable",
            r#"
             foo(X) ->
                 #?REQ_ARGS_STRUCT_NAME{
                    request = Normal#?REQ_STRUCT_NAME{
                         field_name = ~undefined~
                     }
                 }."#,
            expect![[r#"
                foo(X) ->
                    $0VarNameEdited = undefined,
                    #?REQ_ARGS_STRUCT_NAME{
                       request = Normal#?REQ_STRUCT_NAME{
                            field_name = VarNameEdited
                        }
                    }."#]],
        );
    }

    #[test]
    fn test_extract_var_in_record_3() {
        check_assist(
            extract_variable,
            "Extract into variable",
            r#"
             foo(X) ->
                 #?REQ_ARGS_STRUCT_NAME{
                    request = ~Normal~#?REQ_STRUCT_NAME{
                         field_name = undefined
                     }
                 }."#,
            expect![[r#"
                foo(X) ->
                    $0VarNameEdited = Normal,
                    #?REQ_ARGS_STRUCT_NAME{
                       request = VarNameEdited#?REQ_STRUCT_NAME{
                            field_name = undefined
                        }
                    }."#]],
        );
    }

    #[test]
    fn test_extract_var_in_record_4() {
        check_assist(
            extract_variable,
            "Extract into variable",
            r#"
             foo(X) ->
                    #record_name{field_name = ~6~}."#,
            expect![[r#"
                foo(X) ->
                       $0VarNameEdited = 6,
                       #record_name{field_name = VarNameEdited}."#]],
        );
    }

    #[test]
    fn test_extract_var_in_record_5() {
        check_assist(
            extract_variable,
            "Extract into variable",
            r#"
             foo(X) -> X#record_name.~field_name~."#,
            expect!["foo(X) -> $0VarNameEdited = field_name, X#record_name.VarNameEdited."],
        );
    }

    #[test]
    fn check_new_name_is_safe() {
        check_assist_with_user_input(
            extract_variable,
            "Extract into variable",
            "NameClash",
            r#"
             foo() ->
               NameClash = 3,
               ~(1 + 2)~ * 4.
             "#,
            expect![[r#"
                foo() ->
                  NameClash = 3,
                  $0NameClash0 = (1 + 2),
                  NameClash0 * 4.
            "#]],
        );
    }
}

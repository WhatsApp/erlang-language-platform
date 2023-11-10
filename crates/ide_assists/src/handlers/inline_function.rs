/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::iter::zip;
use std::sync::Arc;

use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::find_best_token;
use elp_ide_db::rename::SafetyChecks;
use elp_ide_db::SearchScope;
use elp_ide_db::SymbolClass;
use elp_ide_db::SymbolDefinition;
use elp_syntax::ast;
use elp_syntax::ast::edit::IndentLevel;
use elp_syntax::ast::HasArity;
use elp_syntax::AstNode;
use elp_syntax::NodeOrToken;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use elp_syntax::SyntaxToken;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use fxhash::FxHashSet;
use hir::FunctionClauseBody;
use hir::FunctionDef;
use hir::InFile;
use hir::InFunctionClauseBody;
use hir::Pat;
use hir::ScopeAnalysis;
use hir::Semantic;
use hir::Var;
use itertools::izip;
use text_edit::TextEdit;

use crate::assist_context::AssistContext;
use crate::assist_context::Assists;
use crate::helpers::change_indent;
use crate::helpers::parens_needed;
use crate::helpers::ranges_for_delete_function;
use crate::helpers::simple_param_vars;
use crate::helpers::DEFAULT_INDENT_STEP;

// Assist: inline_function
//
// Replaces the occurrence of a function call with its definition
//
// ```
// foo(B) -> 3 + B.
// bar() -> foo(4).
// ```
// ->
// ```
// bar() -> 3 + 4.
// ```
pub(crate) fn inline_function(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    let InlineData {
        fun,
        delete_definition,
        target,
        references,
    } = can_inline_function(ctx)?;

    let usages = SymbolDefinition::Function(fun.clone()).usages(&ctx.sema);
    if !usages.at_least_one() {
        return None;
    }

    let is_recursive_fn = usages
        .clone()
        .set_scope(&SearchScope::file_range(FileRange {
            file_id: fun.file.file_id,
            range: fun.source(ctx.db().upcast()).syntax().text_range(),
        }))
        .at_least_one();
    if is_recursive_fn {
        cov_mark::hit!(inline_function_recursive);
        return None;
    }

    if !is_safe(ctx, &fun, &references) {
        cov_mark::hit!(inline_function_is_safe);
        return None;
    }

    acc.add(
        AssistId("inline_function", AssistKind::RefactorInline),
        "Inline function",
        target,
        None,
        move |builder| {
            let file_id = ctx.frange.file_id;
            let ast_fun = fun.source(ctx.db().upcast());
            if delete_definition {
                if let Some(delete_ranges) = ranges_for_delete_function(ctx, &ast_fun) {
                    delete_ranges.delete(builder);
                }
            }
            for call in references {
                let infile_ast_fun = InFile::new(fun.file.file_id, &ast_fun);
                let function_body = ctx
                    .db()
                    .function_body(InFile::new(fun.file.file_id, fun.function_id));
                if ast_fun.clauses().count() == 1 {
                    if let Some(ast::FunctionOrMacroClause::FunctionClause(ast_clause)) =
                        ast_fun.clauses().next()
                    {
                        if let Some(clause) =
                            function_body
                                .clauses
                                .iter()
                                .next()
                                .map(|(clause_id, clause)| {
                                    InFunctionClauseBody::new(
                                        clause.clone(),
                                        function_body.function_id,
                                        clause_id,
                                        None,
                                        clause,
                                    )
                                })
                        {
                            if let Some((range, replacement)) = inline_single_function_clause(
                                &ctx.sema,
                                file_id,
                                &infile_ast_fun,
                                &ast_clause,
                                &call,
                                &clause,
                            ) {
                                builder.replace(range, replacement)
                            }
                        }
                    };
                } else if let Some((range, replacement)) =
                    inline_function_as_case(&infile_ast_fun, &call)
                {
                    builder.replace(range, replacement)
                }
            }
        },
    )
}

/// Function has multiple clauses, convert them to a case statement
fn inline_function_as_case(
    fun: &InFile<&ast::FunDecl>,
    call: &ast::Call,
) -> Option<(TextRange, String)> {
    let mut clauses = Vec::default();
    let end_idx = fun.value.clauses().count() - 1;
    let params = params_text(&call.args()?)?;
    clauses.push("".to_string());
    clauses.push(format!("case {params} of"));
    for (idx, clause) in fun.value.clauses().enumerate() {
        match clause {
            ast::FunctionOrMacroClause::FunctionClause(clause) => {
                // Turn the clause into a case
                // foo(Args) -> bar;
                // -->
                //     Args -> bar;
                let params = params_text(&clause.args()?)?;
                let guards = guards_text(clause.guard()).unwrap_or_default();
                let (body, _offset, body_indent) = clause_body_text_with_intro(&clause)?;
                let delta_indent = body_indent.0 as i8;
                let body = change_indent(delta_indent, body);
                let case_clause = if idx != end_idx {
                    format!("  {params} {guards}{body};")
                } else {
                    format!("  {params} {guards}{body}")
                };
                clauses.push(case_clause);
            }
            ast::FunctionOrMacroClause::MacroCallExpr(_) => return None,
        }
    }
    clauses.push("end".to_string());
    let old_indent = IndentLevel::from_node(call.syntax());
    let delta_indent = old_indent.0 as i8 + DEFAULT_INDENT_STEP;
    let replacement_range = if clauses[0].is_empty() {
        call_replacement_range(call)
    } else {
        call.syntax().text_range()
    };
    Some((
        replacement_range,
        change_indent(delta_indent, clauses.join("\n")),
    ))
}

/// When a call is replaced by something starting on a new line, extend
/// the range to exclude any trailing whitespace at the call site.
fn call_replacement_range(call: &ast::Call) -> TextRange {
    fn get_start(call: &ast::Call) -> Option<TextRange> {
        let call_range = call.syntax().text_range();
        let mut token = call.syntax().first_token()?.prev_token()?;
        let mut start_pos = call_range.start();
        while token.kind() == SyntaxKind::WHITESPACE {
            start_pos = token.text_range().start();
            token = token.prev_token()?;
        }
        Some(TextRange::new(start_pos, call.syntax().text_range().end()))
    }

    get_start(call).unwrap_or_else(|| call.syntax().text_range())
}

/// Inline a function having a single clause.
fn inline_single_function_clause(
    sema: &Semantic,
    file_id: FileId,
    ast_fun: &InFile<&ast::FunDecl>,
    ast_clause: &ast::FunctionClause,
    call: &ast::Call,
    clause: &InFunctionClauseBody<&Arc<FunctionClauseBody>>,
) -> Option<(TextRange, String)> {
    if ast_clause.guard().is_some() {
        inline_function_as_case(ast_fun, call)
    } else if ast_clause.body()?.exprs().count() == 1
        && !has_vars_in_clause(sema, ast_fun.file_id, ast_clause)
    {
        inline_simple_function_clause(sema, file_id, ast_clause, call)
    } else {
        inline_single_function_clause_with_begin(ast_clause, call, clause)
    }
}

fn inline_single_function_clause_with_begin(
    ast_clause: &ast::FunctionClause,
    call: &ast::Call,
    clause: &InFunctionClauseBody<&Arc<FunctionClauseBody>>,
) -> Option<(TextRange, String)> {
    let (edited_text, _offset) = clause_body_text(ast_clause)?;

    let body_indent = IndentLevel(DEFAULT_INDENT_STEP as u8);
    let mut final_text = String::default();
    final_text.push_str("\nbegin");

    assign_params_for_begin(&mut final_text, body_indent, ast_clause, call, clause)?;
    if !edited_text.starts_with('\n') {
        final_text.push_str((format!("\n{body_indent}")).as_str());
        final_text.push_str(&edited_text);
    } else {
        // final text starts on a new line, as a block. Adjust its
        // indentation.

        let indent = edited_text
            .chars()
            .skip(1)
            .take_while(|c| *c == ' ')
            .count();
        let delta_indent = body_indent.0 as i8 - indent as i8;
        final_text.push_str(&change_indent(delta_indent, edited_text));
    }
    final_text.push_str("\nend");

    let old_indent = IndentLevel::from_node(call.syntax());
    let delta_indent = old_indent.0 as i8 + DEFAULT_INDENT_STEP;
    let replacement_range = if final_text.starts_with('\n') {
        call_replacement_range(call)
    } else {
        call.syntax().text_range()
    };
    Some((replacement_range, change_indent(delta_indent, final_text)))
}

fn assign_params_for_begin(
    final_text: &mut String,
    body_indent: IndentLevel,
    ast_clause: &ast::FunctionClause,
    call: &ast::Call,
    clause: &InFunctionClauseBody<&Arc<FunctionClauseBody>>,
) -> Option<()> {
    let arity = ast_clause.arity_value()?;
    izip!(
        call.args()?.args(),
        ast_clause.args()?.args(),
        &clause.value.clause.pats
    )
    .enumerate()
    .for_each(|(idx, (val, var, pat))| {
        let is_single_var = if let Pat::Var(_) = &clause[*pat] {
            true
        } else {
            false
        };
        if idx == 0 {
            if let Some(leading_comments) = get_val_preceding_comments(val.syntax()) {
                final_text.push_str((format!("\n{body_indent}{leading_comments}")).as_str());
            }
        };

        let var_str = var.syntax().text();
        let (has_comma, val, comments) =
            if let Some((has_comma, comments)) = get_val_trailing_comments(val.syntax()) {
                (has_comma, val, comments)
            } else {
                (false, val, "".to_string())
            };

        if !comments.is_empty() || !is_single_var || (var_str.to_string() != val.to_string()) {
            final_text.push_str(format!("\n{body_indent}{var_str} = ").as_str());

            if idx == arity - 1 {
                // last one. Will not have a comma, but we insert one before the comment
                final_text.push_str(format!("{val},{comments}").as_str());
            } else {
                final_text.push_str(format!("{val}{comments}").as_str());
                if !has_comma {
                    final_text.push(',');
                }
            }
        }
    });
    Some(())
}

fn get_val_trailing_comments(syntax: &SyntaxNode) -> Option<(bool, String)> {
    let mut token = syntax.last_token()?.next_token()?;
    let mut has_comma = false;
    let mut has_comment = false;
    let mut res = String::default();
    while token.kind() == SyntaxKind::COMMENT
        || token.kind() == SyntaxKind::ANON_COMMA
        || token.kind() == SyntaxKind::WHITESPACE
    {
        res.push_str(&token.text().to_string().clone());
        if token.kind() == SyntaxKind::ANON_COMMA {
            has_comma = true;
        }
        if token.kind() == SyntaxKind::COMMENT {
            has_comment = true;
        }
        token = token.next_token()?;
    }

    if has_comment {
        Some((has_comma, res.trim_end().to_string()))
    } else {
        None
    }
}

fn get_val_preceding_comments(syntax: &SyntaxNode) -> Option<String> {
    let mut token = syntax.first_token()?.prev_token()?;
    let mut res = Vec::default();
    while token.kind() == SyntaxKind::COMMENT
        || token.kind() == SyntaxKind::ANON_COMMA
        || token.kind() == SyntaxKind::WHITESPACE
    {
        res.push(token.text().to_string().clone());
        token = token.prev_token()?;
    }
    res.reverse();
    let res = res.join("").trim().to_string();
    if res.is_empty() { None } else { Some(res) }
}

fn inline_simple_function_clause(
    sema: &Semantic,
    file_id: FileId,
    clause: &ast::FunctionClause,
    call: &ast::Call,
) -> Option<(TextRange, String)> {
    // We need to adjust all the edits to skip the start of the file
    let (mut edited_text, offset) = clause_body_text(clause)?;
    let mut changes = Vec::default();
    zip(call.args()?.args(), clause.args()?.args()).for_each(|(val, var)| {
        let var_syntax = var.syntax();

        let defs = if let Some(v) = ast::Var::cast(var_syntax.clone()) {
            let def = sema.to_def::<ast::Var>(InFile { file_id, value: &v });
            if let Some(defs) = def {
                match defs {
                    hir::DefinitionOrReference::Definition(def) => {
                        Some(vec![SymbolDefinition::Var(def)])
                    }
                    hir::DefinitionOrReference::Reference(defs) => Some(
                        defs.into_iter()
                            .map(SymbolDefinition::Var)
                            .collect::<Vec<_>>(),
                    ),
                }
            } else {
                None
            }
        } else {
            None
        };
        if let Some(defs) = defs {
            let base_name = val.syntax().text().to_string();
            let parened_name = format!("({})", base_name);
            for def in defs {
                match def.rename(
                    sema,
                    &|mvar| {
                        param_substitution(mvar, base_name.clone(), parened_name.clone(), &val)
                            .unwrap_or_else(|| base_name.clone())
                    },
                    SafetyChecks::No,
                ) {
                    Ok(change) => {
                        changes.push(change);
                    }
                    Err(err) => {
                        log::info!("got rename err: {err}");
                    }
                }
            }
        }
    });
    // Build the final edit, accounting for the fact that we are only
    // using the clause body for the replacement, so we need to apply
    // an offset to each edit calculated from editing the whole
    // function.
    let mut builder = TextEdit::builder();
    changes.iter().for_each(|change| {
        change.source_file_edits.values().for_each(|edit| {
            if let Some(edit) = apply_offset(edit, offset) {
                edit.iter().for_each(|(delete, insert)| {
                    builder.replace(*delete, insert.clone());
                });
            }
        })
    });

    let edit = builder.finish();
    edit.apply(&mut edited_text);

    let old_indent = IndentLevel::from_node(call.syntax());
    let delta_indent = old_indent.0 as i8 + DEFAULT_INDENT_STEP;
    let replacement_range = if edited_text.starts_with('\n') {
        call_replacement_range(call)
    } else {
        call.syntax().text_range()
    };
    Some((replacement_range, change_indent(delta_indent, edited_text)))
}

fn param_substitution(
    mvar: Option<&ast::Name>,
    base_name: String,
    parened_name: String,
    val: &ast::Expr,
) -> Option<String> {
    if let Some(ast::Name::Var(var)) = mvar {
        let (_, needed) = parens_needed(val, var)?;
        if needed {
            Some(parened_name)
        } else {
            Some(base_name)
        }
    } else {
        Some(base_name)
    }
}

fn has_vars_in_clause(sema: &Semantic, file_id: FileId, fun_clause: &ast::FunctionClause) -> bool {
    let has_vars = || -> Option<bool> {
        let ast_expr = fun_clause.body()?.exprs().next()?;
        let vars = sema.find_vars_in_clause_ast(&InFile::new(file_id, &ast_expr))?;
        Some(!vars.is_empty())
    }();

    has_vars.unwrap_or(true)
}

fn apply_offset(edit: &TextEdit, offset: TextSize) -> Option<Vec<(TextRange, String)>> {
    Some(
        edit.iter()
            .filter_map(|te| {
                Some((
                    TextRange::new(
                        te.delete.start().checked_sub(offset)?,
                        te.delete.end().checked_sub(offset)?,
                    ),
                    te.insert.clone(),
                ))
            })
            .collect::<Vec<_>>(),
    )
}

/// Given the clause of a single-clause function, return the text in
/// its body.
fn clause_body_text_with_intro(
    clause: &ast::FunctionClause,
) -> Option<(String, TextSize, IndentLevel)> {
    // Get the text of the clause body, including any comments between
    // the `->` and first expression, but excluding the `->` (ANON_DASH_GT).
    let mut offset = None;
    let token = clause.body()?.syntax().last_token()?.next_token()?;
    let trailing_comments = get_trailing_comments(token)?;
    let mut body_text = clause
        .body()?
        .syntax()
        .descendants_with_tokens()
        .skip(1) // Skip the entire node
        .filter_map(|n| match n {
            NodeOrToken::Token(t) => {
                if offset.is_none() {
                    offset = Some(t.text_range().start())
                }
                Some(t.text().to_string())
            }
            NodeOrToken::Node(_) => None,
        })
        .collect::<Vec<_>>()
        .join("");
    body_text.push_str(&trailing_comments);

    let indent = clause
        .body()?
        .syntax()
        .descendants_with_tokens()
        .skip(1) // Skip the entire node
        .skip_while(|n| match n {
            NodeOrToken::Token(t) => t.kind() != SyntaxKind::WHITESPACE,
            NodeOrToken::Node(_) => true,
        })
        .skip(1) // skip the whitespace
        .find_map(|n| match n {
            NodeOrToken::Token(t) => Some(IndentLevel::from_token(&t)),
            NodeOrToken::Node(_) => None,
        });
    Some((body_text, offset?, indent?))
}

fn clause_body_text(clause: &ast::FunctionClause) -> Option<(String, TextSize)> {
    // Get the text of the clause body, including any comments between
    // the `->` and first expression, but excluding the `->` (ANON_DASH_GT).
    let mut offset = None;
    let token = clause.body()?.syntax().last_token()?.next_token()?;
    let trailing_comments = get_trailing_comments(token)?;
    let mut body_text = clause
        .body()?
        .syntax()
        .descendants_with_tokens()
        .skip(2) // Skip the entire node, and initial ANON_DASH_GT token
        .skip_while(|n| match n {
            NodeOrToken::Token(t) => {
                if t.kind() == SyntaxKind::WHITESPACE {
                    !t.text().contains('\n')
                } else {
                    false
                }
            }
            NodeOrToken::Node(_) => true,
        })
        .filter_map(|n| match n {
            NodeOrToken::Token(t) => {
                if offset.is_none() {
                    offset = Some(t.text_range().start())
                }
                Some(t.text().to_string())
            }
            NodeOrToken::Node(_) => None,
        })
        .collect::<Vec<_>>()
        .join("");
    body_text.push_str(&trailing_comments);

    Some((body_text, offset?))
}

fn get_trailing_comments(mut token: SyntaxToken) -> Option<String> {
    let mut res = String::default();
    while token.kind() != SyntaxKind::ANON_DOT && token.kind() != SyntaxKind::ANON_SEMI {
        res.push_str(&token.text().to_string().clone());
        token = token.next_token()?;
    }
    Some(res)
}

fn params_text(args: &ast::ExprArgs) -> Option<String> {
    // As per grammar.js, ast::ExprArgs is always wrapped in parens
    let mut args_str = args
        .syntax()
        .text()
        .to_string()
        .strip_prefix('(')?
        .strip_suffix(')')?
        .trim()
        .to_string();

    // If we have a trailing comment, add a newline.
    // First, get last token, excluding trailing ')'
    let mut token = args.syntax().last_token()?.prev_token()?;
    while token.kind() == SyntaxKind::WHITESPACE {
        token = token.prev_token()?;
    }
    if token.kind() == SyntaxKind::COMMENT {
        args_str.push('\n');
    }

    if args.args().count() == 1 {
        Some(args_str)
    } else {
        Some(format!(
            "{{{}}}", // open and closing braces, around params
            args_str
        ))
    }
}

fn guards_text(guard: Option<ast::Guard>) -> Option<String> {
    let guard = guard?;

    // Step backwards to pick up the 'when' keyword, and any intervening whitespace/comments
    let mut token = guard.syntax().first_token()?.prev_token()?;
    let mut when = Vec::default();
    while token.kind() != SyntaxKind::ANON_WHEN {
        when.push(token.text().to_string().clone());
        token = token.prev_token()?;
    }
    when.push(token.text().to_string()); // Include 'when' token
    when.reverse();

    let guards = guard.syntax().text().to_string();
    when.push(guards);

    let mut trail = String::default();
    let mut token = guard.syntax().last_token()?.next_token()?;
    while token.kind() != SyntaxKind::ANON_DASH_GT {
        trail.push_str(&token.text().to_string().clone());
        token = token.next_token()?;
    }
    if trail.contains('\n') {
        trail.push_str("  ");
    }
    when.push(trail);
    Some(when.join(""))
}

struct InlineData {
    fun: FunctionDef,
    delete_definition: bool,
    target: TextRange,
    references: Vec<ast::Call>,
}

fn can_inline_function(ctx: &AssistContext) -> Option<InlineData> {
    let file_id = ctx.frange.file_id;
    let token = find_best_token(
        &ctx.sema,
        FilePosition {
            file_id,
            offset: ctx.offset(),
        },
    )?;
    let target = SymbolClass::classify(&ctx.sema, token)?;
    let def = match target {
        SymbolClass::Definition(SymbolDefinition::Function(fun)) => {
            Some(SymbolDefinition::Function(fun))
        }
        SymbolClass::Reference { refs, typ: _ } => {
            if let Some(SymbolDefinition::Function(fun)) = refs.iter().next() {
                Some(SymbolDefinition::Function(fun))
            } else {
                None
            }
        }
        _ => None,
    }?;

    let is_local_function = def.file().file_id == file_id;

    match &def {
        SymbolDefinition::Function(fun) => {
            let target = fun.source(ctx.db().upcast()).syntax().text_range();
            let usages = def.clone().usages(&ctx.sema).direct_only().all();
            let mut all_references = Vec::default();
            let mut selected_call_reference = Vec::default();
            usages.iter().for_each(|(_file_id, names)| {
                for name_like in names {
                    if let Some(call) = get_call(name_like.syntax()) {
                        if let Some(name) = call.expr() {
                            if name.syntax().text_range().contains(ctx.offset()) {
                                selected_call_reference.push(call.clone());
                            }
                        }
                        all_references.push(call.clone());
                    };
                }
            });
            let (references, delete_definition) = if selected_call_reference.is_empty() {
                // Cursor must be on a spec or function definition,
                // delete all references
                (all_references, !fun.exported)
            } else {
                (
                    selected_call_reference,
                    all_references.len() == 1 && !fun.exported,
                )
            };
            if references.is_empty() {
                None
            } else {
                Some(InlineData {
                    fun: fun.clone(),
                    delete_definition: is_local_function && delete_definition,
                    target,
                    references,
                })
            }
        }
        _ => None,
    }
}

// AZ:TODO: use hir::Call instead
fn get_call(syntax: &SyntaxNode) -> Option<ast::Call> {
    if let Some(call) = ast::Call::cast(syntax.parent()?) {
        Some(call)
    } else {
        ast::Call::cast(syntax.parent()?.parent()?)
    }
}

/// Check that all variables defined in the clauses of the `FunDecl`
/// are unused in the call location.  This excludes single-variable
/// parameters.
fn is_safe(ctx: &AssistContext, fun: &FunctionDef, references: &[ast::Call]) -> bool {
    fn check_is_safe(
        ctx: &AssistContext,
        fun: &FunctionDef,
        references: &[ast::Call],
    ) -> Option<bool> {
        let function_body = ctx
            .db()
            .function_body(InFile::new(fun.file.file_id, fun.function_id));
        let fun_vars = function_body
            .clauses
            .iter()
            .filter_map(|(clause_id, clause)| {
                ScopeAnalysis::clause_vars_in_scope(
                    &ctx.sema,
                    &InFunctionClauseBody::new(
                        clause.clone(),
                        function_body.function_id,
                        clause_id,
                        None,
                        (),
                    ),
                )
            })
            .fold(FxHashSet::default(), move |mut acc, new: FxHashSet<Var>| {
                acc.extend(new.into_iter());
                acc
            });
        let simple_param_vars = function_body
            .clauses
            .iter()
            .filter_map(|(clause_id, clause)| {
                simple_param_vars(&InFunctionClauseBody::new(
                    clause.clone(),
                    function_body.function_id,
                    clause_id,
                    None,
                    clause,
                ))
            })
            .fold(FxHashSet::default(), move |mut acc, new: FxHashSet<Var>| {
                acc.extend(new.into_iter());
                acc
            });
        let fun_vars: FxHashSet<Var> = fun_vars.difference(&simple_param_vars).copied().collect();

        // At each call site, check that no param clashes with the fun_vars.
        let file_id = ctx.file_id();
        let ok = references.iter().all(|call| {
            // clause_vars is all the top-level vars in the function
            // clause containing the call, before and after it, at the
            // top level.

            let expr = ast::Expr::Call(call.clone());
            let clause_vars = || -> Option<FxHashSet<Var>> {
                let call_function = ctx.sema.to_expr(InFile::new(ctx.file_id(), &expr))?;
                ScopeAnalysis::clause_vars_in_scope(&ctx.sema, &call_function.with_value(()))
            }()
            .unwrap_or_default();

            // We also need to check in the actual scope we are
            // currently in. e.g. a catch clause, where vars are not exported
            if let Some(mut vars) = call_vars_in_scope(&ctx.sema, file_id, call) {
                vars.extend(clause_vars.iter());
                vars.intersection(&fun_vars).count() == 0
            } else {
                false
            }
        });
        Some(ok)
    }
    check_is_safe(ctx, fun, references).unwrap_or(false)
}

fn call_vars_in_scope(
    sema: &Semantic,
    file_id: FileId,
    call: &ast::Call,
) -> Option<FxHashSet<Var>> {
    let resolver = sema.function_clause_resolver(file_id, call.syntax())?;
    let call_expr_id = resolver.expr_id_ast(
        sema.db,
        InFile::new(file_id, &ast::Expr::Call(call.clone())),
    )?;
    let scope = resolver.value.scopes.scope_for_expr(call_expr_id)?;
    let vars = resolver.value.all_vars_in_scope(scope);
    Some(vars)
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::tests::*;

    #[test]
    fn test_inline_function_no_params_from_usage_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo() -> ok.
              bar() -> f~oo()."#,
            expect![[r#"
                bar() -> ok."#]],
        )
    }

    #[test]
    fn test_inline_function_no_params_from_definition_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              fo~o() -> ok.
              bar() -> foo()."#,
            expect![[r#"
                bar() -> ok."#]],
        )
    }

    #[test]
    fn test_inline_function_no_params_multiple_usage_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              fo~o() -> ok.
              baz(X) ->
                Y = foo(),
                X.
              bar() -> foo()."#,
            expect![[r#"
                baz(X) ->
                  Y = ok,
                  X.
                bar() -> ok."#]],
        )
    }

    #[test]
    fn test_inline_function_with_params_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(B) -> 3 + B.
              bar() -> f~oo(4)."#,
            expect![[r#"
                bar() ->
                    begin
                        B = 4,
                        3 + B
                    end."#]],
        )
    }

    #[test]
    fn test_inline_function_with_params_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(A,B) -> A + B.
              bar() -> f~oo(35,4)."#,
            expect![[r#"
                bar() ->
                    begin
                        A = 35,
                        B = 4,
                        A + B
                    end."#]],
        )
    }

    #[test]
    fn test_inline_function_begin_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(A,B) -> A * B.
              bar() -> f~oo(23 - 2,4)."#,
            expect![[r#"
                bar() ->
                    begin
                        A = 23 - 2,
                        B = 4,
                        A * B
                    end."#]],
        )
    }

    #[test]
    fn test_inline_function_begin_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(A,B) ->
                  X = 1,
                  A * B + X.
              bar() -> f~oo(23 - 2,4)."#,
            expect![[r#"
                bar() ->
                    begin
                        A = 23 - 2,
                        B = 4,
                        X = 1,
                        A * B + X
                    end."#]],
        )
    }

    #[test]
    fn test_inline_function_constant_parameter_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(A,0) -> A+3.
              bar() -> f~oo(4,0)."#,
            expect![[r#"
                bar() ->
                    begin
                        A = 4,
                        0 = 0,
                        A+3
                    end."#]],
        )
    }

    #[test]
    fn test_inline_function_multiple_clauses_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(0) -> 5;
              foo(AA) -> AA * 3.
              bar() -> f~oo(4)."#,
            expect![[r#"
                bar() ->
                    case 4 of
                      0 -> 5;
                      AA -> AA * 3
                    end."#]],
        )
    }

    #[test]
    fn test_inline_function_delete_original_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              ok() -> ok.

              foo(0) -> 5;
              foo(AA) -> AA * 3.
              bar() -> f~oo(4)."#,
            expect![[r#"
                ok() -> ok.

                bar() ->
                    case 4 of
                      0 -> 5;
                      AA -> AA * 3
                    end."#]],
        )
    }

    #[test]
    fn test_inline_function_delete_original_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              ok() -> ok.

              foo(AA) -> AA * 3.
              bar() -> f~oo(4)."#,
            expect![[r#"
                ok() -> ok.

                bar() ->
                    begin
                        AA = 4,
                        AA * 3
                    end."#]],
        )
    }

    #[test]
    fn test_inline_function_indentation_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              baz(0) ->
                  Y = 3,
                  Y = 2;
              baz(Z) ->
                  begin
                      Y = 4
                  end,
                  Y + Z.

              foo() ->
                  XX = 3,
                  YY = ba~z(4),
                  YY."#,
            expect![[r#"
                foo() ->
                    XX = 3,
                    YY =
                        case 4 of
                          0 ->
                                Y = 3,
                                Y = 2;
                          Z ->
                                begin
                                    Y = 4
                                end,
                                Y + Z
                        end,
                    YY."#]],
        )
    }

    #[test]
    fn test_inline_function_indentation_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              baz(Z) ->
                  begin
                      Y = 4
                  end,
                  Y + Z.

              foo() ->
                  XX = 3,
                  YY = ba~z(4),
                  YY."#,
            expect![[r#"
                foo() ->
                    XX = 3,
                    YY =
                        begin
                            Z = 4,
                            begin
                                Y = 4
                            end,
                            Y + Z
                        end,
                    YY."#]],
        )
    }

    #[test]
    fn test_inline_function_indentation_3() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              baz(Z)
                -> begin
                      Y = 4
                   end,
                   Y + Z.

              foo() ->
                  XX = 3,
                  YY = ba~z(4),
                  YY."#,
            expect![[r#"
                foo() ->
                    XX = 3,
                    YY =
                        begin
                            Z = 4,
                            begin
                                Y = 4
                             end,
                             Y + Z
                        end,
                    YY."#]],
        )
    }

    #[test]
    fn test_inline_function_with_spec_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              -spec baz() -> ok.
              baz() -> ok.

              foo() ->
                  YY = ba~z()."#,
            expect![[r#"
                foo() ->
                    YY = ok."#]],
        )
    }

    #[test]
    fn test_inline_function_with_spec_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              -spec b~az() -> ok.
              baz() -> ok.

              foo() ->
                  YY = baz()."#,
            expect![[r#"
                foo() ->
                    YY = ok."#]],
        )
    }

    #[test]
    fn test_inline_function_with_comments_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo() ->
                  YY = b~az().

              baz() ->
                %% a comment
                ok."#,
            expect![[r#"
                foo() ->
                    YY =
                          %% a comment
                          ok.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_with_comments_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) ->
                  YY = b~az(X).

              baz(Z) ->
                %% a comment
                1 + Z."#,
            expect![[r#"
                foo(X) ->
                    YY =
                        begin
                            Z = X,
                            %% a comment
                            1 + Z
                        end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_with_comments_3() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) ->
                  YY = b~az(X).

              baz(Z) -> %% a comment
                1 + Z."#,
            expect![[r#"
                foo(X) ->
                    YY =
                        begin
                            Z = X,
                            %% a comment
                          1 + Z
                        end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_with_comments_4() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) ->
                  YY = b~az(X),
                  YY.

              baz(Z) -> %% a comment
                1 + Z."#,
            expect![[r#"
                foo(X) ->
                    YY =
                        begin
                            Z = X,
                            %% a comment
                          1 + Z
                        end,
                    YY.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_with_trailing_comments_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) ->
                  YY = b~az(X).

              baz(Z) ->
                1 + Z
                % a comment
                ."#,
            expect![[r#"
                foo(X) ->
                    YY =
                        begin
                            Z = X,
                            1 + Z
                            % a comment

                        end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_with_trailing_comments_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) ->
                  YY = b~az(X).

              baz(Z) ->
                Y = 1,
                Y + Z
                % a comment
                ."#,
            expect![[r#"
                foo(X) ->
                    YY =
                        begin
                            Z = X,
                            Y = 1,
                            Y + Z
                            % a comment

                        end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_with_case_substitution_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) ->
                  YY = b~az(X).

              baz(Z) ->
                Y = 1,
                Y + Z."#,
            expect![[r#"
                foo(X) ->
                    YY =
                        begin
                            Z = X,
                            Y = 1,
                            Y + Z
                        end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_inline_all_from_function_def_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) -> baz(X).
              bar(Y) -> 1 + baz(Y).

              b~az(Z) -> 1 + Z."#,
            expect![[r#"
                foo(X) ->
                    begin
                        Z = X,
                        1 + Z
                    end.
                bar(Y) -> 1 +
                    begin
                        Z = Y,
                        1 + Z
                    end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_inline_one_from_function_usage_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) -> baz(X).
              bar(Y) -> 1 + b~az(Y).

              baz(Z) -> 1 + Z."#,
            expect![[r#"
                foo(X) -> baz(X).
                bar(Y) -> 1 +
                    begin
                        Z = Y,
                        1 + Z
                    end.

                baz(Z) -> 1 + Z."#]],
        )
    }

    #[test]
    fn test_inline_function_inline_one_from_function_usage_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              bar(Y) -> 1 + b~az(Y).

              baz(Z) -> 1 + Z."#,
            expect![[r#"
                bar(Y) -> 1 +
                    begin
                        Z = Y,
                        1 + Z
                    end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_inline_do_not_delete_exported_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              -export([baz/1]).
              bar(Y) -> 1 + b~az(Y).

              baz(Z) -> 1 + Z."#,
            expect![[r#"
                -export([baz/1]).
                bar(Y) -> 1 +
                    begin
                        Z = Y,
                        1 + Z
                    end.

                baz(Z) -> 1 + Z."#]],
        )
    }

    #[test]
    fn test_inline_function_inline_do_not_delete_exported_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              -export([baz/1]).
              foo(X) -> baz(X).
              bar(Y) -> 1 + baz(Y).

              b~az(Z) -> 1 + Z."#,
            expect![[r#"
                -export([baz/1]).
                foo(X) ->
                    begin
                        Z = X,
                        1 + Z
                    end.
                bar(Y) -> 1 +
                    begin
                        Z = Y,
                        1 + Z
                    end.

                baz(Z) -> 1 + Z."#]],
        )
    }

    #[test]
    fn test_inline_function_inline_do_not_inline_external_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              //- /src/main.erl
              -import(another, [baz/1]).
              bar(Y) -> 1 + b~az(Y).

              //- /src/another.erl
              -export([baz/1]).
              baz(Z) -> 1 + Z."#,
            expect![[r#"
                -import(another, [baz/1]).
                bar(Y) -> 1 +
                    begin
                        Z = Y,
                        1 + Z
                    end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_inline_do_not_inline_external_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              //- /src/main.erl
              bar(Y) -> 1 + another:b~az(Y).

              //- /src/another.erl
              -export([baz/1]).
              baz(Z) -> 1 + Z."#,
            expect![[r#"
                bar(Y) -> 1 +
                    begin
                        Z = Y,
                        1 + Z
                    end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_guards_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) -> b~az(X).

              baz(Z) when Z =:= 42 -> 42;
              baz(Z) -> 1 + Z.
              "#,
            expect![[r#"
                foo(X) ->
                    case X of
                      Z when Z =:= 42 -> 42;
                      Z -> 1 + Z
                    end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_guards_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) -> b~az(X).

              baz(Z) when Z =:= 42 -> 42.
              "#,
            expect![[r#"
                foo(X) ->
                    case X of
                      Z when Z =:= 42 -> 42
                    end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_guards_3() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) -> b~az(X).

              baz(Z) when Z =:= 42;Z =:= 0 -> 42.
              "#,
            expect![[r#"
                foo(X) ->
                    case X of
                      Z when Z =:= 42;Z =:= 0 -> 42
                    end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_guards_4() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) -> b~az(X).

              baz(Z) when A = Z,A =:= 42;Z =:= 0 -> 42.
              "#,
            expect![[r#"
                foo(X) ->
                    case X of
                      Z when A = Z,A =:= 42;Z =:= 0 -> 42
                    end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_guards_5() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) -> b~az(X).

              baz(Z) when
                        % this one
                        Z =:= 42;
                        % that one
                        Z =:= 0
                -> begin
                     42
                   end.
              "#,
            expect![[r#"
                foo(X) ->
                    case X of
                      Z when
                              % this one
                              Z =:= 42;
                              % that one
                              Z =:= 0
                        -> begin
                             42
                           end
                    end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_multiple_params_case_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) -> b~az(X, 5).

              baz(0, A) -> A + 3;
              baz(Z, A) -> Z + A.
              "#,
            expect![[r#"
                foo(X) ->
                    case {X, 5} of
                      {0, A} -> A + 3;
                      {Z, A} -> Z + A
                    end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_multiple_params_case_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) -> b~az(X,5).

              baz(Z,A) -> Z + A.
              "#,
            expect![[r#"
                foo(X) ->
                    begin
                        Z = X,
                        A = 5,
                        Z + A
                    end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_complex_params_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) -> b~az(remote:with_side_effects(X + 1)).

              baz(Z) -> bar(Z) * Z.
              "#,
            expect![[r#"
                foo(X) ->
                    begin
                        Z = remote:with_side_effects(X + 1),
                        bar(Z) * Z
                    end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_complex_params_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) -> b~az(X).

              baz(Z) -> bar(Z) * Z.
              "#,
            expect![[r#"
                foo(X) ->
                    begin
                        Z = X,
                        bar(Z) * Z
                    end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_complex_params_3() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) -> b~az(X,with_side_effects(X + 1)).

              baz(Y,Z) -> bar(Z) * Z + Y.
              "#,
            expect![[r#"
                foo(X) ->
                    begin
                        Y = X,
                        Z = with_side_effects(X + 1),
                        bar(Z) * Z + Y
                    end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_check_variable_name_clash_1() {
        cov_mark::check!(inline_function_is_safe);
        check_assist_not_applicable(
            inline_function,
            r#"
              foo(X) ->
                X = 1,
                b~az(3, X).

              baz(A,B) ->
                X = 1,
                Y = 2,
                (A + 1) * (B + Y).
              "#,
        )
    }

    #[test]
    fn test_inline_function_check_variable_name_clash_2() {
        cov_mark::check!(inline_function_is_safe);
        check_assist_not_applicable(
            inline_function,
            r#"
              foo(X) ->
                Z = 1,
                b~az(3, Z).

              baz(A,B) ->
                X = 1,
                Y = 2,
                (A + 1) * (B + Y).
              "#,
        )
    }

    #[test]
    fn test_inline_function_recursion_1() {
        cov_mark::check!(inline_function_recursive);
        check_assist_not_applicable(
            inline_function,
            r#"
              foo(A) ->
                b~az(A).

              baz(X) ->
                  case X of
                    0 -> 0;
                    _ -> X + baz(X - 1)
                  end.
              "#,
        )
    }

    #[test]
    fn test_inline_function_param_comments_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(A) ->
                b~az(A, %% Use the parameter
                     3).

              baz(X,Y) -> {X,Y}.
              "#,
            expect![[r#"
                foo(A) ->
                      begin
                          X = A, %% Use the parameter
                          Y = 3,
                          {X,Y}
                      end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_param_comments_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(A) ->
                b~az(A,
                     3 %% Use the parameter
                     ).

              baz(X,Y) -> {X,Y}.
              "#,
            expect![[r#"
                foo(A) ->
                      begin
                          X = A,
                          Y = 3, %% Use the parameter
                          {X,Y}
                      end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_param_comments_3() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(A) ->
                b~az(
                     %% A is special
                     A,
                     3
                     ).

              baz(X,Y) -> {X,Y}.
              "#,
            expect![[r#"
                foo(A) ->
                      begin
                          %% A is special
                          X = A,
                          Y = 3,
                          {X,Y}
                      end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_as_case_param_comments_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) ->
                 b~az(X, %% X is special
                      5).

              baz(0, A) -> A + 3;
              baz(Z, A) -> Z + A.
              "#,
            expect![[r#"
                foo(X) ->
                       case {X, %% X is special
                               5} of
                         {0, A} -> A + 3;
                         {Z, A} -> Z + A
                       end.

            "#]],
        )
    }

    #[test]
    fn test_inline_function_as_case_param_comments_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(0) -> 5;
              foo(AA) -> AA * 3.
              bar() -> f~oo(
                            %% Leading comment
                            4 %% Trailing
                           )."#,
            expect![[r#"
                bar() ->
                    case %% Leading comment
                                  4 %% Trailing
                     of
                      0 -> 5;
                      AA -> AA * 3
                    end."#]],
        )
    }

    #[test]
    fn test_inline_function_as_case_function_expr() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(0) -> 5;
              foo(AA) -> AA * 3.
              bar() -> f~oo(baz(4))."#,
            expect![[r#"
                bar() ->
                    case baz(4) of
                      0 -> 5;
                      AA -> AA * 3
                    end."#]],
        )
    }

    #[test]
    fn test_inline_function_param_name_shadowing_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X) -> X + 5.
              bar(X) -> f~oo(X)."#,
            expect![[r#"
                bar(X) ->
                    begin
                        X + 5
                    end."#]],
        )
    }

    #[test]
    fn test_inline_function_param_name_shadowing_2() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X,Y) -> X + Y.
              bar(X) -> f~oo(X,3)."#,
            expect![[r#"
                bar(X) ->
                    begin
                        Y = 3,
                        X + Y
                    end."#]],
        )
    }

    #[test]
    fn test_inline_function_param_name_shadowing_3() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X,Y) -> X + Y.
              bar(X) -> f~oo(X,
                             3 % a comment
                            )."#,
            expect![[r#"
                bar(X) ->
                    begin
                        Y = 3, % a comment
                        X + Y
                    end."#]],
        )
    }

    #[test]
    fn test_inline_function_param_name_shadowing_4() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(X,Y) -> X + Y.
              bar(X) -> f~oo(X, % another comment
                             3 % a comment
                            )."#,
            expect![[r#"
                bar(X) ->
                    begin
                        X = X, % another comment
                        Y = 3, % a comment
                        X + Y
                    end."#]],
        )
    }

    #[test]
    fn test_inline_function_param_name_shadowing_case_1() {
        check_assist(
            inline_function,
            "Inline function",
            r#"
              foo(0) -> 3;
              foo(X) -> X + 5.
              bar(X) -> f~oo(X)."#,
            expect![[r#"
                bar(X) ->
                    case X of
                      0 -> 3;
                      X -> X + 5
                    end."#]],
        )
    }

    #[test]
    fn test_inline_inner_function() {
        // T153086784
        check_assist(
            inline_function,
            "Inline function",
            r#"
              simple_map(Fields) -> {map, Fields, #{}, []}.
              struct(_Config) ->
                  Decode = simple_map(#{1 => {field, {struct, simp~le_map(#{1 => {nested, byte}})}}})."#,
            expect![[r#"
                simple_map(Fields) -> {map, Fields, #{}, []}.
                struct(_Config) ->
                    Decode = simple_map(#{1 => {field, {struct,
                        begin
                            Fields = #{1 => {nested, byte}},
                            {map, Fields, #{}, []}
                        end}}})."#]],
        )
    }
}

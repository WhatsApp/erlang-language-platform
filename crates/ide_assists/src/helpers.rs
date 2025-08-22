/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::iter;
use std::sync::Arc;
use std::sync::LazyLock;

use elp_ide_db::ReferenceClass;
use elp_ide_db::ReferenceType;
use elp_ide_db::SymbolClass;
use elp_ide_db::SymbolDefinition;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::helpers::top_insert_position;
use elp_ide_db::rename::is_safe_function;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_syntax::AstNode;
use elp_syntax::AstPtr;
use elp_syntax::Direction;
use elp_syntax::NodeOrToken;
use elp_syntax::SourceFile;
use elp_syntax::SyntaxElement;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use elp_syntax::SyntaxToken;
use elp_syntax::TextRange;
use elp_syntax::algo;
use elp_syntax::algo::skip_inline_comment;
use elp_syntax::ast;
use elp_syntax::match_ast;
use elp_text_edit::TextEdit;
use elp_text_edit::TextSize;
use fxhash::FxHashSet;
use hir::Attribute;
use hir::Body;
use hir::CompileOption;
use hir::CompileOptionId;
use hir::Expr;
use hir::ExprId;
use hir::FormList;
use hir::FunctionClauseBody;
use hir::FunctionDef;
use hir::InFile;
use hir::InFileAstPtr;
use hir::InFunctionClauseBody;
use hir::Name;
use hir::NameArity;
use hir::Semantic;
use hir::Strategy;
use hir::Var;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::fold_body;
use hir::known;

use crate::assist_context::AssistContext;

pub fn prev_form_nodes(syntax: &SyntaxNode) -> impl Iterator<Item = SyntaxNode> + use<> {
    syntax
        .siblings_with_tokens(elp_syntax::Direction::Prev)
        .skip(1) // Starts with itself
        .filter_map(|node_or_token| node_or_token.into_node())
        .take_while(|node| node.kind() != SyntaxKind::FUN_DECL)
}

/// Use surrounding context to suggest a name for a new variable.
/// Defaults to simply `VarName` for now.
///
/// **NOTE**: it is caller's responsibility to guarantee uniqueness of the name.
/// I.e. it doesn't look for names in scope.
pub(crate) fn suggest_name_for_variable(_expr: &ast::Expr, _sema: &Semantic) -> String {
    "VarName".to_string()
}

/// Given a variable name and vars in scope, return either the
/// original if it does not clash, or one with the smallest numeric suffix to be fresh.
pub(crate) fn freshen_variable_name(
    sema: &Semantic,
    var_name: String,
    vars_in_clause: &Option<FxHashSet<Var>>,
) -> String {
    if let Some(vars_in_clause) = vars_in_clause {
        let is_safe = |name: &String| -> bool {
            vars_in_clause
                .iter()
                .all(|v| name != &v.as_string(sema.db.upcast()))
        };
        if is_safe(&var_name) {
            var_name
        } else {
            let mut i = 0;
            loop {
                let name = format!("{var_name}{i}").to_string();
                if is_safe(&name) {
                    return name;
                }
                i += 1;
            }
        }
    } else {
        var_name
    }
}

/// Given a function name/arity and FileId, return either the original if it
/// does not clash, or one with the smallest numeric suffix to be
/// fresh.
pub(crate) fn freshen_function_name(ctx: &AssistContext, name: String, arity: u32) -> String {
    if is_safe_function(&ctx.sema, ctx.file_id(), &name, arity) {
        name
    } else {
        let mut i = 0;
        loop {
            let candidate_name = format!("{name}_{i}").to_string();
            if is_safe_function(&ctx.sema, ctx.file_id(), &candidate_name, arity) {
                return candidate_name;
            }
            i += 1;
        }
    }
}

pub(crate) fn skip_ws(node: Option<NodeOrToken>) -> Option<TextRange> {
    node.and_then(SyntaxElement::into_token).and_then(|t| {
        if t.kind() == SyntaxKind::WHITESPACE {
            Some(t.text_range())
        } else {
            None
        }
    })
}

pub(crate) fn skip_trailing_separator(node: &SyntaxNode) -> Option<TextRange> {
    let elements = iter::successors(node.next_sibling_or_token(), |n| {
        (*n).next_sibling_or_token()
    });
    for element in elements {
        if let Some(t) = &SyntaxElement::into_token(element)
            && t.kind() != SyntaxKind::WHITESPACE
        {
            return Some(t.text_range());
        }
    }
    None
}

pub(crate) fn find_next_token(node: &SyntaxNode, delimiter: SyntaxKind) -> Option<TextRange> {
    node.children_with_tokens()
        .filter_map(|it| it.into_token())
        .find(|it| it.kind() == delimiter)
        .map(|t| t.text_range())
}

pub(crate) fn skip_trailing_newline(node: &SyntaxNode) -> Option<TextRange> {
    let elements = iter::successors(node.next_sibling_or_token(), |n| {
        (*n).next_sibling_or_token()
    });
    for element in elements {
        if let Some(t) = &SyntaxElement::into_token(element) {
            if t.kind() == SyntaxKind::WHITESPACE && t.text().contains('\n') {
                return Some(t.text_range());
            }
        } else {
            return None;
        }
    }
    None
}

/// Extend the token `TextRange` to include preceding whitespace
pub fn include_preceding_whitespace(token: &SyntaxToken) -> TextRange {
    if let Some(prev) = token.prev_token()
        && let Some(prev) = algo::skip_whitespace_token(prev, Direction::Prev)
    {
        return TextRange::new(prev.text_range().end(), token.text_range().end());
    }
    token.text_range()
}

pub(crate) fn parens_needed(expr: &ast::Expr, var: &ast::Var) -> Option<(TextRange, bool)> {
    let rhs_not_needed = !expr_needs_parens_ast(expr);

    let parent = var.syntax().parent()?;
    let parent_not_needed = match_ast! {
        match parent {
            ast::ExprArgs(_) => true,
            ast::Pipe(_) => true,
            ast::ClauseBody(_) => true,
            ast::CatchExpr(_) => true,
            ast::MatchExpr(_) => true,
            ast::BlockExpr(_) => true,
            ast::CrClause(_) => true,
            _ => false
        }
    };

    Some((
        var.syntax().text_range(),
        !(rhs_not_needed || parent_not_needed),
    ))
}

pub fn expr_needs_parens_ast(expr: &ast::Expr) -> bool {
    let not_needed = matches!(
        expr,
        ast::Expr::ExprMax(ast::ExprMax::Atom(_))
            | ast::Expr::ExprMax(ast::ExprMax::Binary(_))
            | ast::Expr::ExprMax(ast::ExprMax::BinaryComprehension(_))
            | ast::Expr::ExprMax(ast::ExprMax::BlockExpr(_))
            | ast::Expr::ExprMax(ast::ExprMax::CaseExpr(_))
            | ast::Expr::ExprMax(ast::ExprMax::Char(_))
            | ast::Expr::ExprMax(ast::ExprMax::Float(_))
            | ast::Expr::ExprMax(ast::ExprMax::IfExpr(_))
            | ast::Expr::ExprMax(ast::ExprMax::Integer(_))
            | ast::Expr::ExprMax(ast::ExprMax::List(_))
            | ast::Expr::ExprMax(ast::ExprMax::ListComprehension(_))
            | ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(_))
            | ast::Expr::ExprMax(ast::ExprMax::MacroString(_))
            | ast::Expr::ExprMax(ast::ExprMax::ParenExpr(_))
            | ast::Expr::ExprMax(ast::ExprMax::ReceiveExpr(_))
            | ast::Expr::ExprMax(ast::ExprMax::String(_))
            | ast::Expr::ExprMax(ast::ExprMax::TryExpr(_))
            | ast::Expr::ExprMax(ast::ExprMax::Tuple(_))
            | ast::Expr::ExprMax(ast::ExprMax::Var(_))
            | ast::Expr::Call(_)
            | ast::Expr::MapExpr(_)
            | ast::Expr::MapExprUpdate(_)
            | ast::Expr::RecordExpr(_)
            | ast::Expr::RecordFieldExpr(_)
            | ast::Expr::RecordIndexExpr(_)
            | ast::Expr::RecordUpdateExpr(_),
    );
    !not_needed
}

pub fn expr_needs_parens(body: &Body, expr_id: ExprId) -> bool {
    let fold_body = fold_body(
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::VisibleParens,
        },
        body,
    );
    match &fold_body[expr_id] {
        Expr::BinaryOp { .. } => true,

        Expr::Call { .. } => false,
        Expr::Missing => false,
        Expr::Literal(_) => false,
        Expr::Var(_) => false,
        Expr::Match { .. } => false,
        Expr::Tuple { .. } => false,
        Expr::List { .. } => false,
        Expr::Binary { .. } => false,
        Expr::UnaryOp { .. } => false,
        Expr::Record { .. } => false,
        Expr::RecordUpdate { .. } => false,
        Expr::RecordIndex { .. } => false,
        Expr::RecordField { .. } => false,
        Expr::Map { .. } => false,
        Expr::MapUpdate { .. } => false,
        Expr::Catch { .. } => false,
        Expr::MacroCall { .. } => false,
        Expr::Comprehension { .. } => false,
        Expr::Block { .. } => false,
        Expr::If { .. } => false,
        Expr::Case { .. } => false,
        Expr::Receive { .. } => false,
        Expr::Try { .. } => false,
        Expr::CaptureFun { .. } => false,
        Expr::Closure { .. } => false,
        Expr::Maybe { .. } => false,
        Expr::Paren { .. } => false,
        Expr::SsrPlaceholder { .. } => false,
    }
}

/// If the `expr` is parens, unwrap them, and repeat until the inner
/// expression is returned
pub fn unwrap_parens(expr: &ast::Expr) -> Option<ast::Expr> {
    match expr {
        ast::Expr::ExprMax(ast::ExprMax::ParenExpr(expr)) => unwrap_parens(&expr.expr()?),
        _ => Some(expr.clone()),
    }
}

/// Generate an edit to insert parens around the given range
pub fn add_parens_edit(range: &TextRange) -> TextEdit {
    let mut builder = TextEdit::builder();
    builder.insert(range.start(), "(".to_string());
    builder.insert(range.end(), ")".to_string());

    builder.finish()
}

pub(crate) fn change_indent(delta_indent: i8, str: String) -> String {
    let indent_str = " ".repeat(delta_indent.unsigned_abs() as usize);
    if str.contains('\n') {
        // Only change indentation if the new string has more than one line.
        str.split('\n')
            .enumerate()
            .map(|(idx, s)| {
                if idx == 0 && !s.is_empty() {
                    // No leading newline, trim leading whitespace
                    s.trim_start().to_string()
                } else if delta_indent >= 0 {
                    if !s.is_empty() {
                        format!("{indent_str}{s}")
                    } else {
                        s.to_owned()
                    }
                } else if let Some(s) = s.strip_prefix(indent_str.as_str()) {
                    s.to_string()
                } else {
                    // Do not lose useful characters, but remove all leading whitespace
                    s.trim_start().to_string()
                }
            })
            .map(|s| s.trim_end().to_string())
            .collect::<Vec<_>>()
            .join("\n")
    } else {
        str.trim_start().to_string()
    }
}

pub const DEFAULT_INDENT_STEP: i8 = 4;

/// Any parameters to the `Clause` that are just a single variable.
pub(crate) fn simple_param_vars(
    clause: &InFunctionClauseBody<&FunctionClauseBody>,
) -> Option<FxHashSet<Var>> {
    let mut acc = FxHashSet::default();
    clause.value.clause.pats.iter().for_each(|p| {
        if let hir::Pat::Var(v) = &clause[*p] {
            acc.insert(*v);
        }
    });
    Some(acc)
}

#[derive(Debug)]
pub(crate) struct FunctionRanges {
    pub(crate) function: TextRange,
    pub(crate) spec: Option<TextRange>,
    pub(crate) edoc: Vec<TextRange>,
}

impl FunctionRanges {
    pub(crate) fn delete(&self, builder: &mut SourceChangeBuilder) {
        builder.delete(self.function);
        self.spec.into_iter().for_each(|range| {
            builder.delete(range);
        });
        self.edoc.iter().for_each(|range| {
            builder.delete(*range);
        });
    }
}

pub(crate) fn ranges_for_delete_function(
    ctx: &AssistContext,
    ast_fun: &ast::FunDecl,
) -> Option<FunctionRanges> {
    // Look for a possible spec, and delete it too.
    let function_def = match ctx.classify_offset()? {
        SymbolClass::Definition(SymbolDefinition::Function(fun_def)) => Some(fun_def),
        SymbolClass::Reference {
            refs: _,
            typ: ReferenceType::Fuzzy,
        } => None,
        SymbolClass::Reference {
            refs: ReferenceClass::Definition(SymbolDefinition::Function(fun_def)),
            typ: _,
        } => Some(fun_def),
        _ => None,
    }?;

    let fun_asts = function_def.source(ctx.sema.db.upcast());
    let fun_range = function_def.range(ctx.sema.db.upcast())?;

    let edoc = if let Some(file_edoc) = ctx.sema.form_edoc_comments(InFileAstPtr::new(
        ctx.file_id(),
        AstPtr::new(&ast::Form::FunDecl(ast_fun.clone())),
    )) {
        file_edoc
            .comments()
            .filter_map(|c| {
                let comment = ctx.ast_ptr_get(*c)?;
                Some(extend_form_range_for_delete(comment.syntax()))
            })
            .collect()
    } else {
        vec![]
    };

    let spec_range = function_def.spec.and_then(|spec| {
        let ast_spec = ctx.form_ast(InFile::new(spec.file.file_id, spec.spec.form_id));
        ast_spec.map(|ast_spec| extend_form_range_for_delete(ast_spec.syntax()))
    });

    Some(FunctionRanges {
        function: extend_function_range_for_delete(fun_range, fun_asts.last()?.syntax()),
        spec: spec_range,
        edoc,
    })
}

pub fn adjacent_newline(syntax: &SyntaxNode) -> Option<SyntaxToken> {
    let token = syntax.last_token()?.next_token()?;
    if token.kind() == SyntaxKind::WHITESPACE && token.text().starts_with('\n') {
        Some(token)
    } else {
        None
    }
}

// Extend the range of a syntax element.
// If the next sibling is a syntax element of the same kind, extend the range to include the whitespace between them.
// If it is not, only include the first newline (if any).
// This is useful, for example, when deleting an attribute (macro, header, etc)
pub fn extend_range(syntax: &SyntaxNode) -> TextRange {
    if let Some(NodeOrToken::Node(node)) =
        algo::non_whitespace_sibling(NodeOrToken::Node(syntax.clone()), Direction::Next)
        && node.kind() == syntax.kind()
    {
        return extend_form_range_for_delete(syntax);
    }
    extend_range_to_adjacent_newline(syntax)
}

pub fn extend_range_to_adjacent_newline(syntax: &SyntaxNode) -> TextRange {
    let range = syntax.text_range();
    match adjacent_newline(syntax) {
        Some(_token) => TextRange::new(range.start(), range.end() + TextSize::from(1)),
        None => range,
    }
}

pub fn extend_range_to_adjacent_newline_skip_inline_comment(syntax: &SyntaxNode) -> TextRange {
    let range = syntax.text_range();
    match skip_inline_comment(syntax) {
        None => extend_range_to_adjacent_newline(syntax),
        Some(token) => TextRange::new(
            range.start(),
            token.text_range().start() + TextSize::from(1),
        ),
    }
}

pub fn extend_form_range_for_delete(syntax: &SyntaxNode) -> TextRange {
    let orig_range = syntax.text_range();
    extend_function_range_for_delete(orig_range, syntax)
}

fn extend_function_range_for_delete(orig_range: TextRange, last_syntax: &SyntaxNode) -> TextRange {
    let start = orig_range.start();
    let end = match skip_trailing_newline(last_syntax) {
        Some(end) => end.end(),
        None => orig_range.end(),
    };
    TextRange::new(start, end)
}

/// When we want to delete an expression, extend the delete range to
/// remove preceding whitespace and any trailing separator.
pub fn extend_delete_range(syntax: &SyntaxNode) -> TextRange {
    let orig_range = syntax.text_range();
    let start = match skip_ws(syntax.prev_sibling_or_token()) {
        Some(start) => start.start(),
        None => orig_range.start(),
    };
    let end = match skip_trailing_separator(syntax) {
        Some(end) => end.end(),
        None => orig_range.end(),
    };
    TextRange::new(start, end)
}

// ---------------------------------------------------------------------

/// Add an option to the `suite/0` function in a test suite.
pub fn add_suite_0_option<'a>(
    sema: &'a Semantic<'a>,
    file_id: FileId,
    key: &str,
    value: &str,
    insert_at: Option<TextSize>,
    builder: &'a mut SourceChangeBuilder,
) -> Option<()> {
    let source = sema.parse(file_id).value;
    let form_list = sema.form_list(file_id);
    let def_map = sema.def_map(file_id);
    let name_arity = NameArity::new(known::suite, 0);
    if let Some(fun) = def_map.get_function(&name_arity) {
        add_to_suite_0(sema, file_id, fun, &source, key, value, builder);
    } else {
        new_suite_0(
            sema, file_id, &form_list, &source, key, value, insert_at, builder,
        );
    };
    Some(())
}

#[allow(clippy::too_many_arguments)]
fn new_suite_0(
    sema: &Semantic,
    file_id: FileId,
    form_list: &FormList,
    source: &SourceFile,
    key: &str,
    value: &str,
    insert_at: Option<TextSize>,
    builder: &mut SourceChangeBuilder,
) {
    export_suite_0(sema, file_id, builder);
    let insert = first_function_insert_location(insert_at, form_list, source);
    builder.insert(insert, format!("\nsuite() ->\n    [{{{key}, {value}}}].\n"))
}

/// Find the first location in a .erl file to be able to insert a
/// function. This is after all the standard headers, such as module
/// attributes and exports.
pub fn first_function_insert_location(
    insert_at: Option<TextSize>,
    form_list: &FormList,
    source: &SourceFile,
) -> TextSize {
    let insert = insert_at.unwrap_or_else(|| {
        if let Some(module_attr) = form_list.module_attribute() {
            let module_attr_range = module_attr.form_id.get(source).syntax().text_range();
            module_attr_range.end() + TextSize::from(1)
        } else {
            TextSize::from(0)
        }
    });
    if let Some((_, export_attr)) = form_list.exports().last() {
        let export_attr_range = export_attr.form_id.get(source).syntax().text_range();
        export_attr_range.end() + TextSize::from(1)
    } else {
        insert
    }
}

fn add_to_suite_0(
    sema: &Semantic,
    file_id: FileId,
    fun_def: &FunctionDef,
    source: &SourceFile,
    key: &str,
    value: &str,
    builder: &mut SourceChangeBuilder,
) -> Option<()> {
    let fun = fun_def.function_clauses.first()?;
    let fun_ast = fun.form_id.get(source);
    let clause = match fun_ast.clause()? {
        ast::FunctionOrMacroClause::FunctionClause(clause) => clause,
        ast::FunctionOrMacroClause::MacroCallExpr(_) => return None,
    };
    let expr = clause.body()?.exprs().next()?;
    let option = format!("{{{key}, {value}}}");
    match expr {
        ast::Expr::ExprMax(ast::ExprMax::List(list)) => {
            add_or_update_list(&list, key, value, builder);
        }
        ast::Expr::ExprMax(ast::ExprMax::Atom(e)) => {
            let r = e.syntax().text_range();
            builder.replace(r, format!("[{}, {option}]", e.syntax().text()));
        }
        ast::Expr::ExprMax(ast::ExprMax::Tuple(e)) => {
            let r = e.syntax().text_range();
            builder.replace(r, format!("[{}, {option}]", e.syntax().text()));
        }
        _ => return None,
    };
    if !fun_def.exported {
        export_suite_0(sema, file_id, builder);
    }
    Some(())
}

fn export_suite_0(sema: &Semantic, file_id: FileId, builder: &mut SourceChangeBuilder) {
    let name_arity = NameArity::new(known::suite, 0);
    ExportBuilder::new(sema, file_id, ExportForm::Functions, &[name_arity], builder)
        .group_with(NameArity::new(known::all, 0))
        .export_list_pos(ExportListPosition::First)
        .finish();
}

fn add_or_update_list(list: &ast::List, key: &str, value: &str, builder: &mut SourceChangeBuilder) {
    let option = format!("{{{key}, {value}}}");
    let mut done = false;
    list.exprs().for_each(|e| {
        if let ast::Expr::ExprMax(ast::ExprMax::Tuple(e)) = e
            && let Some(ast::Expr::ExprMax(ast::ExprMax::Atom(a))) = e.expr().next()
            && a.text() == Some(key.to_string())
        {
            if e.syntax().text().to_string() != option {
                // We found an existing key, with different value, replace the tuple with the new one
                builder.replace(e.syntax().text_range(), option.to_string());
            };
            done = true;
        };
    });
    if !done {
        // No existing key, insert at the end of the list.
        // Skip the trailing "]"
        let mut r = list.syntax().text_range().end();
        r -= TextSize::from(1);
        builder.insert(r, format!(", {option}"));
    }
}

// ---------------------------------------------------------------------

pub fn add_compile_option<'a>(
    sema: &'a Semantic<'a>,
    file_id: FileId,
    option: &str,
    insert_at: Option<TextSize>,
    builder: &'a mut SourceChangeBuilder,
) -> Option<()> {
    let source = sema.parse(file_id).value;
    let form_list = sema.form_list(file_id);

    builder.edit_file(file_id);
    if form_list.compile_attributes().count() == 0 {
        new_compile_attribute(&form_list, &source, option, insert_at, builder);
        Some(())
    } else if form_list.compile_attributes().count() == 1 {
        // One existing compile attribute, add the option to it.
        let (_, co) = form_list.compile_attributes().next()?;
        add_to_compile_attribute(co, &source, option, builder)
    } else {
        // Multiple, make a new one
        new_compile_attribute(&form_list, &source, option, insert_at, builder);
        Some(())
    }
}

fn new_compile_attribute(
    form_list: &FormList,
    source: &SourceFile,
    option: &str,
    insert_at: Option<TextSize>,
    builder: &mut SourceChangeBuilder,
) {
    let insert = insert_at.unwrap_or_else(|| {
        if let Some(module_attr) = form_list.module_attribute() {
            let module_attr_range = module_attr.form_id.get(source).syntax().text_range();
            module_attr_range.end() + TextSize::from(1)
        } else {
            TextSize::from(0)
        }
    });
    builder.insert(insert, format!("\n-compile([{option}]).\n"))
}

fn add_to_compile_attribute(
    co: &CompileOption,
    source: &SourceFile,
    option: &str,
    builder: &mut SourceChangeBuilder,
) -> Option<()> {
    let attr_ast = co.form_id.get(source);
    match &attr_ast.options()? {
        ast::Expr::ExprMax(ast::ExprMax::List(e)) => {
            // Skip the trailing "]"
            let mut r = e.syntax().text_range().end();
            r -= TextSize::from(1);
            builder.insert(r, format!(", {option}"));
        }
        ast::Expr::ExprMax(ast::ExprMax::Atom(e)) => {
            let r = e.syntax().text_range();
            builder.replace(r, format!("[{}, {option}]", e.syntax().text()));
        }
        ast::Expr::ExprMax(ast::ExprMax::Tuple(e)) => {
            let r = e.syntax().text_range();
            builder.replace(r, format!("[{}, {option}]", e.syntax().text()));
        }
        _ => return None,
    };
    Some(())
}

pub fn rename_atom_in_compile_attribute(
    sema: &Semantic,
    file_id: FileId,
    co_id: &CompileOptionId,
    old: &str,
    new: &str,
    builder: &mut SourceChangeBuilder,
) -> Option<()> {
    let form_list = sema.form_list(file_id);
    let source = sema.parse(file_id).value;
    let co = &form_list[*co_id];
    let attr_ast = co.form_id.get(&source);
    attr_ast
        .options()?
        .syntax()
        .descendants()
        .filter_map(ast::Atom::cast)
        .for_each(|n| {
            if n.syntax().text() == old {
                builder.replace(n.syntax().text_range(), new);
            }
        });
    Some(())
}

pub fn moduledoc_insert_offset(sema: &Semantic, file_id: FileId) -> Option<TextSize> {
    let form_list = sema.form_list(file_id);
    let module_attribute_range = form_list.module_attribute()?;
    let last_significant_attribute = last_significant_attribute(&form_list);
    let last_compile_attribute = last_compile_attribute(&form_list);

    let range = match (last_significant_attribute, last_compile_attribute) {
        (None, None) => extend_range_to_adjacent_newline_skip_inline_comment(
            module_attribute_range
                .form_id
                .get_ast(sema.db, file_id)
                .syntax(),
        ),
        (Some(attr), None) => extend_range_to_adjacent_newline_skip_inline_comment(
            attr.form_id.get_ast(sema.db, file_id).syntax(),
        ),
        (None, Some(compile_option)) => extend_range_to_adjacent_newline_skip_inline_comment(
            compile_option.form_id.get_ast(sema.db, file_id).syntax(),
        ),
        (Some(attr), Some(compile_option)) => {
            if attr.form_id.range(sema.db, file_id).end()
                > compile_option.form_id.range(sema.db, file_id).end()
            {
                extend_range_to_adjacent_newline_skip_inline_comment(
                    attr.form_id.get_ast(sema.db, file_id).syntax(),
                )
            } else {
                extend_range_to_adjacent_newline_skip_inline_comment(
                    compile_option.form_id.get_ast(sema.db, file_id).syntax(),
                )
            }
        }
    };

    Some(range.end())
}

pub fn last_compile_attribute_range(
    sema: &Semantic,
    form_list: &FormList,
    file_id: FileId,
) -> Option<TextRange> {
    let attr = last_compile_attribute(form_list)?;
    Some(attr.form_id.range(sema.db, file_id))
}

pub fn last_compile_attribute(form_list: &FormList) -> Option<&CompileOption> {
    form_list
        .compile_attributes()
        .last()
        .map(|(_idx, attr)| attr)
}

pub fn last_significant_attribute_range(
    sema: &Semantic,
    form_list: &FormList,
    file_id: FileId,
) -> Option<TextRange> {
    let attr = last_significant_attribute(form_list)?;
    Some(attr.form_id.range(sema.db, file_id))
}

pub fn last_significant_attribute(form_list: &FormList) -> Option<&Attribute> {
    static SIGNIFICANT_ATTRIBUTES: LazyLock<FxHashSet<Name>> =
        LazyLock::new(|| FxHashSet::from_iter([known::author, known::oncall]));

    form_list
        .attributes()
        .take_while(|(_idx, attr)| SIGNIFICANT_ATTRIBUTES.contains(&attr.name))
        .last()
        .map(|(_idx, attr)| attr)
}

// ---------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ExportListPosition {
    First,
    Last,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ExportForm {
    Functions,
    #[allow(unused)] // Used in next diff
    Types,
}

pub struct ExportBuilder<'a> {
    sema: &'a Semantic<'a>,
    file_id: FileId,
    export_form: ExportForm,
    items: &'a [NameArity],
    // `group_with`: Add `funs` to the same export as this, if found.
    // If it is added to the existing export, the comment is not used.
    group_with: Option<NameArity>,
    export_list_pos: ExportListPosition,
    insert_at: Option<TextSize>,
    with_comment: Option<String>,
    builder: &'a mut SourceChangeBuilder,
}

impl<'a> ExportBuilder<'a> {
    pub fn new(
        sema: &'a Semantic<'a>,
        file_id: FileId,
        export_form: ExportForm,
        items: &'a [NameArity],
        builder: &'a mut SourceChangeBuilder,
    ) -> ExportBuilder<'a> {
        ExportBuilder {
            sema,
            file_id,
            export_form,
            items,
            group_with: None,
            export_list_pos: ExportListPosition::Last,
            insert_at: None,
            with_comment: None,
            builder,
        }
    }

    pub fn group_with(mut self, name: NameArity) -> ExportBuilder<'a> {
        self.group_with = Some(name);
        self
    }

    pub fn export_list_pos(mut self, pos: ExportListPosition) -> ExportBuilder<'a> {
        self.export_list_pos = pos;
        self
    }

    pub fn insert_at(mut self, location: TextSize) -> ExportBuilder<'a> {
        self.insert_at = Some(location);
        self
    }

    pub fn with_comment(mut self, comment: String) -> ExportBuilder<'a> {
        self.with_comment = Some(comment);
        self
    }

    pub fn finish(&mut self) {
        let source = self.sema.parse(self.file_id).value;
        let form_list = self.sema.form_list(self.file_id);
        let export_text = self
            .items
            .iter()
            .map(|name_arity| format!("{name_arity}"))
            .collect::<Vec<_>>()
            .join(", ");

        let export_form_count = match self.export_form {
            ExportForm::Functions => form_list.exports().count(),
            ExportForm::Types => form_list.type_exports().count(),
        };
        let (insert, text) = if export_form_count == 0 {
            self.new_export(form_list, source, export_text)
        } else {
            // Top priority: group_with
            if let Some(group_with) = &self.group_with {
                if let Some((insert, text)) = || -> Option<_> {
                    let (_, export) = form_list.exports().find(|(_, e)| {
                        e.entries
                            .clone()
                            .any(|fa| &form_list[fa].name == group_with)
                    })?;
                    self.add_to_export(export, &source, &export_text)
                }() {
                    (insert, text)
                } else {
                    self.new_export(form_list, source, export_text)
                }
            } else if self.with_comment.is_some() {
                // Preceding comment for export, always make a fresh one
                self.new_export(form_list, source, export_text)
            } else if let Some((insert, text)) = || -> Option<_> {
                if export_form_count == 1 {
                    // One existing export, add the function to it.

                    match self.export_form {
                        ExportForm::Functions => {
                            let (_, export) = form_list.exports().next()?;
                            self.add_to_export(export, &source, &export_text)
                        }
                        ExportForm::Types => {
                            let (_, export) = form_list.type_exports().next()?;
                            self.add_to_type_export(export, &source, &export_text)
                        }
                    }
                } else {
                    // Multiple
                    None
                }
            }() {
                (insert, text)
            } else {
                // Zero or multiple existing exports, create a fresh one
                self.new_export(form_list, source, export_text)
            }
        };

        self.builder.edit_file(self.file_id);
        self.builder.insert(insert, text)
    }

    fn new_export(
        &self,
        form_list: Arc<FormList>,
        source: SourceFile,
        export_text: String,
    ) -> (TextSize, String) {
        let export_attr = match self.export_form {
            ExportForm::Functions => "export",
            ExportForm::Types => "export_type",
        };
        let mut insert = self
            .insert_at
            .unwrap_or_else(|| top_insert_position(&form_list, &source));

        if self.export_form == ExportForm::Types {
            // Types are normally exported after function exports
            if let Some((_, export)) = form_list.exports().last() {
                let export_range = export.form_id.get(&source).syntax().text_range();
                insert = export_range.end() + TextSize::from(1);
            }
        }
        match &self.with_comment {
            Some(comment) => (
                insert,
                format!("\n%% {comment}\n-{export_attr}([{export_text}]).\n"),
            ),
            None => (insert, format!("\n-{export_attr}([{export_text}]).\n")),
        }
    }

    fn add_to_export(
        &self,
        export: &hir::Export,
        source: &elp_syntax::SourceFile,
        export_text: &String,
    ) -> Option<(TextSize, String)> {
        let export_ast = export.form_id.get(source);

        let maybe_added = match self.export_list_pos {
            ExportListPosition::First => export_ast
                .funs()
                .next()
                .map(|fa| (fa.syntax().text_range().start(), format!("{export_text}, "))),
            ExportListPosition::Last => export_ast
                .funs()
                .last()
                .map(|fa| (fa.syntax().text_range().end(), format!(", {export_text}"))),
        };
        match maybe_added {
            Some(result) => Some(result),
            None => {
                // Empty export list
                let range = find_next_token(export_ast.syntax(), SyntaxKind::ANON_LBRACK)?;
                Some((range.end(), export_text.clone()))
            }
        }
    }

    fn add_to_type_export(
        &self,
        export: &hir::TypeExport,
        source: &elp_syntax::SourceFile,
        export_text: &String,
    ) -> Option<(TextSize, String)> {
        let export_ast = export.form_id.get(source);

        let maybe_added = match self.export_list_pos {
            ExportListPosition::First => export_ast
                .types()
                .next()
                .map(|fa| (fa.syntax().text_range().start(), format!("{export_text}, "))),
            ExportListPosition::Last => export_ast
                .types()
                .last()
                .map(|fa| (fa.syntax().text_range().end(), format!(", {export_text}"))),
        };
        match maybe_added {
            Some(result) => Some(result),
            None => {
                // Empty export list
                let range = find_next_token(export_ast.syntax(), SyntaxKind::ANON_LBRACK)?;
                Some((range.end(), export_text.clone()))
            }
        }
    }
}

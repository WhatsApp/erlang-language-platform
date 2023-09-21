/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! See [`AssistContext`].

use elp_ide_db::assists::AssistContextDiagnostic;
use elp_ide_db::assists::AssistUserInput;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_db::RootDatabase;
use elp_ide_db::SymbolClass;
use elp_syntax::algo;
use elp_syntax::ast::AstNode;
use elp_syntax::label::Label;
use elp_syntax::Direction;
use elp_syntax::SourceFile;
use elp_syntax::SyntaxElement;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxToken;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use elp_syntax::TokenAtOffset;
use fxhash::FxHashSet;
use hir::db::MinDefDatabase;
use hir::AnyExpr;
use hir::Body;
use hir::Expr;
use hir::ExprId;
use hir::FormId;
use hir::InFile;
use hir::InFileAstPtr;
use hir::Semantic;
use hir::Strategy;
use hir::TypeExprId;

use crate::assist_config::AssistConfig;
use crate::Assist;
use crate::AssistId;
use crate::AssistKind;
use crate::AssistResolveStrategy;

/// `AssistContext` allows to apply an assist or check if it could be applied.
///
/// Assists use a somewhat over-engineered approach, given the current needs.
/// The assists workflow consists of two phases. In the first phase, a user asks
/// for the list of available assists. In the second phase, the user picks a
/// particular assist and it gets applied.
///
/// There are two peculiarities here:
///
/// * first, we ideally avoid computing more things then necessary to answer "is
///   assist applicable" in the first phase.
/// * second, when we are applying assist, we don't have a guarantee that there
///   weren't any changes between the point when user asked for assists and when
///   they applied a particular assist. So, when applying assist, we need to do
///   all the checks from scratch.
///
/// To avoid repeating the same code twice for both "check" and "apply"
/// functions, we use an approach reminiscent of that of Django's function based
/// views dealing with forms. Each assist receives a runtime parameter,
/// `resolve`. It first check if an edit is applicable (potentially computing
/// info required to compute the actual edit). If it is applicable, and
/// `resolve` is `true`, it then computes the actual edit.
///
/// So, to implement the original assists workflow, we can first apply each edit
/// with `resolve = false`, and then applying the selected edit again, with
/// `resolve = true` this time.
///
/// Note, however, that we don't actually use such two-phase logic at the
/// moment, because the LSP API is pretty awkward in this place, and it's much
/// easier to just compute the edit eagerly :-)
pub(crate) struct AssistContext<'a> {
    pub(crate) config: &'a AssistConfig,
    pub(crate) sema: Semantic<'a>,
    pub(crate) frange: FileRange,
    pub(crate) diagnostics: &'a [AssistContextDiagnostic],
    pub(crate) user_input: Option<AssistUserInput>,
    trimmed_range: TextRange,
    source_file: SourceFile,
}

impl<'a> AssistContext<'a> {
    pub(crate) fn new(
        db: &'a RootDatabase,
        config: &'a AssistConfig,
        frange: FileRange,
        diagnostics: &'a [AssistContextDiagnostic],
        user_input: Option<AssistUserInput>,
    ) -> AssistContext<'a> {
        let source_file = db.parse(frange.file_id).tree();
        // Trim the selection to omit leading and trailing whitespace.
        // In this context, comments are not included in the
        // whitespace to be skipped, so any comments selected will be
        // included in the reduced range.
        let start = frange.range.start();
        let end = frange.range.end();
        // Temporary for T153426323
        let _pctx = stdx::panic_context::enter("\nAssistContext::new start".to_string());
        let left = source_file.syntax().token_at_offset(start);
        // Temporary for T153426323
        let _pctx = stdx::panic_context::enter("\nAssistContext::new end".to_string());
        let right = source_file.syntax().token_at_offset(end);
        let left = left
            .right_biased()
            .and_then(|t| algo::skip_whitespace_token(t, Direction::Next));
        let right = right
            .left_biased()
            .and_then(|t| algo::skip_whitespace_token(t, Direction::Prev));
        let left = left.map(|t| t.text_range().start().clamp(start, end));
        let right = right.map(|t| t.text_range().end().clamp(start, end));

        let trimmed_range = match (left, right) {
            (Some(left), Some(right)) if left <= right => {
                // Temporary for  T148094436
                let _pctx = stdx::panic_context::enter("\nAssistContext::new".to_string());
                TextRange::new(left, right)
            }
            // Selection solely consists of whitespace so just fall back to the original
            _ => frange.range,
        };

        AssistContext {
            config,
            sema: Semantic::new(db),
            frange,
            trimmed_range,
            source_file,
            diagnostics,
            user_input,
        }
    }

    pub(crate) fn db(&self) -> &dyn MinDefDatabase {
        self.sema.db
    }

    // NB, this ignores active selection.
    pub(crate) fn offset(&self) -> TextSize {
        // Temporary for T153426323
        let _pctx = stdx::panic_context::enter("\nAssistContext::offset".to_string());
        self.frange.range.start()
    }

    pub(crate) fn token_at_offset(&self) -> TokenAtOffset<SyntaxToken> {
        // Temporary for T153426323
        let _pctx = stdx::panic_context::enter("\nAssistContext::token_at_offset".to_string());
        self.source_file.syntax().token_at_offset(self.offset())
    }

    pub(crate) fn find_tokens_syntax_at_offset(
        &self,
        kinds: FxHashSet<SyntaxKind>,
    ) -> Option<SyntaxToken> {
        self.token_at_offset().find(|it| kinds.contains(&it.kind()))
    }

    pub(crate) fn find_node_at_offset<N: AstNode>(&self) -> Option<N> {
        algo::find_node_at_offset(self.source_file.syntax(), self.offset())
    }

    pub(crate) fn find_node_at_custom_offset<N: AstNode>(&self, offset: TextSize) -> Option<N> {
        algo::find_node_at_offset(self.source_file.syntax(), offset)
    }

    /// Returns the selected range trimmed for whitespace tokens, that is the range will be snapped
    /// to the nearest enclosed token.
    pub(crate) fn selection_trimmed(&self) -> TextRange {
        self.trimmed_range
    }

    pub(crate) fn file_id(&self) -> FileId {
        self.frange.file_id
    }

    pub(crate) fn has_empty_selection(&self) -> bool {
        self.trimmed_range.is_empty()
    }

    /// Returns the element covered by the selection range, this
    /// excludes trailing whitespace in the selection.
    pub(crate) fn covering_element(&self) -> SyntaxElement {
        self.source_file
            .syntax()
            .covering_element(self.selection_trimmed())
    }

    pub(crate) fn classify_offset(&self) -> Option<SymbolClass> {
        // Temporary for T153426323
        let _pctx = stdx::panic_context::enter("\nAssistContext::classify_offset".to_string());
        let token = self
            .source_file
            .syntax()
            .token_at_offset(self.offset())
            .right_biased()?; // offset is start of a range
        SymbolClass::classify(&self.sema, InFile::new(self.file_id(), token))
    }

    pub(crate) fn form_ast<T>(&self, form_id: FormId<T>) -> T
    where
        T: AstNode,
    {
        form_id.get(&self.source_file)
    }

    pub(crate) fn ast_ptr_get<T>(&self, ptr: InFileAstPtr<T>) -> Option<T>
    where
        T: AstNode,
    {
        ptr.to_node(&InFile::new(self.file_id(), self.source_file.clone()))
    }

    pub(crate) fn user_input_or(&self, get_default: impl FnOnce() -> String) -> String {
        if let Some(user_input) = &self.user_input {
            user_input.value.clone()
        } else {
            get_default()
        }
    }

    /// Given a list of `hir::ExprId`, create a set of function
    /// arguments as a comma-separated string, with names derived from
    /// the expressions in a user-meaningful way.
    pub(crate) fn create_function_args(&self, args: &[ExprId], body: &Body) -> String {
        args.iter()
            .map(|arg| {
                // IntelliJ seems to turn (numeric)
                // literals into `N`, and then just smash
                // the variables together. So a param
                // called as `X + 1 + Y` becomes `XNY`.
                let vars_and_literals = body.fold_expr(
                    Strategy::TopDown,
                    *arg,
                    Vec::default(),
                    &mut |mut acc, ctx| {
                        match &ctx.item {
                            AnyExpr::Expr(Expr::Var(var)) => {
                                acc.push(var.as_string(self.db().upcast()));
                            }
                            AnyExpr::Expr(Expr::Literal(_)) => {
                                acc.push("N".to_string());
                            }
                            _ => {}
                        };
                        acc
                    },
                );
                vars_and_literals.join("")
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    pub(crate) fn create_function_args_from_types(
        &self,
        args: &[TypeExprId],
        body: &Body,
    ) -> String {
        args.iter()
            .enumerate()
            .map(|(i, typ)| match &body[*typ] {
                hir::TypeExpr::AnnType { var, ty: _ } => var.as_string(self.db().upcast()),
                _ => format!("Arg{}", i + 1),
            })
            .collect::<Vec<_>>()
            .join(",")
    }
}

pub(crate) struct Assists {
    file: FileId,
    resolve: AssistResolveStrategy,
    buf: Vec<Assist>,
    allowed: Option<Vec<AssistKind>>,
}

impl Assists {
    pub(crate) fn new(ctx: &AssistContext, resolve: AssistResolveStrategy) -> Assists {
        Assists {
            resolve,
            file: ctx.frange.file_id,
            buf: Vec::new(),
            allowed: ctx.config.allowed.clone(),
        }
    }

    pub(crate) fn finish(mut self) -> Vec<Assist> {
        self.buf.sort_by_key(|assist| assist.target.len());
        self.buf
    }

    pub(crate) fn add(
        &mut self,
        id: AssistId,
        label: impl Into<String>,
        target: TextRange,
        user_input: Option<AssistUserInput>,
        f: impl FnOnce(&mut SourceChangeBuilder),
    ) -> Option<()> {
        if !self.is_allowed(&id) {
            return None;
        }
        let label = Label::new(label.into());
        let assist = Assist {
            id,
            label,
            group: None,
            target,
            source_change: None,
            user_input,
        };
        self.add_impl(assist, f)
    }

    fn add_impl(
        &mut self,
        mut assist: Assist,
        f: impl FnOnce(&mut SourceChangeBuilder),
    ) -> Option<()> {
        let source_change = if self.resolve.should_resolve(&assist.id) {
            let mut builder = SourceChangeBuilder::new(self.file);
            f(&mut builder);
            Some(builder.finish())
        } else {
            None
        };
        assist.source_change = source_change;

        self.buf.push(assist);
        Some(())
    }

    fn is_allowed(&self, id: &AssistId) -> bool {
        match &self.allowed {
            Some(allowed) => allowed.iter().any(|kind| kind.contains(id.1)),
            None => true,
        }
    }
}

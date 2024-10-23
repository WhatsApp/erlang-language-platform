/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Ability to traverse over the hir ast computing a result

use std::sync::Arc;

use elp_base_db::FileId;
use elp_syntax::TextRange;

use crate::body::BodyOrigin;
use crate::body::FoldBody;
use crate::expr::AnyExpr;
use crate::expr::MaybeExpr;
use crate::AnyExprId;
use crate::Attribute;
use crate::AttributeId;
use crate::Body;
use crate::BodySourceMap;
use crate::CRClause;
use crate::CallTarget;
use crate::Callback;
use crate::CallbackId;
use crate::Clause;
use crate::CompileOption;
use crate::CompileOptionId;
use crate::ComprehensionBuilder;
use crate::ComprehensionExpr;
use crate::Define;
use crate::DefineId;
use crate::Expr;
use crate::ExprId;
use crate::ExprSource;
use crate::FormIdx;
use crate::FunType;
use crate::FunctionDefId;
use crate::HirIdx;
use crate::InFile;
use crate::ListType;
use crate::PPDirective;
use crate::PatId;
use crate::Record;
use crate::RecordFieldBody;
use crate::RecordId;
use crate::Semantic;
use crate::Spec;
use crate::SpecId;
use crate::SpecSig;
use crate::TermId;
use crate::TypeAlias;
use crate::TypeAliasId;
use crate::TypeExpr;
use crate::TypeExprId;

// ---------------------------------------------------------------------

/// Choose the appropriate `FoldBody` to ensure macros are visible or
/// not according to the chosen strategy.
pub fn fold_body(strategy: Strategy, body: &Body) -> FoldBody {
    match strategy.macros {
        MacroStrategy::DoNotExpand | MacroStrategy::ExpandButIncludeMacroCall => FoldBody {
            body,
            macros: VisibleMacros::Yes,
            parens: strategy.parens,
        },
        MacroStrategy::Expand => FoldBody {
            body,
            macros: VisibleMacros::No,
            parens: strategy.parens,
        },
    }
}

pub trait Fold {
    type Id;

    fn fold<'a, T>(
        sema: &Semantic,
        strategy: Strategy,
        id: Self::Id,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T;
}

impl Fold for Spec {
    type Id = InFile<SpecId>;

    fn fold<'a, T>(
        sema: &Semantic,
        strategy: Strategy,
        id: Self::Id,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        let body = sema.db.spec_body(id);
        body.sigs.iter().fold(initial, |acc, spec_sig| {
            FoldCtx::fold_type_spec_sig(strategy, &body.body, spec_sig, acc, callback)
        })
    }
}

impl Fold for Callback {
    type Id = InFile<CallbackId>;

    fn fold<'a, T>(
        sema: &Semantic,
        strategy: Strategy,
        id: InFile<CallbackId>,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        let body = sema.db.callback_body(id);
        body.sigs.iter().fold(initial, |acc, spec_sig| {
            FoldCtx::fold_type_spec_sig(strategy, &body.body, spec_sig, acc, callback)
        })
    }
}

impl Fold for TypeAlias {
    type Id = InFile<TypeAliasId>;

    fn fold<'a, T>(
        sema: &Semantic,
        strategy: Strategy,
        id: Self::Id,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        let body = sema.db.type_body(id);
        FoldCtx::fold_type_expr(strategy, &body.body, body.ty, initial, callback)
    }
}

impl Fold for Record {
    type Id = InFile<RecordId>;

    fn fold<'a, T>(
        sema: &Semantic,
        strategy: Strategy,
        id: Self::Id,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        let body = sema.db.record_body(id);
        body.fields.iter().fold(initial, |acc, item| {
            FoldCtx::fold_record_field_body(strategy, &body.body, item, acc, callback)
        })
    }
}

impl Fold for Attribute {
    type Id = InFile<AttributeId>;

    fn fold<'a, T>(
        sema: &Semantic,
        strategy: Strategy,
        id: Self::Id,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        let body = sema.db.attribute_body(id);
        FoldCtx::fold_term(strategy, &body.body, body.value, initial, callback)
    }
}

impl Fold for CompileOption {
    type Id = InFile<CompileOptionId>;

    fn fold<'a, T>(
        sema: &Semantic,
        strategy: Strategy,
        id: Self::Id,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        let body = sema.db.compile_body(id);
        FoldCtx::fold_term(strategy, &body.body, body.value, initial, callback)
    }
}

impl Fold for Define {
    type Id = InFile<DefineId>;

    fn fold<'a, T>(
        sema: &Semantic,
        strategy: Strategy,
        id: Self::Id,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        if let Some(body) = sema.db.define_body(id) {
            FoldCtx::fold_expr(strategy, &body.body, body.expr, initial, callback)
        } else {
            initial
        }
    }
}

// ---------------------------------------------------------------------

/// Fold over the contents of a file.
#[allow(dead_code)] // Until the balance of the stack lands and it gets used
pub fn fold_file<'a, T>(
    sema: &Semantic,
    strategy: Strategy,
    file_id: FileId,
    initial: T,
    callback: AnyCallBack<'a, T>,
    form_callback: &'a mut dyn FnMut(T, On, FormIdx) -> T,
) -> T {
    let form_list = sema.form_list(file_id);
    let r = form_list.forms().iter().fold(initial, |r, &form_idx| {
        let r = form_callback(r, On::Entry, form_idx);
        let r = match form_idx {
            FormIdx::FunctionClause(function_id) => {
                // We now have only one clause per function, with the
                // FunctionDefId derived from the FunctionId of the
                // first one. So print the whole thing when we have a
                // valid FunctionDefid.
                let def_map = sema.db.def_map(file_id);
                let function_def_id = InFile::new(file_id, FunctionDefId::new(function_id));
                if let Some(_fun_def) = def_map.get_by_function_id(&function_def_id) {
                    sema.fold_function(strategy, function_def_id, r, &mut |a, _, b| callback(a, b))
                } else {
                    r
                }
            }
            FormIdx::TypeAlias(type_alias_id) => sema.fold::<TypeAlias, T>(
                strategy,
                InFile::new(file_id, type_alias_id),
                r,
                &mut |acc, ctx| callback(acc, ctx),
            ),
            FormIdx::Spec(spec_id) => sema.fold::<Spec, T>(
                strategy,
                InFile::new(file_id, spec_id),
                r,
                &mut |acc, ctx| callback(acc, ctx),
            ),
            FormIdx::Callback(callback_id) => sema.fold::<Callback, T>(
                strategy,
                InFile::new(file_id, callback_id),
                r,
                &mut |acc, ctx| callback(acc, ctx),
            ),
            FormIdx::Record(record_id) => sema.fold::<Record, T>(
                strategy,
                InFile::new(file_id, record_id),
                r,
                &mut |acc, ctx| callback(acc, ctx),
            ),
            FormIdx::Attribute(attribute_id) => sema.fold::<Attribute, T>(
                strategy,
                InFile::new(file_id, attribute_id),
                r,
                &mut |acc, ctx| callback(acc, ctx),
            ),
            FormIdx::CompileOption(attribute_id) => sema.fold::<CompileOption, T>(
                strategy,
                InFile::new(file_id, attribute_id),
                r,
                &mut |acc, ctx| callback(acc, ctx),
            ),
            FormIdx::PPDirective(idx) => {
                if let PPDirective::Define(define_id) = &form_list[idx] {
                    sema.fold::<Define, T>(
                        strategy,
                        InFile::new(file_id, *define_id),
                        r,
                        &mut |acc, ctx| callback(acc, ctx),
                    )
                } else {
                    r
                }
            }
            _ => {
                // Will have to do some time?
                r
            }
        };
        form_callback(r, On::Exit, form_idx)
    });
    r
}

// ---------------------------------------------------------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum On {
    Entry,
    Exit,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constructor {
    Guard,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParentId {
    HirIdx(HirIdx),
    Constructor(Constructor),
}

#[derive(Debug)]
pub struct AnyCallBackCtx<'a> {
    pub in_macro: Option<HirIdx>,
    pub parents: &'a Vec<ParentId>,
    pub item_id: AnyExprId,
    pub item: AnyExpr,
    pub body_origin: BodyOrigin,
}

impl<'a> AnyCallBackCtx<'a> {
    pub fn body_with_expr_source(
        &self,
        sema: &Semantic,
    ) -> Option<(Arc<Body>, Arc<BodySourceMap>, ExprSource)> {
        let (body, source) = match self.body_origin {
            BodyOrigin::Invalid(_) => None,
            BodyOrigin::FormIdx { file_id, form_id } => sema.get_body_and_map(file_id, form_id),
            BodyOrigin::Define { file_id, define_id } => {
                let (define_body, body_map) = sema
                    .db
                    .define_body_with_source(InFile::new(file_id, define_id))?;
                Some((define_body.body.clone(), body_map))
            }
        }?;
        let ast = source.any(self.item_id)?;
        Some((body, source, ast))
    }

    pub fn find_range(&self, sema: &Semantic) -> Option<(Arc<Body>, TextRange)> {
        let (body, _, ast) = self.body_with_expr_source(sema)?;
        Some((body, ast.range()))
    }

    pub fn form_id(&self) -> Option<FormIdx> {
        match self.body_origin {
            BodyOrigin::Invalid(_) => None,
            BodyOrigin::FormIdx {
                file_id: _,
                form_id,
            } => Some(form_id),
            BodyOrigin::Define {
                file_id: _,
                define_id: _,
            } => None,
        }
    }

    pub fn in_guard(&self) -> bool {
        self.parents.iter().any(|parent_idx| {
            if let ParentId::Constructor(Constructor::Guard) = parent_idx {
                true
            } else {
                false
            }
        })
    }
}

pub type AnyCallBack<'a, T> = &'a mut dyn FnMut(T, AnyCallBackCtx) -> T;

pub struct FoldCtx<'a, T> {
    body_origin: BodyOrigin,
    body: &'a FoldBody<'a>,
    strategy: Strategy,
    macro_stack: Vec<HirIdx>,
    parents: Vec<ParentId>,
    callback: AnyCallBack<'a, T>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Strategy {
    pub macros: MacroStrategy,
    pub parens: ParenStrategy,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum MacroStrategy {
    /// Fold over HIR, but do not call back for macro expansions, only
    /// the macro call and its arguments. This gives a view of the code as written.
    DoNotExpand,
    /// Call back for a macro expansion, but hide the macro call that is expanded.
    Expand,
    /// Call back for a macro expansion, and also for the macro call that is expanded.
    ExpandButIncludeMacroCall,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ParenStrategy {
    /// `Expr::Paren` will show up in the fold `AnyCallBackCtx` too
    VisibleParens,
    /// Seamlessly expand parens
    InvisibleParens,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum VisibleMacros {
    Yes,
    No,
}

impl<'a> FoldBody<'a> {
    fn normalise_expr(&self, expr: &Expr) -> Expr {
        match expr {
            Expr::Call {
                target:
                    CallTarget::Remote {
                        module,
                        name,
                        parens,
                    },
                args,
            } => match self.parens {
                ParenStrategy::VisibleParens => Expr::Call {
                    target: CallTarget::Remote {
                        module: module.clone(),
                        name: name.clone(),
                        parens: *parens,
                    },
                    args: args.clone(),
                },
                ParenStrategy::InvisibleParens => Expr::Call {
                    target: CallTarget::Remote {
                        module: module.clone(),
                        name: name.clone(),
                        parens: false,
                    },
                    args: args.clone(),
                },
            },
            _ => expr.clone(),
        }
    }
}

impl<'a, T> FoldCtx<'a, T> {
    fn new(
        strategy: Strategy,
        body: &'a FoldBody<'a>,
        body_origin: BodyOrigin,
        callback: AnyCallBack<'a, T>,
    ) -> FoldCtx<'a, T> {
        FoldCtx {
            body_origin,
            body,
            strategy,
            macro_stack: Vec::default(),
            parents: Vec::default(),
            callback,
        }
    }

    fn new_with_parents(
        strategy: Strategy,
        body: &'a FoldBody<'a>,
        body_origin: BodyOrigin,
        parents: Vec<ParentId>,
        callback: AnyCallBack<'a, T>,
    ) -> FoldCtx<'a, T> {
        FoldCtx {
            body_origin,
            body,
            strategy,
            macro_stack: Vec::default(),
            parents,
            callback,
        }
    }

    pub fn fold_expr(
        strategy: Strategy,
        body: &'a Body,
        expr_id: ExprId,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        FoldCtx::new(strategy, &fold_body(strategy, body), body.origin, callback)
            .do_fold_expr(expr_id, initial)
    }

    pub fn fold_expr_with_parents(
        strategy: Strategy,
        body: &'a Body,
        expr_id: ExprId,
        parents: Vec<ParentId>,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        FoldCtx::new_with_parents(
            strategy,
            &fold_body(strategy, body),
            body.origin,
            parents,
            callback,
        )
        .do_fold_expr(expr_id, initial)
    }

    pub fn fold_exprs(
        strategy: Strategy,
        body: &'a Body,
        expr_ids: &[ExprId],
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        FoldCtx::new(strategy, &fold_body(strategy, body), body.origin, callback)
            .do_fold_exprs(expr_ids, initial)
    }

    pub fn fold_pat(
        strategy: Strategy,
        body: &'a Body,
        pat_id: PatId,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        FoldCtx::new(strategy, &fold_body(strategy, body), body.origin, callback)
            .do_fold_pat(pat_id, initial)
    }

    fn in_macro(&self) -> Option<HirIdx> {
        self.macro_stack.first().copied()
    }

    pub fn fold_term(
        strategy: Strategy,
        body: &'a Body,
        term_id: TermId,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        FoldCtx::new(strategy, &fold_body(strategy, body), body.origin, callback)
            .do_fold_term(term_id, initial)
    }

    pub fn fold_type_expr(
        strategy: Strategy,
        body: &'a Body,
        type_expr_id: TypeExprId,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        FoldCtx::new(strategy, &fold_body(strategy, body), body.origin, callback)
            .do_fold_type_expr(type_expr_id, initial)
    }

    pub fn fold_type_exprs(
        strategy: Strategy,
        body: &'a Body,
        type_expr_ids: &[TypeExprId],
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        FoldCtx::new(strategy, &fold_body(strategy, body), body.origin, callback)
            .do_fold_type_exprs(type_expr_ids, initial)
    }

    pub fn fold_type_spec_sig(
        strategy: Strategy,
        body: &'a Body,
        spec_sig: &SpecSig,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        let fold_body = &fold_body(strategy, body);
        let mut ctx = FoldCtx::new(strategy, &fold_body, body.origin, callback);
        let r = ctx.do_fold_type_exprs(&spec_sig.args, initial);
        ctx.macro_stack = Vec::default();
        let r = ctx.do_fold_type_expr(spec_sig.result, r);

        let r = spec_sig.guards.iter().fold(r, |acc, (_, type_expr_id)| {
            ctx.macro_stack = Vec::default();
            ctx.do_fold_type_expr(*type_expr_id, acc)
        });
        r
    }

    pub fn fold_record_field_body(
        strategy: Strategy,
        body: &'a Body,
        record_field_body: &RecordFieldBody,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        let fold_body = fold_body(strategy, body);
        let mut ctx = FoldCtx::new(strategy, &fold_body, body.origin, callback);
        ctx.macro_stack = Vec::default();
        let r = if let Some(expr_id) = record_field_body.expr {
            ctx.do_fold_expr(expr_id, initial)
        } else {
            initial
        };
        let r = if let Some(type_expr_id) = record_field_body.ty {
            ctx.do_fold_type_expr(type_expr_id, r)
        } else {
            r
        };
        r
    }

    // -----------------------------------------------------------------

    fn do_fold_expr(&mut self, expr_id: ExprId, initial: T) -> T {
        self.parents.push(ParentId::HirIdx(HirIdx {
            body_origin: self.body_origin,
            idx: AnyExprId::Expr(expr_id),
        }));
        let expr = &self.body[expr_id];
        let ctx = AnyCallBackCtx {
            in_macro: self.in_macro(),
            parents: &self.parents,
            item_id: AnyExprId::Expr(expr_id),
            item: AnyExpr::Expr(self.body.normalise_expr(expr)),
            body_origin: self.body_origin,
        };
        let acc = (self.callback)(initial, ctx);
        let r = match expr {
            crate::Expr::Missing => acc,
            crate::Expr::Literal(_) => acc,
            crate::Expr::Var(_) => acc,
            crate::Expr::Match { lhs, rhs } => {
                let r = self.do_fold_pat(*lhs, acc);
                self.do_fold_expr(*rhs, r)
            }
            crate::Expr::Tuple { exprs } => self.do_fold_exprs(exprs, acc),
            crate::Expr::List { exprs, tail } => {
                let r = self.do_fold_exprs(exprs, acc);
                if let Some(expr_id) = tail {
                    self.do_fold_expr(*expr_id, r)
                } else {
                    r
                }
            }
            crate::Expr::Binary { segs } => segs.iter().fold(acc, |acc, binary_seg| {
                let mut r = self.do_fold_expr(binary_seg.elem, acc);
                if let Some(expr_id) = binary_seg.size {
                    r = self.do_fold_expr(expr_id, r);
                }
                r
            }),
            crate::Expr::UnaryOp { expr, op: _ } => self.do_fold_expr(*expr, acc),
            crate::Expr::BinaryOp { lhs, rhs, op: _ } => {
                let r = self.do_fold_expr(*lhs, acc);
                self.do_fold_expr(*rhs, r)
            }
            crate::Expr::Record { name: _, fields } => fields
                .iter()
                .fold(acc, |acc, (_, field)| self.do_fold_expr(*field, acc)),
            crate::Expr::RecordUpdate {
                expr,
                name: _,
                fields,
            } => {
                let r = self.do_fold_expr(*expr, acc);
                fields
                    .iter()
                    .fold(r, |acc, (_, field)| self.do_fold_expr(*field, acc))
            }
            crate::Expr::RecordIndex { name: _, field: _ } => acc,
            crate::Expr::RecordField {
                expr,
                name: _,
                field: _,
            } => self.do_fold_expr(*expr, acc),
            crate::Expr::Map { fields } => fields.iter().fold(acc, |acc, (k, v)| {
                let r = self.do_fold_expr(*k, acc);
                self.do_fold_expr(*v, r)
            }),
            crate::Expr::MapUpdate { expr, fields } => {
                let r = self.do_fold_expr(*expr, acc);
                fields.iter().fold(r, |acc, (lhs, _op, rhs)| {
                    let r = self.do_fold_expr(*lhs, acc);
                    self.do_fold_expr(*rhs, r)
                })
            }
            crate::Expr::Catch { expr } => self.do_fold_expr(*expr, acc),
            crate::Expr::MacroCall {
                expansion,
                args,
                macro_def: _,
            } => {
                let r = if self.strategy.macros == MacroStrategy::DoNotExpand {
                    self.do_fold_exprs(args, acc)
                } else {
                    self.macro_stack.push(HirIdx {
                        body_origin: self.body_origin,
                        idx: AnyExprId::Expr(expr_id),
                    });
                    let e = self.do_fold_expr(*expansion, acc);
                    self.macro_stack.pop();
                    e
                };
                r
            }
            crate::Expr::Call { target, args } => {
                let r = match target {
                    CallTarget::Local { name } => self.do_fold_expr(*name, acc),
                    CallTarget::Remote { module, name, .. } => {
                        let r = self.do_fold_expr(*module, acc);
                        self.do_fold_expr(*name, r)
                    }
                };
                args.iter().fold(r, |acc, arg| self.do_fold_expr(*arg, acc))
            }
            crate::Expr::Comprehension { builder, exprs } => match builder {
                ComprehensionBuilder::List(expr) => self.fold_comprehension(expr, exprs, acc),
                ComprehensionBuilder::Binary(expr) => self.fold_comprehension(expr, exprs, acc),
                ComprehensionBuilder::Map(key, value) => {
                    let r = self.fold_comprehension(key, exprs, acc);
                    self.fold_comprehension(value, exprs, r)
                }
            },
            crate::Expr::Block { exprs } => exprs
                .iter()
                .fold(acc, |acc, expr_id| self.do_fold_expr(*expr_id, acc)),
            crate::Expr::If { clauses } => clauses.iter().fold(acc, |acc, clause| {
                self.parents.push(ParentId::Constructor(Constructor::Guard));
                let r = clause.guards.iter().fold(acc, |acc, exprs| {
                    exprs
                        .iter()
                        .fold(acc, |acc, expr| self.do_fold_expr(*expr, acc))
                });
                self.parents.pop();
                clause
                    .exprs
                    .iter()
                    .fold(r, |acc, expr| self.do_fold_expr(*expr, acc))
            }),
            crate::Expr::Case { expr, clauses } => {
                let r = self.do_fold_expr(*expr, acc);
                self.fold_cr_clause(clauses, r)
            }
            crate::Expr::Receive { clauses, after } => {
                let mut r = self.fold_cr_clause(clauses, acc);
                if let Some(after) = after {
                    r = self.do_fold_expr(after.timeout, r);
                    r = self.do_fold_exprs(&after.exprs, r);
                };
                r
            }
            crate::Expr::Try {
                exprs,
                of_clauses,
                catch_clauses,
                after,
            } => {
                let r = exprs
                    .iter()
                    .fold(acc, |acc, expr| self.do_fold_expr(*expr, acc));
                let mut r = self.fold_cr_clause(of_clauses, r);
                r = catch_clauses.iter().fold(r, |acc, clause| {
                    let mut r = acc;
                    if let Some(pat_id) = clause.class {
                        r = self.do_fold_pat(pat_id, r);
                    }
                    r = self.do_fold_pat(clause.reason, r);
                    if let Some(pat_id) = clause.stack {
                        r = self.do_fold_pat(pat_id, r);
                    }

                    self.parents.push(ParentId::Constructor(Constructor::Guard));
                    r = clause
                        .guards
                        .iter()
                        .fold(r, |acc, exprs| self.do_fold_exprs(exprs, acc));
                    self.parents.pop();
                    clause
                        .exprs
                        .iter()
                        .fold(r, |acc, expr| self.do_fold_expr(*expr, acc))
                });
                after
                    .iter()
                    .fold(r, |acc, expr| self.do_fold_expr(*expr, acc))
            }
            crate::Expr::CaptureFun { target, arity } => {
                let r = match target {
                    CallTarget::Local { name } => self.do_fold_expr(*name, acc),
                    CallTarget::Remote { module, name, .. } => {
                        let r = self.do_fold_expr(*module, acc);
                        self.do_fold_expr(*name, r)
                    }
                };
                self.do_fold_expr(*arity, r)
            }
            crate::Expr::Closure { clauses, name: _ } => clauses.iter().fold(
                acc,
                |acc,
                 Clause {
                     pats,
                     guards,
                     exprs,
                 }| {
                    let mut r = pats
                        .iter()
                        .fold(acc, |acc, pat_id| self.do_fold_pat(*pat_id, acc));
                    self.parents.push(ParentId::Constructor(Constructor::Guard));
                    r = guards
                        .iter()
                        .fold(r, |acc, exprs| self.do_fold_exprs(exprs, acc));
                    self.parents.pop();
                    self.do_fold_exprs(exprs, r)
                },
            ),
            Expr::Maybe {
                exprs,
                else_clauses,
            } => {
                let r = exprs.iter().fold(acc, |acc, expr| match expr {
                    MaybeExpr::Cond { lhs, rhs } => {
                        let r = self.do_fold_pat(*lhs, acc);
                        self.do_fold_expr(*rhs, r)
                    }
                    MaybeExpr::Expr(expr) => self.do_fold_expr(*expr, acc),
                });
                self.fold_cr_clause(else_clauses, r)
            }
            crate::Expr::Paren { expr } => self.do_fold_expr(*expr, acc),
        };
        self.parents.pop();
        r
    }

    fn do_fold_pat(&mut self, pat_id: PatId, initial: T) -> T {
        self.parents.push(ParentId::HirIdx(HirIdx {
            body_origin: self.body_origin,
            idx: AnyExprId::Pat(pat_id),
        }));
        let pat = &self.body[pat_id];
        let ctx = AnyCallBackCtx {
            in_macro: self.in_macro(),
            parents: &self.parents,
            item_id: AnyExprId::Pat(pat_id),
            item: AnyExpr::Pat(pat.clone()),
            body_origin: self.body_origin,
        };
        let acc = (self.callback)(initial, ctx);
        let r = match &pat {
            crate::Pat::Missing => acc,
            crate::Pat::Literal(_) => acc,
            crate::Pat::Var(_) => acc,
            crate::Pat::Match { lhs, rhs } => {
                let r = self.do_fold_pat(*lhs, acc);
                self.do_fold_pat(*rhs, r)
            }
            crate::Pat::Tuple { pats } => self.fold_pats(pats, acc),
            crate::Pat::List { pats, tail } => {
                let mut r = self.fold_pats(pats, acc);
                if let Some(pat_id) = tail {
                    r = self.do_fold_pat(*pat_id, r);
                };
                r
            }
            crate::Pat::Binary { segs } => segs.iter().fold(acc, |acc, binary_seg| {
                let mut r = self.do_fold_pat(binary_seg.elem, acc);
                if let Some(expr_id) = binary_seg.size {
                    r = self.do_fold_expr(expr_id, r);
                }
                r
            }),
            crate::Pat::UnaryOp { pat, op: _ } => self.do_fold_pat(*pat, acc),
            crate::Pat::BinaryOp { lhs, rhs, op: _ } => {
                let r = self.do_fold_pat(*lhs, acc);
                self.do_fold_pat(*rhs, r)
            }
            crate::Pat::Record { name: _, fields } => fields
                .iter()
                .fold(acc, |acc, (_, field)| self.do_fold_pat(*field, acc)),
            crate::Pat::RecordIndex { name: _, field: _ } => acc,
            crate::Pat::Map { fields } => fields.iter().fold(acc, |acc, (k, v)| {
                let r = self.do_fold_expr(*k, acc);
                self.do_fold_pat(*v, r)
            }),
            crate::Pat::MacroCall {
                expansion,
                args,
                macro_def: _,
            } => {
                let r = self.do_fold_pat(*expansion, acc);
                args.iter().fold(r, |acc, arg| self.do_fold_expr(*arg, acc))
            }
        };
        self.parents.pop();
        r
    }

    fn do_fold_exprs(&mut self, exprs: &[ExprId], initial: T) -> T {
        exprs
            .iter()
            .fold(initial, |acc, expr_id| self.do_fold_expr(*expr_id, acc))
    }

    fn fold_pats(&mut self, pats: &[PatId], initial: T) -> T {
        pats.iter()
            .fold(initial, |acc, expr_id| self.do_fold_pat(*expr_id, acc))
    }

    fn fold_cr_clause(&mut self, clauses: &[CRClause], initial: T) -> T {
        clauses.iter().fold(initial, |acc, clause| {
            let mut r = self.do_fold_pat(clause.pat, acc);

            self.parents.push(ParentId::Constructor(Constructor::Guard));
            r = clause.guards.iter().fold(r, |acc, exprs| {
                exprs
                    .iter()
                    .fold(acc, |acc, expr| self.do_fold_expr(*expr, acc))
            });
            self.parents.pop();
            clause
                .exprs
                .iter()
                .fold(r, |acc, expr| self.do_fold_expr(*expr, acc))
        })
    }

    fn fold_comprehension(&mut self, expr: &ExprId, exprs: &[ComprehensionExpr], initial: T) -> T {
        let r = self.do_fold_expr(*expr, initial);
        exprs
            .iter()
            .fold(r, |acc, comprehension_expr| match comprehension_expr {
                ComprehensionExpr::BinGenerator { pat, expr } => {
                    let r = self.do_fold_pat(*pat, acc);
                    self.do_fold_expr(*expr, r)
                }
                ComprehensionExpr::ListGenerator { pat, expr } => {
                    let r = self.do_fold_pat(*pat, acc);
                    self.do_fold_expr(*expr, r)
                }
                ComprehensionExpr::Expr(expr) => self.do_fold_expr(*expr, acc),
                ComprehensionExpr::MapGenerator { key, value, expr } => {
                    let r = self.do_fold_pat(*key, acc);
                    let r = self.do_fold_pat(*value, r);
                    self.do_fold_expr(*expr, r)
                }
            })
    }

    pub fn do_fold_term(&mut self, term_id: TermId, initial: T) -> T {
        self.parents.push(ParentId::HirIdx(HirIdx {
            body_origin: self.body_origin,
            idx: AnyExprId::Term(term_id),
        }));
        let term = &self.body[term_id];
        let ctx = AnyCallBackCtx {
            in_macro: self.in_macro(),
            parents: &self.parents,
            item_id: AnyExprId::Term(term_id),
            item: AnyExpr::Term(term.clone()),
            body_origin: self.body_origin,
        };
        let acc = (self.callback)(initial, ctx);
        let r = match &term {
            crate::Term::Missing => acc,
            crate::Term::Literal(_) => acc,
            crate::Term::Binary(_) => acc, // Limited translation of binaries in terms
            crate::Term::Tuple { exprs } => self.do_fold_terms(exprs, acc),
            crate::Term::List { exprs, tail } => {
                let r = self.do_fold_terms(exprs, acc);
                if let Some(term_id) = tail {
                    self.do_fold_term(*term_id, r)
                } else {
                    r
                }
            }
            crate::Term::Map { fields } => fields.iter().fold(acc, |acc, (k, v)| {
                let r = self.do_fold_term(*k, acc);
                self.do_fold_term(*v, r)
            }),
            crate::Term::CaptureFun {
                module: _,
                name: _,
                arity: _,
            } => acc,
            crate::Term::MacroCall {
                expansion,
                args: _,
                macro_def: _,
            } => {
                // We ignore the args for now
                self.do_fold_term(*expansion, acc)
            }
        };
        self.parents.pop();
        r
    }

    fn do_fold_terms(&mut self, terms: &[TermId], initial: T) -> T {
        terms
            .iter()
            .fold(initial, |acc, expr_id| self.do_fold_term(*expr_id, acc))
    }

    pub fn do_fold_type_expr(&mut self, type_expr_id: TypeExprId, initial: T) -> T {
        self.parents.push(ParentId::HirIdx(HirIdx {
            body_origin: self.body_origin,
            idx: AnyExprId::TypeExpr(type_expr_id),
        }));
        let type_expr = &self.body[type_expr_id];
        let ctx = AnyCallBackCtx {
            in_macro: self.in_macro(),
            parents: &self.parents,
            item_id: AnyExprId::TypeExpr(type_expr_id),
            item: AnyExpr::TypeExpr(type_expr.clone()),
            body_origin: self.body_origin,
        };
        let acc = (self.callback)(initial, ctx);
        let r = match &type_expr {
            TypeExpr::Missing => acc,
            TypeExpr::AnnType { var: _, ty } => self.do_fold_type_expr(*ty, acc),
            TypeExpr::BinaryOp { lhs, rhs, op: _ } => {
                let r = self.do_fold_type_expr(*lhs, acc);
                self.do_fold_type_expr(*rhs, r)
            }
            TypeExpr::Call { target, args } => {
                let r = match target {
                    CallTarget::Local { name } => self.do_fold_type_expr(*name, acc),
                    CallTarget::Remote { module, name, .. } => {
                        let r = self.do_fold_type_expr(*module, acc);
                        self.do_fold_type_expr(*name, r)
                    }
                };
                args.iter()
                    .fold(r, |acc, arg| self.do_fold_type_expr(*arg, acc))
            }
            TypeExpr::Fun(fun) => match fun {
                FunType::Any => acc,
                FunType::AnyArgs { result } => self.do_fold_type_expr(*result, acc),
                FunType::Full { params, result } => {
                    let r = self.do_fold_type_exprs(params, acc);
                    self.do_fold_type_expr(*result, r)
                }
            },
            TypeExpr::List(list_type) => match list_type {
                ListType::Empty => acc,
                ListType::Regular(ty) => self.do_fold_type_expr(*ty, acc),
                ListType::NonEmpty(ty) => self.do_fold_type_expr(*ty, acc),
            },
            TypeExpr::Literal(_) => acc,
            TypeExpr::Map { fields } => fields.iter().fold(acc, |acc, (k, _o, v)| {
                let r = self.do_fold_type_expr(*k, acc);
                self.do_fold_type_expr(*v, r)
            }),
            TypeExpr::Union { types } => self.do_fold_type_exprs(types, acc),
            TypeExpr::Range { lhs, rhs } => {
                let r = self.do_fold_type_expr(*lhs, acc);
                self.do_fold_type_expr(*rhs, r)
            }
            TypeExpr::Record { name: _, fields } => fields
                .iter()
                .fold(acc, |acc, (_, field)| self.do_fold_type_expr(*field, acc)),
            TypeExpr::Tuple { args } => args
                .iter()
                .fold(acc, |acc, ty| self.do_fold_type_expr(*ty, acc)),
            TypeExpr::UnaryOp { type_expr, op: _ } => self.do_fold_type_expr(*type_expr, acc),
            TypeExpr::Var(_) => acc,
            TypeExpr::MacroCall {
                expansion,
                args,
                macro_def: _,
            } => {
                let r = if self.strategy.macros == MacroStrategy::DoNotExpand {
                    self.do_fold_exprs(args, acc)
                } else {
                    self.macro_stack.push(HirIdx {
                        body_origin: self.body_origin,
                        idx: AnyExprId::TypeExpr(type_expr_id),
                    });
                    let e = self.do_fold_type_expr(*expansion, acc);
                    self.macro_stack.pop();
                    e
                };
                r
            }
        };
        self.parents.pop();
        r
    }

    fn do_fold_type_exprs(&mut self, types: &[TypeExprId], initial: T) -> T {
        types.iter().fold(initial, |acc, type_expr_id| {
            self.do_fold_type_expr(*type_expr_id, acc)
        })
    }
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use elp_base_db::fixture::WithFixture;
    use elp_base_db::FileId;
    use elp_syntax::algo;
    use elp_syntax::ast;
    use elp_syntax::AstNode;
    use expect_test::expect;
    use expect_test::Expect;
    use la_arena::Idx;
    use la_arena::RawIdx;

    use super::fold_file;
    use super::MacroStrategy;
    use super::ParenStrategy;
    use crate::db::InternDatabase;
    use crate::expr::AnyExpr;
    use crate::fold::FoldCtx;
    use crate::fold::ParentId;
    use crate::fold::Strategy;
    use crate::form_list::Form;
    use crate::test_db::TestDB;
    use crate::AnyExprId;
    use crate::AnyExprRef;
    use crate::Atom;
    use crate::CallTarget;
    use crate::Expr;
    use crate::FormIdx;
    use crate::FunctionClauseBody;
    use crate::FunctionClauseId;
    use crate::InFile;
    use crate::Literal;
    use crate::On;
    use crate::Pat;
    use crate::Semantic;
    use crate::Term;
    use crate::TypeExpr;

    fn to_atom(sema: &Semantic<'_>, ast: InFile<&ast::Atom>) -> Option<Atom> {
        let (body, body_map) = sema.find_body_and_map(ast.file_id, ast.value.syntax())?;
        let expr = ast.map(|atom| ast::Expr::from(ast::ExprMax::from(atom.clone())));
        let any_expr_id = body_map.any_id(expr.as_ref())?;
        let atom = match body.get_any(any_expr_id) {
            AnyExprRef::Expr(Expr::Literal(Literal::Atom(atom))) => atom,
            AnyExprRef::Pat(Pat::Literal(Literal::Atom(atom))) => atom,
            AnyExprRef::TypeExpr(TypeExpr::Literal(Literal::Atom(atom))) => atom,
            AnyExprRef::Term(Term::Literal(Literal::Atom(atom))) => atom,
            _ => return None,
        };

        Some(atom.clone())
    }

    #[test]
    fn traverse_expr() {
        let fixture_str = r#"
bar() ->
  begin
    A = B + 3,
    [A|A],
    Y = ~A,
    catch A,
    begin
      A,
      Y = 6
    end,
    A
  end.
"#;

        let (db, file_id, range_or_offset) = TestDB::with_range_or_offset(fixture_str);
        let sema = Semantic::new(&db);
        let offset = match range_or_offset {
            elp_base_db::fixture::RangeOrOffset::Range(_) => panic!(),
            elp_base_db::fixture::RangeOrOffset::Offset(o) => o,
        };
        let in_file = sema.parse(file_id);
        let source_file = in_file.value;
        let ast_var = algo::find_node_at_offset::<ast::Var>(source_file.syntax(), offset).unwrap();

        let function_clause_id: InFile<FunctionClauseId> = InFile {
            file_id,
            value: Idx::from_raw(RawIdx::from(0)),
        };
        let (body, body_map) =
            FunctionClauseBody::function_clause_body_with_source_query(&db, function_clause_id);

        let expr = ast::Expr::ExprMax(ast::ExprMax::Var(ast_var.clone()));
        let expr_id = body_map
            .expr_id(InFile {
                file_id,
                value: &expr,
            })
            .unwrap();
        let expr = &body.body[expr_id];
        let hir_var = match expr {
            crate::Expr::Var(v) => v,
            _ => panic!(),
        };
        let r: u32 = FoldCtx::fold_expr(
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            &body.body,
            body.clause.exprs[0],
            0,
            &mut |acc, ctx| match ctx.item {
                AnyExpr::Expr(Expr::Var(v)) => {
                    if &v == hir_var {
                        acc + 1
                    } else {
                        acc
                    }
                }
                AnyExpr::Pat(Pat::Var(v)) => {
                    if &v == hir_var {
                        acc + 1
                    } else {
                        acc
                    }
                }
                _ => acc,
            },
        );

        // There are 7 occurrences of the Var "A" in the code example
        expect![[r#"
            7
        "#]]
        .assert_debug_eq(&r);
        expect![[r#"
            Var {
                syntax: VAR@51..52
                  VAR@51..52 "A"
                ,
            }
        "#]]
        .assert_debug_eq(&ast_var);
    }

    #[test]
    fn traverse_term() {
        let fixture_str = r#"
-compile([{f~oo,bar},[baz, {foo}]]).
"#;

        let (db, file_id, range_or_offset) = TestDB::with_range_or_offset(fixture_str);
        let sema = Semantic::new(&db);
        let offset = match range_or_offset {
            elp_base_db::fixture::RangeOrOffset::Range(_) => panic!(),
            elp_base_db::fixture::RangeOrOffset::Offset(o) => o,
        };
        let in_file = sema.parse(file_id);
        let source_file = in_file.value;
        let ast_atom =
            algo::find_node_at_offset::<ast::Atom>(source_file.syntax(), offset).unwrap();
        let hir_atom = to_atom(&sema, InFile::new(file_id, &ast_atom)).unwrap();

        let form_list = sema.form_list(file_id);
        let (idx, _) = form_list.compile_attributes().next().unwrap();
        let compiler_options = sema.db.compile_body(InFile::new(file_id, idx));
        let r = FoldCtx::fold_term(
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            &compiler_options.body,
            compiler_options.value,
            0,
            &mut |acc, ctx| match &ctx.item {
                AnyExpr::Term(Term::Literal(Literal::Atom(atom))) => {
                    if atom == &hir_atom {
                        acc + 1
                    } else {
                        acc
                    }
                }
                _ => acc,
            },
        );

        // There are 2 occurrences of the atom 'foo' in the code example
        expect![[r#"
            2
        "#]]
        .assert_debug_eq(&r);
        expect![[r#"
            Atom {
                syntax: ATOM@11..14
                  ATOM@11..14 "foo"
                ,
            }
        "#]]
        .assert_debug_eq(&ast_atom);
    }

    #[track_caller]
    fn check_macros_expr(
        strategy: Strategy,
        fixture_str: &str,
        tree_expect: Expect,
        r_expect: Expect,
    ) {
        let (db, file_id, range_or_offset) = TestDB::with_range_or_offset(fixture_str);
        let sema = Semantic::new(&db);
        let offset = match range_or_offset {
            elp_base_db::fixture::RangeOrOffset::Range(_) => panic!(),
            elp_base_db::fixture::RangeOrOffset::Offset(o) => o,
        };
        let in_file = sema.parse(file_id);
        let source_file = in_file.value;
        let ast_atom =
            algo::find_node_at_offset::<ast::Atom>(source_file.syntax(), offset).unwrap();
        let hir_atom = to_atom(&sema, InFile::new(file_id, &ast_atom)).unwrap();

        let form_list = sema.form_list(file_id);
        let (function_clause_idx, _) = form_list.function_clauses().next().unwrap();
        let function_body = sema
            .db
            .function_clause_body(InFile::new(file_id, function_clause_idx));

        let r = FoldCtx::fold_expr(
            strategy,
            &function_body.body,
            function_body.clause.exprs[0],
            (0, 0),
            &mut |(in_macro, not_in_macro), ctx| match ctx.item {
                AnyExpr::Expr(Expr::Literal(Literal::Atom(atom))) => {
                    if atom == hir_atom {
                        if ctx.in_macro.is_some() {
                            (in_macro + 1, not_in_macro)
                        } else {
                            (in_macro, not_in_macro + 1)
                        }
                    } else {
                        (in_macro, not_in_macro)
                    }
                }
                _ => (in_macro, not_in_macro),
            },
        );
        tree_expect.assert_eq(&function_body.tree_print(&db));

        r_expect.assert_debug_eq(&r);
    }

    #[test]
    fn macro_aware_full_traversal_expr() {
        check_macros_expr(
            Strategy {
                macros: MacroStrategy::ExpandButIncludeMacroCall,
                parens: ParenStrategy::InvisibleParens,
            },
            r#"
             -define(AA(X), {X,foo}).
             bar() ->
               begin %% clause.exprs[0]
                 ?AA(f~oo),
                 {foo}
               end.
            "#,
            expect![[r#"

            Clause {
                pats
                guards
                exprs
                    Expr::Block {
                        Expr::Tuple {
                            Literal(Atom('foo')),
                            Literal(Atom('foo')),
                        },
                        Expr::Tuple {
                            Literal(Atom('foo')),
                        },
                    },
            }
        "#]],
            expect![[r#"
            (
                2,
                1,
            )
        "#]],
        )
    }

    #[test]
    fn macro_aware_surface_traversal_expr() {
        check_macros_expr(
            Strategy {
                macros: MacroStrategy::DoNotExpand,
                parens: ParenStrategy::InvisibleParens,
            },
            r#"
             -define(AA(X), {X,foo}).
             bar() ->
               begin %% clause.exprs[0]
                 ?AA(f~oo),
                 {foo}
               end.
            "#,
            expect![[r#"

            Clause {
                pats
                guards
                exprs
                    Expr::Block {
                        Expr::Tuple {
                            Literal(Atom('foo')),
                            Literal(Atom('foo')),
                        },
                        Expr::Tuple {
                            Literal(Atom('foo')),
                        },
                    },
            }
        "#]],
            expect![[r#"
            (
                0,
                2,
            )
        "#]],
        )
    }

    #[test]
    fn ignore_macros_expr() {
        check_macros_expr(
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            r#"
             -define(AA(X), {X,foo}).
             bar() ->
               begin %% clause.exprs[0]
                 ?AA(f~oo),
                 {foo}
               end.
            "#,
            expect![[r#"

            Clause {
                pats
                guards
                exprs
                    Expr::Block {
                        Expr::Tuple {
                            Literal(Atom('foo')),
                            Literal(Atom('foo')),
                        },
                        Expr::Tuple {
                            Literal(Atom('foo')),
                        },
                    },
            }
        "#]],
            expect![[r#"
            (
                0,
                3,
            )
        "#]],
        )
    }

    // -----------------------------------------------------------------
    // type expressions

    #[track_caller]
    fn check_traverse_type(fixture_str: &str, n: u32) {
        let (db, file_id, range_or_offset) = TestDB::with_range_or_offset(fixture_str);
        let sema = Semantic::new(&db);
        let offset = match range_or_offset {
            elp_base_db::fixture::RangeOrOffset::Range(_) => panic!(),
            elp_base_db::fixture::RangeOrOffset::Offset(o) => o,
        };
        let in_file = sema.parse(file_id);
        let source_file = in_file.value;
        let ast_atom =
            algo::find_node_at_offset::<ast::Atom>(source_file.syntax(), offset).unwrap();
        let hir_atom = to_atom(&sema, InFile::new(file_id, &ast_atom)).unwrap();

        let form_list = sema.form_list(file_id);
        let (idx, _) = form_list.type_aliases().next().unwrap();
        let type_alias = sema.db.type_body(InFile::new(file_id, idx));
        let r =
            FoldCtx::fold_type_expr(
                Strategy {
                    macros: MacroStrategy::Expand,
                    parens: ParenStrategy::InvisibleParens,
                },
                &type_alias.body,
                type_alias.ty,
                0,
                &mut |acc, ctx| match &ctx.item {
                    AnyExpr::TypeExpr(TypeExpr::Literal(Literal::Atom(atom))) => {
                        if atom == &hir_atom { acc + 1 } else { acc }
                    }
                    _ => acc,
                },
            );

        // Number of occurrences of the atom 'foo' in the code example
        assert_eq!(n, r);
        expect![[r#"foo"#]].assert_eq(&ast_atom.raw_text());
    }

    #[test]
    fn traverse_type_call() {
        let fixture_str = r#"
                 -type bar() :: f~oo().
                 "#;
        check_traverse_type(fixture_str, 1)
    }

    #[test]
    fn traverse_type_ann() {
        let fixture_str = r#"
                 -type bar() :: A :: f~oo().
                 "#;
        check_traverse_type(fixture_str, 1)
    }

    #[test]
    fn traverse_type_binary_op() {
        let fixture_str = r#"
                 -type bar() :: f~oo + foo.
                 "#;
        check_traverse_type(fixture_str, 2)
    }

    #[test]
    fn traverse_type_fun_1() {
        let fixture_str = r#"
                 -type foo1() :: fun() + fo~o.
                 "#;
        check_traverse_type(fixture_str, 1)
    }

    #[test]
    fn traverse_type_fun_2() {
        let fixture_str = r#"
                 -type foo3() :: fun((...) -> f~oo).
                 "#;
        check_traverse_type(fixture_str, 1)
    }

    #[test]
    fn traverse_type_fun_3() {
        let fixture_str = r#"
                 -type foo3() :: fun((a, f~oo) -> foo).
                 "#;
        check_traverse_type(fixture_str, 2)
    }

    #[test]
    fn traverse_type_list_1() {
        let fixture_str = r#"
                 -type foo3() :: [f~oo()].
                 "#;
        check_traverse_type(fixture_str, 1)
    }

    #[test]
    fn traverse_type_list_2() {
        let fixture_str = r#"
                 -type foo3() :: [f~oo(),...].
                 "#;
        check_traverse_type(fixture_str, 1)
    }

    #[test]
    fn traverse_type_map_1() {
        let fixture_str = r#"
                 -type foo() :: #{a => foo, fo~o := d}.
                 "#;
        check_traverse_type(fixture_str, 2)
    }

    #[test]
    fn traverse_type_union_1() {
        let fixture_str = r#"
                 -type foo() :: a | f~oo.
                 "#;
        check_traverse_type(fixture_str, 1)
    }

    #[test]
    fn traverse_type_range_1() {
        let fixture_str = r#"
                 -type foo() :: fo~o | 1..100.
                 "#;
        check_traverse_type(fixture_str, 1)
    }

    #[test]
    fn traverse_type_record_1() {
        let fixture_str = r#"
                 -type foo2(B) :: #record{a :: foo(), fo~o :: B}.
                 "#;
        // Note: traversal does not look into the record field names
        check_traverse_type(fixture_str, 1)
    }

    #[test]
    fn traverse_type_tuple_1() {
        let fixture_str = r#"
                 -type bar() :: {fo~o, bar, foo()}.
                 "#;
        check_traverse_type(fixture_str, 2)
    }

    #[test]
    fn traverse_type_unary_op() {
        let fixture_str = r#"
                 -type bar() :: {-1, f~oo}.
                 "#;
        check_traverse_type(fixture_str, 1)
    }

    // -----------------------------------------------------------------
    // type expression macro traversals

    #[track_caller]
    fn check_macros_type_expr(
        strategy: Strategy,
        fixture_str: &str,
        tree_expect: Expect,
        r_expect: Expect,
    ) {
        let (db, file_id, range_or_offset) = TestDB::with_range_or_offset(fixture_str);
        let sema = Semantic::new(&db);
        let offset = match range_or_offset {
            elp_base_db::fixture::RangeOrOffset::Range(_) => panic!(),
            elp_base_db::fixture::RangeOrOffset::Offset(o) => o,
        };
        let in_file = sema.parse(file_id);
        let source_file = in_file.value;
        let ast_atom =
            algo::find_node_at_offset::<ast::Atom>(source_file.syntax(), offset).unwrap();
        let hir_atom = to_atom(&sema, InFile::new(file_id, &ast_atom)).unwrap();

        let form_list = sema.form_list(file_id);
        let (idx, type_alias) = form_list.type_aliases().next().unwrap();
        let type_alias_body = sema.db.type_body(InFile::new(file_id, idx));

        let r = FoldCtx::fold_type_expr(
            strategy,
            &type_alias_body.body,
            type_alias_body.ty,
            (0, 0),
            &mut |(in_macro, not_in_macro), ctx| match ctx.item {
                AnyExpr::TypeExpr(TypeExpr::Literal(Literal::Atom(atom))) => {
                    if atom == hir_atom {
                        if ctx.in_macro.is_some() {
                            (in_macro + 1, not_in_macro)
                        } else {
                            (in_macro, not_in_macro + 1)
                        }
                    } else {
                        (in_macro, not_in_macro)
                    }
                }
                AnyExpr::Expr(Expr::Literal(Literal::Atom(atom))) => {
                    // For macro args
                    if atom == hir_atom {
                        if ctx.in_macro.is_some() {
                            (in_macro + 1, not_in_macro)
                        } else {
                            (in_macro, not_in_macro + 1)
                        }
                    } else {
                        (in_macro, not_in_macro)
                    }
                }
                _ => (in_macro, not_in_macro),
            },
        );
        tree_expect.assert_eq(&type_alias_body.tree_print(&db, type_alias));

        r_expect.assert_debug_eq(&r);
    }

    #[test]
    fn macro_aware_full_traversal_type_expr() {
        check_macros_type_expr(
            Strategy {
                macros: MacroStrategy::ExpandButIncludeMacroCall,
                parens: ParenStrategy::InvisibleParens,
            },
            r#"
             -define(AA(X), {X,foo}).
             -type baz() :: {fo~o, ?AA(foo)}.
            "#,
            expect![[r#"

                -type baz() :: TypeExpr::Tuple {
                    Literal(Atom('foo')),
                    TypeExpr::Tuple {
                        Literal(Atom('foo')),
                        Literal(Atom('foo')),
                    },
                }.
            "#]],
            expect![[r#"
                (
                    2,
                    1,
                )
            "#]],
        )
    }

    #[test]
    fn macro_aware_surface_traversal_type_expr() {
        check_macros_type_expr(
            Strategy {
                macros: MacroStrategy::DoNotExpand,
                parens: ParenStrategy::InvisibleParens,
            },
            r#"
             -define(AA(X), {X,foo}).
             -type baz() :: {fo~o, ?AA(foo)}.
            "#,
            expect![[r#"

                -type baz() :: TypeExpr::Tuple {
                    Literal(Atom('foo')),
                    TypeExpr::Tuple {
                        Literal(Atom('foo')),
                        Literal(Atom('foo')),
                    },
                }.
        "#]],
            expect![[r#"
            (
                0,
                2,
            )
        "#]],
        )
    }

    #[test]
    fn ignore_macros_type_expr() {
        check_macros_type_expr(
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            r#"
             -define(AA(X), {X,foo}).
             -type baz() :: {fo~o, ?AA(foo)}.
            "#,
            expect![[r#"

                -type baz() :: TypeExpr::Tuple {
                    Literal(Atom('foo')),
                    TypeExpr::Tuple {
                        Literal(Atom('foo')),
                        Literal(Atom('foo')),
                    },
                }.
        "#]],
            expect![[r#"
            (
                0,
                3,
            )
        "#]],
        )
    }

    // end of testing type expression traversals
    // -----------------------------------------------------------------

    #[track_caller]
    fn count_atom_foo(fixture_str: &str, n: u32) {
        count_atom_foo_with_strategy(
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            fixture_str,
            n,
        )
    }

    #[track_caller]
    fn count_atom_foo_with_strategy(strategy: Strategy, fixture_str: &str, n: u32) {
        let (db, file_id, range_or_offset) = TestDB::with_range_or_offset(fixture_str);
        let sema = Semantic::new(&db);
        let offset = match range_or_offset {
            elp_base_db::fixture::RangeOrOffset::Range(_) => panic!(),
            elp_base_db::fixture::RangeOrOffset::Offset(o) => o,
        };
        let in_file = sema.parse(file_id);
        let source_file = in_file.value;

        let ast_atom =
            algo::find_node_at_offset::<ast::Atom>(source_file.syntax(), offset).unwrap();
        expect![[r#"foo"#]].assert_eq(&ast_atom.raw_text());
        let hir_atom_str = ast_atom.raw_text();

        let form_list = sema.db.file_form_list(file_id);

        let r: u32 = fold_file(
            &sema,
            strategy,
            file_id,
            0,
            &mut |acc, ctx| match ctx.item {
                AnyExpr::Expr(Expr::Literal(Literal::Atom(atom))) => {
                    let atom_name = db.lookup_atom(atom);
                    if atom_name.as_str() == hir_atom_str {
                        acc + 1
                    } else {
                        acc
                    }
                }
                AnyExpr::Pat(Pat::Literal(Literal::Atom(atom))) => {
                    let atom_name = db.lookup_atom(atom);
                    if atom_name.as_str() == hir_atom_str {
                        acc + 1
                    } else {
                        acc
                    }
                }
                AnyExpr::TypeExpr(TypeExpr::Literal(Literal::Atom(atom))) => {
                    let atom_name = db.lookup_atom(atom);
                    if atom_name.as_str() == hir_atom_str {
                        acc + 1
                    } else {
                        acc
                    }
                }
                AnyExpr::Term(Term::Literal(Literal::Atom(atom))) => {
                    let atom_name = db.lookup_atom(atom);
                    if atom_name.as_str() == hir_atom_str {
                        acc + 1
                    } else {
                        acc
                    }
                }
                _ => acc,
            },
            &mut |acc, on, form_id: FormIdx| {
                if on == On::Entry {
                    match form_list.get(form_id) {
                        Form::ModuleAttribute(ma) => {
                            if ma.name.as_str() == hir_atom_str.as_str() {
                                acc + 1
                            } else {
                                acc
                            }
                        }
                        Form::FunctionClause(_) => acc,
                        Form::PPDirective(_) => acc,
                        Form::PPCondition(_) => acc,
                        Form::Export(_) => acc,
                        Form::Import(_) => acc,
                        Form::TypeExport(_) => acc,
                        Form::Behaviour(_) => acc,
                        Form::TypeAlias(_) => acc,
                        Form::Spec(_) => acc,
                        Form::Callback(_) => acc,
                        Form::OptionalCallbacks(_) => acc,
                        Form::Record(_) => acc,
                        Form::Attribute(_) => acc,
                        Form::ModuleDocAttribute(_) => acc,
                        Form::DocAttribute(_) => acc,
                        Form::CompileOption(_) => acc,
                        Form::DeprecatedAttribute(_) => acc,
                        Form::FeatureAttribute(_) => acc,
                    }
                } else {
                    acc
                }
            },
        );

        // Count of the occurrences of the atom 'foo' in the code example
        assert_eq!(r, n);
    }

    #[test]
    fn traverse_file_function_1() {
        let fixture_str = r#"
               -module(foo).
               -export([bar/1]).
               bar(0) ->
                 foo;
               bar(X) ->
                 case X of
                   foo -> bar;
                   baz -> 'foo';
                   _ -> f~oo
                 end.
               "#;
        count_atom_foo(fixture_str, 5);
    }

    #[test]
    fn traverse_type_alias() {
        let fixture_str = r#"
               -module(foo).
               -type epp_handle() :: fo~o().
               "#;
        count_atom_foo(fixture_str, 2);
    }

    #[test]
    fn traverse_spec() {
        let fixture_str = r#"
               -module(foo).
               -spec fff() -> fo~o() | foo.
               "#;
        count_atom_foo(fixture_str, 3);
    }

    #[test]
    fn traverse_callback() {
        let fixture_str = r#"
               -module(foo).
               -callback fff() -> fo~o() | foo.
               "#;
        count_atom_foo(fixture_str, 3);
    }

    #[test]
    fn traverse_record() {
        let fixture_str = r#"
               -module(foo).
               -record(r1, {f1 :: f~oo(), foo}).
               "#;
        // Note: fold does not look into field names
        count_atom_foo(fixture_str, 2);
    }

    #[test]
    fn traverse_attribute() {
        let fixture_str = r#"
               -module(foo).
               -wild(r1, {f1, f~oo}).
               "#;
        count_atom_foo(fixture_str, 2);
    }

    #[test]
    fn traverse_compile_option() {
        let fixture_str = r#"
               -module(foo).
               -compile([fo~o, export_all, {foo, nowarn_export_all}]).
               "#;
        count_atom_foo(fixture_str, 3);
    }

    #[test]
    fn traverse_define() {
        let fixture_str = r#"
               -module(foo).
               -define(FOO(X), foo(X,fo~o)).
               "#;
        count_atom_foo(fixture_str, 3);
    }

    #[test]
    fn traverse_macro_clause_1() {
        let fixture_str = r#"
               -module(foo).
               fo~o() -> ok.
               -define(FOO(Res), bar(_) -> Res).
               ?FOO([foo()]).
               "#;
        // We do not see the function name (not looking)
        count_atom_foo(fixture_str, 2);
    }

    #[test]
    fn traverse_macro_clause_surface_only() {
        let fixture_str = r#"
               -define(FOO(Args), bar() -> {Args, Args}).

               fo~o() -> 1.
               ?FOO([foo()]).
               "#;
        // We do not see the function name (not looking)
        count_atom_foo_with_strategy(
            Strategy {
                macros: MacroStrategy::DoNotExpand,
                parens: ParenStrategy::InvisibleParens,
            },
            fixture_str,
            1,
        );
    }

    #[test]
    fn traverse_macro_clause_invisible_macros() {
        let fixture_str = r#"
               -define(FOO(Args), bar() -> {Args, Args}).

               fo~o() -> 1.
               ?FOO([foo()]).
               "#;
        count_atom_foo_with_strategy(
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            fixture_str,
            2,
        );
    }

    #[test]
    fn traverse_macro_clause_visible_macros() {
        let fixture_str = r#"
               -define(FOO(Args), bar() -> {Args, Args}).

               fo~o() -> 1.
               ?FOO([foo()]).
               "#;
        // We do not see the arguments separately, it is up to us to
        // explicitly look at them if we care.
        count_atom_foo_with_strategy(
            Strategy {
                macros: MacroStrategy::ExpandButIncludeMacroCall,
                parens: ParenStrategy::InvisibleParens,
            },
            fixture_str,
            2,
        );
    }

    // -----------------------------------------------------------------
    // Testing MacroCall having macro definition

    #[track_caller]
    fn macro_expansion_origin(fixture_str: &str, expected: Vec<String>) {
        let (db, file_id, _range_or_offset) = TestDB::with_range_or_offset(fixture_str);
        let sema = Semantic::new(&db);

        let r: Vec<_> = fold_file(
            &sema,
            Strategy {
                macros: MacroStrategy::ExpandButIncludeMacroCall,
                parens: ParenStrategy::InvisibleParens,
            },
            file_id,
            Vec::new(),
            &mut |mut acc, ctx| match ctx.item {
                AnyExpr::Expr(Expr::MacroCall {
                    expansion: _,
                    args: _,
                    macro_def,
                }) => {
                    if let Some(def) = macro_def {
                        acc.push(format!("Expr:{:?}", def));
                        acc
                    } else {
                        acc
                    }
                }
                AnyExpr::Pat(Pat::MacroCall {
                    expansion: _,
                    args: _,
                    macro_def,
                }) => {
                    if let Some(def) = macro_def {
                        acc.push(format!("Pat:{:?}", def));
                        acc
                    } else {
                        acc
                    }
                }
                AnyExpr::TypeExpr(TypeExpr::MacroCall {
                    expansion: _,
                    args: _,
                    macro_def,
                }) => {
                    if let Some(def) = macro_def {
                        acc.push(format!("TypeExpr:{:?}", def));
                        acc
                    } else {
                        acc
                    }
                }
                AnyExpr::Term(Term::MacroCall {
                    expansion: _,
                    args: _,
                    macro_def,
                }) => {
                    if let Some(def) = macro_def {
                        acc.push(format!("Term:{:?}", def));
                        acc
                    } else {
                        acc
                    }
                }
                _ => acc,
            },
            &mut |acc, _on, _form_id: FormIdx| acc,
        );

        assert_eq!(r, expected);
    }

    #[test]
    fn macro_expansion() {
        let fixture_str = r#"
               -module(f~oo).
               -define(FOO(X), X).
               -type bar() :: ?FOO(none()).
               -wild(?FOO(atom)).
               blah() ->
                 ?FOO(X) = ?FOO(4).
               "#;
        macro_expansion_origin(
            fixture_str,
            vec![
                "TypeExpr:InFile { file_id: FileId(0), value: Idx::<Define>(0) }".to_string(),
                "Term:InFile { file_id: FileId(0), value: Idx::<Define>(0) }".to_string(),
                "Pat:InFile { file_id: FileId(0), value: Idx::<Define>(0) }".to_string(),
                "Expr:InFile { file_id: FileId(0), value: Idx::<Define>(0) }".to_string(),
            ],
        );
    }

    // -----------------------------------------------------------------

    #[track_caller]
    fn parent_is_anonymous_fun(fixture_str: &str, expected: bool) {
        let (db, position, _diagnostics_enabled) = TestDB::with_position(fixture_str);
        let sema = Semantic::new(&db);
        let offset = position.offset;
        let file_id = position.file_id;
        let in_file = sema.parse(file_id);
        let source_file = in_file.value;

        let ast_atom =
            algo::find_node_at_offset::<ast::Atom>(source_file.syntax(), offset).unwrap();
        expect![[r#"foo"#]].assert_eq(&ast_atom.raw_text());
        let hir_atom_str = ast_atom.raw_text();

        let r: bool = fold_file(
            &sema,
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            file_id,
            false,
            &mut |acc, ctx| match ctx.item {
                AnyExpr::Expr(Expr::Literal(Literal::Atom(atom))) => {
                    let atom_name = db.lookup_atom(atom);
                    if atom_name.as_str() == hir_atom_str {
                        let (body, _) = sema
                            .get_body_and_map(file_id, ctx.form_id().unwrap())
                            .unwrap();
                        ctx.parents.iter().any(|parent_id| match parent_id {
                            ParentId::HirIdx(hir_idx) => match hir_idx.idx {
                                AnyExprId::Expr(idx) => match body[idx] {
                                    Expr::Closure { .. } => true,
                                    _ => false,
                                },
                                _ => false,
                            },
                            ParentId::Constructor(_) => false,
                        })
                    } else {
                        acc
                    }
                }
                _ => acc,
            },
            &mut |acc, _on, _form_id| acc,
        );

        assert_eq!(r, expected);
    }

    #[test]
    fn in_anonymous_fun() {
        let fixture_str = r#"
               -module(a_module).
               -export([bar/0]).
               bar() ->
                 F = fun() -> fo~o end,
                 F().
               "#;
        parent_is_anonymous_fun(fixture_str, true);
    }

    #[test]
    fn not_in_anonymous_fun() {
        let fixture_str = r#"
               -module(a_mdoule).
               -export([bar/0]).
               bar() ->
                 begin
                   F = fo~o,
                   F
                 end.
               "#;
        parent_is_anonymous_fun(fixture_str, false);
    }

    // -----------------------------------------------------------------
    // Start of testing paren visibility

    #[track_caller]
    fn count_parens(fixture_str: &str, n: u32) {
        let (db, fixture) = TestDB::with_fixture(fixture_str);
        let file_id = fixture.files[0];
        let sema = Semantic::new(&db);

        let r = do_count_parens(&sema, ParenStrategy::InvisibleParens, file_id);
        assert_eq!(r, 0);

        let r = do_count_parens(&sema, ParenStrategy::VisibleParens, file_id);
        assert_eq!(r, n);
    }

    fn do_count_parens(sema: &Semantic, parens: ParenStrategy, file_id: FileId) -> u32 {
        fold_file(
            &sema,
            Strategy {
                macros: MacroStrategy::Expand,
                parens,
            },
            file_id,
            0,
            &mut |acc, ctx| match ctx.item {
                AnyExpr::Expr(Expr::Paren { .. }) => acc + 1,
                AnyExpr::Expr(Expr::Call { target, .. }) => match target {
                    CallTarget::Local { .. } => acc,
                    CallTarget::Remote { parens, .. } => {
                        if parens {
                            acc + 1
                        } else {
                            acc
                        }
                    }
                },
                _ => acc,
            },
            &mut |acc, _on, _form_id| acc,
        )
    }

    #[test]
    fn parens_basic() {
        let fixture_str = r#"
              foo(A) ->
                X = (A + (1 * 2)). 
             "#;
        count_parens(fixture_str, 2);
    }

    #[test]
    fn parens_in_maybe() {
        let fixture_str = r#"
              foo(A, B) ->
                maybe
                    {ok, A} ?= a(),
                    true = (A >= 0),
                    {ok, B} ?= b(),
                    ((A) + B)
                end.
             "#;
        count_parens(fixture_str, 3);
    }

    #[test]
    fn parens_in_call_target_simple() {
        let fixture_str = r#"
              foo(A, B) ->
                X = ((bar))(A),
                Y = (modu):(fn)(B),
                X + Y.
             "#;
        count_parens(fixture_str, 4);
    }

    #[test]
    fn parens_in_call_target_remote() {
        let fixture_str = r#"
              foo(A) ->
                Y = (modu:fn)(A),
                Z = ((modu):((fn)))(A).
             "#;
        count_parens(fixture_str, 5);
    }

    // End of testing paren visibility
    // -----------------------------------------------------------------
}

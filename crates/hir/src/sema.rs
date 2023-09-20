/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::iter::FromIterator;
use std::ops::Index;
use std::sync::Arc;
use std::vec::IntoIter;

use elp_base_db::module_name;
use elp_base_db::FileId;
use elp_base_db::ModuleIndex;
use elp_base_db::ModuleName;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::SyntaxNode;
use elp_syntax::TextRange;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use la_arena::RawIdx;

use self::find::FindForm;
pub use self::to_def::CallDef;
pub use self::to_def::DefinitionOrReference;
pub use self::to_def::FaDef;
pub use self::to_def::MacroCallDef;
use self::to_def::ToDef;
use crate::body::scope::ScopeId;
use crate::body::UnexpandedIndex;
use crate::db::MinDefDatabase;
use crate::edoc::EdocHeader;
use crate::expr::AstClauseId;
use crate::expr::ClauseId;
use crate::fold::ExprCallBack;
use crate::fold::ExprCallBackCtx;
use crate::fold::FoldBody;
use crate::fold::FoldCtx;
use crate::fold::PatCallBack;
use crate::fold::PatCallBackCtx;
use crate::fold::Strategy;
pub use crate::intern::MinInternDatabase;
pub use crate::intern::MinInternDatabaseStorage;
use crate::resolver::Resolution;
use crate::resolver::Resolver;
use crate::Body;
use crate::BodySourceMap;
use crate::CRClause;
use crate::Clause;
use crate::DefMap;
use crate::Expr;
use crate::ExprId;
use crate::File;
use crate::FormIdx;
use crate::FormList;
use crate::FunctionBody;
use crate::FunctionId;
use crate::InFile;
use crate::InFileAstPtr;
use crate::Literal;
use crate::MacroName;
use crate::Module;
use crate::Name;
use crate::PPDirective;
use crate::Pat;
use crate::PatId;
use crate::SpecId;
use crate::Term;
use crate::TermId;
use crate::TypeExpr;
use crate::TypeExprId;
use crate::Var;
use crate::VarDef;

mod find;
pub(crate) mod to_def;

pub struct ModuleIter(Arc<ModuleIndex>);

impl IntoIterator for ModuleIter {
    type Item = ModuleName;

    type IntoIter = IntoIter<ModuleName>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.all_modules().into_iter()
    }
}

/// Primary API to get Semantic information from HIR
pub struct Semantic<'db> {
    pub db: &'db dyn MinDefDatabase,
}

impl<'db> Semantic<'db> {
    pub fn new<Db: MinDefDatabase>(db: &'db Db) -> Self {
        Self { db }
    }
}

impl<'db> Semantic<'db> {
    pub fn parse(&self, file_id: FileId) -> InFile<ast::SourceFile> {
        InFile::new(file_id, self.db.parse(file_id).tree())
    }

    pub fn def_map(&self, file_id: FileId) -> Arc<DefMap> {
        self.db.def_map(file_id)
    }

    pub fn form_list(&self, file_id: FileId) -> Arc<FormList> {
        self.db.file_form_list(file_id)
    }

    pub fn to_def<T: ToDef>(&self, ast: InFile<&T>) -> Option<T::Def> {
        ToDef::to_def(self, ast)
    }

    pub fn to_expr(&self, expr: InFile<&ast::Expr>) -> Option<InFunctionBody<ExprId>> {
        let function_id = self.find_enclosing_function(expr.file_id, expr.value.syntax())?;
        let (body, body_map) = self
            .db
            .function_body_with_source(expr.with_value(function_id));
        let expr_id = &body_map.expr_id(expr)?;
        Some(InFunctionBody {
            body,
            function_id: expr.with_value(function_id),
            body_map: Some(body_map).into(),
            value: *expr_id,
        })
    }

    pub fn to_function_body(&self, function_id: InFile<FunctionId>) -> InFunctionBody<()> {
        let (body, body_map) = self.db.function_body_with_source(function_id);
        InFunctionBody {
            body,
            function_id,
            body_map: Some(body_map).into(),
            value: (),
        }
    }

    pub fn resolve_module_names(&self, from_file: FileId) -> Option<ModuleIter> {
        let source_root_id = self.db.file_source_root(from_file);
        let project_id = self.db.app_data(source_root_id)?.project_id;
        let module_index = self.db.module_index(project_id);
        Some(ModuleIter(module_index))
    }

    pub fn module_name(&self, file_id: FileId) -> Option<ModuleName> {
        module_name(self.db.upcast(), file_id)
    }

    pub fn resolve_module_name(&self, file_id: FileId, name: &str) -> Option<Module> {
        let source_root_id = self.db.file_source_root(file_id);
        let project_id = self.db.app_data(source_root_id)?.project_id;
        let module_index = self.db.module_index(project_id);
        let module_file_id = module_index.file_for_module(name)?;
        Some(Module {
            file: File {
                file_id: module_file_id,
            },
        })
    }

    pub fn file_edoc_comments(
        &self,
        file_id: FileId,
    ) -> Option<FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>> {
        self.db.file_edoc_comments(file_id)
    }

    pub fn form_edoc_comments(&self, form: InFileAstPtr<ast::Form>) -> Option<EdocHeader> {
        let file_edoc = self.file_edoc_comments(form.file_id())?;
        file_edoc.get(&form).cloned()
    }

    pub fn resolve_var_to_pats(&self, var_in: InFile<&ast::Var>) -> Option<Vec<PatId>> {
        let function_id = self.find_enclosing_function(var_in.file_id, var_in.value.syntax())?;
        let ast_clause_id = self.find_enclosing_function_clause(var_in.value.syntax())?;
        let resolver = self.ast_clause_resolver(var_in.with_value(function_id), ast_clause_id)?;
        let expr = ast::Expr::ExprMax(ast::ExprMax::Var(var_in.value.clone()));
        if let Some(expr_id) = resolver.expr_id_ast(self.db, var_in.with_value(&expr)) {
            let var = resolver[expr_id].as_var()?;
            resolver.value.resolve_expr_id(&var, expr_id).cloned()
        } else {
            let pat_id = resolver.pat_id_ast(self.db, var_in.with_value(&expr))?;
            let var = resolver[pat_id].as_var()?;
            resolver.value.resolve_pat_id(&var, pat_id).cloned()
        }
    }

    pub fn expand(&self, call: InFile<&ast::MacroCallExpr>) -> Option<(MacroName, String)> {
        let (body, body_source) = self.find_body(call.file_id, call.value.syntax())?;
        let name = body_source.resolved_macro(call)?.name(self.db);
        let expr = ast::Expr::cast(call.value.syntax().clone())?;
        let any_expr_id = body_source.any_id(call.with_value(&expr))?;
        Some((name, body.print_any_expr(self.db.upcast(), any_expr_id)))
    }

    pub fn scope_for(&self, var_in: InFile<&ast::Var>) -> Option<(Resolver, ScopeId)> {
        let function_id = self.find_enclosing_function(var_in.file_id, var_in.value.syntax())?;
        let ast_clause_id = self.find_enclosing_function_clause(var_in.value.syntax())?;
        let resolver = self.ast_clause_resolver(var_in.with_value(function_id), ast_clause_id)?;
        let expr = ast::Expr::ExprMax(ast::ExprMax::Var(var_in.value.clone()));
        if let Some(expr_id) = resolver.expr_id_ast(self.db, var_in.with_value(&expr)) {
            let scope = resolver.value.scopes.scope_for_expr(expr_id)?;
            Some((resolver.value, scope))
        } else {
            let pat_id = resolver.pat_id_ast(self.db, var_in.with_value(&expr))?;
            let scope = resolver.value.scopes.scope_for_pat(pat_id)?;
            Some((resolver.value, scope))
        }
    }

    pub fn find_body(
        &self,
        file_id: FileId,
        syntax: &SyntaxNode,
    ) -> Option<(Arc<Body>, Arc<BodySourceMap>)> {
        let form = syntax.ancestors().find_map(ast::Form::cast)?;
        let form_list = self.db.file_form_list(file_id);
        let form = form_list.find_form(&form)?;
        match form {
            FormIdx::Function(fun) => {
                let (body, map) = self.db.function_body_with_source(InFile::new(file_id, fun));
                Some((body.body.clone(), map))
            }
            FormIdx::Record(record) => {
                let (body, map) = self
                    .db
                    .record_body_with_source(InFile::new(file_id, record));
                Some((body.body.clone(), map))
            }
            FormIdx::Spec(spec) => {
                let (body, map) = self.db.spec_body_with_source(InFile::new(file_id, spec));
                Some((body.body.clone(), map))
            }
            FormIdx::Callback(cb) => {
                let (body, map) = self.db.callback_body_with_source(InFile::new(file_id, cb));
                Some((body.body.clone(), map))
            }
            FormIdx::TypeAlias(alias) => {
                let (body, map) = self.db.type_body_with_source(InFile::new(file_id, alias));
                Some((body.body.clone(), map))
            }
            FormIdx::Attribute(attr) => {
                let (body, map) = self
                    .db
                    .attribute_body_with_source(InFile::new(file_id, attr));
                Some((body.body.clone(), map))
            }
            FormIdx::CompileOption(attr) => {
                let (body, map) = self.db.compile_body_with_source(InFile::new(file_id, attr));
                Some((body.body.clone(), map))
            }
            FormIdx::PPDirective(pp) => match form_list[pp] {
                PPDirective::Define(define) => self
                    .db
                    .define_body_with_source(InFile::new(file_id, define))
                    .map(|(body, map)| (body.body.clone(), map)),
                _ => None,
            },
            _ => None,
        }
    }

    fn find_form<T: FindForm>(&self, ast: InFile<&T>) -> Option<T::Form> {
        FindForm::find(self, ast)
    }

    pub fn find_enclosing_function(
        &self,
        file_id: FileId,
        syntax: &SyntaxNode,
    ) -> Option<FunctionId> {
        let form = syntax.ancestors().find_map(ast::Form::cast)?;
        let form_list = self.db.file_form_list(file_id);
        let form = form_list.find_form(&form)?;
        match form {
            FormIdx::Function(fun) => Some(fun),
            _ => None,
        }
    }

    pub fn find_enclosing_function_clause(&self, syntax: &SyntaxNode) -> Option<AstClauseId> {
        // ClauseId's are allocated sequentially. Find the one we need.
        let fun = syntax.ancestors().find_map(ast::FunDecl::cast)?;
        let idx = fun.clauses().enumerate().find_map(|(idx, clause)| {
            if clause
                .syntax()
                .text_range()
                .contains(syntax.text_range().start())
            {
                Some(idx)
            } else {
                None
            }
        })?;
        Some(AstClauseId::new(ClauseId::from_raw(RawIdx::from(
            idx as u32,
        ))))
    }

    pub fn find_enclosing_spec(&self, file_id: FileId, syntax: &SyntaxNode) -> Option<SpecId> {
        let form = syntax.ancestors().find_map(ast::Form::cast)?;
        let form_list = self.db.file_form_list(file_id);
        let form = form_list.find_form(&form)?;
        match form {
            FormIdx::Spec(fun) => Some(fun),
            _ => None,
        }
    }

    pub fn vardef_source(&self, def: &VarDef) -> ast::Var {
        def.source(self.db.upcast())
    }

    /// Return the free and bound variables in a given ast expression.
    pub fn free_vars_ast(&self, file_id: FileId, expr: &ast::Expr) -> Option<ScopeAnalysis> {
        let function_id = self.find_enclosing_function(file_id, expr.syntax())?;
        let infile_function_id = InFile::new(file_id, function_id);

        let (body, source_map) = self.db.function_body_with_source(infile_function_id);
        let expr_id_in = source_map.expr_id(InFile {
            file_id,
            value: expr,
        })?;
        self.free_vars(&InFunctionBody {
            body,
            function_id: infile_function_id,
            body_map: Some(source_map).into(),
            value: expr_id_in,
        })
    }

    /// Return the free and bound variables in a given expression.
    pub fn free_vars(&self, expr: &InFunctionBody<ExprId>) -> Option<ScopeAnalysis> {
        let function = expr.function_id;
        let scopes = self.db.function_scopes(function);
        let expr_id_in = expr.value;
        let clause_id = expr.clause_id(&expr_id_in)?;
        let clause_scopes = scopes.get(clause_id)?;
        let resolver = Resolver::new(clause_scopes);

        let inside_pats = FoldCtx::fold_expr(
            &expr.body.body,
            Strategy::TopDown,
            expr_id_in,
            FxHashSet::default(),
            &mut |acc, _| acc,
            &mut |mut acc, ctx| {
                acc.insert(ctx.pat_id);
                acc
            },
        );

        let update_vars = |mut analysis: ScopeAnalysis, var_id: Var, defs: Option<&Vec<PatId>>| {
            if let Some(defs) = defs {
                let (inside, outside): (Vec<PatId>, Vec<PatId>) =
                    defs.iter().partition(|pat_id| inside_pats.contains(pat_id));
                if !outside.is_empty() {
                    analysis.free.insert((var_id, outside));
                };
                if !inside.is_empty() {
                    analysis.bound.insert((var_id, inside));
                };
                analysis
            } else {
                analysis
            }
        };

        Some(FoldCtx::fold_expr(
            &expr.body.body,
            Strategy::TopDown,
            expr_id_in,
            ScopeAnalysis::new(),
            &mut |defs, ctx| match ctx.expr {
                Expr::Var(var_id) => {
                    update_vars(defs, var_id, resolver.resolve_expr_id(&var_id, ctx.expr_id))
                }
                _ => defs,
            },
            &mut |defs, ctx| match ctx.pat {
                Pat::Var(var_id) => {
                    update_vars(defs, var_id, resolver.resolve_pat_id(&var_id, ctx.pat_id))
                }
                _ => defs,
            },
        ))
    }

    /// Wrap the `Resolver` for the function clause containing the
    /// `syntax` in an `InFunctionBody`.
    pub fn function_clause_resolver(
        &self,
        file_id: FileId,
        syntax: &SyntaxNode,
    ) -> Option<InFunctionBody<Resolver>> {
        let function_id = self.find_enclosing_function(file_id, syntax)?;
        let ast_clause_id = self.find_enclosing_function_clause(syntax)?;
        self.ast_clause_resolver(InFile::new(file_id, function_id), ast_clause_id)
    }

    pub fn ast_clause_resolver(
        &self,
        function_id: InFile<FunctionId>,
        ast_clause_id: AstClauseId,
    ) -> Option<InFunctionBody<Resolver>> {
        let body = self.db.function_body(function_id);
        let scopes = self.db.function_scopes(function_id);
        let clause_id = body.valid_clause_id(ast_clause_id)?;
        let clause_scopes = scopes.get(clause_id)?;
        let resolver = Resolver::new(clause_scopes);
        Some(InFunctionBody {
            body,
            function_id,
            body_map: None.into(), // We may not need it, do not get it now
            value: resolver,
        })
    }

    pub fn clause_resolver(
        &self,
        function_id: InFile<FunctionId>,
        clause_id: ClauseId,
    ) -> Option<InFunctionBody<Resolver>> {
        let body = self.db.function_body(function_id);
        let scopes = self.db.function_scopes(function_id);
        let clause_scopes = scopes.get(clause_id)?;
        let resolver = Resolver::new(clause_scopes);
        Some(InFunctionBody {
            body,
            function_id,
            body_map: None.into(), // We may not need it, do not get it now
            value: resolver,
        })
    }

    pub fn find_vars_in_clause_ast(&self, expr: &InFile<&ast::Expr>) -> Option<FxHashSet<Var>> {
        let clause = self.find_enclosing_function_clause(expr.value.syntax())?;
        let in_function = self.to_expr(*expr)?;
        let clause_id = in_function.valid_clause_id(clause)?;
        ScopeAnalysis::clause_vars_in_scope(self, &in_function.with_value(&in_function[clause_id]))
    }

    /// Find all other variables within the function clause that resolve
    /// to the one given.
    pub fn find_local_usages(&self, var: InFile<&ast::Var>) -> Option<Vec<ast::Var>> {
        // TODO: replace this function with the appropriate one when the
        // highlight usages feature exists. T128835148
        let var_resolved = self.resolve_var_to_pats(var)?;
        let mut resolved_set = FxHashSet::from_iter(var_resolved);
        let clause = var
            .value
            .syntax()
            .ancestors()
            .find_map(ast::FunctionClause::cast)?;

        // We first extend the resolved_set to the widest one that
        // includes the current variable resolution.  This ensures
        // that if we are looking at a variable in one leg of a case
        // clause, and it has equivalents in another leg, then these
        // are also found.
        clause
            .syntax()
            .descendants()
            .filter_map(ast::Var::cast)
            .for_each(|v| {
                if let Some(ds) = self.resolve_var_to_pats(InFile::new(var.file_id, &v)) {
                    let ds_set = FxHashSet::from_iter(ds);
                    if resolved_set.intersection(&ds_set).next().is_some() {
                        resolved_set.extend(ds_set);
                    }
                }
            });

        // Then we actually check for any variables that resolve to it.
        let vars: Vec<_> = clause
            .syntax()
            .descendants()
            .filter_map(ast::Var::cast)
            .filter_map(|v| {
                if let Some(ds) = self.resolve_var_to_pats(InFile::new(var.file_id, &v)) {
                    // We have resolved a candidate Var.
                    // Check that it resolves to the one we are looking for

                    // We may be in an arm of a case, receive,
                    // try, and so we will only find one
                    // definition. So check for an intersection
                    // with the whole.

                    if resolved_set
                        .intersection(&FxHashSet::from_iter(ds))
                        .next()
                        .is_some()
                    {
                        Some(v)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        if vars.is_empty() { None } else { Some(vars) }
    }

    pub fn fold_function<'a, T>(
        &self,
        function_id: InFile<FunctionId>,
        initial: T,
        for_expr: FunctionExprCallBack<'a, T>,
        for_pat: FunctionPatCallBack<'a, T>,
    ) -> T {
        let function_body = self.db.function_body(function_id);
        fold_function_body(
            WithMacros::No,
            &function_body,
            Strategy::TopDown,
            initial,
            for_expr,
            for_pat,
        )
    }

    pub fn fold_clause<'a, T>(
        &'a self,
        function_id: InFile<FunctionId>,
        clause_id: ClauseId,
        initial: T,
        for_expr: ExprCallBack<'a, T>,
        for_pat: PatCallBack<'a, T>,
    ) -> T {
        let function_body = self.db.function_body(function_id);
        function_body[clause_id]
            .exprs
            .iter()
            .fold(initial, |acc_inner, expr_id| {
                FoldCtx::fold_expr(
                    &function_body.body,
                    Strategy::TopDown,
                    *expr_id,
                    acc_inner,
                    for_expr,
                    for_pat,
                )
            })
    }

    pub fn bound_vars_in_pattern_diagnostic(
        &self,
        file_id: FileId,
    ) -> FxHashSet<(InFile<FunctionId>, PatId, ast::Var)> {
        let def_map = self.def_map(file_id);
        let mut res = FxHashSet::default();
        for def in def_map.get_functions().values() {
            if def.file.file_id == file_id {
                let function_id = InFile::new(file_id, def.function_id);

                self.fold_function(
                    function_id,
                    (),
                    &mut |acc, clause_id, ctx| {
                        if let Some(mut resolver) = self.clause_resolver(function_id, clause_id) {
                            let mut bound_vars =
                                BoundVarsInPat::new(self, &mut resolver, file_id, &mut res);
                            match ctx.expr {
                                Expr::Match { lhs, rhs: _ } => {
                                    bound_vars.report_any_bound_vars(&lhs)
                                }
                                Expr::Case { expr: _, clauses } => {
                                    bound_vars.cr_clauses(&clauses);
                                }
                                Expr::Try {
                                    exprs: _,
                                    of_clauses,
                                    catch_clauses,
                                    after: _,
                                } => {
                                    bound_vars.cr_clauses(&of_clauses);
                                    catch_clauses.iter().for_each(|clause| {
                                        bound_vars.report_any_bound_vars(&clause.reason);
                                    })
                                }
                                _ => {}
                            }
                        };
                        acc
                    },
                    &mut |acc, _, _| acc,
                );
            }
        }
        res
    }

    fn bound_vars_in_pat(
        &self,
        pat_id: &PatId,
        resolver: &mut InFunctionBody<Resolver>,
        file_id: FileId,
    ) -> FxHashSet<(InFile<FunctionId>, PatId, ast::Var)> {
        let parse = self.parse(file_id);
        let body_map = &resolver.get_body_map(self.db);
        FoldCtx::fold_pat(
            &resolver.body.body,
            Strategy::TopDown,
            *pat_id,
            FxHashSet::default(),
            &mut |acc, _| acc,
            &mut |mut acc, ctx| {
                if let Pat::Var(var) = &resolver[ctx.pat_id] {
                    if let Some(pat_ids) = resolver.value.resolve_pat_id(var, ctx.pat_id) {
                        pat_ids.iter().for_each(|def_pat_id| {
                            if &ctx.pat_id != def_pat_id {
                                if let Some(pat_ptr) = body_map.pat(ctx.pat_id) {
                                    if let Some(ast::Expr::ExprMax(ast::ExprMax::Var(var))) =
                                        pat_ptr.to_node(&parse)
                                    {
                                        if var.syntax().text() != "_" {
                                            acc.insert((resolver.function_id, ctx.pat_id, var));
                                        }
                                    }
                                };
                            }
                        });
                    }
                };
                acc
            },
        )
    }

    pub fn is_atom_named(&self, expr: &Expr, known_atom: crate::Name) -> bool {
        match expr {
            Expr::Literal(Literal::Atom(atom)) => self.db.lookup_atom(*atom) == known_atom,
            _ => false,
        }
    }
}

pub type FunctionExprCallBack<'a, T> = &'a mut dyn FnMut(T, ClauseId, ExprCallBackCtx) -> T;
pub type FunctionPatCallBack<'a, T> = &'a mut dyn FnMut(T, ClauseId, PatCallBackCtx) -> T;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WithMacros {
    Yes,
    No,
}

fn fold_function_body<'a, T>(
    with_macros: WithMacros,
    function_body: &FunctionBody,
    strategy: Strategy,
    initial: T,
    for_expr: FunctionExprCallBack<'a, T>,
    for_pat: FunctionPatCallBack<'a, T>,
) -> T {
    function_body
        .clauses
        .iter()
        .fold(initial, |acc, (clause_id, clause)| {
            clause.exprs.iter().fold(acc, |acc_inner, expr_id| {
                let fold_body = if with_macros == WithMacros::Yes {
                    FoldBody::UnexpandedIndex(UnexpandedIndex(&function_body.body))
                } else {
                    FoldBody::Body(&function_body.body)
                };
                FoldCtx::fold_expr_foldbody(
                    &fold_body,
                    strategy,
                    *expr_id,
                    acc_inner,
                    &mut |acc, ctx| for_expr(acc, clause_id, ctx),
                    &mut |acc, ctx| for_pat(acc, clause_id, ctx),
                )
            })
        })
}

// ---------------------------------------------------------------------

struct BoundVarsInPat<'a> {
    sema: &'a Semantic<'a>,
    resolver: &'a mut InFunctionBody<Resolver>,
    file_id: FileId,
    res: &'a mut FxHashSet<(InFile<FunctionId>, PatId, ast::Var)>,
}

impl<'a> BoundVarsInPat<'a> {
    fn new(
        sema: &'a Semantic<'a>,
        resolver: &'a mut InFunctionBody<Resolver>,
        file_id: FileId,
        res: &'a mut FxHashSet<(InFile<FunctionId>, PatId, ast::Var)>,
    ) -> Self {
        BoundVarsInPat {
            sema,
            resolver,
            file_id,
            res,
        }
    }

    fn report_any_bound_vars(&mut self, pat_id: &PatId) {
        let bound_vars = self
            .sema
            .bound_vars_in_pat(pat_id, self.resolver, self.file_id);
        bound_vars.into_iter().for_each(|v| {
            self.res.insert(v);
        });
    }

    fn cr_clauses(&mut self, clauses: &[CRClause]) {
        clauses
            .iter()
            .for_each(|clause| self.report_any_bound_vars(&clause.pat))
    }
}

// ---------------------------------------------------------------------

#[derive(Debug, Default)]
pub struct ScopeAnalysis {
    pub free: FxHashSet<Resolution>,
    pub bound: FxHashSet<Resolution>,
}

impl ScopeAnalysis {
    pub fn new() -> Self {
        Self {
            free: FxHashSet::default(),
            bound: FxHashSet::default(),
        }
    }

    pub fn clause_vars_in_scope(
        sema: &Semantic,
        clause: &InFunctionBody<&Clause>,
    ) -> Option<FxHashSet<Var>> {
        let acc = FxHashSet::default();
        let x = clause.value.exprs.iter().fold(acc, |mut acc, expr| {
            let mut analyzer = ScopeAnalysis::new();
            analyzer.walk_expr(sema, &clause.with_value(*expr));
            analyzer.update_scope_analysis(&mut acc);
            acc
        });
        Some(x)
    }

    pub fn update_scope_analysis(&self, acc: &mut FxHashSet<Var>) {
        acc.extend(self.bound.iter().map(|(v, _p)| v));
        acc.extend(self.free.iter().map(|(v, _p)| v));
    }

    /// Process an expression in the current scope context, updating
    /// the free and bound vars
    pub fn walk_ast_expr(&mut self, sema: &Semantic, file_id: FileId, expr: ast::Expr) {
        if let Some(scopes) = sema.free_vars_ast(file_id, &expr) {
            self.callback(scopes.free, scopes.bound);
        }
    }

    /// Process an expression in the current scope context, updating
    /// the free and bound vars
    pub fn walk_expr(&mut self, sema: &Semantic, expr: &InFunctionBody<ExprId>) {
        if let Some(scopes) = sema.free_vars(expr) {
            self.callback(scopes.free, scopes.bound);
        }
    }

    fn callback(&mut self, free: FxHashSet<Resolution>, bound: FxHashSet<Resolution>) {
        // If any of the free variables are already bound, remove them.
        let (free, _rest): (FxHashSet<Resolution>, FxHashSet<Resolution>) =
            free.into_iter().partition(|v| !self.bound.contains(v));
        self.free.extend(free);
        self.bound.extend(bound);
    }
}

#[derive(Debug, Clone)]
pub struct InFunctionBody<T> {
    body: Arc<FunctionBody>,
    function_id: InFile<FunctionId>,
    // cache body_map if we already have it when wrapping the value.
    // This field should go away once we fully use the hir API only
    body_map: RefCell<Option<Arc<BodySourceMap>>>,
    pub value: T,
}

impl<T> InFunctionBody<T> {
    pub fn new(
        body: Arc<FunctionBody>,
        function_id: InFile<FunctionId>,
        body_map: Option<Arc<BodySourceMap>>,
        value: T,
    ) -> InFunctionBody<T> {
        InFunctionBody {
            body,
            function_id,
            body_map: body_map.into(),
            value,
        }
    }

    pub fn as_ref(&self) -> InFunctionBody<&T> {
        self.with_value(&self.value)
    }

    pub fn with_value<U>(&self, value: U) -> InFunctionBody<U> {
        InFunctionBody {
            body: self.body.clone(),
            function_id: self.function_id,
            body_map: self.body_map.clone(),
            value,
        }
    }

    pub fn file_id(&self) -> FileId {
        self.function_id.file_id
    }

    pub fn get_body_map(&self, db: &dyn MinDefDatabase) -> Arc<BodySourceMap> {
        if let Some(body_map) = &self.body_map.borrow().as_ref() {
            //return explicitly here because borrow is still held in else statement
            //https://stackoverflow.com/questions/30243606/why-is-a-borrow-still-held-in-the-else-block-of-an-if-let
            return Arc::clone(body_map);
        }
        let (_body, body_map) = db.function_body_with_source(self.function_id);
        *self.body_map.borrow_mut() = Some(body_map.clone());
        body_map
    }

    pub fn expr_id(&self, expr: &Expr) -> Option<ExprId> {
        self.body.body.expr_id(expr)
    }

    pub fn expr_id_ast(&self, db: &dyn MinDefDatabase, expr: InFile<&ast::Expr>) -> Option<ExprId> {
        self.get_body_map(db).expr_id(expr)
    }

    pub fn pat_id_ast(&self, db: &dyn MinDefDatabase, expr: InFile<&ast::Expr>) -> Option<PatId> {
        self.get_body_map(db).pat_id(expr)
    }

    pub fn expr_from_id(
        &mut self,
        db: &dyn MinDefDatabase,
        expr: InFile<&ast::Expr>,
    ) -> Option<Expr> {
        let expr_id = self.expr_id_ast(db, expr)?;
        Some(self.body.body[expr_id].clone())
    }

    pub fn valid_clause_id(&self, ast_clause_id: AstClauseId) -> Option<ClauseId> {
        self.body.valid_clause_id(ast_clause_id)
    }

    pub fn clause_id(&self, expr_id: &ExprId) -> Option<ClauseId> {
        // Can we have a reverse lookup for this instead?
        // Or go to syntax, parent, clause id
        let idx = self.body.clauses.iter().find_map(|(idx, clause)| {
            if clause.exprs.iter().any(|expr| {
                FoldCtx::fold_expr(
                    &self.body.body,
                    Strategy::TopDown,
                    *expr,
                    false,
                    &mut |acc, ctx| acc || expr_id == &ctx.expr_id,
                    &mut |acc, _| acc,
                )
            }) {
                Some(idx)
            } else {
                None
            }
        })?;
        Some(idx)
    }

    pub fn clauses(&self) -> impl Iterator<Item = (ClauseId, &Clause)> {
        self.body.clauses.iter()
    }

    pub fn body(&self) -> Arc<Body> {
        self.body.body.clone()
    }

    pub fn fold_expr<'a, R>(
        &self,
        strategy: Strategy,
        expr_id: ExprId,
        initial: R,
        for_expr: ExprCallBack<'a, R>,
        for_pat: PatCallBack<'a, R>,
    ) -> R {
        FoldCtx::fold_expr(
            &self.body.body,
            strategy,
            expr_id,
            initial,
            for_expr,
            for_pat,
        )
    }

    pub fn fold_pat<'a, R>(
        &self,
        strategy: Strategy,
        pat_id: PatId,
        initial: R,
        for_expr: ExprCallBack<'a, R>,
        for_pat: PatCallBack<'a, R>,
    ) -> R {
        FoldCtx::fold_pat(
            &self.body.body,
            strategy,
            pat_id,
            initial,
            for_expr,
            for_pat,
        )
    }

    pub fn fold_function<'a, R>(
        &self,
        initial: R,
        for_expr: FunctionExprCallBack<'a, R>,
        for_pat: FunctionPatCallBack<'a, R>,
    ) -> R {
        fold_function_body(
            WithMacros::No,
            &self.body,
            Strategy::TopDown,
            initial,
            for_expr,
            for_pat,
        )
    }

    pub fn fold_function_with_macros<'a, R>(
        &self,
        strategy: Strategy,
        initial: R,
        for_expr: FunctionExprCallBack<'a, R>,
        for_pat: FunctionPatCallBack<'a, R>,
    ) -> R {
        fold_function_body(
            WithMacros::Yes,
            &self.body,
            strategy,
            initial,
            for_expr,
            for_pat,
        )
    }

    pub fn range_for_expr(&self, db: &dyn MinDefDatabase, expr_id: ExprId) -> Option<TextRange> {
        let body_map = self.get_body_map(db);
        let ast = body_map.expr(expr_id)?;
        Some(ast.range())
    }

    pub fn range_for_pat(&mut self, db: &dyn MinDefDatabase, pat_id: PatId) -> Option<TextRange> {
        let body_map = self.get_body_map(db);
        let ast = body_map.pat(pat_id)?;
        Some(ast.range())
    }

    pub fn as_atom_name(&self, db: &dyn MinDefDatabase, expr: &ExprId) -> Option<Name> {
        Some(db.lookup_atom(self[*expr].as_atom()?))
    }
}

impl<T> Index<ClauseId> for InFunctionBody<T> {
    type Output = Clause;

    fn index(&self, index: ClauseId) -> &Self::Output {
        &self.body[index]
    }
}

impl<T> Index<ExprId> for InFunctionBody<T> {
    type Output = Expr;

    fn index(&self, index: ExprId) -> &Self::Output {
        &self.body.body[index]
    }
}

impl<T> Index<PatId> for InFunctionBody<T> {
    type Output = Pat;

    fn index(&self, index: PatId) -> &Self::Output {
        &self.body.body[index]
    }
}

impl<T> Index<TypeExprId> for InFunctionBody<T> {
    type Output = TypeExpr;

    fn index(&self, index: TypeExprId) -> &Self::Output {
        &self.body.body[index]
    }
}

impl<T> Index<TermId> for InFunctionBody<T> {
    type Output = Term;

    fn index(&self, index: TermId) -> &Self::Output {
        &self.body.body[index]
    }
}

#[cfg(test)]
mod tests {
    use elp_base_db::fixture::WithFixture;
    use elp_base_db::SourceDatabase;
    use elp_syntax::algo::find_node_at_offset;
    use elp_syntax::ast;
    use elp_syntax::AstNode;
    use expect_test::expect;
    use expect_test::Expect;
    use itertools::Itertools;

    use crate::test_db::TestDB;
    use crate::InFile;
    use crate::Semantic;

    #[track_caller]
    fn check_local_usages(fixture_before: &str, expect: Expect) {
        let (db, position) = TestDB::with_position(fixture_before);
        let sema = Semantic::new(&db);

        let file_syntax = db.parse(position.file_id).syntax_node();
        let var: ast::Var = find_node_at_offset(&file_syntax, position.offset).unwrap();
        let usages = sema
            .find_local_usages(InFile {
                file_id: position.file_id,
                value: &var,
            })
            .unwrap();
        expect.assert_debug_eq(&usages);
    }

    #[test]
    fn test_find_local_usages_1() {
        check_local_usages(
            r#"testz() ->
                   case rand:uniform(2) of
                       1 ->
                           Z = 1;
                       2 ->
                           ~Z = 2;
                       Z ->
                           ok
                   end,
                   Z."#,
            expect![[r#"
                [
                    Var {
                        syntax: VAR@109..110
                          VAR@109..110 "Z"
                        ,
                    },
                    Var {
                        syntax: VAR@171..172
                          VAR@171..172 "Z"
                        ,
                    },
                    Var {
                        syntax: VAR@201..202
                          VAR@201..202 "Z"
                        ,
                    },
                    Var {
                        syntax: VAR@279..280
                          VAR@279..280 "Z"
                        ,
                    },
                ]
            "#]],
        )
    }

    #[test]
    fn test_find_local_usages_2() {
        check_local_usages(
            r#"main() ->
                   Y = 5,
                   AssertIs5 = fun (X) ->
                       ~Y = X,
                       erlang:display(Y)
                   end,
                   AssertIs5(2),
                   erlang:display(Y),
                   ok."#,
            expect![[r#"
                [
                    Var {
                        syntax: VAR@29..30
                          VAR@29..30 "Y"
                        ,
                    },
                    Var {
                        syntax: VAR@101..102
                          VAR@101..102 "Y"
                        ,
                    },
                    Var {
                        syntax: VAR@146..147
                          VAR@146..147 "Y"
                        ,
                    },
                    Var {
                        syntax: VAR@240..241
                          VAR@240..241 "Y"
                        ,
                    },
                ]
            "#]],
        )
    }

    #[track_caller]
    fn check_bound_var_in_pattern(fixture: &str) {
        let (db, fixture) = TestDB::with_fixture(fixture);
        let annotations = fixture.annotations(&db);
        let expected: Vec<_> = annotations
            .iter()
            .map(|(fr, _)| fr.range)
            .sorted_by(|a, b| a.start().cmp(&b.start()))
            .collect();
        let file_id = fixture.files[0];
        let sema = Semantic::new(&db);
        let vars = sema.bound_vars_in_pattern_diagnostic(file_id);
        let ranges: Vec<_> = vars
            .iter()
            .map(|(_, _, v)| v.syntax().text_range())
            .sorted_by(|a, b| a.start().cmp(&b.start()))
            .collect();
        assert_eq!(expected, ranges);
    }

    #[test]
    fn bound_variable_in_pattern_1() {
        check_bound_var_in_pattern(
            r#"
              f(Var1) ->
                Var1 = 1.
             %% ^^^^ "#,
        )
    }

    #[test]
    fn bound_variable_in_pattern_2() {
        check_bound_var_in_pattern(
            r#"
              f(Var1) ->
                Var2 = 1."#,
        )
    }

    #[test]
    fn bound_variable_in_pattern_3() {
        check_bound_var_in_pattern(
            r#"
              g(Var2) ->
                case a:b() of
                  {ok, Var2} -> ok;
                    %% ^^^^
                  _ -> error
                end."#,
        )
    }

    #[test]
    fn bound_variable_in_pattern_4() {
        check_bound_var_in_pattern(
            r#"
              h(Var3, Var4) ->
                try a:b() of
                  {New, Var3} ->
                     %% ^^^^
                    New
                catch Var4 ->
                   %% ^^^^
                    error
                end."#,
        )
    }

    #[test]
    fn bound_variable_in_pattern_5() {
        check_bound_var_in_pattern(
            r#"
              fun_expr(New) ->
                fun(New, Var5) ->
                    Var5 = New
                 %% ^^^^
                end."#,
        )
    }

    #[test]
    fn bound_variable_in_pattern_6() {
        check_bound_var_in_pattern(
            r#"
              named_fun_expr() ->
                fun F(New, Var6) ->
                    New = Var6,
                 %% ^^^
                    F = Var6
                 %% ^
                end."#,
        )
    }

    #[test]
    fn bound_variable_in_pattern_not_underscore() {
        check_bound_var_in_pattern(
            // Do not report for '_'
            r#"
             test4(L) ->
                 [H | _] = lists:map(
                   fun app_a_mod2:id/1,
                   L),
                 _  = atom_to_list(H),
                 {H}.
            "#,
        )
    }
}

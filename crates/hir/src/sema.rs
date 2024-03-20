/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::fmt;
use std::iter::FromIterator;
use std::ops::Index;
use std::sync::Arc;
use std::vec::IntoIter;

use elp_base_db::module_name;
use elp_base_db::FileId;
use elp_base_db::FileRange;
use elp_base_db::ModuleIndex;
use elp_base_db::ModuleName;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::SyntaxNode;
use elp_syntax::TextRange;
use elp_types_db::eqwalizer;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use la_arena::Arena;
use la_arena::Idx;
use la_arena::RawIdx;

use self::find::FindForm;
pub use self::to_def::CallDef;
pub use self::to_def::DefinitionOrReference;
pub use self::to_def::FaDef;
pub use self::to_def::MacroCallDef;
use self::to_def::ToDef;
use crate::body::scope::ScopeId;
use crate::body::FunctionClauseBody;
use crate::db::DefDatabase;
use crate::def_map::FunctionDefId;
use crate::edoc::EdocHeader;
use crate::expr::AnyExpr;
use crate::expr::AstClauseId;
use crate::expr::ClauseId;
use crate::fold::AnyCallBack;
use crate::fold::AnyCallBackCtx;
use crate::fold::Fold;
use crate::fold::FoldCtx;
use crate::fold::Strategy;
pub use crate::intern::InternDatabase;
pub use crate::intern::InternDatabaseStorage;
use crate::resolver::Resolution;
use crate::resolver::Resolver;
use crate::AnyExprId;
use crate::Body;
use crate::BodySourceMap;
use crate::CRClause;
use crate::CallbackDef;
use crate::DefMap;
use crate::Expr;
use crate::ExprId;
use crate::File;
use crate::FormIdx;
use crate::FormList;
use crate::FunctionBody;
use crate::FunctionClauseId;
use crate::FunctionDef;
use crate::InFile;
use crate::InFileAstPtr;
use crate::Literal;
use crate::MacroName;
use crate::Module;
use crate::Name;
use crate::NameArity;
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
pub mod to_def;

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
    pub db: &'db dyn DefDatabase,
}

impl<'db> fmt::Debug for Semantic<'db> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Information-free print, to allow `fmt::Debug` on structs
        // containing this.
        write!(f, "Semantic{{}}")
    }
}

impl<'db> Semantic<'db> {
    pub fn new<Db: DefDatabase>(db: &'db Db) -> Self {
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

    pub fn function_def_id(&self, function_id: &InFile<FunctionClauseId>) -> Option<FunctionDefId> {
        let def_map = self.def_map(function_id.file_id);
        def_map.function_def_id(&function_id.value).cloned()
    }

    pub fn to_expr(&self, expr: InFile<&ast::Expr>) -> Option<InFunctionClauseBody<ExprId>> {
        let function_id =
            self.find_enclosing_function_clause_id(expr.file_id, expr.value.syntax())?;
        let (body, body_map) = self
            .db
            .function_clause_body_with_source(expr.with_value(function_id));
        let expr_id = &body_map.expr_id(expr)?;
        Some(InFunctionClauseBody {
            sema: self,
            body,
            function_clause_id: expr.with_value(function_id),
            body_map: Some(body_map).into(),
            value: *expr_id,
        })
    }

    pub fn to_pat(&self, expr: InFile<&ast::Expr>) -> Option<InFunctionClauseBody<PatId>> {
        let function_id =
            self.find_enclosing_function_clause_id(expr.file_id, expr.value.syntax())?;
        let (body, body_map) = self
            .db
            .function_clause_body_with_source(expr.with_value(function_id));
        let pat_id = &body_map.pat_id(expr)?;
        Some(InFunctionClauseBody {
            sema: self,
            body,
            function_clause_id: expr.with_value(function_id),
            body_map: Some(body_map).into(),
            value: *pat_id,
        })
    }

    pub fn to_function_body(&self, function_id: InFile<FunctionDefId>) -> InFunctionBody<()> {
        let body = self.db.function_body(function_id);
        InFunctionBody::new(self, body, function_id, ())
    }

    pub fn to_function_clause_body(
        &self,
        function_clause_id: InFile<FunctionClauseId>,
    ) -> InFunctionClauseBody<()> {
        let (body, body_map) = self.db.function_clause_body_with_source(function_clause_id);
        InFunctionClauseBody {
            sema: self,
            body,
            function_clause_id,
            body_map: Some(body_map).into(),
            value: (),
        }
    }

    pub fn to_clause_body(
        &self,
        syntax: InFile<&SyntaxNode>,
    ) -> Option<(ClauseId, Arc<FunctionClauseBody>)> {
        let function_id = self.find_enclosing_function(syntax.file_id, syntax.value)?;
        let function_body = self.db.function_body(syntax.with_value(function_id));
        let ast_clause_id = self.find_enclosing_function_clause(syntax.value)?;
        let clause_id = function_body.valid_clause_id(ast_clause_id)?;
        let body = &function_body[clause_id];
        Some((clause_id, body.clone()))
    }

    pub fn to_clause_body_with_source(
        &self,
        syntax: InFile<&SyntaxNode>,
    ) -> Option<(ClauseId, Arc<FunctionClauseBody>, Arc<BodySourceMap>)> {
        let function_id = self.find_enclosing_function(syntax.file_id, syntax.value)?;
        let (function_body, body_maps) = self
            .db
            .function_body_with_source(syntax.with_value(function_id));
        let ast_clause_id = self.find_enclosing_function_clause(syntax.value)?;
        let clause_id = function_body.valid_clause_id(ast_clause_id)?;
        let map_idx: u32 = clause_id.into_raw().into();
        let body_map = body_maps.get(map_idx as usize)?;
        let body = &function_body[clause_id];
        Some((clause_id, body.clone(), body_map.clone()))
    }

    pub fn resolve_module_names(&self, from_file: FileId) -> Option<ModuleIter> {
        // Context for T171541590
        let _ = stdx::panic_context::enter(format!("\nresolve_module_names: {:?}", from_file));
        let source_root_id = self.db.file_source_root(from_file);
        let project_id = self.db.app_data(source_root_id)?.project_id;
        let module_index = self.db.module_index(project_id);
        Some(ModuleIter(module_index))
    }

    pub fn resolve_behaviour(
        &self,
        file_id: FileId,
        name: &Name,
    ) -> Option<(Module, FxHashMap<NameArity, CallbackDef>)> {
        let behaviour = self.resolve_module_name(file_id, name.as_str())?;
        let behaviour_def_map = self.db.def_map(behaviour.file.file_id);
        Some((behaviour, behaviour_def_map.get_callbacks().clone()))
    }

    pub fn resolve_implemented_callbacks(&self, file_id: FileId) -> FxHashSet<NameArity> {
        let mut res = FxHashSet::default();
        let def_map = self.def_map(file_id);
        def_map.get_behaviours().iter().for_each(|name| {
            if let Some((_behaviour, callbacks)) = self.resolve_behaviour(file_id, name) {
                callbacks.iter().for_each(|(na, _def)| {
                    res.insert(na.clone());
                })
            };
        });
        res
    }

    pub fn module_name(&self, file_id: FileId) -> Option<ModuleName> {
        module_name(self.db.upcast(), file_id)
    }

    pub fn resolve_module_name(&self, file_id: FileId, name: &str) -> Option<Module> {
        // Context for T171541590
        let _ = stdx::panic_context::enter(format!("\nresolve_module_names {:?}", file_id));
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
        let function_id =
            self.find_enclosing_function_clause_id(var_in.file_id, var_in.value.syntax())?;
        let resolver = self.ast_clause_resolver(var_in.with_value(function_id))?;
        let expr = ast::Expr::ExprMax(ast::ExprMax::Var(var_in.value.clone()));
        if let Some(expr_id) = resolver.expr_id_ast(var_in.with_value(&expr)) {
            let var = resolver[expr_id].as_var()?;
            resolver.value.resolve_expr_id(&var, expr_id).cloned()
        } else {
            let pat_id = resolver.pat_id_ast(var_in.with_value(&expr))?;
            let var = resolver[pat_id].as_var()?;
            resolver.value.resolve_pat_id(&var, pat_id).cloned()
        }
    }

    pub fn expand(&self, call: InFile<&ast::MacroCallExpr>) -> Option<(MacroName, String)> {
        let (body, body_source) = self.find_body_and_map(call.file_id, call.value.syntax())?;
        let name = body_source.resolved_macro(call)?.name(self.db);
        let expr = ast::Expr::cast(call.value.syntax().clone())?;
        let any_expr_id = body_source.any_id(call.with_value(&expr))?;
        Some((name, body.print_any_expr(self.db.upcast(), any_expr_id)))
    }

    pub fn scope_for(&self, var_in: InFile<&ast::Var>) -> Option<(Resolver, ScopeId)> {
        let function_clause_id =
            self.find_enclosing_function_clause_id(var_in.file_id, var_in.value.syntax())?;
        let resolver = self.ast_clause_resolver(var_in.with_value(function_clause_id))?;
        let expr = ast::Expr::ExprMax(ast::ExprMax::Var(var_in.value.clone()));
        if let Some(expr_id) = resolver.expr_id_ast(var_in.with_value(&expr)) {
            let scope = resolver.value.scopes.scope_for_expr(expr_id)?;
            Some((resolver.value, scope))
        } else {
            let pat_id = resolver.pat_id_ast(var_in.with_value(&expr))?;
            let scope = resolver.value.scopes.scope_for_pat(pat_id)?;
            Some((resolver.value, scope))
        }
    }

    pub fn find_body_and_map(
        &self,
        file_id: FileId,
        syntax: &SyntaxNode,
    ) -> Option<(Arc<Body>, Arc<BodySourceMap>)> {
        let form = syntax.ancestors().find_map(ast::Form::cast)?;
        let form_list = self.db.file_form_list(file_id);
        let form = form_list.find_form(&form)?;
        self.get_body_and_map(file_id, form)
    }

    pub fn get_body_and_map(
        &self,
        file_id: FileId,
        form: FormIdx,
    ) -> Option<(Arc<Body>, Arc<BodySourceMap>)> {
        match form {
            FormIdx::FunctionClause(fun) => {
                let (body, map) = self
                    .db
                    .function_clause_body_with_source(InFile::new(file_id, fun));
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
            FormIdx::PPDirective(pp) => {
                let form_list = self.db.file_form_list(file_id);
                match form_list[pp] {
                    PPDirective::Define(define) => self
                        .db
                        .define_body_with_source(InFile::new(file_id, define))
                        .map(|(body, map)| (body.body.clone(), map)),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn find_form<T: FindForm>(&self, ast: InFile<&T>) -> Option<T::Form> {
        FindForm::find(self, ast)
    }

    /// Note: our grammar now has one function per clause. So this
    /// returns the `FunctionDefId` of the combined functions.
    pub fn find_enclosing_function(
        &self,
        file_id: FileId,
        syntax: &SyntaxNode,
    ) -> Option<FunctionDefId> {
        let function_clause_id = self.find_enclosing_function_clause_id(file_id, syntax)?;
        self.function_def_id(&InFile::new(file_id, function_clause_id))
    }

    pub fn find_enclosing_function_body(
        &self,
        file_id: FileId,
        syntax: &SyntaxNode,
    ) -> Option<Arc<FunctionBody>> {
        let function_id = self.find_enclosing_function(file_id, syntax)?;
        Some(self.db.function_body(InFile::new(file_id, function_id)))
    }

    pub fn find_enclosing_function_clause_id(
        &self,
        file_id: FileId,
        syntax: &SyntaxNode,
    ) -> Option<FunctionClauseId> {
        let form = syntax.ancestors().find_map(ast::Form::cast)?;
        let form_list = self.db.file_form_list(file_id);
        let form = form_list.find_form(&form)?;
        match form {
            FormIdx::FunctionClause(fun) => Some(fun),
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

    // Try to keep this private, it should go away one day
    fn range_for_anyexpr(&self, body: &Body, expr_id: &AnyExprId) -> Option<TextRange> {
        let body_map = body.get_body_map(self)?;
        let ast = body_map.any(*expr_id)?;
        Some(ast.range())
    }

    /// We expose a high-level function, which internally does some
    /// horrible things to get the expression range and then queries for that.
    /// When we eventually improve this, we will not have to rewrite code using this API.
    pub fn expr_type(&self, body: &Body, expr_id: &ExprId) -> Option<eqwalizer::types::Type> {
        let range = self.range_for_anyexpr(body, &AnyExprId::Expr(*expr_id))?;
        let type_info = self.db.eqwalizer_type_at_position(FileRange {
            file_id: body.origin.file_id(),
            range,
        })?;
        Some(type_info.0.clone())
    }
    /// We expose a high-level function, which internally does some
    /// horrible things to get the expression range and then queries for that.
    /// When we eventually improve this, we will not have to rewrite code using this API.
    pub fn pat_type(&self, body: &Body, pat_id: &PatId) -> Option<eqwalizer::types::Type> {
        let range = self.range_for_anyexpr(body, &AnyExprId::Pat(*pat_id))?;
        let type_info = self.db.eqwalizer_type_at_position(FileRange {
            file_id: body.origin.file_id(),
            range,
        })?;
        Some(type_info.0.clone())
    }

    /// Return the free and bound variables in a given ast expression.
    pub fn free_vars_ast(&self, file_id: FileId, expr: &ast::Expr) -> Option<ScopeAnalysis> {
        let function_clause_id = self.find_enclosing_function_clause_id(file_id, expr.syntax())?;
        let infile_function_id = InFile::new(file_id, function_clause_id);

        let (body, source_map) = self.db.function_clause_body_with_source(infile_function_id);
        let expr_id_in = source_map.expr_id(InFile {
            file_id,
            value: expr,
        })?;
        self.free_vars(&InFunctionClauseBody {
            sema: self,
            body,
            function_clause_id: infile_function_id,
            body_map: Some(source_map).into(),
            value: expr_id_in,
        })
    }

    pub fn function_def(&self, function_id: &InFile<FunctionDefId>) -> Option<FunctionDef> {
        let def_map = self.def_map(function_id.file_id);
        def_map.get_by_function_id(function_id).cloned()
    }

    pub fn function_clause_id(
        &self,
        function_id: &InFile<FunctionDefId>,
        clause_id: ClauseId,
    ) -> Option<FunctionClauseId> {
        let function = self.function_def(function_id)?;
        let n: u32 = clause_id.into_raw().into();
        function.function_clause_ids.get(n as usize).cloned()
    }

    /// Return the free and bound variables in a given expression.
    pub fn free_vars(&self, expr: &InFunctionClauseBody<ExprId>) -> Option<ScopeAnalysis> {
        let function_clause = expr.function_clause_id;
        let clause_scopes = self.db.function_clause_scopes(function_clause);
        let expr_id_in = expr.value;
        let resolver = Resolver::new(clause_scopes);

        let inside_pats = FoldCtx::fold_expr(
            Strategy::InvisibleMacros,
            &expr.body.body,
            expr_id_in,
            FxHashSet::default(),
            &mut |mut acc, ctx| {
                match ctx.item_id {
                    AnyExprId::Pat(pat_id) => {
                        acc.insert(pat_id);
                    }
                    _ => {}
                };
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
            Strategy::InvisibleMacros,
            &expr.body.body,
            expr_id_in,
            ScopeAnalysis::new(),
            &mut |defs, ctx| match ctx.item {
                AnyExpr::Expr(Expr::Var(var_id)) => update_vars(
                    defs,
                    var_id,
                    resolver.resolve_any_expr_id(&var_id, ctx.item_id),
                ),
                AnyExpr::Pat(Pat::Var(var_id)) => update_vars(
                    defs,
                    var_id,
                    resolver.resolve_any_expr_id(&var_id, ctx.item_id),
                ),
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
    ) -> Option<InFunctionClauseBody<Resolver>> {
        let function_clause_id = self.find_enclosing_function_clause_id(file_id, syntax)?;
        self.ast_clause_resolver(InFile::new(file_id, function_clause_id))
    }

    pub fn ast_clause_resolver(
        &self,
        function_clause_id: InFile<FunctionClauseId>,
    ) -> Option<InFunctionClauseBody<Resolver>> {
        let body = self.db.function_clause_body(function_clause_id);
        let scopes = self.db.function_clause_scopes(function_clause_id);
        let resolver = Resolver::new(scopes);
        Some(InFunctionClauseBody {
            sema: self,
            body,
            function_clause_id,
            body_map: None.into(), // We may not need it, do not get it now
            value: resolver,
        })
    }

    pub fn clause_resolver(
        &self,
        function_clause_id: InFile<FunctionClauseId>,
    ) -> Option<InFunctionClauseBody<Resolver>> {
        let body = self.db.function_clause_body(function_clause_id);
        let scopes = self.db.function_clause_scopes(function_clause_id);
        let resolver = Resolver::new(scopes);
        Some(InFunctionClauseBody {
            sema: self,
            body,
            function_clause_id,
            body_map: None.into(), // We may not need it, do not get it now
            value: resolver,
        })
    }

    pub fn find_vars_in_clause_ast(
        &self,
        expr: &InFile<&ast::Expr>,
    ) -> Option<FxHashSet<crate::Var>> {
        let in_function = self.to_expr(*expr)?;
        ScopeAnalysis::clause_vars_in_scope(self, &in_function.with_value(()))
    }

    /// Find all other variables within the function clause that resolve
    /// to the one given.
    pub fn find_local_usages_ast(&self, var: InFile<&ast::Var>) -> Option<Vec<ast::Var>> {
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

    /// Find all other variables within the function clause that resolve
    /// to the one given.
    pub fn find_local_usages(
        &self,
        // Ideally we would use a type that encodes: Either an ExprId
        // for a Expr::Var, or a PatId for a Pat::Var (and maybe
        // TypeExpr::Var in future).
        in_clause: &InFunctionClauseBody<AnyExprId>,
    ) -> Option<Vec<(AnyExprId, crate::Var)>> {
        let resolver = in_clause.resolver();
        let var_resolved: Vec<_> = match in_clause.value {
            AnyExprId::Expr(expr_id) => {
                let var = in_clause[expr_id].as_var()?;
                resolver.resolve_expr_id(&var, expr_id).cloned()?
            }
            AnyExprId::Pat(pat_id) => {
                let var = in_clause[pat_id].as_var()?;
                resolver.resolve_pat_id(&var, pat_id).cloned()?
            }
            _ => vec![],
        };

        let mut resolved_set = FxHashSet::from_iter(var_resolved);
        // We first extend the resolved_set to the widest one that
        // includes the current variable resolution.  This ensures
        // that if we are looking at a variable in one leg of a case
        // clause, and it has equivalents in another leg, then these
        // are also found.
        in_clause
            .body_exprs()
            .for_each(|(expr_id, expr)| match expr {
                Expr::Var(v) => {
                    if let Some(ds) = resolver.resolve_expr_id(&v, expr_id).cloned() {
                        let ds_set = FxHashSet::from_iter(ds);
                        if resolved_set.intersection(&ds_set).next().is_some() {
                            resolved_set.extend(ds_set);
                        }
                    }
                }
                _ => {}
            });
        in_clause.body_pats().for_each(|(pat_id, pat)| match pat {
            Pat::Var(v) => {
                if let Some(ds) = resolver.resolve_pat_id(&v, pat_id).cloned() {
                    let ds_set = FxHashSet::from_iter(ds);
                    if resolved_set.intersection(&ds_set).next().is_some() {
                        resolved_set.extend(ds_set);
                    }
                }
            }
            _ => {}
        });

        // Then we actually check for any variables that resolve to it.
        // If we resolve a candidate Var, Check that it resolves to
        // the one we are looking for
        // We may be in an arm of a case, receive, try, and so we will
        // only find one definition. So check for an intersection with
        // the whole.
        let mut vars: Vec<_> = in_clause
            .body_exprs()
            .filter_map(|(expr_id, expr)| match expr {
                Expr::Var(v) => {
                    if let Some(ds) = resolver.resolve_expr_id(&v, expr_id).cloned() {
                        {
                            if resolved_set
                                .intersection(&FxHashSet::from_iter(ds))
                                .any(|_| true)
                            {
                                Some((expr_id.into(), *v))
                            } else {
                                None
                            }
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect();
        vars.extend(in_clause.body_pats().filter_map(|(pat_id, pat)| match pat {
            Pat::Var(v) => {
                if let Some(ds) = resolver.resolve_pat_id(&v, pat_id).cloned() {
                    {
                        if resolved_set
                            .intersection(&FxHashSet::from_iter(ds))
                            .any(|_| true)
                        {
                            Some((AnyExprId::Pat(pat_id), *v))
                        } else {
                            None
                        }
                    }
                } else {
                    None
                }
            }
            _ => None,
        }));

        if vars.is_empty() { None } else { Some(vars) }
    }

    // -----------------------------------------------------------------
    // Folds

    pub fn fold<'a, F: Fold, T>(
        &self,
        strategy: Strategy,
        id: F::Id,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        F::fold(self, strategy, id, initial, callback)
    }

    pub fn fold_function<'a, T>(
        &self,
        strategy: Strategy,
        function_id: InFile<FunctionDefId>,
        initial: T,
        callback: FunctionAnyCallBack<'a, T>,
    ) -> T {
        let function_body = self.db.function_body(function_id);
        fold_function_body(strategy, &function_body, initial, callback)
    }

    pub fn fold_clause<'a, T>(
        &'a self,
        strategy: Strategy,
        function_clause_id: InFile<FunctionClauseId>,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        let function_clause_body = self.db.function_clause_body(function_clause_id);
        function_clause_body
            .clause
            .exprs
            .iter()
            .fold(initial, |acc_inner, expr_id| {
                FoldCtx::fold_expr(
                    strategy,
                    &function_clause_body.body,
                    *expr_id,
                    acc_inner,
                    callback,
                )
            })
    }

    // Folds end
    // -----------------------------------------------------------------

    pub fn bound_vars_in_pattern_diagnostic(
        &self,
        file_id: FileId,
    ) -> FxHashSet<(InFile<FunctionClauseId>, PatId, ast::Var)> {
        let def_map = self.def_map(file_id);
        let mut res = FxHashSet::default();
        for (function_id, def) in def_map.get_function_clauses() {
            if def.file.file_id == file_id {
                let function_id = InFile::new(file_id, *function_id);
                let body = self.db.function_clause_body(function_id);

                fold_function_clause_body(Strategy::InvisibleMacros, &body, (), &mut |acc, ctx| {
                    if let Some(mut resolver) = self.clause_resolver(function_id) {
                        let mut bound_vars =
                            BoundVarsInPat::new(self, &mut resolver, file_id, &mut res);
                        match ctx.item {
                            AnyExpr::Expr(Expr::Match { lhs, rhs: _ }) => {
                                bound_vars.report_any_bound_vars(&lhs)
                            }
                            AnyExpr::Expr(Expr::Case { expr: _, clauses }) => {
                                bound_vars.cr_clauses(&clauses);
                            }
                            AnyExpr::Expr(Expr::Try {
                                exprs: _,
                                of_clauses,
                                catch_clauses,
                                after: _,
                            }) => {
                                bound_vars.cr_clauses(&of_clauses);
                                catch_clauses.iter().for_each(|clause| {
                                    bound_vars.report_any_bound_vars(&clause.reason);
                                })
                            }
                            _ => {}
                        }
                    };
                    acc
                });
            }
        }
        res
    }

    fn bound_vars_in_pat(
        &self,
        pat_id: &PatId,
        resolver: &mut InFunctionClauseBody<Resolver>,
        file_id: FileId,
    ) -> FxHashSet<(InFile<FunctionClauseId>, PatId, ast::Var)> {
        let parse = self.parse(file_id);
        let body_map = &resolver.get_body_map();
        FoldCtx::fold_pat(
            Strategy::InvisibleMacros,
            &resolver.body.body,
            *pat_id,
            FxHashSet::default(),
            &mut |mut acc, ctx| {
                match ctx.item_id {
                    AnyExprId::Pat(pat_id) => {
                        if let Pat::Var(var) = &resolver[pat_id] {
                            if let Some(pat_ids) = resolver.value.resolve_pat_id(var, pat_id) {
                                pat_ids.iter().for_each(|def_pat_id| {
                                    if &pat_id != def_pat_id {
                                        if let Some(pat_ptr) = body_map.pat(pat_id) {
                                            if let Some(ast::Expr::ExprMax(ast::ExprMax::Var(
                                                var,
                                            ))) = pat_ptr.to_node(&parse)
                                            {
                                                if var.syntax().text() != "_" {
                                                    acc.insert((
                                                        resolver.function_clause_id,
                                                        pat_id,
                                                        var,
                                                    ));
                                                }
                                            }
                                        };
                                    }
                                });
                            }
                        };
                    }
                    _ => {}
                }
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

pub type FunctionAnyCallBack<'a, T> = &'a mut dyn FnMut(T, ClauseId, AnyCallBackCtx) -> T;

fn fold_function_body<'a, T>(
    strategy: Strategy,
    function_body: &FunctionBody,
    initial: T,
    callback: FunctionAnyCallBack<'a, T>,
) -> T {
    function_body
        .clauses
        .iter()
        .fold(initial, |acc, (clause_id, clause)| {
            fold_function_clause_body(strategy, clause, acc, &mut |acc, ctx| {
                callback(acc, clause_id, ctx)
            })
        })
}

fn fold_function_clause_body<'a, T>(
    strategy: Strategy,
    function_clause_body: &FunctionClauseBody,
    initial: T,
    callback: AnyCallBack<'a, T>,
) -> T {
    match &function_clause_body.from_macro {
        Some(from_macro) if strategy == Strategy::SurfaceOnly => FoldCtx::fold_exprs(
            strategy,
            &function_clause_body.body,
            &from_macro.args,
            initial,
            callback,
        ),
        _ => {
            let initial =
                function_clause_body
                    .clause
                    .pats
                    .iter()
                    .fold(initial, |acc_inner, pat_id| {
                        FoldCtx::fold_pat(
                            strategy,
                            &function_clause_body.body,
                            *pat_id,
                            acc_inner,
                            callback,
                        )
                    });

            let initial = function_clause_body.clause.guards.iter().flatten().fold(
                initial,
                |acc_inner, expr_id| {
                    FoldCtx::fold_expr(
                        strategy,
                        &function_clause_body.body,
                        *expr_id,
                        acc_inner,
                        callback,
                    )
                },
            );

            function_clause_body
                .clause
                .exprs
                .iter()
                .fold(initial, |acc_inner, expr_id| {
                    FoldCtx::fold_expr(
                        strategy,
                        &function_clause_body.body,
                        *expr_id,
                        acc_inner,
                        callback,
                    )
                })
        }
    }
}

// ---------------------------------------------------------------------

struct BoundVarsInPat<'a> {
    sema: &'a Semantic<'a>,
    resolver: &'a mut InFunctionClauseBody<'a, Resolver>,
    file_id: FileId,
    res: &'a mut FxHashSet<(InFile<FunctionClauseId>, PatId, ast::Var)>,
}

impl<'a> BoundVarsInPat<'a> {
    fn new(
        sema: &'a Semantic<'a>,
        resolver: &'a mut InFunctionClauseBody<'a, Resolver>,
        file_id: FileId,
        res: &'a mut FxHashSet<(InFile<FunctionClauseId>, PatId, ast::Var)>,
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
        clause: &InFunctionClauseBody<()>,
    ) -> Option<FxHashSet<Var>> {
        let acc = FxHashSet::default();
        let x = clause.body.clause.exprs.iter().fold(acc, |mut acc, expr| {
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
    pub fn walk_expr(&mut self, sema: &Semantic, expr: &InFunctionClauseBody<ExprId>) {
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
pub struct InFunctionBody<'a, T> {
    body: Arc<FunctionBody>,
    function_id: InFile<FunctionDefId>,
    clause_bodies: Arena<InFunctionClauseBody<'a, T>>,
    pub value: T,
}

impl<'a, T: Clone> InFunctionBody<'a, T> {
    pub fn new(
        sema: &'a Semantic<'a>,
        body: Arc<FunctionBody>,
        function_id: InFile<FunctionDefId>,
        value: T,
    ) -> InFunctionBody<T> {
        let clause_bodies = body
            .clauses
            .iter()
            .zip(body.clause_ids.iter())
            .map(|((_, clause), clause_function_id)| {
                InFunctionClauseBody::new(
                    sema,
                    clause.clone(),
                    function_id.with_value(*clause_function_id),
                    None,
                    value.clone(),
                )
            })
            .collect();
        InFunctionBody {
            body,
            function_id,
            clause_bodies,
            value,
        }
    }

    pub fn as_ref(&self) -> InFunctionBody<&T> {
        self.with_value(&self.value)
    }

    pub fn with_value<U>(&self, value: U) -> InFunctionBody<U>
    where
        U: Clone,
    {
        InFunctionBody {
            body: self.body.clone(),
            function_id: self.function_id,
            clause_bodies: self
                .clause_bodies
                .iter()
                .map(|(_, c)| c.with_value(value.clone()))
                .collect(),
            value,
        }
    }

    pub fn file_id(&self) -> FileId {
        self.function_id.file_id
    }

    pub fn form_id(&self) -> FormIdx {
        FormIdx::FunctionClause(self.body.clause_ids[0])
    }

    pub fn function_id(&self) -> FunctionDefId {
        self.function_id.value
    }

    pub fn get_body_map(&self, clause_id: ClauseId) -> Arc<BodySourceMap> {
        self.in_clause(clause_id).get_body_map()
    }

    pub fn valid_clause_id(&self, ast_clause_id: AstClauseId) -> Option<ClauseId> {
        self.body.valid_clause_id(ast_clause_id)
    }

    pub fn clauses(&self) -> impl Iterator<Item = (ClauseId, &Arc<FunctionClauseBody>)> {
        self.body.clauses.iter()
    }

    pub fn clause(&'a self, clause_id: ClauseId) -> &'a Arc<FunctionClauseBody> {
        &self.body.clauses[clause_id]
    }

    pub fn in_clause(&'a self, clause_id: ClauseId) -> &'a InFunctionClauseBody<T> {
        let idx = Idx::from_raw(clause_id.into_raw());
        &self.clause_bodies[idx]
    }

    pub fn body(&self, clause_id: ClauseId) -> Arc<Body> {
        self.body.clauses[clause_id].body.clone()
    }

    pub fn fold_function<R>(
        &self,
        strategy: Strategy,
        initial: R,
        callback: FunctionAnyCallBack<'a, R>,
    ) -> R {
        fold_function_body(strategy, &self.body, initial, callback)
    }

    pub fn range_for_expr(&self, clause_id: ClauseId, expr_id: ExprId) -> Option<TextRange> {
        self.in_clause(clause_id).range_for_expr(expr_id)
    }

    pub fn range_for_any(&self, clause_id: ClauseId, id: AnyExprId) -> Option<TextRange> {
        self.in_clause(clause_id).range_for_any(id)
    }

    pub fn range_for_pat(&mut self, clause_id: ClauseId, pat_id: PatId) -> Option<TextRange> {
        self.in_clause(clause_id).range_for_pat(pat_id)
    }
}

impl<'a, T> Index<ClauseId> for InFunctionBody<'a, T> {
    type Output = FunctionClauseBody;

    fn index(&self, index: ClauseId) -> &Self::Output {
        &self.body[index]
    }
}

// ---------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct InFunctionClauseBody<'a, T> {
    pub sema: &'a Semantic<'a>,
    pub body: Arc<FunctionClauseBody>,
    pub function_clause_id: InFile<FunctionClauseId>,
    // cache body_map if we already have it when wrapping the value.
    // This field should go away once we fully use the hir API only
    body_map: RefCell<Option<Arc<BodySourceMap>>>,
    pub value: T,
}

impl<'a, T> InFunctionClauseBody<'a, T> {
    pub fn new(
        sema: &'a Semantic<'a>,
        body: Arc<FunctionClauseBody>,
        function_clause_id: InFile<FunctionClauseId>,
        body_map: Option<Arc<BodySourceMap>>,
        value: T,
    ) -> InFunctionClauseBody<T> {
        InFunctionClauseBody {
            sema,
            body,
            function_clause_id,
            body_map: body_map.into(),
            value,
        }
    }

    pub fn as_ref(&self) -> InFunctionClauseBody<&T> {
        self.with_value(&self.value)
    }

    pub fn with_value<U>(&self, value: U) -> InFunctionClauseBody<U> {
        InFunctionClauseBody {
            sema: self.sema,
            body: self.body.clone(),
            function_clause_id: self.function_clause_id,
            body_map: self.body_map.clone(),
            value,
        }
    }

    pub fn file_id(&self) -> FileId {
        self.function_clause_id.file_id
    }

    pub fn ast_fun_decl(&self) -> ast::FunDecl {
        let form_list = self.sema.db.file_form_list(self.function_clause_id.file_id);
        let function = &form_list[self.function_clause_id.value];
        let function_ast = function
            .form_id
            .get(&self.function_clause_id.file_syntax(self.sema.db.upcast()));
        function_ast
    }

    pub fn body_exprs(&self) -> impl Iterator<Item = (ExprId, &Expr)> {
        self.body.body.exprs.iter()
    }

    pub fn body_pats(&self) -> impl Iterator<Item = (PatId, &Pat)> {
        self.body.body.pats.iter()
    }

    pub fn get_body_map(&self) -> Arc<BodySourceMap> {
        if let Some(body_map) = &self.body_map.borrow().as_ref() {
            //return explicitly here because borrow is still held in else statement
            //https://stackoverflow.com/questions/30243606/why-is-a-borrow-still-held-in-the-else-block-of-an-if-let
            return Arc::clone(body_map);
        }
        let (_body, body_map) = self
            .sema
            .db
            .function_clause_body_with_source(self.function_clause_id);
        *self.body_map.borrow_mut() = Some(body_map.clone());
        body_map
    }

    pub fn expr_id(&self, expr: &Expr) -> Option<ExprId> {
        self.body.body.expr_id(expr)
    }

    pub fn expr_id_ast(&self, expr: InFile<&ast::Expr>) -> Option<ExprId> {
        self.get_body_map().expr_id(expr)
    }

    pub fn pat_id_ast(&self, expr: InFile<&ast::Expr>) -> Option<PatId> {
        self.get_body_map().pat_id(expr)
    }

    pub fn body(&self) -> Arc<Body> {
        self.body.body.clone()
    }

    pub fn fold_expr<R>(
        &self,
        strategy: Strategy,
        expr_id: ExprId,
        initial: R,
        callback: AnyCallBack<'a, R>,
    ) -> R {
        FoldCtx::fold_expr(strategy, &self.body.body, expr_id, initial, callback)
    }

    pub fn fold_pat<R>(
        &self,
        strategy: Strategy,
        pat_id: PatId,
        initial: R,
        callback: AnyCallBack<'a, R>,
    ) -> R {
        FoldCtx::fold_pat(strategy, &self.body.body, pat_id, initial, callback)
    }

    pub fn fold_clause<R>(
        &self,
        strategy: Strategy,
        initial: R,
        callback: AnyCallBack<'a, R>,
    ) -> R {
        fold_function_clause_body(strategy, &self.body, initial, callback)
    }

    pub fn range(&self) -> TextRange {
        self.ast_fun_decl().syntax().text_range()
    }

    pub fn range_for_expr(&self, expr_id: ExprId) -> Option<TextRange> {
        let body_map = self.get_body_map();
        let ast = body_map.expr(expr_id)?;
        Some(ast.range())
    }

    pub fn range_for_any(&self, id: AnyExprId) -> Option<TextRange> {
        let body_map = self.get_body_map();
        let ast = body_map.any(id)?;
        Some(ast.range())
    }

    pub fn range_for_pat(&self, pat_id: PatId) -> Option<TextRange> {
        let body_map = self.get_body_map();
        let ast = body_map.pat(pat_id)?;
        Some(ast.range())
    }

    pub fn as_atom_name(&self, expr: &ExprId) -> Option<Name> {
        Some(self.sema.db.lookup_atom(self[*expr].as_atom()?))
    }

    fn resolver(&self) -> Resolver {
        // Should this be a field in InFunctionClauseBody?
        let clause_scopes = self.sema.db.function_clause_scopes(self.function_clause_id);
        Resolver::new(clause_scopes.clone())
    }

    pub fn to_var_def_any(
        &self,
        id: AnyExprId,
    ) -> Option<DefinitionOrReference<VarDef, Vec<VarDef>>> {
        match id {
            AnyExprId::Expr(expr_id) => self.to_var_def_expr(expr_id),
            AnyExprId::Pat(pat_id) => self.to_var_def_pat(pat_id),
            _ => None,
        }
    }

    pub fn to_var_def_pat(
        &self,
        pat_id: PatId,
    ) -> Option<DefinitionOrReference<VarDef, Vec<VarDef>>> {
        match &self[pat_id] {
            Pat::Var(_) => {
                let resolver = self.resolver();
                let (var, original_pat_id, pat_ids) = {
                    let var = self[pat_id].as_var()?;
                    (var, Some(pat_id), resolver.resolve_pat_id(&var, pat_id)?)
                };
                self.to_var_def(pat_ids, var, original_pat_id)
            }
            _ => None,
        }
    }

    pub fn to_var_def_expr(
        &self,
        expr_id: ExprId,
    ) -> Option<DefinitionOrReference<VarDef, Vec<VarDef>>> {
        match &self[expr_id] {
            Expr::Var(_) => {
                let clause_scopes = self.sema.db.function_clause_scopes(self.function_clause_id);
                let resolver = Resolver::new(clause_scopes.clone());
                let (var, original_pat_id, pat_ids) = {
                    let var = self[expr_id].as_var()?;
                    (var, None, resolver.resolve_expr_id(&var, expr_id)?)
                };
                self.to_var_def(pat_ids, var, original_pat_id)
            }
            _ => None,
        }
    }

    pub fn to_var_def(
        &self,
        pat_ids: &Vec<PatId>,
        var: Var,
        original_pat_id: Option<PatId>,
    ) -> Option<DefinitionOrReference<VarDef, Vec<VarDef>>> {
        let mut self_def = false;
        let mut resolved = pat_ids
            .iter()
            .filter_map(|&pat_id| {
                let var_def = self.get_body_map().pat(pat_id)?;
                let def = VarDef {
                    file: File {
                        file_id: var_def.file_id(),
                    },
                    var: var_def.value().cast()?,
                    hir_var: var,
                };
                if Some(pat_id) == original_pat_id {
                    self_def = true;
                };
                Some(def)
            })
            .collect::<Vec<_>>();
        if self_def {
            assert!(resolved.len() == 1);
            // swap_remove dance necessary to take ownership of element without copying
            Some(DefinitionOrReference::Definition(resolved.swap_remove(0)))
        } else if !resolved.is_empty() {
            Some(DefinitionOrReference::Reference(resolved))
        } else {
            None
        }
    }

    pub fn tree_print_any_expr(&self, expr: AnyExprId) -> String {
        self.body().tree_print_any_expr(self.sema.db.upcast(), expr)
    }
}

impl<'a, T> Index<ExprId> for InFunctionClauseBody<'a, T> {
    type Output = Expr;

    fn index(&self, index: ExprId) -> &Self::Output {
        &self.body.body[index]
    }
}

impl<'a, T> Index<PatId> for InFunctionClauseBody<'a, T> {
    type Output = Pat;

    fn index(&self, index: PatId) -> &Self::Output {
        &self.body.body[index]
    }
}

impl<'a, T> Index<TypeExprId> for InFunctionClauseBody<'a, T> {
    type Output = TypeExpr;

    fn index(&self, index: TypeExprId) -> &Self::Output {
        &self.body.body[index]
    }
}

impl<'a, T> Index<TermId> for InFunctionClauseBody<'a, T> {
    type Output = Term;

    fn index(&self, index: TermId) -> &Self::Output {
        &self.body.body[index]
    }
}

// ---------------------------------------------------------------------

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
    use crate::AnyExprId;
    use crate::InFile;
    use crate::InFunctionClauseBody;
    use crate::Semantic;

    #[track_caller]
    fn check_local_usages(fixture_before: &str, expect: Expect) {
        let (db, position, _) = TestDB::with_position(fixture_before);
        let sema = Semantic::new(&db);

        let file_syntax = db.parse(position.file_id).syntax_node();
        let var: ast::Var = find_node_at_offset(&file_syntax, position.offset).unwrap();
        let in_clause = sema
            .to_pat(InFile::new(
                position.file_id,
                &ast::Expr::ExprMax(ast::ExprMax::Var(var)),
            ))
            .unwrap();
        let usages = sema
            .find_local_usages(&in_clause.with_value(AnyExprId::Pat(in_clause.value)))
            .unwrap();
        let usages: Vec<_> = usages
            .iter()
            .map(|(id, v)| (in_clause.range_for_any(*id).unwrap(), v.as_string(&db)))
            .sorted_by_key(|(r, _)| r.start())
            .collect();
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
                    (
                        109..110,
                        "Z",
                    ),
                    (
                        171..172,
                        "Z",
                    ),
                    (
                        201..202,
                        "Z",
                    ),
                    (
                        279..280,
                        "Z",
                    ),
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
                    (
                        29..30,
                        "Y",
                    ),
                    (
                        101..102,
                        "Y",
                    ),
                    (
                        146..147,
                        "Y",
                    ),
                    (
                        240..241,
                        "Y",
                    ),
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

    #[track_caller]
    fn check_in_clause_ast(fixture_before: &str, expect: Expect) {
        let (db, position, _) = TestDB::with_position(fixture_before);
        let sema = Semantic::new(&db);

        let file_syntax = db.parse(position.file_id).syntax_node();
        let token = file_syntax
            .token_at_offset(position.offset)
            .right_biased()
            .unwrap();
        let node = token.parent().unwrap();
        let (_clause_id, clause) = sema
            .to_clause_body(InFile::new(position.file_id, &node))
            .unwrap();
        let function_clause_id = sema
            .find_enclosing_function_clause_id(position.file_id, &node)
            .unwrap();
        let in_clause = InFunctionClauseBody::new(
            &sema,
            clause,
            InFile::new(position.file_id, function_clause_id),
            None,
            (),
        );
        let ast = in_clause.ast_fun_decl();
        expect.assert_debug_eq(&ast);
    }

    #[test]
    fn infunctionclausebody_get_ast() {
        check_in_clause_ast(
            r#"foo(0) -> ok;
               foo(~A) -> this.
                   "#,
            expect![[r#"
                FunDecl {
                    syntax: FUN_DECL@29..44
                      FUNCTION_CLAUSE@29..43
                        ATOM@29..32
                          ATOM@29..32 "foo"
                        EXPR_ARGS@32..35
                          ANON_LPAREN@32..33 "("
                          VAR@33..34
                            VAR@33..34 "A"
                          ANON_RPAREN@34..35 ")"
                        WHITESPACE@35..36 " "
                        CLAUSE_BODY@36..43
                          ANON_DASH_GT@36..38 "->"
                          WHITESPACE@38..39 " "
                          ATOM@39..43
                            ATOM@39..43 "this"
                      ANON_DOT@43..44 "."
                    ,
                }
            "#]],
        );
    }

    #[test]
    fn infunctionclausebody_get_ast_from_macro() {
        check_in_clause_ast(
            r#"
             -define(CLAUSE(Res), foo(_) -> Res).

             foo() -> 1;
             ?CLAUSE(~V).
                   "#,
            expect![[r#"
                FunDecl {
                    syntax: FUN_DECL@50..61
                      MACRO_CALL_EXPR@50..60
                        ANON_QMARK@50..51 "?"
                        VAR@51..57
                          VAR@51..57 "CLAUSE"
                        MACRO_CALL_ARGS@57..60
                          ANON_LPAREN@57..58 "("
                          MACRO_EXPR@58..59
                            VAR@58..59
                              VAR@58..59 "V"
                          ANON_RPAREN@59..60 ")"
                      ANON_DOT@60..61 "."
                    ,
                }
            "#]],
        );
    }
}

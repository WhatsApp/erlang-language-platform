/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Name resolution for expressions.
//!
//! This models what happens in elp_lint.erl
//! An overview prepared by Richard Carlsson can be found at
//! https://docs.google.com/document/d/1_qukz2RD5Bc5U4npfGwfzARJxF5feBLNiLKXsuVNy-g/edit
//!
//! We make the following assumptions in this code
//!
//! - We follow the process erlc does
//!
//! - We are not processing diagnostics.  In some cases this means
//!   that we compute a binding for a variable that will generate a
//!   warning or error from the Erlang compiler.  Examples are unsafe
//!   variables escaping from a case clause, or bindings escaping from
//!   the `catch` part of a `try .. catch`.
//!
//!   We prioritise the ability of a user to navigate using
//!   go-to-definition, even if the code is broken, as it may be a
//!   transient state, and diagnostics from other sources will give
//!   additional feedback.

use std::ops::Index;
use std::ops::IndexMut;
use std::sync::Arc;

use elp_syntax::ast::LogicOp;
use fxhash::FxHashMap;
use la_arena::Arena;
use la_arena::ArenaMap;
use la_arena::Idx;

use super::FoldBody;
use crate::Body;
use crate::CRClause;
use crate::ComprehensionBuilder;
use crate::ComprehensionExpr;
use crate::ExprId;
use crate::FunctionClauseBody;
use crate::FunctionClauseId;
use crate::InFile;
use crate::Name;
use crate::PatId;
use crate::Var;
use crate::db::DefDatabase;
use crate::def_map::FunctionDefId;
use crate::expr::BinaryOp as SyntaxBinaryOp;
use crate::expr::ClauseId;
use crate::expr::MaybeExpr;
use crate::fold::ParenStrategy;
use crate::fold::VisibleMacros;

pub type ScopeId = Idx<ScopeData>;

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionScopes {
    // Invariant: These are the same order as in FunctionBody
    clause_scopes: ArenaMap<ClauseId, ExprScopes>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExprScopes {
    scopes: Arena<ScopeData>,
    scope_by_expr: FxHashMap<ExprId, ScopeId>,
    scope_by_pat: FxHashMap<PatId, ScopeId>,
    // Interned value of the anonymous variable ('_').
    anonymous_var: Var,
}

impl Index<ScopeId> for ExprScopes {
    type Output = ScopeData;

    fn index(&self, index: ScopeId) -> &Self::Output {
        &self.scopes[index]
    }
}

impl IndexMut<ScopeId> for ExprScopes {
    fn index_mut(&mut self, index: ScopeId) -> &mut ScopeData {
        &mut self.scopes[index]
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ScopeData {
    parent: Option<ScopeId>,
    entries: ScopeEntries,
}

type ScopeEntryMap = FxHashMap<Var, Vec<PatId>>;
#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct ScopeEntries {
    data: ScopeEntryMap,
}

impl ScopeEntries {
    fn insert(&mut self, name: Var, pats: Vec<PatId>) {
        self.data
            .entry(name)
            .and_modify(|v| v.extend(pats.clone()))
            .or_insert_with(|| pats.clone());
    }

    pub fn lookup(&self, name: &Var) -> Option<&Vec<PatId>> {
        self.data.get(name)
    }

    pub fn names(&self) -> impl Iterator<Item = Var> + '_ {
        self.data.keys().copied()
    }
}

/// See https://fburl.com/code/otca3wbd for the equivalent data
/// structures in elp_lint.erl.  We track the elp_lint state in a
/// `ScopeEntry`, and usage in `VarTable`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum VarUsage {
    Used,
    UnUsed,
}

/// Equivalent to elp_lint VarTable.
#[derive(Debug, Default, Clone)]
struct VarTable {
    vars: FxHashMap<Var, VarUsage>,
}

impl VarTable {
    // For a variable in a pattern:
    //  - If it is not in the bound set, it is new and is added to the set (as yet unused)
    //  - If it is already in the bound set, it is simply marked as used
    fn new_in_pattern(&mut self, var: Var) {
        self.vars
            .entry(var)
            .and_modify(|e| *e = VarUsage::Used)
            .or_insert(VarUsage::UnUsed);
    }

    fn set_used(&mut self, var: &Var) {
        self.vars.insert(*var, VarUsage::Used);
    }

    fn is_new(&self, var: &Var) -> bool {
        match self.vars.get(var) {
            Some(v) => *v == VarUsage::UnUsed,
            None => true,
        }
    }

    fn merge(&mut self, other: &VarTable) {
        for (k, v) in &other.vars {
            self.vars
                .entry(*k)
                .and_modify(|v2| {
                    if *v2 == VarUsage::Used || *v == VarUsage::Used {
                        *v2 = VarUsage::Used;
                    } else {
                        *v2 = VarUsage::UnUsed;
                    }
                })
                .or_insert(*v);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddBinding {
    Always,
    IfUnused,
}

impl FunctionScopes {
    pub(crate) fn function_scopes_query(
        db: &dyn DefDatabase,
        function_id: InFile<FunctionDefId>,
    ) -> Arc<FunctionScopes> {
        let function_body = db.function_body(function_id);
        let anonymous_var = db.var(Name::ANONYMOUS);
        let clause_scopes = function_body
            .clauses
            .iter()
            .map(|(idx, clause)| (idx, ExprScopes::for_clause(clause, anonymous_var)))
            .collect();
        Arc::new(FunctionScopes { clause_scopes })
    }

    pub(crate) fn function_clause_scopes_query(
        db: &dyn DefDatabase,
        function_clause_id: InFile<FunctionClauseId>,
    ) -> Arc<ExprScopes> {
        let function_clause_body = db.function_clause_body(function_clause_id);
        let anonymous_var = db.var(Name::ANONYMOUS);
        Arc::new(ExprScopes::for_clause(&function_clause_body, anonymous_var))
    }

    #[cfg(test)]
    pub(crate) fn get(&self, clause: ClauseId) -> Option<ExprScopes> {
        self.clause_scopes.get(clause).cloned()
    }
}

impl ExprScopes {
    fn for_clause(body: &FunctionClauseBody, anonymous_var: Var) -> ExprScopes {
        let mut scopes = ExprScopes {
            scopes: Arena::default(),
            scope_by_expr: FxHashMap::default(),
            scope_by_pat: FxHashMap::default(),
            anonymous_var,
        };
        let mut root = scopes.root_scope();
        let mut vt = VarTable::default();
        scopes.add_params_bindings(&body.body, &mut root, &body.clause.pats, &mut vt);
        for exprs in &body.clause.guards {
            for expr_id in exprs {
                compute_expr_scopes(*expr_id, &body.body, &mut scopes, &mut root, &mut vt);
            }
        }
        for expr_id in &body.clause.exprs {
            compute_expr_scopes(*expr_id, &body.body, &mut scopes, &mut root, &mut vt);
        }
        scopes
    }

    pub fn entries(&self, scope: ScopeId) -> &ScopeEntries {
        &self.scopes[scope].entries
    }

    pub fn scope_chain(&self, scope: Option<ScopeId>) -> impl Iterator<Item = ScopeId> + '_ {
        std::iter::successors(scope, move |&scope| self.scopes[scope].parent)
    }

    pub fn scope_for_expr(&self, expr_id: ExprId) -> Option<ScopeId> {
        self.scope_by_expr.get(&expr_id).copied()
    }

    pub fn scope_for_pat(&self, pat_id: PatId) -> Option<ScopeId> {
        self.scope_by_pat.get(&pat_id).copied()
    }

    fn root_scope(&mut self) -> ScopeId {
        self.scopes.alloc(ScopeData {
            parent: None,
            entries: ScopeEntries::default(),
        })
    }

    fn new_scope(&mut self, parent: ScopeId) -> ScopeId {
        self.scopes.alloc(ScopeData {
            parent: Some(parent),
            entries: ScopeEntries::default(),
        })
    }

    fn set_scope_expr(&mut self, node: ExprId, scope: ScopeId) {
        self.scope_by_expr.insert(node, scope);
    }

    fn set_scope_pat(&mut self, node: PatId, scope: ScopeId) {
        self.scope_by_pat.insert(node, scope);
    }

    fn add_bindings(
        &mut self,
        body: &Body,
        scope: &mut ScopeId,
        pat: PatId,
        vt: &mut VarTable,
        add_bindings: AddBinding,
    ) {
        self.set_scope_pat(pat, *scope);
        let pattern = &body[pat];
        match pattern {
            crate::Pat::Missing => {}
            crate::Pat::Literal(_) => {}
            crate::Pat::Var(var) => {
                if var != &self.anonymous_var {
                    vt.new_in_pattern(*var);
                    if vt.is_new(var) {
                        vt.set_used(var);
                        if add_bindings == AddBinding::IfUnused {
                            self.scopes[*scope].entries.insert(*var, vec![pat]);
                        }
                    }
                    if add_bindings != AddBinding::IfUnused {
                        self.scopes[*scope].entries.insert(*var, vec![pat]);
                    }
                }
            }
            crate::Pat::Match { lhs, rhs } => {
                self.add_bindings(body, scope, *lhs, vt, add_bindings);
                self.add_bindings(body, scope, *rhs, vt, add_bindings);
            }
            crate::Pat::Tuple { pats } => {
                for pat in pats {
                    self.add_bindings(body, scope, *pat, vt, add_bindings)
                }
            }
            crate::Pat::List { pats, tail } => {
                for pat in pats {
                    self.add_bindings(body, scope, *pat, vt, add_bindings)
                }
                tail.map(|pat| self.add_bindings(body, scope, pat, vt, add_bindings));
            }
            crate::Pat::Binary { segs } => {
                for seg in segs {
                    if let Some(expr) = seg.size {
                        compute_expr_scopes(expr, body, self, scope, vt);
                    }
                    self.add_bindings(body, scope, seg.elem, vt, add_bindings)
                }
            }
            crate::Pat::UnaryOp { pat, op: _ } => {
                self.add_bindings(body, scope, *pat, vt, add_bindings);
            }
            crate::Pat::BinaryOp { lhs, rhs, op: _ } => {
                self.add_bindings(body, scope, *lhs, vt, add_bindings);
                self.add_bindings(body, scope, *rhs, vt, add_bindings);
            }
            crate::Pat::Record { name: _, fields } => {
                for (_, pat) in fields {
                    self.add_bindings(body, scope, *pat, vt, add_bindings);
                }
            }
            crate::Pat::RecordIndex { name: _, field: _ } => {}
            crate::Pat::Map { fields } => {
                for (expr, pat) in fields {
                    compute_expr_scopes(*expr, body, self, scope, vt);
                    self.add_bindings(body, scope, *pat, vt, add_bindings);
                }
            }
            crate::Pat::MacroCall {
                expansion,
                args,
                macro_def: _,
                macro_name: _,
            } => {
                self.add_bindings(body, scope, *expansion, vt, add_bindings);
                for arg in args {
                    compute_expr_scopes(*arg, body, self, scope, vt);
                }
            }
            crate::Pat::Paren { pat } => {
                self.add_bindings(body, scope, *pat, vt, add_bindings);
            }
            crate::Pat::SsrPlaceholder(_ssr) => {}
        };
    }

    fn add_params_bindings(
        &mut self,
        body: &Body,
        scope: &mut ScopeId,
        params: &[PatId],
        vt: &mut VarTable,
    ) {
        // Function parameters always create new bindings in the current scope,
        // even if they shadow variables from outer scopes. Here, we ensure that variable
        // references inside the fun resolve to the parameter binding in the head of the fun
        // clause, not the binding from the outer scope.
        // src: https://www.erlang.org/doc/system/funs.html#variable-bindings-within-a-fun
        params
            .iter()
            .for_each(|pat| self.add_bindings(body, scope, *pat, vt, AddBinding::Always));
    }
}

fn compute_expr_scopes(
    expr: ExprId,
    body: &Body,
    scopes: &mut ExprScopes,
    scope: &mut ScopeId,
    vt: &mut VarTable,
) {
    scopes.set_scope_expr(expr, *scope);
    match &(FoldBody {
        body,
        macros: VisibleMacros::Yes,
        parens: ParenStrategy::InvisibleParens,
    })[expr]
    {
        crate::Expr::Missing => {}
        crate::Expr::Literal(_) => {}
        crate::Expr::Var(_) => {}
        crate::Expr::Match { lhs, rhs } => {
            compute_expr_scopes(*rhs, body, scopes, scope, vt);
            scopes.add_bindings(body, scope, *lhs, vt, AddBinding::IfUnused);
        }
        crate::Expr::Tuple { exprs } => {
            for expr in exprs {
                compute_expr_scopes(*expr, body, scopes, scope, vt);
            }
        }
        crate::Expr::List { exprs, tail } => {
            for expr in exprs {
                compute_expr_scopes(*expr, body, scopes, scope, vt);
            }
            if let Some(tail) = tail {
                compute_expr_scopes(*tail, body, scopes, scope, vt);
            }
        }
        crate::Expr::Binary { segs } => {
            for seg in segs {
                compute_expr_scopes(seg.elem, body, scopes, scope, vt);
                if let Some(size) = seg.size {
                    compute_expr_scopes(size, body, scopes, scope, vt);
                }
            }
        }
        crate::Expr::UnaryOp { expr, op: _ } => {
            compute_expr_scopes(*expr, body, scopes, scope, vt);
        }
        crate::Expr::BinaryOp { lhs, rhs, op } => {
            match op {
                SyntaxBinaryOp::LogicOp(
                    LogicOp::Or { lazy: true } | LogicOp::And { lazy: true },
                ) => {
                    // orelse/andalso: sequential — RHS sees LHS bindings.
                    // In elp_lint.erl, new RHS bindings are marked unsafe,
                    // but we don't track unsafe here.
                    compute_expr_scopes(*lhs, body, scopes, scope, vt);
                    compute_expr_scopes(*rhs, body, scopes, scope, vt);
                }
                _ => {
                    // All other binary ops: parallel — both sides see only
                    // pre-existing bindings.
                    // Matches erl_lint.erl line 2905: expr_list([L,R], Vt, St).
                    // https://github.com/erlang/otp/blob/OTP-28.3.2/lib/stdlib/src/erl_lint.erl#L2905
                    // Each side is evaluated in its own child scope so
                    // neither sees the other's bindings.
                    let mut lhs_vt = vt.clone();
                    let mut lhs_scope = scopes.new_scope(*scope);
                    compute_expr_scopes(*lhs, body, scopes, &mut lhs_scope, &mut lhs_vt);
                    let mut rhs_vt = vt.clone();
                    let mut rhs_scope = scopes.new_scope(*scope);
                    compute_expr_scopes(*rhs, body, scopes, &mut rhs_scope, &mut rhs_vt);
                    // Collect new bindings from both child scopes.
                    let mut new_entries: FxHashMap<Var, Vec<PatId>> = FxHashMap::default();
                    for child in [lhs_scope, rhs_scope] {
                        for (name, pats) in &scopes[child].entries.data {
                            new_entries
                                .entry(*name)
                                .and_modify(|v| v.extend(pats.clone()))
                                .or_insert_with(|| pats.clone());
                        }
                    }
                    // Only create an export scope if there are new bindings.
                    // Unlike add_exported_scopes (used for case/if/receive),
                    // we carry forward the parent scope's entries so that an
                    // enclosing case clause doesn't lose earlier bindings.
                    if !new_entries.is_empty() {
                        let parent_entries = scopes[*scope].entries.data.clone();
                        *scope = scopes.new_scope(*scope);
                        for (name, pats) in parent_entries {
                            scopes[*scope].entries.insert(name, pats);
                        }
                        for (name, pats) in new_entries {
                            scopes[*scope].entries.insert(name, pats);
                        }
                    }
                    vt.merge(&lhs_vt);
                    vt.merge(&rhs_vt);
                }
            }
        }
        crate::Expr::Record { name: _, fields } => {
            for (_, expr) in fields {
                compute_expr_scopes(*expr, body, scopes, scope, vt);
            }
        }
        crate::Expr::RecordUpdate {
            expr,
            name: _,
            fields,
        } => {
            compute_expr_scopes(*expr, body, scopes, scope, vt);
            for (_, expr) in fields {
                compute_expr_scopes(*expr, body, scopes, scope, vt);
            }
        }
        crate::Expr::RecordIndex { name: _, field: _ } => {}
        crate::Expr::RecordField {
            expr,
            name: _,
            field: _,
        } => {
            compute_expr_scopes(*expr, body, scopes, scope, vt);
        }
        crate::Expr::Map { fields } => {
            for (lhs, rhs) in fields {
                compute_expr_scopes(*lhs, body, scopes, scope, vt);
                compute_expr_scopes(*rhs, body, scopes, scope, vt);
            }
        }
        crate::Expr::MapUpdate { expr, fields } => {
            compute_expr_scopes(*expr, body, scopes, scope, vt);
            for (lhs, _, rhs) in fields {
                compute_expr_scopes(*lhs, body, scopes, scope, vt);
                compute_expr_scopes(*rhs, body, scopes, scope, vt);
            }
        }
        crate::Expr::Catch { expr } => {
            compute_expr_scopes(*expr, body, scopes, scope, vt);
        }
        crate::Expr::MacroCall {
            expansion,
            args,
            macro_def: _,
            macro_name: _,
        } => {
            compute_expr_scopes(*expansion, body, scopes, scope, vt);
            for arg in args {
                compute_expr_scopes(*arg, body, scopes, scope, vt);
            }
        }
        crate::Expr::Call { target, args } => {
            match target {
                crate::CallTarget::Local { name } => {
                    compute_expr_scopes(*name, body, scopes, scope, vt);
                }
                crate::CallTarget::Remote { module, name, .. } => {
                    compute_expr_scopes(*module, body, scopes, scope, vt);
                    compute_expr_scopes(*name, body, scopes, scope, vt);
                }
            }
            for arg in args {
                compute_expr_scopes(*arg, body, scopes, scope, vt);
            }
        }
        crate::Expr::Comprehension { builder, exprs } => {
            let mut sub_vt = vt.clone();
            let scope = &mut scopes.new_scope(*scope);
            for expr in exprs {
                compute_comprehension_expr_scopes(body, scopes, &mut sub_vt, scope, expr);
            }
            match builder {
                ComprehensionBuilder::List(expr) => {
                    compute_expr_scopes(*expr, body, scopes, scope, &mut sub_vt)
                }
                ComprehensionBuilder::Binary(expr) => {
                    compute_expr_scopes(*expr, body, scopes, scope, &mut sub_vt)
                }
                ComprehensionBuilder::Map(key, value) => {
                    compute_expr_scopes(*key, body, scopes, scope, &mut sub_vt);
                    compute_expr_scopes(*value, body, scopes, scope, &mut sub_vt)
                }
            }
        }
        crate::Expr::Block { exprs } => {
            for expr in exprs {
                compute_expr_scopes(*expr, body, scopes, scope, vt);
            }
        }
        crate::Expr::If { clauses } => {
            let if_scopes: Vec<_> = clauses
                .iter()
                .map(|clause| {
                    let mut scope = scopes.new_scope(*scope);
                    let mut sub_vt = vt.clone();
                    for guard_exprs in &clause.guards {
                        for guard in guard_exprs {
                            compute_expr_scopes(*guard, body, scopes, &mut scope, &mut sub_vt);
                        }
                    }
                    for expr in &clause.exprs {
                        compute_expr_scopes(*expr, body, scopes, &mut scope, &mut sub_vt);
                    }
                    (scope, sub_vt)
                })
                .collect();
            vt.merge(&add_exported_scopes(scopes, scope, &if_scopes));
        }
        crate::Expr::Case { expr, clauses } => {
            compute_expr_scopes(*expr, body, scopes, scope, vt);
            let clause_scopes = compute_clause_scopes(clauses, body, scopes, scope, vt);
            vt.merge(&add_exported_scopes(scopes, scope, &clause_scopes));
        }
        crate::Expr::Receive { clauses, after } => {
            let mut clause_scopes = compute_clause_scopes(clauses, body, scopes, scope, vt);
            if let Some(ra) = after {
                let mut sub_vt = vt.clone();
                let mut timeout_scope = scopes.new_scope(*scope);
                compute_expr_scopes(ra.timeout, body, scopes, &mut timeout_scope, &mut sub_vt);
                clause_scopes.push((timeout_scope, sub_vt));
                let mut sub_vt2 = vt.clone();
                // After-body scope is a sibling of timeout scope (child of
                // outer scope), not a child of timeout scope. In Erlang,
                // ToEs is evaluated against Vt (the base).
                // See erl_lint.erl `expr({'receive',Anno,Cs,To,ToEs}, Vt, St0)`:
                // https://github.com/erlang/otp/blob/OTP-28.3.2/lib/stdlib/src/erl_lint.erl#L2766-L2774
                let mut after_scope = scopes.new_scope(*scope);
                for expr in &ra.exprs {
                    compute_expr_scopes(*expr, body, scopes, &mut after_scope, &mut sub_vt2);
                }
                clause_scopes.push((after_scope, sub_vt2));
            };
            vt.merge(&add_exported_scopes(scopes, scope, &clause_scopes));
        }
        crate::Expr::Try {
            exprs,
            of_clauses,
            catch_clauses,
            after,
        } => {
            // From elp_lint: The only exports we allow are from the exprs to the of_clauses.
            let mut expr_vt = vt.clone();
            for expr in exprs {
                compute_expr_scopes(*expr, body, scopes, scope, &mut expr_vt);
            }
            let mut clause_scopes =
                compute_clause_scopes(of_clauses, body, scopes, scope, &mut expr_vt);
            for clause in catch_clauses {
                let mut sub_vt = vt.clone();
                let mut scope = scopes.new_scope(*scope);
                if let Some(class) = clause.class {
                    scopes.add_bindings(body, &mut scope, class, &mut sub_vt, AddBinding::IfUnused);
                }
                scopes.add_bindings(
                    body,
                    &mut scope,
                    clause.reason,
                    &mut sub_vt,
                    AddBinding::IfUnused,
                );
                if let Some(stack) = clause.stack {
                    scopes.add_bindings(body, &mut scope, stack, &mut sub_vt, AddBinding::IfUnused);
                }
                for guard in &clause.guards {
                    for expr in guard {
                        compute_expr_scopes(*expr, body, scopes, &mut scope, &mut sub_vt);
                    }
                }
                for expr in &clause.exprs {
                    compute_expr_scopes(*expr, body, scopes, &mut scope, &mut sub_vt);
                }
                clause_scopes.push((scope, sub_vt));
            }

            let mut sub_vt2 = vt.clone();
            for expr in after {
                compute_expr_scopes(*expr, body, scopes, scope, &mut sub_vt2);
            }
            vt.merge(&add_exported_scopes(scopes, scope, &clause_scopes));
        }
        crate::Expr::CaptureFun { target, arity } => {
            match target {
                crate::CallTarget::Local { name } => {
                    compute_expr_scopes(*name, body, scopes, scope, vt);
                }
                crate::CallTarget::Remote { module, name, .. } => {
                    compute_expr_scopes(*module, body, scopes, scope, vt);
                    compute_expr_scopes(*name, body, scopes, scope, vt);
                }
            };
            compute_expr_scopes(*arity, body, scopes, scope, vt);
        }
        crate::Expr::Closure { clauses, name } => {
            for clause in clauses.iter() {
                let mut sub_vt = vt.clone();
                let mut scope = scopes.new_scope(*scope);
                // For named funs, add the name to the closure's nested scope (not the outer scope)
                // This ensures the name is only visible inside the closure for recursion,
                // while the LHS of any containing match becomes the defining binding in outer scope
                if let Some(name) = name {
                    scopes.add_bindings(body, &mut scope, *name, &mut sub_vt, AddBinding::Always);
                }
                scopes.add_params_bindings(body, &mut scope, &clause.pats, &mut sub_vt);

                for guards in &clause.guards {
                    for guard in guards {
                        compute_expr_scopes(*guard, body, scopes, &mut scope, &mut sub_vt);
                    }
                }
                for expr in &clause.exprs {
                    compute_expr_scopes(*expr, body, scopes, &mut scope, &mut sub_vt);
                }
            }
        }
        crate::Expr::Maybe {
            exprs,
            else_clauses,
        } => {
            let mut expr_vt = vt.clone();
            let mut expr_scope = scopes.new_scope(*scope);

            for expr in exprs {
                match expr {
                    MaybeExpr::Cond { lhs, rhs } => {
                        compute_expr_scopes(*rhs, body, scopes, &mut expr_scope, &mut expr_vt);
                        scopes.add_bindings(
                            body,
                            &mut expr_scope,
                            *lhs,
                            &mut expr_vt,
                            AddBinding::IfUnused,
                        );
                    }
                    MaybeExpr::Expr(expr) => {
                        compute_expr_scopes(*expr, body, scopes, &mut expr_scope, &mut expr_vt);
                    }
                }
            }

            // In Erlang, maybe expressions export NO variables.
            // Process else-clauses in a sub-scope that doesn't export.
            let else_vt = vt.clone();
            let else_scope = scopes.new_scope(*scope);
            for clause in else_clauses {
                let mut sub_vt = else_vt.clone();
                let mut clause_scope = scopes.new_scope(else_scope);
                scopes.add_bindings(
                    body,
                    &mut clause_scope,
                    clause.pat,
                    &mut sub_vt,
                    AddBinding::IfUnused,
                );
                for guards in &clause.guards {
                    for guard in guards {
                        compute_expr_scopes(*guard, body, scopes, &mut clause_scope, &mut sub_vt);
                    }
                }
                for expr in &clause.exprs {
                    compute_expr_scopes(*expr, body, scopes, &mut clause_scope, &mut sub_vt);
                }
            }
        }
        crate::Expr::Paren { expr } => {
            compute_expr_scopes(*expr, body, scopes, scope, vt);
        }
        crate::Expr::SsrPlaceholder(_ssr) => {}
    }
}

fn compute_comprehension_expr_scopes(
    body: &Body,
    scopes: &mut ExprScopes,
    sub_vt: &mut VarTable,
    scope: &mut Idx<ScopeData>,
    expr: &ComprehensionExpr,
) {
    match expr {
        ComprehensionExpr::BinGenerator {
            pat,
            expr,
            strict: _,
        } => {
            // Evaluate generator expression in a sub-scope so that
            // expression-local bindings don't leak to subsequent
            // qualifiers.  Matches elp_lint.erl's vtold(Evt, Vt).
            let mut gen_vt = sub_vt.clone();
            let mut gen_scope = scopes.new_scope(*scope);
            compute_expr_scopes(*expr, body, scopes, &mut gen_scope, &mut gen_vt);
            *scope = scopes.new_scope(*scope);
            scopes.add_bindings(body, scope, *pat, sub_vt, AddBinding::Always);
        }
        ComprehensionExpr::ListGenerator {
            pat,
            expr,
            strict: _,
        } => {
            let mut gen_vt = sub_vt.clone();
            let mut gen_scope = scopes.new_scope(*scope);
            compute_expr_scopes(*expr, body, scopes, &mut gen_scope, &mut gen_vt);
            *scope = scopes.new_scope(*scope);
            scopes.add_bindings(body, scope, *pat, sub_vt, AddBinding::Always);
        }
        ComprehensionExpr::Expr(expr) => compute_expr_scopes(*expr, body, scopes, scope, sub_vt),
        ComprehensionExpr::MapGenerator {
            key,
            value,
            expr,
            strict: _,
        } => {
            let mut gen_vt = sub_vt.clone();
            let mut gen_scope = scopes.new_scope(*scope);
            compute_expr_scopes(*expr, body, scopes, &mut gen_scope, &mut gen_vt);
            *scope = scopes.new_scope(*scope);
            scopes.add_bindings(body, scope, *key, sub_vt, AddBinding::Always);
            scopes.add_bindings(body, scope, *value, sub_vt, AddBinding::Always);
        }
        ComprehensionExpr::Zip(exprs) => {
            // In elp_lint.erl (handle_generators), all zip generator
            // expressions are evaluated against the same pre-zip Vt
            // before any patterns are bound.
            for expr in exprs {
                match expr {
                    ComprehensionExpr::ListGenerator { expr, .. }
                    | ComprehensionExpr::BinGenerator { expr, .. }
                    | ComprehensionExpr::MapGenerator { expr, .. } => {
                        let mut gen_vt = sub_vt.clone();
                        let mut gen_scope = scopes.new_scope(*scope);
                        compute_expr_scopes(*expr, body, scopes, &mut gen_scope, &mut gen_vt);
                    }
                    _ => {}
                }
            }
            for expr in exprs {
                match expr {
                    ComprehensionExpr::ListGenerator { pat, .. }
                    | ComprehensionExpr::BinGenerator { pat, .. } => {
                        *scope = scopes.new_scope(*scope);
                        scopes.add_bindings(body, scope, *pat, sub_vt, AddBinding::Always);
                    }
                    ComprehensionExpr::MapGenerator { key, value, .. } => {
                        *scope = scopes.new_scope(*scope);
                        scopes.add_bindings(body, scope, *key, sub_vt, AddBinding::Always);
                        scopes.add_bindings(body, scope, *value, sub_vt, AddBinding::Always);
                    }
                    _ => {}
                }
            }
        }
    };
}

fn compute_clause_scopes(
    clauses: &[CRClause],
    body: &Body,
    scopes: &mut ExprScopes,
    scope: &mut ScopeId,
    vt: &mut VarTable,
) -> Vec<(ScopeId, VarTable)> {
    clauses
        .iter()
        .map(|clause| {
            let mut sub_vt = vt.clone();
            let mut scope = scopes.new_scope(*scope);
            scopes.add_bindings(
                body,
                &mut scope,
                clause.pat,
                &mut sub_vt,
                AddBinding::IfUnused,
            );
            for guards in &clause.guards {
                for guard in guards {
                    compute_expr_scopes(*guard, body, scopes, &mut scope, &mut sub_vt);
                }
            }
            for expr in &clause.exprs {
                compute_expr_scopes(*expr, body, scopes, &mut scope, &mut sub_vt);
            }
            (scope, sub_vt)
        })
        .collect()
}

fn add_exported_scopes(
    scopes: &mut ExprScopes,
    current_scope_id: &mut ScopeId,
    clause_scopes: &[(ScopeId, VarTable)],
) -> VarTable {
    let mut scope_info: FxHashMap<Var, Vec<PatId>> = FxHashMap::default();
    let mut ret_vt = VarTable::default();
    for (clause_scope, vt) in clause_scopes {
        let scope = &scopes[*clause_scope];
        for (name, pats) in &scope.entries.data {
            scope_info
                .entry(*name)
                .and_modify(|v| v.extend(pats))
                .or_insert_with(|| pats.clone());
        }
        ret_vt.merge(vt);
    }

    // And add them to a new scope to be valid after this point in the source
    *current_scope_id = scopes.new_scope(*current_scope_id);
    let current_scope = &mut scopes[*current_scope_id];
    for (name, pats) in scope_info {
        current_scope.entries.insert(name, pats);
    }
    ret_vt
}

#[cfg(test)]
mod tests {
    use elp_base_db::FileId;
    use elp_base_db::SourceDatabase;
    use elp_base_db::assert_eq_text;
    use elp_base_db::extract_offset;
    use elp_base_db::fixture::WithFixture;
    use elp_syntax::AstNode;
    use elp_syntax::algo::find_node_at_offset;
    use elp_syntax::ast;

    use crate::FunctionDefId;
    use crate::InFile;
    use crate::Semantic;
    use crate::db::DefDatabase;
    use crate::db::InternDatabase;
    use crate::test_db::TestDB;

    // Return the first function found in the test fixture
    fn find_function(db: &TestDB, file_id: FileId) -> FunctionDefId {
        let def_map = db.def_map(file_id);
        let (_, fun) = def_map.get_functions().next().unwrap();
        fun.function_id
    }

    #[track_caller]
    fn do_check(elp_fixture: &str, expected: &[&str]) {
        let (offset, code) = extract_offset(elp_fixture);
        let code = {
            let mut buf = String::new();
            let off: usize = offset.into();
            buf.push_str(&code[..off]);
            buf.push_str("M~arker");
            buf.push_str(&code[off..]);
            buf
        };

        let (db, fixture) = TestDB::with_fixture(&code);
        let position = fixture.position();
        let sema = Semantic::new(&db);
        let file_id = position.file_id;
        let offset = position.offset;

        let file_syntax = db.parse(file_id).syntax_node();
        let var: ast::Var = find_node_at_offset(&file_syntax, offset).unwrap();
        let marker = ast::Expr::ExprMax(ast::ExprMax::Var(var.clone()));
        let function = find_function(&db, file_id);

        let scopes = db.function_scopes(InFile {
            file_id,
            value: function,
        });
        let ast_clause_id = sema.find_enclosing_function_clause(var.syntax()).unwrap();
        let in_clause = sema.to_expr(InFile::new(file_id, &marker)).unwrap();

        let body = db.function_body(InFile::new(file_id, function));

        let expr_id = in_clause
            .expr_id_ast(InFile {
                file_id,
                value: &marker,
            })
            .unwrap();
        let clause_id = body.valid_clause_id(ast_clause_id).unwrap();
        let clause_scope = scopes.get(clause_id).unwrap();
        let scope = clause_scope.scope_for_expr(expr_id);

        let actual = clause_scope
            .scope_chain(scope)
            .flat_map(|scope| clause_scope.entries(scope).names())
            .map(|it| db.lookup_var(it).as_str().to_string())
            .collect::<Vec<_>>()
            .join("\n");
        let expected = expected.join("\n");
        assert_eq_text!(&expected, &actual);
    }

    // NOTE: Most of the scoping tests are done via the goto_definition.rs tests

    #[test]
    fn test_function_param_scope() {
        do_check(
            r"
            f(Bar, Baz) ->
              ~.
            ",
            &["Bar", "Baz"],
        );
    }

    #[test]
    fn test_simple_assign() {
        do_check(
            r"
            f() ->
              X = 1,
              ~.
            ",
            &["X"],
        );
    }

    #[test]
    fn test_case_guard_exports_pattern_var() {
        // Pattern variables in case clauses with guards should be
        // exported and visible after the case expression.
        // Guards should NOT create new scopes (matching elp_lint.erl).
        do_check(
            r"
            f(X) ->
              case X of
                {ok, Y} when is_integer(Y) -> Y;
                _ -> 0
              end,
              ~.
            ",
            &["Y", "X"],
        );
    }

    #[test]
    fn test_maybe_cond_no_leak() {
        // Condition bindings in a maybe expression should NOT be
        // visible after the maybe. In Erlang, maybe expressions
        // export NO variables.
        do_check(
            r"
            f() ->
              maybe
                {ok, X} ?= foo()
              end,
              ~.
            ",
            &[],
        );
    }

    #[test]
    fn test_maybe_cond_visible_inside() {
        // Condition bindings in a maybe expression should be visible
        // within the maybe body for subsequent expressions.
        do_check(
            r"
            f() ->
              maybe
                {ok, X} ?= foo(),
                ~
              end.
            ",
            &["X"],
        );
    }

    #[test]
    fn test_try_expr_vt_flows_to_of_clauses() {
        // Bindings from the try body should be visible in the
        // of-clauses. In elp_lint.erl, success clauses get
        // vtupdate(Evt0, Vt), but the current code passes the
        // original vt instead of expr_vt.
        do_check(
            r"
            f() ->
              try
                X = 1,
                X
              of
                Val -> ~
              catch
                _ -> 0
              end.
            ",
            &["Val", "X"],
        );
    }

    #[test]
    fn test_receive_after_exports() {
        // Bindings in the after-body of a receive should be exported
        // and visible after the receive expression. The after-body
        // scope should be a sibling of the timeout scope (child of
        // the outer scope), not a child of the timeout scope.
        // The timeout expression should not be pushed as a separate
        // clause entry — only the after-body should be.
        do_check(
            r"
            f() ->
              receive
                {ok, X} -> X
              after
                1000 ->
                  Y = 0,
                  Y
              end,
              ~.
            ",
            &["X", "Y"],
        );
    }

    #[test]
    fn test_closure_guard_no_new_scope() {
        // Guard expressions in closures should NOT create new scopes,
        // matching elp_lint.erl behavior. Parameters should remain
        // visible in the closure body.
        do_check(
            r"
            f() ->
              F = fun(X) when is_integer(X) -> ~, X end,
              F.
            ",
            &["X", "F"],
        );
    }

    #[test]
    fn test_generator_expr_locals_not_leaked() {
        // Variables bound inside a generator expression (RHS of <-)
        // should not be visible to subsequent qualifiers or the
        // comprehension body. In elp_lint.erl, handle_generator
        // discards expression-local variables via vtold(Evt, Vt).
        do_check(
            r"
            f() ->
              [~ || Y <- (X = [1,2,3])].
            ",
            &["Y"],
        );
    }

    #[test]
    fn test_zip_expr_does_not_see_sibling_pattern() {
        // In a zip generator (&&), all generator expressions should
        // be evaluated before any patterns are bound. In elp_lint.erl,
        // handle_generators evaluates all expressions against the
        // same pre-zip Vt. The second generator's expression should
        // NOT see the first generator's pattern variable.
        do_check(
            r"
            f() ->
              [{X, Y} || X <- [1,2,3] && Y <- [~]].
            ",
            &[],
        );
    }

    #[test]
    fn test_zip_comprehension() {
        do_check(
            r"
            f() ->
              As = [1,2,3],
              Bs = [4,5,6],
              [{X,Y, ~} || X <- As && Y <- Bs]
              .
            ",
            &["Y", "X", "As", "Bs"],
        );
    }

    #[test]
    fn test_binop_parallel_scoping() {
        // In a general binary op like `+`, both sides are evaluated
        // against the same original scope (parallel). The RHS should
        // NOT see bindings from the LHS.
        do_check(
            r"
            f() ->
                (X = 1) + (~ + X).
            ",
            &[],
        );
    }

    #[test]
    fn test_binop_exports_both_sides() {
        // Both sides of a general binary op should export their
        // bindings after the expression.
        do_check(
            r"
            f() ->
                (X = 1) + (Y = 2),
                ~.
            ",
            &["X", "Y"],
        );
    }

    #[test]
    fn test_binop_preserves_earlier_bindings_in_case() {
        // A BinaryOp must not create a new export scope that hides
        // earlier bindings from the enclosing case clause. If it does,
        // the case export only sees the BinaryOp's export scope entries
        // and loses Z (which was bound before the BinaryOp).
        do_check(
            r"
            f() ->
                case 1 of
                    _ -> Z = 3, Z + (Y = 2)
                end,
                ~.
            ",
            &["Z", "Y"],
        );
    }

    #[test]
    fn test_orelse_rhs_sees_lhs() {
        // orelse/andalso use sequential scoping: the RHS sees LHS bindings.
        do_check(
            r"
            f() ->
                (X = 1) orelse (~ + X).
            ",
            &["X"],
        );
    }

    #[test]
    fn test_fun_param_shadows_used_outer_var() {
        // When an outer variable X has been bound outside the fun, the
        // fun parameter X should still properly shadow it. Previously,
        // there was a bug where AddBinding::IfUnused would skip adding the
        // fun parameter if the outer variable was already marked as "used"
        // in the VarTable.
        // The scope chain shows the fun's X first, then the comprehension's X,
        // plus List from the function parameter.
        do_check(
            r"
            f(List) ->
                [foo(X, fun(X) -> ~ + X end) || X <- List].
            ",
            &["X", "X", "List"],
        );
    }
}

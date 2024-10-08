/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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

use fxhash::FxHashMap;
use la_arena::Arena;
use la_arena::ArenaMap;
use la_arena::Idx;

use super::UnexpandedIndex;
use crate::db::DefDatabase;
use crate::def_map::FunctionDefId;
use crate::expr::ClauseId;
use crate::expr::MaybeExpr;
use crate::fold::ParenStrategy;
use crate::fold::VisibleMacros;
use crate::Body;
use crate::CRClause;
use crate::ComprehensionBuilder;
use crate::ExprId;
use crate::FunctionClauseBody;
use crate::FunctionClauseId;
use crate::InFile;
use crate::Name;
use crate::PatId;
use crate::Var;

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
            .map(|(idx, clause)| (idx, ExprScopes::for_clause(&clause, anonymous_var)))
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
            } => {
                self.add_bindings(body, scope, *expansion, vt, add_bindings);
                for arg in args {
                    compute_expr_scopes(*arg, body, self, scope, vt);
                }
            }
        };
    }

    fn add_params_bindings(
        &mut self,
        body: &Body,
        scope: &mut ScopeId,
        params: &[PatId],
        vt: &mut VarTable,
    ) {
        params
            .iter()
            .for_each(|pat| self.add_bindings(body, scope, *pat, vt, AddBinding::IfUnused));
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
    match &(UnexpandedIndex {
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
        crate::Expr::BinaryOp { lhs, rhs, op: _ } => {
            // TODO: deal with `(X = 1) + (Y = 2)` binding both sides, and exporting values
            compute_expr_scopes(*lhs, body, scopes, scope, vt);
            compute_expr_scopes(*rhs, body, scopes, scope, vt);
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
                match expr {
                    crate::ComprehensionExpr::BinGenerator { pat, expr } => {
                        compute_expr_scopes(*expr, body, scopes, scope, &mut sub_vt);
                        *scope = scopes.new_scope(*scope);
                        scopes.add_bindings(body, scope, *pat, &mut sub_vt, AddBinding::Always);
                    }
                    crate::ComprehensionExpr::ListGenerator { pat, expr } => {
                        compute_expr_scopes(*expr, body, scopes, scope, &mut sub_vt);
                        *scope = scopes.new_scope(*scope);
                        scopes.add_bindings(body, scope, *pat, &mut sub_vt, AddBinding::Always);
                    }
                    crate::ComprehensionExpr::Expr(expr) => {
                        compute_expr_scopes(*expr, body, scopes, scope, &mut sub_vt)
                    }
                    crate::ComprehensionExpr::MapGenerator { key, value, expr } => {
                        compute_expr_scopes(*expr, body, scopes, scope, &mut sub_vt);
                        *scope = scopes.new_scope(*scope);
                        scopes.add_bindings(body, scope, *key, &mut sub_vt, AddBinding::Always);
                        scopes.add_bindings(body, scope, *value, &mut sub_vt, AddBinding::Always);
                    }
                };
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
                let mut scope = scopes.new_scope(*scope);
                compute_expr_scopes(ra.timeout, body, scopes, &mut scope, &mut sub_vt);
                clause_scopes.push((scope, sub_vt));
                let mut sub_vt2 = vt.clone();
                let mut scope = scopes.new_scope(scope);
                for expr in &ra.exprs {
                    compute_expr_scopes(*expr, body, scopes, &mut scope, &mut sub_vt2);
                }
                clause_scopes.push((scope, sub_vt2));
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
            let mut clause_scopes = compute_clause_scopes(of_clauses, body, scopes, scope, vt);
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
            if let Some(name) = name {
                scopes.add_bindings(body, scope, *name, vt, AddBinding::Always);
            }
            for clause in clauses.iter() {
                let mut sub_vt = vt.clone();
                let mut scope = scopes.new_scope(*scope);
                scopes.add_params_bindings(body, &mut scope, &clause.pats, &mut sub_vt);

                for guards in &clause.guards {
                    for guard in guards {
                        scope = scopes.new_scope(scope);
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
                        compute_expr_scopes(*rhs, body, scopes, &mut expr_scope, vt);
                        scopes.add_bindings(body, scope, *lhs, vt, AddBinding::IfUnused);
                    }
                    MaybeExpr::Expr(expr) => {
                        compute_expr_scopes(*expr, body, scopes, &mut expr_scope, &mut expr_vt);
                    }
                }
            }

            let clause_scopes = compute_clause_scopes(else_clauses, body, scopes, scope, vt);
            vt.merge(&add_exported_scopes(scopes, scope, &clause_scopes));
        }
        crate::Expr::Paren { expr } => {
            compute_expr_scopes(*expr, body, scopes, scope, vt);
        }
    }
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
                    scope = scopes.new_scope(scope);
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
    use elp_base_db::assert_eq_text;
    use elp_base_db::fixture::extract_offset;
    use elp_base_db::fixture::WithFixture;
    use elp_base_db::FileId;
    use elp_base_db::SourceDatabase;
    use elp_syntax::algo::find_node_at_offset;
    use elp_syntax::ast;
    use elp_syntax::AstNode;

    use crate::db::DefDatabase;
    use crate::db::InternDatabase;
    use crate::test_db::TestDB;
    use crate::FunctionDefId;
    use crate::InFile;
    use crate::Semantic;

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

        let (db, position, _) = TestDB::with_position(&code);
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
                file_id: file_id.into(),
                value: &marker,
            })
            .unwrap();
        let clause_id = body.valid_clause_id(ast_clause_id).unwrap();
        let clause_scope = scopes.get(clause_id).unwrap();
        let scope = clause_scope.scope_for_expr(expr_id);

        let actual: _ = clause_scope
            .scope_chain(scope)
            .flat_map(|scope| clause_scope.entries(scope).names())
            .map(|it| db.lookup_var(it).as_str().to_string())
            .collect::<Vec<_>>()
            .join("\n");
        let expected = expected.join("\n");
        assert_eq_text!(&expected, &actual);
    }

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
}

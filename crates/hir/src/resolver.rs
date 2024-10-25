/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Name resolution façade.

use std::sync::Arc;

use fxhash::FxHashSet;

use crate::body::scope::ExprScopes;
use crate::body::scope::ScopeId;
use crate::AnyExprId;
use crate::ExprId;
use crate::PatId;
use crate::Var;

pub type Resolution = (Var, Vec<PatId>);

#[derive(Debug, Clone)]
pub struct Resolver {
    pub scopes: Arc<ExprScopes>,
}

impl Resolver {
    pub fn new(expr_scopes: Arc<ExprScopes>) -> Resolver {
        Resolver {
            scopes: expr_scopes,
        }
    }

    pub fn resolve_any_expr_id(&self, var: &Var, id: AnyExprId) -> Option<&Vec<PatId>> {
        match id {
            AnyExprId::Expr(expr_id) => self.resolve_expr_id(var, expr_id),
            AnyExprId::Pat(pat_id) => self.resolve_pat_id(var, pat_id),
            AnyExprId::TypeExpr(_) => None,
            AnyExprId::Term(_) => None,
        }
    }

    pub fn resolve_pat_id(&self, var: &Var, pat_id: PatId) -> Option<&Vec<PatId>> {
        let scope = self.scopes.scope_for_pat(pat_id)?;
        self.resolve_var_in_scope(var, scope)
    }

    pub fn resolve_expr_id(&self, var: &Var, expr_id: ExprId) -> Option<&Vec<PatId>> {
        let scope = self.scopes.scope_for_expr(expr_id)?;
        self.resolve_var_in_scope(var, scope)
    }

    pub fn resolve_var_in_scope(&self, var: &Var, scope: ScopeId) -> Option<&Vec<PatId>> {
        self.scopes
            .scope_chain(Some(scope))
            .find_map(|scope| self.scopes.entries(scope).lookup(var))
    }

    pub fn scope_for_expr(&self, expr_id: ExprId) -> Option<ScopeId> {
        self.scopes.scope_for_expr(expr_id)
    }

    pub fn all_vars_in_scope(&self, scope: ScopeId) -> FxHashSet<Var> {
        self.scopes
            .scope_chain(Some(scope))
            .flat_map(|scope| self.scopes.entries(scope).names())
            .collect()
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Name resolution façade.

use fxhash::FxHashSet;

use crate::body::scope::ExprScopes;
use crate::body::scope::ScopeId;
use crate::ExprId;
use crate::PatId;
use crate::Var;

pub type Resolution = (Var, Vec<PatId>);

#[derive(Debug, Clone)]
pub struct Resolver {
    pub scopes: ExprScopes,
}

impl Resolver {
    pub fn new(expr_scopes: ExprScopes) -> Resolver {
        Resolver {
            scopes: expr_scopes,
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

    pub fn all_vars_in_scope(&self, scope: ScopeId) -> FxHashSet<Var> {
        self.scopes
            .scope_chain(Some(scope))
            .flat_map(|scope| self.scopes.entries(scope).names())
            .collect()
    }
}

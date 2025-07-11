/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use elp_base_db::salsa;
use elp_base_db::salsa::plumbing::AsId;

use crate::Name;

#[ra_ap_query_group_macro::query_group]
pub trait InternDatabase: salsa::Database {
    #[salsa::interned]
    fn atom(&self, name: Name) -> Atom;

    #[salsa::interned]
    fn var(&self, name: Name) -> Var;

    #[salsa::interned]
    fn ssr(&self, source: Arc<str>) -> SsrSource;
}

#[salsa::interned(no_lifetime)]
pub struct Atom {
    pub name: Name,
}

#[salsa::interned(no_lifetime)]
pub struct Var {
    pub name: Name,
}

#[salsa::interned(no_lifetime)]
pub struct SsrSource {
    pub source: Arc<str>,
}

impl Atom {
    pub fn as_string(&self, db: &dyn InternDatabase) -> String {
        db.lookup_atom(*self).to_string()
    }

    pub fn as_name(&self, db: &dyn InternDatabase) -> Name {
        db.lookup_atom(*self)
    }
}

impl Var {
    pub fn as_string(&self, db: &dyn InternDatabase) -> String {
        db.lookup_var(*self).to_string()
    }

    pub fn as_name(&self, db: &dyn InternDatabase) -> Name {
        db.lookup_var(*self)
    }
}

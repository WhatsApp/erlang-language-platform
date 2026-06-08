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
use ustr::Ustr;

use crate::Name;

#[ra_ap_query_group_macro::query_group]
pub trait InternDatabase: salsa::Database {
    #[salsa::interned]
    fn ssr(&self, source: Arc<str>) -> SsrSource;
}

/// An interned atom, backed by a global concurrent string interner.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Atom(Ustr);

impl std::hash::Hash for Atom {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_str().hash(state);
    }
}

impl PartialOrd for Atom {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Atom {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.as_str().cmp(other.0.as_str())
    }
}

impl Atom {
    pub fn new(name: &Name) -> Atom {
        Atom(Ustr::from(name.as_str()))
    }

    pub fn as_string(&self) -> String {
        self.0.to_string()
    }

    pub fn as_name(&self) -> Name {
        Name::from_erlang_service(self.0.as_str())
    }

    pub fn is_ssr_placeholder(&self) -> bool {
        self.0
            .as_str()
            .starts_with(elp_syntax::ast::SSR_ATOM_PLACEHOLDER_PREFIX)
    }
}

/// An interned variable name, backed by a global concurrent string interner.
///
/// Same rationale as `Atom` — see above.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Var(Ustr);

impl std::hash::Hash for Var {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_str().hash(state);
    }
}

impl PartialOrd for Var {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Var {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.as_str().cmp(other.0.as_str())
    }
}

impl Var {
    pub fn new(name: &Name) -> Var {
        Var(Ustr::from(name.as_str()))
    }

    pub fn as_string(&self) -> String {
        self.0.to_string()
    }

    pub fn as_name(&self) -> Name {
        Name::from_erlang_service(self.0.as_str())
    }

    /// Returns true if this variable is an SSR placeholder.
    /// SSR placeholders are variables with names starting with `_@`.
    /// This is true for both single-element placeholders (`_@Name`) and
    /// glob placeholders (`_@@Name`). Use
    /// [`Self::is_ssr_glob_placeholder`] to distinguish.
    pub fn is_ssr_placeholder(&self) -> bool {
        elp_syntax::ast::is_ssr_placeholder_name(self.0.as_str())
    }

    /// Returns true if this variable is an SSR glob placeholder.
    /// Glob placeholders are variables with names starting with `_@@`,
    /// and bind zero-or-more elements within a sequence (tuple, list,
    /// call args, etc.), à la Erlang Merl.
    pub fn is_ssr_glob_placeholder(&self) -> bool {
        elp_syntax::ast::is_ssr_glob_placeholder_name(self.0.as_str())
    }
}

#[salsa::interned(no_lifetime)]
pub struct SsrSource {
    pub source: Arc<str>,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn atom(name: &str) -> Atom {
        Atom::new(&Name::from_erlang_service(name))
    }

    fn var(name: &str) -> Var {
        Var::new(&Name::from_erlang_service(name))
    }

    #[test]
    fn test_atom_is_ssr_placeholder() {
        assert!(!atom("foo").is_ssr_placeholder());
        assert!(!atom("_foo").is_ssr_placeholder());
        assert!(atom("@Foo").is_ssr_placeholder());
        assert!(atom("@_").is_ssr_placeholder());
    }

    #[test]
    fn test_is_ssr_placeholder() {
        assert!(!var("X").is_ssr_placeholder());
        assert!(!var("_X").is_ssr_placeholder());
        assert!(!var("_").is_ssr_placeholder());
        assert!(var("_@X").is_ssr_placeholder());
        assert!(var("_@_").is_ssr_placeholder());
        // Glob placeholders are also SSR placeholders.
        assert!(var("_@@X").is_ssr_placeholder());
        assert!(var("_@@_").is_ssr_placeholder());
    }

    #[test]
    fn test_is_ssr_glob_placeholder() {
        assert!(!var("X").is_ssr_glob_placeholder());
        assert!(!var("_X").is_ssr_glob_placeholder());
        assert!(!var("_@X").is_ssr_glob_placeholder());
        assert!(!var("_@_").is_ssr_glob_placeholder());
        assert!(var("_@@X").is_ssr_glob_placeholder());
        assert!(var("_@@_").is_ssr_glob_placeholder());
        assert!(var("_@@Rest").is_ssr_glob_placeholder());
    }
}

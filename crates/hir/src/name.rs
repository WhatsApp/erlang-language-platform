/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! See [`Name`].

// @fb-only: pub mod meta_only;

use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt;
use std::ops::Deref;
use std::sync::OnceLock;

use elp_base_db::to_quoted_string;
use elp_syntax::SmolStr;
use elp_syntax::ast;
use elp_syntax::unescape;
use ustr::Ustr;

/// `Name` is a wrapper around string, in Erlang abstract forms represented
/// as raw atoms, which is used in hir for both references and declarations.
// The derived `PartialOrd` / `Ord` delegate to `Ustr`, which compares by string
// *content* (`as_str()`), so ordering is identical to the previous `SmolStr`
// behaviour.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Name(Ustr);

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}

// Hashing is by string *content*, not by `Ustr`'s stored hash: the latter is
// seeded per-process from the OS RNG (ahash is built with `runtime-rng`), so
// deriving `Hash` would make iteration order of the `Name`-keyed `FxHashMap`s
// vary between runs. Mirrors `Atom` / `Var` in `intern.rs`.
impl std::hash::Hash for Name {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl Name {
    /// Note: this is private to make creating name from random string hard.
    fn new(text: &str) -> Name {
        Name(Ustr::from(text))
    }

    /// Note: The one time it's okay to make a Name from an arbitrary string
    ///       is when reading it from the wire when talking to erlang_service
    pub fn from_erlang_service(text: &str) -> Name {
        Name::new(text)
    }

    /// Shortcut to create a plain text name.
    fn new_inline(text: &str) -> Name {
        Name::new(text)
    }

    pub fn raw(&self) -> SmolStr {
        SmolStr::new(self.0.as_str())
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn to_quoted_string(&self) -> Cow<'_, str> {
        if self == &*MISSING {
            Cow::Borrowed(self.as_str())
        } else {
            to_quoted_string(self.as_str())
        }
    }

    /// Resolve a name from the text of token.
    ///
    /// This replicates the atom normalisation done in the Erlang parser
    fn resolve(raw_text: &str) -> Name {
        let escaped = unescape::unescape_string(raw_text).unwrap_or(Cow::Borrowed(raw_text));
        Name::new(&escaped)
    }

    pub(super) fn arg(argument_index_starting_from_one: usize) -> Name {
        Self::new(&format!("Arg{argument_index_starting_from_one}"))
    }
}

impl Deref for Name {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_str()
    }
}

impl PartialEq<str> for Name {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl<'a> PartialEq<&'a str> for Name {
    fn eq(&self, other: &&'a str) -> bool {
        self.as_str() == *other
    }
}

/// Sentinel `Name` for things missing in the source code.
///
/// A module-level `LazyLock`, on the same principle as [`known`]: a future
/// `Name(Ustr)` cannot be built in a `const` context, and Rust has no associated
/// `static`, so this cannot live on `impl Name`. Interned once, then shared.
///
/// Ideally missing names would have `gensym` semantics (each equal only to
/// itself), but that is hard to express in salsa, so we punt for now.
pub static MISSING: std::sync::LazyLock<Name> =
    std::sync::LazyLock::new(|| Name::new_inline("[missing name]"));

/// Sentinel `Name` for anonymous vars (`_`). See [`MISSING`].
pub static ANONYMOUS: std::sync::LazyLock<Name> =
    std::sync::LazyLock::new(|| Name::new_inline("_"));

/// `NameArity` is a wrapper around `Name` with arity attached,
/// this is used frequently in Erlang for identifying various language elements
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NameArity(Name, u32);

impl fmt::Display for NameArity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.0.to_quoted_string(), self.1)
    }
}

impl NameArity {
    pub const fn new(name: Name, arity: u32) -> NameArity {
        NameArity(name, arity)
    }

    pub fn name(&self) -> &Name {
        &self.0
    }

    pub fn arity(&self) -> u32 {
        self.1
    }

    pub fn as_label(&self) -> String {
        format!("{self}")
    }
}

/// `MacroName` is a wrapper around `Name` with optional arity attached,
/// this is used in Erlang for identifying macros
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MacroName(Name, Option<u32>);

impl fmt::Display for MacroName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(arity) = self.1 {
            write!(f, "{}/{}", self.0, arity)
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl MacroName {
    pub const fn new(name: Name, arity: Option<u32>) -> MacroName {
        MacroName(name, arity)
    }

    pub fn name(&self) -> &Name {
        &self.0
    }

    pub fn arity(&self) -> Option<u32> {
        self.1
    }

    pub fn with_arity(self, arity: Option<u32>) -> Self {
        MacroName(self.0, arity)
    }

    pub fn extended_display_name(&self) -> String {
        match self.arity() {
            Some(_arity) => format!("{self}"),
            None => format!("{self}/NOARGS"),
        }
    }
}

// There's explicitly no conversion from `ast::Name`
// It represents names before macro resolution, which should be
// processed to obtain the final name - plain atom

pub trait AsName {
    fn as_name(&self) -> Name;
}

impl AsName for ast::Atom {
    fn as_name(&self) -> Name {
        Name::resolve(&self.raw_text())
    }
}

impl AsName for ast::Var {
    fn as_name(&self) -> Name {
        Name::new(self.text().as_str())
    }
}

impl AsName for ast::MacroName {
    fn as_name(&self) -> Name {
        match self {
            ast::MacroName::Atom(atom) => atom.as_name(),
            ast::MacroName::Var(var) => var.as_name(),
        }
    }
}

pub fn erlang_funs() -> &'static HashSet<NameArity> {
    static ERLANG_FUNS: OnceLock<HashSet<NameArity>> = OnceLock::new();
    ERLANG_FUNS.get_or_init(|| {
        HashSet::from_iter(ast::erlang_funs().iter().map(|&f| {
            NameArity::new(
                Name::new_inline(f.0),
                f.1.try_into().expect("arity should fit in u32"),
            )
        }))
    })
}

pub mod known {
    // Each known name is a lazily-interned `Name` rather than a `const`: a
    // future `Name(Ustr)` cannot be constructed in a `const` context, so these
    // are `LazyLock`s (interned once on first use). Call sites deref them
    // (`*known::foo`, `&*known::foo`, `known::foo.clone()`).
    macro_rules! known_names {
        ($($ident:ident),* $(,)?) => {
            $(
                #[allow(bad_style)]
                pub static $ident: std::sync::LazyLock<super::Name> =
                    std::sync::LazyLock::new(|| super::Name::new_inline(stringify!($ident)));
            )*
        };
    }

    known_names!(
        // predefined macros
        FILE,
        FUNCTION_NAME,
        FUNCTION_ARITY,
        LINE,
        MODULE,
        MODULE_STRING,
        MACHINE,
        OTP_RELEASE,
        // well known macros
        assertEqual,
        assertEqualSorted,
        assertMatch,
        // predefined values
        ELP,
        // test macros
        TEST,
        DEBUG,
        // known atoms
        apply,
        behaviour_info,
        behavior_info,
        client,
        defined,
        dialyzer,
        erlang,
        error,
        export_all,
        find,
        get_stacktrace,
        graphql_parser,
        graphql_scanner,
        handle,
        hidden,
        id,
        is_atom,
        is_binary,
        is_call,
        is_function,
        is_integer,
        is_list,
        is_map,
        is_record,
        is_tuple,
        is_var,
        lager,
        main,
        maps,
        module_info,
        modules,
        no_auto_import,
        ok,
        parse_transform,
        restart,
        server,
        stacktrace,
        start_link,
        shutdown,
        significant,
        start,
        string,
        thrift_parser,
        thrift_scanner,
        undefined,
        utf8,
        // Common Test framework
        all,
        end_per_group,
        end_per_testcase,
        end_per_suite,
        group,
        groups,
        init_per_group,
        init_per_testcase,
        init_per_suite,
        params,
        suite,
        testcase,
        // Warnings
        nowarn_missing_spec_all,
        nowarn_missing_spec,
        warn_missing_spec_all,
        warn_missing_spec,
        // Attribute names
        author,
        codegen_source,
        compile,
        doc,
        moduledoc,
        nifs,
        on_load,
        oncall,
        // Dialyzer options
        error_handling,
        extra_return,
        missing_return,
        no_behaviours,
        no_contracts,
        no_extra_return,
        no_fail_call,
        no_fun_app,
        no_improper_lists,
        no_match,
        no_missing_calls,
        no_missing_return,
        no_opaque,
        no_opaque_union,
        no_return,
        no_undefined_callbacks,
        no_underspecs,
        no_unknown,
        no_unused,
        nowarn_function,
        opaque_union,
        overspecs,
        race_conditions,
        specdiffs,
        underspecs,
        unmatched_returns,
        unknown,
    );

    #[allow(bad_style)]
    pub static true_name: std::sync::LazyLock<super::Name> =
        std::sync::LazyLock::new(|| super::Name::new_inline("true"));
    #[allow(bad_style)]
    pub static false_name: std::sync::LazyLock<super::Name> =
        std::sync::LazyLock::new(|| super::Name::new_inline("false"));
    #[allow(bad_style)]
    pub static type_name: std::sync::LazyLock<super::Name> =
        std::sync::LazyLock::new(|| super::Name::new_inline("type"));
}

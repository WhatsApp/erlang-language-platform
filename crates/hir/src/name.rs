/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! See [`Name`].

use std::borrow::Cow;
use std::fmt;
use std::ops::Deref;

use elp_base_db::to_quoted_string;
use elp_syntax::ast;
use elp_syntax::unescape;
use elp_syntax::SmolStr;

/// `Name` is a wrapper around string, in Erlang abstract forms represented
/// as raw atoms, which is used in hir for both references and declarations
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Name(SmolStr);

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl Name {
    /// A fake name for things missing in the source code.
    ///
    /// Ideally, we want a `gensym` semantics for missing names -- each missing
    /// name is equal only to itself. It's not clear how to implement this in
    /// salsa though, so we punt on that bit for a moment.
    pub const MISSING: Self = Self::new_inline("[missing name]");
    /// Ditto for anonymous vars
    pub const ANONYMOUS: Self = Self::new_inline("_");

    /// Note: this is private to make creating name from random string hard.
    const fn new(text: SmolStr) -> Name {
        Name(text)
    }

    /// Note: The one time it's okay to make a Name from an arbitrary string
    ///       is when reading it from the wire when talking to erlang_service
    pub fn from_erlang_service(text: &str) -> Name {
        Name(SmolStr::new(text))
    }

    /// Shortcut to create inline plain text name
    const fn new_inline(text: &str) -> Name {
        Name::new(SmolStr::new_inline(text))
    }

    pub fn raw(&self) -> SmolStr {
        self.0.clone()
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn to_quoted_string(&self) -> String {
        if self == &Self::MISSING {
            self.to_string()
        } else {
            to_quoted_string(self.as_str())
        }
    }

    /// Resolve a name from the text of token.
    ///
    /// This replicates the atom normalisation done in the Erlang parser
    fn resolve(raw_text: &str) -> Name {
        let escaped = unescape::unescape_string(raw_text).unwrap_or(Cow::Borrowed(raw_text));
        Name::new(escaped.into())
    }

    pub(super) fn arg(argument_index_starting_from_one: usize) -> Name {
        Self::new(SmolStr::new(format!(
            "Arg{argument_index_starting_from_one}"
        )))
    }
}

impl Deref for Name {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq<str> for Name {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl<'a> PartialEq<&'a str> for Name {
    fn eq(&self, other: &&'a str) -> bool {
        self.0 == *other
    }
}

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
        Name::new(self.text().into())
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

pub mod known {
    macro_rules! known_names {
        ($($ident:ident),* $(,)?) => {
            $(
                #[allow(bad_style)]
                pub const $ident: super::Name =
                    super::Name::new_inline(stringify!($ident));
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
        // predefined values
        ELP,
        // known atoms
        erlang,
        apply,
        export_all,
        parse_transform,
        // Common Test framework
        all,
        group,
        groups,
        init_per_suite,
        end_per_suite,
        testcase,
        warn_missing_spec,
        nowarn_missing_spec,
        warn_missing_spec_all,
        nowarn_missing_spec_all,
    );
}

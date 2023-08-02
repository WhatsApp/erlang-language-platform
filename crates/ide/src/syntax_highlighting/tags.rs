/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Defines token tags we use for syntax highlighting.
//! A tag is not unlike a CSS class.

use std::fmt;
use std::fmt::Write;
use std::ops;

use elp_ide_db::SymbolKind;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Highlight {
    pub tag: HlTag,
    pub mods: HlMods,
}

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HlMods(u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum HlTag {
    Symbol(SymbolKind),

    // For things which don't have a specific highlight. This is the
    // default for anything we do not specifically set, and maps to VS Code `generic` type
    None,
}

// Don't forget to adjust the feature description in
// crates/ide/src/syntax_highlighting.rs.  And make sure to use the
// lsp strings used when converting to the protocol in
// crates\elp\src\semantic_tokens.rs, not the names of the variants
// here.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(u8)]
pub enum HlMod {
    /// Bound variable in pattern.
    Bound = 0,
    // Local vs exported function name.
    ExportedFunction,
    DeprecatedFunction,
}

impl HlTag {
    fn as_str(self) -> &'static str {
        match self {
            HlTag::Symbol(symbol) => match symbol {
                // Tied in to to_proto::symbol_kind
                SymbolKind::File => "file",
                SymbolKind::Module => "module",
                SymbolKind::Function => "function",
                SymbolKind::Record => "struct",
                SymbolKind::RecordField => "struct",
                SymbolKind::Type => "type_parameter",
                SymbolKind::Define => "constant",
                SymbolKind::Variable => "variable",
                SymbolKind::Callback => "function",
            },
            HlTag::None => "none",
        }
    }
}

impl fmt::Display for HlTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}

impl HlMod {
    const ALL: &'static [HlMod; 3] = &[
        HlMod::Bound,
        HlMod::ExportedFunction,
        HlMod::DeprecatedFunction,
    ];

    fn as_str(self) -> &'static str {
        match self {
            HlMod::Bound => "bound",
            HlMod::ExportedFunction => "exported_function",
            HlMod::DeprecatedFunction => "deprecated_function",
        }
    }

    fn mask(self) -> u32 {
        1 << (self as u32)
    }
}

impl fmt::Display for HlMod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}

impl fmt::Display for Highlight {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.tag.fmt(f)?;
        for modifier in self.mods.iter() {
            f.write_char('.')?;
            modifier.fmt(f)?;
        }
        Ok(())
    }
}

impl From<HlTag> for Highlight {
    fn from(tag: HlTag) -> Highlight {
        Highlight::new(tag)
    }
}

impl Highlight {
    pub(crate) fn new(tag: HlTag) -> Highlight {
        Highlight {
            tag,
            mods: HlMods::default(),
        }
    }
    pub fn is_empty(&self) -> bool {
        self.tag == HlTag::None && self.mods.is_empty()
    }
}
impl ops::BitOr<HlMod> for HlTag {
    type Output = Highlight;

    fn bitor(self, rhs: HlMod) -> Highlight {
        Highlight::new(self) | rhs
    }
}

impl ops::BitOrAssign<HlMod> for HlMods {
    fn bitor_assign(&mut self, rhs: HlMod) {
        self.0 |= rhs.mask();
    }
}

impl ops::BitOrAssign<HlMod> for Highlight {
    fn bitor_assign(&mut self, rhs: HlMod) {
        self.mods |= rhs;
    }
}

impl ops::BitOr<HlMod> for Highlight {
    type Output = Highlight;

    fn bitor(mut self, rhs: HlMod) -> Highlight {
        self |= rhs;
        self
    }
}

impl HlMods {
    pub fn is_empty(&self) -> bool {
        self.0 == 0
    }

    pub fn contains(self, m: HlMod) -> bool {
        self.0 & m.mask() == m.mask()
    }

    pub fn iter(self) -> impl Iterator<Item = HlMod> {
        HlMod::ALL
            .iter()
            .copied()
            .filter(move |it| self.0 & it.mask() == it.mask())
    }
}

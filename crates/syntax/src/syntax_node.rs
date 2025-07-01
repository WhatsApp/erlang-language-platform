/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use num_traits::FromPrimitive;
use num_traits::ToPrimitive;
use rowan::Language;

use crate::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ELPLanguage {}

impl Language for ELPLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        Self::Kind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.to_u16().unwrap())
    }
}

pub type SyntaxNode = rowan::SyntaxNode<ELPLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<ELPLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<ELPLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<ELPLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<ELPLanguage>;
pub type NodeOrToken = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

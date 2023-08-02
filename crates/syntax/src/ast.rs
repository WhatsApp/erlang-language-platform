/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Abstract Syntax Tree, layered on top of untyped `SyntaxNode`s

// We simply re-export the generated ast under a top-level namespace
// pub use crate::generated::gen_ast::*;
pub use crate::ast::generated::nodes::*;

pub mod edit;
mod erlang;
pub mod generated;
mod node_ext;
mod operators;
mod traits;

use std::marker::PhantomData;

pub use self::erlang::in_erlang_module;
pub use self::erlang::is_erlang_fun;
pub use self::erlang::is_erlang_type;
pub use self::generated::nodes::*;
pub use self::node_ext::Arity;
pub use self::node_ext::HasArity;
pub use self::operators::ArithOp;
pub use self::operators::BinaryOp;
pub use self::operators::CompOp;
pub use self::operators::ListOp;
pub use self::operators::LogicOp;
pub use self::operators::MapOp;
pub use self::operators::Ordering;
pub use self::operators::UnaryOp;
pub use self::traits::*;
use crate::syntax_node::SyntaxNode;
use crate::syntax_node::SyntaxNodeChildren;
use crate::syntax_node::SyntaxToken;
use crate::SyntaxKind;

/// The main trait to go from untyped `SyntaxNode`  to a typed ast. The
/// conversion itself has zero runtime cost: ast and syntax nodes have exactly
/// the same representation: a pointer to the tree root and a pointer to the
/// node itself.
pub trait AstNode {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
}

/// An iterator over `SyntaxNode` children of a particular AST type.
#[derive(Debug, Clone)]
pub struct AstChildren<N> {
    inner: SyntaxNodeChildren,
    ph: PhantomData<N>,
}

impl<N> AstChildren<N> {
    fn new(parent: &SyntaxNode) -> Self {
        AstChildren {
            inner: parent.children(),
            ph: PhantomData,
        }
    }
}

impl<N: AstNode> Iterator for AstChildren<N> {
    type Item = N;
    fn next(&mut self) -> Option<N> {
        self.inner.find_map(N::cast)
    }
}

mod support {
    use super::AstChildren;
    use super::AstNode;
    use super::SyntaxKind;
    use super::SyntaxNode;
    use super::SyntaxToken;

    pub(super) fn child<N: AstNode>(parent: &SyntaxNode, nth: usize) -> Option<N> {
        parent.children().flat_map(N::cast).nth(nth)
    }

    pub(super) fn children<N: AstNode>(parent: &SyntaxNode) -> AstChildren<N> {
        AstChildren::new(parent)
    }

    pub(super) fn token(parent: &SyntaxNode, kind: SyntaxKind, nth: usize) -> Option<SyntaxToken> {
        parent
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .filter(|it| it.kind() == kind)
            .nth(nth)
    }
}

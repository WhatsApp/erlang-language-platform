/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Collection of assorted algorithms for syntax trees.

use std::ops::RangeInclusive;

use itertools::Itertools;
use rowan::NodeOrToken;

use crate::AstNode;
use crate::Direction;
use crate::SyntaxElement;
use crate::SyntaxKind;
use crate::SyntaxNode;
use crate::SyntaxNodePtr;
use crate::SyntaxToken;
use crate::TextRange;
use crate::TextSize;

/// Returns ancestors of the node at the offset, sorted by length. This should
/// do the right thing at an edge, e.g. when searching for expressions at `{
/// ^foo }` we will get the name reference instead of the whole block, which
/// we would get if we just did `find_token_at_offset(...).flat_map(|t|
/// t.parent().ancestors())`.
pub fn ancestors_at_offset(
    node: &SyntaxNode,
    offset: TextSize,
) -> Option<impl Iterator<Item = SyntaxNode> + use<>> {
    // Check that token_at_offset will not fail an assertion, and
    // return None if so
    let range = node.text_range();
    if range.start() <= offset && offset <= range.end() {
        Some(
            node.token_at_offset(offset)
                .map(|token| token.parent_ancestors())
                .kmerge_by(|node1, node2| node1.text_range().len() < node2.text_range().len()),
        )
    } else {
        None
    }
}

/// Finds a node of specific Ast type at offset. Note that this is slightly
/// imprecise: if the cursor is strictly between two nodes of the desired type,
/// as in
///
/// ``no_run
/// -module(bar).|-export([foo/0]).
/// ``
///
/// then the shorter node will be silently preferred.
pub fn find_node_at_offset<N: AstNode>(syntax: &SyntaxNode, offset: TextSize) -> Option<N> {
    ancestors_at_offset(syntax, offset).and_then(|mut ns| ns.find_map(N::cast))
}

pub fn find_node_at_range<N: AstNode>(syntax: &SyntaxNode, range: TextRange) -> Option<N> {
    syntax.covering_element(range).ancestors().find_map(N::cast)
}

/// Skip to next non `trivia` token
pub fn skip_trivia_token(mut token: SyntaxToken, direction: Direction) -> Option<SyntaxToken> {
    while token.kind().is_trivia() {
        token = match direction {
            Direction::Next => token.next_token()?,
            Direction::Prev => token.prev_token()?,
        }
    }
    Some(token)
}

/// Skip to next non `whitespace` token.  This does not skip comments.
pub fn skip_whitespace_token(mut token: SyntaxToken, direction: Direction) -> Option<SyntaxToken> {
    while token.kind() == SyntaxKind::WHITESPACE {
        token = match direction {
            Direction::Next => token.next_token()?,
            Direction::Prev => token.prev_token()?,
        }
    }
    Some(token)
}

/// Skip inline comments, stopping as soon as a new line is encountered.
pub fn skip_inline_comment(syntax: &SyntaxNode) -> Option<SyntaxToken> {
    let mut token = syntax.last_token()?.next_token()?;
    while token.kind() == SyntaxKind::COMMENT
        || token.kind() == SyntaxKind::WHITESPACE && !token.text().starts_with("\n")
    {
        token = token.next_token()?;
    }
    Some(token)
}

/// Finds the first sibling in the given direction which is not `trivia`
pub fn non_trivia_sibling(element: SyntaxElement, direction: Direction) -> Option<SyntaxElement> {
    return match element {
        NodeOrToken::Node(node) => node
            .siblings_with_tokens(direction)
            .skip(1)
            .find(not_trivia),
        NodeOrToken::Token(token) => token
            .siblings_with_tokens(direction)
            .skip(1)
            .find(not_trivia),
    };

    fn not_trivia(element: &SyntaxElement) -> bool {
        match element {
            NodeOrToken::Node(_) => true,
            NodeOrToken::Token(token) => !token.kind().is_trivia(),
        }
    }
}

/// Finds the first sibling in the given direction which is not a comment
pub fn non_whitespace_sibling(
    element: SyntaxElement,
    direction: Direction,
) -> Option<SyntaxElement> {
    return match element {
        NodeOrToken::Node(node) => node
            .siblings_with_tokens(direction)
            .skip(1)
            .find(not_whitespace),
        NodeOrToken::Token(token) => token
            .siblings_with_tokens(direction)
            .skip(1)
            .find(not_whitespace),
    };

    fn not_whitespace(element: &SyntaxElement) -> bool {
        match element {
            NodeOrToken::Node(_) => true,
            NodeOrToken::Token(token) => token.kind() != SyntaxKind::WHITESPACE,
        }
    }
}

pub fn least_common_ancestor(u: &SyntaxNode, v: &SyntaxNode) -> Option<SyntaxNode> {
    if u == v {
        return Some(u.clone());
    }

    let u_depth = u.ancestors().count();
    let v_depth = v.ancestors().count();
    let keep = u_depth.min(v_depth);

    let u_candidates = u.ancestors().skip(u_depth - keep);
    let v_candidates = v.ancestors().skip(v_depth - keep);
    let (res, _) = u_candidates.zip(v_candidates).find(|(x, y)| x == y)?;
    Some(res)
}

pub fn neighbor<T: AstNode>(me: &T, direction: Direction) -> Option<T> {
    me.syntax().siblings(direction).skip(1).find_map(T::cast)
}

pub fn has_errors(node: &SyntaxNode) -> bool {
    node.children().any(|it| it.kind() == SyntaxKind::ERROR)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum InsertPosition<T> {
    First,
    Last,
    Before(T),
    After(T),
}

/// Adds specified children (tokens or nodes) to the current node at the
/// specific position.
///
/// This is a type-unsafe low-level editing API, if you need to use it,
/// prefer to create a type-safe abstraction on top of it instead.
pub fn insert_children(
    parent: &SyntaxNode,
    position: InsertPosition<SyntaxElement>,
    to_insert: impl IntoIterator<Item = SyntaxElement>,
) -> SyntaxNode {
    let mut to_insert = to_insert.into_iter();
    _insert_children(parent, position, &mut to_insert)
}

fn _insert_children(
    parent: &SyntaxNode,
    position: InsertPosition<SyntaxElement>,
    to_insert: &mut dyn Iterator<Item = SyntaxElement>,
) -> SyntaxNode {
    let mut delta = TextSize::default();
    let to_insert = to_insert.map(|element| {
        delta += element.text_range().len();
        to_green_element(element)
    });

    let parent_green = parent.green();
    let mut old_children = parent_green.children().map(|it| match it {
        NodeOrToken::Token(it) => NodeOrToken::Token(it.to_owned()),
        NodeOrToken::Node(it) => NodeOrToken::Node(it.to_owned()),
    });

    let new_children = match &position {
        InsertPosition::First => to_insert.chain(old_children).collect::<Vec<_>>(),
        InsertPosition::Last => old_children.chain(to_insert).collect::<Vec<_>>(),
        InsertPosition::Before(anchor) | InsertPosition::After(anchor) => {
            let take_anchor = if let InsertPosition::After(_) = position {
                1
            } else {
                0
            };
            let split_at = position_of_child(parent, anchor.clone()) + take_anchor;
            let before = old_children.by_ref().take(split_at).collect::<Vec<_>>();
            before
                .into_iter()
                .chain(to_insert)
                .chain(old_children)
                .collect::<Vec<_>>()
        }
    };

    with_children(parent, new_children)
}

/// Replaces all nodes in `to_delete` with nodes from `to_insert`
///
/// This is a type-unsafe low-level editing API, if you need to use it,
/// prefer to create a type-safe abstraction on top of it instead.
pub fn replace_children(
    parent: &SyntaxNode,
    to_delete: RangeInclusive<SyntaxElement>,
    to_insert: impl IntoIterator<Item = SyntaxElement>,
) -> SyntaxNode {
    let mut to_insert = to_insert.into_iter();
    _replace_children(parent, to_delete, &mut to_insert)
}

fn _replace_children(
    parent: &SyntaxNode,
    to_delete: RangeInclusive<SyntaxElement>,
    to_insert: &mut dyn Iterator<Item = SyntaxElement>,
) -> SyntaxNode {
    let start = position_of_child(parent, to_delete.start().clone());
    let end = position_of_child(parent, to_delete.end().clone());
    let parent_green = parent.green();
    let mut old_children = parent_green.children().map(|it| match it {
        NodeOrToken::Token(it) => NodeOrToken::Token(it.to_owned()),
        NodeOrToken::Node(it) => NodeOrToken::Node(it.to_owned()),
    });

    let before = old_children.by_ref().take(start).collect::<Vec<_>>();
    let new_children = before
        .into_iter()
        .chain(to_insert.map(to_green_element))
        .chain(old_children.skip(end + 1 - start))
        .collect::<Vec<_>>();
    with_children(parent, new_children)
}

fn with_children(
    parent: &SyntaxNode,
    new_children: Vec<NodeOrToken<rowan::GreenNode, rowan::GreenToken>>,
) -> SyntaxNode {
    let _p = tracing::info_span!("with_children").entered();

    let new_green = rowan::GreenNode::new(rowan::SyntaxKind(parent.kind() as u16), new_children);
    with_green(parent, new_green)
}

fn with_green(syntax_node: &SyntaxNode, green: rowan::GreenNode) -> SyntaxNode {
    let len = green.children().map(|it| it.text_len()).sum::<TextSize>();
    let new_root_node = syntax_node.replace_with(green);
    let new_root_node = SyntaxNode::new_root(new_root_node);

    // FIXME: use a more elegant way to re-fetch the node (#1185), make
    // `range` private afterwards
    let mut ptr = SyntaxNodePtr::new(syntax_node);
    ptr.range = TextRange::at(ptr.range.start(), len);
    ptr.to_node(&new_root_node)
}

fn position_of_child(parent: &SyntaxNode, child: SyntaxElement) -> usize {
    parent
        .children_with_tokens()
        .position(|it| it == child)
        .expect("element is not a child of current element")
}

fn to_green_element(element: SyntaxElement) -> NodeOrToken<rowan::GreenNode, rowan::GreenToken> {
    match element {
        NodeOrToken::Node(it) => it.green().into(),
        NodeOrToken::Token(it) => it.green().to_owned().into(),
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Collection of assorted algorithms for syntax trees.

use std::hash::BuildHasherDefault;
use std::ops::RangeInclusive;

use fxhash::FxHashMap;
use indexmap::IndexMap;
use itertools::Itertools;
use rowan::NodeOrToken;
use text_edit::TextEditBuilder;

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

type FxIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<fxhash::FxHasher>>;

#[derive(Debug, Hash, PartialEq, Eq)]
enum TreeDiffInsertPos {
    After(SyntaxElement),
    AsFirstChild(SyntaxElement),
}

#[derive(Debug)]
pub struct TreeDiff {
    replacements: FxHashMap<SyntaxElement, SyntaxElement>,
    deletions: Vec<SyntaxElement>,
    // the vec as well as the indexmap are both here to preserve order
    insertions: FxIndexMap<TreeDiffInsertPos, Vec<SyntaxElement>>,
}

impl TreeDiff {
    pub fn into_text_edit(&self, builder: &mut TextEditBuilder) {
        let _p = tracing::info_span!("into_text_edit").entered();

        for (anchor, to) in self.insertions.iter() {
            let offset = match anchor {
                TreeDiffInsertPos::After(it) => it.text_range().end(),
                TreeDiffInsertPos::AsFirstChild(it) => it.text_range().start(),
            };
            to.iter()
                .for_each(|to| builder.insert(offset, to.to_string()));
        }
        for (from, to) in self.replacements.iter() {
            builder.replace(from.text_range(), to.to_string())
        }
        for text_range in self.deletions.iter().map(SyntaxElement::text_range) {
            builder.delete(text_range);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.replacements.is_empty() && self.deletions.is_empty() && self.insertions.is_empty()
    }
}

/// Finds a (potentially minimal) diff, which, applied to `from`, will result in `to`.
///
/// Specifically, returns a structure that consists of a replacements, insertions and deletions
/// such that applying this map on `from` will result in `to`.
///
/// This function tries to find a fine-grained diff.
pub fn diff(from: &SyntaxNode, to: &SyntaxNode) -> TreeDiff {
    let _p = tracing::info_span!("diff").entered();

    let mut diff = TreeDiff {
        replacements: FxHashMap::default(),
        insertions: FxIndexMap::default(),
        deletions: Vec::new(),
    };
    let (from, to) = (from.clone().into(), to.clone().into());

    if !syntax_element_eq(&from, &to) {
        go(&mut diff, from, to);
    }
    return diff;

    fn syntax_element_eq(lhs: &SyntaxElement, rhs: &SyntaxElement) -> bool {
        lhs.kind() == rhs.kind()
            && lhs.text_range().len() == rhs.text_range().len()
            && match (&lhs, &rhs) {
                (NodeOrToken::Node(lhs), NodeOrToken::Node(rhs)) => {
                    lhs == rhs || lhs.text() == rhs.text()
                }
                (NodeOrToken::Token(lhs), NodeOrToken::Token(rhs)) => lhs.text() == rhs.text(),
                _ => false,
            }
    }

    // FIXME: this is horribly inefficient. I bet there's a cool algorithm to diff trees properly.
    fn go(diff: &mut TreeDiff, lhs: SyntaxElement, rhs: SyntaxElement) {
        let (lhs, rhs) = match lhs.as_node().zip(rhs.as_node()) {
            Some((lhs, rhs)) => (lhs, rhs),
            _ => {
                cov_mark::hit!(diff_node_token_replace);
                diff.replacements.insert(lhs, rhs);
                return;
            }
        };

        let mut look_ahead_scratch = Vec::default();

        let mut rhs_children = rhs.children_with_tokens();
        let mut lhs_children = lhs.children_with_tokens();
        let mut last_lhs = None;
        loop {
            let lhs_child = lhs_children.next();
            match (lhs_child.clone(), rhs_children.next()) {
                (None, None) => break,
                (None, Some(element)) => {
                    let insert_pos = match last_lhs.clone() {
                        Some(prev) => {
                            cov_mark::hit!(diff_insert);
                            TreeDiffInsertPos::After(prev)
                        }
                        // first iteration, insert into out parent as the first child
                        None => {
                            cov_mark::hit!(diff_insert_as_first_child);
                            TreeDiffInsertPos::AsFirstChild(lhs.clone().into())
                        }
                    };
                    diff.insertions.entry(insert_pos).or_default().push(element);
                }
                (Some(element), None) => {
                    cov_mark::hit!(diff_delete);
                    diff.deletions.push(element);
                }
                (Some(ref lhs_ele), Some(ref rhs_ele)) if syntax_element_eq(lhs_ele, rhs_ele) => {}
                (Some(lhs_ele), Some(rhs_ele)) => {
                    // nodes differ, look for lhs_ele in rhs, if its found we can mark everything up
                    // until that element as insertions. This is important to keep the diff minimal
                    // in regards to insertions that have been actually done, this is important for
                    // use insertions as we do not want to replace the entire module node.
                    look_ahead_scratch.push(rhs_ele.clone());
                    let mut rhs_children_clone = rhs_children.clone();
                    let mut insert = false;
                    for rhs_child in rhs_children_clone.by_ref() {
                        if syntax_element_eq(&lhs_ele, &rhs_child) {
                            cov_mark::hit!(diff_insertions);
                            insert = true;
                            break;
                        } else {
                            look_ahead_scratch.push(rhs_child);
                        }
                    }
                    let drain = look_ahead_scratch.drain(..);
                    if insert {
                        let insert_pos = if let Some(prev) = last_lhs.clone().filter(|_| insert) {
                            TreeDiffInsertPos::After(prev)
                        } else {
                            cov_mark::hit!(insert_first_child);
                            TreeDiffInsertPos::AsFirstChild(lhs.clone().into())
                        };

                        diff.insertions.entry(insert_pos).or_default().extend(drain);
                        rhs_children = rhs_children_clone;
                    } else {
                        go(diff, lhs_ele, rhs_ele)
                    }
                }
            }
            last_lhs = lhs_child.or(last_lhs);
        }
    }
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

#[cfg(test)]
mod tests {
    use expect_test::Expect;
    use expect_test::expect;
    use itertools::Itertools;
    use text_edit::TextEdit;

    use crate::AstNode;
    use crate::SyntaxElement;
    use crate::SyntaxKind;

    #[test]
    fn replace_node_token() {
        cov_mark::check!(diff_node_token_replace);
        check_diff(
            r#"-module(foo)."#,
            r#"ident"#,
            expect![[r#"
                insertions:



                replacements:

                Line 0: Token(ANON_DASH@0..1 "-") -> ident

                deletions:

                Line 1: module
                Line 1: (
                Line 1: foo
                Line 1: )
                Line 1: .
            "#]],
        );
    }

    #[test]
    fn replace_parent() {
        cov_mark::check!(diff_node_token_replace);
        check_diff(
            r#""#,
            r#"-module(foo)."#,
            expect![[r#"
                insertions:



                replacements:

                Line 0: Token(SOURCE_FILE@0..0 "") -> -module(foo).

                deletions:


            "#]],
        );
    }

    #[test]
    fn insert_last() {
        cov_mark::check!(diff_insert);
        check_diff(
            r#"
-module(foo).
-use(foo).
-use(bar)."#,
            r#"
-module(foo).
-use(foo).
-use(bar).
-use(baz)."#,
            expect![[r#"
                insertions:

                Line 3: After(Node(WILD_ATTRIBUTE@26..36))
                -> "\n"
                -> -use(baz).

                replacements:



                deletions:


            "#]],
        );
    }

    #[test]
    fn insert_middle() {
        check_diff(
            r#"
-module(foo).
-use(foo).
-use(bar)."#,
            r#"
-module(foo).
-use(foo).
-use(baz).
-use(bar)."#,
            expect![[r#"
                insertions:

                Line 3: After(Token(WHITESPACE@25..26 "\n"))
                -> -use(baz).
                -> "\n"

                replacements:



                deletions:


            "#]],
        )
    }

    #[test]
    fn insert_first() {
        check_diff(
            r#"
-use(bar).
-use(baz)."#,
            r#"
-export([foo/0]).
-use(bar).
-use(baz)."#,
            expect![[r#"
                insertions:

                Line 0: After(Token(WHITESPACE@0..1 "\n"))
                -> -export([foo/0]).
                -> "\n"

                replacements:



                deletions:


            "#]],
        )
    }

    #[test]
    fn first_child_insertion() {
        // cov_mark::check!(insert_first_child);
        check_diff(
            r#"
main() ->
  ok."#,
            r#"
-module(foo).

main() ->
  ok."#,
            expect![[r#"
                insertions:

                Line 0: After(Token(WHITESPACE@0..1 "\n"))
                -> -module(foo).
                -> "\n\n"

                replacements:



                deletions:


            "#]],
        );
    }

    #[test]
    fn delete_last() {
        cov_mark::check!(diff_delete);
        check_diff(
            r#"-module(foo).
            -bar([baz])."#,
            r#"-module(foo)."#,
            expect![[r#"
                insertions:



                replacements:



                deletions:

                Line 1: "\n            "
                Line 2: -bar([baz]).
            "#]],
        );
    }

    #[test]
    fn delete_middle() {
        // cov_mark::check!(diff_insertions);
        check_diff(
            r#"
-export([foo/0,bar/1]).
-bar(aaa).

-foo(bbb).
"#,
            r#"
-export([foo/0,bar/1]).

-foo(bbb).
"#,
            expect![[r#"
                insertions:

                Line 1: After(Node(EXPORT_ATTRIBUTE@1..24))
                -> "\n\n"
                -> -foo(bbb).

                replacements:



                deletions:

                Line 2: -bar(aaa).
                Line 3: "\n\n"
                Line 4: -foo(bbb).
                Line 5: "\n"
            "#]],
        )
    }

    #[test]
    fn delete_first() {
        check_diff(
            r#"
-export([foo/0,bar/1]).

-foo(bbb).
"#,
            r#"
-foo(bbb).
"#,
            expect![[r#"
                insertions:



                replacements:

                Line 1: Token(ANON_DASH@1..2 "-") -> -foo
                Line 2: Token(ANON_EXPORT@2..8 "export") -> (bbb)
                Line 2: Token(ANON_LPAREN@8..9 "(") -> .
                Line 2: Token(WHITESPACE@24..26 "\n\n") -> "\n"

                deletions:

                Line 2: [
                Line 2: foo/0
                Line 2: ,
                Line 2: bar/1
                Line 2: ]
                Line 2: )
                Line 2: .
                Line 3: -foo(bbb).
                Line 4: "\n"
            "#]],
        )
    }

    fn check_diff(from: &str, to: &str, expected_diff: Expect) {
        let from_node = crate::SourceFile::parse_text(from).tree().syntax().clone();
        let to_node = crate::SourceFile::parse_text(to).tree().syntax().clone();
        let diff = super::diff(&from_node, &to_node);

        let line_number =
            |syn: &SyntaxElement| from[..syn.text_range().start().into()].lines().count();

        let fmt_syntax = |syn: &SyntaxElement| match syn.kind() {
            SyntaxKind::WHITESPACE => format!("{:?}", syn.to_string()),
            _ => format!("{}", syn),
        };

        let insertions =
            diff.insertions
                .iter()
                .format_with("\n", |(k, v), f| -> Result<(), std::fmt::Error> {
                    f(&format!(
                        "Line {}: {:?}\n-> {}",
                        line_number(match k {
                            super::TreeDiffInsertPos::After(syn) => syn,
                            super::TreeDiffInsertPos::AsFirstChild(syn) => syn,
                        }),
                        k,
                        v.iter().format_with("\n-> ", |v, f| f(&fmt_syntax(v)))
                    ))
                });

        let replacements = diff
            .replacements
            .iter()
            .sorted_by_key(|(syntax, _)| syntax.text_range().start())
            .format_with("\n", |(k, v), f| {
                f(&format!(
                    "Line {}: {:?} -> {}",
                    line_number(k),
                    k,
                    fmt_syntax(v)
                ))
            });

        let deletions = diff.deletions.iter().format_with("\n", |v, f| {
            f(&format!("Line {}: {}", line_number(v), &fmt_syntax(v)))
        });

        let actual = format!(
            "insertions:\n\n{}\n\nreplacements:\n\n{}\n\ndeletions:\n\n{}\n",
            insertions, replacements, deletions
        );
        expected_diff.assert_eq(&actual);

        let mut from = from.to_owned();
        let mut text_edit = TextEdit::builder();
        diff.into_text_edit(&mut text_edit);
        text_edit.finish().apply(&mut from);
        assert_eq!(&*from, to, "diff did not turn `from` to `to`");
    }
}

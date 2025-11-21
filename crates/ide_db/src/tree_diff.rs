/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Basic tree diffing functionality.

use std::hash::BuildHasherDefault;

use elp_syntax::NodeOrToken;
use elp_syntax::SyntaxElement;
use elp_syntax::SyntaxNode;
use fxhash::FxHashMap;
use indexmap::IndexMap;

use crate::text_edit::TextEditBuilder;

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

#[cfg(test)]
mod tests {
    use elp_syntax::AstNode;
    use elp_syntax::SourceFile;
    use elp_syntax::SyntaxElement;
    use elp_syntax::SyntaxKind;
    use expect_test::Expect;
    use expect_test::expect;
    use itertools::Itertools;

    use crate::text_edit::TextEdit;

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
        let from_node = SourceFile::parse_text(from).tree().syntax().clone();
        let to_node = SourceFile::parse_text(to).tree().syntax().clone();
        let diff = super::diff(&from_node, &to_node);

        let line_number =
            |syn: &SyntaxElement| from[..syn.text_range().start().into()].lines().count();

        let fmt_syntax = |syn: &SyntaxElement| match syn.kind() {
            SyntaxKind::WHITESPACE => format!("{:?}", syn.to_string()),
            _ => format!("{syn}"),
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
            "insertions:\n\n{insertions}\n\nreplacements:\n\n{replacements}\n\ndeletions:\n\n{deletions}\n"
        );
        expected_diff.assert_eq(&actual);

        let mut from = from.to_owned();
        let mut text_edit = TextEdit::builder();
        diff.into_text_edit(&mut text_edit);
        text_edit.finish().apply(&mut from);
        assert_eq!(&*from, to, "diff did not turn `from` to `to`");
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Extract edoc comments from a file
//!
//! From https://www.erlang.org/doc/apps/edoc/chapter.html#introduction
//! Quote
//!    EDoc lets you write the documentation of an Erlang program as comments
//!    in the source code itself, using tags on the form "@Name ...". A
//!    source file does not have to contain tags for EDoc to generate its
//!    documentation, but without tags the result will only contain the basic
//!    available information that can be extracted from the module.

//!    A tag must be the first thing on a comment line, except for leading
//!    '%' characters and whitespace. The comment must be between program
//!    declarations, and not on the same line as any program text. All the
//!    following text - including consecutive comment lines - up until the
//!    end of the comment or the next tagged line, is taken as the content of
//!    the tag.

//!    Tags are associated with the nearest following program construct "of
//!    significance" (the module name declaration and function
//!    definitions). Other constructs are ignored.
//! End Quote

use elp_base_db::FileId;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::AstPtr;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use elp_syntax::TextRange;
use fxhash::FxHashMap;
use lazy_static::lazy_static;
use regex::Regex;

use crate::db::DefDatabase;
use crate::InFileAstPtr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdocHeader {
    form: InFileAstPtr<ast::Form>,
    tags: Vec<EdocTag>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdocTag {
    name: String,
    comments: Vec<EdocComment>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdocComment {
    range: TextRange,
    source: String,
    syntax: InFileAstPtr<ast::Comment>,
}

impl EdocHeader {
    pub fn text_ranges(&self) -> Vec<TextRange> {
        self.tags.iter().flat_map(|tag| tag.text_ranges()).collect()
    }

    pub fn comments(&self) -> Vec<InFileAstPtr<ast::Comment>> {
        self.tags.iter().flat_map(|tag| tag.comments()).collect()
    }

    pub fn doc(&self) -> impl Iterator<Item = &EdocTag> {
        return self.tags.iter().filter(|tag| tag.name == "doc");
    }

    pub fn params(&self) -> FxHashMap<String, String> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^%+\s+@param ([^\s]+)\s+(.*)$").unwrap();
        }
        let mut res = FxHashMap::default();
        for source in self.sources_by_tag("param".to_string()) {
            if let Some(captures) = RE.captures(&source) {
                if captures.len() == 3 {
                    res.insert(captures[1].to_string(), captures[2].to_string());
                }
            }
        }
        res
    }

    pub fn sources_by_tag(&self, name: String) -> Vec<String> {
        self.tags
            .iter()
            .filter(|tag| tag.name == name)
            .flat_map(|tag| tag.sources())
            .collect()
    }

    pub fn function_tags(&self, name: String) -> Vec<String> {
        self.tags
            .iter()
            .filter(|tag| tag.name == name)
            .flat_map(|tag| tag.sources())
            .collect()
    }
}

impl EdocTag {
    pub fn text_ranges(&self) -> Vec<TextRange> {
        self.comments.iter().map(|comment| comment.range).collect()
    }
    pub fn comments(&self) -> Vec<InFileAstPtr<ast::Comment>> {
        self.comments.iter().map(|comment| comment.syntax).collect()
    }
    pub fn sources(&self) -> Vec<String> {
        self.comments
            .iter()
            .map(|comment| comment.source.clone())
            .collect()
    }
}

pub fn file_edoc_comments_query(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> Option<FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>> {
    let source = db.parse(file_id).tree();
    let mut res = FxHashMap::default();
    source.forms().for_each(|f| {
        if is_significant(f.syntax()) {
            let mut comments: Vec<_> = prev_form_nodes(f.syntax())
                .filter(|syntax| {
                    syntax.kind() == elp_syntax::SyntaxKind::COMMENT && only_comment_on_line(syntax)
                })
                .filter_map(ast::Comment::cast)
                .collect();
            comments.reverse();
            if let Some(edoc) =
                edoc_from_comments(InFileAstPtr::new(file_id, AstPtr::new(&f)), &comments)
            {
                res.insert(edoc.form, edoc);
            }
        }
    });
    Some(res)
}

fn edoc_from_comments(
    form: InFileAstPtr<ast::Form>,
    comments: &[ast::Comment],
) -> Option<EdocHeader> {
    let tags = comments
        .iter()
        .skip_while(|c| contains_edoc_tag(&c.syntax().text().to_string()).is_none())
        .map(|c| {
            (
                contains_edoc_tag(&c.syntax().text().to_string()),
                EdocComment {
                    range: c.syntax().text_range(),
                    source: c.syntax().text().to_string(),
                    syntax: InFileAstPtr::new(form.file_id(), AstPtr::new(c)),
                },
            )
        })
        .fold(Vec::default(), |mut acc, (tag, comment)| {
            if let Some(name) = tag {
                acc.push(EdocTag {
                    name,
                    comments: vec![comment],
                });
            } else if !&acc.is_empty() {
                let mut t = acc[0].clone();
                t.comments.push(comment);
                acc[0] = EdocTag {
                    name: t.name,
                    comments: t.comments,
                }
            }
            acc
        });
    if tags.is_empty() {
        None
    } else {
        Some(EdocHeader { form, tags })
    }
}

/// An edoc comment must be alone on a line, it cannot come after
/// code.
fn only_comment_on_line(comment: &SyntaxNode) -> bool {
    // We check for a positive "other" found on the same line, in case
    // the comment is the first line of the file, which will not have
    // a preceding newline.
    let mut node = comment
        .siblings_with_tokens(elp_syntax::Direction::Prev)
        .skip(1); // Starts with itself

    loop {
        if let Some(node) = node.next() {
            if let Some(tok) = node.into_token() {
                if tok.kind() == SyntaxKind::WHITESPACE && tok.text().contains('\n') {
                    return true;
                }
            } else {
                return false;
            }
        } else {
            return true;
        };
    }
}

fn prev_form_nodes(syntax: &SyntaxNode) -> impl Iterator<Item = SyntaxNode> {
    syntax
        .siblings_with_tokens(elp_syntax::Direction::Prev)
        .skip(1) // Starts with itself
        .filter_map(|node_or_token| node_or_token.into_node())
        .take_while(|node| !is_significant(node))
}

/// Significant forms for edoc are functions and module declarations.
fn is_significant(node: &SyntaxNode) -> bool {
    node.kind() == SyntaxKind::FUN_DECL || node.kind() == SyntaxKind::MODULE_ATTRIBUTE
}

/// Check if the given comment starts with an edoc tag.
///    A tag must be the first thing on a comment line, except for leading
///    '%' characters and whitespace.
fn contains_edoc_tag(comment: &str) -> Option<String> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^%+\s+@([^\s]+).*$").unwrap();
    }
    RE.captures_iter(comment).next().map(|c| c[1].to_string())
}

#[cfg(test)]
mod tests {
    use elp_base_db::fixture::WithFixture;
    use elp_syntax::ast;
    use expect_test::expect;
    use expect_test::Expect;
    use fxhash::FxHashMap;

    use super::contains_edoc_tag;
    use super::file_edoc_comments_query;
    use super::EdocComment;
    use super::EdocHeader;
    use super::EdocTag;
    use crate::test_db::TestDB;
    use crate::InFileAstPtr;

    fn test_print(edoc: FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>) -> String {
        let mut buf = String::default();
        edoc.iter().for_each(|(_k, EdocHeader { form, tags })| {
            buf.push_str(format!("{}\n", form.syntax_ptr_string()).as_str());
            tags.iter().for_each(|EdocTag { name, comments }| {
                buf.push_str(format!("  {}\n", name).as_str());
                comments.iter().for_each(
                    |EdocComment {
                         range,
                         source,
                         syntax: _,
                     }| {
                        buf.push_str(format!("    {:?}: \"{}\"\n", range, source).as_str());
                    },
                );
            });
        });
        buf
    }

    #[track_caller]
    fn check(fixture: &str, expected: Expect) {
        let (db, fixture) = TestDB::with_fixture(fixture);
        let file_id = fixture.files[0];
        let edocs = file_edoc_comments_query(&db, file_id);
        expected.assert_eq(&test_print(edocs.unwrap()))
    }

    #[test]
    fn test_contains_annotation() {
        expect![[r#"
            Some(
                "foo",
            )
        "#]]
        .assert_debug_eq(&contains_edoc_tag("%% @foo bar"));
    }

    #[test]
    fn edoc_1() {
        check(
            r#"
                %% @doc blah
                %% @param Foo ${2:Argument description}
                %% @param Arg2 ${3:Argument description}
                %% @returns ${4:Return description}
                foo(Foo, some_atom) -> ok.
"#,
            expect![[r#"
                SyntaxNodePtr { range: 130..156, kind: FUN_DECL }
                  doc
                    0..12: "%% @doc blah"
                  param
                    13..52: "%% @param Foo ${2:Argument description}"
                  param
                    53..93: "%% @param Arg2 ${3:Argument description}"
                  returns
                    94..129: "%% @returns ${4:Return description}"
            "#]],
        )
    }

    #[test]
    fn edoc_2() {
        check(
            r#"
                %% Just a normal comment
                %% @param Foo ${2:Argument description}
                %% Does not have a tag
                %% @returns ${4:Return description}
                foo(Foo, some_atom) -> ok.
"#,
            expect![[r#"
                SyntaxNodePtr { range: 124..150, kind: FUN_DECL }
                  param
                    25..64: "%% @param Foo ${2:Argument description}"
                    65..87: "%% Does not have a tag"
                  returns
                    88..123: "%% @returns ${4:Return description}"
            "#]],
        )
    }

    #[test]
    fn edoc_must_be_alone_on_line() {
        check(
            r#"
                bar() -> ok. %% @doc not an edoc comment
                %% @tag is an edoc comment
                foo(Foo, some_atom) -> ok.
"#,
            expect![[r#"
                SyntaxNodePtr { range: 68..94, kind: FUN_DECL }
                  tag
                    41..67: "%% @tag is an edoc comment"
            "#]],
        )
    }

    #[test]
    fn edoc_ignores_insignificant_forms() {
        check(
            r#"
                %% @tag is an edoc comment
                -compile(warn_missing_spec).
                -include_lib("stdlib/include/assert.hrl").
                -define(X,3).
                -export([foo/2]).
                -import(erlang, []).
                -type a_type() :: typ | false.
                -export_type([a_type/0]).
                -behaviour(gen_server).
                -callback do_it(Typ :: a_type()) -> ok.
                -spec foo(Foo :: type1(), type2()) -> ok.
                -opaque client() :: #client{}.
                -type client2() :: #client2{}.
                -optional_callbacks([do_it/1]).
                -record(state, {profile}).
                -wild(attr).
                %% Part of the same edoc
                foo(Foo, some_atom) -> ok.
"#,
            expect![[r#"
                SyntaxNodePtr { range: 474..500, kind: FUN_DECL }
                  tag
                    0..26: "%% @tag is an edoc comment"
                    449..473: "%% Part of the same edoc"
            "#]],
        )
    }

    #[test]
    fn edoc_module_attribute() {
        check(
            r#"
                %% @tag is an edoc comment
                -module(foo).
"#,
            expect![[r#"
                SyntaxNodePtr { range: 27..40, kind: MODULE_ATTRIBUTE }
                  tag
                    0..26: "%% @tag is an edoc comment"
            "#]],
        )
    }

    #[test]
    fn edoc_multiple() {
        check(
            r#"
                %% @foo is an edoc comment
                -module(foo).

                %% @doc fff is ...
                fff() -> ok.

                %% @doc This will be ignored
"#,
            // Note: if you update the grammar, the order of these nodes
            // may change.
            expect![[r#"
                SyntaxNodePtr { range: 27..40, kind: MODULE_ATTRIBUTE }
                  foo
                    0..26: "%% @foo is an edoc comment"
                SyntaxNodePtr { range: 61..73, kind: FUN_DECL }
                  doc
                    42..60: "%% @doc fff is ..."
            "#]],
        )
    }
}

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
//!    the tag. The @end tag is used to explicitly mark the end of a comment.

//!    Tags are associated with the nearest following program construct "of
//!    significance" (the module name declaration and function
//!    definitions). Other constructs are ignored.
//! End Quote

use elp_base_db::FileId;
use elp_base_db::SourceDatabase;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::AstPtr;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use fxhash::FxHashMap;
use lazy_static::lazy_static;
use regex::Regex;

use crate::db::DefDatabase;
use crate::InFileAstPtr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdocHeader {
    pub kind: EdocHeaderKind,
    form: InFileAstPtr<ast::Form>,
    pub doc: Option<Doc>,
    pub params: FxHashMap<String, Param>,
    pub returns: Option<Returns>,
    pub range: TextRange,
}

impl EdocHeader {
    pub fn comments(&self) -> Vec<InFileAstPtr<ast::Comment>> {
        let mut res = vec![];
        if let Some(doc) = &self.doc {
            res.extend(doc.comments());
        }
        for (_name, param) in &self.params {
            res.extend(param.comments());
        }
        if let Some(returns) = &self.returns {
            res.extend(returns.comments());
        }
        res
    }

    pub fn to_markdown(&self) -> String {
        let mut res = vec![];
        if let Some(doc) = &self.doc {
            let prefix = match self.kind {
                EdocHeaderKind::Module => "\n-moduledoc",
                EdocHeaderKind::Function => "-doc",
            };
            res.push(format!("{prefix} \"\"\""));
            for line in &doc.lines {
                if let Some(text) = &line.text {
                    res.push(convert_single_quotes(&convert_triple_quotes(text)));
                }
            }
            res.push("\"\"\".".to_string());
        }
        res.join("\n")
    }
}

pub trait Tag {
    fn tag(&self) -> String;
    fn comments(&self) -> Vec<InFileAstPtr<ast::Comment>>;
    fn tag_range(&self, db: &dyn SourceDatabase) -> Option<TextRange> {
        let tag = self.tag();
        let comment = self.comments().first()?.to_ast(db);
        let text = comment.syntax().text().to_string();
        let start = TextSize::new(text.find(&tag)? as u32);
        let end = start + TextSize::new(tag.len() as u32);
        let offset = comment.syntax().text_range().start();
        Some(TextRange::new(offset + start, offset + end))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EdocHeaderKind {
    Module,
    Function,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Doc {
    pub lines: Vec<EdocDocLine>,
}

impl Tag for Doc {
    fn tag(&self) -> String {
        "@doc".to_string()
    }

    fn comments(&self) -> Vec<InFileAstPtr<ast::Comment>> {
        self.lines.iter().map(|line| line.syntax.clone()).collect()
    }
}

impl Tag for Param {
    fn tag(&self) -> String {
        "@param".to_string()
    }

    fn comments(&self) -> Vec<InFileAstPtr<ast::Comment>> {
        vec![self.syntax]
    }
}

impl Tag for Returns {
    fn tag(&self) -> String {
        "@returns".to_string()
    }

    fn comments(&self) -> Vec<InFileAstPtr<ast::Comment>> {
        vec![self.syntax]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdocDocLine {
    pub text: Option<String>,
    pub syntax: InFileAstPtr<ast::Comment>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: String,
    pub description: String,
    pub syntax: InFileAstPtr<ast::Comment>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Returns {
    text: String,
    syntax: InFileAstPtr<ast::Comment>,
}

pub fn file_edoc_comments_query(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> Option<FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>> {
    let source = db.parse(file_id).tree();
    let mut res = FxHashMap::default();
    source.forms().for_each(|f| {
        if let Some(kind) = edoc_header_kind(f.syntax()) {
            let mut comments: Vec<_> = prev_form_nodes(f.syntax())
                .filter(|syntax| {
                    syntax.kind() == elp_syntax::SyntaxKind::COMMENT && only_comment_on_line(syntax)
                })
                .filter_map(ast::Comment::cast)
                .collect();
            comments.reverse();
            let comments: Vec<&ast::Comment> = comments
                .iter()
                .skip_while(|c| extract_edoc_tag(&c.syntax().text().to_string()).is_none())
                .collect();
            if let Some(comment) = comments.last() {
                let form = InFileAstPtr::new(file_id, AstPtr::new(&f));
                let edoc = parse_edoc(kind, form, &comments, comment.syntax().text_range());
                res.insert(edoc.form, edoc);
            }
        }
    });
    Some(res)
}

fn parse_edoc(
    kind: EdocHeaderKind,
    form: InFileAstPtr<ast::Form>,
    comments: &Vec<&ast::Comment>,
    range: TextRange,
) -> EdocHeader {
    let mut parsing_doc = false;
    let mut doc_lines = vec![];
    let mut params = FxHashMap::default();
    let mut returns = None;
    let mut range = range.clone();
    for comment in comments {
        let mut text = comment.syntax().text().to_string();
        range = range.cover(comment.syntax().text_range());
        let tag = extract_edoc_tag(&text);
        match tag {
            None => {
                if parsing_doc {
                    let text = text.trim_start_matches(|c: char| c == '%' || c.is_whitespace());
                    doc_lines.push(EdocDocLine {
                        text: Some(text.to_string()),
                        syntax: InFileAstPtr::new(form.file_id(), AstPtr::new(comment)),
                    });
                }
            }
            Some(tag) => {
                if let Some(start) = text.find(&tag) {
                    if text.len() > start + tag.len() {
                        text = text[start + tag.len() + 1..].to_string();
                    } else {
                        text = "".to_string();
                    }
                }
                match tag.as_str() {
                    "doc" => {
                        parsing_doc = true;
                        doc_lines.push(EdocDocLine {
                            text: Some(text),
                            syntax: InFileAstPtr::new(form.file_id(), AstPtr::new(comment)),
                        });
                    }
                    "end" => {
                        parsing_doc = false;
                        doc_lines.push(EdocDocLine {
                            text: None,
                            syntax: InFileAstPtr::new(form.file_id(), AstPtr::new(comment)),
                        });
                    }
                    "param" => {
                        if let Some((name, description)) = text.split_once(" ") {
                            params.insert(
                                name.to_string(),
                                Param {
                                    name: name.to_string(),
                                    description: description.to_string(),
                                    syntax: InFileAstPtr::new(form.file_id(), AstPtr::new(comment)),
                                },
                            );
                        }
                    }
                    "returns" => {
                        returns = Some(Returns {
                            text,
                            syntax: InFileAstPtr::new(form.file_id(), AstPtr::new(comment)),
                        });
                    }
                    _ => {}
                }
            }
        }
    }
    let doc = if doc_lines.is_empty() {
        None
    } else {
        Some(Doc { lines: doc_lines })
    };
    EdocHeader {
        kind,
        form,
        range,
        doc,
        params,
        returns,
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
        .take_while(|node| !edoc_header_kind(node).is_some())
}

fn edoc_header_kind(node: &SyntaxNode) -> Option<EdocHeaderKind> {
    match node.kind() {
        SyntaxKind::FUN_DECL => Some(EdocHeaderKind::Function),
        SyntaxKind::MODULE_ATTRIBUTE => Some(EdocHeaderKind::Module),
        _ => None,
    }
}

/// Check if the given comment starts with an edoc tag.
///    A tag must be the first thing on a comment line, except for leading
///    '%' characters and whitespace.
fn extract_edoc_tag(comment: &str) -> Option<String> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^%+\s+@([^\s]+).*$").unwrap();
    }
    RE.captures_iter(comment).next().map(|c| c[1].to_string())
}

fn convert_single_quotes(comment: &str) -> String {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"`([^']*)'").unwrap();
    }
    RE.replace_all(comment, "`$1`").to_string()
}

fn convert_triple_quotes(comment: &str) -> String {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"'''[']*").unwrap();
    }
    RE.replace_all(comment, "```").to_string()
}

#[cfg(test)]
mod tests {
    use elp_base_db::fixture::WithFixture;
    use elp_syntax::ast;
    use expect_test::expect;
    use expect_test::Expect;
    use fxhash::FxHashMap;

    use super::extract_edoc_tag;
    use super::file_edoc_comments_query;
    use super::EdocHeader;
    use crate::edoc::Param;
    use crate::edoc::Returns;
    use crate::test_db::TestDB;
    use crate::InFileAstPtr;

    fn test_print(edoc: FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>) -> String {
        let mut buf = String::default();
        edoc.iter().for_each(
            |(
                _k,
                EdocHeader {
                    kind,
                    range,
                    form,
                    doc,
                    params,
                    returns,
                },
            )| {
                buf.push_str(format!("{}\n", form.syntax_ptr_string()).as_str());
                buf.push_str(format!("{:?}:{:?}\n", kind, range).as_str());
                if let Some(doc) = doc {
                    if !doc.lines.is_empty() {
                        buf.push_str(format!("  doc\n").as_str());
                        doc.lines.iter().for_each(|line| {
                            if let Some(text) = &line.text {
                                buf.push_str(
                                    format!("    {:?}: \"{}\"\n", line.syntax.range(), text)
                                        .as_str(),
                                );
                            }
                        });
                    }
                }
                if !params.is_empty() {
                    buf.push_str(format!("  params\n").as_str());
                    params.iter().for_each(
                        |(
                            _name,
                            Param {
                                name,
                                description,
                                syntax,
                            },
                        )| {
                            buf.push_str(format!("    {}\n", name).as_str());
                            buf.push_str(
                                format!("      {:?}: \"{}\"\n", syntax.range(), description)
                                    .as_str(),
                            );
                        },
                    );
                }
                if let Some(Returns { text, syntax }) = returns {
                    buf.push_str(format!("  returns\n").as_str());
                    buf.push_str(format!("    {:?}: \"{}\"\n", syntax.range(), text).as_str());
                }
            },
        );
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
        .assert_debug_eq(&extract_edoc_tag("%% @foo bar"));
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
                Function:0..129
                  doc
                    0..12: "blah"
                  params
                    Foo
                      13..52: "${2:Argument description}"
                    Arg2
                      53..93: "${3:Argument description}"
                  returns
                    94..129: "${4:Return description}"
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
                Function:25..123
                  params
                    Foo
                      25..64: "${2:Argument description}"
                  returns
                    88..123: "${4:Return description}"
            "#]],
        )
    }

    #[test]
    fn edoc_must_be_alone_on_line() {
        check(
            r#"
                bar() -> ok. %% @doc not an edoc comment
                %% @param Foo This is a valid edoc comment
                foo(Foo, some_atom) -> ok.
"#,
            expect![[r#"
                SyntaxNodePtr { range: 84..110, kind: FUN_DECL }
                Function:41..83
                  params
                    Foo
                      41..83: "This is a valid edoc comment"
            "#]],
        )
    }

    #[test]
    fn edoc_ignores_insignificant_forms() {
        check(
            r#"
                %% @doc is an edoc comment
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
                Function:0..473
                  doc
                    0..26: "is an edoc comment"
                    449..473: "Part of the same edoc"
            "#]],
        )
    }

    #[test]
    fn edoc_module_attribute() {
        check(
            r#"
                %% @doc is an edoc comment
                -module(foo).
"#,
            expect![[r#"
                SyntaxNodePtr { range: 27..40, kind: MODULE_ATTRIBUTE }
                Module:0..26
                  doc
                    0..26: "is an edoc comment"
            "#]],
        )
    }

    #[test]
    fn edoc_multiple() {
        check(
            r#"
                %% @doc is an edoc comment
                -module(foo).

                %% @doc fff is ...
                fff() -> ok.

                %% @doc This will be ignored
"#,
            // Note: if you update the grammar, the order of these nodes
            // may change.
            expect![[r#"
                SyntaxNodePtr { range: 27..40, kind: MODULE_ATTRIBUTE }
                Module:0..26
                  doc
                    0..26: "is an edoc comment"
                SyntaxNodePtr { range: 61..73, kind: FUN_DECL }
                Function:42..60
                  doc
                    42..60: "fff is ..."
            "#]],
        )
    }

    #[test]
    fn edoc_end() {
        check(
            r#"
                %% @doc First line
                %%      Second line
                %% @end
                %% ---------
                %% % @format
                -module(main).
                f() -> ok.
"#,
            expect![[r#"
                SyntaxNodePtr { range: 73..87, kind: MODULE_ATTRIBUTE }
                Module:0..72
                  doc
                    0..18: "First line"
                    19..38: "Second line"
            "#]],
        )
    }

    #[test]
    fn edoc_solid() {
        check(
            r#"
                %% Foo
                %% @doc Bar
                %% Baz
                -module(main).
                f() -> ok.
"#,
            expect![[r#"
                SyntaxNodePtr { range: 26..40, kind: MODULE_ATTRIBUTE }
                Module:7..25
                  doc
                    7..18: "Bar"
                    19..25: "Baz"
            "#]],
        )
    }
}

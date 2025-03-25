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

use std::borrow::Cow;
use std::sync::Arc;
use std::sync::LazyLock;

use elp_base_db::FileId;
use elp_syntax::ast;
use elp_syntax::ast::Form;
use elp_syntax::AstNode;
use elp_syntax::AstPtr;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use fxhash::FxHashMap;
use htmlentity::entity::ICodedDataTrait;
use regex::Regex;

use crate::db::DefDatabase;
use crate::FunctionDef;
use crate::InFileAstPtr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdocHeader {
    pub kind: EdocHeaderKind,
    pub doc: Option<Tag>,
    pub params: FxHashMap<String, Tag>,
    pub returns: Option<Tag>,
    pub equiv: Option<Tag>,
    pub authors: Vec<Tag>,
    pub copyright: Option<Tag>,
    pub plaintext_copyright: Option<String>,
    pub copyright_prefix: Option<String>,
    pub unknown: Vec<(String, Tag)>,
    pub ranges: Vec<TextRange>,
}

impl EdocHeader {
    pub fn start(&self) -> Option<TextSize> {
        self.ranges.first().map(|range| range.start())
    }

    pub fn comments(&self) -> impl Iterator<Item = &InFileAstPtr<ast::Comment>> {
        self.doc
            .iter()
            .chain(self.params.values())
            .chain(&self.returns)
            .chain(&self.equiv)
            .chain(&self.authors)
            .chain(&self.copyright)
            .chain(self.unknown.iter().map(|(_, tag)| tag))
            .flat_map(|tag| tag.lines.iter().map(|line| &line.syntax))
    }

    pub fn copyright_comment(&self) -> Option<String> {
        let copyright = self.copyright.clone()?;
        let copyright_prefix = self.copyright_prefix.clone()?;
        if let Some(plaintext_copyright) = &self.plaintext_copyright {
            if plaintext_copyright.contains(&copyright.description()) {
                return None;
            }
        }
        Some(format!(
            "{copyright_prefix} Copyright {}\n",
            copyright.description()
        ))
    }

    pub fn to_eep59(&self) -> String {
        let mut res = String::new();
        for author in &self.authors {
            res.push_str(&format!("-author(\"{}\").\n", author.description()));
        }
        if let Some(doc) = &self.doc {
            let prefix = match self.kind {
                EdocHeaderKind::Module => "-moduledoc",
                EdocHeaderKind::Function => "-doc",
            };
            res.push_str(&format!("{prefix} \"\"\"\n"));
            if let Some(text) = doc.to_markdown() {
                res.push_str(&text);
            }
            if let Some(returns) = &self.returns {
                res.push_str("*Returns:* ");
                if let Some(text) = returns.to_markdown() {
                    res.push_str(&text);
                }
            }
            for (name, tag) in &self.unknown {
                let name = capitalize_first_char(name).unwrap_or(name.to_string());
                res.push_str(&format!("  *{}:* ", name));
                if let Some((head, tail)) = &tag.lines.split_first() {
                    if let Some(text) = &head.content {
                        res.push_str(&format!("{text}\n"));
                    }
                    for line in tail.iter() {
                        if let Some(text) = line.to_markdown() {
                            res.push_str(&format!("  {}", text));
                        }
                    }
                }
            }
            res.push_str(&format!("\"\"\".\n"));
        }
        let mut metadata = String::new();
        for (name, param) in &self.params {
            let mut description = String::new();
            for line in &param.lines {
                if let Some(content) = line.to_markdown() {
                    description.push_str(&format!("{} ", &content.trim()));
                }
            }
            metadata.push_str(&format!("\"{name}\" => \"{}\", ", description.trim()));
        }
        if let Some(equiv) = &self.equiv {
            if let Some(equivalent_to) = equiv.to_markdown() {
                metadata.push_str(&format!("equiv => {}, ", equivalent_to.trim()));
            }
        }
        if !metadata.is_empty() {
            res.push_str(&format!("-doc #{{{}}}.\n", metadata.trim_end_matches(", ")));
        }
        res
    }
}

fn capitalize_first_char(word: &str) -> Option<String> {
    let mut chars = word.chars();
    chars
        .next()
        .map(|char| char.to_uppercase().to_string() + &chars.as_str())
}

fn decode_html_entities(text: &str) -> Cow<str> {
    let decoded = htmlentity::entity::decode(text.as_bytes());
    if decoded.entity_count() == 0 {
        Cow::Borrowed(text)
    } else {
        match decoded.to_string() {
            Ok(decoded) => Cow::Owned(decoded),
            Err(_) => Cow::Borrowed(text),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EdocHeaderKind {
    Module,
    Function,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tag {
    pub lines: Vec<Line>,
    pub range: TextRange,
}

impl Tag {
    pub fn description(&self) -> String {
        let mut res = String::new();
        for line in &self.lines {
            if let Some(content) = &line.content {
                res.push_str(&format!("{}", content))
            };
        }
        res
    }
    pub fn to_markdown(&self) -> Option<String> {
        let mut res = String::new();
        for line in &self.lines {
            if let Some(text) = line.to_markdown() {
                res.push_str(&text);
            }
        }
        ensure_non_empty(&res)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Line {
    pub content: Option<String>,
    pub syntax: InFileAstPtr<ast::Comment>,
}

impl Line {
    pub fn to_markdown(&self) -> Option<String> {
        let content = self.content.clone()?;
        Some(format!("{}\n", convert_to_markdown(&content).to_string()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TagName {
    kind: TagKind,
    range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TagKind {
    Doc,
    Returns,
    Param(String),
    Equiv,
    Author,
    Copyright,
    Unknown(String),
}

pub fn file_edoc_comments_query(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> Option<Arc<FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>>> {
    let mut res = FxHashMap::default();
    if let Some((form, header)) = module_doc_header(db, file_id) {
        res.insert(form, header);
    }
    let def_map = db.def_map_local(file_id);
    for (_name_arity, def) in def_map.get_functions() {
        if let Some((form, header)) = function_doc_header(db, file_id, def) {
            res.insert(form, header);
        }
    }
    Some(Arc::new(res))
}

fn module_doc_header(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> Option<(InFileAstPtr<ast::Form>, EdocHeader)> {
    let form_list = db.file_form_list(file_id);
    let module_attribute = form_list.module_attribute()?;
    let ast = module_attribute.form_id.get_ast(db, file_id);
    let syntax = ast.syntax();
    let form = ast::Form::cast(syntax.clone())?;
    edoc_header(file_id, &form, &syntax, EdocHeaderKind::Module)
}

fn function_doc_header(
    db: &dyn DefDatabase,
    file_id: FileId,
    def: &FunctionDef,
) -> Option<(InFileAstPtr<ast::Form>, EdocHeader)> {
    let decls = def.source(db.upcast());
    let decl = decls.first()?;
    let form = ast::Form::cast(decl.syntax().clone())?;
    let syntax = form.syntax();
    spec_doc_header(db, file_id, &def, &form).or(edoc_header(
        file_id,
        &form,
        syntax,
        EdocHeaderKind::Function,
    ))
}

fn spec_doc_header(
    db: &dyn DefDatabase,
    file_id: FileId,
    def: &FunctionDef,
    form: &Form,
) -> Option<(InFileAstPtr<ast::Form>, EdocHeader)> {
    let spec_def = def.spec.clone()?;
    let spec = spec_def.source(db.upcast());
    let spec_syntax = spec.syntax();
    edoc_header(file_id, &form, spec_syntax, EdocHeaderKind::Function)
}

fn edoc_header(
    file_id: FileId,
    form: &ast::Form,
    syntax: &SyntaxNode,
    kind: EdocHeaderKind,
) -> Option<(InFileAstPtr<ast::Form>, EdocHeader)> {
    let mut comments: Vec<_> = prev_form_nodes(syntax)
        .filter_map(ast::Comment::cast)
        .filter(|comment| only_comment_on_line(comment))
        .collect();
    comments.reverse();

    let form = InFileAstPtr::new(file_id, AstPtr::new(form));
    Some((form, parse_edoc(kind, form, &comments)?))
}

#[derive(Debug, Default)]
struct ParseContext {
    ranges: Vec<TextRange>,
    current_tag: Option<TagName>,
    lines: Vec<Line>,
    plaintext_copyright: Option<String>,
    copyright_prefix: Option<String>,

    doc: Option<Tag>,
    returns: Option<Tag>,
    params: FxHashMap<String, Tag>,
    equiv: Option<Tag>,
    authors: Vec<Tag>,
    copyright: Option<Tag>,
    unknown: Vec<(String, Tag)>,
}

impl ParseContext {
    fn add_line(&mut self, line: Line) {
        self.lines.push(line);
    }
    fn start_tag(
        &mut self,
        kind: TagKind,
        range: TextRange,
        content: &str,
        comment: &ast::Comment,
        syntax: InFileAstPtr<ast::Comment>,
    ) {
        let tag = TagName {
            kind,
            range: range + comment.syntax().text_range().start(),
        };
        self.add_line(Line {
            content: ensure_non_empty(content),
            syntax,
        });
        self.current_tag = Some(tag);
    }
    fn end_tag(&mut self, content: &str, syntax: InFileAstPtr<ast::Comment>) {
        self.add_line(Line {
            content: ensure_non_empty(content),
            syntax,
        });
    }
    fn process_tag(&mut self) {
        match &self.current_tag {
            Some(tag_name) => {
                match &tag_name.kind {
                    TagKind::Doc => {
                        self.doc = Some(Tag {
                            lines: self.lines.clone(),
                            range: tag_name.range,
                        });
                    }
                    TagKind::Returns => {
                        self.returns = Some(Tag {
                            lines: self.lines.clone(),
                            range: tag_name.range,
                        });
                    }
                    TagKind::Param(name) => {
                        self.params.insert(
                            name.to_string(),
                            Tag {
                                lines: self.lines.clone(),
                                range: tag_name.range,
                            },
                        );
                    }
                    TagKind::Equiv => {
                        self.equiv = Some(Tag {
                            lines: self.lines.clone(),
                            range: tag_name.range,
                        });
                    }
                    TagKind::Author => {
                        self.authors.push(Tag {
                            lines: self.lines.clone(),
                            range: tag_name.range,
                        });
                    }
                    TagKind::Copyright => {
                        self.copyright = Some(Tag {
                            lines: self.lines.clone(),
                            range: tag_name.range,
                        });
                    }
                    TagKind::Unknown(unknown) => {
                        self.unknown.push((
                            unknown.to_string(),
                            Tag {
                                lines: self.lines.clone(),
                                range: tag_name.range,
                            },
                        ));
                    }
                }
                self.current_tag = None;
                self.lines = vec![];
            }
            None => (),
        }
    }
    fn to_edoc_header(self, kind: EdocHeaderKind) -> Option<EdocHeader> {
        if self.doc.is_none()
            && self.returns.is_none()
            && self.params.is_empty()
            && self.equiv.is_none()
            && self.unknown.is_empty()
        {
            return None;
        }
        Some(EdocHeader {
            kind,
            ranges: self.ranges,
            plaintext_copyright: self.plaintext_copyright,
            doc: self.doc,
            params: self.params,
            returns: self.returns,
            equiv: self.equiv,
            authors: self.authors,
            copyright: self.copyright,
            copyright_prefix: self.copyright_prefix,
            unknown: self.unknown,
        })
    }
}

fn ensure_non_empty(text: &str) -> Option<String> {
    if text.trim().is_empty() {
        None
    } else {
        Some(text.to_string())
    }
}

fn parse_edoc(
    kind: EdocHeaderKind,
    form: InFileAstPtr<ast::Form>,
    comments: &[ast::Comment],
) -> Option<EdocHeader> {
    let mut context = ParseContext::default();

    static COPYRIGHT_RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"^%+\s+Copyright ?(.*)$").unwrap());

    for comment in comments {
        let text = comment.syntax().text().to_string();
        let syntax = InFileAstPtr::new(form.file_id(), AstPtr::new(comment));
        match extract_edoc_tag_and_content(&text) {
            None => {
                if let Some(captures) = COPYRIGHT_RE.captures(&text) {
                    if let Some(content) = captures.get(1) {
                        context.plaintext_copyright = Some(content.as_str().to_string());
                    }
                }
                if let Some(_tag) = &context.current_tag {
                    context.ranges.push(comment.syntax().text_range());
                    let content = text.trim_start_matches(|c: char| c == '%' || c.is_whitespace());
                    context.add_line(Line {
                        content: Some(content.to_string()),
                        syntax,
                    });
                }
            }
            Some((range, prefix, tag, content)) => {
                if tag != "end" {
                    context.process_tag();
                }
                context.ranges.push(comment.syntax().text_range());
                match tag {
                    "doc" => {
                        context.start_tag(TagKind::Doc, range, content, comment, syntax);
                    }
                    "returns" => {
                        context.start_tag(TagKind::Returns, range, content, comment, syntax);
                    }
                    "param" => {
                        if let Some((name, content)) = content.split_once(" ") {
                            context.start_tag(
                                TagKind::Param(name.to_string()),
                                range,
                                content,
                                comment,
                                syntax,
                            );
                        }
                    }
                    "equiv" => {
                        context.start_tag(TagKind::Equiv, range, content, comment, syntax);
                    }
                    "author" => {
                        context.start_tag(TagKind::Author, range, content, comment, syntax);
                    }
                    "copyright" => {
                        context.copyright_prefix = Some(prefix.to_string());
                        context.start_tag(TagKind::Copyright, range, content, comment, syntax);
                    }
                    "end" => {
                        context.end_tag(content, syntax);
                        context.process_tag();
                    }
                    unknown => {
                        context.start_tag(
                            TagKind::Unknown(unknown.to_string()),
                            range,
                            content,
                            comment,
                            syntax,
                        );
                    }
                }
            }
        }
    }
    context.process_tag();

    context.to_edoc_header(kind)
}

/// An edoc comment must be alone on a line, it cannot come after
/// code.
fn only_comment_on_line(comment: &ast::Comment) -> bool {
    // We check for a positive "other" found on the same line, in case
    // the comment is the first line of the file, which will not have
    // a preceding newline.
    let mut node = comment
        .syntax()
        .siblings_with_tokens(elp_syntax::Direction::Prev)
        .skip(1); // Starts with itself

    while let Some(node) = node.next() {
        if let Some(tok) = node.into_token() {
            if tok.kind() == SyntaxKind::WHITESPACE && tok.text().contains('\n') {
                return true;
            }
        } else {
            return false;
        }
    }
    true
}

fn prev_form_nodes(syntax: &SyntaxNode) -> impl Iterator<Item = SyntaxNode> {
    syntax
        .siblings(elp_syntax::Direction::Prev)
        .skip(1) // Starts with itself
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
fn extract_edoc_tag_and_content(comment: &str) -> Option<(TextRange, &str, &str, &str)> {
    static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^(%+)\s+@([^\s]+) ?(.*)$").unwrap());
    let captures = RE.captures(comment)?;
    let prefix = captures.get(1)?;
    let tag = captures.get(2)?;
    // add the leading @ to the range
    let start = TextSize::new((tag.start() - 1) as u32);
    let range = TextRange::new(start, TextSize::new(tag.end() as u32));
    Some((
        range,
        prefix.as_str(),
        tag.as_str(),
        captures.get(3)?.as_str(),
    ))
}

fn convert_single_quotes(comment: &str) -> Cow<str> {
    static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"`([^']*)'").unwrap());
    RE.replace_all(comment, "`$1`")
}

fn convert_triple_quotes(comment: &str) -> Cow<str> {
    static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"'''[']*").unwrap());
    RE.replace_all(comment, "```")
}

fn convert_to_markdown(text: &str) -> String {
    convert_single_quotes(&convert_triple_quotes(&decode_html_entities(text))).to_string()
}

#[cfg(test)]
mod tests {
    use elp_base_db::fixture::WithFixture;
    use elp_syntax::ast;
    use expect_test::expect;
    use expect_test::Expect;
    use fxhash::FxHashMap;

    use super::*;
    use crate::test_db::TestDB;
    use crate::InFileAstPtr;

    fn test_print(edoc: &FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>) -> String {
        let mut buf = String::default();
        edoc.iter().for_each(
            |(
                _k,
                EdocHeader {
                    kind,
                    doc,
                    params,
                    returns,
                    equiv,
                    ..
                },
            )| {
                buf.push_str(&format!("{:?}\n", kind));
                if let Some(doc) = doc {
                    if !doc.lines.is_empty() {
                        buf.push_str("  doc\n");
                        doc.lines.iter().for_each(|line| {
                            if let Some(text) = &line.content {
                                buf.push_str(&format!(
                                    "    {:?}: \"{}\"\n",
                                    line.syntax.range(),
                                    text
                                ));
                            }
                        });
                    }
                }
                if !params.is_empty() {
                    buf.push_str("  params\n");
                    params.iter().for_each(|(name, param)| {
                        buf.push_str(&format!("    {name}\n"));
                        if !param.lines.is_empty() {
                            param.lines.iter().for_each(|line| {
                                if let Some(text) = &line.content {
                                    buf.push_str(&format!(
                                        "      {:?}: \"{}\"\n",
                                        line.syntax.range(),
                                        text
                                    ));
                                }
                            });
                        }
                    });
                }
                if let Some(Tag { lines, .. }) = returns {
                    if !lines.is_empty() {
                        buf.push_str("  returns\n");
                        lines.iter().for_each(|line| {
                            if let Some(text) = &line.content {
                                buf.push_str(&format!(
                                    "    {:?}: \"{}\"\n",
                                    line.syntax.range(),
                                    text
                                ));
                            }
                        });
                    }
                }
                if let Some(Tag { lines, .. }) = equiv {
                    if !lines.is_empty() {
                        buf.push_str("  equiv\n");
                        lines.iter().for_each(|line| {
                            if let Some(text) = &line.content {
                                buf.push_str(&format!(
                                    "    {:?}: \"{}\"\n",
                                    line.syntax.range(),
                                    text
                                ));
                            }
                        });
                    }
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
        expected.assert_eq(&test_print(&*edocs.unwrap()))
    }

    #[test]
    fn test_contains_annotation() {
        expect![[r#"
            Some(
                (
                    3..7,
                    "%%",
                    "foo",
                    "bar",
                ),
            )
        "#]]
        .assert_debug_eq(&extract_edoc_tag_and_content("%% @foo bar"));
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
                Function
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
                Function
                  params
                    Foo
                      25..64: "${2:Argument description}"
                      65..87: "Does not have a tag"
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
                Function
                  params
                    Foo
                      41..83: "This is a valid edoc comment"
            "#]],
        )
    }

    #[test]
    fn edoc_just_end() {
        check(
            r#"
                %% @end
                bar() -> ok.
"#,
            expect![""],
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
                Function
                  doc
                    0..26: "is an edoc comment"
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
                Module
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
                Module
                  doc
                    0..26: "is an edoc comment"
                Function
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
                Module
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
                Module
                  doc
                    7..18: "Bar"
                    19..25: "Baz"
            "#]],
        )
    }

    #[test]
    fn edoc_incorrect_usage_on_type() {
        check(
            r#"
                -module(main).
                -export([main/2]).
                -export_type([my_integer/0]).

                %% @d~oc This is an incorrect type doc
                -type my_integer() :: integer().

                -type my_integer2() :: integer().

                %% @doc These are docs for the main function
                -spec main(any(), any()) -> ok.
                main(A, B) ->
                    dep().

                dep() -> ok.
"#,
            expect![[r#"
                Function
                  doc
                    172..216: "These are docs for the main function"
            "#]],
        )
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
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
use elp_syntax::AstNode;
use elp_syntax::AstPtr;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use elp_syntax::ast;
use elp_syntax::ast::Form;
use fxhash::FxHashMap;
use htmlentity::entity::ICodedDataTrait;
use itertools::Itertools;
use regex::Regex;
use stdx::trim_indent;

use crate::FunctionDef;
use crate::InFileAstPtr;
use crate::db::DefDatabase;
use crate::form_list::DocAttributeId;

#[allow(clippy::large_enum_variant)]
pub enum FunctionDoc {
    EdocHeader(EdocHeader),
    DocAttributeId(DocAttributeId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdocHeader {
    pub kind: EdocHeaderKind,
    pub doc: Option<Tag>,
    pub params: Vec<(String, Tag)>,
    pub returns: Option<Tag>,
    pub deprecated: Option<Tag>,
    pub equiv: Option<Tag>,
    pub authors: Vec<Tag>,
    pub copyright: Option<Tag>,
    pub hidden: Option<Tag>,
    pub sees: Vec<Tag>,
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
            .chain(self.params.iter().map(|(_name, tag)| tag))
            .chain(&self.returns)
            .chain(&self.deprecated)
            .chain(&self.equiv)
            .chain(&self.authors)
            .chain(&self.copyright)
            .chain(&self.hidden)
            .chain(&self.sees)
            .chain(self.unknown.iter().map(|(_, tag)| tag))
            .flat_map(|tag| tag.lines.iter().map(|line| &line.syntax))
            .sorted_by(|a, b| a.range().range.start().cmp(&b.range().range.start()))
    }
}

fn decode_html_entities(text: &str) -> Cow<'_, str> {
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

fn is_divider(text: &str) -> bool {
    static RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"^%*\s*-+$").expect("regex should be valid"));
    RE.is_match(text)
}

fn reference_to_exdoc(text: &str) -> String {
    if text.contains('/') {
        text.to_string()
    } else {
        format!("m:{text}")
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
                res.push_str(&content.to_string())
            };
        }
        res
    }
    pub fn to_markdown(&self) -> Option<String> {
        let mut res = String::new();
        let (head, tail) = self.lines.split_first()?;
        let head = head.to_markdown().unwrap_or("".to_string());
        for line in tail {
            if let Some(text) = line.to_markdown() {
                res.push_str(&text.to_string());
            }
        }
        ensure_non_empty(&convert_link_macros(&format!(
            "{}{}",
            head,
            trim_indent(&res)
        )))
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
        Some(format!("{}\n", convert_to_markdown(&content)))
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
    Param(Option<String>),
    Deprecated,
    Equiv,
    Author,
    Copyright,
    Hidden,
    See,
    Unknown(String),
}

pub fn file_edoc_comments_dispatch(
    db: &dyn DefDatabase,
    file_id: FileId,
) -> Option<Arc<FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>>> {
    db.file_edoc_comments_interned(elp_base_db::InternedFileId::new(db, file_id))
}

pub fn file_edoc_comments_inner(
    db: &dyn DefDatabase,
    fid: elp_base_db::InternedFileId,
) -> Option<Arc<FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>>> {
    let file_id = fid.file_id(db);
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
    edoc_header(file_id, &form, syntax, EdocHeaderKind::Module)
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
    spec_doc_header(db, file_id, def, &form).or(edoc_header(
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
    edoc_header(file_id, form, spec_syntax, EdocHeaderKind::Function)
}

fn edoc_header(
    file_id: FileId,
    form: &ast::Form,
    syntax: &SyntaxNode,
    kind: EdocHeaderKind,
) -> Option<(InFileAstPtr<ast::Form>, EdocHeader)> {
    let mut comments: Vec<_> = prev_form_nodes(syntax)
        .filter_map(ast::Comment::cast)
        .filter(only_comment_on_line)
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

    doc: Option<Tag>,
    returns: Option<Tag>,
    params: Vec<(String, Tag)>,
    deprecated: Option<Tag>,
    equiv: Option<Tag>,
    authors: Vec<Tag>,
    copyright: Option<Tag>,
    hidden: Option<Tag>,
    sees: Vec<Tag>,
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
    fn update_current_tag(&mut self, tag: Option<TagName>) {
        self.current_tag = tag
    }
    fn process_tag(&mut self) {
        if let Some(last_comment) = self.lines.last()
            && let Some(content) = &last_comment.content
            && is_divider(content)
        {
            _ = self.lines.pop();
        }
        if let Some(tag_name) = &self.current_tag {
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
                    let name = name.clone().unwrap_or_default();
                    self.params.push((
                        name,
                        Tag {
                            lines: self.lines.clone(),
                            range: tag_name.range,
                        },
                    ));
                }
                TagKind::Deprecated => {
                    self.deprecated = Some(Tag {
                        lines: self.lines.clone(),
                        range: tag_name.range,
                    });
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
                TagKind::Hidden => {
                    self.hidden = Some(Tag {
                        lines: self.lines.clone(),
                        range: tag_name.range,
                    });
                }
                TagKind::See => {
                    self.sees.push(Tag {
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
    }
    fn into_edoc_header(self, kind: EdocHeaderKind) -> Option<EdocHeader> {
        if self.doc.is_none()
            && self.returns.is_none()
            && self.params.is_empty()
            && self.deprecated.is_none()
            && self.equiv.is_none()
            && self.hidden.is_none()
            && self.sees.is_empty()
            && self.unknown.is_empty()
        {
            return None;
        }
        Some(EdocHeader {
            kind,
            ranges: self.ranges,
            doc: self.doc,
            params: self.params,
            returns: self.returns,
            deprecated: self.deprecated,
            equiv: self.equiv,
            authors: self.authors,
            copyright: self.copyright,
            hidden: self.hidden,
            sees: self.sees,
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

    for comment in comments {
        let text = comment.syntax().text().to_string();
        let syntax = InFileAstPtr::new(form.file_id(), AstPtr::new(comment));
        match extract_edoc_tag_and_content(&text) {
            None => {
                if let Some(tag) = &context.current_tag {
                    context.ranges.push(comment.syntax().text_range());
                    let content = text.trim_start_matches('%');
                    if tag.kind == TagKind::Param(None) {
                        match extract_param_name_and_content(content.trim()) {
                            None => {
                                context.add_line(Line {
                                    content: Some(content.to_string()),
                                    syntax,
                                });
                            }
                            Some((name, content)) => {
                                let current_tag = TagName {
                                    kind: TagKind::Param(Some(name.to_string())),
                                    range: tag.range,
                                };
                                context.update_current_tag(Some(current_tag));
                                context.add_line(Line {
                                    content: Some(content.to_string()),
                                    syntax,
                                });
                            }
                        }
                    } else {
                        context.add_line(Line {
                            content: Some(content.to_string()),
                            syntax,
                        });
                    }
                }
            }
            Some((range, tag, content)) => {
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
                    "param" => match extract_param_name_and_content(content) {
                        None => {
                            context.start_tag(TagKind::Param(None), range, content, comment, syntax)
                        }
                        Some((name, content)) => {
                            context.start_tag(
                                TagKind::Param(Some(name.to_string())),
                                range,
                                content,
                                comment,
                                syntax,
                            );
                        }
                    },
                    "deprecated" => {
                        context.start_tag(TagKind::Deprecated, range, content, comment, syntax);
                    }
                    "equiv" => {
                        context.start_tag(TagKind::Equiv, range, content, comment, syntax);
                    }
                    "author" => {
                        context.start_tag(TagKind::Author, range, content, comment, syntax);
                    }
                    "copyright" => {
                        context.start_tag(TagKind::Copyright, range, content, comment, syntax);
                    }
                    "end" => {
                        context.end_tag(content, syntax);
                        context.process_tag();
                    }
                    "hidden" | "private" => {
                        context.start_tag(TagKind::Hidden, range, content, comment, syntax);
                    }
                    "see" => {
                        context.start_tag(TagKind::See, range, content, comment, syntax);
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

    context.into_edoc_header(kind)
}

fn extract_param_name_and_content(content: &str) -> Option<(&str, &str)> {
    if content.is_empty() {
        None
    } else if let Some((name, content)) = content.split_once(" ") {
        Some((
            name,
            content
                .trim_start_matches(is_param_name_separator)
                .trim_start(),
        ))
    } else {
        Some((content, ""))
    }
}

fn is_param_name_separator(c: char) -> bool {
    c == ':' || c == '-' || c == ','
}

/// An edoc comment must be alone on a line, it cannot come after
/// code.
fn only_comment_on_line(comment: &ast::Comment) -> bool {
    // We check for a positive "other" found on the same line, in case
    // the comment is the first line of the file, which will not have
    // a preceding newline.
    let node = comment
        .syntax()
        .siblings_with_tokens(elp_syntax::Direction::Prev)
        .skip(1); // Starts with itself

    for node in node {
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

fn prev_form_nodes(syntax: &SyntaxNode) -> impl Iterator<Item = SyntaxNode> + use<> {
    syntax
        .siblings(elp_syntax::Direction::Prev)
        .skip(1) // Starts with itself
        .take_while(|node| edoc_header_kind(node).is_none())
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
fn extract_edoc_tag_and_content(comment: &str) -> Option<(TextRange, &str, &str)> {
    static RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"^%+\s+@([^\s]+) ?(.*)$").expect("regex should be valid"));
    let captures = RE.captures(comment)?;
    let tag = captures.get(1)?;
    // add the leading @ to the range
    let start = TextSize::new((tag.start() - 1) as u32);
    let range = TextRange::new(start, TextSize::new(tag.end() as u32));
    Some((range, tag.as_str(), captures.get(2)?.as_str()))
}

fn convert_single_quotes(comment: &str) -> Cow<'_, str> {
    static RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"`([^']*)'").expect("regex should be valid"));
    RE.replace_all(comment, "`$1`")
}

fn convert_triple_quotes(comment: &str) -> Cow<'_, str> {
    static RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"'''[']*").expect("regex should be valid"));
    RE.replace_all(comment, "```")
}

fn convert_link_macros(comment: &str) -> Cow<'_, str> {
    static RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"\{@link\s+([^\s]+)\}").expect("regex should be valid"));
    RE.replace_all(comment, |captures: &regex::Captures<'_>| {
        if let Some(m) = captures.get(1) {
            format!("`{}`", reference_to_exdoc(m.as_str()))
        } else {
            "".to_string()
        }
    })
}

fn convert_to_markdown(text: &str) -> String {
    convert_single_quotes(&convert_triple_quotes(&decode_html_entities(text))).to_string()
}

#[cfg(test)]
mod tests {
    use elp_base_db::fixture::WithFixture;
    use elp_syntax::ast;
    use expect_test::Expect;
    use expect_test::expect;
    use fxhash::FxHashMap;

    use super::*;
    use crate::InFileAstPtr;
    use crate::test_db::TestDB;

    fn test_print(edoc: &FxHashMap<InFileAstPtr<ast::Form>, EdocHeader>) -> String {
        let mut buf = String::default();
        let mut edocs: Vec<_> = edoc.iter().collect();
        edocs.sort_by_key(|(k, _)| k.range().range.start());
        edocs.iter().for_each(
            |(
                _k,
                EdocHeader {
                    kind,
                    doc,
                    params,
                    returns,
                    deprecated,
                    equiv,
                    hidden,
                    ..
                },
            )| {
                buf.push_str(&format!("{kind:?}\n"));
                if let Some(doc) = doc
                    && !doc.lines.is_empty()
                {
                    buf.push_str("  doc\n");
                    doc.lines.iter().for_each(|line| {
                        if let Some(text) = &line.content {
                            buf.push_str(&format!(
                                "    {:?}: \"{}\"\n",
                                line.syntax.range().range,
                                text
                            ));
                        }
                    });
                }
                if let Some(Tag { lines, .. }) = deprecated
                    && !lines.is_empty()
                {
                    buf.push_str("  deprecated\n");
                    lines.iter().for_each(|line| {
                        if let Some(text) = &line.content {
                            buf.push_str(&format!(
                                "    {:?}: \"{}\"\n",
                                line.syntax.range().range,
                                text
                            ));
                        }
                    });
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
                                        line.syntax.range().range,
                                        text
                                    ));
                                }
                            });
                        }
                    });
                }
                if let Some(Tag { lines, .. }) = returns
                    && !lines.is_empty()
                {
                    buf.push_str("  returns\n");
                    lines.iter().for_each(|line| {
                        if let Some(text) = &line.content {
                            buf.push_str(&format!(
                                "    {:?}: \"{}\"\n",
                                line.syntax.range().range,
                                text
                            ));
                        }
                    });
                }
                if let Some(Tag { lines, .. }) = equiv
                    && !lines.is_empty()
                {
                    buf.push_str("  equiv\n");
                    lines.iter().for_each(|line| {
                        if let Some(text) = &line.content {
                            buf.push_str(&format!(
                                "    {:?}: \"{}\"\n",
                                line.syntax.range().range,
                                text
                            ));
                        }
                    });
                }
                if let Some(Tag { .. }) = hidden {
                    buf.push_str("  hidden\n");
                }
            },
        );
        buf
    }

    #[track_caller]
    fn check(fixture: &str, expected: Expect) {
        let (db, fixture) = TestDB::with_fixture(fixture);
        let file_id = fixture.files[0];
        let edocs = file_edoc_comments_dispatch(&db, file_id);
        expected.assert_eq(&test_print(&edocs.unwrap()))
    }

    #[test]
    fn test_contains_annotation() {
        expect![[r#"
            Some(
                (
                    3..7,
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
                      65..87: " Does not have a tag"
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
                -nominal nclient() :: #client{}.
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
                    19..38: "      Second line"
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
                    19..25: " Baz"
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

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
use elp_base_db::SourceDatabase;
use elp_syntax::AstNode;
use elp_syntax::AstPtr;
use elp_syntax::Direction;
use elp_syntax::NodeOrToken;
use elp_syntax::SyntaxElement;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use elp_syntax::algo;
use elp_syntax::ast;
use elp_syntax::ast::Form;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use htmlentity::entity::ICodedDataTrait;
use itertools::Itertools;
use regex::Regex;
use stdx::trim_indent;

use crate::FunctionDef;
use crate::InFileAstPtr;
use crate::db::DefDatabase;
use crate::form_list::DocAttributeId;

pub enum FunctionDoc {
    EdocHeader(EdocHeader),
    DocAttributeId(DocAttributeId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EdocHeader {
    pub kind: EdocHeaderKind,
    pub exported: bool,
    pub doc: Option<Tag>,
    pub params: Vec<(String, Tag)>,
    pub returns: Option<Tag>,
    pub deprecated: Option<Tag>,
    pub equiv: Option<Tag>,
    pub authors: Vec<Tag>,
    pub copyright: Option<Tag>,
    pub plaintext_copyrights: FxHashSet<String>,
    pub copyright_prefix: Option<String>,
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

    pub fn copyright_comment(&self) -> Option<String> {
        let copyright = self.copyright.clone()?;
        let copyright_prefix = self.copyright_prefix.clone()?;
        for plaintext_copyright in &self.plaintext_copyrights {
            if plaintext_copyright.contains(&copyright.description()) {
                return None;
            }
        }
        Some(format!(
            "{copyright_prefix} Copyright {}\n",
            copyright.description()
        ))
    }

    pub fn prev_divider(&self, db: &dyn SourceDatabase) -> Option<SyntaxNode> {
        let first_comment = self.comments().next()?;
        divider(first_comment.to_ast(db).syntax(), Direction::Prev)
    }

    pub fn next_divider(&self, db: &dyn SourceDatabase) -> Option<SyntaxNode> {
        let last_comment = self.comments().last()?;
        divider(last_comment.to_ast(db).syntax(), Direction::Next)
    }

    pub fn to_eep59(&self) -> String {
        let mut res = String::new();
        res.push_str(&self.doc_attribute());
        res.push_str(&self.hidden_attribute());
        res.push_str(&self.metadata_attribute());
        res
    }

    pub fn doc_attribute(&self) -> String {
        let mut res = String::new();
        res.push_str(&self.doc_content());
        res.push_str(&self.return_tag());
        res.push_str(&self.see_tags());
        res.push_str(&self.unknown_tags());

        if !res.is_empty() {
            let prefix = match self.kind {
                EdocHeaderKind::Module => "-moduledoc",
                EdocHeaderKind::Function => "-doc",
            };
            format!("{prefix} \"\"\"\n{}\n\"\"\".\n", res.trim_end())
        } else {
            res
        }
    }

    pub fn doc_content(&self) -> String {
        let mut res = String::new();
        if let Some(doc) = &self.doc {
            if let Some(markdown) = doc.to_markdown() {
                res.push_str(&markdown);
            }
        }
        res
    }

    pub fn return_tag(&self) -> String {
        let mut res = String::new();
        if let Some(returns) = &self.returns {
            res.push_str("### Returns\n");
            if let Some(text) = returns.to_markdown() {
                res.push_str(&text);
            }
        }
        res
    }

    pub fn see_tags(&self) -> String {
        let mut res = String::new();
        for tag in &self.sees {
            if let Some(text) = tag.to_markdown() {
                match wrap_reference_in_backquotes(&text) {
                    Some(wrapped) => res.push_str(&format!("See {wrapped}")),
                    None => res.push_str(&format!("See `{text}`")),
                }
            }
        }
        res
    }

    pub fn unknown_tags(&self) -> String {
        let mut res = String::new();
        for (name, tag) in &self.unknown {
            let name = capitalize_first_char(name).unwrap_or(name.to_string());
            res.push_str(&format!("### {name}\n"));
            if let Some(text) = tag.to_markdown() {
                res.push_str(&text);
            }
        }
        res
    }

    pub fn hidden_attribute(&self) -> String {
        let mut res = String::new();
        let mut bug_9672 = false;
        if self.exported && self.hidden.is_some() {
            let hidden_attribute = match self.kind {
                EdocHeaderKind::Module => "-moduledoc hidden.",
                EdocHeaderKind::Function => "-doc hidden.",
            };
            if let Some(doc) = &self.doc {
                if doc.to_markdown().is_some() {
                    bug_9672 = true;
                }
            }
            if bug_9672 {
                // Due to a bug in OTP, we cannot add the hidden attribute if there's actual documentation
                // https://github.com/erlang/otp/issues/9672
                // If that's the case, add the tag as a comment instead, and link to the bug
                let bug_url = "https://github.com/erlang/otp/issues/9672";
                res.push_str(&format!("%% {hidden_attribute} {bug_url}\n"));
            } else {
                res.push_str(&format!("{hidden_attribute}\n"));
            }
        }
        res
    }

    pub fn params_metadata(&self) -> String {
        let mut res = String::new();
        for (name, param) in &self.params {
            if !name.is_empty() {
                let mut description = String::new();
                for line in &param.lines {
                    if let Some(content) = line.to_markdown() {
                        description.push_str(&format!("{} ", &content.trim()));
                    }
                }
                res.push_str(&format!(
                    "\"{}\" => \"{}\", ",
                    name.trim_end_matches(is_param_name_separator),
                    description.trim().replace("\"", "\\\"")
                ));
            }
        }
        res.trim_end_matches(", ").to_string()
    }

    pub fn metadata_attribute(&self) -> String {
        let mut res = String::new();
        let params_metadata = &self.params_metadata();
        if let Some(deprecated) = &self.deprecated {
            let deprecated_comment = match deprecated.to_markdown() {
                Some(deprecated_comment) => deprecated_comment,
                None => "".to_string(),
            };
            res.push_str(&format!(
                "deprecated => \"{}\", ",
                convert_link_macros(&deprecated_comment).trim()
            ));
        }
        if !params_metadata.is_empty() {
            res.push_str(&format!("params => #{{{}}}, ", params_metadata.trim()));
        }
        if let Some(equiv) = &self.equiv {
            if let Some(equivalent_to) = equiv.to_markdown() {
                res.push_str(&format!("equiv => {}, ", equivalent_to.trim()));
            }
        }
        let prefix = match self.kind {
            EdocHeaderKind::Module => "moduledoc",
            EdocHeaderKind::Function => "doc",
        };
        if !res.is_empty() {
            format!("-{} #{{{}}}.\n", prefix, res.trim_end_matches(", "))
        } else {
            res
        }
    }
}

fn capitalize_first_char(word: &str) -> Option<String> {
    let mut chars = word.chars();
    chars
        .next()
        .map(|char| char.to_uppercase().to_string() + chars.as_str())
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

fn is_divider(text: &str) -> bool {
    static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^%*\s*-+$").unwrap());
    RE.is_match(text)
}

fn divider(syntax: &SyntaxNode, direction: Direction) -> Option<SyntaxNode> {
    if let Some(NodeOrToken::Node(node)) =
        algo::non_whitespace_sibling(NodeOrToken::Node(syntax.clone()), direction)
    {
        if node.kind() == SyntaxKind::COMMENT
            && is_divider(&node.text().to_string())
            && !next_to_empty_line(&node, direction)
        {
            return Some(node);
        }
    }
    None
}

fn next_to_empty_line(syntax: &SyntaxNode, direction: Direction) -> bool {
    let sibling_or_token = match direction {
        Direction::Next => syntax.prev_sibling_or_token(),
        Direction::Prev => syntax.next_sibling_or_token(),
    };
    match sibling_or_token {
        Some(element) => is_empty_line(&element),
        None => false,
    }
}

fn is_empty_line(element: &SyntaxElement) -> bool {
    match element {
        NodeOrToken::Token(token) => token.text() == "\n\n",
        _ => false,
    }
}

fn wrap_reference_in_backquotes(text: &str) -> Option<String> {
    static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"^([^\s.]+)").unwrap());
    let captures = RE.captures(text)?;
    let reference = captures.get(1)?;
    let rest = &text[reference.end()..];
    Some(format!(
        "`{}`{}",
        reference_to_exdoc(reference.as_str()),
        rest
    ))
}

fn reference_to_exdoc(text: &str) -> String {
    if text.contains('/') {
        text.to_string()
    } else {
        format!("m:{}", text)
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
        ensure_non_empty(&format!(
            "{}{}",
            &head,
            &trim_indent(&convert_link_macros(&res))
        ))
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
    edoc_header(file_id, &form, syntax, EdocHeaderKind::Module, true)
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
    let exported = def.exported;
    spec_doc_header(db, file_id, def, &form, exported).or(edoc_header(
        file_id,
        &form,
        syntax,
        EdocHeaderKind::Function,
        exported,
    ))
}

fn spec_doc_header(
    db: &dyn DefDatabase,
    file_id: FileId,
    def: &FunctionDef,
    form: &Form,
    exported: bool,
) -> Option<(InFileAstPtr<ast::Form>, EdocHeader)> {
    let spec_def = def.spec.clone()?;
    let spec = spec_def.source(db.upcast());
    let spec_syntax = spec.syntax();
    edoc_header(
        file_id,
        form,
        spec_syntax,
        EdocHeaderKind::Function,
        exported,
    )
}

fn edoc_header(
    file_id: FileId,
    form: &ast::Form,
    syntax: &SyntaxNode,
    kind: EdocHeaderKind,
    exported: bool,
) -> Option<(InFileAstPtr<ast::Form>, EdocHeader)> {
    let mut comments: Vec<_> = prev_form_nodes(syntax)
        .filter_map(ast::Comment::cast)
        .filter(only_comment_on_line)
        .collect();
    comments.reverse();

    let form = InFileAstPtr::new(file_id, AstPtr::new(form));
    Some((form, parse_edoc(kind, exported, form, &comments)?))
}

#[derive(Debug, Default)]
struct ParseContext {
    exported: bool,
    ranges: Vec<TextRange>,
    current_tag: Option<TagName>,
    lines: Vec<Line>,
    plaintext_copyrights: FxHashSet<String>,
    copyright_prefix: Option<String>,

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
        if let Some(last_comment) = self.lines.last() {
            if let Some(content) = &last_comment.content {
                if is_divider(content) {
                    _ = self.lines.pop();
                }
            }
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
            exported: self.exported,
            ranges: self.ranges,
            plaintext_copyrights: self.plaintext_copyrights,
            doc: self.doc,
            params: self.params,
            returns: self.returns,
            deprecated: self.deprecated,
            equiv: self.equiv,
            authors: self.authors,
            copyright: self.copyright,
            copyright_prefix: self.copyright_prefix,
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
    exported: bool,
    form: InFileAstPtr<ast::Form>,
    comments: &[ast::Comment],
) -> Option<EdocHeader> {
    let mut context = ParseContext {
        exported,
        ..Default::default()
    };

    static COPYRIGHT_RE: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"^%+\s+Copyright ?(.*)$").unwrap());

    for comment in comments {
        let text = comment.syntax().text().to_string();
        let syntax = InFileAstPtr::new(form.file_id(), AstPtr::new(comment));
        match extract_edoc_tag_and_content(&text) {
            None => {
                if let Some(captures) = COPYRIGHT_RE.captures(&text) {
                    if let Some(content) = captures.get(1) {
                        context
                            .plaintext_copyrights
                            .insert(content.as_str().to_string());
                    }
                }
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
                        context.copyright_prefix = Some(prefix.to_string());
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

fn convert_link_macros(comment: &str) -> Cow<str> {
    static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\{@link\s+([^\s]+)\}").unwrap());
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
                buf.push_str(&format!("{:?}\n", kind));
                if let Some(doc) = doc {
                    if !doc.lines.is_empty() {
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
                }
                if let Some(Tag { lines, .. }) = deprecated {
                    if !lines.is_empty() {
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
                if let Some(Tag { lines, .. }) = returns {
                    if !lines.is_empty() {
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
                }
                if let Some(Tag { lines, .. }) = equiv {
                    if !lines.is_empty() {
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
        let edocs = file_edoc_comments_query(&db, file_id);
        expected.assert_eq(&test_print(&edocs.unwrap()))
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

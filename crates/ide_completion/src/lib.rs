/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;

use ctx::CtxKind;
use elp_base_db::FileId;
use elp_ide_db::RootDatabase;
use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::helpers::top_insert_position;
use elp_syntax::AstNode;
use elp_syntax::SourceFile;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use elp_syntax::SyntaxToken;
use elp_syntax::TextSize;
use hir::InFile;
use hir::IncludeAttribute;
use hir::Semantic;
pub use macros::get_include_file;

type DoneFlag = bool;

#[cfg(test)]
mod tests;

mod attributes;
mod ctx;
mod export_functions;
mod export_types;
mod functions;
mod helpers;
mod keywords;
mod macros;
mod maps;
// @fb-only: mod meta_only;
mod modules;
mod records;
mod spec;
mod types;
mod vars;

/*
For token-based completions, this is the maximum number of previous tokens we consider.
*/
static MAX_PREVIOUS_TOKENS_LEN: usize = 16;

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Completion {
    pub label: String,
    pub kind: Kind,
    pub contents: Contents,
    // The position is used in the 'resolve' phase to look for documentation
    pub position: Option<FilePosition>,
    pub sort_text: Option<String>,
    pub deprecated: bool,
    pub additional_edit: Option<(FilePosition, IncludeFile)>,
}

impl fmt::Display for Completion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let deprecated = match self.deprecated {
            true => format!(", deprecated:{}", self.deprecated),
            false => "".to_string(),
        };
        let include = match &self.additional_edit {
            Some((file_pos, include)) => format!(
                ", include:{:?}:{:?}",
                &file_pos.offset,
                include.as_attribute().trim_end()
            ),
            None => "".to_string(),
        };
        let sort = match &self.sort_text {
            Some(st) => format!(", sort_text:{}", st),
            None => "".to_string(),
        };
        write!(
            f,
            "{{label:{}, kind:{:?}, contents:{:?}, position:{:?}{}{}{}}}",
            self.label, self.kind, self.contents, self.position, deprecated, include, sort,
        )
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum Contents {
    SameAsLabel,
    String(String),
    Snippet(String),
}

/// More erlangy version of `lsp_types::completion::CompletionItemKind`
#[derive(Copy, Clone, Hash, PartialEq, Eq, Debug)]
pub enum Kind {
    Function,
    Keyword,
    Module,
    RecordField,
    Type,
    Behavior,
    Macro,
    #[allow(dead_code)] // TODO: T126083972
    Operator,
    #[allow(dead_code)] // TODO: T126083980
    Record,
    Variable,
    Attribute,
    AiAssist,
    Map,
}

#[derive(Debug)]
struct Ctx<'a> {
    ctx_kind: CtxKind,
    sema: &'a Semantic<'a>,
    parsed: InFile<SourceFile>,
    trigger: Option<char>,
    previous_tokens: Option<Vec<(SyntaxKind, SyntaxToken)>>,
    next_token: Option<SyntaxToken>,
    file_position: FilePosition,
}

pub fn completions(
    db: &RootDatabase,
    file_position: FilePosition,
    trigger: Option<char>,
) -> Vec<Completion> {
    let sema = &Semantic::new(db);
    let parsed = sema.parse(file_position.file_id);
    let node = parsed.value.syntax();
    let ctx_kind = CtxKind::new(node, file_position.offset);
    let mut acc = Vec::new();
    let previous_tokens = get_previous_tokens(node, file_position);
    let next_token = right_biased_token(node, file_position);
    let ctx = &Ctx {
        ctx_kind: ctx_kind.clone(),
        sema,
        parsed,
        file_position,
        previous_tokens,
        next_token,
        trigger,
    };

    match ctx_kind {
        CtxKind::Comment => (),
        CtxKind::Expr => {
            let _ = macros::add_completions(&mut acc, ctx)
                || maps::add_completions(&mut acc, ctx)
                || records::add_completions(&mut acc, ctx)
                || functions::add_completions(&mut acc, ctx)
                || vars::add_completions(&mut acc, ctx)
                || modules::add_completions(&mut acc, ctx)
                || keywords::add_completions(&mut acc, ctx);
        }
        CtxKind::Type => {
            let _ = macros::add_completions(&mut acc, ctx)
                || types::add_completions(&mut acc, ctx)
                || modules::add_completions(&mut acc, ctx);
        }
        CtxKind::Export => {
            export_functions::add_completions(&mut acc, ctx);
        }
        CtxKind::ExportType => {
            export_types::add_completions(&mut acc, ctx);
        }
        CtxKind::Spec => {
            spec::add_completions(&mut acc, ctx);
        }
        CtxKind::Dialyzer => {
            functions::add_completions(&mut acc, ctx);
        }
        CtxKind::Other => {
            let _ = attributes::add_completions(&mut acc, ctx)
                // @fb-only: || meta_only::add_completions(&mut acc, ctx)
                || vars::add_completions(&mut acc, ctx)
                || maps::add_completions(&mut acc, ctx)
                || records::add_completions(&mut acc, ctx);
        }
    }
    // Sort for maintainable snapshot tests (mirrors LSP client sort):
    // sort_text takes priority (matching LSP behavior), then label
    acc.sort_by(|c1, c2| {
        let st1 = c1.sort_text.as_deref().unwrap_or("");
        let st2 = c2.sort_text.as_deref().unwrap_or("");
        st1.cmp(st2).then_with(|| c1.label.cmp(&c2.label))
    });
    acc
}

// Note: in an ideal world, we would not need to use much token-level information
// to get reasonable error-recovery for completions.
// See T154356210
fn get_previous_tokens(
    node: &SyntaxNode,
    file_position: FilePosition,
) -> Option<Vec<(SyntaxKind, SyntaxToken)>> {
    let mut token = node.token_at_offset(file_position.offset).left_biased()?;
    let mut tokens = Vec::new();

    while token.text_range().start() >= 0.into() && tokens.len() < MAX_PREVIOUS_TOKENS_LEN {
        let next_opt = token.prev_token();
        if !token.kind().is_trivia() {
            tokens.push(token.clone());
        }
        if let Some(next) = next_opt {
            token = next;
        } else {
            break;
        }
    }
    Some(
        tokens
            .into_iter()
            .rev()
            .map(|tok| (tok.kind(), tok))
            .collect::<Vec<_>>(),
    )
}

fn right_biased_token(node: &SyntaxNode, file_position: FilePosition) -> Option<SyntaxToken> {
    node.token_at_offset(file_position.offset).right_biased()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IncludeFile {
    pub include_lib: bool,
    pub path: String,
    pub app_name: String,
}

impl IncludeFile {
    pub fn as_attribute(&self) -> String {
        if self.include_lib {
            format!("-include_lib(\"{}\").\n", self.path)
        } else {
            format!("-include(\"{}\").\n", self.path)
        }
    }

    pub fn insert_position_if_needed(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<FilePosition> {
        let form_list = sema.form_list(file_id);
        let existing_import = form_list.includes().any(|(_, include)| match include {
            IncludeAttribute::Include { path, .. } => path == &self.path,
            IncludeAttribute::IncludeLib { path, .. } => path == &self.path,
        });
        if existing_import {
            None
        } else {
            let includes: Vec<_> = form_list.includes().collect();
            let offset = if includes.is_empty() {
                let source = sema.parse(file_id);
                top_insert_position(&form_list, &source.value)
            } else {
                let is_sorted = includes
                    .windows(2)
                    .all(|pair| pair[0].1.path().as_str() <= pair[1].1.path().as_str());
                if is_sorted {
                    // Includes are alphabetically sorted — insert at the right position.
                    let self_path = self.path.as_str();
                    let between_idx = includes.windows(2).position(|pair| {
                        pair[0].1.path().as_str() <= self_path
                            && pair[1].1.path().as_str() >= self_path
                    });
                    if let Some(idx) = between_idx {
                        let prev_end = includes[idx].1.form_id().range(sema.db, file_id).end();
                        let next_start = includes[idx + 1]
                            .1
                            .form_id()
                            .range(sema.db, file_id)
                            .start();
                        if next_start - prev_end > TextSize::new(1) {
                            // Blank line between includes (group boundary) —
                            // append to last group to avoid splitting groups.
                            let (_, last) = includes.last().unwrap();
                            last.form_id().range(sema.db, file_id).end() + TextSize::new(1)
                        } else {
                            next_start
                        }
                    } else if includes[0].1.path().as_str() >= self_path {
                        // Alphabetically before all existing includes.
                        includes[0].1.form_id().range(sema.db, file_id).start()
                    } else {
                        // Alphabetically after all existing includes.
                        let (_, last) = includes.last().unwrap();
                        last.form_id().range(sema.db, file_id).end() + TextSize::new(1)
                    }
                } else {
                    // Includes are not sorted — append after last to avoid
                    // breaking dependency order.
                    let (_, last) = includes.last().unwrap();
                    last.form_id().range(sema.db, file_id).end() + TextSize::new(1)
                }
            };
            Some(FilePosition { file_id, offset })
        }
    }
}

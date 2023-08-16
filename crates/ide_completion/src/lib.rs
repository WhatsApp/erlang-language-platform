/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use ctx::Ctx;
use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::RootDatabase;
use elp_syntax::AstNode;
use elp_syntax::SourceFile;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use elp_syntax::SyntaxToken;
use hir::db::MinDefDatabase;
use hir::InFile;
use hir::Semantic;

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
// @fb-only: mod meta_only;
mod modules;
mod records;
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
}

impl fmt::Display for Completion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.deprecated {
            true => write!(
                f,
                "{{label:{}, kind:{:?}, contents:{:?}, position:{:?}, deprecated:{}}}",
                self.label, self.kind, self.contents, self.position, self.deprecated
            ),
            false => write!(
                f,
                "{{label:{}, kind:{:?}, contents:{:?}, position:{:?}}}",
                self.label, self.kind, self.contents, self.position
            ),
        }
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
}

struct Args<'a> {
    db: &'a dyn MinDefDatabase,
    sema: &'a Semantic<'a>,
    parsed: InFile<SourceFile>,
    trigger: Option<char>,
    previous_tokens: Option<Vec<(SyntaxKind, SyntaxToken)>>,
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
    let node_range = node.text_range();
    // Check the failing assert condition for T153426323
    if !(node_range.start() <= file_position.offset && file_position.offset <= node_range.end()) {
        let original_token = node.token_at_offset(file_position.offset).left_biased();
        // Confirming this as the origin for T153426323
        log::error!(
            "completions:invalid position {:?} for range {:?}, original_token={:?}",
            file_position.offset,
            node.text_range(),
            original_token
        );
        return vec![];
    }
    let ctx = Ctx::new(node, file_position.offset);
    let mut acc = Vec::new();
    let previous_tokens = get_previous_tokens(node, file_position);
    let args = &Args {
        db,
        sema,
        parsed,
        file_position,
        previous_tokens,
        trigger,
    };

    match ctx {
        Ctx::Expr => {
            let _ = macros::add_completions(&mut acc, args)
                || records::add_completions(&mut acc, args)
                || functions::add_completions(&mut acc, args)
                || vars::add_completions(&mut acc, args)
                || modules::add_completions(&mut acc, args)
                || keywords::add_completions(&mut acc, args);
        }
        Ctx::Type => {
            let _ = macros::add_completions(&mut acc, args)
                || types::add_completions(&mut acc, args)
                || modules::add_completions(&mut acc, args);
        }
        Ctx::Export => {
            export_functions::add_completions(&mut acc, args);
        }
        Ctx::ExportType => {
            export_types::add_completions(&mut acc, args);
        }
        Ctx::Other => {
            let _ = attributes::add_completions(&mut acc, args)
                // @fb-only: || meta_only::add_completions(&mut acc, args)
                || vars::add_completions(&mut acc, args);
        }
    }
    // Sort for maintainable snapshot tests:
    // sorting isn't necessary for prod because LSP client sorts
    acc.sort_by(|c1, c2| c1.label.cmp(&c2.label));
    acc
}

// Note: in an ideal world, we would not need to use much token-level information
// to get reasonable error-recovery for completions.
// See T154356210
fn get_previous_tokens(
    node: &SyntaxNode,
    file_position: FilePosition,
) -> Option<Vec<(SyntaxKind, SyntaxToken)>> {
    // Temporary for T153426323
    let _pctx = stdx::panic_context::enter("\nget_previous_tokens".to_string());
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

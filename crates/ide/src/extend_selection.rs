/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::RootDatabase;
use elp_syntax::ast::AstNode;
use elp_syntax::Direction;
use elp_syntax::NodeOrToken;
use elp_syntax::SyntaxKind::*;
use elp_syntax::SyntaxKind::{self};
use elp_syntax::SyntaxNode;
use elp_syntax::SyntaxToken;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use elp_syntax::TokenAtOffset;
use hir::Semantic;

use crate::FileRange;

// Feature: Expand and Shrink Selection
//
// Extends or shrinks the current selection to the encompassing syntactic construct
// (expression, function, module, etc). It works with multiple cursors.
//
// |===
// | Editor  | Shortcut
//
// | VS Code | kbd:[Alt+Shift+→], kbd:[Alt+Shift+←]
// |===
pub(crate) fn extend_selection(db: &RootDatabase, frange: FileRange) -> TextRange {
    let sema = Semantic::new(db);
    let source_file = sema.parse(frange.file_id);
    try_extend_selection(source_file.value.syntax(), frange).unwrap_or(frange.range)
}

fn try_extend_selection(root: &SyntaxNode, frange: FileRange) -> Option<TextRange> {
    let range = frange.range;

    let string_kinds = [COMMENT, STRING, BINARY];
    let list_kinds = [
        ANONYMOUS_FUN,
        BIT_TYPE_LIST,
        BLOCK_EXPR,
        CALLBACK,
        CASE_EXPR,
        CLAUSE_BODY,
        CONCATABLES,
        EXPORT_ATTRIBUTE,
        EXPORT_TYPE_ATTRIBUTE,
        EXPR_ARGS,
        FUN_DECL,
        GUARD_CLAUSE,
        GUARD,
        IF_EXPR,
        IMPORT_ATTRIBUTE,
        LC_EXPRS,
        LIST,
        MACRO_CALL_ARGS,
        MAP_EXPR_UPDATE,
        MAP_EXPR,
        OPTIONAL_CALLBACKS_ATTRIBUTE,
        PP_INCLUDE_LIB,
        PP_INCLUDE,
        RECEIVE_EXPR,
        RECORD_DECL,
        RECORD_EXPR,
        RECORD_UPDATE_EXPR,
        REPLACEMENT_CR_CLAUSES,
        REPLACEMENT_FUNCTION_CLAUSES,
        REPLACEMENT_GUARD_AND,
        REPLACEMENT_GUARD_OR,
        SOURCE_FILE,
        SPEC,
        TRY_AFTER,
        TRY_EXPR,
        TUPLE,
        TYPE_GUARDS,
        VAR_ARGS,
    ];

    if range.is_empty() {
        let offset = range.start();
        // Temporary for T153426323
        let _pctx = stdx::panic_context::enter(format!("\ntry_extend_selection"));
        let mut leaves = root.token_at_offset(offset);
        if leaves.clone().all(|it| it.kind() == WHITESPACE) {
            return Some(extend_ws(root, leaves.next()?, offset));
        }
        let leaf_range = match leaves {
            TokenAtOffset::None => return None,
            TokenAtOffset::Single(l) => {
                if string_kinds.contains(&l.kind()) {
                    extend_single_word_in_comment_or_string(&l, offset)
                        .unwrap_or_else(|| l.text_range())
                } else {
                    l.text_range()
                }
            }
            TokenAtOffset::Between(l, r) => pick_best(l, r).text_range(),
        };
        return Some(leaf_range);
    };
    let node = match root.covering_element(range) {
        NodeOrToken::Token(token) => {
            if token.text_range() != range {
                return Some(token.text_range());
            }
            token.parent()?
        }
        NodeOrToken::Node(node) => node,
    };

    if node.text_range() != range {
        return Some(node.text_range());
    }

    let node = shallowest_node(&node);

    if node.parent().map(|n| list_kinds.contains(&n.kind())) == Some(true) {
        if let Some(range) = extend_list_item(&node) {
            return Some(range);
        }
    }

    node.parent().map(|it| it.text_range())
}

/// Find the shallowest node with same range, which allows us to traverse siblings.
fn shallowest_node(node: &SyntaxNode) -> SyntaxNode {
    node.ancestors()
        .take_while(|n| n.text_range() == node.text_range())
        .last()
        .unwrap()
}

fn extend_single_word_in_comment_or_string(
    leaf: &SyntaxToken,
    offset: TextSize,
) -> Option<TextRange> {
    let text: &str = leaf.text();
    let cursor_position: u32 = (offset - leaf.text_range().start()).into();

    let (before, after) = text.split_at(cursor_position as usize);

    fn non_word_char(c: char) -> bool {
        !(c.is_alphanumeric() || c == '_')
    }

    let start_idx = before.rfind(non_word_char)? as u32;
    let end_idx = after.find(non_word_char).unwrap_or_else(|| after.len()) as u32;

    let from: TextSize = (start_idx + 1).into();
    let to: TextSize = (cursor_position + end_idx).into();

    // Temporary for  T148094436
    let _pctx = stdx::panic_context::enter(format!("\nextend_single_word_in_comment_or_string"));
    let range = TextRange::new(from, to);
    if range.is_empty() {
        None
    } else {
        Some(range + leaf.text_range().start())
    }
}

fn extend_ws(root: &SyntaxNode, ws: SyntaxToken, offset: TextSize) -> TextRange {
    let ws_text = ws.text();
    // Temporary for  T148094436
    let _pctx = stdx::panic_context::enter(format!("\nextend_ws"));
    let suffix = TextRange::new(offset, ws.text_range().end()) - ws.text_range().start();
    let prefix = TextRange::new(ws.text_range().start(), offset) - ws.text_range().start();
    let ws_suffix = &ws_text[suffix];
    let ws_prefix = &ws_text[prefix];
    if ws_text.contains('\n') && !ws_suffix.contains('\n') {
        if let Some(node) = ws.next_sibling_or_token() {
            let start = match ws_prefix.rfind('\n') {
                Some(idx) => ws.text_range().start() + TextSize::from((idx + 1) as u32),
                None => node.text_range().start(),
            };
            let end = if root.text().char_at(node.text_range().end()) == Some('\n') {
                node.text_range().end() + TextSize::of('\n')
            } else {
                node.text_range().end()
            };
            return TextRange::new(start, end);
        }
    }
    ws.text_range()
}

fn pick_best(l: SyntaxToken, r: SyntaxToken) -> SyntaxToken {
    return if priority(&r) > priority(&l) { r } else { l };
    fn priority(n: &SyntaxToken) -> usize {
        match n.kind() {
            WHITESPACE => 0,
            _ => 1,
        }
    }
}

/// Extend list item selection to include nearby delimiter and whitespace.
fn extend_list_item(node: &SyntaxNode) -> Option<TextRange> {
    fn is_single_line_ws(node: &SyntaxToken) -> bool {
        node.kind() == WHITESPACE && !node.text().contains('\n')
    }

    fn nearby_delimiter(
        delimiter_kind: SyntaxKind,
        node: &SyntaxNode,
        dir: Direction,
    ) -> Option<SyntaxToken> {
        node.siblings_with_tokens(dir)
            .skip(1)
            .find(|node| match node {
                NodeOrToken::Node(_) => true,
                NodeOrToken::Token(it) => !is_single_line_ws(it),
            })
            .and_then(|it| it.into_token())
            .filter(|node| node.kind() == delimiter_kind)
    }

    let delimiter = SyntaxKind::ANON_COMMA;

    if let Some(delimiter_node) = nearby_delimiter(delimiter, node, Direction::Next) {
        // Include any following whitespace when delimiter is after list item.
        let final_node = delimiter_node
            .next_sibling_or_token()
            .and_then(|it| it.into_token())
            .filter(is_single_line_ws)
            .unwrap_or(delimiter_node);

        // Temporary for  T148094436
        let _pctx = stdx::panic_context::enter(format!("\nextend_list_item1"));
        return Some(TextRange::new(
            node.text_range().start(),
            final_node.text_range().end(),
        ));
    }
    if let Some(delimiter_node) = nearby_delimiter(delimiter, node, Direction::Prev) {
        // Temporary for  T148094436
        let _pctx = stdx::panic_context::enter(format!("\nextend_list_item2"));
        return Some(TextRange::new(
            delimiter_node.text_range().start(),
            node.text_range().end(),
        ));
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixture;

    fn do_check(before: &str, afters: &[&str]) {
        let (analysis, position) = fixture::position(before);
        let before = analysis.file_text(position.file_id).unwrap();
        let range = TextRange::empty(position.offset);
        let mut frange = FileRange {
            file_id: position.file_id,
            range,
        };

        for &after in afters {
            frange.range = analysis.extend_selection(frange).unwrap();
            let actual = &before[frange.range];
            assert_eq!(after, actual);
        }
    }

    #[test]
    fn test_extend_selection_arith_expression() {
        do_check(
            r#"foo() -> ~1 + 1"#,
            &["1", "1 + 1", "-> 1 + 1", "foo() -> 1 + 1"],
        );
    }

    #[test]
    fn test_extend_selection_function_args() {
        do_check(r#"foo(X~) -> ok."#, &["X", "(X)", "foo(X) -> ok"]);
        do_check(
            r#"foo(X, ~Y) -> ok."#,
            &["Y", ", Y", "(X, Y)", "foo(X, Y) -> ok", "foo(X, Y) -> ok."],
        );
        do_check(
            r#"foo(X, ~Y :: integer()) -> ok."#,
            &[
                "Y",
                "Y ::",
                "Y :: integer()",
                ", Y :: integer()",
                "(X, Y :: integer())",
                "foo(X, Y :: integer()) -> ok",
                "foo(X, Y :: integer()) -> ok.",
            ],
        );
        do_check(
            r#"foo({X, ~Y :: integer(), Z}) -> ok."#,
            &[
                "Y",
                "Y ::",
                "Y :: integer()",
                "Y :: integer(), ",
                "{X, Y :: integer(), Z}",
                "({X, Y :: integer(), Z})",
                "foo({X, Y :: integer(), Z}) -> ok",
                "foo({X, Y :: integer(), Z}) -> ok.",
            ],
        );
    }

    #[test]
    fn test_extend_selection_tuples() {
        do_check(
            r#"foo() -> {1, t~wo, "three"}"#,
            &["two", "two, ", "{1, two, \"three\"}"],
        );
    }

    #[test]
    fn test_extend_selection_lists() {
        do_check(
            r#"foo() -> [1, tw~o, "three"]"#,
            &["two", "two, ", "[1, two, \"three\"]"],
        );
    }

    #[test]
    fn test_extend_selection_strings() {
        do_check(
            r#"
-module(strings).

some_strings() ->
  X = "abc", %% Look, a comment!
  Y = "123"
      "4~56"
      "789".
"#,
            &[
                "456",
                "\"456\"",
                "\"123\"\n      \"456\"\n      \"789\"",
                "Y = \"123\"\n      \"456\"\n      \"789\"",
                "->\n  X = \"abc\", %% Look, a comment!\n  Y = \"123\"\n      \"456\"\n      \"789\"",
            ],
        );
    }

    #[test]
    fn test_extend_guards() {
        do_check(
            r#"
-module(guards).

foo(X) when is_in~teger(X) andalso not is_boolean(X) ->
  42.
"#,
            &[
                "is_integer",
                "is_integer(X)",
                "is_integer(X) andalso not is_boolean(X)",
                "foo(X) when is_integer(X) andalso not is_boolean(X) ->\n  42",
            ],
        );
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_syntax::AstNode;
use elp_syntax::NodeOrToken;
use elp_syntax::SyntaxElement;
use elp_syntax::SyntaxKind;
use elp_syntax::SyntaxNode;
use elp_syntax::SyntaxToken;
use elp_syntax::TextSize;
use elp_syntax::algo;
use elp_syntax::ast;
use elp_syntax::match_ast;
use elp_syntax::ted::Element;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CtxKind {
    Comment,
    Expr,
    Type,
    Export,
    ExportType,
    Spec,
    Dialyzer,
    Other,
}

impl CtxKind {
    pub fn new(node: &SyntaxNode, offset: TextSize) -> Self {
        if Self::is_comment(node, offset) {
            Self::Comment
        } else if Self::is_atom_colon(node, offset) && Self::is_expr(node, offset) {
            Self::Expr
        } else if Self::is_export(node, offset) {
            Self::Export
        } else if Self::is_export_type(node, offset) {
            Self::ExportType
        } else if Self::is_attribute(node, offset)
            || Self::is_type_level_param(node, offset)
            || Self::is_pattern(node, offset)
        {
            Self::Other
        } else if Self::is_type(node, offset) {
            Self::Type
        } else if Self::is_spec(node, offset) {
            Self::Spec
        } else if Self::is_dialyzer(node, offset) {
            Self::Dialyzer
        } else if Self::is_expr(node, offset) || Self::is_pp_define(node, offset) {
            Self::Expr
        } else {
            Self::Other
        }
    }
    fn is_comment(node: &SyntaxNode, offset: TextSize) -> bool {
        algo::ancestors_at_offset(node, offset)
            .into_iter()
            .flatten()
            .any(|ancestor| ancestor.kind() == SyntaxKind::COMMENT)
    }
    fn is_atom_colon(node: &SyntaxNode, offset: TextSize) -> bool {
        if let Some(parent) = algo::ancestors_at_offset(node, offset).and_then(|mut ns| ns.next()) {
            match_ast! {
                    match parent {
                        ast::RemoteModule(_) => {
                            true
                        },
                        _ => false
                    }
            }
        } else {
            false
        }
    }
    fn is_export(node: &SyntaxNode, offset: TextSize) -> bool {
        algo::find_node_at_offset::<ast::ExportAttribute>(node, offset).is_some()
    }
    fn is_export_type(node: &SyntaxNode, offset: TextSize) -> bool {
        algo::find_node_at_offset::<ast::ExportTypeAttribute>(node, offset).is_some()
    }
    fn is_spec(node: &SyntaxNode, offset: TextSize) -> bool {
        algo::find_node_at_offset::<ast::Spec>(node, offset).is_some()
    }
    fn is_dialyzer(node: &SyntaxNode, offset: TextSize) -> bool {
        if let Some(wild_attr) = algo::find_node_at_offset::<ast::WildAttribute>(node, offset) {
            if let Some(name) = wild_attr.name() {
                return name.syntax().text() == "-dialyzer";
            }
        }
        false
    }
    fn is_pp_define(node: &SyntaxNode, offset: TextSize) -> bool {
        algo::find_node_at_offset::<ast::PpDefine>(node, offset).is_some()
    }
    fn is_attribute(_node: &SyntaxNode, _offset: TextSize) -> bool {
        false
    }
    fn is_type_level_param(node: &SyntaxNode, offset: TextSize) -> bool {
        let head_opt = algo::find_node_at_offset::<ast::TypeAlias>(node, offset)
            .and_then(|type_alias| type_alias.name())
            .or(algo::find_node_at_offset::<ast::Opaque>(node, offset)
                .and_then(|opaque| opaque.name()));
        head_opt
            .map(|head| offset <= head.syntax().text_range().end())
            .unwrap_or_default()
    }
    fn is_pattern(node: &SyntaxNode, offset: TextSize) -> bool {
        if let Some(mut ancestors) = algo::ancestors_at_offset(node, offset) {
            ancestors.any(|n| {
            let is_match = |node: &SyntaxNode| node.text_range() == n.text_range();
            if let Some(parent) = n.parent() {
                match_ast! {
                        match parent {
                            ast::CatchClause(parent) => {
                                if let Some(it) = parent.pat() {
                                    return is_match(it.syntax())
                                }
                            },
                            ast::FunClause(parent) => {
                                if let Some(it) = parent.args() {
                                    return is_match(it.syntax())
                                }
                            },
                            ast::FunctionClause(parent) => {
                                if let Some(it) = parent.args() {
                                    return is_match(it.syntax())
                                }
                            },
                            ast::MatchExpr(parent) => {
                                let prev_token = Self::previous_non_trivia_sibling_or_token(parent.syntax());
                                if Self::is_in_error(node, offset) {
                                    if let Some(NodeOrToken::Token(token)) = prev_token {
                                        if token.kind() == SyntaxKind::ANON_CASE {
                                            return false;
                                        }
                                    }
                                }
                                if let Some(it) = parent.lhs() {
                                    return is_match(it.syntax())
                                }
                            },
                            ast::CrClause(parent) => {
                                if let Some(it) = parent.pat() {
                                    return is_match(it.syntax())
                                }
                            },
                            _ => ()
                        }
                }
            }
            false
        })
        } else {
            false
        }
    }

    fn is_expr(node: &SyntaxNode, offset: TextSize) -> bool {
        let mut in_expr = true;
        let ancestor_offset = if let Some(ancestors) = algo::ancestors_at_offset(node, offset) {
            ancestors
                .inspect(|n| {
                    if n.kind() == SyntaxKind::TYPE_SIG {
                        in_expr = false;
                    };
                })
                .take_while(|n| n.kind() != SyntaxKind::SOURCE_FILE)
                .last()
                .and_then(|n| n.first_token())
                .map(|tok: SyntaxToken| tok.text_range().start())
                .unwrap_or_default()
        } else {
            return false;
        };
        if !in_expr {
            return false;
        }
        if let Some(mut tok) = node.token_at_offset(offset).left_biased() {
            if tok.text_range().start() < ancestor_offset {
                return false;
            }
            while let Some(prev) = tok.prev_token() {
                tok = prev;
                if tok.kind() == SyntaxKind::ANON_DASH_GT {
                    return true;
                }
            }
            false
        } else {
            false
        }
    }

    fn is_type(node: &SyntaxNode, offset: TextSize) -> bool {
        if let Some(ancestors) = algo::ancestors_at_offset(node, offset) {
            let mut error_seen = false;
            for n in ancestors {
                if n.kind() == SyntaxKind::ERROR {
                    error_seen = true;
                }
                match_ast! {
                    match n {
                        ast::Spec(_) => {
                            // For an incomplete spec, the name shows
                            // up in an ERROR node, and the following
                            // fun as the spec.
                            if !error_seen {
                                return true;
                            }
                        },
                        ast::TypeName(_) => {
                            return false;
                        },
                        ast::TypeAlias(_) => {
                            return true;
                        },
                        ast::Opaque(_) => {
                            return true;
                        },
                        ast::Nominal(_) => {
                            return true;
                        },
                        ast::FieldType(_) => {
                            return true;
                        },
                        _ => ()
                    }
                };
            }
        }
        false
    }
    fn is_in_error(node: &SyntaxNode, offset: TextSize) -> bool {
        if let Some(mut ancestors) = algo::ancestors_at_offset(node, offset) {
            ancestors.any(|n| n.kind() == SyntaxKind::ERROR)
        } else {
            false
        }
    }
    fn previous_non_trivia_sibling_or_token(node: &SyntaxNode) -> Option<SyntaxElement> {
        let mut sot = node.prev_sibling_or_token();
        while let Some(NodeOrToken::Token(inner)) = sot {
            if !inner.kind().is_trivia() {
                return Some(inner.syntax_element());
            } else {
                sot = inner.prev_sibling_or_token();
            }
        }
        None
    }
}

/// Tests of internals, delete when autocomplete is full-featured T126163525
#[cfg(test)]
mod ctx_tests {
    use elp_base_db::SourceDatabase;
    use elp_ide_db::RootDatabase;
    use elp_ide_db::elp_base_db::FilePosition;
    use elp_ide_db::elp_base_db::fixture::WithFixture;
    use elp_syntax::AstNode;
    use hir::Semantic;

    use crate::CtxKind;

    fn ctx(code: &str) -> CtxKind {
        let (db, fixture) = RootDatabase::with_fixture(code);
        let FilePosition { file_id, offset } = fixture.position();
        let sema = Semantic::new(&db);
        let parsed = sema.parse(file_id);
        let node = parsed.value.syntax();
        let offset = db.clamp_offset(file_id, offset);
        CtxKind::new(node, offset)
    }

    #[test]
    fn expr_ctx() {
        assert_eq!(
            ctx(r#"
        -module(sample).
        test() ->
            ~X.
        "#),
            CtxKind::Expr
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test() ->
            case 1 of.
                1 -> ~2
            end.
        "#),
            CtxKind::Expr,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test() ->
            fun(_) -> ~X end.
        "#),
            CtxKind::Expr,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test() ->
            try 1
            of
              1 -> X~
            catch
                _:_ -> ok
            catch
                _:_ -> ok
            end.
        "#),
            CtxKind::Expr
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        main(_) ->
            #{(maps:from_list([~])) => 3}.
        "#),
            CtxKind::Expr
        );
    }

    #[test]
    fn expr_ctx_2() {
        assert_eq!(
            ctx(r#"
        -module(completion).

        start() ->
            lists:~
            ok = preload_modules(),
            ok.
        "#),
            CtxKind::Expr // Ctx::Other
        );
    }

    #[test]
    fn ctx_pattern() {
        assert_eq!(
            ctx(r#"
        -module(sample).
        test(Y, X) ->
            ~Y = X.
        "#),
            CtxKind::Other,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test(X) ->
            case rand:uniform(1) of
                {X~} -> true
            end.
        "#),
            CtxKind::Other,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test(X) ->
            fun(X~) -> 1 end.
        "#),
            CtxKind::Other,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test() ->
            receive
                [X~] -> true
            end.
        "#),
            CtxKind::Other,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test() ->
            try [1]
            of
              [X~] -> true
            catch
                _:_ -> ok
            end.
        "#),
            CtxKind::Other,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test(X) ->
            if
                X~ -> ok
                true -> error
            end.

        "#),
            CtxKind::Expr,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test(X~) ->
            ok.
        "#),
            CtxKind::Other,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test(Y, X) ->
            try ok of
                X~ ->

        "#),
            CtxKind::Expr,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test(Y, X) ->
            try ok of
                ok -> ok
            catch
                X~ -> ok
        "#),
            CtxKind::Other,
        );
    }

    #[test]
    // Known cases where error recovery for detecting context is inaccurate.
    // AST-based techniques may be more accurate, see D39766695 for details.
    fn ctx_pattern_error_recovery_wip() {
        assert_eq!(
            ctx(r#"
        -module(sample).
        test(Y, X) ->
            try ok of
                X~ ->

        "#),
            // should be Ctx::Other
            CtxKind::Expr,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test(Y, X) ->
            try ok of
                ok -> ok
            catch
                X~
        "#),
            // should be Ctx::Other
            CtxKind::Expr,
        );
    }

    #[test]
    fn test_type_param_ctx() {
        assert_eq!(
            ctx(r#"
        -module(sample).
        -type ty(s~) :: ok.
        "#),
            CtxKind::Other
        );
    }

    #[test]
    fn test_export_ctx() {
        assert_eq!(
            ctx(r#"
        -module(sample).
        -export([
            f~
        ])
        "#),
            CtxKind::Export
        );
    }

    #[test]
    fn test_export_type_ctx() {
        assert_eq!(
            ctx(r#"
        -module(sample).
        -export_type([
            t~
        ])
        "#),
            CtxKind::ExportType
        );
    }

    #[test]
    fn test_spec_ctx() {
        assert_eq!(
            ctx(r#"
        -module(sample).
        -spec t~
        table() -> ok.
        ])
        "#),
            CtxKind::Spec
        );
    }

    #[test]
    fn test_type_ctx() {
        assert_eq!(
            ctx(r#"
        -module(sample).
        -spec test() -> ~
        test() -> ok.
        "#),
            CtxKind::Type
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        -spec test() -> o~k
        test() -> ok.
        "#),
            CtxKind::Type
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        -spec test(o~) -> ok.
        test() -> ok.
        "#),
            CtxKind::Type
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        -record(foo, {field1, field2 :: X~}).
        "#),
            CtxKind::Type
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        -opaque test() :: ~.
        "#),
            CtxKind::Type
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        -nominal test() :: ~.
        "#),
            CtxKind::Type
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        -type test() :: m~
        "#),
            CtxKind::Type
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        -spec test() -> ~ok.
        "#),
            CtxKind::Type
        );
    }

    #[test]
    fn test_ctx_error_recovery() {
        assert_eq!(
            ctx(r#"
        -module(sample).
        test() ->
            ~
        "#),
            CtxKind::Expr
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test() ->
            X + ~
        "#),
            CtxKind::Expr,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test() ->
            X + ~.
        "#),
            CtxKind::Expr,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test() ->
            case rand:uniform(1) of
                1 -> ~X

        "#),
            CtxKind::Expr,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test() ->
            (erlang:term_to_binary(~

        "#),
            CtxKind::Expr,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        test() ->
            (erlang:term_to_binary(~.

        "#),
            CtxKind::Expr,
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        -type ty() :: ~
        "#),
            CtxKind::Other
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        -type ty() :: l~.
        "#),
            CtxKind::Type
        );

        assert_eq!(
            ctx(r#"
        -module(sample).
        -record(rec, {field = lists:map(fun(X) -> X + 1 end, [1, ~])}).
        "#),
            CtxKind::Expr,
        );
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::TryInto;
use std::iter;
use std::marker::PhantomData;
use std::ops::Range;
use std::sync::Arc;

use itertools::Either;
use num_traits::FromPrimitive;
use rowan::GreenNodeBuilder;
use rowan::Language;
use tree_sitter::Node;
use tree_sitter::Tree;
use tree_sitter::TreeCursor;

use crate::tree_sitter_elp::Parser;

mod ptr;
mod syntax_error;
mod syntax_kind;
mod token_text;

pub mod algo;
pub mod ast;
pub mod label;
pub mod syntax_node;
pub mod ted;
pub mod tree_sitter_elp;
pub mod unescape;

pub use rowan::Direction;
pub use rowan::GreenNode;
pub use rowan::TextRange;
pub use rowan::TextSize;
pub use rowan::TokenAtOffset;
pub use rowan::WalkEvent;
pub use smol_str::SmolStr;
pub use syntax_kind::SyntaxKind;
pub use syntax_node::*;

pub use crate::algo::InsertPosition;
pub use crate::ast::AstNode;
pub use crate::ast::SourceFile;
pub use crate::ptr::AstPtr;
pub use crate::ptr::SyntaxNodePtr;
pub use crate::syntax_error::SyntaxError;
pub use crate::token_text::TokenText;

/// `Parse` is the result of the parsing: a syntax tree and a collection of
/// errors.
///
/// Note that we always produce a syntax tree, even for completely invalid
/// files.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Parse<T> {
    green: GreenNode,
    errors: Arc<Vec<SyntaxError>>,
    _ty: PhantomData<fn() -> T>,
}

impl<T> Clone for Parse<T> {
    fn clone(&self) -> Parse<T> {
        Parse {
            green: self.green.clone(),
            errors: self.errors.clone(),
            _ty: PhantomData,
        }
    }
}

impl<T> Parse<T> {
    pub fn syntax_node(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }

    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }
}

impl<T: AstNode> Parse<T> {
    pub fn tree(&self) -> T {
        T::cast(self.syntax_node()).unwrap()
    }

    pub fn ok(self) -> Result<T, Arc<Vec<SyntaxError>>> {
        if self.errors.is_empty() {
            Ok(self.tree())
        } else {
            Err(self.errors)
        }
    }

    pub fn from_ast(ast: &T) -> Parse<T> {
        Parse {
            green: ast.syntax().green().into(),
            errors: Arc::new(vec![]),
            _ty: PhantomData,
        }
    }
}

// ---------------------------------------------------------------------

struct Converter<'tree, 'text> {
    cursor: TreeCursor<'tree>,
    text: &'text str,
    errors: Vec<SyntaxError>,
    builder: GreenNodeBuilder<'static>,
    last_position: usize,
    pending_syntax_error: Option<Node<'tree>>,
    // Debug counter to try and track down the panic on calling self.builder.finish()
    open_count: isize,
}

impl<'tree, 'text> Converter<'tree, 'text> {
    pub fn new(tree: &'tree Tree, text: &'text str) -> Converter<'tree, 'text> {
        Converter {
            cursor: tree.walk(),
            text,
            errors: Vec::new(),
            builder: GreenNodeBuilder::new(),
            last_position: 0,
            pending_syntax_error: None,
            open_count: 0,
        }
    }

    pub fn convert(mut self) -> (GreenNode, Vec<SyntaxError>) {
        self.convert_node();

        if self.open_count > 0 {
            // If the open_count is > 0 it means we have unfinished builder nodes.
            // Close the nodes.
            while self.open_count > 0 {
                self.builder.finish_node();
                self.open_count -= 1;
            }
        }

        (self.builder.finish(), self.errors)
    }

    // Enter a node, and return a flag as to whether to recurse into
    // it or not.
    fn enter_node(&mut self, is_root: bool) -> bool {
        let node = self.cursor.node();
        let kind = SyntaxKind::from_u16(node.kind_id()).unwrap();
        let range = node.byte_range();
        if node.is_error() {
            self.pending_syntax_error = Some(node);
            let mut ret = false;
            if is_root {
                // Our parser has an invariant that the top level
                // node is of kind SOURCE_FILE.  We are aborting after
                // adding this node, so make sure this is the case
                self.start_node(SyntaxKind::SOURCE_FILE, range.start, is_root);

                // Make sure we capture whatever structure is wrapped
                // inside the ERROR node.
                if node.child_count() == 0 {
                    self.token(kind, &range, is_root);
                } else {
                    self.start_node(kind, range.start, is_root);
                    ret = true;
                };
            } else if node.child_count() == 0 {
                self.token(kind, &range, is_root);
            } else {
                self.start_node(kind, range.start, is_root);
                ret = true;
            }

            ret
        } else if node.is_missing() {
            let text = node.kind();
            self.error_missing(text.to_string(), range);
            false
        } else if node.child_count() == 0 {
            if node.is_named() {
                // We capture the node, and its enclosed token
                self.start_node(kind, range.start, is_root);
                self.token(kind, &range, false);
                self.finish_node(range.end, is_root);
            } else {
                self.token(kind, &range, is_root);
            }
            false
        } else {
            self.start_node(kind, range.start, is_root);
            true
        }
    }

    fn exit_node(&mut self, node: Node, is_root: bool) {
        if let Some(syntax_error_node) = &self.pending_syntax_error {
            if &node == syntax_error_node {
                self.error_syntax(node.byte_range());
                self.pending_syntax_error = None;
            }
        }
        if !((is_root && node.is_error()) || node.is_missing() || node.child_count() == 0) {
            let range = node.byte_range();
            self.finish_node(range.end, is_root);
        }
    }

    // Based on https://github.com/tree-sitter/tree-sitter/discussions/878
    fn convert_node(&mut self) {
        if self.enter_node(true) {
            let mut recurse = true;

            loop {
                // enter child
                if recurse && self.cursor.goto_first_child() {
                    recurse = self.enter_node(false);
                } else {
                    let node = self.cursor.node();

                    // go to sibling
                    if self.cursor.goto_next_sibling() {
                        self.exit_node(node, false);
                        recurse = self.enter_node(false);
                    } else if self.cursor.goto_parent() {
                        self.exit_node(node, false);
                        recurse = false;
                    } else {
                        self.exit_node(node, true);
                        break;
                    }
                }
            }
        } else {
            let node = self.cursor.node();
            self.exit_node(node, true);
        }
    }

    fn error_syntax(&mut self, range: Range<usize>) {
        let range = convert_range(range);
        self.errors.push(SyntaxError::error(range));
    }

    fn error_missing(&mut self, msg: impl Into<String>, range: Range<usize>) {
        let range = convert_range(range);
        self.errors.push(SyntaxError::missing(msg, range));
    }

    fn token(&mut self, kind: SyntaxKind, range: &Range<usize>, is_root: bool) {
        if is_root {
            self.start_node(kind, range.start, is_root);
        }

        self.update_position(range.start, range.end);
        let text = &self.text[range.clone()];
        self.builder.token(ELPLanguage::kind_to_raw(kind), text);

        if is_root {
            self.finish_node(range.end, is_root);
        }
    }

    fn start_node(&mut self, kind: SyntaxKind, range_start: usize, is_root: bool) {
        // Root node has whitespace inside of it (since there's nothing outside),
        // all other nodes have it outside
        if !is_root {
            self.update_position(range_start, range_start);
        }
        self.builder.start_node(ELPLanguage::kind_to_raw(kind));
        self.open_count += 1;
        if is_root {
            self.update_position(range_start, range_start);
        }
    }

    fn finish_node(&mut self, range_end: usize, is_root: bool) {
        // Root node has whitespace inside of it (since there's nothing outside),
        // all other nodes have it outside
        if is_root {
            self.update_position(range_end, range_end);
        }
        self.builder.finish_node();
        self.open_count -= 1;
        if !is_root {
            self.update_position(range_end, range_end);
        }
    }

    fn update_position(&mut self, start: usize, end: usize) {
        if self.last_position < start {
            let kind = ELPLanguage::kind_to_raw(SyntaxKind::WHITESPACE);
            let text = &self.text[self.last_position..start];
            self.builder.token(kind, text);
        }
        self.last_position = end;
    }
}

fn convert_range(range: Range<usize>) -> TextRange {
    TextRange::new(
        range.start.try_into().unwrap(),
        range.end.try_into().unwrap(),
    )
}

// ---------------------------------------------------------------------

impl SourceFile {
    pub fn parse_text(text: &str) -> Parse<SourceFile> {
        let mut parser = Parser::new();
        let tree = parser.parse(text).expect("parsing should always succeed");
        let (green, errors) = Converter::new(&tree, text).convert();
        let root = SyntaxNode::new_root(green.clone());

        assert_eq!(root.kind(), SyntaxKind::SOURCE_FILE);
        Parse {
            green,
            errors: Arc::new(errors),
            _ty: PhantomData,
        }
    }

    /// If a single form has an error in it, the grammar may introduce
    /// an ERROR node that spans the entire top of the tree, but which
    /// nevertheless contains all the correctly parsed forms bar the
    /// erroneous one.  In this case, look into the ERROR node and
    /// return the valid forms found.
    pub fn forms(&self) -> impl Iterator<Item = ast::Form> {
        if self.syntax().first_child().map(|n| n.kind()) == Some(SyntaxKind::ERROR) {
            if let Some(child) = self.syntax().first_child() {
                Either::Left(
                    iter::successors(child.first_child(), |n| n.next_sibling())
                        .filter_map(ast::Form::cast),
                )
            } else {
                Either::Right(self.forms_only())
            }
        } else {
            Either::Right(self.forms_only())
        }
    }

    pub fn is_erlang_config_file(&self) -> bool {
        self.exprs().next().is_some()
    }
}

// ---------------------------------------------------------------------

/// Matches a `SyntaxNode` against an `ast` type.
///
/// # Example:
///
/// ```ignore
/// match_ast! {
///     match node {
///         ast::CallExpr(it) => { ... },
///         ast::MethodCallExpr(it) => { ... },
///         ast::MacroCallExpr(it) => { ... },
///         _ => None,
///     }
/// }
/// ```
#[macro_export]
macro_rules! match_ast {
    (match $node:ident { $($tt:tt)* }) => { match_ast!(match ($node) { $($tt)* }) };

    (match ($node:expr) {
        $( ast::$ast:ident($it:pat) => $res:expr, )*
        _ => $catch_all:expr $(,)?
    }) => {{
        $( if let Some($it) = ast::$ast::cast($node.clone()) { $res } else )*
        { $catch_all }
    }};
}

// ---------------------------------------------------------------------

// To run the tests via cargo
// cargo test --package elp_syntax --lib
#[cfg(test)]
mod tests {
    use expect_test::Expect;
    use expect_test::expect;
    use rowan::Direction;
    use rowan::NodeOrToken;
    use rowan::SyntaxText;
    use rowan::WalkEvent;
    use stdx::format_to;

    use super::*;

    #[test]
    fn syntax_node() {
        check_node(
            "foo(1) -> 2 + 3.",
            expect![[r#"
                SOURCE_FILE@0..16
                  FUN_DECL@0..16
                    FUNCTION_CLAUSE@0..15
                      ATOM@0..3
                        ATOM@0..3 "foo"
                      EXPR_ARGS@3..6
                        ANON_LPAREN@3..4 "("
                        INTEGER@4..5
                          INTEGER@4..5 "1"
                        ANON_RPAREN@5..6 ")"
                      WHITESPACE@6..7 " "
                      CLAUSE_BODY@7..15
                        ANON_DASH_GT@7..9 "->"
                        WHITESPACE@9..10 " "
                        BINARY_OP_EXPR@10..15
                          INTEGER@10..11
                            INTEGER@10..11 "2"
                          WHITESPACE@11..12 " "
                          ANON_PLUS@12..13 "+"
                          WHITESPACE@13..14 " "
                          INTEGER@14..15
                            INTEGER@14..15 "3"
                    ANON_DOT@15..16 ".""#]],
        )
    }

    #[test]
    fn whitespace() {
        check_node(
            "\nf() ->\n\t 1 + 2\t.\n\n",
            expect![[r#"
                SOURCE_FILE@0..19
                  WHITESPACE@0..1 "\n"
                  FUN_DECL@1..17
                    FUNCTION_CLAUSE@1..15
                      ATOM@1..2
                        ATOM@1..2 "f"
                      EXPR_ARGS@2..4
                        ANON_LPAREN@2..3 "("
                        ANON_RPAREN@3..4 ")"
                      WHITESPACE@4..5 " "
                      CLAUSE_BODY@5..15
                        ANON_DASH_GT@5..7 "->"
                        WHITESPACE@7..10 "\n\t "
                        BINARY_OP_EXPR@10..15
                          INTEGER@10..11
                            INTEGER@10..11 "1"
                          WHITESPACE@11..12 " "
                          ANON_PLUS@12..13 "+"
                          WHITESPACE@13..14 " "
                          INTEGER@14..15
                            INTEGER@14..15 "2"
                    WHITESPACE@15..16 "\t"
                    ANON_DOT@16..17 "."
                  WHITESPACE@17..19 "\n\n""#]],
        );
    }

    #[test]
    fn error_nodes1() {
        // Note: even though the FUN_DECL is in an ERROR node, we
        // still lower it into something we can use.  See test
        // body::tests::lowering_with_error_nodes
        check_node(
            "f(1a) -> ok begin 1 end.",
            expect![[r#"
                SOURCE_FILE@0..24
                  ERROR@0..17
                    FUN_DECL@0..11
                      FUNCTION_CLAUSE@0..11
                        ATOM@0..1
                          ATOM@0..1 "f"
                        EXPR_ARGS@1..5
                          ANON_LPAREN@1..2 "("
                          ERROR@2..3
                            INTEGER@2..3
                              INTEGER@2..3 "1"
                          ATOM@3..4
                            ATOM@3..4 "a"
                          ANON_RPAREN@4..5 ")"
                        WHITESPACE@5..6 " "
                        CLAUSE_BODY@6..11
                          ANON_DASH_GT@6..8 "->"
                          WHITESPACE@8..9 " "
                          ATOM@9..11
                            ATOM@9..11 "ok"
                    WHITESPACE@11..12 " "
                    ATOM@12..17
                      ATOM@12..17 "begin"
                  WHITESPACE@17..18 " "
                  INTEGER@18..19
                    INTEGER@18..19 "1"
                  WHITESPACE@19..20 " "
                  ATOM@20..23
                    ATOM@20..23 "end"
                  ANON_DOT@23..24 ".""#]],
        );
    }

    #[test]
    fn error_nodes2() {
        let input = "-define(,ok).";
        let parse = ast::SourceFile::parse_text(input);

        assert_eq!(parse.errors().len(), 1);

        check_node(
            "-define(,ok).",
            expect![[r#"
                SOURCE_FILE@0..13
                  PP_DEFINE@0..13
                    ANON_DASH@0..1 "-"
                    ANON_DEFINE@1..7 "define"
                    ANON_LPAREN@7..8 "("
                    MACRO_LHS@8..8
                    ANON_COMMA@8..9 ","
                    ATOM@9..11
                      ATOM@9..11 "ok"
                    ANON_RPAREN@11..12 ")"
                    ANON_DOT@12..13 ".""#]],
        )
    }

    #[test]
    fn error_nodes_3() {
        let input = r#"
            -module(foo).

            test(Y, X) ->
                try ok of
                    X ->"#;
        let parse = ast::SourceFile::parse_text(input);

        assert_eq!(parse.errors().len(), 1);

        // Check that the ERROR node contains what was parsed, even
        // though the end result was error.
        check_node(
            input,
            expect![[r#"
                SOURCE_FILE@0..104
                  WHITESPACE@0..13 "\n            "
                  ERROR@13..104
                    MODULE_ATTRIBUTE@13..26
                      ANON_DASH@13..14 "-"
                      ANON_MODULE@14..20 "module"
                      ANON_LPAREN@20..21 "("
                      ATOM@21..24
                        ATOM@21..24 "foo"
                      ANON_RPAREN@24..25 ")"
                      ANON_DOT@25..26 "."
                    WHITESPACE@26..40 "\n\n            "
                    ATOM@40..44
                      ATOM@40..44 "test"
                    EXPR_ARGS@44..50
                      ANON_LPAREN@44..45 "("
                      VAR@45..46
                        VAR@45..46 "Y"
                      ANON_COMMA@46..47 ","
                      WHITESPACE@47..48 " "
                      VAR@48..49
                        VAR@48..49 "X"
                      ANON_RPAREN@49..50 ")"
                    WHITESPACE@50..51 " "
                    ANON_DASH_GT@51..53 "->"
                    WHITESPACE@53..70 "\n                "
                    ANON_TRY@70..73 "try"
                    WHITESPACE@73..74 " "
                    ATOM@74..76
                      ATOM@74..76 "ok"
                    WHITESPACE@76..77 " "
                    ANON_OF@77..79 "of"
                    WHITESPACE@79..100 "\n                    "
                    VAR@100..101
                      VAR@100..101 "X"
                    WHITESPACE@101..102 " "
                    ANON_DASH_GT@102..104 "->""#]],
        )
    }

    fn check_node(input: &str, expected: Expect) {
        let parse = ast::SourceFile::parse_text(input);

        let actual_tree = format!("{:#?}", parse.syntax_node());
        // We cut off the last byte because formatting the SyntaxNode adds on a newline at the end.
        expected.assert_eq(&actual_tree[0..actual_tree.len() - 1]);

        let expected_range = TextRange::new(TextSize::from(0), TextSize::of(input));
        assert_eq!(parse.syntax_node().text_range(), expected_range);
    }

    /// This test does not assert anything and instead just shows off the crate's
    /// API.
    #[test]
    fn api_walkthrough() {
        // use ast::{ModuleItemOwner, NameOwner};

        let source_code = "-module(foo).\nfoo(Bar) -> 1 + 1.";

        // `SourceFile` is the main entry point.
        //
        // The `parse` method returns a `Parse` -- a pair of syntax tree and a list
        // of errors. That is, syntax tree is constructed even in presence of errors.
        let parse = ast::SourceFile::parse_text(source_code);
        assert!(parse.errors().is_empty());

        // The `tree` method returns an owned syntax node of type `SourceFile`.
        // Owned nodes are cheap: inside, they are `Rc` handles to the underling data.
        let file: ast::SourceFile = parse.tree();

        // `SourceFile` is the root of the syntax tree. We can iterate file's items.
        // Let's fetch the three top level forms.
        let mut module_attr = None;
        let mut func = None;
        for item in file.forms() {
            match item {
                ast::Form::ModuleAttribute(f) => module_attr = Some(f),
                ast::Form::FunDecl(f) => func = Some(f),
                x => panic!("{:?}", x),
                // _ => unreachable!(),
            }
        }
        let module: ast::ModuleAttribute = module_attr.unwrap();
        expect![[r#"
            ModuleAttribute {
                syntax: MODULE_ATTRIBUTE@0..13
                  ANON_DASH@0..1 "-"
                  ANON_MODULE@1..7 "module"
                  ANON_LPAREN@7..8 "("
                  ATOM@8..11
                    ATOM@8..11 "foo"
                  ANON_RPAREN@11..12 ")"
                  ANON_DOT@12..13 "."
                ,
            }"#]]
        .assert_eq(format!("{:#?}", module).as_str());

        match module.name().unwrap() {
            ast::Name::Atom(name) => assert_eq!(name.raw_text(), "foo"),
            ast::Name::MacroCallExpr(_) | ast::Name::Var(_) => {
                panic!("unexpected name: {:?}", module.name())
            }
        }

        let fun: ast::FunDecl = func.unwrap();
        expect![[r#"
            FunDecl {
                syntax: FUN_DECL@14..32
                  FUNCTION_CLAUSE@14..31
                    ATOM@14..17
                      ATOM@14..17 "foo"
                    EXPR_ARGS@17..22
                      ANON_LPAREN@17..18 "("
                      VAR@18..21
                        VAR@18..21 "Bar"
                      ANON_RPAREN@21..22 ")"
                    WHITESPACE@22..23 " "
                    CLAUSE_BODY@23..31
                      ANON_DASH_GT@23..25 "->"
                      WHITESPACE@25..26 " "
                      BINARY_OP_EXPR@26..31
                        INTEGER@26..27
                          INTEGER@26..27 "1"
                        WHITESPACE@27..28 " "
                        ANON_PLUS@28..29 "+"
                        WHITESPACE@29..30 " "
                        INTEGER@30..31
                          INTEGER@30..31 "1"
                  ANON_DOT@31..32 "."
                ,
            }"#]]
        .assert_eq(format!("{:#?}", fun).as_str());
        let fun_clauses = fun.clauses();
        assert_eq!(fun.clauses().count(), 1);

        let clauses_cast: Vec<ast::FunctionClause> = file
            .syntax()
            .descendants()
            .filter_map(ast::FunctionClause::cast)
            .collect();
        expect![[r#"
            FunctionClause {
                syntax: FUNCTION_CLAUSE@14..31
                  ATOM@14..17
                    ATOM@14..17 "foo"
                  EXPR_ARGS@17..22
                    ANON_LPAREN@17..18 "("
                    VAR@18..21
                      VAR@18..21 "Bar"
                    ANON_RPAREN@21..22 ")"
                  WHITESPACE@22..23 " "
                  CLAUSE_BODY@23..31
                    ANON_DASH_GT@23..25 "->"
                    WHITESPACE@25..26 " "
                    BINARY_OP_EXPR@26..31
                      INTEGER@26..27
                        INTEGER@26..27 "1"
                      WHITESPACE@27..28 " "
                      ANON_PLUS@28..29 "+"
                      WHITESPACE@29..30 " "
                      INTEGER@30..31
                        INTEGER@30..31 "1"
                ,
            }"#]]
        .assert_eq(format!("{:#?}", clauses_cast[0]).as_str());
        let function_clause = clauses_cast[0].clone();

        let mut clause = None;
        for item in fun_clauses {
            match item {
                ast::FunctionOrMacroClause::FunctionClause(f) => clause = Some(f),
                x => panic!("{:?}", x),
                // _ => unreachable!(),
            }
        }
        expect![[r#"
            FunctionClause {
                syntax: FUNCTION_CLAUSE@14..31
                  ATOM@14..17
                    ATOM@14..17 "foo"
                  EXPR_ARGS@17..22
                    ANON_LPAREN@17..18 "("
                    VAR@18..21
                      VAR@18..21 "Bar"
                    ANON_RPAREN@21..22 ")"
                  WHITESPACE@22..23 " "
                  CLAUSE_BODY@23..31
                    ANON_DASH_GT@23..25 "->"
                    WHITESPACE@25..26 " "
                    BINARY_OP_EXPR@26..31
                      INTEGER@26..27
                        INTEGER@26..27 "1"
                      WHITESPACE@27..28 " "
                      ANON_PLUS@28..29 "+"
                      WHITESPACE@29..30 " "
                      INTEGER@30..31
                        INTEGER@30..31 "1"
                ,
            }"#]]
        .assert_eq(format!("{:#?}", clause.unwrap()).as_str());

        // Each AST node has a bunch of getters for children. All getters return
        // `Option`s though, to account for incomplete code. Some getters are common
        // for several kinds of node. In this case, a trait like `ast::NameOwner`
        // usually exists. By convention, all ast types should be used with `ast::`
        // qualifier.
        let name = function_clause.name();
        let name = match name.unwrap() {
            ast::Name::Atom(a) => a,
            ast::Name::Var(_) | ast::Name::MacroCallExpr(_) => todo!(),
        };
        assert_eq!(name.raw_text(), "foo");

        // Let's get the `1 + 1` expression!
        // let body: ast::BlockExpr = func.body().unwrap();
        // let expr: ast::Expr = body.tail_expr().unwrap();

        let body = function_clause.body().unwrap();
        let mut oexpr = None;
        for e in body.exprs() {
            oexpr = Some(e);
        }
        let expr = oexpr.unwrap();

        // Enums are used to group related ast nodes together, and can be used for
        // matching. However, because there are no public fields, it's possible to
        // match only the top level enum: that is the price we pay for increased API
        // flexibility
        let bin_expr: &ast::BinaryOpExpr = match &expr {
            ast::Expr::BinaryOpExpr(e) => e,
            _ => unreachable!(),
        };
        expect![[r#"
            BinaryOpExpr {
                syntax: BINARY_OP_EXPR@26..31
                  INTEGER@26..27
                    INTEGER@26..27 "1"
                  WHITESPACE@27..28 " "
                  ANON_PLUS@28..29 "+"
                  WHITESPACE@29..30 " "
                  INTEGER@30..31
                    INTEGER@30..31 "1"
                ,
            }"#]]
        .assert_eq(format!("{:#?}", bin_expr).as_str());

        // Besides the "typed" AST API, there's an untyped CST one as well.
        // To switch from AST to CST, call `.syntax()` method:
        let expr_syntax: &SyntaxNode = expr.syntax();

        // Note how `expr` and `bin_expr` are in fact the same node underneath:
        assert!(expr_syntax == bin_expr.syntax());

        // To go from CST to AST, `AstNode::cast` function is used:
        let _expr: ast::Expr = match ast::Expr::cast(expr_syntax.clone()) {
            Some(e) => e,
            None => unreachable!(),
        };

        // The two properties each syntax node has is a `SyntaxKind`:
        assert_eq!(expr_syntax.kind(), SyntaxKind::BINARY_OP_EXPR);

        // And text range:
        assert_eq!(
            expr_syntax.text_range(),
            TextRange::new(26.into(), 31.into())
        );

        // You can get node's text as a `SyntaxText` object, which will traverse the
        // tree collecting token's text:
        let text: SyntaxText = expr_syntax.text();
        assert_eq!(text.to_string(), "1 + 1");

        // There's a bunch of traversal methods on `SyntaxNode`:
        assert_eq!(expr_syntax.parent().as_ref(), Some(body.syntax()));
        assert_eq!(
            body.syntax().first_child_or_token().map(|it| it.kind()),
            Some(SyntaxKind::ANON_DASH_GT)
        );
        // assert_eq!(
        //     expr_syntax.next_sibling_or_token().map(|it| it.kind()),
        //     Some(SyntaxKind::WHITESPACE)
        // );

        // As well as some iterator helpers:
        let f = expr_syntax.ancestors().find_map(ast::FunDecl::cast);
        assert_eq!(f.unwrap(), fun);
        assert!(
            expr_syntax
                .siblings_with_tokens(Direction::Next)
                .any(|it| it.kind() == SyntaxKind::BINARY_OP_EXPR)
        );

        assert_eq!(
            expr_syntax.descendants_with_tokens().count(),
            8, // 5 tokens `1`, ` `, `+`, ` `, `1`
               // 2 literal expressions: `1`, `1`
               // 1 the node itself: `1 + 1`
        );

        // There's also a `preorder` method with a more fine-grained iteration control:
        let mut buf = String::new();
        let mut indent = 0;
        for event in expr_syntax.preorder_with_tokens() {
            match event {
                WalkEvent::Enter(node) => {
                    let text = match &node {
                        NodeOrToken::Node(it) => it.text().to_string(),
                        NodeOrToken::Token(it) => it.text().to_string(),
                    };
                    format_to!(
                        buf,
                        "{:indent$}{:?} {:?}\n",
                        " ",
                        text,
                        node.kind(),
                        indent = indent
                    );
                    indent += 2;
                }
                WalkEvent::Leave(_) => indent -= 2,
            }
        }
        assert_eq!(indent, 0);
        expect![[r#"
            "1 + 1" BINARY_OP_EXPR
              "1" INTEGER
                "1" INTEGER
              " " WHITESPACE
              "+" ANON_PLUS
              " " WHITESPACE
              "1" INTEGER
                "1" INTEGER"#]]
        .assert_eq(buf.trim());

        // To recursively process the tree, there are three approaches:
        // 1. explicitly call getter methods on AST nodes.
        // 2. use descendants and `AstNode::cast`.
        // 3. use descendants and `match_ast!`.
        //
        // Here's how the first one looks like:
        let exprs_cast: Vec<String> = file
            .syntax()
            .descendants()
            .filter_map(ast::Expr::cast)
            .map(|expr| expr.syntax().text().to_string())
            .collect();

        // An alternative is to use a macro.
        let mut exprs_visit = Vec::new();
        for node in file.syntax().descendants() {
            match_ast! {
                match node {
                    ast::Expr(it) => {
                        let res = it.syntax().text().to_string();
                        exprs_visit.push(res);
                    },
                    _ => (),
                }
            }
        }
        assert_eq!(exprs_cast, exprs_visit);
    }

    #[test]
    fn char_escapes() {
        let source_code = r#"foo(Bar) -> [$\$, $\r, $\s, $\n, $\t]."#;

        let parse = ast::SourceFile::parse_text(source_code);
        assert_eq!(format!("{:?}", parse.errors()), "[]");
        assert!(parse.errors().is_empty());
    }

    #[test]
    fn validate_missing_cleanup() {
        // Test reporting of missing field value.
        let source_code = r#"f() -> #name{field = }."#;
        let parse = ast::SourceFile::parse_text(source_code);

        expect!["[Error(19..20)]"].assert_eq(format!("{:?}", parse.errors()).as_str());
    }

    #[test]
    fn rowan() {
        let source_code = r#"
-if (true).
-define(CASE_START_PEER_NODE,
    case true of
        Node ->
            Node;
).
-else.
-define(CASE_START_PEER_NODE,
    case true of
        Node ->
            Node;).
-endif.
"#;

        let parse = ast::SourceFile::parse_text(source_code);
        expect![[r#"
            [
                Error(
                    94..96,
                ),
                Error(
                    102..103,
                ),
                Error(
                    184..186,
                ),
            ]
        "#]]
        .assert_debug_eq(&parse.errors().iter().collect::<Vec<_>>());
        expect!["SourceFile { syntax: SOURCE_FILE@0..194 }"]
            .assert_eq(format!("{:?}", parse.tree()).as_str());
    }

    #[test]
    fn identify_non_form_code() {
        // We can now parse a file containing only expressions at the top level, typically used for `consult`.
        // Check that we can distinguish them from normal files
        let source_code = r#"
             {deps, []}.

             {escript_incl_apps, [erlang_service]}.
             {escript_main_app, erlang_service}.
             {escript_name, erlang_service}.
             {escript_emu_args, "%%! +sbtu +A0 +sbwt none +sbwtdcpu none +sbwtdio none -noinput -mode minimal -escript main erlang_service -enable-feature maybe_expr\n"}.

             {base_dir, "../../../../buck-out/elp/erlang_service"}.

             %% Profiles
             {profiles, [
                 {debug, [{erl_opts, [debug_info, {d, 'DEBUG'}]}]},
                 {release, [{erl_opts, [no_debug_info, deterministic]}]}
             ]}.
             "#;
        let parse = ast::SourceFile::parse_text(source_code);
        assert_eq!(true, parse.tree().is_erlang_config_file());
    }

    #[test]
    fn identify_form_code() {
        // We can now parse a file containing only expressions at the top level, typically used for `consult`.
        // Check that we can distinguish them from normal files
        let source_code = r#"
             -module(main).
             foo() -> ok.
             "#;
        let parse = ast::SourceFile::parse_text(source_code);
        assert_eq!(false, parse.tree().is_erlang_config_file());
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Various extension methods to ast Nodes, which are hard to code-generate.
//! Extensions for various expressions live in a sibling `expr_extensions` module.

use std::borrow::Cow;

use lazy_static::lazy_static;
use regex::Regex;
use rowan::GreenNodeData;
use rowan::GreenTokenData;
use rowan::NodeOrToken;
use rowan::TextRange;
use smol_str::SmolStr;
use stdx::trim_indent;

use super::generated::nodes;
use super::operators::GeneratorOp;
use super::ArithOp;
use super::BinaryOp;
use super::CompOp;
use super::FunctionOrMacroClause;
use super::ListOp;
use super::LogicOp;
use super::MapOp;
use super::Name;
use super::Ordering;
use super::PpDefine;
use super::UnaryOp;
use crate::ast::AstNode;
use crate::ast::SyntaxNode;
use crate::token_text::TokenText;
use crate::unescape;
use crate::unescape::unescape_string;
use crate::SyntaxKind;
use crate::SyntaxKind::*;
use crate::SyntaxToken;

impl nodes::MacroName {
    pub fn raw_text(&self) -> TokenText<'_> {
        match self {
            nodes::MacroName::Atom(a) => a.raw_text(),
            nodes::MacroName::Var(v) => v.text(),
        }
    }
    pub fn text(&self) -> Option<String> {
        unescape_string(&self.raw_text()).map(|cow| cow.to_string())
    }
}

impl nodes::FunDecl {
    pub fn name(&self) -> Option<Name> {
        match self.clause() {
            Some(c) => match c {
                nodes::FunctionOrMacroClause::FunctionClause(c) => c.name(),
                nodes::FunctionOrMacroClause::MacroCallExpr(_) => None,
            },
            None => None,
        }
    }
    pub fn clauses(&self) -> impl Iterator<Item = FunctionOrMacroClause> {
        self.clause().into_iter()
    }

    pub fn separator(&self) -> Option<(ClauseSeparator, SyntaxToken)> {
        clause_separator(self.syntax())
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ClauseSeparator {
    Missing,
    Semi,
    Dot,
}

fn clause_separator(parent: &SyntaxNode) -> Option<(ClauseSeparator, SyntaxToken)> {
    parent
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .filter(|it| it.kind() != WHITESPACE && it.kind() != COMMENT)
        .find_map(|c| Some((match_clause_separator(&c)?, c)))
}

fn match_clause_separator(c: &SyntaxToken) -> Option<ClauseSeparator> {
    match c.kind() {
        ANON_DOT => Some(ClauseSeparator::Dot),
        ANON_SEMI => Some(ClauseSeparator::Semi),
        _ => Some(ClauseSeparator::Missing),
    }
}

impl nodes::Atom {
    pub fn raw_text(&self) -> TokenText {
        text_of_token(self.syntax())
    }

    pub fn text(&self) -> Option<String> {
        unescape_string(&self.raw_text()).map(|cow| cow.to_string())
    }
}

pub const SSR_PLACEHOLDER_PREFIX: &'static str = "_@";

impl nodes::Var {
    pub fn text(&self) -> TokenText {
        text_of_token(self.syntax())
    }

    /// Returns true if this is a SSR placeholder, when parsing an SSR
    /// template.
    /// This is recognised by having a prefix of `_@`
    pub fn is_ssr_placeholder(&self) -> bool {
        self.text().starts_with(SSR_PLACEHOLDER_PREFIX)
    }
}

impl nodes::Char {
    pub fn text(&self) -> TokenText {
        text_of_token(self.syntax())
    }
}

impl nodes::Float {
    pub fn text(&self) -> TokenText {
        text_of_token(self.syntax())
    }
}

impl nodes::Integer {
    pub fn text(&self) -> TokenText {
        text_of_token(self.syntax())
    }
}

impl nodes::String {
    pub fn text(&self) -> TokenText {
        text_of_token(self.syntax())
    }
}

fn text_of_token(node: &SyntaxNode) -> TokenText {
    fn first_token(green_ref: &GreenNodeData) -> &GreenTokenData {
        green_ref
            .children()
            .next()
            .and_then(NodeOrToken::into_token)
            .unwrap()
    }

    match node.green() {
        Cow::Borrowed(green_ref) => TokenText::borrowed(first_token(green_ref).text()),
        Cow::Owned(green) => TokenText::owned(first_token(&green).to_owned()),
    }
}

// ---------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Comment {
    pub(crate) syntax: SyntaxNode,
}

impl AstNode for Comment {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == COMMENT
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}

// ---------------------------------------------------------------------

/// Used for macro arity definitions. It distinguishes between
///  `-define(FOO,x).  => None
///  `-define(FOO(),x).  => Some(0)
pub type Arity = Option<usize>;

pub trait HasArity {
    fn arity_value(&self) -> Arity;
}

impl HasArity for super::PpDefine {
    fn arity_value(&self) -> super::Arity {
        self.arity()
    }
}

impl HasArity for super::Fa {
    fn arity_value(&self) -> Arity {
        self.arity()?.value()?.arity_value()
    }
}

impl HasArity for super::TypeSig {
    fn arity_value(&self) -> Arity {
        Some(self.args()?.args().count())
    }
}

impl HasArity for super::Spec {
    fn arity_value(&self) -> Arity {
        // We assume all the signatures have the same arity
        self.sigs().next()?.arity_value()
    }
}

impl HasArity for super::FunDecl {
    fn arity_value(&self) -> Arity {
        self.clause()?.arity_value()
    }
}

impl HasArity for super::FunctionOrMacroClause {
    fn arity_value(&self) -> Arity {
        match self {
            super::FunctionOrMacroClause::FunctionClause(it) => it.arity_value(),
            super::FunctionOrMacroClause::MacroCallExpr(it) => it.arity_value(),
        }
    }
}

impl HasArity for super::FunctionClause {
    fn arity_value(&self) -> Arity {
        Some(self.args()?.args().count())
    }
}

impl HasArity for super::FunClause {
    fn arity_value(&self) -> Arity {
        Some(self.args()?.args().count())
    }
}

impl HasArity for super::MacroCallExpr {
    fn arity_value(&self) -> Arity {
        self.arity()
    }
}

impl HasArity for super::Call {
    fn arity_value(&self) -> Arity {
        Some(self.args()?.args().count())
    }
}

impl HasArity for super::Callback {
    fn arity_value(&self) -> Arity {
        Some(self.sigs().next()?.args()?.args().count())
    }
}

impl HasArity for super::ArityValue {
    fn arity_value(&self) -> Arity {
        match self {
            super::ArityValue::Integer(i) => Some((*i).clone().into()),
            super::ArityValue::MacroCallExpr(_) => None,
            super::ArityValue::Var(_) => None,
        }
    }
}

impl HasArity for super::InternalFun {
    fn arity_value(&self) -> Arity {
        self.arity()?.value()?.arity_value()
    }
}

impl HasArity for super::ExternalFun {
    fn arity_value(&self) -> Arity {
        self.arity()?.value()?.arity_value()
    }
}

impl HasArity for super::TypeAlias {
    fn arity_value(&self) -> Arity {
        Some(self.name()?.args()?.args().count())
    }
}

impl HasArity for super::Opaque {
    fn arity_value(&self) -> Arity {
        Some(self.name()?.args()?.args().count())
    }
}

impl HasArity for super::ExprArgs {
    fn arity_value(&self) -> Arity {
        Some(self.args().count())
    }
}

impl HasArity for super::MacroCallArgs {
    fn arity_value(&self) -> Arity {
        Some(self.args().count())
    }
}

// ---------------------------------------------------------------------

impl PpDefine {
    pub fn args(&self) -> impl Iterator<Item = nodes::Var> {
        self.lhs()
            .into_iter()
            .flat_map(|args| args.args())
            .flat_map(|args| args.args())
    }

    pub fn name(&self) -> Option<nodes::MacroName> {
        self.lhs()?.name()
    }

    pub fn arity(&self) -> Arity {
        self.lhs()?.args().map(|_| self.args().count())
    }
}

impl super::MacroCallExpr {
    pub fn arity(&self) -> Option<usize> {
        self.args().map(|args| args.args().count())
    }
}

// ---------------------------------------------------------------------

impl super::ExprMax {
    pub fn name(&self) -> Option<SmolStr> {
        match self {
            super::ExprMax::Atom(it) => Some(it.text()?.into()),
            super::ExprMax::Var(it) => Some(it.text().into()),
            super::ExprMax::ParenExpr(it) => match it.expr()? {
                super::Expr::ExprMax(e) => e.name(),
                _ => None,
            },
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------

impl Name {
    pub fn text(&self) -> Option<String> {
        match self {
            Name::Atom(atom) => atom.text(),
            Name::MacroCallExpr(_) => None,
            Name::Var(v) => Some(v.text().to_string()),
        }
    }
}

// ---------------------------------------------------------------------

impl nodes::PpInclude {
    pub fn text_range(&self) -> TextRange {
        self.syntax.text_range()
    }
}
impl nodes::PpIncludeLib {
    pub fn text_range(&self) -> TextRange {
        self.syntax.text_range()
    }
}

// ---------------------------------------------------------------------
// rust standard types conversions
// ---------------------------------------------------------------------

impl nodes::Integer {
    /// Erlang Integer literals can have underscores. Return text with
    /// them stripped out.
    fn clean_text(self) -> String {
        let text: std::string::String = From::from(self.syntax.text());
        text.replace('_', "")
    }
}

impl From<nodes::Integer> for usize {
    fn from(i: nodes::Integer) -> Self {
        i.clean_text().trim().parse().unwrap()
    }
}
impl From<nodes::Integer> for u32 {
    fn from(i: nodes::Integer) -> Self {
        i.clean_text().trim().parse().unwrap()
    }
}
impl From<nodes::Integer> for i32 {
    fn from(i: nodes::Integer) -> Self {
        i.clean_text().trim().parse().unwrap()
    }
}
impl From<nodes::String> for std::string::String {
    fn from(s: nodes::String) -> Self {
        let source: std::string::String = From::from(s.syntax.text());
        trim_quotes_and_sigils(&source)
    }
}

/// Trim leading and trailing quote marks and sigils.  It also dedents
/// triple quoted strings, and returns verbatim or quoted strings
/// according to the sigil
///  No sigil     : Quoted (unescaped) (not tq strings
///  `~s"` : Quoted (unescaped)
///  `~B"` : Verbatim, but unescape '"' and NL for SQ
///  `~S"` : Verbatim for tq, unescape '"' and NL for sq
///  `~"`  : Verbatim for tq, Quoted for single quote
///  `~b"` : Verbatim vor tq, Quoted (unescaped) for sq
fn trim_quotes_and_sigils(s: &str) -> String {
    lazy_static! {
        // See https://docs.rs/regex/latest/regex/#example-verbose-mode
        // And https://docs.rs/regex/latest/regex/#syntax
        static ref RE: Regex = Regex::new(r#"(?sx) # . match \n, whitespace ignored, allow comments
              ^~?([bBsS]?) # Optional sigil prefix                              (cap 1)
              ("+)         # followed by quotes, as a match group to count them (cap 2)
              (.*)         # The actual string contents we care about           (cap 3)
              $            # End of string. Do not match quotes, we trim separately
            "#).unwrap();
    }
    let mut quoted = true;
    let trimmed = if let Some(captures) = RE.captures(s) {
        let is_tq = captures[2].starts_with("\"\"\"");
        if captures.len() > 3 {
            if (captures[1] == *"B" || captures[1] == *"S")
                || (captures[1] == *"" && is_tq)
                || (captures[1] == *"b" && is_tq)
            {
                quoted = false;
            }
            if is_tq {
                let trimmed = trim_indent(&captures[3].trim_end_matches("\""));
                if let Some(rest) = trimmed.strip_suffix("\n") {
                    // tq string terminates with \n whitespace, quotes
                    rest.to_string()
                } else {
                    trimmed
                }
            } else {
                if captures[1] == *"B" || captures[1] == *"S" {
                    // Verbatim non-tq string. Only escape processed
                    // is for `\"`, to avoid clash with final terminator.
                    captures[3]
                        .trim_end_matches("\"")
                        .to_string()
                        .replace("\\\"", "\"")
                } else {
                    captures[3].trim_end_matches("\"").to_string()
                }
            }
        } else {
            captures[0].to_string()
        }
    } else {
        s.to_string()
    };
    if quoted {
        unescape::unescape_string(&format!("\"{trimmed}\""))
            .map(|s| s.to_string())
            .unwrap_or(trimmed)
    } else {
        trimmed
    }
}

// Generator types
impl super::Generator {
    pub fn op(&self) -> Option<(GeneratorOp, SyntaxToken)> {
        generator_op(&self.syntax())
    }

    pub fn strict(&self) -> bool {
        self.op()
            .map(|(op, _)| op == GeneratorOp::Plain { strict: true })
            .unwrap_or(false)
    }
}

impl super::BGenerator {
    pub fn op(&self) -> Option<(GeneratorOp, SyntaxToken)> {
        generator_op(&self.syntax())
    }

    pub fn strict(&self) -> bool {
        self.op()
            .map(|(op, _)| op == GeneratorOp::Binary { strict: true })
            .unwrap_or(false)
    }
}

impl super::MapGenerator {
    pub fn op(&self) -> Option<(GeneratorOp, SyntaxToken)> {
        generator_op(&self.syntax())
    }

    pub fn strict(&self) -> bool {
        self.op()
            .map(|(op, _)| op == GeneratorOp::Plain { strict: true })
            .unwrap_or(false)
    }
}

fn generator_op(parent: &SyntaxNode) -> Option<(GeneratorOp, SyntaxToken)> {
    parent
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .find_map(|c| Some((match_generator_op(&c)?, c)))
}

fn match_generator_op(c: &SyntaxToken) -> Option<GeneratorOp> {
    match c.kind() {
        ANON_LT_DASH => Some(GeneratorOp::Plain { strict: false }),
        ANON_LT_COLON_DASH => Some(GeneratorOp::Plain { strict: true }),
        ANON_LT_EQ => Some(GeneratorOp::Binary { strict: false }),
        ANON_LT_COLON_EQ => Some(GeneratorOp::Binary { strict: true }),
        _ => None,
    }
}

// Operators

impl super::UnaryOpExpr {
    pub fn op(&self) -> Option<(UnaryOp, SyntaxToken)> {
        unary_op(self.syntax())
    }
}

impl super::BinaryOpExpr {
    pub fn op(&self) -> Option<(BinaryOp, SyntaxToken)> {
        binary_op(self.syntax())
    }
}

impl super::MapField {
    pub fn op(&self) -> Option<(MapOp, SyntaxToken)> {
        map_op(self.syntax())
    }
}

fn unary_op(parent: &SyntaxNode) -> Option<(UnaryOp, SyntaxToken)> {
    parent
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .find_map(|c| Some((match_unary_op(&c)?, c)))
}

fn binary_op(parent: &SyntaxNode) -> Option<(BinaryOp, SyntaxToken)> {
    parent
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .find_map(|c| Some((match_binary_op(&c)?, c)))
}

fn map_op(parent: &SyntaxNode) -> Option<(MapOp, SyntaxToken)> {
    parent
        .children_with_tokens()
        .filter_map(|it| it.into_token())
        .find_map(|c| Some((match_map_op(&c)?, c)))
}

fn match_unary_op(c: &SyntaxToken) -> Option<UnaryOp> {
    match c.kind() {
        ANON_PLUS => Some(UnaryOp::Plus),
        ANON_DASH => Some(UnaryOp::Minus),
        ANON_BNOT => Some(UnaryOp::Bnot),
        ANON_NOT => Some(UnaryOp::Not),
        _ => None,
    }
}

fn match_binary_op(c: &SyntaxToken) -> Option<BinaryOp> {
    match c.kind() {
        ANON_AND => Some(BinaryOp::LogicOp(LogicOp::And { lazy: false })),
        ANON_ANDALSO => Some(BinaryOp::LogicOp(LogicOp::And { lazy: true })),
        ANON_OR => Some(BinaryOp::LogicOp(LogicOp::Or { lazy: false })),
        ANON_ORELSE => Some(BinaryOp::LogicOp(LogicOp::Or { lazy: true })),
        ANON_XOR => Some(BinaryOp::LogicOp(LogicOp::Xor)),

        ANON_DASH_DASH => Some(BinaryOp::ListOp(ListOp::Subtract)),
        ANON_PLUS_PLUS => Some(BinaryOp::ListOp(ListOp::Append)),

        ANON_BANG => Some(BinaryOp::Send),

        ANON_BAND => Some(BinaryOp::ArithOp(ArithOp::Band)),
        ANON_BOR => Some(BinaryOp::ArithOp(ArithOp::Bor)),
        ANON_BSL => Some(BinaryOp::ArithOp(ArithOp::Bsl)),
        ANON_BSR => Some(BinaryOp::ArithOp(ArithOp::Bsr)),
        ANON_BXOR => Some(BinaryOp::ArithOp(ArithOp::Bxor)),
        ANON_DASH => Some(BinaryOp::ArithOp(ArithOp::Sub)),
        ANON_DIV => Some(BinaryOp::ArithOp(ArithOp::Div)),
        ANON_PLUS => Some(BinaryOp::ArithOp(ArithOp::Add)),
        ANON_REM => Some(BinaryOp::ArithOp(ArithOp::Rem)),
        ANON_SLASH => Some(BinaryOp::ArithOp(ArithOp::FloatDiv)),
        ANON_STAR => Some(BinaryOp::ArithOp(ArithOp::Mul)),

        ANON_EQ_COLON_EQ => Some(BinaryOp::CompOp(CompOp::Eq {
            strict: true,
            negated: false,
        })),
        ANON_EQ_EQ => Some(BinaryOp::CompOp(CompOp::Eq {
            strict: false,
            negated: false,
        })),
        ANON_EQ_SLASH_EQ => Some(BinaryOp::CompOp(CompOp::Eq {
            strict: true,
            negated: true,
        })),
        ANON_SLASH_EQ => Some(BinaryOp::CompOp(CompOp::Eq {
            strict: false,
            negated: true,
        })),
        ANON_EQ_LT => Some(BinaryOp::CompOp(CompOp::Ord {
            ordering: Ordering::Less,
            strict: false,
        })),
        ANON_GT => Some(BinaryOp::CompOp(CompOp::Ord {
            ordering: Ordering::Greater,
            strict: true,
        })),
        ANON_GT_EQ => Some(BinaryOp::CompOp(CompOp::Ord {
            ordering: Ordering::Greater,
            strict: false,
        })),
        ANON_LT => Some(BinaryOp::CompOp(CompOp::Ord {
            ordering: Ordering::Less,
            strict: true,
        })),

        _ => None,
    }
}

fn match_map_op(c: &SyntaxToken) -> Option<MapOp> {
    match c.kind() {
        ANON_COLON_EQ => Some(MapOp::Exact),
        ANON_EQ_GT => Some(MapOp::Assoc),
        _ => None,
    }
}

// ---------------------------------------------------------------------
// conversions between different "expr" types
// ---------------------------------------------------------------------

impl From<nodes::BitExpr> for nodes::Expr {
    fn from(expr: nodes::BitExpr) -> Self {
        match expr {
            nodes::BitExpr::ExprMax(expr) => nodes::Expr::ExprMax(expr),
            nodes::BitExpr::BinaryOpExpr(expr) => nodes::Expr::BinaryOpExpr(expr),
            nodes::BitExpr::UnaryOpExpr(expr) => nodes::Expr::UnaryOpExpr(expr),
        }
    }
}

impl From<nodes::RecordExprBase> for nodes::Expr {
    fn from(expr: nodes::RecordExprBase) -> Self {
        match expr {
            nodes::RecordExprBase::ExprMax(expr) => nodes::Expr::ExprMax(expr),
            nodes::RecordExprBase::RecordExpr(expr) => nodes::Expr::RecordExpr(expr),
            nodes::RecordExprBase::RecordFieldExpr(expr) => nodes::Expr::RecordFieldExpr(expr),
            nodes::RecordExprBase::RecordIndexExpr(expr) => nodes::Expr::RecordIndexExpr(expr),
            nodes::RecordExprBase::RecordUpdateExpr(expr) => nodes::Expr::RecordUpdateExpr(expr),
        }
    }
}

impl From<nodes::MapExprBase> for nodes::Expr {
    fn from(expr: nodes::MapExprBase) -> Self {
        match expr {
            nodes::MapExprBase::ExprMax(expr) => nodes::Expr::ExprMax(expr),
            nodes::MapExprBase::MapExpr(expr) => nodes::Expr::MapExpr(expr),
            nodes::MapExprBase::MapExprUpdate(expr) => nodes::Expr::MapExprUpdate(expr),
        }
    }
}

impl From<nodes::Name> for nodes::Expr {
    fn from(expr: nodes::Name) -> Self {
        match expr {
            Name::Atom(atom) => nodes::Expr::ExprMax(nodes::ExprMax::Atom(atom)),
            Name::MacroCallExpr(mac) => nodes::Expr::ExprMax(nodes::ExprMax::MacroCallExpr(mac)),
            Name::Var(var) => nodes::Expr::ExprMax(nodes::ExprMax::Var(var)),
        }
    }
}

impl From<nodes::ArityValue> for nodes::Expr {
    fn from(expr: nodes::ArityValue) -> Self {
        match expr {
            nodes::ArityValue::Integer(int) => nodes::Expr::ExprMax(nodes::ExprMax::Integer(int)),
            nodes::ArityValue::MacroCallExpr(mac) => {
                nodes::Expr::ExprMax(nodes::ExprMax::MacroCallExpr(mac))
            }
            nodes::ArityValue::Var(var) => nodes::Expr::ExprMax(nodes::ExprMax::Var(var)),
        }
    }
}

impl From<nodes::CatchPat> for nodes::Expr {
    fn from(expr: nodes::CatchPat) -> Self {
        match expr {
            nodes::CatchPat::ExprMax(expr_max) => nodes::Expr::ExprMax(expr_max),
            nodes::CatchPat::BinaryOpExpr(binary_op) => nodes::Expr::BinaryOpExpr(binary_op),
            nodes::CatchPat::MapExpr(map) => nodes::Expr::MapExpr(map),
            nodes::CatchPat::RecordExpr(record) => nodes::Expr::RecordExpr(record),
            nodes::CatchPat::RecordIndexExpr(index) => nodes::Expr::RecordIndexExpr(index),
            nodes::CatchPat::UnaryOpExpr(unary_op) => nodes::Expr::UnaryOpExpr(unary_op),
        }
    }
}

impl From<nodes::Var> for nodes::Expr {
    fn from(var: nodes::Var) -> Self {
        nodes::Expr::ExprMax(nodes::ExprMax::Var(var))
    }
}

// ---------------------------------------------------------------------
// missing From implementations for nested forms
// ---------------------------------------------------------------------

impl From<nodes::PpDefine> for nodes::Form {
    fn from(node: nodes::PpDefine) -> nodes::Form {
        nodes::Form::PreprocessorDirective(node.into())
    }
}
impl From<nodes::PpElif> for nodes::Form {
    fn from(node: nodes::PpElif) -> nodes::Form {
        nodes::Form::PreprocessorDirective(node.into())
    }
}
impl From<nodes::PpElse> for nodes::Form {
    fn from(node: nodes::PpElse) -> nodes::Form {
        nodes::Form::PreprocessorDirective(node.into())
    }
}
impl From<nodes::PpEndif> for nodes::Form {
    fn from(node: nodes::PpEndif) -> nodes::Form {
        nodes::Form::PreprocessorDirective(node.into())
    }
}
impl From<nodes::PpIf> for nodes::Form {
    fn from(node: nodes::PpIf) -> nodes::Form {
        nodes::Form::PreprocessorDirective(node.into())
    }
}
impl From<nodes::PpIfdef> for nodes::Form {
    fn from(node: nodes::PpIfdef) -> nodes::Form {
        nodes::Form::PreprocessorDirective(node.into())
    }
}
impl From<nodes::PpIfndef> for nodes::Form {
    fn from(node: nodes::PpIfndef) -> nodes::Form {
        nodes::Form::PreprocessorDirective(node.into())
    }
}
impl From<nodes::PpInclude> for nodes::Form {
    fn from(node: nodes::PpInclude) -> nodes::Form {
        nodes::Form::PreprocessorDirective(node.into())
    }
}
impl From<nodes::PpIncludeLib> for nodes::Form {
    fn from(node: nodes::PpIncludeLib) -> nodes::Form {
        nodes::Form::PreprocessorDirective(node.into())
    }
}
impl From<nodes::PpUndef> for nodes::Form {
    fn from(node: nodes::PpUndef) -> nodes::Form {
        nodes::Form::PreprocessorDirective(node.into())
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::ast;

    fn parse_expr(arg: &str) -> ast::Expr {
        let text = format!("f() -> {}.", arg);
        let parse = ast::SourceFile::parse_text(&text);

        if !parse.errors().is_empty() {
            panic!("expected no parse errors, got: {:?}", parse.errors());
        }

        let fun = match parse.tree().forms().next().expect("no form parsed") {
            ast::Form::FunDecl(fun) => fun,
            got => panic!("expected fun, got: {:?}", got),
        };

        let clause = match fun.clauses().next().expect("no clauses parsed") {
            ast::FunctionOrMacroClause::FunctionClause(clause) => clause,
            got => panic!("expected clause, got: {:?}", got),
        };

        clause
            .body()
            .iter()
            .flat_map(|body| body.exprs())
            .next()
            .expect("no expression parsed")
    }

    #[test]
    fn test_unary_op_expr() {
        fn check(parse: &str, expected_op_text: &str, expected_op: UnaryOp) {
            let (op, token) = match parse_expr(parse) {
                ast::Expr::UnaryOpExpr(unary_op) => unary_op.op().expect("no operator found"),
                got => panic!("expected unary op, got: {:?}", got),
            };

            assert_eq!(token.text(), expected_op_text);
            assert_eq!(op.to_string(), expected_op_text);
            assert_eq!(op, expected_op);
        }

        check("+1", "+", UnaryOp::Plus);
        check("-1", "-", UnaryOp::Minus);
        check("not 1", "not", UnaryOp::Not);
        check("bnot 1", "bnot", UnaryOp::Bnot);
    }

    #[test]
    fn test_binary_op_expr() {
        fn check(parse: &str, expected_op_text: &str, expected_op: BinaryOp) {
            let (op, token) = match parse_expr(parse) {
                ast::Expr::BinaryOpExpr(binary_op) => binary_op.op().expect("no operator found"),
                got => panic!("expected binary op, got: {:?}", got),
            };

            assert_eq!(token.text(), expected_op_text);
            assert_eq!(op.to_string(), expected_op_text);
            assert_eq!(op, expected_op);
        }

        check(
            "X and Y",
            "and",
            BinaryOp::LogicOp(LogicOp::And { lazy: false }),
        );
        check(
            "X andalso Y",
            "andalso",
            BinaryOp::LogicOp(LogicOp::And { lazy: true }),
        );
        check(
            "X or Y",
            "or",
            BinaryOp::LogicOp(LogicOp::Or { lazy: false }),
        );
        check(
            "X orelse Y",
            "orelse",
            BinaryOp::LogicOp(LogicOp::Or { lazy: true }),
        );
        check("X xor Y", "xor", BinaryOp::LogicOp(LogicOp::Xor));

        check("X ++ Y", "++", BinaryOp::ListOp(ListOp::Append));
        check("X -- Y", "--", BinaryOp::ListOp(ListOp::Subtract));

        check("X ! Y", "!", BinaryOp::Send);

        check("X / Y", "/", BinaryOp::ArithOp(ArithOp::FloatDiv));
        check("X * Y", "*", BinaryOp::ArithOp(ArithOp::Mul));
        check("X - Y", "-", BinaryOp::ArithOp(ArithOp::Sub));
        check("X + Y", "+", BinaryOp::ArithOp(ArithOp::Add));
        check("X band Y", "band", BinaryOp::ArithOp(ArithOp::Band));
        check("X bor Y", "bor", BinaryOp::ArithOp(ArithOp::Bor));
        check("X bsl Y", "bsl", BinaryOp::ArithOp(ArithOp::Bsl));
        check("X bsr Y", "bsr", BinaryOp::ArithOp(ArithOp::Bsr));
        check("X div Y", "div", BinaryOp::ArithOp(ArithOp::Div));
        check("X rem Y", "rem", BinaryOp::ArithOp(ArithOp::Rem));

        check(
            "X =:= Y",
            "=:=",
            BinaryOp::CompOp(CompOp::Eq {
                strict: true,
                negated: false,
            }),
        );
        check(
            "X == Y",
            "==",
            BinaryOp::CompOp(CompOp::Eq {
                strict: false,
                negated: false,
            }),
        );
        check(
            "X =/= Y",
            "=/=",
            BinaryOp::CompOp(CompOp::Eq {
                strict: true,
                negated: true,
            }),
        );
        check(
            "X /= Y",
            "/=",
            BinaryOp::CompOp(CompOp::Eq {
                strict: false,
                negated: true,
            }),
        );
        check(
            "X =< Y",
            "=<",
            BinaryOp::CompOp(CompOp::Ord {
                ordering: Ordering::Less,
                strict: false,
            }),
        );
        check(
            "X > Y",
            ">",
            BinaryOp::CompOp(CompOp::Ord {
                ordering: Ordering::Greater,
                strict: true,
            }),
        );
        check(
            "X >= Y",
            ">=",
            BinaryOp::CompOp(CompOp::Ord {
                ordering: Ordering::Greater,
                strict: false,
            }),
        );
        check(
            "X < Y",
            "<",
            BinaryOp::CompOp(CompOp::Ord {
                ordering: Ordering::Less,
                strict: true,
            }),
        );
    }

    #[test]
    fn test_unary_map_expr() {
        fn check(parse: &str, expected_op_text: &str, expected_op: MapOp) {
            let parse = format!("#{{{}}}", parse);
            let map_expr = match parse_expr(&parse) {
                ast::Expr::MapExpr(map_expr) => map_expr,
                got => panic!("expected map expr, got: {:?}", got),
            };

            let field = map_expr.fields().next().expect("no map fields parsed");

            let (op, token) = field.op().unwrap();

            assert_eq!(token.text(), expected_op_text);
            assert_eq!(op.to_string(), expected_op_text);
            assert_eq!(op, expected_op);
        }

        check("X => Y", "=>", MapOp::Assoc);
        check("X := Y", ":=", MapOp::Exact);
    }

    #[test]
    fn test_trim_quotes_and_sigils() {
        // Note: \d escapes to ctrl-?, i.e. ^?

        // Quoted
        expect![[r#"ab"c""#]].assert_eq(&trim_quotes_and_sigils(r#"ab\"c\"\d"#));
        expect![[r#"ab"c""#]].assert_eq(&trim_quotes_and_sigils(r#""ab\"c\"\d""#));
        expect![[r#"ab"c""#]].assert_eq(&trim_quotes_and_sigils(r#"~"ab\"c\"\d""#));
        expect![[r#"ab"c""#]].assert_eq(&trim_quotes_and_sigils(r#"~s"ab\"c\"\d""#));
        expect![[r#"ab"c""#]].assert_eq(&trim_quotes_and_sigils(r#"~b"ab\"c\"\d""#));

        // Verbatim
        expect![[r#"ab"c"\d"#]].assert_eq(&trim_quotes_and_sigils(r#"~S"ab\"c\"\d""#));
        expect![[r#"ab"c"\d"#]].assert_eq(&trim_quotes_and_sigils(r#"~B"ab\"c\"\d""#));

        // Triple quoted strings -------------------
        // Quoted
        expect![[r#"ab"c"\d"#]].assert_eq(&trim_quotes_and_sigils(
            // Quoted Binary
            r#"~b"""
              ab"c"\d
              """"#,
        ));
        expect![[r#"ab"c""#]].assert_eq(&trim_quotes_and_sigils(
            // Quoted String
            r#"~s"""
              ab"c"\d
              """"#,
        ));

        // Verbatim
        expect![[r#"ab\"c\"\d"#]].assert_eq(&trim_quotes_and_sigils(
            // Normal (verbatim) string
            r#""""
              ab\"c\"\d
              """"#,
        ));
        expect![[r#"ab\"c\"\d"#]].assert_eq(&trim_quotes_and_sigils(
            // Verbatim String (explicit sigil, not needed)
            r#"~S"""
              ab\"c\"\d
              """"#,
        ));
        expect![[r#"ab\"c\"\d"#]].assert_eq(&trim_quotes_and_sigils(
            // Verbatim Binary (default sigil)
            r#"~"""
              ab\"c\"\d
              """"#,
        ));
        expect![[r#"ab\"c\"\d"#]].assert_eq(&trim_quotes_and_sigils(
            // Verbatim Binary (explicit sigil)
            r#"~B"""
              ab\"c\"\d
              """"#,
        ));

        // -----------------------------
        expect![[r#"
                     """
                    ab\"c\"\d"#]]
        .assert_eq(&trim_quotes_and_sigils(
            // Normal (verbatim) string, multiple `"` delimiters, with internal `"""`
            r#"""""
               """
              ab\"c\"\d
              """""#,
        ));
    }
}

//! @generated file, do not edit by hand, see `xtask/src/codegen.rs`

#![allow(dead_code)]
#[doc = r" TODO: remove this pragma"]
use crate::{
    ast::{support, AstChildren, AstNode},
    SyntaxKind::{self, *},
    SyntaxNode, SyntaxToken,
};
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnnType {
    pub(crate) syntax: SyntaxNode,
}
impl AnnType {
    pub fn ty(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn var(&self) -> Option<AnnVar> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for AnnType {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == ANN_TYPE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for AnnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnnVar {
    pub(crate) syntax: SyntaxNode,
}
impl AnnVar {
    pub fn var(&self) -> Option<Var> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for AnnVar {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == ANN_VAR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for AnnVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnonymousFun {
    pub(crate) syntax: SyntaxNode,
}
impl AnonymousFun {
    pub fn clauses(&self) -> AstChildren<FunClause> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for AnonymousFun {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == ANONYMOUS_FUN
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for AnonymousFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Arity {
    pub(crate) syntax: SyntaxNode,
}
impl Arity {
    pub fn value(&self) -> Option<ArityValue> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for Arity {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == ARITY
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for Arity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArityValue {
    Integer(Integer),
    MacroCallExpr(MacroCallExpr),
    Var(Var),
}
impl AstNode for ArityValue {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            INTEGER | MACRO_CALL_EXPR | VAR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            INTEGER => Some(ArityValue::Integer(Integer { syntax })),
            MACRO_CALL_EXPR => Some(ArityValue::MacroCallExpr(MacroCallExpr { syntax })),
            VAR => Some(ArityValue::Var(Var { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            ArityValue::Integer(it) => it.syntax(),
            ArityValue::MacroCallExpr(it) => it.syntax(),
            ArityValue::Var(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<Integer> for ArityValue {
    fn from(node: Integer) -> ArityValue {
        ArityValue::Integer(node)
    }
}
impl From<MacroCallExpr> for ArityValue {
    fn from(node: MacroCallExpr) -> ArityValue {
        ArityValue::MacroCallExpr(node)
    }
}
impl From<Var> for ArityValue {
    fn from(node: Var) -> ArityValue {
        ArityValue::Var(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for ArityValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Literal 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Atom {
    pub(crate) syntax: SyntaxNode,
}
#[doc = r" Via NodeType::Literal 2"]
impl Atom {
    pub fn self_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, ATOM, 0)
    }
}
#[doc = r" Via NodeType::Literal 2"]
impl AstNode for Atom {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == ATOM
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
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AttrName {
    pub(crate) syntax: SyntaxNode,
}
impl AttrName {
    pub fn name(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for AttrName {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == ATTR_NAME
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for AttrName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BGenerator {
    pub(crate) syntax: SyntaxNode,
}
impl BGenerator {
    pub fn lhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn rhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 1usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for BGenerator {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == B_GENERATOR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for BGenerator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BehaviourAttribute {
    pub(crate) syntax: SyntaxNode,
}
impl BehaviourAttribute {
    pub fn name(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for BehaviourAttribute {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == BEHAVIOUR_ATTRIBUTE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for BehaviourAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinElement {
    pub(crate) syntax: SyntaxNode,
}
impl BinElement {
    pub fn element(&self) -> Option<BitExpr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn size(&self) -> Option<BitSizeExpr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn types(&self) -> Option<BitTypeList> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for BinElement {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == BIN_ELEMENT
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for BinElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Binary {
    pub(crate) syntax: SyntaxNode,
}
impl Binary {
    pub fn elements(&self) -> AstChildren<BinElement> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for Binary {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == BINARY
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinaryComprehension {
    pub(crate) syntax: SyntaxNode,
}
impl BinaryComprehension {
    pub fn expr(&self) -> Option<ExprMax> {
        support::child(&self.syntax, 0usize)
    }
    pub fn lc_exprs(&self) -> Option<LcExprs> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for BinaryComprehension {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == BINARY_COMPREHENSION
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for BinaryComprehension {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinaryOpExpr {
    pub(crate) syntax: SyntaxNode,
}
impl BinaryOpExpr {
    pub fn lhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn rhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 1usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for BinaryOpExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == BINARY_OP_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for BinaryOpExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BitExpr {
    ExprMax(ExprMax),
    BinaryOpExpr(BinaryOpExpr),
    UnaryOpExpr(UnaryOpExpr),
}
impl AstNode for BitExpr {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ANONYMOUS_FUN | ATOM | BINARY | BINARY_COMPREHENSION | BLOCK_EXPR | CASE_EXPR
            | CHAR | CONCATABLES | EXTERNAL_FUN | FLOAT | FUN_TYPE | IF_EXPR | INTEGER
            | INTERNAL_FUN | LIST | LIST_COMPREHENSION | MACRO_CALL_EXPR | MACRO_STRING
            | MAP_COMPREHENSION | MAYBE_EXPR | PAREN_EXPR | RECEIVE_EXPR | STRING | TRY_EXPR
            | TUPLE | VAR | BINARY_OP_EXPR | UNARY_OP_EXPR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            ANONYMOUS_FUN | ATOM | BINARY | BINARY_COMPREHENSION | BLOCK_EXPR | CASE_EXPR
            | CHAR | CONCATABLES | EXTERNAL_FUN | FLOAT | FUN_TYPE | IF_EXPR | INTEGER
            | INTERNAL_FUN | LIST | LIST_COMPREHENSION | MACRO_CALL_EXPR | MACRO_STRING
            | MAP_COMPREHENSION | MAYBE_EXPR | PAREN_EXPR | RECEIVE_EXPR | STRING | TRY_EXPR
            | TUPLE | VAR => Some(BitExpr::ExprMax(ExprMax::cast(syntax).unwrap())),
            BINARY_OP_EXPR => Some(BitExpr::BinaryOpExpr(BinaryOpExpr { syntax })),
            UNARY_OP_EXPR => Some(BitExpr::UnaryOpExpr(UnaryOpExpr { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            BitExpr::ExprMax(it) => it.syntax(),
            BitExpr::BinaryOpExpr(it) => it.syntax(),
            BitExpr::UnaryOpExpr(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<ExprMax> for BitExpr {
    fn from(node: ExprMax) -> BitExpr {
        BitExpr::ExprMax(node)
    }
}
impl From<BinaryOpExpr> for BitExpr {
    fn from(node: BinaryOpExpr) -> BitExpr {
        BitExpr::BinaryOpExpr(node)
    }
}
impl From<UnaryOpExpr> for BitExpr {
    fn from(node: UnaryOpExpr) -> BitExpr {
        BitExpr::UnaryOpExpr(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for BitExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BitSizeExpr {
    pub(crate) syntax: SyntaxNode,
}
impl BitSizeExpr {
    pub fn size(&self) -> Option<BitExpr> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for BitSizeExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == BIT_SIZE_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for BitSizeExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BitType {
    Name(Name),
    BitTypeUnit(BitTypeUnit),
}
impl AstNode for BitType {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ATOM | MACRO_CALL_EXPR | VAR | BIT_TYPE_UNIT => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            ATOM | MACRO_CALL_EXPR | VAR => Some(BitType::Name(Name::cast(syntax).unwrap())),
            BIT_TYPE_UNIT => Some(BitType::BitTypeUnit(BitTypeUnit { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            BitType::Name(it) => it.syntax(),
            BitType::BitTypeUnit(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<Name> for BitType {
    fn from(node: Name) -> BitType {
        BitType::Name(node)
    }
}
impl From<BitTypeUnit> for BitType {
    fn from(node: BitTypeUnit) -> BitType {
        BitType::BitTypeUnit(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for BitType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BitTypeList {
    pub(crate) syntax: SyntaxNode,
}
impl BitTypeList {
    pub fn types(&self) -> AstChildren<BitType> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for BitTypeList {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == BIT_TYPE_LIST
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for BitTypeList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BitTypeUnit {
    pub(crate) syntax: SyntaxNode,
}
impl BitTypeUnit {
    pub fn size(&self) -> Option<ArityValue> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for BitTypeUnit {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == BIT_TYPE_UNIT
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for BitTypeUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockExpr {
    pub(crate) syntax: SyntaxNode,
}
impl BlockExpr {
    pub fn exprs(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for BlockExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == BLOCK_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for BlockExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call {
    pub(crate) syntax: SyntaxNode,
}
impl Call {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn args(&self) -> Option<ExprArgs> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for Call {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == CALL
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Callback {
    pub(crate) syntax: SyntaxNode,
}
impl Callback {
    pub fn fun(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
    pub fn module(&self) -> Option<Module> {
        support::child(&self.syntax, 0usize)
    }
    pub fn sigs(&self) -> AstChildren<TypeSig> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for Callback {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == CALLBACK
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for Callback {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CaseExpr {
    pub(crate) syntax: SyntaxNode,
}
impl CaseExpr {
    pub fn clauses(&self) -> AstChildren<CrClauseOrMacro> {
        support::children(&self.syntax)
    }
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for CaseExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == CASE_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for CaseExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CatchClause {
    pub(crate) syntax: SyntaxNode,
}
impl CatchClause {
    pub fn pat(&self) -> Option<CatchPat> {
        support::child(&self.syntax, 0usize)
    }
    pub fn body(&self) -> Option<ClauseBody> {
        support::child(&self.syntax, 0usize)
    }
    pub fn guard(&self) -> Option<Guard> {
        support::child(&self.syntax, 0usize)
    }
    pub fn class(&self) -> Option<TryClass> {
        support::child(&self.syntax, 0usize)
    }
    pub fn stack(&self) -> Option<TryStack> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for CatchClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == CATCH_CLAUSE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for CatchClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CatchExpr {
    pub(crate) syntax: SyntaxNode,
}
impl CatchExpr {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for CatchExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == CATCH_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for CatchExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CatchPat {
    ExprMax(ExprMax),
    BinaryOpExpr(BinaryOpExpr),
    MapExpr(MapExpr),
    RecordExpr(RecordExpr),
    RecordIndexExpr(RecordIndexExpr),
    UnaryOpExpr(UnaryOpExpr),
}
impl AstNode for CatchPat {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ANONYMOUS_FUN | ATOM | BINARY | BINARY_COMPREHENSION | BLOCK_EXPR | CASE_EXPR
            | CHAR | CONCATABLES | EXTERNAL_FUN | FLOAT | FUN_TYPE | IF_EXPR | INTEGER
            | INTERNAL_FUN | LIST | LIST_COMPREHENSION | MACRO_CALL_EXPR | MACRO_STRING
            | MAP_COMPREHENSION | MAYBE_EXPR | PAREN_EXPR | RECEIVE_EXPR | STRING | TRY_EXPR
            | TUPLE | VAR | BINARY_OP_EXPR | MAP_EXPR | RECORD_EXPR | RECORD_INDEX_EXPR
            | UNARY_OP_EXPR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            ANONYMOUS_FUN | ATOM | BINARY | BINARY_COMPREHENSION | BLOCK_EXPR | CASE_EXPR
            | CHAR | CONCATABLES | EXTERNAL_FUN | FLOAT | FUN_TYPE | IF_EXPR | INTEGER
            | INTERNAL_FUN | LIST | LIST_COMPREHENSION | MACRO_CALL_EXPR | MACRO_STRING
            | MAP_COMPREHENSION | MAYBE_EXPR | PAREN_EXPR | RECEIVE_EXPR | STRING | TRY_EXPR
            | TUPLE | VAR => Some(CatchPat::ExprMax(ExprMax::cast(syntax).unwrap())),
            BINARY_OP_EXPR => Some(CatchPat::BinaryOpExpr(BinaryOpExpr { syntax })),
            MAP_EXPR => Some(CatchPat::MapExpr(MapExpr { syntax })),
            RECORD_EXPR => Some(CatchPat::RecordExpr(RecordExpr { syntax })),
            RECORD_INDEX_EXPR => Some(CatchPat::RecordIndexExpr(RecordIndexExpr { syntax })),
            UNARY_OP_EXPR => Some(CatchPat::UnaryOpExpr(UnaryOpExpr { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            CatchPat::ExprMax(it) => it.syntax(),
            CatchPat::BinaryOpExpr(it) => it.syntax(),
            CatchPat::MapExpr(it) => it.syntax(),
            CatchPat::RecordExpr(it) => it.syntax(),
            CatchPat::RecordIndexExpr(it) => it.syntax(),
            CatchPat::UnaryOpExpr(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<ExprMax> for CatchPat {
    fn from(node: ExprMax) -> CatchPat {
        CatchPat::ExprMax(node)
    }
}
impl From<BinaryOpExpr> for CatchPat {
    fn from(node: BinaryOpExpr) -> CatchPat {
        CatchPat::BinaryOpExpr(node)
    }
}
impl From<MapExpr> for CatchPat {
    fn from(node: MapExpr) -> CatchPat {
        CatchPat::MapExpr(node)
    }
}
impl From<RecordExpr> for CatchPat {
    fn from(node: RecordExpr) -> CatchPat {
        CatchPat::RecordExpr(node)
    }
}
impl From<RecordIndexExpr> for CatchPat {
    fn from(node: RecordIndexExpr) -> CatchPat {
        CatchPat::RecordIndexExpr(node)
    }
}
impl From<UnaryOpExpr> for CatchPat {
    fn from(node: UnaryOpExpr) -> CatchPat {
        CatchPat::UnaryOpExpr(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for CatchPat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Literal 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Char {
    pub(crate) syntax: SyntaxNode,
}
#[doc = r" Via NodeType::Literal 2"]
impl Char {
    pub fn self_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, CHAR, 0)
    }
}
#[doc = r" Via NodeType::Literal 2"]
impl AstNode for Char {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == CHAR
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
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClauseBody {
    pub(crate) syntax: SyntaxNode,
}
impl ClauseBody {
    pub fn exprs(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ClauseBody {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == CLAUSE_BODY
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ClauseBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Literal 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Comment {
    pub(crate) syntax: SyntaxNode,
}
#[doc = r" Via NodeType::Literal 2"]
impl Comment {
    pub fn self_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, COMMENT, 0)
    }
}
#[doc = r" Via NodeType::Literal 2"]
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
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CompileOptionsAttribute {
    pub(crate) syntax: SyntaxNode,
}
impl CompileOptionsAttribute {
    pub fn options(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for CompileOptionsAttribute {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == COMPILE_OPTIONS_ATTRIBUTE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for CompileOptionsAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Concatable {
    MacroCallExpr(MacroCallExpr),
    MacroString(MacroString),
    String(String),
    Var(Var),
}
impl AstNode for Concatable {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            MACRO_CALL_EXPR | MACRO_STRING | STRING | VAR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            MACRO_CALL_EXPR => Some(Concatable::MacroCallExpr(MacroCallExpr { syntax })),
            MACRO_STRING => Some(Concatable::MacroString(MacroString { syntax })),
            STRING => Some(Concatable::String(String { syntax })),
            VAR => Some(Concatable::Var(Var { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Concatable::MacroCallExpr(it) => it.syntax(),
            Concatable::MacroString(it) => it.syntax(),
            Concatable::String(it) => it.syntax(),
            Concatable::Var(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<MacroCallExpr> for Concatable {
    fn from(node: MacroCallExpr) -> Concatable {
        Concatable::MacroCallExpr(node)
    }
}
impl From<MacroString> for Concatable {
    fn from(node: MacroString) -> Concatable {
        Concatable::MacroString(node)
    }
}
impl From<String> for Concatable {
    fn from(node: String) -> Concatable {
        Concatable::String(node)
    }
}
impl From<Var> for Concatable {
    fn from(node: Var) -> Concatable {
        Concatable::Var(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for Concatable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Concatables {
    pub(crate) syntax: SyntaxNode,
}
impl Concatables {
    pub fn elems(&self) -> AstChildren<Concatable> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for Concatables {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == CONCATABLES
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for Concatables {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CondMatchExpr {
    pub(crate) syntax: SyntaxNode,
}
impl CondMatchExpr {
    pub fn lhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn rhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 1usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for CondMatchExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == COND_MATCH_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for CondMatchExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CrClause {
    pub(crate) syntax: SyntaxNode,
}
impl CrClause {
    pub fn pat(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn body(&self) -> Option<ClauseBody> {
        support::child(&self.syntax, 0usize)
    }
    pub fn guard(&self) -> Option<Guard> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for CrClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == CR_CLAUSE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for CrClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CrClauseOrMacro {
    CrClause(CrClause),
    MacroCallExpr(MacroCallExpr),
}
impl AstNode for CrClauseOrMacro {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            CR_CLAUSE | MACRO_CALL_EXPR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            CR_CLAUSE => Some(CrClauseOrMacro::CrClause(CrClause { syntax })),
            MACRO_CALL_EXPR => Some(CrClauseOrMacro::MacroCallExpr(MacroCallExpr { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            CrClauseOrMacro::CrClause(it) => it.syntax(),
            CrClauseOrMacro::MacroCallExpr(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<CrClause> for CrClauseOrMacro {
    fn from(node: CrClause) -> CrClauseOrMacro {
        CrClauseOrMacro::CrClause(node)
    }
}
impl From<MacroCallExpr> for CrClauseOrMacro {
    fn from(node: MacroCallExpr) -> CrClauseOrMacro {
        CrClauseOrMacro::MacroCallExpr(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for CrClauseOrMacro {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeprecatedAttribute {
    pub(crate) syntax: SyntaxNode,
}
impl DeprecatedAttribute {
    pub fn attr(&self) -> Option<DeprecatedDetails> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for DeprecatedAttribute {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == DEPRECATED_ATTRIBUTE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for DeprecatedAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeprecatedDetails {
    DeprecatedFa(DeprecatedFa),
    DeprecatedFas(DeprecatedFas),
    DeprecatedModule(DeprecatedModule),
}
impl AstNode for DeprecatedDetails {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            DEPRECATED_FA | DEPRECATED_FAS | DEPRECATED_MODULE => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            DEPRECATED_FA => Some(DeprecatedDetails::DeprecatedFa(DeprecatedFa { syntax })),
            DEPRECATED_FAS => Some(DeprecatedDetails::DeprecatedFas(DeprecatedFas { syntax })),
            DEPRECATED_MODULE => Some(DeprecatedDetails::DeprecatedModule(DeprecatedModule {
                syntax,
            })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            DeprecatedDetails::DeprecatedFa(it) => it.syntax(),
            DeprecatedDetails::DeprecatedFas(it) => it.syntax(),
            DeprecatedDetails::DeprecatedModule(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<DeprecatedFa> for DeprecatedDetails {
    fn from(node: DeprecatedFa) -> DeprecatedDetails {
        DeprecatedDetails::DeprecatedFa(node)
    }
}
impl From<DeprecatedFas> for DeprecatedDetails {
    fn from(node: DeprecatedFas) -> DeprecatedDetails {
        DeprecatedDetails::DeprecatedFas(node)
    }
}
impl From<DeprecatedModule> for DeprecatedDetails {
    fn from(node: DeprecatedModule) -> DeprecatedDetails {
        DeprecatedDetails::DeprecatedModule(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for DeprecatedDetails {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeprecatedFa {
    pub(crate) syntax: SyntaxNode,
}
impl DeprecatedFa {
    pub fn arity(&self) -> Option<DeprecatedFunArity> {
        support::child(&self.syntax, 0usize)
    }
    pub fn fun(&self) -> Option<Atom> {
        support::child(&self.syntax, 0usize)
    }
    pub fn desc(&self) -> Option<DeprecationDesc> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for DeprecatedFa {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == DEPRECATED_FA
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for DeprecatedFa {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeprecatedFas {
    pub(crate) syntax: SyntaxNode,
}
impl DeprecatedFas {
    pub fn fa(&self) -> AstChildren<DeprecatedFa> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for DeprecatedFas {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == DEPRECATED_FAS
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for DeprecatedFas {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeprecatedFunArity {
    DeprecatedWildcard(DeprecatedWildcard),
    Integer(Integer),
}
impl AstNode for DeprecatedFunArity {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            DEPRECATED_WILDCARD | INTEGER => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            DEPRECATED_WILDCARD => {
                Some(DeprecatedFunArity::DeprecatedWildcard(DeprecatedWildcard {
                    syntax,
                }))
            }
            INTEGER => Some(DeprecatedFunArity::Integer(Integer { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            DeprecatedFunArity::DeprecatedWildcard(it) => it.syntax(),
            DeprecatedFunArity::Integer(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<DeprecatedWildcard> for DeprecatedFunArity {
    fn from(node: DeprecatedWildcard) -> DeprecatedFunArity {
        DeprecatedFunArity::DeprecatedWildcard(node)
    }
}
impl From<Integer> for DeprecatedFunArity {
    fn from(node: Integer) -> DeprecatedFunArity {
        DeprecatedFunArity::Integer(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for DeprecatedFunArity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeprecatedModule {
    pub(crate) syntax: SyntaxNode,
}
impl DeprecatedModule {
    pub fn module(&self) -> Option<Atom> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for DeprecatedModule {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == DEPRECATED_MODULE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for DeprecatedModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Literal 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeprecatedWildcard {
    pub(crate) syntax: SyntaxNode,
}
#[doc = r" Via NodeType::Literal 2"]
impl DeprecatedWildcard {
    pub fn self_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, DEPRECATED_WILDCARD, 0)
    }
}
#[doc = r" Via NodeType::Literal 2"]
impl AstNode for DeprecatedWildcard {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == DEPRECATED_WILDCARD
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
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeprecationDesc {
    pub(crate) syntax: SyntaxNode,
}
impl DeprecationDesc {
    pub fn desc(&self) -> Option<Desc> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for DeprecationDesc {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == DEPRECATION_DESC
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for DeprecationDesc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Desc {
    Atom(Atom),
    MultiString(MultiString),
}
impl AstNode for Desc {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ATOM | MULTI_STRING => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            ATOM => Some(Desc::Atom(Atom { syntax })),
            MULTI_STRING => Some(Desc::MultiString(MultiString { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Desc::Atom(it) => it.syntax(),
            Desc::MultiString(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<Atom> for Desc {
    fn from(node: Atom) -> Desc {
        Desc::Atom(node)
    }
}
impl From<MultiString> for Desc {
    fn from(node: MultiString) -> Desc {
        Desc::MultiString(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for Desc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Literal 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Dotdotdot {
    pub(crate) syntax: SyntaxNode,
}
#[doc = r" Via NodeType::Literal 2"]
impl Dotdotdot {
    pub fn self_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, DOTDOTDOT, 0)
    }
}
#[doc = r" Via NodeType::Literal 2"]
impl AstNode for Dotdotdot {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == DOTDOTDOT
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
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExportAttribute {
    pub(crate) syntax: SyntaxNode,
}
impl ExportAttribute {
    pub fn funs(&self) -> AstChildren<Fa> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ExportAttribute {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EXPORT_ATTRIBUTE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ExportAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExportTypeAttribute {
    pub(crate) syntax: SyntaxNode,
}
impl ExportTypeAttribute {
    pub fn types(&self) -> AstChildren<Fa> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ExportTypeAttribute {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EXPORT_TYPE_ATTRIBUTE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ExportTypeAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    ExprMax(ExprMax),
    AnnType(AnnType),
    BinaryOpExpr(BinaryOpExpr),
    Call(Call),
    CatchExpr(CatchExpr),
    CondMatchExpr(CondMatchExpr),
    Dotdotdot(Dotdotdot),
    MapExpr(MapExpr),
    MapExprUpdate(MapExprUpdate),
    MatchExpr(MatchExpr),
    Pipe(Pipe),
    RangeType(RangeType),
    RecordExpr(RecordExpr),
    RecordFieldExpr(RecordFieldExpr),
    RecordIndexExpr(RecordIndexExpr),
    RecordUpdateExpr(RecordUpdateExpr),
    Remote(Remote),
    UnaryOpExpr(UnaryOpExpr),
}
impl AstNode for Expr {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ANONYMOUS_FUN | ATOM | BINARY | BINARY_COMPREHENSION | BLOCK_EXPR | CASE_EXPR
            | CHAR | CONCATABLES | EXTERNAL_FUN | FLOAT | FUN_TYPE | IF_EXPR | INTEGER
            | INTERNAL_FUN | LIST | LIST_COMPREHENSION | MACRO_CALL_EXPR | MACRO_STRING
            | MAP_COMPREHENSION | MAYBE_EXPR | PAREN_EXPR | RECEIVE_EXPR | STRING | TRY_EXPR
            | TUPLE | VAR | ANN_TYPE | BINARY_OP_EXPR | CALL | CATCH_EXPR | COND_MATCH_EXPR
            | DOTDOTDOT | MAP_EXPR | MAP_EXPR_UPDATE | MATCH_EXPR | PIPE | RANGE_TYPE
            | RECORD_EXPR | RECORD_FIELD_EXPR | RECORD_INDEX_EXPR | RECORD_UPDATE_EXPR | REMOTE
            | UNARY_OP_EXPR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            ANONYMOUS_FUN | ATOM | BINARY | BINARY_COMPREHENSION | BLOCK_EXPR | CASE_EXPR
            | CHAR | CONCATABLES | EXTERNAL_FUN | FLOAT | FUN_TYPE | IF_EXPR | INTEGER
            | INTERNAL_FUN | LIST | LIST_COMPREHENSION | MACRO_CALL_EXPR | MACRO_STRING
            | MAP_COMPREHENSION | MAYBE_EXPR | PAREN_EXPR | RECEIVE_EXPR | STRING | TRY_EXPR
            | TUPLE | VAR => Some(Expr::ExprMax(ExprMax::cast(syntax).unwrap())),
            ANN_TYPE => Some(Expr::AnnType(AnnType { syntax })),
            BINARY_OP_EXPR => Some(Expr::BinaryOpExpr(BinaryOpExpr { syntax })),
            CALL => Some(Expr::Call(Call { syntax })),
            CATCH_EXPR => Some(Expr::CatchExpr(CatchExpr { syntax })),
            COND_MATCH_EXPR => Some(Expr::CondMatchExpr(CondMatchExpr { syntax })),
            DOTDOTDOT => Some(Expr::Dotdotdot(Dotdotdot { syntax })),
            MAP_EXPR => Some(Expr::MapExpr(MapExpr { syntax })),
            MAP_EXPR_UPDATE => Some(Expr::MapExprUpdate(MapExprUpdate { syntax })),
            MATCH_EXPR => Some(Expr::MatchExpr(MatchExpr { syntax })),
            PIPE => Some(Expr::Pipe(Pipe { syntax })),
            RANGE_TYPE => Some(Expr::RangeType(RangeType { syntax })),
            RECORD_EXPR => Some(Expr::RecordExpr(RecordExpr { syntax })),
            RECORD_FIELD_EXPR => Some(Expr::RecordFieldExpr(RecordFieldExpr { syntax })),
            RECORD_INDEX_EXPR => Some(Expr::RecordIndexExpr(RecordIndexExpr { syntax })),
            RECORD_UPDATE_EXPR => Some(Expr::RecordUpdateExpr(RecordUpdateExpr { syntax })),
            REMOTE => Some(Expr::Remote(Remote { syntax })),
            UNARY_OP_EXPR => Some(Expr::UnaryOpExpr(UnaryOpExpr { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Expr::ExprMax(it) => it.syntax(),
            Expr::AnnType(it) => it.syntax(),
            Expr::BinaryOpExpr(it) => it.syntax(),
            Expr::Call(it) => it.syntax(),
            Expr::CatchExpr(it) => it.syntax(),
            Expr::CondMatchExpr(it) => it.syntax(),
            Expr::Dotdotdot(it) => it.syntax(),
            Expr::MapExpr(it) => it.syntax(),
            Expr::MapExprUpdate(it) => it.syntax(),
            Expr::MatchExpr(it) => it.syntax(),
            Expr::Pipe(it) => it.syntax(),
            Expr::RangeType(it) => it.syntax(),
            Expr::RecordExpr(it) => it.syntax(),
            Expr::RecordFieldExpr(it) => it.syntax(),
            Expr::RecordIndexExpr(it) => it.syntax(),
            Expr::RecordUpdateExpr(it) => it.syntax(),
            Expr::Remote(it) => it.syntax(),
            Expr::UnaryOpExpr(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<ExprMax> for Expr {
    fn from(node: ExprMax) -> Expr {
        Expr::ExprMax(node)
    }
}
impl From<AnnType> for Expr {
    fn from(node: AnnType) -> Expr {
        Expr::AnnType(node)
    }
}
impl From<BinaryOpExpr> for Expr {
    fn from(node: BinaryOpExpr) -> Expr {
        Expr::BinaryOpExpr(node)
    }
}
impl From<Call> for Expr {
    fn from(node: Call) -> Expr {
        Expr::Call(node)
    }
}
impl From<CatchExpr> for Expr {
    fn from(node: CatchExpr) -> Expr {
        Expr::CatchExpr(node)
    }
}
impl From<CondMatchExpr> for Expr {
    fn from(node: CondMatchExpr) -> Expr {
        Expr::CondMatchExpr(node)
    }
}
impl From<Dotdotdot> for Expr {
    fn from(node: Dotdotdot) -> Expr {
        Expr::Dotdotdot(node)
    }
}
impl From<MapExpr> for Expr {
    fn from(node: MapExpr) -> Expr {
        Expr::MapExpr(node)
    }
}
impl From<MapExprUpdate> for Expr {
    fn from(node: MapExprUpdate) -> Expr {
        Expr::MapExprUpdate(node)
    }
}
impl From<MatchExpr> for Expr {
    fn from(node: MatchExpr) -> Expr {
        Expr::MatchExpr(node)
    }
}
impl From<Pipe> for Expr {
    fn from(node: Pipe) -> Expr {
        Expr::Pipe(node)
    }
}
impl From<RangeType> for Expr {
    fn from(node: RangeType) -> Expr {
        Expr::RangeType(node)
    }
}
impl From<RecordExpr> for Expr {
    fn from(node: RecordExpr) -> Expr {
        Expr::RecordExpr(node)
    }
}
impl From<RecordFieldExpr> for Expr {
    fn from(node: RecordFieldExpr) -> Expr {
        Expr::RecordFieldExpr(node)
    }
}
impl From<RecordIndexExpr> for Expr {
    fn from(node: RecordIndexExpr) -> Expr {
        Expr::RecordIndexExpr(node)
    }
}
impl From<RecordUpdateExpr> for Expr {
    fn from(node: RecordUpdateExpr) -> Expr {
        Expr::RecordUpdateExpr(node)
    }
}
impl From<Remote> for Expr {
    fn from(node: Remote) -> Expr {
        Expr::Remote(node)
    }
}
impl From<UnaryOpExpr> for Expr {
    fn from(node: UnaryOpExpr) -> Expr {
        Expr::UnaryOpExpr(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprArgs {
    pub(crate) syntax: SyntaxNode,
}
impl ExprArgs {
    pub fn args(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ExprArgs {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EXPR_ARGS
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ExprArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprMax {
    AnonymousFun(AnonymousFun),
    Atom(Atom),
    Binary(Binary),
    BinaryComprehension(BinaryComprehension),
    BlockExpr(BlockExpr),
    CaseExpr(CaseExpr),
    Char(Char),
    Concatables(Concatables),
    ExternalFun(ExternalFun),
    Float(Float),
    FunType(FunType),
    IfExpr(IfExpr),
    Integer(Integer),
    InternalFun(InternalFun),
    List(List),
    ListComprehension(ListComprehension),
    MacroCallExpr(MacroCallExpr),
    MacroString(MacroString),
    MapComprehension(MapComprehension),
    MaybeExpr(MaybeExpr),
    ParenExpr(ParenExpr),
    ReceiveExpr(ReceiveExpr),
    String(String),
    TryExpr(TryExpr),
    Tuple(Tuple),
    Var(Var),
}
impl AstNode for ExprMax {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ANONYMOUS_FUN | ATOM | BINARY | BINARY_COMPREHENSION | BLOCK_EXPR | CASE_EXPR
            | CHAR | CONCATABLES | EXTERNAL_FUN | FLOAT | FUN_TYPE | IF_EXPR | INTEGER
            | INTERNAL_FUN | LIST | LIST_COMPREHENSION | MACRO_CALL_EXPR | MACRO_STRING
            | MAP_COMPREHENSION | MAYBE_EXPR | PAREN_EXPR | RECEIVE_EXPR | STRING | TRY_EXPR
            | TUPLE | VAR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            ANONYMOUS_FUN => Some(ExprMax::AnonymousFun(AnonymousFun { syntax })),
            ATOM => Some(ExprMax::Atom(Atom { syntax })),
            BINARY => Some(ExprMax::Binary(Binary { syntax })),
            BINARY_COMPREHENSION => {
                Some(ExprMax::BinaryComprehension(BinaryComprehension { syntax }))
            }
            BLOCK_EXPR => Some(ExprMax::BlockExpr(BlockExpr { syntax })),
            CASE_EXPR => Some(ExprMax::CaseExpr(CaseExpr { syntax })),
            CHAR => Some(ExprMax::Char(Char { syntax })),
            CONCATABLES => Some(ExprMax::Concatables(Concatables { syntax })),
            EXTERNAL_FUN => Some(ExprMax::ExternalFun(ExternalFun { syntax })),
            FLOAT => Some(ExprMax::Float(Float { syntax })),
            FUN_TYPE => Some(ExprMax::FunType(FunType { syntax })),
            IF_EXPR => Some(ExprMax::IfExpr(IfExpr { syntax })),
            INTEGER => Some(ExprMax::Integer(Integer { syntax })),
            INTERNAL_FUN => Some(ExprMax::InternalFun(InternalFun { syntax })),
            LIST => Some(ExprMax::List(List { syntax })),
            LIST_COMPREHENSION => Some(ExprMax::ListComprehension(ListComprehension { syntax })),
            MACRO_CALL_EXPR => Some(ExprMax::MacroCallExpr(MacroCallExpr { syntax })),
            MACRO_STRING => Some(ExprMax::MacroString(MacroString { syntax })),
            MAP_COMPREHENSION => Some(ExprMax::MapComprehension(MapComprehension { syntax })),
            MAYBE_EXPR => Some(ExprMax::MaybeExpr(MaybeExpr { syntax })),
            PAREN_EXPR => Some(ExprMax::ParenExpr(ParenExpr { syntax })),
            RECEIVE_EXPR => Some(ExprMax::ReceiveExpr(ReceiveExpr { syntax })),
            STRING => Some(ExprMax::String(String { syntax })),
            TRY_EXPR => Some(ExprMax::TryExpr(TryExpr { syntax })),
            TUPLE => Some(ExprMax::Tuple(Tuple { syntax })),
            VAR => Some(ExprMax::Var(Var { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            ExprMax::AnonymousFun(it) => it.syntax(),
            ExprMax::Atom(it) => it.syntax(),
            ExprMax::Binary(it) => it.syntax(),
            ExprMax::BinaryComprehension(it) => it.syntax(),
            ExprMax::BlockExpr(it) => it.syntax(),
            ExprMax::CaseExpr(it) => it.syntax(),
            ExprMax::Char(it) => it.syntax(),
            ExprMax::Concatables(it) => it.syntax(),
            ExprMax::ExternalFun(it) => it.syntax(),
            ExprMax::Float(it) => it.syntax(),
            ExprMax::FunType(it) => it.syntax(),
            ExprMax::IfExpr(it) => it.syntax(),
            ExprMax::Integer(it) => it.syntax(),
            ExprMax::InternalFun(it) => it.syntax(),
            ExprMax::List(it) => it.syntax(),
            ExprMax::ListComprehension(it) => it.syntax(),
            ExprMax::MacroCallExpr(it) => it.syntax(),
            ExprMax::MacroString(it) => it.syntax(),
            ExprMax::MapComprehension(it) => it.syntax(),
            ExprMax::MaybeExpr(it) => it.syntax(),
            ExprMax::ParenExpr(it) => it.syntax(),
            ExprMax::ReceiveExpr(it) => it.syntax(),
            ExprMax::String(it) => it.syntax(),
            ExprMax::TryExpr(it) => it.syntax(),
            ExprMax::Tuple(it) => it.syntax(),
            ExprMax::Var(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<AnonymousFun> for ExprMax {
    fn from(node: AnonymousFun) -> ExprMax {
        ExprMax::AnonymousFun(node)
    }
}
impl From<Atom> for ExprMax {
    fn from(node: Atom) -> ExprMax {
        ExprMax::Atom(node)
    }
}
impl From<Binary> for ExprMax {
    fn from(node: Binary) -> ExprMax {
        ExprMax::Binary(node)
    }
}
impl From<BinaryComprehension> for ExprMax {
    fn from(node: BinaryComprehension) -> ExprMax {
        ExprMax::BinaryComprehension(node)
    }
}
impl From<BlockExpr> for ExprMax {
    fn from(node: BlockExpr) -> ExprMax {
        ExprMax::BlockExpr(node)
    }
}
impl From<CaseExpr> for ExprMax {
    fn from(node: CaseExpr) -> ExprMax {
        ExprMax::CaseExpr(node)
    }
}
impl From<Char> for ExprMax {
    fn from(node: Char) -> ExprMax {
        ExprMax::Char(node)
    }
}
impl From<Concatables> for ExprMax {
    fn from(node: Concatables) -> ExprMax {
        ExprMax::Concatables(node)
    }
}
impl From<ExternalFun> for ExprMax {
    fn from(node: ExternalFun) -> ExprMax {
        ExprMax::ExternalFun(node)
    }
}
impl From<Float> for ExprMax {
    fn from(node: Float) -> ExprMax {
        ExprMax::Float(node)
    }
}
impl From<FunType> for ExprMax {
    fn from(node: FunType) -> ExprMax {
        ExprMax::FunType(node)
    }
}
impl From<IfExpr> for ExprMax {
    fn from(node: IfExpr) -> ExprMax {
        ExprMax::IfExpr(node)
    }
}
impl From<Integer> for ExprMax {
    fn from(node: Integer) -> ExprMax {
        ExprMax::Integer(node)
    }
}
impl From<InternalFun> for ExprMax {
    fn from(node: InternalFun) -> ExprMax {
        ExprMax::InternalFun(node)
    }
}
impl From<List> for ExprMax {
    fn from(node: List) -> ExprMax {
        ExprMax::List(node)
    }
}
impl From<ListComprehension> for ExprMax {
    fn from(node: ListComprehension) -> ExprMax {
        ExprMax::ListComprehension(node)
    }
}
impl From<MacroCallExpr> for ExprMax {
    fn from(node: MacroCallExpr) -> ExprMax {
        ExprMax::MacroCallExpr(node)
    }
}
impl From<MacroString> for ExprMax {
    fn from(node: MacroString) -> ExprMax {
        ExprMax::MacroString(node)
    }
}
impl From<MapComprehension> for ExprMax {
    fn from(node: MapComprehension) -> ExprMax {
        ExprMax::MapComprehension(node)
    }
}
impl From<MaybeExpr> for ExprMax {
    fn from(node: MaybeExpr) -> ExprMax {
        ExprMax::MaybeExpr(node)
    }
}
impl From<ParenExpr> for ExprMax {
    fn from(node: ParenExpr) -> ExprMax {
        ExprMax::ParenExpr(node)
    }
}
impl From<ReceiveExpr> for ExprMax {
    fn from(node: ReceiveExpr) -> ExprMax {
        ExprMax::ReceiveExpr(node)
    }
}
impl From<String> for ExprMax {
    fn from(node: String) -> ExprMax {
        ExprMax::String(node)
    }
}
impl From<TryExpr> for ExprMax {
    fn from(node: TryExpr) -> ExprMax {
        ExprMax::TryExpr(node)
    }
}
impl From<Tuple> for ExprMax {
    fn from(node: Tuple) -> ExprMax {
        ExprMax::Tuple(node)
    }
}
impl From<Var> for ExprMax {
    fn from(node: Var) -> ExprMax {
        ExprMax::Var(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for ExprMax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternalFun {
    pub(crate) syntax: SyntaxNode,
}
impl ExternalFun {
    pub fn fun(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
    pub fn arity(&self) -> Option<Arity> {
        support::child(&self.syntax, 0usize)
    }
    pub fn module(&self) -> Option<Module> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ExternalFun {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EXTERNAL_FUN
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ExternalFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Fa {
    pub(crate) syntax: SyntaxNode,
}
impl Fa {
    pub fn fun(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
    pub fn arity(&self) -> Option<Arity> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for Fa {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == FA
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for Fa {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FeatureAttribute {
    pub(crate) syntax: SyntaxNode,
}
impl FeatureAttribute {
    pub fn feature(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn flag(&self) -> Option<Expr> {
        support::child(&self.syntax, 1usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for FeatureAttribute {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == FEATURE_ATTRIBUTE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for FeatureAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldExpr {
    pub(crate) syntax: SyntaxNode,
}
impl FieldExpr {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for FieldExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == FIELD_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for FieldExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldType {
    pub(crate) syntax: SyntaxNode,
}
impl FieldType {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for FieldType {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == FIELD_TYPE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for FieldType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FileAttribute {
    pub(crate) syntax: SyntaxNode,
}
impl FileAttribute {
    pub fn original_line(&self) -> Option<Integer> {
        support::child(&self.syntax, 0usize)
    }
    pub fn original_file(&self) -> Option<String> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for FileAttribute {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == FILE_ATTRIBUTE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for FileAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Literal 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Float {
    pub(crate) syntax: SyntaxNode,
}
#[doc = r" Via NodeType::Literal 2"]
impl Float {
    pub fn self_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, FLOAT, 0)
    }
}
#[doc = r" Via NodeType::Literal 2"]
impl AstNode for Float {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == FLOAT
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
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Form {
    PreprocessorDirective(PreprocessorDirective),
    BehaviourAttribute(BehaviourAttribute),
    Callback(Callback),
    CompileOptionsAttribute(CompileOptionsAttribute),
    DeprecatedAttribute(DeprecatedAttribute),
    ExportAttribute(ExportAttribute),
    ExportTypeAttribute(ExportTypeAttribute),
    FeatureAttribute(FeatureAttribute),
    FileAttribute(FileAttribute),
    FunDecl(FunDecl),
    ImportAttribute(ImportAttribute),
    ModuleAttribute(ModuleAttribute),
    Opaque(Opaque),
    OptionalCallbacksAttribute(OptionalCallbacksAttribute),
    RecordDecl(RecordDecl),
    Shebang(Shebang),
    Spec(Spec),
    SsrDefinition(SsrDefinition),
    TypeAlias(TypeAlias),
    WildAttribute(WildAttribute),
}
impl AstNode for Form {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            PP_DEFINE
            | PP_ELIF
            | PP_ELSE
            | PP_ENDIF
            | PP_IF
            | PP_IFDEF
            | PP_IFNDEF
            | PP_INCLUDE
            | PP_INCLUDE_LIB
            | PP_UNDEF
            | BEHAVIOUR_ATTRIBUTE
            | CALLBACK
            | COMPILE_OPTIONS_ATTRIBUTE
            | DEPRECATED_ATTRIBUTE
            | EXPORT_ATTRIBUTE
            | EXPORT_TYPE_ATTRIBUTE
            | FEATURE_ATTRIBUTE
            | FILE_ATTRIBUTE
            | FUN_DECL
            | IMPORT_ATTRIBUTE
            | MODULE_ATTRIBUTE
            | OPAQUE
            | OPTIONAL_CALLBACKS_ATTRIBUTE
            | RECORD_DECL
            | SHEBANG
            | SPEC
            | SSR_DEFINITION
            | TYPE_ALIAS
            | WILD_ATTRIBUTE => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            PP_DEFINE | PP_ELIF | PP_ELSE | PP_ENDIF | PP_IF | PP_IFDEF | PP_IFNDEF
            | PP_INCLUDE | PP_INCLUDE_LIB | PP_UNDEF => Some(Form::PreprocessorDirective(
                PreprocessorDirective::cast(syntax).unwrap(),
            )),
            BEHAVIOUR_ATTRIBUTE => Some(Form::BehaviourAttribute(BehaviourAttribute { syntax })),
            CALLBACK => Some(Form::Callback(Callback { syntax })),
            COMPILE_OPTIONS_ATTRIBUTE => {
                Some(Form::CompileOptionsAttribute(CompileOptionsAttribute {
                    syntax,
                }))
            }
            DEPRECATED_ATTRIBUTE => Some(Form::DeprecatedAttribute(DeprecatedAttribute { syntax })),
            EXPORT_ATTRIBUTE => Some(Form::ExportAttribute(ExportAttribute { syntax })),
            EXPORT_TYPE_ATTRIBUTE => {
                Some(Form::ExportTypeAttribute(ExportTypeAttribute { syntax }))
            }
            FEATURE_ATTRIBUTE => Some(Form::FeatureAttribute(FeatureAttribute { syntax })),
            FILE_ATTRIBUTE => Some(Form::FileAttribute(FileAttribute { syntax })),
            FUN_DECL => Some(Form::FunDecl(FunDecl { syntax })),
            IMPORT_ATTRIBUTE => Some(Form::ImportAttribute(ImportAttribute { syntax })),
            MODULE_ATTRIBUTE => Some(Form::ModuleAttribute(ModuleAttribute { syntax })),
            OPAQUE => Some(Form::Opaque(Opaque { syntax })),
            OPTIONAL_CALLBACKS_ATTRIBUTE => Some(Form::OptionalCallbacksAttribute(
                OptionalCallbacksAttribute { syntax },
            )),
            RECORD_DECL => Some(Form::RecordDecl(RecordDecl { syntax })),
            SHEBANG => Some(Form::Shebang(Shebang { syntax })),
            SPEC => Some(Form::Spec(Spec { syntax })),
            SSR_DEFINITION => Some(Form::SsrDefinition(SsrDefinition { syntax })),
            TYPE_ALIAS => Some(Form::TypeAlias(TypeAlias { syntax })),
            WILD_ATTRIBUTE => Some(Form::WildAttribute(WildAttribute { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Form::PreprocessorDirective(it) => it.syntax(),
            Form::BehaviourAttribute(it) => it.syntax(),
            Form::Callback(it) => it.syntax(),
            Form::CompileOptionsAttribute(it) => it.syntax(),
            Form::DeprecatedAttribute(it) => it.syntax(),
            Form::ExportAttribute(it) => it.syntax(),
            Form::ExportTypeAttribute(it) => it.syntax(),
            Form::FeatureAttribute(it) => it.syntax(),
            Form::FileAttribute(it) => it.syntax(),
            Form::FunDecl(it) => it.syntax(),
            Form::ImportAttribute(it) => it.syntax(),
            Form::ModuleAttribute(it) => it.syntax(),
            Form::Opaque(it) => it.syntax(),
            Form::OptionalCallbacksAttribute(it) => it.syntax(),
            Form::RecordDecl(it) => it.syntax(),
            Form::Shebang(it) => it.syntax(),
            Form::Spec(it) => it.syntax(),
            Form::SsrDefinition(it) => it.syntax(),
            Form::TypeAlias(it) => it.syntax(),
            Form::WildAttribute(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<PreprocessorDirective> for Form {
    fn from(node: PreprocessorDirective) -> Form {
        Form::PreprocessorDirective(node)
    }
}
impl From<BehaviourAttribute> for Form {
    fn from(node: BehaviourAttribute) -> Form {
        Form::BehaviourAttribute(node)
    }
}
impl From<Callback> for Form {
    fn from(node: Callback) -> Form {
        Form::Callback(node)
    }
}
impl From<CompileOptionsAttribute> for Form {
    fn from(node: CompileOptionsAttribute) -> Form {
        Form::CompileOptionsAttribute(node)
    }
}
impl From<DeprecatedAttribute> for Form {
    fn from(node: DeprecatedAttribute) -> Form {
        Form::DeprecatedAttribute(node)
    }
}
impl From<ExportAttribute> for Form {
    fn from(node: ExportAttribute) -> Form {
        Form::ExportAttribute(node)
    }
}
impl From<ExportTypeAttribute> for Form {
    fn from(node: ExportTypeAttribute) -> Form {
        Form::ExportTypeAttribute(node)
    }
}
impl From<FeatureAttribute> for Form {
    fn from(node: FeatureAttribute) -> Form {
        Form::FeatureAttribute(node)
    }
}
impl From<FileAttribute> for Form {
    fn from(node: FileAttribute) -> Form {
        Form::FileAttribute(node)
    }
}
impl From<FunDecl> for Form {
    fn from(node: FunDecl) -> Form {
        Form::FunDecl(node)
    }
}
impl From<ImportAttribute> for Form {
    fn from(node: ImportAttribute) -> Form {
        Form::ImportAttribute(node)
    }
}
impl From<ModuleAttribute> for Form {
    fn from(node: ModuleAttribute) -> Form {
        Form::ModuleAttribute(node)
    }
}
impl From<Opaque> for Form {
    fn from(node: Opaque) -> Form {
        Form::Opaque(node)
    }
}
impl From<OptionalCallbacksAttribute> for Form {
    fn from(node: OptionalCallbacksAttribute) -> Form {
        Form::OptionalCallbacksAttribute(node)
    }
}
impl From<RecordDecl> for Form {
    fn from(node: RecordDecl) -> Form {
        Form::RecordDecl(node)
    }
}
impl From<Shebang> for Form {
    fn from(node: Shebang) -> Form {
        Form::Shebang(node)
    }
}
impl From<Spec> for Form {
    fn from(node: Spec) -> Form {
        Form::Spec(node)
    }
}
impl From<SsrDefinition> for Form {
    fn from(node: SsrDefinition) -> Form {
        Form::SsrDefinition(node)
    }
}
impl From<TypeAlias> for Form {
    fn from(node: TypeAlias) -> Form {
        Form::TypeAlias(node)
    }
}
impl From<WildAttribute> for Form {
    fn from(node: WildAttribute) -> Form {
        Form::WildAttribute(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for Form {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunClause {
    pub(crate) syntax: SyntaxNode,
}
impl FunClause {
    pub fn body(&self) -> Option<ClauseBody> {
        support::child(&self.syntax, 0usize)
    }
    pub fn args(&self) -> Option<ExprArgs> {
        support::child(&self.syntax, 0usize)
    }
    pub fn guard(&self) -> Option<Guard> {
        support::child(&self.syntax, 0usize)
    }
    pub fn name(&self) -> Option<Var> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for FunClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == FUN_CLAUSE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for FunClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunDecl {
    pub(crate) syntax: SyntaxNode,
}
impl FunDecl {
    pub fn clause(&self) -> Option<FunctionOrMacroClause> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for FunDecl {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == FUN_DECL
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for FunDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunType {
    pub(crate) syntax: SyntaxNode,
}
impl FunType {
    pub fn sig(&self) -> Option<FunTypeSig> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for FunType {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == FUN_TYPE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for FunType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunTypeSig {
    pub(crate) syntax: SyntaxNode,
}
impl FunTypeSig {
    pub fn ty(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn args(&self) -> Option<ExprArgs> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for FunTypeSig {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == FUN_TYPE_SIG
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for FunTypeSig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionClause {
    pub(crate) syntax: SyntaxNode,
}
impl FunctionClause {
    pub fn name(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
    pub fn body(&self) -> Option<ClauseBody> {
        support::child(&self.syntax, 0usize)
    }
    pub fn args(&self) -> Option<ExprArgs> {
        support::child(&self.syntax, 0usize)
    }
    pub fn guard(&self) -> Option<Guard> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for FunctionClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == FUNCTION_CLAUSE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for FunctionClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FunctionOrMacroClause {
    FunctionClause(FunctionClause),
    MacroCallExpr(MacroCallExpr),
}
impl AstNode for FunctionOrMacroClause {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            FUNCTION_CLAUSE | MACRO_CALL_EXPR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            FUNCTION_CLAUSE => Some(FunctionOrMacroClause::FunctionClause(FunctionClause {
                syntax,
            })),
            MACRO_CALL_EXPR => Some(FunctionOrMacroClause::MacroCallExpr(MacroCallExpr {
                syntax,
            })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            FunctionOrMacroClause::FunctionClause(it) => it.syntax(),
            FunctionOrMacroClause::MacroCallExpr(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<FunctionClause> for FunctionOrMacroClause {
    fn from(node: FunctionClause) -> FunctionOrMacroClause {
        FunctionOrMacroClause::FunctionClause(node)
    }
}
impl From<MacroCallExpr> for FunctionOrMacroClause {
    fn from(node: MacroCallExpr) -> FunctionOrMacroClause {
        FunctionOrMacroClause::MacroCallExpr(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for FunctionOrMacroClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Generator {
    pub(crate) syntax: SyntaxNode,
}
impl Generator {
    pub fn lhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn rhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 1usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for Generator {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == GENERATOR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for Generator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Guard {
    pub(crate) syntax: SyntaxNode,
}
impl Guard {
    pub fn clauses(&self) -> AstChildren<GuardClause> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for Guard {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == GUARD
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for Guard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GuardClause {
    pub(crate) syntax: SyntaxNode,
}
impl GuardClause {
    pub fn exprs(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for GuardClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == GUARD_CLAUSE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for GuardClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfClause {
    pub(crate) syntax: SyntaxNode,
}
impl IfClause {
    pub fn body(&self) -> Option<ClauseBody> {
        support::child(&self.syntax, 0usize)
    }
    pub fn guard(&self) -> Option<Guard> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for IfClause {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == IF_CLAUSE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for IfClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfExpr {
    pub(crate) syntax: SyntaxNode,
}
impl IfExpr {
    pub fn clauses(&self) -> AstChildren<IfClause> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for IfExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == IF_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for IfExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportAttribute {
    pub(crate) syntax: SyntaxNode,
}
impl ImportAttribute {
    pub fn module(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
    pub fn funs(&self) -> AstChildren<Fa> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ImportAttribute {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == IMPORT_ATTRIBUTE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ImportAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IncludeDetail {
    MacroCallExpr(MacroCallExpr),
    String(String),
}
impl AstNode for IncludeDetail {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            MACRO_CALL_EXPR | STRING => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            MACRO_CALL_EXPR => Some(IncludeDetail::MacroCallExpr(MacroCallExpr { syntax })),
            STRING => Some(IncludeDetail::String(String { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            IncludeDetail::MacroCallExpr(it) => it.syntax(),
            IncludeDetail::String(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<MacroCallExpr> for IncludeDetail {
    fn from(node: MacroCallExpr) -> IncludeDetail {
        IncludeDetail::MacroCallExpr(node)
    }
}
impl From<String> for IncludeDetail {
    fn from(node: String) -> IncludeDetail {
        IncludeDetail::String(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for IncludeDetail {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Literal 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Integer {
    pub(crate) syntax: SyntaxNode,
}
#[doc = r" Via NodeType::Literal 2"]
impl Integer {
    pub fn self_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, INTEGER, 0)
    }
}
#[doc = r" Via NodeType::Literal 2"]
impl AstNode for Integer {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == INTEGER
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
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InternalFun {
    pub(crate) syntax: SyntaxNode,
}
impl InternalFun {
    pub fn fun(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
    pub fn arity(&self) -> Option<Arity> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for InternalFun {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == INTERNAL_FUN
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for InternalFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LcExpr {
    Expr(Expr),
    BGenerator(BGenerator),
    Generator(Generator),
    MapGenerator(MapGenerator),
}
impl AstNode for LcExpr {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ANONYMOUS_FUN | ATOM | BINARY | BINARY_COMPREHENSION | BLOCK_EXPR | CASE_EXPR
            | CHAR | CONCATABLES | EXTERNAL_FUN | FLOAT | FUN_TYPE | IF_EXPR | INTEGER
            | INTERNAL_FUN | LIST | LIST_COMPREHENSION | MACRO_CALL_EXPR | MACRO_STRING
            | MAP_COMPREHENSION | MAYBE_EXPR | PAREN_EXPR | RECEIVE_EXPR | STRING | TRY_EXPR
            | TUPLE | VAR | ANN_TYPE | BINARY_OP_EXPR | CALL | CATCH_EXPR | COND_MATCH_EXPR
            | DOTDOTDOT | MAP_EXPR | MAP_EXPR_UPDATE | MATCH_EXPR | PIPE | RANGE_TYPE
            | RECORD_EXPR | RECORD_FIELD_EXPR | RECORD_INDEX_EXPR | RECORD_UPDATE_EXPR | REMOTE
            | UNARY_OP_EXPR | B_GENERATOR | GENERATOR | MAP_GENERATOR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            ANONYMOUS_FUN | ATOM | BINARY | BINARY_COMPREHENSION | BLOCK_EXPR | CASE_EXPR
            | CHAR | CONCATABLES | EXTERNAL_FUN | FLOAT | FUN_TYPE | IF_EXPR | INTEGER
            | INTERNAL_FUN | LIST | LIST_COMPREHENSION | MACRO_CALL_EXPR | MACRO_STRING
            | MAP_COMPREHENSION | MAYBE_EXPR | PAREN_EXPR | RECEIVE_EXPR | STRING | TRY_EXPR
            | TUPLE | VAR | ANN_TYPE | BINARY_OP_EXPR | CALL | CATCH_EXPR | COND_MATCH_EXPR
            | DOTDOTDOT | MAP_EXPR | MAP_EXPR_UPDATE | MATCH_EXPR | PIPE | RANGE_TYPE
            | RECORD_EXPR | RECORD_FIELD_EXPR | RECORD_INDEX_EXPR | RECORD_UPDATE_EXPR | REMOTE
            | UNARY_OP_EXPR => Some(LcExpr::Expr(Expr::cast(syntax).unwrap())),
            B_GENERATOR => Some(LcExpr::BGenerator(BGenerator { syntax })),
            GENERATOR => Some(LcExpr::Generator(Generator { syntax })),
            MAP_GENERATOR => Some(LcExpr::MapGenerator(MapGenerator { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            LcExpr::Expr(it) => it.syntax(),
            LcExpr::BGenerator(it) => it.syntax(),
            LcExpr::Generator(it) => it.syntax(),
            LcExpr::MapGenerator(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<Expr> for LcExpr {
    fn from(node: Expr) -> LcExpr {
        LcExpr::Expr(node)
    }
}
impl From<BGenerator> for LcExpr {
    fn from(node: BGenerator) -> LcExpr {
        LcExpr::BGenerator(node)
    }
}
impl From<Generator> for LcExpr {
    fn from(node: Generator) -> LcExpr {
        LcExpr::Generator(node)
    }
}
impl From<MapGenerator> for LcExpr {
    fn from(node: MapGenerator) -> LcExpr {
        LcExpr::MapGenerator(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for LcExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LcExprs {
    pub(crate) syntax: SyntaxNode,
}
impl LcExprs {
    pub fn exprs(&self) -> AstChildren<LcOrZcExpr> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for LcExprs {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LC_EXPRS
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for LcExprs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LcOrZcExpr {
    pub(crate) syntax: SyntaxNode,
}
impl LcOrZcExpr {
    pub fn exprs(&self) -> AstChildren<LcExpr> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for LcOrZcExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LC_OR_ZC_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for LcOrZcExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct List {
    pub(crate) syntax: SyntaxNode,
}
impl List {
    pub fn exprs(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for List {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LIST
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ListComprehension {
    pub(crate) syntax: SyntaxNode,
}
impl ListComprehension {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn lc_exprs(&self) -> Option<LcExprs> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ListComprehension {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LIST_COMPREHENSION
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ListComprehension {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroCallArgs {
    pub(crate) syntax: SyntaxNode,
}
impl MacroCallArgs {
    pub fn args(&self) -> AstChildren<MacroExpr> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for MacroCallArgs {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MACRO_CALL_ARGS
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for MacroCallArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroCallExpr {
    pub(crate) syntax: SyntaxNode,
}
impl MacroCallExpr {
    pub fn name(&self) -> Option<MacroName> {
        support::child(&self.syntax, 0usize)
    }
    pub fn args(&self) -> Option<MacroCallArgs> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for MacroCallExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MACRO_CALL_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for MacroCallExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MacroDefReplacement {
    Expr(Expr),
    ReplacementCrClauses(ReplacementCrClauses),
    ReplacementExprGuard(ReplacementExprGuard),
    ReplacementFunctionClauses(ReplacementFunctionClauses),
    ReplacementGuardAnd(ReplacementGuardAnd),
    ReplacementGuardOr(ReplacementGuardOr),
    ReplacementParens(ReplacementParens),
}
impl AstNode for MacroDefReplacement {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ANONYMOUS_FUN
            | ATOM
            | BINARY
            | BINARY_COMPREHENSION
            | BLOCK_EXPR
            | CASE_EXPR
            | CHAR
            | CONCATABLES
            | EXTERNAL_FUN
            | FLOAT
            | FUN_TYPE
            | IF_EXPR
            | INTEGER
            | INTERNAL_FUN
            | LIST
            | LIST_COMPREHENSION
            | MACRO_CALL_EXPR
            | MACRO_STRING
            | MAP_COMPREHENSION
            | MAYBE_EXPR
            | PAREN_EXPR
            | RECEIVE_EXPR
            | STRING
            | TRY_EXPR
            | TUPLE
            | VAR
            | ANN_TYPE
            | BINARY_OP_EXPR
            | CALL
            | CATCH_EXPR
            | COND_MATCH_EXPR
            | DOTDOTDOT
            | MAP_EXPR
            | MAP_EXPR_UPDATE
            | MATCH_EXPR
            | PIPE
            | RANGE_TYPE
            | RECORD_EXPR
            | RECORD_FIELD_EXPR
            | RECORD_INDEX_EXPR
            | RECORD_UPDATE_EXPR
            | REMOTE
            | UNARY_OP_EXPR
            | REPLACEMENT_CR_CLAUSES
            | REPLACEMENT_EXPR_GUARD
            | REPLACEMENT_FUNCTION_CLAUSES
            | REPLACEMENT_GUARD_AND
            | REPLACEMENT_GUARD_OR
            | REPLACEMENT_PARENS => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            ANONYMOUS_FUN | ATOM | BINARY | BINARY_COMPREHENSION | BLOCK_EXPR | CASE_EXPR
            | CHAR | CONCATABLES | EXTERNAL_FUN | FLOAT | FUN_TYPE | IF_EXPR | INTEGER
            | INTERNAL_FUN | LIST | LIST_COMPREHENSION | MACRO_CALL_EXPR | MACRO_STRING
            | MAP_COMPREHENSION | MAYBE_EXPR | PAREN_EXPR | RECEIVE_EXPR | STRING | TRY_EXPR
            | TUPLE | VAR | ANN_TYPE | BINARY_OP_EXPR | CALL | CATCH_EXPR | COND_MATCH_EXPR
            | DOTDOTDOT | MAP_EXPR | MAP_EXPR_UPDATE | MATCH_EXPR | PIPE | RANGE_TYPE
            | RECORD_EXPR | RECORD_FIELD_EXPR | RECORD_INDEX_EXPR | RECORD_UPDATE_EXPR | REMOTE
            | UNARY_OP_EXPR => Some(MacroDefReplacement::Expr(Expr::cast(syntax).unwrap())),
            REPLACEMENT_CR_CLAUSES => Some(MacroDefReplacement::ReplacementCrClauses(
                ReplacementCrClauses { syntax },
            )),
            REPLACEMENT_EXPR_GUARD => Some(MacroDefReplacement::ReplacementExprGuard(
                ReplacementExprGuard { syntax },
            )),
            REPLACEMENT_FUNCTION_CLAUSES => Some(MacroDefReplacement::ReplacementFunctionClauses(
                ReplacementFunctionClauses { syntax },
            )),
            REPLACEMENT_GUARD_AND => Some(MacroDefReplacement::ReplacementGuardAnd(
                ReplacementGuardAnd { syntax },
            )),
            REPLACEMENT_GUARD_OR => Some(MacroDefReplacement::ReplacementGuardOr(
                ReplacementGuardOr { syntax },
            )),
            REPLACEMENT_PARENS => Some(MacroDefReplacement::ReplacementParens(ReplacementParens {
                syntax,
            })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            MacroDefReplacement::Expr(it) => it.syntax(),
            MacroDefReplacement::ReplacementCrClauses(it) => it.syntax(),
            MacroDefReplacement::ReplacementExprGuard(it) => it.syntax(),
            MacroDefReplacement::ReplacementFunctionClauses(it) => it.syntax(),
            MacroDefReplacement::ReplacementGuardAnd(it) => it.syntax(),
            MacroDefReplacement::ReplacementGuardOr(it) => it.syntax(),
            MacroDefReplacement::ReplacementParens(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<Expr> for MacroDefReplacement {
    fn from(node: Expr) -> MacroDefReplacement {
        MacroDefReplacement::Expr(node)
    }
}
impl From<ReplacementCrClauses> for MacroDefReplacement {
    fn from(node: ReplacementCrClauses) -> MacroDefReplacement {
        MacroDefReplacement::ReplacementCrClauses(node)
    }
}
impl From<ReplacementExprGuard> for MacroDefReplacement {
    fn from(node: ReplacementExprGuard) -> MacroDefReplacement {
        MacroDefReplacement::ReplacementExprGuard(node)
    }
}
impl From<ReplacementFunctionClauses> for MacroDefReplacement {
    fn from(node: ReplacementFunctionClauses) -> MacroDefReplacement {
        MacroDefReplacement::ReplacementFunctionClauses(node)
    }
}
impl From<ReplacementGuardAnd> for MacroDefReplacement {
    fn from(node: ReplacementGuardAnd) -> MacroDefReplacement {
        MacroDefReplacement::ReplacementGuardAnd(node)
    }
}
impl From<ReplacementGuardOr> for MacroDefReplacement {
    fn from(node: ReplacementGuardOr) -> MacroDefReplacement {
        MacroDefReplacement::ReplacementGuardOr(node)
    }
}
impl From<ReplacementParens> for MacroDefReplacement {
    fn from(node: ReplacementParens) -> MacroDefReplacement {
        MacroDefReplacement::ReplacementParens(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for MacroDefReplacement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroExpr {
    pub(crate) syntax: SyntaxNode,
}
impl MacroExpr {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn guard(&self) -> Option<Expr> {
        support::child(&self.syntax, 1usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for MacroExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MACRO_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for MacroExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroLhs {
    pub(crate) syntax: SyntaxNode,
}
impl MacroLhs {
    pub fn name(&self) -> Option<MacroName> {
        support::child(&self.syntax, 0usize)
    }
    pub fn args(&self) -> Option<VarArgs> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for MacroLhs {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MACRO_LHS
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for MacroLhs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MacroName {
    Atom(Atom),
    Var(Var),
}
impl AstNode for MacroName {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ATOM | VAR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            ATOM => Some(MacroName::Atom(Atom { syntax })),
            VAR => Some(MacroName::Var(Var { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            MacroName::Atom(it) => it.syntax(),
            MacroName::Var(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<Atom> for MacroName {
    fn from(node: Atom) -> MacroName {
        MacroName::Atom(node)
    }
}
impl From<Var> for MacroName {
    fn from(node: Var) -> MacroName {
        MacroName::Var(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for MacroName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroString {
    pub(crate) syntax: SyntaxNode,
}
impl MacroString {
    pub fn name(&self) -> Option<MacroName> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for MacroString {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MACRO_STRING
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for MacroString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MapComprehension {
    pub(crate) syntax: SyntaxNode,
}
impl MapComprehension {
    pub fn lc_exprs(&self) -> Option<LcExprs> {
        support::child(&self.syntax, 0usize)
    }
    pub fn expr(&self) -> Option<MapField> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for MapComprehension {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MAP_COMPREHENSION
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for MapComprehension {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MapExpr {
    pub(crate) syntax: SyntaxNode,
}
impl MapExpr {
    pub fn fields(&self) -> AstChildren<MapField> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for MapExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MAP_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for MapExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MapExprBase {
    ExprMax(ExprMax),
    MapExpr(MapExpr),
    MapExprUpdate(MapExprUpdate),
}
impl AstNode for MapExprBase {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ANONYMOUS_FUN | ATOM | BINARY | BINARY_COMPREHENSION | BLOCK_EXPR | CASE_EXPR
            | CHAR | CONCATABLES | EXTERNAL_FUN | FLOAT | FUN_TYPE | IF_EXPR | INTEGER
            | INTERNAL_FUN | LIST | LIST_COMPREHENSION | MACRO_CALL_EXPR | MACRO_STRING
            | MAP_COMPREHENSION | MAYBE_EXPR | PAREN_EXPR | RECEIVE_EXPR | STRING | TRY_EXPR
            | TUPLE | VAR | MAP_EXPR | MAP_EXPR_UPDATE => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            ANONYMOUS_FUN | ATOM | BINARY | BINARY_COMPREHENSION | BLOCK_EXPR | CASE_EXPR
            | CHAR | CONCATABLES | EXTERNAL_FUN | FLOAT | FUN_TYPE | IF_EXPR | INTEGER
            | INTERNAL_FUN | LIST | LIST_COMPREHENSION | MACRO_CALL_EXPR | MACRO_STRING
            | MAP_COMPREHENSION | MAYBE_EXPR | PAREN_EXPR | RECEIVE_EXPR | STRING | TRY_EXPR
            | TUPLE | VAR => Some(MapExprBase::ExprMax(ExprMax::cast(syntax).unwrap())),
            MAP_EXPR => Some(MapExprBase::MapExpr(MapExpr { syntax })),
            MAP_EXPR_UPDATE => Some(MapExprBase::MapExprUpdate(MapExprUpdate { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            MapExprBase::ExprMax(it) => it.syntax(),
            MapExprBase::MapExpr(it) => it.syntax(),
            MapExprBase::MapExprUpdate(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<ExprMax> for MapExprBase {
    fn from(node: ExprMax) -> MapExprBase {
        MapExprBase::ExprMax(node)
    }
}
impl From<MapExpr> for MapExprBase {
    fn from(node: MapExpr) -> MapExprBase {
        MapExprBase::MapExpr(node)
    }
}
impl From<MapExprUpdate> for MapExprBase {
    fn from(node: MapExprUpdate) -> MapExprBase {
        MapExprBase::MapExprUpdate(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for MapExprBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MapExprUpdate {
    pub(crate) syntax: SyntaxNode,
}
impl MapExprUpdate {
    pub fn expr(&self) -> Option<MapExprBase> {
        support::child(&self.syntax, 0usize)
    }
    pub fn fields(&self) -> AstChildren<MapField> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for MapExprUpdate {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MAP_EXPR_UPDATE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for MapExprUpdate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MapField {
    pub(crate) syntax: SyntaxNode,
}
impl MapField {
    pub fn key(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn value(&self) -> Option<Expr> {
        support::child(&self.syntax, 1usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for MapField {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MAP_FIELD
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for MapField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MapGenerator {
    pub(crate) syntax: SyntaxNode,
}
impl MapGenerator {
    pub fn rhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn lhs(&self) -> Option<MapField> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for MapGenerator {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MAP_GENERATOR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for MapGenerator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MatchExpr {
    pub(crate) syntax: SyntaxNode,
}
impl MatchExpr {
    pub fn lhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn rhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 1usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for MatchExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MATCH_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for MatchExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MaybeExpr {
    pub(crate) syntax: SyntaxNode,
}
impl MaybeExpr {
    pub fn clauses(&self) -> AstChildren<CrClauseOrMacro> {
        support::children(&self.syntax)
    }
    pub fn exprs(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for MaybeExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MAYBE_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for MaybeExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub(crate) syntax: SyntaxNode,
}
impl Module {
    pub fn name(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for Module {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MODULE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleAttribute {
    pub(crate) syntax: SyntaxNode,
}
impl ModuleAttribute {
    pub fn name(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ModuleAttribute {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MODULE_ATTRIBUTE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ModuleAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MultiString {
    pub(crate) syntax: SyntaxNode,
}
impl MultiString {
    pub fn elems(&self) -> AstChildren<StringLike> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for MultiString {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MULTI_STRING
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for MultiString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Name {
    Atom(Atom),
    MacroCallExpr(MacroCallExpr),
    Var(Var),
}
impl AstNode for Name {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ATOM | MACRO_CALL_EXPR | VAR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            ATOM => Some(Name::Atom(Atom { syntax })),
            MACRO_CALL_EXPR => Some(Name::MacroCallExpr(MacroCallExpr { syntax })),
            VAR => Some(Name::Var(Var { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Name::Atom(it) => it.syntax(),
            Name::MacroCallExpr(it) => it.syntax(),
            Name::Var(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<Atom> for Name {
    fn from(node: Atom) -> Name {
        Name::Atom(node)
    }
}
impl From<MacroCallExpr> for Name {
    fn from(node: MacroCallExpr) -> Name {
        Name::MacroCallExpr(node)
    }
}
impl From<Var> for Name {
    fn from(node: Var) -> Name {
        Name::Var(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Opaque {
    pub(crate) syntax: SyntaxNode,
}
impl Opaque {
    pub fn ty(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn name(&self) -> Option<TypeName> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for Opaque {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == OPAQUE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for Opaque {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OptionalCallbacksAttribute {
    pub(crate) syntax: SyntaxNode,
}
impl OptionalCallbacksAttribute {
    pub fn callbacks(&self) -> AstChildren<Fa> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for OptionalCallbacksAttribute {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == OPTIONAL_CALLBACKS_ATTRIBUTE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for OptionalCallbacksAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParenExpr {
    pub(crate) syntax: SyntaxNode,
}
impl ParenExpr {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ParenExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PAREN_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ParenExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Pipe {
    pub(crate) syntax: SyntaxNode,
}
impl Pipe {
    pub fn lhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn rhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 1usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for Pipe {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PIPE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for Pipe {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PpDefine {
    pub(crate) syntax: SyntaxNode,
}
impl PpDefine {
    pub fn replacement(&self) -> Option<MacroDefReplacement> {
        support::child(&self.syntax, 0usize)
    }
    pub fn lhs(&self) -> Option<MacroLhs> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for PpDefine {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PP_DEFINE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for PpDefine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PpElif {
    pub(crate) syntax: SyntaxNode,
}
impl PpElif {
    pub fn cond(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for PpElif {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PP_ELIF
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for PpElif {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PpElse {
    pub(crate) syntax: SyntaxNode,
}
impl PpElse {}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for PpElse {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PP_ELSE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for PpElse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PpEndif {
    pub(crate) syntax: SyntaxNode,
}
impl PpEndif {}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for PpEndif {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PP_ENDIF
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for PpEndif {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PpIf {
    pub(crate) syntax: SyntaxNode,
}
impl PpIf {
    pub fn cond(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for PpIf {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PP_IF
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for PpIf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PpIfdef {
    pub(crate) syntax: SyntaxNode,
}
impl PpIfdef {
    pub fn name(&self) -> Option<MacroName> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for PpIfdef {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PP_IFDEF
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for PpIfdef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PpIfndef {
    pub(crate) syntax: SyntaxNode,
}
impl PpIfndef {
    pub fn name(&self) -> Option<MacroName> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for PpIfndef {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PP_IFNDEF
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for PpIfndef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PpInclude {
    pub(crate) syntax: SyntaxNode,
}
impl PpInclude {
    pub fn file(&self) -> AstChildren<IncludeDetail> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for PpInclude {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PP_INCLUDE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for PpInclude {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PpIncludeLib {
    pub(crate) syntax: SyntaxNode,
}
impl PpIncludeLib {
    pub fn file(&self) -> AstChildren<IncludeDetail> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for PpIncludeLib {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PP_INCLUDE_LIB
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for PpIncludeLib {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PpUndef {
    pub(crate) syntax: SyntaxNode,
}
impl PpUndef {
    pub fn name(&self) -> Option<MacroName> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for PpUndef {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PP_UNDEF
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for PpUndef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PreprocessorDirective {
    PpDefine(PpDefine),
    PpElif(PpElif),
    PpElse(PpElse),
    PpEndif(PpEndif),
    PpIf(PpIf),
    PpIfdef(PpIfdef),
    PpIfndef(PpIfndef),
    PpInclude(PpInclude),
    PpIncludeLib(PpIncludeLib),
    PpUndef(PpUndef),
}
impl AstNode for PreprocessorDirective {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            PP_DEFINE | PP_ELIF | PP_ELSE | PP_ENDIF | PP_IF | PP_IFDEF | PP_IFNDEF
            | PP_INCLUDE | PP_INCLUDE_LIB | PP_UNDEF => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            PP_DEFINE => Some(PreprocessorDirective::PpDefine(PpDefine { syntax })),
            PP_ELIF => Some(PreprocessorDirective::PpElif(PpElif { syntax })),
            PP_ELSE => Some(PreprocessorDirective::PpElse(PpElse { syntax })),
            PP_ENDIF => Some(PreprocessorDirective::PpEndif(PpEndif { syntax })),
            PP_IF => Some(PreprocessorDirective::PpIf(PpIf { syntax })),
            PP_IFDEF => Some(PreprocessorDirective::PpIfdef(PpIfdef { syntax })),
            PP_IFNDEF => Some(PreprocessorDirective::PpIfndef(PpIfndef { syntax })),
            PP_INCLUDE => Some(PreprocessorDirective::PpInclude(PpInclude { syntax })),
            PP_INCLUDE_LIB => Some(PreprocessorDirective::PpIncludeLib(PpIncludeLib { syntax })),
            PP_UNDEF => Some(PreprocessorDirective::PpUndef(PpUndef { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            PreprocessorDirective::PpDefine(it) => it.syntax(),
            PreprocessorDirective::PpElif(it) => it.syntax(),
            PreprocessorDirective::PpElse(it) => it.syntax(),
            PreprocessorDirective::PpEndif(it) => it.syntax(),
            PreprocessorDirective::PpIf(it) => it.syntax(),
            PreprocessorDirective::PpIfdef(it) => it.syntax(),
            PreprocessorDirective::PpIfndef(it) => it.syntax(),
            PreprocessorDirective::PpInclude(it) => it.syntax(),
            PreprocessorDirective::PpIncludeLib(it) => it.syntax(),
            PreprocessorDirective::PpUndef(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<PpDefine> for PreprocessorDirective {
    fn from(node: PpDefine) -> PreprocessorDirective {
        PreprocessorDirective::PpDefine(node)
    }
}
impl From<PpElif> for PreprocessorDirective {
    fn from(node: PpElif) -> PreprocessorDirective {
        PreprocessorDirective::PpElif(node)
    }
}
impl From<PpElse> for PreprocessorDirective {
    fn from(node: PpElse) -> PreprocessorDirective {
        PreprocessorDirective::PpElse(node)
    }
}
impl From<PpEndif> for PreprocessorDirective {
    fn from(node: PpEndif) -> PreprocessorDirective {
        PreprocessorDirective::PpEndif(node)
    }
}
impl From<PpIf> for PreprocessorDirective {
    fn from(node: PpIf) -> PreprocessorDirective {
        PreprocessorDirective::PpIf(node)
    }
}
impl From<PpIfdef> for PreprocessorDirective {
    fn from(node: PpIfdef) -> PreprocessorDirective {
        PreprocessorDirective::PpIfdef(node)
    }
}
impl From<PpIfndef> for PreprocessorDirective {
    fn from(node: PpIfndef) -> PreprocessorDirective {
        PreprocessorDirective::PpIfndef(node)
    }
}
impl From<PpInclude> for PreprocessorDirective {
    fn from(node: PpInclude) -> PreprocessorDirective {
        PreprocessorDirective::PpInclude(node)
    }
}
impl From<PpIncludeLib> for PreprocessorDirective {
    fn from(node: PpIncludeLib) -> PreprocessorDirective {
        PreprocessorDirective::PpIncludeLib(node)
    }
}
impl From<PpUndef> for PreprocessorDirective {
    fn from(node: PpUndef) -> PreprocessorDirective {
        PreprocessorDirective::PpUndef(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for PreprocessorDirective {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RangeType {
    pub(crate) syntax: SyntaxNode,
}
impl RangeType {
    pub fn lhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn rhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 1usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for RangeType {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == RANGE_TYPE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for RangeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReceiveAfter {
    pub(crate) syntax: SyntaxNode,
}
impl ReceiveAfter {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn body(&self) -> Option<ClauseBody> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ReceiveAfter {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == RECEIVE_AFTER
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ReceiveAfter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReceiveExpr {
    pub(crate) syntax: SyntaxNode,
}
impl ReceiveExpr {
    pub fn clauses(&self) -> AstChildren<CrClauseOrMacro> {
        support::children(&self.syntax)
    }
    pub fn after(&self) -> Option<ReceiveAfter> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ReceiveExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == RECEIVE_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ReceiveExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordDecl {
    pub(crate) syntax: SyntaxNode,
}
impl RecordDecl {
    pub fn name(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
    pub fn fields(&self) -> AstChildren<RecordField> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for RecordDecl {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == RECORD_DECL
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for RecordDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordExpr {
    pub(crate) syntax: SyntaxNode,
}
impl RecordExpr {
    pub fn fields(&self) -> AstChildren<RecordField> {
        support::children(&self.syntax)
    }
    pub fn name(&self) -> Option<RecordName> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for RecordExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == RECORD_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for RecordExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecordExprBase {
    ExprMax(ExprMax),
    RecordExpr(RecordExpr),
    RecordFieldExpr(RecordFieldExpr),
    RecordIndexExpr(RecordIndexExpr),
    RecordUpdateExpr(RecordUpdateExpr),
}
impl AstNode for RecordExprBase {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            ANONYMOUS_FUN | ATOM | BINARY | BINARY_COMPREHENSION | BLOCK_EXPR | CASE_EXPR
            | CHAR | CONCATABLES | EXTERNAL_FUN | FLOAT | FUN_TYPE | IF_EXPR | INTEGER
            | INTERNAL_FUN | LIST | LIST_COMPREHENSION | MACRO_CALL_EXPR | MACRO_STRING
            | MAP_COMPREHENSION | MAYBE_EXPR | PAREN_EXPR | RECEIVE_EXPR | STRING | TRY_EXPR
            | TUPLE | VAR | RECORD_EXPR | RECORD_FIELD_EXPR | RECORD_INDEX_EXPR
            | RECORD_UPDATE_EXPR => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            ANONYMOUS_FUN | ATOM | BINARY | BINARY_COMPREHENSION | BLOCK_EXPR | CASE_EXPR
            | CHAR | CONCATABLES | EXTERNAL_FUN | FLOAT | FUN_TYPE | IF_EXPR | INTEGER
            | INTERNAL_FUN | LIST | LIST_COMPREHENSION | MACRO_CALL_EXPR | MACRO_STRING
            | MAP_COMPREHENSION | MAYBE_EXPR | PAREN_EXPR | RECEIVE_EXPR | STRING | TRY_EXPR
            | TUPLE | VAR => Some(RecordExprBase::ExprMax(ExprMax::cast(syntax).unwrap())),
            RECORD_EXPR => Some(RecordExprBase::RecordExpr(RecordExpr { syntax })),
            RECORD_FIELD_EXPR => Some(RecordExprBase::RecordFieldExpr(RecordFieldExpr { syntax })),
            RECORD_INDEX_EXPR => Some(RecordExprBase::RecordIndexExpr(RecordIndexExpr { syntax })),
            RECORD_UPDATE_EXPR => Some(RecordExprBase::RecordUpdateExpr(RecordUpdateExpr {
                syntax,
            })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            RecordExprBase::ExprMax(it) => it.syntax(),
            RecordExprBase::RecordExpr(it) => it.syntax(),
            RecordExprBase::RecordFieldExpr(it) => it.syntax(),
            RecordExprBase::RecordIndexExpr(it) => it.syntax(),
            RecordExprBase::RecordUpdateExpr(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<ExprMax> for RecordExprBase {
    fn from(node: ExprMax) -> RecordExprBase {
        RecordExprBase::ExprMax(node)
    }
}
impl From<RecordExpr> for RecordExprBase {
    fn from(node: RecordExpr) -> RecordExprBase {
        RecordExprBase::RecordExpr(node)
    }
}
impl From<RecordFieldExpr> for RecordExprBase {
    fn from(node: RecordFieldExpr) -> RecordExprBase {
        RecordExprBase::RecordFieldExpr(node)
    }
}
impl From<RecordIndexExpr> for RecordExprBase {
    fn from(node: RecordIndexExpr) -> RecordExprBase {
        RecordExprBase::RecordIndexExpr(node)
    }
}
impl From<RecordUpdateExpr> for RecordExprBase {
    fn from(node: RecordUpdateExpr) -> RecordExprBase {
        RecordExprBase::RecordUpdateExpr(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for RecordExprBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordField {
    pub(crate) syntax: SyntaxNode,
}
impl RecordField {
    pub fn name(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
    pub fn expr(&self) -> Option<FieldExpr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn ty(&self) -> Option<FieldType> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for RecordField {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == RECORD_FIELD
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for RecordField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordFieldExpr {
    pub(crate) syntax: SyntaxNode,
}
impl RecordFieldExpr {
    pub fn expr(&self) -> Option<RecordExprBase> {
        support::child(&self.syntax, 0usize)
    }
    pub fn field(&self) -> Option<RecordFieldName> {
        support::child(&self.syntax, 0usize)
    }
    pub fn name(&self) -> Option<RecordName> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for RecordFieldExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == RECORD_FIELD_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for RecordFieldExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordFieldName {
    pub(crate) syntax: SyntaxNode,
}
impl RecordFieldName {
    pub fn name(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for RecordFieldName {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == RECORD_FIELD_NAME
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for RecordFieldName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordIndexExpr {
    pub(crate) syntax: SyntaxNode,
}
impl RecordIndexExpr {
    pub fn field(&self) -> Option<RecordFieldName> {
        support::child(&self.syntax, 0usize)
    }
    pub fn name(&self) -> Option<RecordName> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for RecordIndexExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == RECORD_INDEX_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for RecordIndexExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordName {
    pub(crate) syntax: SyntaxNode,
}
impl RecordName {
    pub fn name(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for RecordName {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == RECORD_NAME
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for RecordName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordUpdateExpr {
    pub(crate) syntax: SyntaxNode,
}
impl RecordUpdateExpr {
    pub fn expr(&self) -> Option<RecordExprBase> {
        support::child(&self.syntax, 0usize)
    }
    pub fn fields(&self) -> AstChildren<RecordField> {
        support::children(&self.syntax)
    }
    pub fn name(&self) -> Option<RecordName> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for RecordUpdateExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == RECORD_UPDATE_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for RecordUpdateExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Remote {
    pub(crate) syntax: SyntaxNode,
}
impl Remote {
    pub fn fun(&self) -> Option<ExprMax> {
        support::child(&self.syntax, 0usize)
    }
    pub fn module(&self) -> Option<RemoteModule> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for Remote {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == REMOTE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for Remote {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RemoteModule {
    pub(crate) syntax: SyntaxNode,
}
impl RemoteModule {
    pub fn module(&self) -> Option<ExprMax> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for RemoteModule {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == REMOTE_MODULE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for RemoteModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReplacementCrClauses {
    pub(crate) syntax: SyntaxNode,
}
impl ReplacementCrClauses {
    pub fn clauses(&self) -> AstChildren<CrClauseOrMacro> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ReplacementCrClauses {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == REPLACEMENT_CR_CLAUSES
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ReplacementCrClauses {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReplacementExprGuard {
    pub(crate) syntax: SyntaxNode,
}
impl ReplacementExprGuard {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn guard(&self) -> Option<Guard> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ReplacementExprGuard {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == REPLACEMENT_EXPR_GUARD
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ReplacementExprGuard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReplacementFunctionClauses {
    pub(crate) syntax: SyntaxNode,
}
impl ReplacementFunctionClauses {
    pub fn clauses(&self) -> AstChildren<FunctionOrMacroClause> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ReplacementFunctionClauses {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == REPLACEMENT_FUNCTION_CLAUSES
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ReplacementFunctionClauses {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReplacementGuardAnd {
    pub(crate) syntax: SyntaxNode,
}
impl ReplacementGuardAnd {
    pub fn guard(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ReplacementGuardAnd {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == REPLACEMENT_GUARD_AND
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ReplacementGuardAnd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReplacementGuardOr {
    pub(crate) syntax: SyntaxNode,
}
impl ReplacementGuardOr {
    pub fn guard(&self) -> AstChildren<ReplacementGuardAnd> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ReplacementGuardOr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == REPLACEMENT_GUARD_OR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ReplacementGuardOr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReplacementParens {
    pub(crate) syntax: SyntaxNode,
}
impl ReplacementParens {}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for ReplacementParens {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == REPLACEMENT_PARENS
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for ReplacementParens {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Literal 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Shebang {
    pub(crate) syntax: SyntaxNode,
}
#[doc = r" Via NodeType::Literal 2"]
impl Shebang {
    pub fn self_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SHEBANG, 0)
    }
}
#[doc = r" Via NodeType::Literal 2"]
impl AstNode for Shebang {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SHEBANG
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
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceFile {
    pub(crate) syntax: SyntaxNode,
}
impl SourceFile {
    pub fn exprs(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
    pub fn forms_only(&self) -> AstChildren<Form> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for SourceFile {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SOURCE_FILE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spec {
    pub(crate) syntax: SyntaxNode,
}
impl Spec {
    pub fn fun(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
    pub fn module(&self) -> Option<Module> {
        support::child(&self.syntax, 0usize)
    }
    pub fn sigs(&self) -> AstChildren<TypeSig> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for Spec {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SPEC
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for Spec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SsrDefinition {
    pub(crate) syntax: SyntaxNode,
}
impl SsrDefinition {
    pub fn lhs(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn rhs(&self) -> Option<SsrReplacement> {
        support::child(&self.syntax, 0usize)
    }
    pub fn when(&self) -> Option<SsrWhen> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for SsrDefinition {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SSR_DEFINITION
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for SsrDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SsrReplacement {
    pub(crate) syntax: SyntaxNode,
}
impl SsrReplacement {
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for SsrReplacement {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SSR_REPLACEMENT
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for SsrReplacement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SsrWhen {
    pub(crate) syntax: SyntaxNode,
}
impl SsrWhen {
    pub fn guard(&self) -> Option<Guard> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for SsrWhen {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SSR_WHEN
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for SsrWhen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct String {
    pub(crate) syntax: SyntaxNode,
}
impl String {}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for String {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == STRING
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Enum 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StringLike {
    MacroCallExpr(MacroCallExpr),
    MacroString(MacroString),
    String(String),
}
impl AstNode for StringLike {
    #[allow(clippy::match_like_matches_macro)]
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            MACRO_CALL_EXPR | MACRO_STRING | STRING => true,
            _ => false,
        }
    }
    #[allow(clippy::match_like_matches_macro)]
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        match syntax.kind() {
            MACRO_CALL_EXPR => Some(StringLike::MacroCallExpr(MacroCallExpr { syntax })),
            MACRO_STRING => Some(StringLike::MacroString(MacroString { syntax })),
            STRING => Some(StringLike::String(String { syntax })),
            _ => None,
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            StringLike::MacroCallExpr(it) => it.syntax(),
            StringLike::MacroString(it) => it.syntax(),
            StringLike::String(it) => it.syntax(),
        }
    }
}
#[doc = r" Via NodeType::Enum 2 forms"]
impl From<MacroCallExpr> for StringLike {
    fn from(node: MacroCallExpr) -> StringLike {
        StringLike::MacroCallExpr(node)
    }
}
impl From<MacroString> for StringLike {
    fn from(node: MacroString) -> StringLike {
        StringLike::MacroString(node)
    }
}
impl From<String> for StringLike {
    fn from(node: String) -> StringLike {
        StringLike::String(node)
    }
}
#[doc = r" Via NodeType::Enum 2 display"]
impl std::fmt::Display for StringLike {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TryAfter {
    pub(crate) syntax: SyntaxNode,
}
impl TryAfter {
    pub fn exprs(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for TryAfter {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TRY_AFTER
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for TryAfter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TryClass {
    pub(crate) syntax: SyntaxNode,
}
impl TryClass {
    pub fn class(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for TryClass {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TRY_CLASS
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for TryClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TryExpr {
    pub(crate) syntax: SyntaxNode,
}
impl TryExpr {
    pub fn clauses(&self) -> AstChildren<CrClauseOrMacro> {
        support::children(&self.syntax)
    }
    pub fn exprs(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
    pub fn catch(&self) -> AstChildren<CatchClause> {
        support::children(&self.syntax)
    }
    pub fn after(&self) -> Option<TryAfter> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for TryExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TRY_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for TryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TryStack {
    pub(crate) syntax: SyntaxNode,
}
impl TryStack {
    pub fn class(&self) -> Option<Var> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for TryStack {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TRY_STACK
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for TryStack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tuple {
    pub(crate) syntax: SyntaxNode,
}
impl Tuple {
    pub fn expr(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for Tuple {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TUPLE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeAlias {
    pub(crate) syntax: SyntaxNode,
}
impl TypeAlias {
    pub fn ty(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn name(&self) -> Option<TypeName> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for TypeAlias {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TYPE_ALIAS
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for TypeAlias {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGuards {
    pub(crate) syntax: SyntaxNode,
}
impl TypeGuards {
    pub fn guards(&self) -> AstChildren<AnnType> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for TypeGuards {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TYPE_GUARDS
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for TypeGuards {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeName {
    pub(crate) syntax: SyntaxNode,
}
impl TypeName {
    pub fn name(&self) -> Option<Name> {
        support::child(&self.syntax, 0usize)
    }
    pub fn args(&self) -> Option<VarArgs> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for TypeName {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TYPE_NAME
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeSig {
    pub(crate) syntax: SyntaxNode,
}
impl TypeSig {
    pub fn ty(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn args(&self) -> Option<ExprArgs> {
        support::child(&self.syntax, 0usize)
    }
    pub fn guard(&self) -> Option<TypeGuards> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for TypeSig {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TYPE_SIG
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for TypeSig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnaryOpExpr {
    pub(crate) syntax: SyntaxNode,
}
impl UnaryOpExpr {
    pub fn operand(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for UnaryOpExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == UNARY_OP_EXPR
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for UnaryOpExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Literal 2"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub(crate) syntax: SyntaxNode,
}
#[doc = r" Via NodeType::Literal 2"]
impl Var {
    pub fn self_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, VAR, 0)
    }
}
#[doc = r" Via NodeType::Literal 2"]
impl AstNode for Var {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == VAR
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
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarArgs {
    pub(crate) syntax: SyntaxNode,
}
impl VarArgs {
    pub fn args(&self) -> AstChildren<Var> {
        support::children(&self.syntax)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for VarArgs {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == VAR_ARGS
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for VarArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Via NodeType::Node 2 struct inner"]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WildAttribute {
    pub(crate) syntax: SyntaxNode,
}
impl WildAttribute {
    pub fn value(&self) -> Option<Expr> {
        support::child(&self.syntax, 0usize)
    }
    pub fn name(&self) -> Option<AttrName> {
        support::child(&self.syntax, 0usize)
    }
}
#[doc = r" Via NodeType::Node 2 struct"]
impl AstNode for WildAttribute {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == WILD_ATTRIBUTE
    }
    #[doc = r" Via field_casts"]
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
#[doc = r" Via NodeType::Node 2 display"]
impl std::fmt::Display for WildAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
#[doc = r" Tell emacs to automatically reload this file if it changes"]
#[doc = r" Local Variables:"]
#[doc = r" auto-revert-mode: 1"]
#[doc = r" End:"]
fn _dummy() -> bool {
    false
}

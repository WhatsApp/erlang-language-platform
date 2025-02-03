/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str::Chars;
use std::sync::Arc;

use elp_base_db::FileId;
pub use elp_syntax::ast::BinaryOp;
pub use elp_syntax::ast::MapOp;
pub use elp_syntax::ast::UnaryOp;
use elp_syntax::SmolStr;
use elp_syntax::TextRange;
use la_arena::Idx;

use crate::known;
use crate::sema;
use crate::Atom;
use crate::Body;
use crate::DefineId;
use crate::FunctionClauseBody;
use crate::FunctionDef;
use crate::InFile;
use crate::InFunctionClauseBody;
use crate::RecordFieldId;
use crate::Semantic;
use crate::TypeAliasDef;
use crate::Var;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum AnyExprId {
    Expr(ExprId),
    Pat(PatId),
    TypeExpr(TypeExprId),
    Term(TermId),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AnyExpr {
    Expr(Expr),
    Pat(Pat),
    TypeExpr(TypeExpr),
    Term(Term),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AnyExprRef<'a> {
    Expr(&'a Expr),
    Pat(&'a Pat),
    TypeExpr(&'a TypeExpr),
    Term(&'a Term),
}

impl<'a> AnyExprRef<'a> {
    pub fn variant_str(&self) -> &'static str {
        match self {
            AnyExprRef::Expr(it) => it.variant_str(),
            AnyExprRef::Pat(it) => it.variant_str(),
            AnyExprRef::TypeExpr(it) => it.variant_str(),
            AnyExprRef::Term(it) => it.variant_str(),
        }
    }
}

// ---------------------------------------------------------------------

impl From<ExprId> for AnyExprId {
    fn from(id: ExprId) -> Self {
        AnyExprId::Expr(id)
    }
}

impl From<PatId> for AnyExprId {
    fn from(id: PatId) -> Self {
        AnyExprId::Pat(id)
    }
}

impl From<TypeExprId> for AnyExprId {
    fn from(id: TypeExprId) -> Self {
        AnyExprId::TypeExpr(id)
    }
}

impl From<TermId> for AnyExprId {
    fn from(id: TermId) -> Self {
        AnyExprId::Term(id)
    }
}
// ---------------------------------------------------------------------

impl From<Expr> for AnyExpr {
    fn from(item: Expr) -> Self {
        AnyExpr::Expr(item)
    }
}

impl From<Pat> for AnyExpr {
    fn from(item: Pat) -> Self {
        AnyExpr::Pat(item)
    }
}

impl From<TypeExpr> for AnyExpr {
    fn from(item: TypeExpr) -> Self {
        AnyExpr::TypeExpr(item)
    }
}

impl From<Term> for AnyExpr {
    fn from(item: Term) -> Self {
        AnyExpr::Term(item)
    }
}

// ---------------------------------------------------------------------

impl<'a> From<&'a Expr> for AnyExprRef<'a> {
    fn from(item: &'a Expr) -> Self {
        AnyExprRef::Expr(item)
    }
}

impl<'a> From<&'a Pat> for AnyExprRef<'a> {
    fn from(item: &'a Pat) -> Self {
        AnyExprRef::Pat(item)
    }
}

impl<'a> From<&'a TypeExpr> for AnyExprRef<'a> {
    fn from(item: &'a TypeExpr) -> Self {
        AnyExprRef::TypeExpr(item)
    }
}

impl<'a> From<&'a Term> for AnyExprRef<'a> {
    fn from(item: &'a Term) -> Self {
        AnyExprRef::Term(item)
    }
}

// ---------------------------------------------------------------------

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Literal {
    String(StringVariant),
    Char(char),
    Atom(Atom),
    Integer(i128), // TODO: bigints
    Float(u64),    // FIXME: f64 is not Eq
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum StringVariant {
    Normal(String),
    Verbatim(String),
}

impl StringVariant {
    pub fn chars(&self) -> Chars<'_> {
        match self {
            StringVariant::Normal(s) => s.chars(),
            StringVariant::Verbatim(s) => s.chars(),
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            StringVariant::Normal(s) => s.clone(),
            StringVariant::Verbatim(s) => s.clone(),
        }
    }
}

impl Literal {
    pub fn negate(&self) -> Option<Self> {
        match self {
            Literal::String(_) => None,
            Literal::Atom(_) => None,
            // Weird, but allowed https://github.com/erlang/otp/blob/09c601fa2183d4c545791ebcd68f869a5ab912a4/lib/stdlib/src/erl_parse.yrl#L1432
            Literal::Char(ch) => Some(Literal::Integer(-(*ch as i128))),
            Literal::Integer(int) => Some(Literal::Integer(-int)),
            Literal::Float(bits) => Some(Literal::Float((-f64::from_bits(*bits)).to_bits())),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MacroCallName {
    Var(Var),
    Atom(Atom),
    Missing,
}

pub type ExprId = Idx<Expr>;

#[derive(Debug, Clone, Eq, PartialEq)]
/// A regular Erlang expression
pub enum Expr {
    /// This is produced if the syntax tree does not have a required
    /// expression piece, or it was in some way invalid
    Missing,
    Literal(Literal),
    Var(Var),
    Match {
        lhs: PatId,
        rhs: ExprId,
    },
    Tuple {
        exprs: Vec<ExprId>,
    },
    List {
        exprs: Vec<ExprId>,
        tail: Option<ExprId>,
    },
    Binary {
        segs: Vec<BinarySeg<ExprId>>,
    },
    UnaryOp {
        expr: ExprId,
        op: UnaryOp,
    },
    BinaryOp {
        lhs: ExprId,
        rhs: ExprId,
        op: BinaryOp,
    },
    Record {
        name: Atom,
        fields: Vec<(Atom, ExprId)>,
    },
    RecordUpdate {
        expr: ExprId,
        name: Atom,
        fields: Vec<(Atom, ExprId)>,
    },
    RecordIndex {
        name: Atom,
        field: Atom,
    },
    RecordField {
        expr: ExprId,
        name: Atom,
        field: Atom,
    },
    Map {
        fields: Vec<(ExprId, ExprId)>,
    },
    MapUpdate {
        expr: ExprId,
        fields: Vec<(ExprId, MapOp, ExprId)>,
    },
    Catch {
        expr: ExprId,
    },
    MacroCall {
        // This constructor captures the point a macro is expanded
        // into an expression. This allows us to separately track the
        // arguments, for things like highlight related, or unused
        // function arguments.
        expansion: ExprId,
        args: Vec<ExprId>,
        macro_def: Option<InFile<DefineId>>,
        macro_name: MacroCallName,
    },
    Call {
        target: CallTarget<ExprId>,
        args: Vec<ExprId>,
    },
    Comprehension {
        builder: ComprehensionBuilder,
        exprs: Vec<ComprehensionExpr>,
    },
    Block {
        exprs: Vec<ExprId>,
    },
    If {
        clauses: Vec<IfClause>,
    },
    Case {
        expr: ExprId,
        clauses: Vec<CRClause>,
    },
    Receive {
        clauses: Vec<CRClause>,
        after: Option<ReceiveAfter>,
    },
    Try {
        exprs: Vec<ExprId>,
        of_clauses: Vec<CRClause>,
        catch_clauses: Vec<CatchClause>,
        after: Vec<ExprId>,
    },
    CaptureFun {
        target: CallTarget<ExprId>,
        arity: ExprId,
    },
    Closure {
        clauses: Vec<Clause>,
        name: Option<PatId>,
    },
    Maybe {
        exprs: Vec<MaybeExpr>,
        else_clauses: Vec<CRClause>,
    },
    Paren {
        // This constructor allows us to analyze the usage of parens
        // when deciding on assists.
        // Much like `Expr::MacroCall`, it is normally hidden during a
        // `fold`, but can be made visible if needed.
        expr: ExprId,
    },
    SsrPlaceholder(SsrPlaceholder),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct SsrPlaceholder {
    pub var: Var,
}

// We need to treat Var specially for SSR matching, as we want to be
// able to match a Pat::Var and Expr::Var occurring as a placeholder
// result as the same, if the placeholders have the same name.
pub const COMMON_VAR_VARIANT_STR: &'static str = "::Var";

impl Expr {
    pub fn as_atom(&self) -> Option<Atom> {
        match self {
            Expr::Literal(Literal::Atom(atom)) => Some(*atom),
            _ => None,
        }
    }

    pub fn as_var(&self) -> Option<Var> {
        match self {
            Expr::Var(var) => Some(*var),
            _ => None,
        }
    }

    pub fn as_record_name(&self) -> Option<&Atom> {
        match self {
            Expr::Record { name, .. }
            | Expr::RecordField { name, .. }
            | Expr::RecordIndex { name, .. }
            | Expr::RecordUpdate { name, .. } => Some(name),
            _ => None,
        }
    }

    pub fn list_length(&self) -> Option<usize> {
        match &self {
            Expr::List { exprs, tail } => {
                // Deal with a simple list only.
                if tail.is_some() {
                    None
                } else {
                    Some(exprs.len())
                }
            }
            _ => None,
        }
    }

    /// Check whether the provided atom is contained in the list.
    /// If the list contains elements other than literals, return None.
    pub fn literal_list_contains_atom(
        &self,
        def_fb: &InFunctionClauseBody<&FunctionDef>,
        name: &str,
    ) -> Option<bool> {
        let body = def_fb.body();
        match &self {
            Expr::List { exprs, .. } => {
                let mut literals_only = true;
                let res = exprs.iter().any(|expr| match &body[*expr] {
                    Expr::Literal(Literal::Atom(_atom)) => {
                        if let Some(atom_name) = def_fb.as_atom_name(expr) {
                            atom_name == name
                        } else {
                            false
                        }
                    }
                    Expr::Literal(_literal) => false,
                    _ => {
                        literals_only = false;
                        false
                    }
                });
                if literals_only { Some(res) } else { None }
            }
            _ => None,
        }
    }

    pub fn variant_str(&self) -> &'static str {
        match &self {
            Expr::Missing => "Expr::Missing",
            Expr::Literal(_) => "Expr::Literal",
            Expr::Var(_) => COMMON_VAR_VARIANT_STR,
            Expr::Match { .. } => "Expr::Match",
            Expr::Tuple { .. } => "Expr::Tuple",
            Expr::List { .. } => "Expr::List",
            Expr::Binary { .. } => "Expr::Binary",
            Expr::UnaryOp { .. } => "Expr::UnaryOp",
            Expr::BinaryOp { .. } => "Expr::BinaryOp",
            Expr::Record { .. } => "Expr::Record",
            Expr::RecordUpdate { .. } => "Expr::RecordUpdate",
            Expr::RecordIndex { .. } => "Expr::RecordIndex",
            Expr::RecordField { .. } => "Expr::RecordField",
            Expr::Map { .. } => "Expr::Map",
            Expr::MapUpdate { .. } => "Expr::MapUpdate",
            Expr::Catch { .. } => "Expr::Catch",
            Expr::MacroCall { .. } => "Expr::MacroCall",
            Expr::Call { .. } => "Expr::Call",
            Expr::Comprehension { .. } => "Expr::Comprehension",
            Expr::Block { .. } => "Expr::Block",
            Expr::If { .. } => "Expr::If",
            Expr::Case { .. } => "Expr::Case",
            Expr::Receive { .. } => "Expr::Receive",
            Expr::Try { .. } => "Expr::Try",
            Expr::CaptureFun { .. } => "Expr::CaptureFun",
            Expr::Closure { .. } => "Expr::Closure",
            Expr::Maybe { .. } => "Expr::Maybe",
            Expr::Paren { .. } => "Expr::Paren",
            Expr::SsrPlaceholder(_) => "Expr::SsrPlaceholder",
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MaybeExpr {
    Cond { lhs: PatId, rhs: ExprId },
    Expr(ExprId),
}

pub type ClauseId = Idx<Arc<FunctionClauseBody>>;

/// There should be a 1:1 mapping between the clause number as
/// extracted from the low-level `ast` and the HIR one. Under some
/// circumstances it is invalid, so tag it so we can de-reference to
/// an Option<Clause>.
/// The invalid cases can be generated by having a macro that expands
/// into multiple clauses.
pub struct AstClauseId {
    pub(crate) clause_id: ClauseId,
}

impl AstClauseId {
    pub(crate) fn new(clause_id: ClauseId) -> AstClauseId {
        AstClauseId { clause_id }
    }
}

pub type Guards = Vec<Vec<ExprId>>;

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Clause {
    pub pats: Vec<PatId>,
    pub guards: Guards,
    pub exprs: Vec<ExprId>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CRClause {
    pub pat: PatId,
    pub guards: Guards,
    pub exprs: Vec<ExprId>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IfClause {
    pub guards: Guards,
    pub exprs: Vec<ExprId>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CatchClause {
    pub class: Option<PatId>,
    pub reason: PatId,
    pub stack: Option<PatId>,
    pub guards: Guards,
    pub exprs: Vec<ExprId>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RecordFieldBody {
    pub field_id: RecordFieldId,
    pub expr: Option<ExprId>,
    pub ty: Option<TypeExprId>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ReceiveAfter {
    pub timeout: ExprId,
    pub exprs: Vec<ExprId>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CallTarget<Id> {
    Local { name: Id },
    Remote { module: Id, name: Id, parens: bool },
}

impl CallTarget<TypeExprId> {
    pub fn resolve_call(
        &self,
        arity: u32,
        sema: &Semantic,
        file_id: FileId,
        body: &Body,
    ) -> Option<TypeAliasDef> {
        sema::to_def::resolve_type_target(sema, self, Some(arity), file_id, body)
    }

    pub fn label(&self, arity: u32, sema: &Semantic, body: &Body) -> Option<SmolStr> {
        match self {
            CallTarget::Local { name } => {
                let name = sema.db.lookup_atom(body[*name].as_atom()?);
                Some(SmolStr::new(format!("{name}/{arity}")))
            }
            CallTarget::Remote { module, name, .. } => {
                let name = sema.db.lookup_atom(body[*name].as_atom()?);
                let module = sema.db.lookup_atom(body[*module].as_atom()?);
                Some(SmolStr::new(format!("{module}:{name}/{arity}",)))
            }
        }
    }
}

impl CallTarget<ExprId> {
    pub fn resolve_call(
        &self,
        arity: u32,
        sema: &Semantic,
        file_id: FileId,
        body: &Body,
    ) -> Option<FunctionDef> {
        sema::to_def::resolve_call_target(sema, self, Some(arity), file_id, body)
    }

    pub fn label(&self, arity: u32, sema: &Semantic, body: &Body) -> Option<SmolStr> {
        match self {
            CallTarget::Local { name } => {
                let name = sema.db.lookup_atom(body[*name].as_atom()?);
                Some(SmolStr::new(format!("{name}/{arity}")))
            }
            CallTarget::Remote { module, name, .. } => {
                let name = sema.db.lookup_atom(body[*name].as_atom()?);
                let module = sema.db.lookup_atom(body[*module].as_atom()?);
                Some(SmolStr::new(format!("{module}:{name}/{arity}",)))
            }
        }
    }

    pub fn label_short(&self, sema: &Semantic, body: &Body) -> Option<SmolStr> {
        match self {
            CallTarget::Local { name } => {
                let name = sema.db.lookup_atom(body[*name].as_atom()?);
                Some(SmolStr::new(format!("{name}")))
            }
            CallTarget::Remote { module, name, .. } => {
                let name = sema.db.lookup_atom(body[*name].as_atom()?);
                let module = sema.db.lookup_atom(body[*module].as_atom()?);
                Some(SmolStr::new(format!("{module}:{name}",)))
            }
        }
    }

    pub fn is_module_fun(
        &self,
        sema: &Semantic,
        in_clause: &InFunctionClauseBody<&FunctionDef>,
        module_name: crate::Name,
        fun_name: crate::Name,
    ) -> bool {
        match self {
            CallTarget::Local { name: _ } => false,
            CallTarget::Remote { module, name, .. } => {
                sema.is_atom_named(&in_clause[*module], module_name)
                    && sema.is_atom_named(&in_clause[*name], fun_name)
            }
        }
    }

    pub fn range(&self, in_clause: &InFunctionClauseBody<&FunctionDef>) -> Option<TextRange> {
        match self {
            CallTarget::Local { name } => in_clause.range_for_expr(*name),
            CallTarget::Remote { module, name, .. } => {
                let name_range = in_clause.range_for_expr(*name)?;
                if let Some(module_range) = in_clause.range_for_expr(*module) {
                    Some(module_range.cover(name_range))
                } else {
                    // We may have the erlang module, inserted while lowering
                    let module_atom = &in_clause[*module].as_atom()?;
                    if in_clause.sema.db.lookup_atom(*module_atom) == known::erlang {
                        Some(name_range)
                    } else {
                        None
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ComprehensionBuilder {
    List(ExprId),
    Binary(ExprId),
    Map(ExprId, ExprId),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ComprehensionExpr {
    BinGenerator {
        pat: PatId,
        expr: ExprId,
        strict: bool,
    },
    ListGenerator {
        pat: PatId,
        expr: ExprId,
        strict: bool,
    },
    MapGenerator {
        key: PatId,
        value: PatId,
        expr: ExprId,
        strict: bool,
    },
    Expr(ExprId),
}

pub type PatId = Idx<Pat>;

#[derive(Debug, Clone, Eq, PartialEq)]
/// A regular Erlang pattern
pub enum Pat {
    Missing,
    Literal(Literal),
    Var(Var),
    Match {
        lhs: PatId,
        rhs: PatId,
    },
    Tuple {
        pats: Vec<PatId>,
    },
    List {
        pats: Vec<PatId>,
        tail: Option<PatId>,
    },
    Binary {
        segs: Vec<BinarySeg<PatId>>,
    },
    UnaryOp {
        pat: PatId,
        op: UnaryOp,
    },
    BinaryOp {
        lhs: PatId,
        rhs: PatId,
        op: BinaryOp,
    },
    Record {
        name: Atom,
        fields: Vec<(Atom, PatId)>,
    },
    RecordIndex {
        name: Atom,
        field: Atom,
    },
    /// map keys in patterns are allowed to be a subset of expressions
    Map {
        fields: Vec<(ExprId, PatId)>,
    },
    MacroCall {
        // This constructor captures the point a macro is expanded
        // into an expression. This allows us to separately track the
        // arguments, for things like highlight related, or unused
        // function arguments.
        expansion: PatId,
        args: Vec<ExprId>,
        macro_def: Option<InFile<DefineId>>,
        macro_name: MacroCallName,
    },
    Paren {
        // This constructor allows us to analyze the usage of parens
        // when deciding on assists.
        // Much like `Pat::MacroCall`, it is normally hidden during a
        // `fold`, but can be made visible if needed.
        pat: PatId,
    },
    SsrPlaceholder(SsrPlaceholder),
}

impl Pat {
    pub fn as_var(&self) -> Option<Var> {
        match self {
            Pat::Var(var) => Some(*var),
            _ => None,
        }
    }

    pub fn variant_str(&self) -> &'static str {
        match &self {
            Pat::Missing => "Pat::Missing",
            Pat::Literal(_) => "Pat::Literal",
            Pat::Var(_) => COMMON_VAR_VARIANT_STR,
            Pat::Match { .. } => "Pat::Match",
            Pat::Tuple { .. } => "Pat::Tuple",
            Pat::List { .. } => "Pat::List",
            Pat::Binary { .. } => "Pat::Binary",
            Pat::UnaryOp { .. } => "Pat::UnaryOp",
            Pat::BinaryOp { .. } => "Pat::BinaryOp",
            Pat::Record { .. } => "Pat::Record",
            Pat::RecordIndex { .. } => "Pat::RecordIndex",
            Pat::Map { .. } => "Pat::Map",
            Pat::MacroCall { .. } => "Pat::MacroCall",
            Pat::Paren { .. } => "Pat::Paren",
            Pat::SsrPlaceholder(_) => "Pat::SsrPlaceholder",
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BinarySeg<Val> {
    pub elem: Val,
    pub size: Option<ExprId>,
    // TODO we might want to normalise this, but it's pretty complex
    // See logic in https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_bits.erl
    pub tys: Vec<Atom>,
    pub unit: Option<i128>,
}

impl<T> BinarySeg<T> {
    pub fn with_value<U>(&self, value: U) -> BinarySeg<U> {
        BinarySeg {
            elem: value,
            size: self.size,
            tys: self.tys.clone(),
            unit: self.unit,
        }
    }

    pub fn map<F: FnOnce(T) -> U, U>(self, f: F) -> BinarySeg<U> {
        BinarySeg {
            elem: f(self.elem),
            size: self.size,
            tys: self.tys,
            unit: self.unit,
        }
    }
}

pub type TermId = Idx<Term>;

#[derive(Debug, Clone, Eq, PartialEq)]
/// A limited expression translated as a constant term, e.g. in module attributes
pub enum Term {
    Missing,
    Literal(Literal),
    Binary(Vec<u8>),
    Tuple {
        exprs: Vec<TermId>,
    },
    List {
        exprs: Vec<TermId>,
        tail: Option<TermId>,
    },
    Map {
        fields: Vec<(TermId, TermId)>,
    },
    CaptureFun {
        module: Atom,
        name: Atom,
        arity: u32,
    },
    MacroCall {
        // This constructor captures the point a macro is expanded
        // into an expression. This allows us to separately track the
        // arguments, for things like highlight related, or unused
        // function arguments.
        expansion: TermId,
        args: Vec<ExprId>,
        macro_def: Option<InFile<DefineId>>,
        macro_name: MacroCallName,
    },
}

pub type TypeExprId = Idx<TypeExpr>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeExpr {
    AnnType {
        var: Var,
        ty: TypeExprId,
    },
    BinaryOp {
        lhs: TypeExprId,
        rhs: TypeExprId,
        op: BinaryOp,
    },
    Call {
        target: CallTarget<TypeExprId>,
        args: Vec<TypeExprId>,
    },
    Fun(FunType),
    List(ListType),
    Literal(Literal),
    Map {
        fields: Vec<(TypeExprId, MapOp, TypeExprId)>,
    },
    Missing,
    Union {
        types: Vec<TypeExprId>,
    },
    Range {
        lhs: TypeExprId,
        rhs: TypeExprId,
    },
    Record {
        name: Atom,
        fields: Vec<(Atom, TypeExprId)>,
    },
    Tuple {
        args: Vec<TypeExprId>,
    },
    UnaryOp {
        type_expr: TypeExprId,
        op: UnaryOp,
    },
    Var(Var),
    MacroCall {
        // This constructor captures the point a macro is expanded
        // into an expression. This allows us to separately track the
        // arguments, for things like highlight related, or unused
        // function arguments.
        expansion: TypeExprId,
        args: Vec<ExprId>,
        macro_def: Option<InFile<DefineId>>,
        macro_name: MacroCallName,
    },
    SsrPlaceholder(SsrPlaceholder),
}

impl TypeExpr {
    pub fn as_atom(&self) -> Option<Atom> {
        match self {
            TypeExpr::Literal(Literal::Atom(atom)) => Some(*atom),
            _ => None,
        }
    }

    pub fn variant_str(&self) -> &'static str {
        match &self {
            TypeExpr::AnnType { .. } => "TypeExpr::AnnType",
            TypeExpr::BinaryOp { .. } => "TypeExpr::BinaryOp",
            TypeExpr::Call { .. } => "TypeExpr::Call",
            TypeExpr::Fun(_) => "TypeExpr::Fun",
            TypeExpr::List(_) => "TypeExpr::List",
            TypeExpr::Literal(_) => "TypeExpr::Literal",
            TypeExpr::Map { .. } => "TypeExpr::Map",
            TypeExpr::Missing => "TypeExpr::Missing",
            TypeExpr::Union { .. } => "TypeExpr::Union",
            TypeExpr::Range { .. } => "TypeExpr::Range",
            TypeExpr::Record { .. } => "TypeExpr::Record",
            TypeExpr::Tuple { .. } => "TypeExpr::Tuple",
            TypeExpr::UnaryOp { .. } => "TypeExpr::UnaryOp",
            TypeExpr::Var(_) => "TypeExpr::Var",
            TypeExpr::MacroCall { .. } => "TypeExpr::MacroCall",
            TypeExpr::SsrPlaceholder(_) => "TypeExpr::SsrPlaceholder",
        }
    }
}

impl Term {
    pub fn variant_str(&self) -> &'static str {
        match &self {
            Term::Missing => "Term::Missing",
            Term::Literal(_) => "Term::Literal",
            Term::Binary(_) => "Term::Binary",
            Term::Tuple { .. } => "Term::Tuple",
            Term::List { .. } => "Term::List",
            Term::Map { .. } => "Term::Map",
            Term::CaptureFun { .. } => "Term::CaptureFun",
            Term::MacroCall { .. } => "Term::MacroCall",
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FunType {
    Any,
    AnyArgs {
        result: TypeExprId,
    },
    Full {
        params: Vec<TypeExprId>,
        result: TypeExprId,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ListType {
    Empty,
    Regular(TypeExprId),
    NonEmpty(TypeExprId),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SpecSig {
    pub args: Vec<TypeExprId>,
    pub result: TypeExprId,
    pub guards: Vec<(Var, TypeExprId)>,
}

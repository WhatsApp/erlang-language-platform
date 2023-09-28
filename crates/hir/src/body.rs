/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::Index;
use std::sync::Arc;

use elp_base_db::FileId;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::AstPtr;
use elp_syntax::TextRange;
use fxhash::FxHashMap;
use la_arena::Arena;
use la_arena::ArenaMap;
use la_arena::RawIdx;

use crate::db::MinDefDatabase;
use crate::db::MinInternDatabase;
use crate::expr::AstClauseId;
use crate::expr::ClauseId;
use crate::fold::AnyCallBack;
use crate::AnyExprId;
use crate::AnyExprRef;
use crate::Attribute;
use crate::AttributeId;
use crate::Callback;
use crate::CallbackId;
use crate::Clause;
use crate::CompileOption;
use crate::CompileOptionId;
use crate::DefineId;
use crate::Expr;
use crate::ExprId;
use crate::FoldCtx;
use crate::FormIdx;
use crate::FormList;
use crate::Function;
use crate::FunctionId;
use crate::InFile;
use crate::Pat;
use crate::PatId;
use crate::RecordFieldBody;
use crate::RecordId;
use crate::ResolvedMacro;
use crate::Spec;
use crate::SpecId;
use crate::SpecSig;
use crate::Strategy;
use crate::Term;
use crate::TermId;
use crate::TypeAlias;
use crate::TypeAliasId;
use crate::TypeExpr;
use crate::TypeExprId;
use crate::Var;

mod lower;
mod pretty;
pub mod scope;

#[cfg(test)]
mod tests;
mod tree_print;

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Body {
    pub exprs: Arena<Expr>,
    pub pats: Arena<Pat>,
    pub type_exprs: Arena<TypeExpr>,
    pub terms: Arena<Term>,
}

/// A wrapper around `Body` that indexes the macro expansion points
#[derive(Debug, PartialEq, Eq)]
pub struct UnexpandedIndex<'a>(pub &'a Body);

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionBody {
    pub function_id: InFile<FunctionId>,
    pub body: Arc<Body>,
    pub clauses: Arena<Clause>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeBody {
    pub body: Arc<Body>,
    pub vars: Vec<Var>,
    pub ty: TypeExprId,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SpecBody {
    pub body: Arc<Body>,
    pub sigs: Vec<SpecSig>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct RecordBody {
    pub body: Arc<Body>,
    pub fields: Vec<RecordFieldBody>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AttributeBody {
    pub body: Arc<Body>,
    pub value: TermId,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DefineBody {
    pub body: Arc<Body>,
    pub expr: ExprId,
}

impl Body {
    fn shrink_to_fit(&mut self) {
        // Exhaustive match to require handling new fields.
        let Body {
            exprs,
            pats,
            type_exprs,
            terms,
        } = self;
        exprs.shrink_to_fit();
        pats.shrink_to_fit();
        type_exprs.shrink_to_fit();
        terms.shrink_to_fit();
    }

    pub fn print_any_expr(&self, db: &dyn MinInternDatabase, expr: AnyExprId) -> String {
        match expr {
            AnyExprId::Expr(expr_id) => pretty::print_expr(db, self, expr_id),
            AnyExprId::Pat(pat_id) => pretty::print_pat(db, self, pat_id),
            AnyExprId::TypeExpr(type_id) => pretty::print_type(db, self, type_id),
            AnyExprId::Term(term_id) => pretty::print_term(db, self, term_id),
        }
    }

    pub fn tree_print_any_expr(&self, db: &dyn MinInternDatabase, expr: AnyExprId) -> String {
        match expr {
            AnyExprId::Expr(expr_id) => tree_print::print_expr(db, self, expr_id),
            AnyExprId::Pat(pat_id) => tree_print::print_pat(db, self, pat_id),
            AnyExprId::TypeExpr(type_id) => tree_print::print_type(db, self, type_id),
            AnyExprId::Term(term_id) => tree_print::print_term(db, self, term_id),
        }
    }

    pub fn get_any(&self, id: AnyExprId) -> AnyExprRef<'_> {
        match id {
            AnyExprId::Expr(expr_id) => AnyExprRef::Expr(&self[expr_id]),
            AnyExprId::Pat(pat_id) => AnyExprRef::Pat(&self[pat_id]),
            AnyExprId::TypeExpr(type_id) => AnyExprRef::TypeExpr(&self[type_id]),
            AnyExprId::Term(term_id) => AnyExprRef::Term(&self[term_id]),
        }
    }

    pub fn expr_id(&self, expr: &Expr) -> Option<ExprId> {
        self.exprs
            .iter()
            .find_map(|(k, v)| if v == expr { Some(k) } else { None })
    }

    pub fn fold_expr<'a, T>(
        &self,
        strategy: Strategy,
        form_id: FormIdx,
        expr_id: ExprId,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        FoldCtx::fold_expr(self, strategy, form_id, expr_id, initial, callback)
    }

    pub fn fold_pat<'a, T>(
        &self,
        strategy: Strategy,
        form_id: FormIdx,
        pat_id: PatId,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        FoldCtx::fold_pat(self, strategy, form_id, pat_id, initial, callback)
    }
}

impl FunctionBody {
    pub(crate) fn function_body_with_source_query(
        db: &dyn MinDefDatabase,
        function_id: InFile<FunctionId>,
    ) -> (Arc<FunctionBody>, Arc<BodySourceMap>) {
        let form_list = db.file_form_list(function_id.file_id);
        let function = &form_list[function_id.value];
        let function_ast = function.form_id.get(&function_id.file_syntax(db.upcast()));

        let mut ctx = lower::Ctx::new(db, function_id.file_id);
        ctx.set_function_info(&function.name);
        let (body, source_map) = ctx.lower_function(function_id, &function_ast);
        (Arc::new(body), Arc::new(source_map))
    }

    pub fn form_id(&self) -> FormIdx {
        FormIdx::Function(self.function_id.value)
    }

    pub fn print(&self, db: &dyn MinInternDatabase, form: &Function) -> String {
        pretty::print_function(db, self, form)
    }

    pub fn tree_print(&self, db: &dyn MinInternDatabase) -> String {
        tree_print::print_function(db, self)
    }

    pub fn valid_clause_id(&self, ast_clause_id: AstClauseId) -> Option<ClauseId> {
        if ast_clause_id.clause_id.into_raw() < RawIdx::from(self.clauses.len() as u32) {
            Some(ast_clause_id.clause_id)
        } else {
            None
        }
    }
}

impl TypeBody {
    pub(crate) fn type_body_with_source_query(
        db: &dyn MinDefDatabase,
        type_alias_id: InFile<TypeAliasId>,
    ) -> (Arc<TypeBody>, Arc<BodySourceMap>) {
        let form_list = db.file_form_list(type_alias_id.file_id);
        let ctx = lower::Ctx::new(db, type_alias_id.file_id);
        let source = type_alias_id.file_syntax(db.upcast());
        let (body, source_map) = match form_list[type_alias_id.value] {
            TypeAlias::Regular { form_id, .. } => ctx.lower_type_alias(&form_id.get(&source)),
            TypeAlias::Opaque { form_id, .. } => ctx.lower_opaque_type_alias(&form_id.get(&source)),
        };
        (Arc::new(body), Arc::new(source_map))
    }

    pub fn print(&self, db: &dyn MinInternDatabase, form: &TypeAlias) -> String {
        pretty::print_type_alias(db, self, form)
    }

    pub fn tree_print(&self, db: &dyn MinInternDatabase, form: &TypeAlias) -> String {
        tree_print::print_type_alias(db, self, form)
    }
}

impl DefineBody {
    pub(crate) fn define_body_with_source_query(
        db: &dyn MinDefDatabase,
        define_id: InFile<DefineId>,
    ) -> Option<(Arc<DefineBody>, Arc<BodySourceMap>)> {
        let form_list = db.file_form_list(define_id.file_id);
        let source = define_id.file_syntax(db.upcast());
        let define = &form_list[define_id.value];
        let define_ast = define.form_id.get(&source);
        let (body, source_map) =
            lower::Ctx::new(db, define_id.file_id).lower_define(&define_ast)?;
        Some((Arc::new(body), Arc::new(source_map)))
    }
}

pub enum SpecOrCallback {
    Spec(Spec),
    Callback(Callback),
}

impl SpecBody {
    pub(crate) fn spec_body_with_source_query(
        db: &dyn MinDefDatabase,
        spec_id: InFile<SpecId>,
    ) -> (Arc<SpecBody>, Arc<BodySourceMap>) {
        let form_list = db.file_form_list(spec_id.file_id);
        let spec_ast = form_list[spec_id.value]
            .form_id
            .get(&spec_id.file_syntax(db.upcast()));

        let (body, source_map) = lower::Ctx::new(db, spec_id.file_id).lower_spec(&spec_ast);
        (Arc::new(body), Arc::new(source_map))
    }

    pub(crate) fn callback_body_with_source_query(
        db: &dyn MinDefDatabase,
        callback_id: InFile<CallbackId>,
    ) -> (Arc<SpecBody>, Arc<BodySourceMap>) {
        let form_list = db.file_form_list(callback_id.file_id);
        let callback_ast = form_list[callback_id.value]
            .form_id
            .get(&callback_id.file_syntax(db.upcast()));

        let (body, source_map) =
            lower::Ctx::new(db, callback_id.file_id).lower_callback(&callback_ast);
        (Arc::new(body), Arc::new(source_map))
    }

    pub fn print(&self, db: &dyn MinInternDatabase, form: SpecOrCallback) -> String {
        pretty::print_spec(db, self, form)
    }
}

impl RecordBody {
    pub(crate) fn record_body_with_source_query(
        db: &dyn MinDefDatabase,
        record_id: InFile<RecordId>,
    ) -> (Arc<RecordBody>, Arc<BodySourceMap>) {
        let form_list = db.file_form_list(record_id.file_id);
        let record = &form_list[record_id.value];
        let record_ast = record.form_id.get(&record_id.file_syntax(db.upcast()));

        let (body, source_map) =
            lower::Ctx::new(db, record_id.file_id).lower_record(record, &record_ast);
        (Arc::new(body), Arc::new(source_map))
    }

    pub fn print(
        &self,
        db: &dyn MinInternDatabase,
        form_list: &FormList,
        record_id: RecordId,
    ) -> String {
        let form = &form_list[record_id];
        pretty::print_record(db, self, form, form_list)
    }
}

pub enum AnyAttribute {
    CompileOption(CompileOption),
    Attribute(Attribute),
}

impl AttributeBody {
    pub(crate) fn attribute_body_with_source_query(
        db: &dyn MinDefDatabase,
        attribute_id: InFile<AttributeId>,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>) {
        let form_list = db.file_form_list(attribute_id.file_id);
        let attribute_ast = form_list[attribute_id.value]
            .form_id
            .get(&attribute_id.file_syntax(db.upcast()));

        let (body, source_map) =
            lower::Ctx::new(db, attribute_id.file_id).lower_attribute(&attribute_ast);
        (Arc::new(body), Arc::new(source_map))
    }

    pub(crate) fn compile_body_with_source_query(
        db: &dyn MinDefDatabase,
        attribute_id: InFile<CompileOptionId>,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>) {
        let form_list = db.file_form_list(attribute_id.file_id);
        let attribute_ast = form_list[attribute_id.value]
            .form_id
            .get(&attribute_id.file_syntax(db.upcast()));

        let (body, source_map) =
            lower::Ctx::new(db, attribute_id.file_id).lower_compile(&attribute_ast);
        (Arc::new(body), Arc::new(source_map))
    }

    pub fn print(&self, db: &dyn MinInternDatabase, form: AnyAttribute) -> String {
        pretty::print_attribute(db, self, &form)
    }

    pub fn tree_print(&self, db: &dyn MinInternDatabase, form: AnyAttribute) -> String {
        tree_print::print_attribute(db, self, &form)
    }
}

impl Index<ClauseId> for FunctionBody {
    type Output = Clause;

    fn index(&self, index: ClauseId) -> &Self::Output {
        &self.clauses[index]
    }
}

impl<'a> Index<ExprId> for UnexpandedIndex<'a> {
    type Output = Expr;

    fn index(&self, index: ExprId) -> &Self::Output {
        // Do not "look through" macro expansion
        &self.0.exprs[index]
    }
}

impl Index<ExprId> for Body {
    type Output = Expr;

    fn index(&self, index: ExprId) -> &Self::Output {
        // "look through" macro expansion.
        match &self.exprs[index] {
            Expr::MacroCall { expansion, args: _ } => &self.exprs[*expansion],
            expr => expr,
        }
    }
}

impl<'a> Index<PatId> for UnexpandedIndex<'a> {
    type Output = Pat;

    fn index(&self, index: PatId) -> &Self::Output {
        // Do not "look through" macro expansion
        &self.0.pats[index]
    }
}

impl Index<PatId> for Body {
    type Output = Pat;

    fn index(&self, index: PatId) -> &Self::Output {
        // "look through" macro expansion.
        match &self.pats[index] {
            Pat::MacroCall { expansion, args: _ } => &self.pats[*expansion],
            pat => pat,
        }
    }
}

impl<'a> Index<TypeExprId> for UnexpandedIndex<'a> {
    type Output = TypeExpr;

    fn index(&self, index: TypeExprId) -> &Self::Output {
        // Do not "look through" macro expansion
        &self.0.type_exprs[index]
    }
}

impl Index<TypeExprId> for Body {
    type Output = TypeExpr;

    fn index(&self, index: TypeExprId) -> &Self::Output {
        // "look through" macro expansion.
        match &self.type_exprs[index] {
            TypeExpr::MacroCall { expansion, args: _ } => &self.type_exprs[*expansion],
            type_expr => type_expr,
        }
    }
}

impl<'a> Index<TermId> for UnexpandedIndex<'a> {
    type Output = Term;

    fn index(&self, index: TermId) -> &Self::Output {
        // Do not "look through" macro expansion
        &self.0.terms[index]
    }
}

impl Index<TermId> for Body {
    type Output = Term;

    fn index(&self, index: TermId) -> &Self::Output {
        // "look through" macro expansion.
        match &self.terms[index] {
            Term::MacroCall { expansion, args: _ } => &self.terms[*expansion],
            term => term,
        }
    }
}

pub type ExprSource = InFileAstPtr<ast::Expr>;

pub type MacroSource = InFileAstPtr<ast::MacroCallExpr>;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct InFileAstPtr<T>(InFile<AstPtr<T>>)
where
    T: AstNode,
    InFile<AstPtr<T>>: Copy;

impl<T> Copy for InFileAstPtr<T>
where
    T: AstNode + std::clone::Clone,
    InFile<AstPtr<T>>: Copy,
{
}

impl<T: AstNode> InFileAstPtr<T> {
    pub fn new(file_id: FileId, ptr: AstPtr<T>) -> InFileAstPtr<T> {
        Self(InFile::new(file_id, ptr))
    }

    fn from_infile(in_file: InFile<&T>) -> InFileAstPtr<T> {
        InFileAstPtr::new(in_file.file_id, AstPtr::new(in_file.value))
    }

    pub fn file_id(&self) -> FileId {
        self.0.file_id
    }

    // Restrict access to the bare AstPtr. This allows us to prevent
    // calls to AstPtr::to_node with the incorrect SourceFile ast.
    // Because HIR expands macros, when walking the HIR ast you can
    // easily have an original ast item originating from a different
    // file, and it will then panic.
    pub(crate) fn value(&self) -> AstPtr<T> {
        self.0.value
    }

    pub fn to_node(&self, parse: &InFile<ast::SourceFile>) -> Option<T> {
        if self.0.file_id == parse.file_id {
            Some(self.0.value.to_node(parse.value.syntax()))
        } else {
            None
        }
    }

    pub(crate) fn range(&self) -> TextRange {
        self.0.value.syntax_node_ptr().range()
    }

    #[cfg(test)]
    pub(crate) fn syntax_ptr_string(&self) -> String {
        format!("{:?}", self.0.value.syntax_node_ptr())
    }
}

/// A form body together with the mapping from syntax nodes to HIR expression
/// IDs. This is needed to go from e.g. a position in a file to the HIR
/// expression containing it; but for static analysis, got definition, etc.,
/// we want to operate on a structure that is agnostic to the actual positions
/// of expressions in the file, so that we don't recompute results whenever some
/// whitespace is typed, or unrelated details in the file change.
///
/// One complication here is that, due to macro expansion, a single `Body` might
/// be spread across several files. So, for each ExprId and PatId, we record
/// both the FileId and the position inside the file.
#[derive(Default, Debug, Eq, PartialEq)]
pub struct BodySourceMap {
    expr_map: FxHashMap<ExprSource, ExprId>,
    expr_map_back: ArenaMap<ExprId, ExprSource>,
    pat_map: FxHashMap<ExprSource, PatId>,
    pat_map_back: ArenaMap<PatId, ExprSource>,
    type_expr_map: FxHashMap<ExprSource, TypeExprId>,
    type_expr_map_back: ArenaMap<TypeExprId, ExprSource>,
    term_map: FxHashMap<ExprSource, TermId>,
    term_map_back: ArenaMap<TermId, ExprSource>,
    macro_map: FxHashMap<MacroSource, ResolvedMacro>,
}

impl BodySourceMap {
    pub fn expr_id(&self, expr: InFile<&ast::Expr>) -> Option<ExprId> {
        self.expr_map.get(&InFileAstPtr::from_infile(expr)).copied()
    }

    pub fn expr(&self, expr_id: ExprId) -> Option<ExprSource> {
        self.expr_map_back.get(expr_id).copied()
    }

    pub fn pat_id(&self, expr: InFile<&ast::Expr>) -> Option<PatId> {
        self.pat_map.get(&InFileAstPtr::from_infile(expr)).copied()
    }

    pub fn pat(&self, pat_id: PatId) -> Option<ExprSource> {
        self.pat_map_back.get(pat_id).copied()
    }

    pub fn type_expr_id(&self, expr: InFile<&ast::Expr>) -> Option<TypeExprId> {
        self.type_expr_map
            .get(&InFileAstPtr::from_infile(expr))
            .copied()
    }

    pub fn term_id(&self, expr: InFile<&ast::Expr>) -> Option<TermId> {
        self.term_map.get(&InFileAstPtr::from_infile(expr)).copied()
    }

    pub fn any_id(&self, expr: InFile<&ast::Expr>) -> Option<AnyExprId> {
        let ptr = InFileAstPtr::from_infile(expr);
        let expr_id = self.expr_map.get(&ptr).copied().map(AnyExprId::Expr);
        expr_id
            .or_else(|| self.pat_map.get(&ptr).copied().map(AnyExprId::Pat))
            .or_else(|| {
                self.type_expr_map
                    .get(&ptr)
                    .copied()
                    .map(AnyExprId::TypeExpr)
            })
            .or_else(|| self.term_map.get(&ptr).copied().map(AnyExprId::Term))
    }

    pub fn any(&self, id: AnyExprId) -> Option<ExprSource> {
        match id {
            AnyExprId::Expr(expr_id) => self.expr_map_back.get(expr_id).copied(),
            AnyExprId::Pat(pat_id) => self.pat_map_back.get(pat_id).copied(),
            AnyExprId::TypeExpr(_) => None,
            AnyExprId::Term(_) => None,
        }
    }

    pub fn resolved_macro(&self, call: InFile<&ast::MacroCallExpr>) -> Option<ResolvedMacro> {
        self.macro_map
            .get(&InFileAstPtr::from_infile(call))
            .copied()
    }
}

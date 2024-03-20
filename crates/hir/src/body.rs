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

use self::lower::MacroInformation;
use crate::db::DefDatabase;
use crate::db::InternDatabase;
use crate::def_map::FunctionDefId;
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
use crate::FunctionClause;
use crate::FunctionClauseId;
use crate::InFile;
use crate::Literal;
use crate::Name;
use crate::NameArity;
use crate::Pat;
use crate::PatId;
use crate::RecordFieldBody;
use crate::RecordId;
use crate::ResolvedMacro;
use crate::Semantic;
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

#[derive(Debug, PartialEq, Eq)]
pub struct Body {
    pub exprs: Arena<Expr>,
    pub pats: Arena<Pat>,
    pub type_exprs: Arena<TypeExpr>,
    pub terms: Arena<Term>,
    pub origin: BodyOrigin,
}

/// A wrapper around `Body` that indexes the macro expansion points
#[derive(Debug, PartialEq, Eq)]
pub struct UnexpandedIndex<'a>(pub &'a Body);

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionBody {
    pub function_id: InFile<FunctionDefId>,
    pub clause_ids: Vec<FunctionClauseId>,
    pub clauses: Arena<Arc<FunctionClauseBody>>,
    pub spec: Option<Arc<SpecBody>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionClauseBody {
    pub name: Option<NameArity>,
    pub from_macro: Option<TopLevelMacro>,
    pub body: Arc<Body>,
    pub clause: Clause,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TopLevelMacro {
    pub args: Vec<ExprId>,
    pub macro_def: InFile<DefineId>,
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BodyOrigin {
    Invalid(FileId),
    FormIdx {
        file_id: FileId,
        form_id: FormIdx,
    },
    Define {
        file_id: FileId,
        define_id: DefineId,
    },
}

impl BodyOrigin {
    pub fn new(file_id: FileId, form_id: FormIdx) -> BodyOrigin {
        BodyOrigin::FormIdx { file_id, form_id }
    }

    pub fn file_id(&self) -> FileId {
        match self {
            BodyOrigin::Invalid(file_id) => *file_id,
            BodyOrigin::FormIdx {
                file_id,
                form_id: _,
            } => *file_id,
            BodyOrigin::Define {
                file_id,
                define_id: _,
            } => *file_id,
        }
    }

    pub fn is_valid(&self) -> bool {
        match self {
            BodyOrigin::FormIdx { .. } => true,
            BodyOrigin::Define { .. } => true,
            BodyOrigin::Invalid(_) => false,
        }
    }
}

impl Body {
    fn new(origin: BodyOrigin) -> Body {
        Body {
            exprs: Arena::default(),
            pats: Arena::default(),
            type_exprs: Arena::default(),
            terms: Arena::default(),
            origin,
        }
    }

    fn shrink_to_fit(&mut self) {
        // Exhaustive match to require handling new fields.
        let Body {
            exprs,
            pats,
            type_exprs,
            terms,
            origin: _,
        } = self;
        exprs.shrink_to_fit();
        pats.shrink_to_fit();
        type_exprs.shrink_to_fit();
        terms.shrink_to_fit();
    }

    pub fn get_body_map(&self, sema: &Semantic) -> Option<Arc<BodySourceMap>> {
        match self.origin {
            BodyOrigin::Invalid(_) => None,
            BodyOrigin::FormIdx { file_id, form_id } => {
                let (_, body_map) = sema.get_body_and_map(file_id, form_id)?;
                Some(body_map)
            }
            BodyOrigin::Define { file_id, define_id } => {
                let (_, body_map) = sema
                    .db
                    .define_body_with_source(InFile::new(file_id, define_id))?;
                Some(body_map)
            }
        }
    }

    pub fn print_any_expr(&self, db: &dyn InternDatabase, expr: AnyExprId) -> String {
        match expr {
            AnyExprId::Expr(expr_id) => pretty::print_expr(db, self, expr_id),
            AnyExprId::Pat(pat_id) => pretty::print_pat(db, self, pat_id),
            AnyExprId::TypeExpr(type_id) => pretty::print_type(db, self, type_id),
            AnyExprId::Term(term_id) => pretty::print_term(db, self, term_id),
        }
    }

    pub fn tree_print_any_expr(&self, db: &dyn InternDatabase, expr: AnyExprId) -> String {
        match expr {
            AnyExprId::Expr(expr_id) => tree_print::print_expr(db, self, expr_id),
            AnyExprId::Pat(pat_id) => tree_print::print_pat(db, self, pat_id),
            AnyExprId::TypeExpr(type_id) => tree_print::print_type(db, self, type_id),
            AnyExprId::Term(term_id) => tree_print::print_term(db, self, term_id),
        }
    }

    pub fn get_atom_name(&self, sema: &Semantic, name: &ExprId) -> Option<Name> {
        match self[*name] {
            Expr::Literal(Literal::Atom(atom)) => Some(sema.db.lookup_atom(atom)),
            _ => None,
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

    pub fn is_macro(&self, id: AnyExprId) -> bool {
        match id {
            AnyExprId::Expr(idx) => match &self.exprs[idx] {
                Expr::MacroCall { .. } => true,
                _ => false,
            },
            AnyExprId::Pat(idx) => match &self.pats[idx] {
                Pat::MacroCall { .. } => true,
                _ => false,
            },
            AnyExprId::TypeExpr(idx) => match &self.type_exprs[idx] {
                TypeExpr::MacroCall { .. } => true,
                _ => false,
            },
            AnyExprId::Term(idx) => match &self.terms[idx] {
                Term::MacroCall { .. } => true,
                _ => false,
            },
        }
    }

    pub fn fold_expr<'a, T>(
        &self,
        strategy: Strategy,
        expr_id: ExprId,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        FoldCtx::fold_expr(strategy, self, expr_id, initial, callback)
    }

    pub fn fold_pat<'a, T>(
        &self,
        strategy: Strategy,
        pat_id: PatId,
        initial: T,
        callback: AnyCallBack<'a, T>,
    ) -> T {
        FoldCtx::fold_pat(strategy, self, pat_id, initial, callback)
    }

    // -----------------------------------------------------------------
    // HIR API. Perhaps move to its own struct some time.

    /// Given an HIR `AnyExprId` for a `Map` literal possibly
    /// containing maps as key values, look up the series of keys in
    /// the `path`.
    pub fn lookup_map_path(
        &self,
        db: &dyn InternDatabase,
        map_id: AnyExprId,
        path: &[String],
    ) -> Option<AnyExprId> {
        let key = path.get(0)?;
        match map_id {
            AnyExprId::Expr(id) => {
                match &self[id] {
                    Expr::Map { fields } => fields.iter().find_map(|(key_id, val_id)| match &self
                        [*key_id]
                    {
                        Expr::Literal(Literal::Atom(atom)) => {
                            if &atom.as_string(db) == key {
                                if path.len() == 1 {
                                    Some(AnyExprId::Expr(*val_id))
                                } else {
                                    self.lookup_map_path(db, AnyExprId::Expr(*val_id), &path[1..])
                                }
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }),
                    _ => None,
                }
            }
            AnyExprId::Pat(id) => match &self[id] {
                Pat::Map { fields } => {
                    fields
                        .iter()
                        .find_map(|(key_id, val_id)| match &self[*key_id] {
                            Expr::Literal(Literal::Atom(atom)) => {
                                if &atom.as_string(db) == key {
                                    if path.len() == 1 {
                                        Some(AnyExprId::Pat(*val_id))
                                    } else {
                                        self.lookup_map_path(
                                            db,
                                            AnyExprId::Pat(*val_id),
                                            &path[1..],
                                        )
                                    }
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        })
                }
                _ => None,
            },
            AnyExprId::TypeExpr(id) => match &self[id] {
                TypeExpr::Map { fields } => {
                    fields
                        .iter()
                        .find_map(|(key_id, _op, val_id)| match &self[*key_id] {
                            TypeExpr::Literal(Literal::Atom(atom)) => {
                                if &atom.as_string(db) == key {
                                    if path.len() == 1 {
                                        Some(AnyExprId::TypeExpr(*val_id))
                                    } else {
                                        self.lookup_map_path(
                                            db,
                                            AnyExprId::TypeExpr(*val_id),
                                            &path[1..],
                                        )
                                    }
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        })
                }
                _ => None,
            },
            AnyExprId::Term(id) => {
                match &self[id] {
                    Term::Map { fields } => fields.iter().find_map(|(key_id, val_id)| match &self
                        [*key_id]
                    {
                        Term::Literal(Literal::Atom(atom)) => {
                            if &atom.as_string(db) == key {
                                if path.len() == 1 {
                                    Some(AnyExprId::Term(*val_id))
                                } else {
                                    self.lookup_map_path(db, AnyExprId::Term(*val_id), &path[1..])
                                }
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }),
                    _ => None,
                }
            }
        }
    }
}

impl FunctionBody {
    pub(crate) fn function_body_with_source_query(
        db: &dyn DefDatabase,
        function_id: InFile<FunctionDefId>,
    ) -> (Arc<FunctionBody>, Vec<Arc<BodySourceMap>>) {
        let def_map = db.def_map(function_id.file_id);
        if let Some(fun_def) = def_map.get_by_function_id(&function_id) {
            let fun_asts = fun_def.source(db.upcast());

            let mut ctx = lower::Ctx::new(db, BodyOrigin::Invalid(function_id.file_id));
            let name = &fun_def.function_clauses[0].name;
            ctx.set_function_info(name);
            let (mut body, source_maps) =
                ctx.lower_function(function_id, fun_def.function_clause_ids.clone(), &fun_asts);
            if let Some(spec) = &fun_def.spec {
                let spec = db.spec_body(InFile::new(spec.file.file_id, spec.spec_id));
                body.spec = Some(spec);
            }
            (Arc::new(body), source_maps)
        } else {
            (
                Arc::new(FunctionBody {
                    function_id,
                    clause_ids: vec![],
                    clauses: Arena::default(),
                    spec: None,
                }),
                vec![],
            )
        }
    }

    pub fn form_id(&self, clause_id: ClauseId) -> Option<FormIdx> {
        let n: u32 = clause_id.into_raw().into();
        let function_id = self.clause_ids.get(n as usize)?;
        Some(FormIdx::FunctionClause(*function_id))
    }

    pub fn spec_body(&self) -> Option<Arc<SpecBody>> {
        match &self.spec {
            Some(spec) => Some(spec.clone()),
            None => None,
        }
    }

    pub fn print(&self, db: &dyn InternDatabase, form: &FunctionClause) -> String {
        pretty::print_function_clause(db, self, form)
    }

    pub fn tree_print(&self, db: &dyn InternDatabase) -> String {
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

impl FunctionClauseBody {
    pub(crate) fn function_clause_body_with_source_query(
        db: &dyn DefDatabase,
        function_clause_id: InFile<FunctionClauseId>,
    ) -> (Arc<FunctionClauseBody>, Arc<BodySourceMap>) {
        fn empty(origin: BodyOrigin) -> (Arc<FunctionClauseBody>, Arc<BodySourceMap>) {
            (
                Arc::new(FunctionClauseBody {
                    name: None,
                    from_macro: None,
                    body: Arc::new(Body::new(origin)),
                    clause: Clause::default(),
                }),
                Arc::new(BodySourceMap::default()),
            )
        }

        let form_list = db.file_form_list(function_clause_id.file_id);
        let function = &form_list[function_clause_id.value];
        let function_ast = function
            .form_id
            .get(&function_clause_id.file_syntax(db.upcast()));
        let body_origin = BodyOrigin::new(
            function_clause_id.file_id,
            FormIdx::FunctionClause(function_clause_id.value),
        );
        if let Some(clause_ast) = function_ast.clause() {
            let mut ctx = lower::Ctx::new(db, body_origin);
            ctx.set_function_info(&function.name);
            if let Some((body, source_map)) = ctx
                .lower_clause_or_macro_body(clause_ast, &function_clause_id, None)
                .next()
            {
                (Arc::new(body), Arc::new(source_map))
            } else {
                empty(body_origin)
            }
        } else {
            empty(body_origin)
        }
    }

    pub(crate) fn lower_clause_body(
        db: &dyn DefDatabase,
        clause_ast: &ast::FunctionClause,
        clause_id: &InFile<FunctionClauseId>,
        macrostack: MacroInformation,
        macro_def: Option<(InFile<DefineId>, Vec<ast::MacroExpr>)>,
    ) -> (FunctionClauseBody, BodySourceMap) {
        let mut ctx = lower::Ctx::new(
            db,
            BodyOrigin::new(clause_id.file_id, FormIdx::FunctionClause(clause_id.value)),
        );
        ctx.set_function_info_from_ast(clause_ast);
        ctx.set_macro_information(macrostack);
        let from_macro =
            macro_def.map(|(macro_def, args)| ctx.lower_top_level_macro(args, macro_def));
        let (body, source_map) = ctx.lower_function_clause(&clause_ast, from_macro);
        (body, source_map)
    }

    pub fn tree_print(&self, db: &dyn InternDatabase) -> String {
        tree_print::print_function_clause(db, self)
    }
}

impl TypeBody {
    pub(crate) fn type_body_with_source_query(
        db: &dyn DefDatabase,
        type_alias_id: InFile<TypeAliasId>,
    ) -> (Arc<TypeBody>, Arc<BodySourceMap>) {
        let form_list = db.file_form_list(type_alias_id.file_id);
        let ctx = lower::Ctx::new(
            db,
            BodyOrigin::new(
                type_alias_id.file_id,
                FormIdx::TypeAlias(type_alias_id.value),
            ),
        );
        let source = type_alias_id.file_syntax(db.upcast());
        let (body, source_map) = match form_list[type_alias_id.value] {
            TypeAlias::Regular { form_id, .. } => ctx.lower_type_alias(&form_id.get(&source)),
            TypeAlias::Opaque { form_id, .. } => ctx.lower_opaque_type_alias(&form_id.get(&source)),
        };
        (Arc::new(body), Arc::new(source_map))
    }

    pub fn print(&self, db: &dyn InternDatabase, form: &TypeAlias) -> String {
        pretty::print_type_alias(db, self, form)
    }

    pub fn tree_print(&self, db: &dyn InternDatabase, form: &TypeAlias) -> String {
        tree_print::print_type_alias(db, self, form)
    }
}

impl DefineBody {
    pub(crate) fn define_body_with_source_query(
        db: &dyn DefDatabase,
        define_id: InFile<DefineId>,
    ) -> Option<(Arc<DefineBody>, Arc<BodySourceMap>)> {
        let form_list = db.file_form_list(define_id.file_id);
        let source = define_id.file_syntax(db.upcast());
        let define = &form_list[define_id.value];
        let define_ast = define.form_id.get(&source);
        let (body, source_map) = lower::Ctx::new(
            db,
            BodyOrigin::Define {
                file_id: define_id.file_id,
                define_id: define_id.value,
            },
        )
        .lower_define(&define_ast)?;
        Some((Arc::new(body), Arc::new(source_map)))
    }
}

pub enum SpecOrCallback {
    Spec(Spec),
    Callback(Callback),
}

impl SpecBody {
    pub(crate) fn spec_body_with_source_query(
        db: &dyn DefDatabase,
        spec_id: InFile<SpecId>,
    ) -> (Arc<SpecBody>, Arc<BodySourceMap>) {
        let form_list = db.file_form_list(spec_id.file_id);
        let spec_ast = form_list[spec_id.value]
            .form_id
            .get(&spec_id.file_syntax(db.upcast()));

        let (body, source_map) = lower::Ctx::new(
            db,
            BodyOrigin::new(spec_id.file_id, FormIdx::Spec(spec_id.value)),
        )
        .lower_spec(&spec_ast);
        (Arc::new(body), Arc::new(source_map))
    }

    pub(crate) fn callback_body_with_source_query(
        db: &dyn DefDatabase,
        callback_id: InFile<CallbackId>,
    ) -> (Arc<SpecBody>, Arc<BodySourceMap>) {
        let form_list = db.file_form_list(callback_id.file_id);
        let callback_ast = form_list[callback_id.value]
            .form_id
            .get(&callback_id.file_syntax(db.upcast()));

        let (body, source_map) = lower::Ctx::new(
            db,
            BodyOrigin::new(callback_id.file_id, FormIdx::Callback(callback_id.value)),
        )
        .lower_callback(&callback_ast);
        (Arc::new(body), Arc::new(source_map))
    }

    pub fn spec_id(&self) -> Option<SpecId> {
        if let BodyOrigin::FormIdx {
            file_id: _,
            form_id: FormIdx::Spec(spec_id),
        } = self.body.origin
        {
            Some(spec_id)
        } else {
            None
        }
    }

    pub fn print(&self, db: &dyn InternDatabase, form: SpecOrCallback) -> String {
        pretty::print_spec(db, self, form)
    }
}

impl RecordBody {
    pub(crate) fn record_body_with_source_query(
        db: &dyn DefDatabase,
        record_id: InFile<RecordId>,
    ) -> (Arc<RecordBody>, Arc<BodySourceMap>) {
        let form_list = db.file_form_list(record_id.file_id);
        let record = &form_list[record_id.value];
        let record_ast = record.form_id.get(&record_id.file_syntax(db.upcast()));

        let (body, source_map) = lower::Ctx::new(
            db,
            BodyOrigin::new(record_id.file_id, FormIdx::Record(record_id.value)),
        )
        .lower_record(record, &record_ast);
        (Arc::new(body), Arc::new(source_map))
    }

    pub fn print(
        &self,
        db: &dyn InternDatabase,
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
        db: &dyn DefDatabase,
        attribute_id: InFile<AttributeId>,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>) {
        let form_list = db.file_form_list(attribute_id.file_id);
        let attribute_ast = form_list[attribute_id.value]
            .form_id
            .get(&attribute_id.file_syntax(db.upcast()));

        let (body, source_map) = lower::Ctx::new(
            db,
            BodyOrigin::new(attribute_id.file_id, FormIdx::Attribute(attribute_id.value)),
        )
        .lower_attribute(&attribute_ast);
        (Arc::new(body), Arc::new(source_map))
    }

    pub(crate) fn compile_body_with_source_query(
        db: &dyn DefDatabase,
        attribute_id: InFile<CompileOptionId>,
    ) -> (Arc<AttributeBody>, Arc<BodySourceMap>) {
        let form_list = db.file_form_list(attribute_id.file_id);
        let attribute_ast = form_list[attribute_id.value]
            .form_id
            .get(&attribute_id.file_syntax(db.upcast()));

        let (body, source_map) = lower::Ctx::new(
            db,
            BodyOrigin::new(
                attribute_id.file_id,
                FormIdx::CompileOption(attribute_id.value),
            ),
        )
        .lower_compile(&attribute_ast);
        (Arc::new(body), Arc::new(source_map))
    }

    pub fn print(&self, db: &dyn InternDatabase, form: AnyAttribute) -> String {
        pretty::print_attribute(db, self, &form)
    }

    pub fn tree_print(&self, db: &dyn InternDatabase, form: AnyAttribute) -> String {
        tree_print::print_attribute(db, self, &form)
    }
}

impl Index<ClauseId> for FunctionBody {
    type Output = Arc<FunctionClauseBody>;

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
            Expr::MacroCall {
                expansion,
                args: _,
                macro_def: _,
            } => &self.exprs[*expansion],
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
            Pat::MacroCall {
                expansion,
                args: _,
                macro_def: _,
            } => &self.pats[*expansion],
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
            TypeExpr::MacroCall {
                expansion,
                args: _,
                macro_def: _,
            } => &self.type_exprs[*expansion],
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
            Term::MacroCall {
                expansion,
                args: _,
                macro_def: _,
            } => &self.terms[*expansion],
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

    pub fn range(&self) -> TextRange {
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

    pub fn type_expr(&self, type_expr_id: TypeExprId) -> Option<ExprSource> {
        self.type_expr_map_back.get(type_expr_id).copied()
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
            AnyExprId::TypeExpr(type_id) => self.type_expr_map_back.get(type_id).copied(),
            AnyExprId::Term(term_id) => self.term_map_back.get(term_id).copied(),
        }
    }

    pub fn resolved_macro(&self, call: InFile<&ast::MacroCallExpr>) -> Option<ResolvedMacro> {
        self.macro_map
            .get(&InFileAstPtr::from_infile(call))
            .copied()
    }
}

#[cfg(test)]
mod local_tests {
    use elp_base_db::fixture::WithFixture;
    use elp_base_db::SourceDatabase;
    use elp_syntax::algo::find_node_at_offset;
    use elp_syntax::ast;
    use elp_syntax::AstNode;

    use crate::test_db::TestDB;
    use crate::AnyExprId;
    use crate::InFile;
    use crate::Semantic;

    fn check_is_macro_expr(fixture: &str) {
        let (db, position, _) = TestDB::with_position(fixture);
        let sema = Semantic::new(&db);

        let file_syntax = db.parse(position.file_id).syntax_node();
        let expr: ast::Expr = find_node_at_offset(&file_syntax, position.offset).unwrap();
        let macro_call = sema.to_expr(InFile::new(position.file_id, &expr)).unwrap();
        assert!(
            macro_call
                .body()
                .is_macro(AnyExprId::Expr(macro_call.value))
        );
    }

    fn check_is_macro_pat(fixture: &str) {
        let (db, position, _) = TestDB::with_position(fixture);
        let sema = Semantic::new(&db);

        let file_syntax = db.parse(position.file_id).syntax_node();
        let pat: ast::Expr = find_node_at_offset(&file_syntax, position.offset).unwrap();
        let macro_call = sema.to_pat(InFile::new(position.file_id, &pat)).unwrap();
        assert!(macro_call.body().is_macro(AnyExprId::Pat(macro_call.value)));
    }

    #[test]
    fn in_macro_expr() {
        check_is_macro_expr(
            r#"
            -module(main).
            -define(SOME_CONST, 1).

            foo() -> ~?SOME_CONST * 1024.
            "#,
        )
    }

    #[test]
    fn in_macro_pat() {
        check_is_macro_pat(
            r#"
            -module(main).
            -define(SOME_CONST, 1).

            foo(~?SOME_CONST) -> 1024.
            "#,
        )
    }

    #[track_caller]
    fn check_map_path_expr(path: &[&str], valid: bool, fixture: &str) {
        let (db, position, _) = TestDB::with_position(fixture);
        let sema = Semantic::new(&db);

        let file_syntax = db.parse(position.file_id).syntax_node();
        let val: ast::Expr = find_node_at_offset(&file_syntax, position.offset).unwrap();
        let fun: ast::FunDecl = find_node_at_offset(&file_syntax, position.offset).unwrap();
        let map = fun
            .syntax()
            .descendants()
            .find_map(ast::MapExpr::cast)
            .unwrap();
        let in_clause = sema
            .to_expr(InFile::new(position.file_id, &ast::Expr::MapExpr(map)))
            .unwrap();
        let path = path.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        if let Some(found) = in_clause
            .body()
            .lookup_map_path(&db, in_clause.value.into(), &path)
        {
            let astptr_found = in_clause.get_body_map().any(found).unwrap().value();
            let ast_found = astptr_found.to_node(&file_syntax);
            if valid {
                debug_assert_eq!(val, ast_found);
            } else {
                panic!("expected invalid path, found {:?}", ast_found);
            }
        } else {
            if valid {
                panic!("expected valid path, nothing found");
            } else {
                // pass
            }
        }
    }

    #[test]
    fn map_path_expr() {
        check_map_path_expr(
            &vec!["k1", "k2"],
            true,
            r#"
            -module(main).

            foo() -> #{ k1 => #{ k2 => v~al}}.
            "#,
        )
    }

    #[test]
    fn map_path_expr_not_found() {
        check_map_path_expr(
            &vec!["k1", "k3"],
            false,
            r#"
            -module(main).

            foo() -> #{ k1 => #{ k2 => v~al}}.
            "#,
        )
    }
}

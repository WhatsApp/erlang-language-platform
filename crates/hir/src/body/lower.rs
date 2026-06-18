/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::iter;
use std::iter::once;
use std::sync::Arc;

use either::Either;
use elp_base_db::FileId;
use elp_syntax::AstNode;
use elp_syntax::AstPtr;
use elp_syntax::SyntaxKind;
use elp_syntax::ast;
use elp_syntax::ast::ExprMax;
use elp_syntax::ast::HasArity;
use elp_syntax::ast::MacroCallArgs;
use elp_syntax::ast::MacroDefReplacement;
use elp_syntax::ast::MapOp;
use elp_syntax::ast::is_erlang_fun;
use elp_syntax::ast::is_erlang_type;
use elp_syntax::unescape;
use fxhash::FxHashMap;

use super::BodyOrigin;
use super::FunctionClauseBody;
use super::InFileAstPtr;
use super::SSR_SOURCE_FILE_ID;
use super::SsrBody;
use super::SsrPatternIds;
use super::TopLevelMacro;
use crate::Atom;
use crate::AttributeBody;
use crate::BasedInteger;
use crate::BinarySeg;
use crate::Body;
use crate::BodyDiagnostic;
use crate::BodySourceMap;
use crate::CRClause;
use crate::CallTarget;
use crate::CatchClause;
use crate::Clause;
use crate::ComprehensionBuilder;
use crate::ComprehensionExpr;
use crate::DefineBody;
use crate::DefineId;
use crate::Expr;
use crate::ExprId;
use crate::ExprSource;
use crate::FunType;
use crate::FunctionClauseId;
use crate::IfClause;
use crate::InFile;
use crate::ListType;
use crate::Literal;
use crate::MacroName;
use crate::MacroSource;
use crate::Name;
use crate::NameArity;
use crate::PPConditionId;
use crate::Pat;
use crate::PatId;
use crate::ReceiveAfter;
use crate::Record;
use crate::RecordBody;
use crate::RecordFieldBody;
use crate::ResolvedMacro;
use crate::SpecBody;
use crate::SpecSig;
use crate::SsrSource;
use crate::Term;
use crate::TermId;
use crate::TypeBody;
use crate::TypeExpr;
use crate::TypeExprId;
use crate::Var;
use crate::db::DefDatabase;
use crate::expr::Guards;
use crate::expr::MacroCallName;
use crate::expr::MaybeExpr;
use crate::expr::NamedAtom;
use crate::expr::NativeRecordName;
use crate::expr::StringVariant;
use crate::form_list::ConditionEnvId;
use crate::known;
use crate::macro_exp;
use crate::macro_exp::BuiltInMacro;
use crate::name::AsName;
use crate::preprocessor::compute_file_macro_defs;

#[derive(Debug, Clone)]
pub(crate) struct MacroStackEntry {
    name: MacroName,
    file_id: FileId,
    var_map: FxHashMap<Var, ast::MacroExpr>,
    parent_id: usize,
}

pub struct Ctx<'a> {
    db: &'a dyn DefDatabase,
    macro_stack: Vec<MacroStackEntry>,
    macro_stack_id: usize,
    function_info: Option<(Atom, u32)>,
    function_name: Option<NameArity>, // Equivalent to function_info, cached
    // OTP epp.erl compat: arity as epp would compute it (may differ from
    // actual arity due to the update_fun_name_1 bracket-only bug).
    // https://github.com/erlang/otp/issues/10705
    // Used exclusively for ?FUNCTION_ARITY expansion.
    epp_function_arity: Option<u32>,
    body: Body,
    module_expr_ids: FxHashMap<Name, ExprId>, // Lowering of module `Name`s as literals
    erlang_type_expr_id: Option<TypeExprId>,
    source_map: BodySourceMap,
    // For sanity checks, when presetting macro environment
    starting_stack_size: usize,
    // For transferring corresponding entries from the macro_stack to
    // the source_map.macro_map when recursing
    macro_source_map: FxHashMap<MacroName, MacroSource>,
    // We are stealing syntax for SSR placeholders, to match what merl does.
    // This means we need to lower a Var specially when processing a SSR
    // template, where if it has a prefix of `_@` it is a placeholder.
    in_ssr: bool,
    // Track when we're lowering the RHS of a define preprocessor directive
    in_macro_rhs: bool,
    // Diagnostics collected during body lowering
    diagnostics: Vec<BodyDiagnostic>,
    // Local macro definitions for context during body lowering.
    // Uses Arc to share snapshots from the preprocessor without cloning.
    local_macro_defs: Option<Arc<FxHashMap<MacroName, InFile<DefineId>>>>,
    // Module name override for ?MODULE etc. when processing included files
    module_name_override: Option<Name>,
    // When true, skip import resolution (imported_module returns None).
    // Used in condition expression lowering to break the salsa cycle:
    // file_preprocessor_analysis -> lower_condition_expr -> lower_condition_body
    //   -> imported_module -> def_map -> file_preprocessor_analysis
    // Condition expressions only contain guard expressions, whose BIFs are
    // resolved via is_erlang_fun, not through the import table.
    skip_import_resolution: bool,
    // Guards extracted from ReplacementExprGuard macros during pattern lowering.
    // When a macro like ?DISCONNECT_ERROR expands to `Pattern when Guard1; Guard2; ...`
    // (a ReplacementExprGuard), the pattern part is lowered normally but the guards
    // are stored here so that lower_cr_clause can merge them with the clause's own guards.
    pending_macro_guards: Guards,
    // When true, ReplacementExprGuard macros in pattern position will stash
    // their guards into pending_macro_guards. Only set during lower_cr_clause
    // pattern lowering to prevent guards leaking from other pattern contexts.
    in_cr_clause_pattern: bool,
}

#[derive(Debug)]
enum MacroReplacement {
    BuiltIn(BuiltInMacro),
    Ast(InFile<DefineId>, ast::MacroDefReplacement),
    BuiltInArgs(BuiltInMacro, MacroCallArgs),
    AstArgs(InFile<DefineId>, ast::MacroDefReplacement, MacroCallArgs),
}

pub(crate) type MacroInformation = (
    Vec<MacroStackEntry>,
    usize,
    FxHashMap<MacroSource, ResolvedMacro>,
);

impl<'a> Ctx<'a> {
    pub fn new(db: &'a dyn DefDatabase, origin: BodyOrigin) -> Self {
        Self {
            db,
            macro_stack: vec![MacroStackEntry {
                name: MacroName::new(Name::MISSING, None),
                file_id: origin.file_id(),
                var_map: FxHashMap::default(),
                parent_id: 0,
            }],
            macro_stack_id: 0,
            function_info: None,
            function_name: None,
            epp_function_arity: None,
            body: Body::new(origin),
            module_expr_ids: FxHashMap::default(),
            erlang_type_expr_id: None,
            source_map: BodySourceMap::default(),
            starting_stack_size: 1,
            macro_source_map: FxHashMap::default(),
            in_ssr: false,
            in_macro_rhs: false,
            diagnostics: Vec::new(),
            local_macro_defs: None,
            module_name_override: None,
            skip_import_resolution: false,
            pending_macro_guards: Vec::new(),
            in_cr_clause_pattern: false,
        }
    }

    pub fn file_id(&self) -> FileId {
        self.body.origin.file_id()
    }

    pub fn set_function_info(&mut self, info: &NameArity) {
        let name = Atom::new(info.name());
        let arity = info.arity();
        self.function_info = Some((name, arity));
        self.function_name = Some(info.clone());
    }

    pub fn set_function_info_from_ast(&mut self, clause: &ast::FunctionClause) {
        let info = self.resolve_name_arity(clause);
        if let Some(info) = info {
            let name = Atom::new(info.name());
            let arity = info.arity();
            self.function_info = Some((name, arity));
            self.function_name = Some(info.clone());
        }
    }

    /// Set the epp-compatible function arity used exclusively for
    /// `?FUNCTION_ARITY` expansion.  OTP's `epp:update_fun_name_1`
    /// scans the **first clause** of the function to determine the
    /// arity; when that clause's first argument consists entirely of
    /// bracket tokens (e.g. `[]`, `{}`, `<<>>`), the arity is off by
    /// one.  We replicate the bug so our abstract forms match the
    /// compiler output.
    /// <https://github.com/erlang/otp/issues/10705>
    ///
    /// `first_clause` must be the AST of the **first** clause of the
    /// enclosing function, regardless of which clause is currently
    /// being lowered.
    pub fn set_epp_function_arity(&mut self, first_clause: &ast::FunctionClause) {
        if elp_base_db::Otp::epp_has_function_arity_bug()
            && let Some((_, arity)) = self.function_info
            && is_first_arg_bracket_only(first_clause)
        {
            self.epp_function_arity = Some(arity.saturating_sub(1));
        }
    }

    pub fn function_name(&self) -> Option<NameArity> {
        self.function_name.clone()
    }

    pub(crate) fn set_local_macro_defs(
        &mut self,
        defs: Arc<FxHashMap<MacroName, InFile<DefineId>>>,
    ) {
        self.local_macro_defs = Some(defs);
    }

    /// Set local macro defs from the preprocessor analysis for a given form's
    /// ConditionEnvId. This gives body lowering the correct point-in-file macro
    /// state, matching the behavior of lower_condition_body.
    ///
    /// Only effective when `ifdef` is enabled, since that is when
    /// ConditionEnvIds properly differentiate points in the file (a new
    /// ID is allocated at each -define/-undef/-include). When disabled,
    /// all forms share the root env and we fall back to db.resolve_macro().
    // Groundwork: used in the next commit (2/n macro-state).
    #[allow(dead_code)]
    pub(crate) fn set_macro_defs_from_preprocessor(
        &mut self,
        file_id: FileId,
        env_id: ConditionEnvId,
    ) {
        let env = self.db.project_macro_environment(file_id);
        if !env.ifdef {
            return;
        }
        let macro_defs = compute_file_macro_defs(self.db, file_id, env);
        if let Some(defs) = macro_defs.macro_defs_for_env(env_id) {
            self.local_macro_defs = Some(Arc::clone(defs));
        }
    }

    pub(crate) fn set_module_name_override(&mut self, name: Option<Name>) {
        self.module_name_override = name;
    }

    fn resolve_macro_name(&self, name: &MacroName) -> Option<ResolvedMacro> {
        // Check local macro definitions first (set during body lowering)
        if let Some(ref local_defs) = self.local_macro_defs {
            if let Some(def_idx) = local_defs.get(name) {
                return Some(ResolvedMacro::User(*def_idx));
            }
            // When local defs are set, only check built-in macros as fallback.
            // Do not call db.resolve_macro() — local defs already provide the
            // complete point-in-time macro state.
            // Note: do NOT fall back to arity=None here. The caller
            // (resolve_macro) handles arity fallback and needs to
            // distinguish exact-arity matches from no-arity matches to
            // correctly create MacroReplacement::Ast vs AstArgs.
            return macro_exp::resolve_built_in(name)
                .and_then(|opt| opt.map(ResolvedMacro::BuiltIn));
        }

        // Fall back to database query (normal non-condition path)
        self.db.resolve_macro(self.file_id(), name.clone())
    }

    fn get_macro_information(&self) -> MacroInformation {
        let mut macro_map: FxHashMap<MacroSource, ResolvedMacro> = FxHashMap::default();
        self.macro_stack.iter().for_each(|e| {
            if let Some(macro_source) = self.macro_source_map.get(&e.name)
                && let Some(resolution) = self.source_map.macro_map.get(macro_source)
            {
                macro_map.insert(*macro_source, *resolution);
            }
        });
        (self.macro_stack.clone(), self.macro_stack_id, macro_map)
    }

    pub(crate) fn set_macro_information(&mut self, stack: MacroInformation) {
        let (macro_stack, macro_stack_id, macro_map) = stack;
        self.macro_stack = macro_stack;
        self.macro_stack_id = macro_stack_id;
        self.starting_stack_size = self.macro_stack.len();
        self.source_map.macro_map = macro_map;
    }

    fn finish(mut self) -> (Arc<Body>, BodySourceMap) {
        // Verify macro expansion state
        let entry = self.macro_stack.pop().expect("BUG: macro stack empty");
        if self.macro_stack.is_empty() {
            // We can only check this at the actual end, not when
            // finishing a recursive case.
            assert_eq!(entry.file_id, self.file_id());
            assert_eq!(entry.parent_id, 0);
            assert!(entry.var_map.is_empty());
        }

        // Add one for the pop
        assert!(self.macro_stack.len() + 1 == self.starting_stack_size);

        assert!(self.body.origin.is_valid());
        self.body.shrink_to_fit();
        self.source_map.diagnostics = self.diagnostics;
        (Arc::new(self.body), self.source_map)
    }

    pub fn lower_function_clause(
        mut self,
        function_clause: &ast::FunctionClause,
        from_macro: Option<TopLevelMacro>,
    ) -> (FunctionClauseBody, BodySourceMap) {
        let name = self.function_name();
        let clause = self.lower_clause(function_clause);
        let (body, source_map) = self.finish();

        (
            FunctionClauseBody {
                name,
                from_macro,
                body,
                clause,
            },
            source_map,
        )
    }

    pub fn lower_top_level_macro(
        &mut self,
        args: Vec<ast::MacroExpr>,
        macro_def: InFile<DefineId>,
    ) -> TopLevelMacro {
        let args = args
            .iter()
            .map(|expr| self.lower_optional_expr(expr.expr()))
            .collect();
        TopLevelMacro { args, macro_def }
    }

    pub fn resolve_name_arity(
        &mut self,
        function_clause: &ast::FunctionClause,
    ) -> Option<NameArity> {
        let named_atom = self.resolve_name(function_clause.name()?)?;
        let name = named_atom.as_name();
        let arity = function_clause.args()?.args().count().try_into().ok()?;
        Some(NameArity::new(name, arity))
    }

    pub fn lower_type_alias(self, type_alias: &ast::TypeAlias) -> (TypeBody, BodySourceMap) {
        self.do_lower_type_alias(type_alias.name(), type_alias.ty())
    }

    pub fn lower_nominal_type(self, nominal_type: &ast::Nominal) -> (TypeBody, BodySourceMap) {
        self.do_lower_type_alias(nominal_type.name(), nominal_type.ty())
    }

    pub fn lower_opaque_type_alias(self, type_alias: &ast::Opaque) -> (TypeBody, BodySourceMap) {
        self.do_lower_type_alias(type_alias.name(), type_alias.ty())
    }

    fn do_lower_type_alias(
        mut self,
        name: Option<ast::TypeName>,
        ty: Option<ast::Expr>,
    ) -> (TypeBody, BodySourceMap) {
        let vars = name
            .and_then(|name| name.args())
            .iter()
            .flat_map(|args| args.args())
            .map(|var| Var::new(&var.as_name()))
            .collect();
        let ty = self.lower_optional_type_expr(ty);
        let (body, source_map) = self.finish();

        (TypeBody { body, vars, ty }, source_map)
    }

    pub fn lower_record(
        mut self,
        record: &Record,
        ast: &ast::RecordDecl,
    ) -> (RecordBody, BodySourceMap) {
        let fields = record
            .fields
            .clone()
            .zip(ast.fields())
            .map(|(field_id, field)| {
                let expr = field
                    .expr()
                    .and_then(|field| field.expr())
                    .map(|expr| self.lower_expr(&expr));
                let ty = field
                    .ty()
                    .and_then(|field| field.expr())
                    .map(|expr| self.lower_type_expr(&expr));
                RecordFieldBody { field_id, expr, ty }
            })
            .collect();

        let (body, source_map) = self.finish();
        (RecordBody { body, fields }, source_map)
    }

    pub fn lower_spec(mut self, spec: &ast::Spec) -> (SpecBody, BodySourceMap) {
        let sigs = self.lower_sigs(spec.sigs());
        let (body, source_map) = self.finish();
        (SpecBody { body, sigs }, source_map)
    }

    pub fn lower_callback(mut self, callback: &ast::Callback) -> (SpecBody, BodySourceMap) {
        let sigs = self.lower_sigs(callback.sigs());
        let (body, source_map) = self.finish();
        (SpecBody { body, sigs }, source_map)
    }

    fn lower_sigs(&mut self, sigs: impl Iterator<Item = ast::TypeSig>) -> Vec<SpecSig> {
        sigs.map(|sig| {
            let args = sig
                .args()
                .iter()
                .flat_map(|args| args.args())
                .map(|arg| self.lower_type_expr(&arg))
                .collect();
            let result = self.lower_optional_type_expr(sig.ty());
            let guards = sig
                .guard()
                .iter()
                .flat_map(|guards| guards.guards())
                .flat_map(|guard| {
                    let ty = self.lower_optional_type_expr(guard.ty());
                    let var = Var::new(&guard.var()?.var()?.as_name());
                    Some((var, ty))
                })
                .collect();
            SpecSig {
                args,
                result,
                guards,
            }
        })
        .collect()
    }

    pub fn lower_attribute(mut self, attr: &ast::WildAttribute) -> (AttributeBody, BodySourceMap) {
        let value = self.lower_optional_term(attr.value());
        let (body, source_map) = self.finish();
        (AttributeBody { body, value }, source_map)
    }

    pub fn lower_define(mut self, define: &ast::PpDefine) -> (DefineBody, BodySourceMap) {
        let replacement = define.replacement();
        match replacement {
            Some(MacroDefReplacement::Expr(expr)) => {
                self.in_macro_rhs = true;
                let expr = self.lower_expr(&expr);
                self.in_macro_rhs = false;
                let (body, source_map) = self.finish();
                (DefineBody { body, expr }, source_map)
            }
            _ => {
                let expr = self.alloc_expr(Expr::Missing, None);
                let (body, source_map) = self.finish();
                (DefineBody { body, expr }, source_map)
            }
        }
    }

    pub fn lower_compile(
        mut self,
        attr: &ast::CompileOptionsAttribute,
    ) -> (AttributeBody, BodySourceMap) {
        let value = self.lower_optional_term(attr.options());
        let (body, source_map) = self.finish();
        (AttributeBody { body, value }, source_map)
    }

    pub(crate) fn lower_clause_or_macro_body(
        &mut self,
        clause: ast::FunctionOrMacroClause,
        clause_id: &InFile<FunctionClauseId>,
        macro_def: Option<(InFile<DefineId>, Vec<ast::MacroExpr>)>,
        first_clause_ast: Option<&ast::FunctionClause>,
    ) -> impl Iterator<Item = (FunctionClauseBody, BodySourceMap)> + use<> {
        match clause {
            ast::FunctionOrMacroClause::FunctionClause(clause) => {
                let macrostack = self.get_macro_information();
                Either::Left(once(FunctionClauseBody::lower_clause_body(
                    self.db,
                    &clause,
                    clause_id,
                    macrostack,
                    macro_def,
                    first_clause_ast,
                )))
            }
            ast::FunctionOrMacroClause::MacroCallExpr(call) => {
                Either::Right(
                    self.resolve_macro(&call, |this, _source, replacement| {
                        match replacement {
                            MacroReplacement::Ast(
                                def_idx,
                                ast::MacroDefReplacement::ReplacementFunctionClauses(clauses),
                            ) => clauses
                                .clauses()
                                .flat_map(|clause| {
                                    let args: Vec<_> =
                                        call.args().iter().flat_map(|args| args.args()).collect();
                                    // Is this clause_id appropriate here?
                                    this.lower_clause_or_macro_body(
                                        clause,
                                        clause_id,
                                        Some((def_idx, args)),
                                        None,
                                    )
                                })
                                .collect(),
                            // no built-in macro makes sense in this place
                            MacroReplacement::Ast(_, _) | MacroReplacement::BuiltIn(_) => vec![],
                            // args make no sense here
                            MacroReplacement::AstArgs(_, _, _)
                            | MacroReplacement::BuiltInArgs(_, _) => vec![],
                        }
                    })
                    .into_iter()
                    .flatten(),
                )
            }
        }
    }

    fn lower_clause(&mut self, clause: &ast::FunctionClause) -> Clause {
        let pats = clause
            .args()
            .iter()
            .flat_map(|args| args.args())
            .map(|pat| self.lower_pat(&pat))
            .collect();
        let guards = self.lower_guards(clause.guard());
        let exprs = self.lower_clause_body(clause.body());

        Clause {
            pats,
            guards,
            exprs,
        }
    }

    pub fn lower_ssr(
        mut self,
        ssr_source: SsrSource,
        ssr: &ast::SsrDefinition,
    ) -> Option<(SsrBody, BodySourceMap)> {
        self.in_ssr = true;
        let lhs_ast = ssr.lhs()?;
        let lhs_expr = self.lower_expr(&lhs_ast);
        let lhs_pat = self.lower_pat(&lhs_ast);
        let rhs = ssr.rhs().and_then(|rhs| {
            rhs.expr().map(|rhs_ast| SsrPatternIds {
                expr: self.lower_expr(&rhs_ast),
                pat: self.lower_pat(&rhs_ast),
            })
        });
        let when = ssr.r#where().map(|w| self.lower_guards(w.guard()));
        let (body, source_map) = self.finish();
        Some((
            SsrBody {
                ssr_source,
                body,
                pattern: SsrPatternIds {
                    expr: lhs_expr,
                    pat: lhs_pat,
                },
                template: rhs,
                when,
            },
            source_map,
        ))
    }

    fn lower_optional_pat(&mut self, expr: Option<ast::Expr>) -> PatId {
        if let Some(expr) = &expr {
            self.lower_pat(expr)
        } else {
            self.alloc_pat(Pat::Missing, None)
        }
    }

    fn lower_pat(&mut self, expr: &ast::Expr) -> PatId {
        match expr {
            ast::Expr::ExprMax(expr_max) => self.lower_pat_max(expr_max, expr),
            ast::Expr::AnnType(ann) => {
                let _ = self.lower_optional_pat(ann.ty());
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::Expr::BinaryOpExpr(binary_op) => {
                let lhs = self.lower_optional_pat(binary_op.lhs());
                let rhs = self.lower_optional_pat(binary_op.rhs());
                if let Some((op, _)) = binary_op.op() {
                    self.alloc_pat(Pat::BinaryOp { lhs, op, rhs }, Some(expr))
                } else {
                    self.alloc_pat(Pat::Missing, Some(expr))
                }
            }
            ast::Expr::Call(call) => {
                let _ = self.lower_optional_pat(call.expr());
                call.args()
                    .iter()
                    .flat_map(|args| args.args())
                    .for_each(|expr| {
                        let _ = self.lower_pat(&expr);
                    });
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::Expr::CatchExpr(catch) => {
                let _ = self.lower_optional_pat(catch.expr());
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::Expr::Dotdotdot(_) => self.alloc_pat(Pat::Missing, Some(expr)),
            ast::Expr::MapExpr(map) => {
                let fields = map
                    .fields()
                    .flat_map(|field| {
                        let key = self.lower_optional_expr(field.key());
                        let value = self.lower_optional_pat(field.value());
                        if let Some((ast::MapOp::Exact, _)) = field.op() {
                            Some((key, value))
                        } else {
                            None
                        }
                    })
                    .collect();
                self.alloc_pat(Pat::Map { fields }, Some(expr))
            }
            ast::Expr::MapExprUpdate(update) => {
                let _ = self.lower_optional_pat(update.expr().map(Into::into));
                update.fields().for_each(|field| {
                    let _ = self.lower_optional_expr(field.key());
                    let _ = self.lower_optional_expr(field.value());
                });
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::Expr::MatchExpr(mat) => {
                let lhs = self.lower_optional_pat(mat.lhs());
                let rhs = self.lower_optional_pat(mat.rhs());
                self.alloc_pat(Pat::Match { lhs, rhs }, Some(expr))
            }
            ast::Expr::Pipe(pipe) => {
                let _ = self.lower_optional_pat(pipe.lhs());
                let _ = self.lower_optional_pat(pipe.rhs());
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::Expr::RangeType(range) => {
                let _ = self.lower_optional_pat(range.lhs());
                let _ = self.lower_optional_pat(range.rhs());
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::Expr::RecordExpr(record) => {
                let name = record.name().and_then(|n| self.resolve_name(n.name()?));
                let (fields, default_field) = self.lower_record_fields_pat(record.fields());
                if let Some(name) = name {
                    self.alloc_pat(
                        Pat::Record {
                            name,
                            fields,
                            default_field,
                        },
                        Some(expr),
                    )
                } else {
                    self.alloc_pat(Pat::Missing, Some(expr))
                }
            }
            ast::Expr::RecordFieldExpr(field) => {
                let _ = self.lower_optional_pat(field.expr().map(Into::into));
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::Expr::RecordIndexExpr(index) => {
                let name = index.name().and_then(|n| self.resolve_name(n.name()?));
                let field = index.field().and_then(|n| self.resolve_name(n.name()?));
                if let (Some(name), Some(field)) = (name, field) {
                    self.alloc_pat(Pat::RecordIndex { name, field }, Some(expr))
                } else {
                    self.alloc_pat(Pat::Missing, Some(expr))
                }
            }
            ast::Expr::RecordUpdateExpr(update) => {
                let _ = self.lower_optional_pat(update.expr().map(Into::into));
                update
                    .fields()
                    .flat_map(|field| field.expr()?.expr())
                    .for_each(|expr| {
                        let _ = self.lower_pat(&expr);
                    });
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::Expr::Remote(remote) => match remote.fun() {
                Some(ast::Expr::Call(call)) => {
                    let _ =
                        self.lower_optional_pat(remote.module().and_then(|module| module.module()));
                    let _ = self.lower_optional_pat(call.expr());
                    call.args()
                        .iter()
                        .flat_map(|args| args.args())
                        .for_each(|expr| {
                            let _ = self.lower_pat(&expr);
                        });
                    self.alloc_pat(Pat::Missing, Some(expr))
                }
                _ => {
                    let _ =
                        self.lower_optional_pat(remote.module().and_then(|module| module.module()));
                    let _ = self.lower_optional_pat(remote.fun());
                    self.alloc_pat(Pat::Missing, Some(expr))
                }
            },
            ast::Expr::UnaryOpExpr(unary_op) => {
                let operand = self.lower_optional_pat(unary_op.operand());
                if let Some((op, _)) = unary_op.op() {
                    self.alloc_pat(Pat::UnaryOp { pat: operand, op }, Some(expr))
                } else {
                    self.alloc_pat(Pat::Missing, Some(expr))
                }
            }
            ast::Expr::CondMatchExpr(cond) => {
                self.lower_optional_pat(cond.lhs());
                self.lower_optional_pat(cond.rhs());
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::Expr::AnonRecordExpr(record) => {
                let fields = self.lower_native_record_fields_pat(record.fields());
                self.alloc_pat(
                    Pat::NativeRecord {
                        name: NativeRecordName::Anon,
                        fields,
                    },
                    Some(expr),
                )
            }
            ast::Expr::AnonRecordFieldExpr(field_expr) => {
                let _ = self.lower_optional_pat(field_expr.expr().map(Into::into));
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::Expr::AnonRecordUpdateExpr(update) => {
                let _ = self.lower_optional_pat(update.expr().map(Into::into));
                update
                    .fields()
                    .flat_map(|field| field.expr()?.expr())
                    .for_each(|expr| {
                        let _ = self.lower_pat(&expr);
                    });
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::Expr::QualifiedRecordExpr(record) => {
                let name = record
                    .name()
                    .and_then(|n| self.lower_qualified_record_name(&n));
                let fields = self.lower_native_record_fields_pat(record.fields());
                if let Some(name) = name {
                    self.alloc_pat(Pat::NativeRecord { name, fields }, Some(expr))
                } else {
                    self.alloc_pat(Pat::Missing, Some(expr))
                }
            }
            ast::Expr::QualifiedRecordFieldExpr(field_expr) => {
                let _ = self.lower_optional_pat(field_expr.expr().map(Into::into));
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::Expr::QualifiedRecordUpdateExpr(update) => {
                let _ = self.lower_optional_pat(update.expr().map(Into::into));
                update
                    .fields()
                    .flat_map(|field| field.expr()?.expr())
                    .for_each(|expr| {
                        let _ = self.lower_pat(&expr);
                    });
                self.alloc_pat(Pat::Missing, Some(expr))
            }
        }
    }

    fn lower_pat_max(&mut self, expr_max: &ast::ExprMax, expr: &ast::Expr) -> PatId {
        match expr_max {
            ast::ExprMax::AnonymousFun(fun) => {
                fun.clauses().for_each(|clause| {
                    clause
                        .args()
                        .iter()
                        .flat_map(|args| args.args())
                        .for_each(|pat| {
                            let _ = self.lower_pat(&pat);
                        });
                    let _ = self.lower_guards(clause.guard());
                    let _ = self.lower_clause_body(clause.body());
                });
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::ExprMax::Atom(atom) => {
                let atom = Atom::new(&atom.as_name());
                self.alloc_pat(Pat::Literal(Literal::Atom(atom)), Some(expr))
            }
            ast::ExprMax::Binary(bin) => {
                let segs = bin
                    .elements()
                    .flat_map(|element| {
                        self.lower_bin_element_or_expand_macro(
                            &element,
                            Self::lower_optional_pat,
                            |this, call| this.try_expand_binary_pat_macro(call),
                        )
                    })
                    .collect();
                self.alloc_pat(Pat::Binary { segs }, Some(expr))
            }
            ast::ExprMax::BinaryComprehension(bc) => {
                let _ = self.lower_optional_pat(bc.expr().map(Into::into));
                let _ = self.lower_lc_exprs(bc.lc_exprs());
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::ExprMax::BlockExpr(block) => {
                block.exprs().for_each(|expr| {
                    self.lower_expr(&expr);
                });
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::ExprMax::CaseExpr(case) => {
                let _ = self.lower_optional_pat(case.expr());
                let _ = case
                    .clauses()
                    .flat_map(|clause| self.lower_cr_clause(clause))
                    .last();
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::ExprMax::Char(char) => {
                let value = lower_char(char).map_or(Pat::Missing, Pat::Literal);
                self.alloc_pat(value, Some(expr))
            }
            ast::ExprMax::Concatables(concat) => {
                let value = self
                    .lower_concat_with_macros(concat)
                    .map_or(Pat::Missing, Pat::Literal);
                self.alloc_pat(value, Some(expr))
            }
            ast::ExprMax::ExternalFun(fun) => {
                let _ = self.lower_optional_pat(
                    fun.module()
                        .and_then(|module| module.name())
                        .map(Into::into),
                );
                let _ = self.lower_optional_pat(fun.fun().map(Into::into));
                let _ = self.lower_optional_pat(
                    fun.arity().and_then(|arity| arity.value()).map(Into::into),
                );
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::ExprMax::Float(float) => {
                let value = lower_float(float).map_or(Pat::Missing, Pat::Literal);
                self.alloc_pat(value, Some(expr))
            }
            ast::ExprMax::FunType(fun) => {
                if let Some(sig) = fun.sig() {
                    let _ = self.lower_optional_pat(sig.ty());
                    sig.args()
                        .iter()
                        .flat_map(|args| args.args())
                        .for_each(|pat| {
                            let _ = self.lower_pat(&pat);
                        });
                }
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::ExprMax::IfExpr(if_expr) => {
                if_expr.clauses().for_each(|clause| {
                    let _ = self.lower_guards(clause.guard());
                    let _ = self.lower_clause_body(clause.body());
                });
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::ExprMax::Integer(int) => {
                let value = lower_int(int).map_or(Pat::Missing, Pat::Literal);
                self.alloc_pat(value, Some(expr))
            }
            ast::ExprMax::InternalFun(fun) => {
                let _ = self.lower_optional_pat(fun.fun().map(Into::into));
                let _ = self.lower_optional_pat(
                    fun.arity().and_then(|arity| arity.value()).map(Into::into),
                );
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::ExprMax::List(list) => {
                let (pats, tail) = self.lower_list(
                    list,
                    |this| this.alloc_pat(Pat::Missing, None),
                    |this, expr| this.lower_pat(expr),
                    |this, call| this.try_expand_list_pat_macro(call),
                );
                self.alloc_pat(Pat::List { pats, tail }, Some(expr))
            }
            ast::ExprMax::ListComprehension(lc) => {
                lc.exprs().for_each(|e| {
                    let _ = self.lower_pat(&e);
                });
                let _ = self.lower_lc_exprs(lc.lc_exprs());
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::ExprMax::MacroCallExpr(call) => self
                .resolve_macro(call, |this, source, replacement| match replacement {
                    MacroReplacement::BuiltIn(built_in) => this
                        .lower_built_in_macro(built_in)
                        .map(|literal| {
                            let pat_id = this.alloc_pat(Pat::Literal(literal), Some(expr));
                            this.record_pat_source(pat_id, source);
                            (None, pat_id)
                        }),
                    MacroReplacement::Ast(def_idx,ast::MacroDefReplacement::Expr(macro_expr)) => {
                        let pat_id = this.lower_pat(&macro_expr);
                        this.record_pat_source(pat_id, source);
                        Some((Some(def_idx), pat_id))
                    }
                    MacroReplacement::Ast(def_idx, ast::MacroDefReplacement::ReplacementExprGuard(expr_guard)) => {
                        // Macro expands to `Pattern when Guard1; Guard2; ...`
                        // (e.g., ?DISCONNECT_ERROR). Lower the pattern part and,
                        // if in a CR-clause pattern, stash the guards so
                        // lower_cr_clause can merge them.
                        if let Some(macro_expr) = expr_guard.expr() {
                            let pat_id = this.lower_pat(&macro_expr);
                            this.record_pat_source(pat_id, source);
                            if this.in_cr_clause_pattern {
                                let guards = this.lower_guards(expr_guard.guard());
                                this.pending_macro_guards.extend(guards);
                            }
                            Some((Some(def_idx), pat_id))
                        } else {
                            None
                        }
                    }
                    MacroReplacement::Ast(_,_)
                    // calls are not allowed in patterns
                    | MacroReplacement::BuiltInArgs(_, _)
                    | MacroReplacement::AstArgs(_,_, _) => None,
                })
                .flatten()
                .map(|(macro_def, expansion)| {
                    let args = call
                        .args()
                        .iter()
                        .flat_map(|args| args.args())
                        .map(|expr| self.lower_optional_expr(expr.expr()))
                        .collect();
                    self.alloc_pat(
                        Pat::MacroCall {
                            expansion,
                            args,
                            macro_def,
                            macro_name: self.macro_call_name(call.name()),
                        },
                        Some(expr),
                    )
                })
                .unwrap_or_else(|| {
                    // Record diagnostic for unresolved macro
                    self.record_unresolved_macro(call);

                    let expansion = self.alloc_pat(Pat::Missing, Some(expr));
                    let args = call
                        .args()
                        .iter()
                        .flat_map(|args| args.args())
                        .map(|expr| self.lower_optional_expr(expr.expr()))
                        .collect();
                    self.alloc_pat(
                        Pat::MacroCall {
                            expansion,
                            args,
                            macro_def: None,
                            macro_name: self.macro_call_name(call.name()),
                        },
                        Some(expr),
                    )
                }),
            ast::ExprMax::MacroString(ms) => {
                if let Some(lit) = self.resolve_macro_string(ms) {
                    self.alloc_pat(Pat::Literal(lit), Some(expr))
                } else {
                    self.alloc_pat(Pat::Missing, Some(expr))
                }
            }
            ExprMax::MapComprehension(map_comp) => {
                map_comp.exprs().for_each(|mf| {
                    let _ = self.lower_optional_pat(mf.key());
                    let _ = self.lower_optional_pat(mf.value());
                });
                let _ = self.lower_lc_exprs(map_comp.lc_exprs());
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::ExprMax::MaybeExpr(maybe) => {
                maybe.exprs().for_each(|expr| {
                    self.lower_expr(&expr);
                });
                let _ = maybe
                    .clauses()
                    .flat_map(|clause| self.lower_cr_clause(clause))
                    .last();
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::ExprMax::ParenExpr(paren_expr) => {
                if let Some(inner_paren_expr) = paren_expr.expr() {
                    let pat_id = self.lower_pat(&inner_paren_expr);
                    self.alloc_pat(Pat::Paren { pat: pat_id }, Some(expr))
                } else {
                    self.alloc_pat(Pat::Missing, Some(expr))
                }
            }
            ast::ExprMax::ReceiveExpr(receive) => {
                let _ = receive
                    .clauses()
                    .flat_map(|clause| self.lower_cr_clause(clause))
                    .last();
                let _ = receive.after().map(|after| {
                    let _ = self.lower_optional_expr(after.expr());
                    let _ = self.lower_clause_body(after.body());
                });
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::ExprMax::String(str) => {
                let value = self
                    .lower_str_or_sigil(
                        str,
                        expr,
                        Self::lower_binary_string_literal_pat,
                        Pat::Literal,
                    )
                    .unwrap_or(Pat::Missing);
                self.alloc_pat(value, Some(expr))
            }
            ast::ExprMax::TryExpr(try_expr) => {
                try_expr.exprs().for_each(|expr| {
                    self.lower_pat(&expr);
                });
                let _ = try_expr
                    .clauses()
                    .flat_map(|clause| self.lower_cr_clause(clause))
                    .last();
                try_expr.catch().for_each(|clause| {
                    let _ = clause
                        .class()
                        .and_then(|class| class.class())
                        .map(|class| self.lower_pat(&class.into()));
                    let _ = self.lower_optional_pat(clause.pat().map(Into::into));
                    let _ = clause
                        .stack()
                        .and_then(|stack| stack.class())
                        .map(|var| self.lower_pat(&ast::Expr::ExprMax(ast::ExprMax::Var(var))));
                    let _ = self.lower_guards(clause.guard());
                    let _ = self.lower_clause_body(clause.body());
                });
                try_expr
                    .after()
                    .iter()
                    .flat_map(|after| after.exprs())
                    .for_each(|expr| {
                        self.lower_pat(&expr);
                    });
                self.alloc_pat(Pat::Missing, Some(expr))
            }
            ast::ExprMax::Tuple(tup) => {
                let pats = self.expand_or_lower(
                    tup.expr(),
                    |this, call| this.try_expand_list_pat_macro(call),
                    |this, expr| this.lower_pat(expr),
                );
                self.alloc_pat(Pat::Tuple { pats }, Some(expr))
            }
            ast::ExprMax::Var(var) => self
                .resolve_var(var, |this, expr| this.lower_optional_pat(expr.expr()))
                .unwrap_or_else(|var| self.alloc_pat(Pat::Var(var), Some(expr))),
        }
    }

    fn lower_optional_expr(&mut self, expr: Option<ast::Expr>) -> ExprId {
        if let Some(expr) = &expr {
            self.lower_expr(expr)
        } else {
            self.alloc_expr(Expr::Missing, None)
        }
    }

    fn lower_expr(&mut self, expr: &ast::Expr) -> ExprId {
        match expr {
            ast::Expr::ExprMax(expr_max) => self.lower_expr_max(expr_max, expr),
            ast::Expr::AnnType(ann) => {
                let _ = self.lower_optional_expr(ann.ty());
                self.alloc_expr(Expr::Missing, Some(expr))
            }
            ast::Expr::BinaryOpExpr(binary_op) => {
                let lhs = self.lower_optional_expr(binary_op.lhs());
                let rhs = self.lower_optional_expr(binary_op.rhs());
                if let Some((op, _)) = binary_op.op() {
                    self.alloc_expr(Expr::BinaryOp { lhs, op, rhs }, Some(expr))
                } else {
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            }
            ast::Expr::Call(call) => {
                let target = self.lower_call_target(call.expr(), call.arity_value());
                let args = self.expand_or_lower(
                    call.args().iter().flat_map(|args| args.args()),
                    |this, call| this.try_expand_list_expr_macro(call),
                    |this, expr| this.lower_expr(expr),
                );
                self.alloc_expr(Expr::Call { target, args }, Some(expr))
            }
            ast::Expr::CatchExpr(catch) => {
                let value = self.lower_optional_expr(catch.expr());
                self.alloc_expr(Expr::Catch { expr: value }, Some(expr))
            }
            ast::Expr::Dotdotdot(_) => self.alloc_expr(Expr::Missing, Some(expr)),
            ast::Expr::MapExpr(map) => {
                // Keep both `=>` (Assoc) and `:=` (Exact) fields. While `:=` is
                // only valid in patterns in standard Erlang, a map literal can
                // appear as a macro argument where the eventual context is
                // unknown, and dropping `:=` fields makes their inner
                // expressions unreachable for folds (e.g. SSR matching).
                let fields = map
                    .fields()
                    .map(|field| {
                        let key = self.lower_optional_expr(field.key());
                        let value = self.lower_optional_expr(field.value());
                        (key, value)
                    })
                    .collect();
                self.alloc_expr(Expr::Map { fields }, Some(expr))
            }
            ast::Expr::MapExprUpdate(update) => {
                let base = self.lower_optional_expr(update.expr().map(Into::into));
                let fields = update
                    .fields()
                    .flat_map(|field| {
                        let key = self.lower_optional_expr(field.key());
                        let value = self.lower_optional_expr(field.value());
                        Some((key, field.op()?.0, value))
                    })
                    .collect();
                self.alloc_expr(Expr::MapUpdate { expr: base, fields }, Some(expr))
            }
            ast::Expr::MatchExpr(mat) => {
                let lhs = self.lower_optional_pat(mat.lhs());
                let rhs = self.lower_optional_expr(mat.rhs());
                self.alloc_expr(Expr::Match { lhs, rhs }, Some(expr))
            }
            ast::Expr::Pipe(pipe) => {
                let _ = self.lower_optional_expr(pipe.lhs());
                let _ = self.lower_optional_expr(pipe.rhs());
                self.alloc_expr(Expr::Missing, Some(expr))
            }
            ast::Expr::RangeType(range) => {
                let _ = self.lower_optional_expr(range.lhs());
                let _ = self.lower_optional_expr(range.rhs());
                self.alloc_expr(Expr::Missing, Some(expr))
            }
            ast::Expr::RecordExpr(record) => {
                let name = record.name().and_then(|n| self.resolve_name(n.name()?));
                let (fields, default_field) = self.lower_record_fields(record.fields());
                if let Some(name) = name {
                    self.alloc_expr(
                        Expr::Record {
                            name,
                            fields,
                            default_field,
                        },
                        Some(expr),
                    )
                } else {
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            }
            ast::Expr::RecordFieldExpr(field) => {
                let base = self.lower_optional_expr(field.expr().map(Into::into));
                let name = field.name().and_then(|n| self.resolve_name(n.name()?));
                let field = field.field().and_then(|n| self.resolve_name(n.name()?));
                if let (Some(name), Some(field)) = (name, field) {
                    self.alloc_expr(
                        Expr::RecordField {
                            expr: base,
                            name,
                            field,
                        },
                        Some(expr),
                    )
                } else {
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            }
            ast::Expr::RecordIndexExpr(index) => {
                let name = index.name().and_then(|n| self.resolve_name(n.name()?));
                let field = index.field().and_then(|n| self.resolve_name(n.name()?));
                if let (Some(name), Some(field)) = (name, field) {
                    self.alloc_expr(Expr::RecordIndex { name, field }, Some(expr))
                } else {
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            }
            ast::Expr::RecordUpdateExpr(update) => {
                let base = self.lower_optional_expr(update.expr().map(Into::into));
                let name = update.name().and_then(|n| self.resolve_name(n.name()?));
                let (fields, _default_field) = self.lower_record_fields(update.fields());
                if let Some(name) = name {
                    self.alloc_expr(
                        Expr::RecordUpdate {
                            expr: base,
                            name,
                            fields,
                        },
                        Some(expr),
                    )
                } else {
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            }
            ast::Expr::Remote(remote) => match remote.fun() {
                Some(ast::Expr::Call(call)) => {
                    let module = self
                        .lower_optional_expr(remote.module().and_then(|module| module.module()));
                    let name_target = self.lower_call_target(call.expr(), call.arity_value());
                    let target = match name_target {
                        CallTarget::Local { name } => CallTarget::Remote {
                            module,
                            name,
                            parens: false,
                            unqualified: false,
                        },
                        CallTarget::Remote { name, parens, .. } => {
                            // Nested remote: the inner call resolved via
                            // BIF/import inference, but we have an explicit
                            // outer module qualifier here, so the resulting
                            // call is NOT unqualified.
                            CallTarget::Remote {
                                module,
                                name,
                                parens,
                                unqualified: false,
                            }
                        }
                    };
                    let args = self.expand_or_lower(
                        call.args().iter().flat_map(|args| args.args()),
                        |this, call| this.try_expand_list_expr_macro(call),
                        |this, expr| this.lower_expr(expr),
                    );
                    let expr_id = self.alloc_expr(Expr::Call { target, args }, Some(expr));
                    // Also record the inner Call node in the forward source map
                    // so that body_map.any_id(ast::Expr::Call) resolves too.
                    let call_ast = ast::Expr::from(call);
                    let call_ptr = AstPtr::new(&call_ast);
                    let call_source = InFileAstPtr::new(self.curr_file_id(), call_ptr);
                    self.source_map.expr_map.insert(call_source, expr_id);
                    expr_id
                }
                _ => {
                    // Bare remote reference: M:F (without call args)
                    let _ = self
                        .lower_optional_expr(remote.module().and_then(|module| module.module()));
                    let _ = self.lower_optional_expr(remote.fun());
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            },
            ast::Expr::UnaryOpExpr(unary_op) => {
                let operand = self.lower_optional_expr(unary_op.operand());
                if let Some((op, _)) = unary_op.op() {
                    self.alloc_expr(Expr::UnaryOp { expr: operand, op }, Some(expr))
                } else {
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            }
            ast::Expr::CondMatchExpr(cond) => {
                let pat_id = self.lower_optional_pat(cond.lhs());
                let expr_id = self.lower_optional_expr(cond.rhs());
                if self.in_ssr {
                    // Bare cond match, wrap it in a Maybe
                    self.alloc_expr(
                        Expr::Maybe {
                            exprs: vec![MaybeExpr::Cond {
                                lhs: pat_id,
                                rhs: expr_id,
                            }],
                            else_clauses: vec![],
                        },
                        Some(expr),
                    )
                } else {
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            }
            ast::Expr::AnonRecordExpr(record) => {
                let fields = self.lower_native_record_fields(record.fields());
                self.alloc_expr(
                    Expr::NativeRecord {
                        name: NativeRecordName::Anon,
                        fields,
                    },
                    Some(expr),
                )
            }
            ast::Expr::AnonRecordFieldExpr(field_expr) => {
                let base = self.lower_optional_expr(field_expr.expr().map(Into::into));
                let field = field_expr
                    .field()
                    .and_then(|n| self.resolve_name(n.name()?));
                if let Some(field) = field {
                    self.alloc_expr(
                        Expr::NativeRecordField {
                            expr: base,
                            name: NativeRecordName::Anon,
                            field,
                        },
                        Some(expr),
                    )
                } else {
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            }
            ast::Expr::AnonRecordUpdateExpr(update) => {
                let base = self.lower_optional_expr(update.expr().map(Into::into));
                let fields = self.lower_native_record_fields(update.fields());
                self.alloc_expr(
                    Expr::NativeRecordUpdate {
                        expr: base,
                        name: NativeRecordName::Anon,
                        fields,
                    },
                    Some(expr),
                )
            }
            ast::Expr::QualifiedRecordExpr(record) => {
                let name = record
                    .name()
                    .and_then(|n| self.lower_qualified_record_name(&n));
                let fields = self.lower_native_record_fields(record.fields());
                if let Some(name) = name {
                    self.alloc_expr(Expr::NativeRecord { name, fields }, Some(expr))
                } else {
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            }
            ast::Expr::QualifiedRecordFieldExpr(field_expr) => {
                let base = self.lower_optional_expr(field_expr.expr().map(Into::into));
                let name = field_expr
                    .name()
                    .and_then(|n| self.lower_qualified_record_name(&n));
                let field = field_expr
                    .field()
                    .and_then(|n| self.resolve_name(n.name()?));
                if let (Some(name), Some(field)) = (name, field) {
                    self.alloc_expr(
                        Expr::NativeRecordField {
                            expr: base,
                            name,
                            field,
                        },
                        Some(expr),
                    )
                } else {
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            }
            ast::Expr::QualifiedRecordUpdateExpr(update) => {
                let base = self.lower_optional_expr(update.expr().map(Into::into));
                let name = update
                    .name()
                    .and_then(|n| self.lower_qualified_record_name(&n));
                let fields = self.lower_native_record_fields(update.fields());
                if let Some(name) = name {
                    self.alloc_expr(
                        Expr::NativeRecordUpdate {
                            expr: base,
                            name,
                            fields,
                        },
                        Some(expr),
                    )
                } else {
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            }
        }
    }

    fn lower_call_target(
        &mut self,
        expr: Option<ast::Expr>,
        arity: ast::Arity,
    ) -> CallTarget<ExprId> {
        match expr.as_ref() {
            Some(ast::Expr::ExprMax(ast::ExprMax::ParenExpr(paren))) => {
                let inner_expr_id = self.alloc_expr(Expr::Missing, paren.expr().as_ref());
                let _ = self.alloc_expr(
                    Expr::Paren {
                        expr: inner_expr_id,
                    },
                    expr.as_ref(),
                );
                match self.lower_call_target(paren.expr(), arity) {
                    CallTarget::Local { name } => {
                        let parened_name =
                            self.alloc_expr(Expr::Paren { expr: name }, expr.as_ref());
                        CallTarget::Local { name: parened_name }
                    }
                    // The entire remote call is wrapped in parens. How do we capture this?
                    CallTarget::Remote {
                        module,
                        name,
                        unqualified,
                        ..
                    } => CallTarget::Remote {
                        module,
                        name,
                        parens: true,
                        unqualified,
                    },
                }
            }
            Some(ast::Expr::Remote(remote)) => CallTarget::Remote {
                module: self
                    .lower_optional_expr(remote.module().and_then(|module| module.module())),
                name: self.lower_optional_expr(remote.fun()),
                parens: false,
                unqualified: false,
            },
            Some(ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(call))) => self
                .resolve_macro(call, |this, source, replacement| match replacement {
                    MacroReplacement::BuiltIn(built_in) => {
                        this.lower_built_in_macro(built_in).map(|literal| {
                            let name = this.alloc_expr(Expr::Literal(literal), None);
                            this.record_expr_source(name, source);
                            CallTarget::Local { name }
                        })
                    }
                    MacroReplacement::Ast(_defidx, ast::MacroDefReplacement::Expr(expr)) => {
                        Some(this.lower_call_target(Some(expr), arity))
                    }
                    MacroReplacement::Ast(_, _) => None,
                    // This would mean double parens in the call - invalid
                    MacroReplacement::BuiltInArgs(_, _) | MacroReplacement::AstArgs(_, _, _) => {
                        None
                    }
                })
                .flatten()
                .unwrap_or_else(|| {
                    // Record diagnostic for unresolved macro
                    self.record_unresolved_macro(call);

                    call.args()
                        .iter()
                        .flat_map(|args| args.args())
                        .for_each(|expr| {
                            let _ = self.lower_optional_expr(expr.expr());
                            let _ = self.lower_optional_expr(expr.guard());
                        });
                    CallTarget::Local {
                        name: self.alloc_expr(Expr::Missing, expr.as_ref()),
                    }
                }),
            Some(expr) => {
                let name = self.lower_expr(expr);
                self.disambiguate_call_target(name, arity)
            }
            None => CallTarget::Local {
                name: self.alloc_expr(Expr::Missing, None),
            },
        }
    }

    fn disambiguate_call_target(&mut self, name: ExprId, arity: ast::Arity) -> CallTarget<ExprId> {
        if let Some(module) = self.import_or_erlang_bif(name, arity) {
            CallTarget::Remote {
                module,
                name,
                parens: false,
                unqualified: true,
            }
        } else {
            CallTarget::Local { name }
        }
    }

    fn import_or_erlang_bif(&mut self, name_expr_id: ExprId, arity: ast::Arity) -> Option<ExprId> {
        let atom = self.body[name_expr_id].as_atom()?;
        let name = atom.as_name();

        // Check that it's not imported, e.g. -import(lists, [length/1]).
        if let Some(import_module) = self.imported_module(name.clone(), arity) {
            Some(self.module_expr_id(&import_module))
        } else if is_erlang_fun(&atom.as_string(), arity?) && !self.is_no_auto_import(&name, arity)
        {
            Some(self.erlang_expr_id())
        } else {
            None
        }
    }

    /// An auto-imported BIF can be disabled for a module via
    /// `-compile({no_auto_import, [F/A]})` (or `-compile(no_auto_import)` for
    /// all BIFs). When disabled, an unqualified call `F(...)` resolves to the
    /// local function rather than `erlang:F/A`, so it must not be rewritten to
    /// a remote call.
    fn is_no_auto_import(&self, name: &Name, arity: ast::Arity) -> bool {
        // Mirror `imported_module`: avoid the def_map query when import
        // resolution is skipped (to break the preprocessor salsa cycle) or in
        // SSR patterns, which have no concrete module.
        if self.skip_import_resolution {
            return false;
        }
        if self.file_id() == SSR_SOURCE_FILE_ID {
            return false;
        }
        let Some(arity) = arity else {
            return false;
        };
        let name_arity = NameArity::new(name.clone(), arity as u32);
        self.db
            .def_map(self.file_id())
            .is_no_auto_import(&name_arity)
    }

    fn imported_module(&self, name: Name, arity: ast::Arity) -> Option<Name> {
        if self.skip_import_resolution {
            return None;
        }
        if self.file_id() != SSR_SOURCE_FILE_ID {
            let name_arity = NameArity::new(name, arity? as u32);
            self.db
                .def_map(self.file_id())
                .get_imports()
                .get(&name_arity)
                .cloned()
        } else {
            None
        }
    }

    fn erlang_expr_id(&mut self) -> ExprId {
        self.module_expr_id(&known::erlang)
    }

    fn erlang_type_expr_id(&mut self) -> TypeExprId {
        self.module_type_expr_id(&known::erlang)
    }

    fn module_expr_id(&mut self, module: &Name) -> ExprId {
        if let Some(expr_id) = self.module_expr_ids.get(module) {
            *expr_id
        } else {
            let atom = Atom::new(module);
            let expr_id = self.alloc_expr(Expr::Literal(Literal::Atom(atom)), None);
            self.module_expr_ids.insert(module.clone(), expr_id);
            expr_id
        }
    }

    fn module_type_expr_id(&mut self, module: &Name) -> TypeExprId {
        if let Some(expr_id) = self.erlang_type_expr_id {
            expr_id
        } else {
            let atom = Atom::new(module);
            let type_expr_id = self.alloc_type_expr(TypeExpr::Literal(Literal::Atom(atom)), None);
            self.erlang_type_expr_id = Some(type_expr_id);
            type_expr_id
        }
    }

    fn lower_expr_max(&mut self, expr_max: &ast::ExprMax, expr: &ast::Expr) -> ExprId {
        match expr_max {
            ast::ExprMax::AnonymousFun(fun) => {
                let mut name = None;
                let clauses = fun
                    .clauses()
                    .map(|clause| {
                        if let Some(found_name) = clause.name() {
                            name = Some(self.lower_pat(&found_name.into()));
                        }
                        let pats = clause
                            .args()
                            .iter()
                            .flat_map(|args| args.args())
                            .map(|pat| self.lower_pat(&pat))
                            .collect();
                        let guards = self.lower_guards(clause.guard());
                        let exprs = self.lower_clause_body(clause.body());
                        Clause {
                            pats,
                            guards,
                            exprs,
                        }
                    })
                    .collect();
                self.alloc_expr(Expr::Closure { clauses, name }, Some(expr))
            }
            ast::ExprMax::Atom(atom) => {
                let atom = Atom::new(&atom.as_name());
                self.alloc_expr(Expr::Literal(Literal::Atom(atom)), Some(expr))
            }
            ast::ExprMax::Binary(bin) => {
                let segs = bin
                    .elements()
                    .flat_map(|element| {
                        self.lower_bin_element_or_expand_macro(
                            &element,
                            Self::lower_optional_expr,
                            |this, call| this.try_expand_binary_expr_macro(call),
                        )
                    })
                    .collect();
                self.alloc_expr(Expr::Binary { segs }, Some(expr))
            }
            ast::ExprMax::BinaryComprehension(bc) => {
                let value = self.lower_optional_expr(bc.expr().map(Into::into));
                let builder = ComprehensionBuilder::Binary(value);
                let exprs = self.lower_lc_exprs(bc.lc_exprs());
                self.alloc_expr(Expr::Comprehension { builder, exprs }, Some(expr))
            }
            ast::ExprMax::BlockExpr(block) => {
                let exprs = self.lower_body_exprs(block.exprs());
                self.alloc_expr(Expr::Block { exprs }, Some(expr))
            }
            ast::ExprMax::CaseExpr(case) => {
                let value = self.lower_optional_expr(case.expr());
                let clauses = case
                    .clauses()
                    .flat_map(|clause| self.lower_cr_clause(clause))
                    .collect();
                self.alloc_expr(
                    Expr::Case {
                        expr: value,
                        clauses,
                    },
                    Some(expr),
                )
            }
            ast::ExprMax::Char(char) => {
                let value = lower_char(char).map_or(Expr::Missing, Expr::Literal);
                self.alloc_expr(value, Some(expr))
            }
            ast::ExprMax::Concatables(concat) => {
                let value = self
                    .lower_concat_with_macros(concat)
                    .map_or(Expr::Missing, Expr::Literal);
                self.alloc_expr(value, Some(expr))
            }
            ast::ExprMax::ExternalFun(fun) => {
                let target = CallTarget::Remote {
                    module: self.lower_optional_expr(
                        fun.module()
                            .and_then(|module| module.name())
                            .map(Into::into),
                    ),
                    name: self.lower_optional_expr(fun.fun().map(Into::into)),
                    parens: false,
                    unqualified: false,
                };
                let arity = self.lower_optional_expr(
                    fun.arity().and_then(|arity| arity.value()).map(Into::into),
                );
                self.alloc_expr(Expr::CaptureFun { target, arity }, Some(expr))
            }
            ast::ExprMax::Float(float) => {
                let value = lower_float(float).map_or(Expr::Missing, Expr::Literal);
                self.alloc_expr(value, Some(expr))
            }
            ast::ExprMax::FunType(fun) => {
                if let Some(sig) = fun.sig() {
                    let _ = self.lower_optional_expr(sig.ty());
                    sig.args()
                        .iter()
                        .flat_map(|args| args.args())
                        .for_each(|pat| {
                            let _ = self.lower_expr(&pat);
                        });
                }
                self.alloc_expr(Expr::Missing, Some(expr))
            }
            ast::ExprMax::IfExpr(if_expr) => {
                let clauses = if_expr
                    .clauses()
                    .map(|clause| {
                        let guards = self.lower_guards(clause.guard());
                        let exprs = self.lower_clause_body(clause.body());
                        IfClause { guards, exprs }
                    })
                    .collect();
                self.alloc_expr(Expr::If { clauses }, Some(expr))
            }
            ast::ExprMax::Integer(int) => {
                let value = lower_int(int).map_or(Expr::Missing, Expr::Literal);
                self.alloc_expr(value, Some(expr))
            }
            ast::ExprMax::InternalFun(fun) => {
                let arity = fun
                    .arity()
                    .and_then(|arity| arity.value().and_then(|arity| arity.arity_value()));
                let name = self.lower_optional_expr(fun.fun().map(Into::into));
                let target = self.disambiguate_call_target(name, arity);
                let arity = self.lower_optional_expr(
                    fun.arity().and_then(|arity| arity.value()).map(Into::into),
                );
                self.alloc_expr(Expr::CaptureFun { target, arity }, Some(expr))
            }
            ast::ExprMax::List(list) => {
                let (exprs, tail) = self.lower_list(
                    list,
                    |this| this.alloc_expr(Expr::Missing, None),
                    |this, expr| this.lower_expr(expr),
                    |this, call| this.try_expand_list_expr_macro(call),
                );
                self.alloc_expr(Expr::List { exprs, tail }, Some(expr))
            }
            ast::ExprMax::ListComprehension(lc) => {
                let values: Vec<ExprId> = lc.exprs().map(|e| self.lower_expr(&e)).collect();
                let builder = ComprehensionBuilder::List(values);
                let exprs = self.lower_lc_exprs(lc.lc_exprs());
                self.alloc_expr(Expr::Comprehension { builder, exprs }, Some(expr))
            }
            ast::ExprMax::MacroCallExpr(call) => {
                self.resolve_macro(call, |this, source, replacement| match replacement {
                    MacroReplacement::BuiltIn(built_in) => {
                        this.lower_built_in_macro(built_in).map(|literal| {
                            let expr_id = this.alloc_expr(Expr::Literal(literal), None);
                            this.record_expr_source(expr_id, source);
                            (None, expr_id)
                        })
                    }
                    MacroReplacement::Ast(def_idx, ast::MacroDefReplacement::Expr(macro_expr)) => {
                        let expr_id = this.lower_expr(&macro_expr);
                        this.record_expr_source(expr_id, source);
                        Some((Some(def_idx), expr_id))
                    }
                    MacroReplacement::Ast(_, _) => None,
                    MacroReplacement::BuiltInArgs(built_in, args) => {
                        let name = this
                            .lower_built_in_macro(built_in)
                            .map(|literal| this.alloc_expr(Expr::Literal(literal), None))
                            .unwrap_or_else(|| this.alloc_expr(Expr::Missing, None));
                        let target = CallTarget::Local { name };
                        let args = args
                            .args()
                            .map(|expr| this.lower_optional_expr(expr.expr()))
                            .collect();
                        let expr_id = this.alloc_expr(Expr::Call { target, args }, None);
                        this.record_expr_source(expr_id, source);
                        Some((None, expr_id))
                    }
                    MacroReplacement::AstArgs(
                        def_idx,
                        ast::MacroDefReplacement::Expr(replacement),
                        args,
                    ) => {
                        let target = this.lower_call_target(Some(replacement), args.arity_value());
                        let args = args
                            .args()
                            .map(|expr| this.lower_optional_expr(expr.expr()))
                            .collect();
                        let expr_id = this.alloc_expr(Expr::Call { target, args }, None);
                        this.record_expr_source(expr_id, source);
                        Some((Some(def_idx), expr_id))
                    }
                    MacroReplacement::AstArgs(_, _, _) => None,
                })
                .flatten()
                .map(|(macro_def, expansion)| {
                    let args = call
                        .args()
                        .iter()
                        .flat_map(|args| args.args())
                        .map(|expr| self.lower_optional_expr(expr.expr()))
                        .collect();
                    self.alloc_expr(
                        Expr::MacroCall {
                            expansion,
                            args,
                            macro_def,
                            macro_name: self.macro_call_name(call.name()),
                        },
                        Some(expr),
                    )
                })
                .unwrap_or_else(|| {
                    // Record diagnostic for unresolved macro
                    self.record_unresolved_macro(call);

                    let expansion = self.alloc_expr(Expr::Missing, Some(expr));
                    let args = call
                        .args()
                        .iter()
                        .flat_map(|args| args.args())
                        .map(|expr| self.lower_optional_expr(expr.expr()))
                        .collect();
                    self.alloc_expr(
                        Expr::MacroCall {
                            expansion,
                            args,
                            macro_def: None,
                            macro_name: self.macro_call_name(call.name()),
                        },
                        Some(expr),
                    )
                })
            }
            ast::ExprMax::MacroString(ms) => {
                if let Some(lit) = self.resolve_macro_string(ms) {
                    self.alloc_expr(Expr::Literal(lit), Some(expr))
                } else {
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            }
            ast::ExprMax::ParenExpr(paren_expr) => {
                if let Some(inner_expr) = paren_expr.expr() {
                    let expr_id = self.lower_expr(&inner_expr);
                    self.alloc_expr(Expr::Paren { expr: expr_id }, Some(expr))
                } else {
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            }
            ast::ExprMax::ReceiveExpr(receive) => {
                let clauses = receive
                    .clauses()
                    .flat_map(|clause| self.lower_cr_clause(clause))
                    .collect();
                let after = receive.after().map(|after| {
                    let timeout = self.lower_optional_expr(after.expr());
                    let exprs = self.lower_clause_body(after.body());
                    ReceiveAfter { timeout, exprs }
                });
                self.alloc_expr(Expr::Receive { clauses, after }, Some(expr))
            }
            ast::ExprMax::String(str) => {
                let value = self
                    .lower_str_or_sigil(
                        str,
                        expr,
                        Self::lower_binary_string_literal_expr,
                        Expr::Literal,
                    )
                    .unwrap_or(Expr::Missing);
                self.alloc_expr(value, Some(expr))
            }
            ast::ExprMax::TryExpr(try_expr) => {
                let exprs = self.lower_body_exprs(try_expr.exprs());
                let of_clauses = try_expr
                    .clauses()
                    .flat_map(|clause| self.lower_cr_clause(clause))
                    .collect();
                let catch_clauses = try_expr
                    .catch()
                    .map(|clause| {
                        let class = clause
                            .class()
                            .and_then(|class| class.class())
                            .map(|class| self.lower_pat(&class.into()));
                        let reason = self.lower_optional_pat(clause.pat().map(Into::into));
                        let stack = clause
                            .stack()
                            .and_then(|stack| stack.class())
                            .map(|var| self.lower_pat(&ast::Expr::ExprMax(ast::ExprMax::Var(var))));
                        let guards = self.lower_guards(clause.guard());
                        let exprs = self.lower_clause_body(clause.body());
                        CatchClause {
                            class,
                            reason,
                            stack,
                            guards,
                            exprs,
                        }
                    })
                    .collect();
                let after = try_expr
                    .after()
                    .iter()
                    .flat_map(|after| self.lower_body_exprs(after.exprs()))
                    .collect();
                self.alloc_expr(
                    Expr::Try {
                        exprs,
                        of_clauses,
                        catch_clauses,
                        after,
                    },
                    Some(expr),
                )
            }
            ast::ExprMax::Tuple(tup) => {
                let exprs = self.expand_or_lower(
                    tup.expr(),
                    |this, call| this.try_expand_list_expr_macro(call),
                    |this, expr| this.lower_expr(expr),
                );
                self.alloc_expr(Expr::Tuple { exprs }, Some(expr))
            }
            ast::ExprMax::Var(var) => self
                .resolve_var(var, |this, expr| this.lower_optional_expr(expr.expr()))
                .unwrap_or_else(|var| self.alloc_expr(Expr::Var(var), Some(expr))),
            ast::ExprMax::MaybeExpr(maybe) => {
                let exprs = maybe
                    .exprs()
                    .map(|expr| self.lower_maybe_expr(&expr))
                    .collect();

                let else_clauses = maybe
                    .clauses()
                    .flat_map(|clause| self.lower_cr_clause(clause))
                    .collect();
                self.alloc_expr(
                    Expr::Maybe {
                        exprs,
                        else_clauses,
                    },
                    Some(expr),
                )
            }
            ast::ExprMax::MapComprehension(map_comp) => {
                let map_fields: Vec<_> = map_comp.exprs().collect();
                let all_assoc = map_fields
                    .iter()
                    .all(|mf| matches!(mf.op(), Some((MapOp::Assoc, _))));
                if all_assoc {
                    let fields: Vec<(ExprId, ExprId)> = map_fields
                        .iter()
                        .map(|mf| {
                            let key = self.lower_optional_expr(mf.key());
                            let value = self.lower_optional_expr(mf.value());
                            (key, value)
                        })
                        .collect();
                    let exprs = self.lower_lc_exprs(map_comp.lc_exprs());
                    self.alloc_expr(
                        Expr::Comprehension {
                            builder: ComprehensionBuilder::Map(fields),
                            exprs,
                        },
                        Some(expr),
                    )
                } else {
                    // `:=` in map comprehension template is invalid;
                    // the erlang_service will report the error.
                    self.alloc_expr(Expr::Missing, Some(expr))
                }
            }
        }
    }

    fn lower_maybe_expr(&mut self, expr: &ast::Expr) -> MaybeExpr {
        match expr {
            ast::Expr::CondMatchExpr(cond) => {
                let pat_id = self.lower_optional_pat(cond.lhs());
                let expr_id = self.lower_optional_expr(cond.rhs());
                self.alloc_expr(Expr::Missing, Some(expr));
                MaybeExpr::Cond {
                    lhs: pat_id,
                    rhs: expr_id,
                }
            }
            ast::Expr::ExprMax(ast::ExprMax::ParenExpr(paren)) => match paren.expr() {
                // According to EEP49 the `CondMatchExpr` can only
                // occur at the top level. So we lower the
                // paren-wrapped expr as an ordinary one.
                Some(paren_expr) => {
                    let expr_id = self.lower_expr(&paren_expr);
                    let expr_id = self.alloc_expr(Expr::Paren { expr: expr_id }, Some(expr));
                    MaybeExpr::Expr(expr_id)
                }
                None => MaybeExpr::Expr(self.alloc_expr(Expr::Missing, None)),
            },
            e => MaybeExpr::Expr(self.lower_expr(e)),
        }
    }

    fn lower_list<Id>(
        &mut self,
        list: &ast::List,
        make_missing: impl Fn(&mut Self) -> Id,
        lower: impl Fn(&mut Self, &ast::Expr) -> Id,
        try_expand_multi: impl Fn(&mut Self, &ast::MacroCallExpr) -> Option<Vec<Id>>,
    ) -> (Vec<Id>, Option<Id>) {
        let mut tail = None;
        let mut ids = vec![];

        for expr in list.exprs() {
            if let ast::Expr::Pipe(pipe) = &expr {
                if let Some(lhs_expr) = pipe.lhs() {
                    if let ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(ref call)) = lhs_expr
                        && let Some(expanded) = try_expand_multi(self, call)
                    {
                        ids.extend(expanded);
                    } else {
                        ids.push(lower(self, &lhs_expr));
                    }
                } else {
                    ids.push(make_missing(self));
                }

                if let Some(tail) = tail {
                    // TODO: add error
                    ids.push(tail)
                }
                tail = pipe.rhs().map(|expr| lower(self, &expr));
            } else if let ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(ref call)) = expr
                && let Some(expanded) = try_expand_multi(self, call)
            {
                ids.extend(expanded);
            } else {
                ids.push(lower(self, &expr));
            }
        }

        (ids, tail)
    }

    fn lower_bin_element<Id>(
        &mut self,
        element: &ast::BinElement,
        lower: fn(&mut Self, Option<ast::Expr>) -> Id,
    ) -> Option<BinarySeg<Id>> {
        let elem = lower(self, element.element().map(Into::into));
        let size = element
            .size()
            .and_then(|size| size.size())
            .map(|expr| self.lower_expr(&expr.into()));

        let mut unit = None;
        let tys = element
            .types()
            .iter()
            .flat_map(|types| types.types())
            .flat_map(|ty| match ty {
                ast::BitType::Name(name) => self.resolve_name(name).map(|na| na.atom),
                ast::BitType::BitTypeUnit(ty_unit) => {
                    unit = ty_unit.size().and_then(|unit| self.resolve_arity(unit));
                    None
                }
            })
            .collect();

        Some(BinarySeg {
            elem,
            size,
            unit,
            tys,
        })
    }

    fn lower_cr_clause(
        &mut self,
        clause: ast::CrClauseOrMacro,
    ) -> impl Iterator<Item = CRClause> + use<> {
        match clause {
            ast::CrClauseOrMacro::CrClause(clause) => {
                self.in_cr_clause_pattern = true;
                let pat = self.lower_optional_pat(clause.pat());
                self.in_cr_clause_pattern = false;
                let macro_guards = std::mem::take(&mut self.pending_macro_guards);
                let guards = if !macro_guards.is_empty() {
                    // OTP preprocessor does lexical substitution, so
                    // `?MACRO_WITH_GUARD when extra` produces a double
                    // `when` — a syntax error reported by the parser.
                    // Still lower the clause guard for source map coverage.
                    let _ = self.lower_guards(clause.guard());
                    macro_guards
                } else {
                    self.lower_guards(clause.guard())
                };
                let exprs = self.lower_clause_body(clause.body());
                Either::Left(Some(CRClause { pat, guards, exprs }).into_iter())
            }
            ast::CrClauseOrMacro::MacroCallExpr(call) => {
                Either::Right(
                    self.resolve_macro(&call, |this, _source, replacement| {
                        match replacement {
                            MacroReplacement::Ast(
                                _defidx,
                                ast::MacroDefReplacement::ReplacementCrClauses(clauses),
                            ) => clauses
                                .clauses()
                                .flat_map(|clause| this.lower_cr_clause(clause))
                                .collect(),
                            // no built-in macro makes sense in this place
                            MacroReplacement::Ast(_, _) | MacroReplacement::BuiltIn(_) => vec![],
                            // args make no sense here
                            MacroReplacement::AstArgs(_, _, _)
                            | MacroReplacement::BuiltInArgs(_, _) => vec![],
                        }
                    })
                    .into_iter()
                    .flatten(),
                )
            }
        }
    }

    fn lower_guards(&mut self, guards: Option<ast::Guard>) -> Guards {
        guards
            .iter()
            .flat_map(|guard| guard.clauses())
            .flat_map(|clause| self.lower_guard_clause(clause))
            .collect()
    }

    /// Lower a single guard clause, handling macro expansion.
    ///
    /// A guard clause is a comma-separated list of guard expressions.
    /// If any expression is a macro that expands to a `ReplacementGuardOr`,
    /// the single clause is split into multiple clauses using textual
    /// substitution semantics: the first OR branch merges with whatever
    /// preceded the macro, and the last branch receives whatever follows.
    fn lower_guard_clause(&mut self, clause: ast::GuardClause) -> Vec<Vec<ExprId>> {
        let mut result_clauses: Vec<Vec<ExprId>> = vec![vec![]];

        for expr in clause.exprs() {
            if let ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(ref call)) = expr
                && let Some(expanded) = self.try_expand_guard_macro(call)
            {
                let mut iter = expanded.into_iter();
                if let Some(first_branch) = iter.next() {
                    result_clauses.last_mut().unwrap().extend(first_branch);
                    for branch in iter {
                        result_clauses.push(branch);
                    }
                }
                continue;
            }
            let expr_id = self.lower_expr(&expr);
            result_clauses.last_mut().unwrap().push(expr_id);
        }

        result_clauses
    }

    /// Try to expand a macro call as a guard replacement.
    ///
    /// Returns `Some(clauses)` if the macro expands to `ReplacementGuardOr`
    /// or `ReplacementGuardAnd`, where each inner Vec is a conjunction (AND)
    /// and the outer Vec is a disjunction (OR).
    fn lower_guard_and_exprs(&mut self, guard_and: &ast::ReplacementGuardAnd) -> Vec<ExprId> {
        guard_and
            .guard()
            .map(|expr| self.lower_expr(&expr))
            .collect()
    }

    fn try_expand_guard_macro(&mut self, call: &ast::MacroCallExpr) -> Option<Vec<Vec<ExprId>>> {
        self.resolve_macro(call, |this, _source, replacement| match replacement {
            MacroReplacement::Ast(_, ast::MacroDefReplacement::ReplacementGuardOr(guard_or)) => {
                Some(
                    guard_or
                        .guard()
                        .map(|guard_and| this.lower_guard_and_exprs(&guard_and))
                        .collect(),
                )
            }
            MacroReplacement::Ast(_, ast::MacroDefReplacement::ReplacementGuardAnd(guard_and)) => {
                Some(vec![this.lower_guard_and_exprs(&guard_and)])
            }
            // Wrapper macro whose replacement is itself a macro call, e.g.
            //   -define(B(X), ?A(X)).
            //   -define(A(X), X =:= a; X =:= b).
            // `?B(X)` in guard context must recurse into `?A(X)` so the
            // guard-OR/AND structure is preserved rather than falling
            // through to `lower_expr` and producing `Missing`.
            MacroReplacement::Ast(
                _,
                ast::MacroDefReplacement::Expr(ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(
                    ref inner_call,
                ))),
            ) => this.try_expand_guard_macro(inner_call),
            _ => None,
        })
        .flatten()
    }

    fn lower_clause_body(&mut self, body: Option<ast::ClauseBody>) -> Vec<ExprId> {
        self.lower_body_exprs(body.iter().flat_map(|body| body.exprs()))
    }

    /// Lower a sequence of body expressions, expanding macros that produce
    /// multiple comma-separated expressions (`ReplacementGuardAnd`).
    ///
    /// This must be used in any context where expressions are sequenced
    /// (clause bodies, try bodies, try-after blocks, begin...end blocks)
    /// so that macros like `-define(M(X), a(X), b(X)).` correctly expand
    /// to multiple body expressions rather than producing `Expr::Missing`.
    fn lower_body_exprs(&mut self, exprs: impl Iterator<Item = ast::Expr>) -> Vec<ExprId> {
        exprs
            .flat_map(|expr| {
                if let ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(ref call)) = expr
                    && let Some(exprs) = self.try_expand_body_macro(call)
                {
                    return exprs;
                }
                vec![self.lower_expr(&expr)]
            })
            .collect()
    }

    /// Try to expand a macro call in clause body context as multiple expressions.
    ///
    /// When a macro expands to `ReplacementGuardAnd` (comma-separated expressions),
    /// each sub-expression is lowered individually and returned as separate body
    /// expressions. This matches the Erlang preprocessor behavior where macro
    /// expansion happens before parsing, so comma-separated expressions in a macro
    /// body become individual clause body expressions.
    ///
    /// Also handles the case where a macro expands to another macro call
    /// (single `Expr`) whose own expansion is `ReplacementGuardAnd`.
    /// This occurs with wrapper macros like:
    ///   -define(OUTER(X), ?INNER("prefix", X)).
    ///   -define(INNER(A, B), expr1(A), expr2(B)).
    /// where `?OUTER(Y)` in a clause body should produce two separate
    /// body expressions, not a single `missing` atom.
    fn try_expand_body_macro(&mut self, call: &ast::MacroCallExpr) -> Option<Vec<ExprId>> {
        self.resolve_macro(call, |this, _source, replacement| match replacement {
            MacroReplacement::Ast(_, ast::MacroDefReplacement::ReplacementGuardAnd(guard_and)) => {
                Some(this.lower_body_exprs(guard_and.guard()))
            }
            MacroReplacement::Ast(
                _,
                ast::MacroDefReplacement::Expr(ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(
                    ref inner_call,
                ))),
            ) => this.try_expand_body_macro(inner_call),
            _ => None,
        })
        .flatten()
    }

    /// Try to expand a macro call that appears as an element of a tuple type
    /// into multiple type expressions.
    ///
    /// Handles macros like `-define(VER5, xx:version(), xx:version(), ...).`
    /// whose body is a comma-separated list of types parsed as
    /// `ReplacementGuardAnd`. When used inside a tuple type like
    /// `{atom(), ?VER5, binary()}`, the macro should splice its elements
    /// into the tuple rather than producing a single `any()`.
    fn try_expand_type_tuple_macro(
        &mut self,
        call: &ast::MacroCallExpr,
    ) -> Option<Vec<TypeExprId>> {
        self.resolve_macro(call, |this, _source, replacement| match replacement {
            MacroReplacement::Ast(_, ast::MacroDefReplacement::ReplacementGuardAnd(guard_and)) => {
                Some(
                    guard_and
                        .guard()
                        .flat_map(|expr| {
                            if let ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(ref inner_call)) =
                                expr
                                && let Some(types) = this.try_expand_type_tuple_macro(inner_call)
                            {
                                return types;
                            }
                            vec![this.lower_type_expr(&expr)]
                        })
                        .collect::<Vec<_>>(),
                )
            }
            MacroReplacement::Ast(
                _,
                ast::MacroDefReplacement::Expr(ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(
                    ref inner_call,
                ))),
            ) => this.try_expand_type_tuple_macro(inner_call),
            _ => None,
        })
        .flatten()
    }

    /// Splice macro elements into a comma-separated sequence.
    ///
    /// For each element, if it is a macro call that `try_expand` knows how to
    /// expand into multiple items, splice those items in; otherwise `lower` the
    /// element into a single item. Shared by the tuple/list pattern and
    /// expression lowering paths.
    fn expand_or_lower<T>(
        &mut self,
        exprs: impl Iterator<Item = ast::Expr>,
        try_expand: impl Fn(&mut Self, &ast::MacroCallExpr) -> Option<Vec<T>>,
        lower: impl Fn(&mut Self, &ast::Expr) -> T,
    ) -> Vec<T> {
        exprs
            .flat_map(|expr| {
                if let ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(ref call)) = expr
                    && let Some(items) = try_expand(self, call)
                {
                    return items;
                }
                vec![lower(self, &expr)]
            })
            .collect()
    }

    /// Try to expand a macro call that appears as an element of a list pattern
    /// into multiple patterns.
    ///
    /// Handles macros like `-define(ELEMS, {M1, F1, A1}, {M2, F2, A2}).`
    /// whose body is a comma-separated list of patterns parsed as
    /// `ReplacementGuardAnd`. When used inside a list pattern like
    /// `[?ELEMS, ?ELEMS | Stack]`, the macro should splice its elements
    /// into the list rather than producing a single `_` (missing).
    fn try_expand_list_pat_macro(&mut self, call: &ast::MacroCallExpr) -> Option<Vec<PatId>> {
        self.resolve_macro(call, |this, _source, replacement| match replacement {
            MacroReplacement::Ast(_, ast::MacroDefReplacement::ReplacementGuardAnd(guard_and)) => {
                Some(this.expand_or_lower(
                    guard_and.guard(),
                    |this, call| this.try_expand_list_pat_macro(call),
                    |this, expr| this.lower_pat(expr),
                ))
            }
            MacroReplacement::Ast(
                _,
                ast::MacroDefReplacement::Expr(ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(
                    ref inner_call,
                ))),
            ) => this.try_expand_list_pat_macro(inner_call),
            _ => None,
        })
        .flatten()
    }

    /// Try to expand a macro call that appears as an element of a list expression
    /// into multiple expressions.
    ///
    /// Handles macros like `-define(ELEMS, expr1, expr2, expr3).`
    /// whose body is a comma-separated list of expressions parsed as
    /// `ReplacementGuardAnd`. When used inside a list expression like
    /// `[?ELEMS | Tail]`, the macro should splice its elements
    /// into the list rather than producing a single `missing` atom.
    fn try_expand_list_expr_macro(&mut self, call: &ast::MacroCallExpr) -> Option<Vec<ExprId>> {
        self.resolve_macro(call, |this, _source, replacement| match replacement {
            MacroReplacement::Ast(_, ast::MacroDefReplacement::ReplacementGuardAnd(guard_and)) => {
                Some(this.expand_or_lower(
                    guard_and.guard(),
                    |this, call| this.try_expand_list_expr_macro(call),
                    |this, expr| this.lower_expr(expr),
                ))
            }
            MacroReplacement::Ast(
                _,
                ast::MacroDefReplacement::Expr(ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(
                    ref inner_call,
                ))),
            ) => this.try_expand_list_expr_macro(inner_call),
            _ => None,
        })
        .flatten()
    }

    /// Try to expand a macro call that appears as a binary element
    /// into multiple binary segments (patterns).
    ///
    /// Handles macros like `-define(PREFIX, 13,10,13,10).`
    /// whose body is a comma-separated list of expressions parsed as
    /// `ReplacementGuardAnd`. When used inside a binary pattern like
    /// `<<?PREFIX, _/binary>>`, the macro should splice its
    /// elements into the binary as separate segments rather than
    /// producing a single `_` (missing).
    fn try_expand_binary_pat_macro(
        &mut self,
        call: &ast::MacroCallExpr,
    ) -> Option<Vec<BinarySeg<PatId>>> {
        self.resolve_macro(call, |this, _source, replacement| match replacement {
            MacroReplacement::Ast(_, ast::MacroDefReplacement::ReplacementGuardAnd(guard_and)) => {
                Some(
                    guard_and
                        .guard()
                        .flat_map(|expr| {
                            if let ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(ref inner_call)) =
                                expr
                                && let Some(segs) = this.try_expand_binary_pat_macro(inner_call)
                            {
                                return segs;
                            }
                            vec![BinarySeg {
                                elem: this.lower_pat(&expr),
                                size: None,
                                tys: vec![],
                                unit: None,
                            }]
                        })
                        .collect::<Vec<_>>(),
                )
            }
            MacroReplacement::Ast(
                _,
                ast::MacroDefReplacement::Expr(ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(
                    ref inner_call,
                ))),
            ) => this.try_expand_binary_pat_macro(inner_call),
            _ => None,
        })
        .flatten()
    }

    /// Try to expand a macro call that appears as a binary element
    /// into multiple binary segments (expressions).
    fn try_expand_binary_expr_macro(
        &mut self,
        call: &ast::MacroCallExpr,
    ) -> Option<Vec<BinarySeg<ExprId>>> {
        self.resolve_macro(call, |this, _source, replacement| match replacement {
            MacroReplacement::Ast(_, ast::MacroDefReplacement::ReplacementGuardAnd(guard_and)) => {
                Some(
                    guard_and
                        .guard()
                        .flat_map(|expr| {
                            if let ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(ref inner_call)) =
                                expr
                                && let Some(segs) = this.try_expand_binary_expr_macro(inner_call)
                            {
                                return segs;
                            }
                            vec![BinarySeg {
                                elem: this.lower_expr(&expr),
                                size: None,
                                tys: vec![],
                                unit: None,
                            }]
                        })
                        .collect::<Vec<_>>(),
                )
            }
            MacroReplacement::Ast(
                _,
                ast::MacroDefReplacement::Expr(ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(
                    ref inner_call,
                ))),
            ) => this.try_expand_binary_expr_macro(inner_call),
            _ => None,
        })
        .flatten()
    }

    /// Lower a binary element, handling macros that expand to multiple
    /// binary segments (e.g., `-define(PREFIX, 13,10,13,10).` used as
    /// `<<?PREFIX, Rest/binary>>`).
    ///
    /// When the element is a macro call that expands to comma-separated
    /// values and has no size/type specifiers, the macro's elements are
    /// spliced as individual segments. Otherwise, falls through to
    /// the standard single-element lowering.
    fn lower_bin_element_or_expand_macro<Id>(
        &mut self,
        element: &ast::BinElement,
        lower: fn(&mut Self, Option<ast::Expr>) -> Id,
        try_expand_multi: impl Fn(&mut Self, &ast::MacroCallExpr) -> Option<Vec<BinarySeg<Id>>>,
    ) -> Vec<BinarySeg<Id>> {
        // Only attempt macro expansion if the element has no size or type
        // specifiers (they can't meaningfully apply to multiple segments).
        if element.size().is_none()
            && element.types().is_none()
            && let Some(ast::BitExpr::ExprMax(ast::ExprMax::MacroCallExpr(ref call))) =
                element.element()
            && let Some(expanded) = try_expand_multi(self, call)
        {
            return expanded;
        }
        self.lower_bin_element(element, lower).into_iter().collect()
    }

    fn lower_lc_exprs(&mut self, exprs: Option<ast::LcExprs>) -> Vec<ComprehensionExpr> {
        // If the LcExprs has more than one child, it is a zip generator
        exprs
            .iter()
            .flat_map(|exprs| exprs.exprs())
            .map(|lc_or_zc_expr| {
                let mut exprs: Vec<_> = lc_or_zc_expr
                    .exprs()
                    .map(|lc_expr| self.lower_lc_expr(lc_expr))
                    .collect();
                if exprs.len() == 1 {
                    exprs.remove(0)
                } else {
                    ComprehensionExpr::Zip(exprs)
                }
            })
            .collect()
    }

    fn lower_lc_expr(&mut self, expr: ast::LcExpr) -> ComprehensionExpr {
        match expr {
            ast::LcExpr::Expr(expr) => ComprehensionExpr::Expr(self.lower_expr(&expr)),
            ast::LcExpr::BGenerator(bin_gen) => {
                let pat = self.lower_optional_pat(bin_gen.lhs());
                let expr = self.lower_optional_expr(bin_gen.rhs());
                ComprehensionExpr::BinGenerator {
                    pat,
                    expr,
                    strict: bin_gen.strict(),
                }
            }
            ast::LcExpr::Generator(list_gen) => {
                let pat = self.lower_optional_pat(list_gen.lhs());
                let expr = self.lower_optional_expr(list_gen.rhs());
                ComprehensionExpr::ListGenerator {
                    pat,
                    expr,
                    strict: list_gen.strict(),
                }
            }
            ast::LcExpr::MapGenerator(map_gen) => {
                let key = self.lower_optional_pat(map_gen.lhs().and_then(|mf| mf.key()));
                let value = self.lower_optional_pat(map_gen.lhs().and_then(|mf| mf.value()));
                let expr = self.lower_optional_expr(map_gen.rhs());
                ComprehensionExpr::MapGenerator {
                    key,
                    value,
                    expr,
                    strict: map_gen.strict(),
                }
            }
        }
    }

    fn lower_optional_type_expr(&mut self, expr: Option<ast::Expr>) -> TypeExprId {
        if let Some(expr) = &expr {
            self.lower_type_expr(expr)
        } else {
            self.alloc_type_expr(TypeExpr::Missing, None)
        }
    }

    fn lower_type_expr(&mut self, expr: &ast::Expr) -> TypeExprId {
        match expr {
            ast::Expr::ExprMax(expr_max) => self.lower_type_expr_max(expr_max, expr),
            ast::Expr::AnnType(ann) => {
                let ty = self.lower_optional_type_expr(ann.ty());
                if let Some(var) = ann.var().and_then(|var| var.var()) {
                    let var = Var::new(&var.as_name());
                    self.alloc_type_expr(TypeExpr::AnnType { var, ty }, Some(expr))
                } else {
                    self.alloc_type_expr(TypeExpr::Missing, Some(expr))
                }
            }
            ast::Expr::BinaryOpExpr(binary_op) => {
                let lhs = self.lower_optional_type_expr(binary_op.lhs());
                let rhs = self.lower_optional_type_expr(binary_op.rhs());
                if let Some((op, _)) = binary_op.op() {
                    self.alloc_type_expr(TypeExpr::BinaryOp { lhs, op, rhs }, Some(expr))
                } else {
                    self.alloc_type_expr(TypeExpr::Missing, Some(expr))
                }
            }
            ast::Expr::Call(call) => {
                let target = self.lower_type_call_target(call.expr(), call.arity_value());
                let args = call
                    .args()
                    .iter()
                    .flat_map(|args| args.args())
                    .map(|expr| self.lower_type_expr(&expr))
                    .collect();
                self.alloc_type_expr(TypeExpr::Call { target, args }, Some(expr))
            }
            ast::Expr::CatchExpr(catch) => {
                let _ = self.lower_optional_type_expr(catch.expr());
                self.alloc_type_expr(TypeExpr::Missing, Some(expr))
            }
            ast::Expr::Dotdotdot(_) => self.alloc_type_expr(TypeExpr::Missing, Some(expr)),
            ast::Expr::MapExpr(map) => {
                let fields = map
                    .fields()
                    .flat_map(|field| {
                        let key = self.lower_optional_type_expr(field.key());
                        let value = self.lower_optional_type_expr(field.value());
                        Some((key, field.op()?.0, value))
                    })
                    .collect();
                self.alloc_type_expr(TypeExpr::Map { fields }, Some(expr))
            }
            ast::Expr::MapExprUpdate(update) => {
                let _ = self.lower_optional_type_expr(update.expr().map(Into::into));
                update.fields().for_each(|field| {
                    let _ = self.lower_optional_type_expr(field.key());
                    let _ = self.lower_optional_type_expr(field.value());
                });
                self.alloc_type_expr(TypeExpr::Missing, Some(expr))
            }
            ast::Expr::MatchExpr(mat) => {
                let _ = self.lower_optional_type_expr(mat.lhs());
                let _ = self.lower_optional_type_expr(mat.rhs());
                self.alloc_type_expr(TypeExpr::Missing, Some(expr))
            }
            ast::Expr::Pipe(pipe) => {
                let mut pipe = pipe.clone();
                let mut types = vec![self.lower_optional_type_expr(pipe.lhs())];
                while let Some(ast::Expr::Pipe(next)) = pipe.rhs() {
                    types.push(self.lower_optional_type_expr(next.lhs()));
                    pipe = next;
                }
                types.push(self.lower_optional_type_expr(pipe.rhs()));
                self.alloc_type_expr(TypeExpr::Union { types }, Some(expr))
            }
            ast::Expr::RangeType(range) => {
                let lhs = self.lower_optional_type_expr(range.lhs());
                let rhs = self.lower_optional_type_expr(range.rhs());
                self.alloc_type_expr(TypeExpr::Range { lhs, rhs }, Some(expr))
            }
            ast::Expr::RecordExpr(record) => {
                let name = record.name().and_then(|n| self.resolve_name(n.name()?));
                let fields = self.lower_record_fields_type(record.fields());
                if let Some(name) = name {
                    self.alloc_type_expr(TypeExpr::Record { name, fields }, Some(expr))
                } else {
                    self.alloc_type_expr(TypeExpr::Missing, Some(expr))
                }
            }
            ast::Expr::RecordFieldExpr(field) => {
                let _ = self.lower_optional_type_expr(field.expr().map(Into::into));
                self.alloc_type_expr(TypeExpr::Missing, Some(expr))
            }
            ast::Expr::RecordIndexExpr(_index) => {
                self.alloc_type_expr(TypeExpr::Missing, Some(expr))
            }
            ast::Expr::RecordUpdateExpr(update) => {
                let _ = self.lower_optional_type_expr(update.expr().map(Into::into));
                update.fields().for_each(|field| {
                    field.expr().iter().for_each(|field_expr| {
                        self.lower_optional_type_expr(field_expr.expr());
                    });
                });
                self.alloc_type_expr(TypeExpr::Missing, Some(expr))
            }
            ast::Expr::Remote(remote) => match remote.fun() {
                Some(ast::Expr::Call(call)) => {
                    let module = self.lower_optional_type_expr(
                        remote.module().and_then(|module| module.module()),
                    );
                    let name_target = self.lower_type_call_target(call.expr(), call.arity_value());
                    let target = match name_target {
                        CallTarget::Local { name } => CallTarget::Remote {
                            module,
                            name,
                            parens: false,
                            unqualified: false,
                        },
                        CallTarget::Remote { name, parens, .. } => {
                            // Nested remote: the inner call resolved via
                            // BIF/import inference, but we have an explicit
                            // outer module qualifier here, so the resulting
                            // call is NOT unqualified.
                            CallTarget::Remote {
                                module,
                                name,
                                parens,
                                unqualified: false,
                            }
                        }
                    };
                    let args = call
                        .args()
                        .iter()
                        .flat_map(|args| args.args())
                        .map(|expr| self.lower_type_expr(&expr))
                        .collect();
                    let type_expr_id =
                        self.alloc_type_expr(TypeExpr::Call { target, args }, Some(expr));
                    // Also record the inner Call node in the forward source map
                    let call_ast = ast::Expr::from(call);
                    let call_ptr = AstPtr::new(&call_ast);
                    let call_source = InFileAstPtr::new(self.curr_file_id(), call_ptr);
                    self.source_map
                        .type_expr_map
                        .insert(call_source, type_expr_id);
                    type_expr_id
                }
                _ => {
                    let _ = self.lower_optional_type_expr(
                        remote.module().and_then(|module| module.module()),
                    );
                    let _ = self.lower_optional_type_expr(remote.fun());
                    self.alloc_type_expr(TypeExpr::Missing, Some(expr))
                }
            },
            ast::Expr::UnaryOpExpr(unary_op) => {
                let operand = self.lower_optional_type_expr(unary_op.operand());
                if let Some((op, _)) = unary_op.op() {
                    self.alloc_type_expr(
                        TypeExpr::UnaryOp {
                            type_expr: operand,
                            op,
                        },
                        Some(expr),
                    )
                } else {
                    self.alloc_type_expr(TypeExpr::Missing, Some(expr))
                }
            }
            ast::Expr::CondMatchExpr(cond) => {
                let _ = self.lower_optional_type_expr(cond.lhs());
                let _ = self.lower_optional_type_expr(cond.rhs());
                self.alloc_type_expr(TypeExpr::Missing, Some(expr))
            }
            ast::Expr::AnonRecordExpr(record) => {
                let fields = self.lower_record_fields_type(record.fields());
                self.alloc_type_expr(
                    TypeExpr::NativeRecord {
                        name: NativeRecordName::Anon,
                        fields,
                    },
                    Some(expr),
                )
            }
            ast::Expr::AnonRecordFieldExpr(_) | ast::Expr::AnonRecordUpdateExpr(_) => {
                self.alloc_type_expr(TypeExpr::Missing, Some(expr))
            }
            ast::Expr::QualifiedRecordExpr(record) => {
                let name = record
                    .name()
                    .and_then(|n| self.lower_qualified_record_name(&n));
                let fields = self.lower_record_fields_type(record.fields());
                if let Some(name) = name {
                    self.alloc_type_expr(TypeExpr::NativeRecord { name, fields }, Some(expr))
                } else {
                    self.alloc_type_expr(TypeExpr::Missing, Some(expr))
                }
            }
            ast::Expr::QualifiedRecordFieldExpr(_) | ast::Expr::QualifiedRecordUpdateExpr(_) => {
                self.alloc_type_expr(TypeExpr::Missing, Some(expr))
            }
        }
    }

    fn lower_type_call_target(
        &mut self,
        expr: Option<ast::Expr>,
        arity: ast::Arity,
    ) -> CallTarget<TypeExprId> {
        match expr.as_ref() {
            Some(ast::Expr::ExprMax(ast::ExprMax::ParenExpr(paren))) => {
                self.lower_type_call_target(paren.expr(), arity)
            }
            Some(ast::Expr::Remote(remote)) => CallTarget::Remote {
                module: self
                    .lower_optional_type_expr(remote.module().and_then(|module| module.module())),
                name: self.lower_optional_type_expr(remote.fun()),
                parens: false,
                unqualified: false,
            },
            Some(ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(call))) => self
                .resolve_macro(call, |this, source, replacement| match replacement {
                    MacroReplacement::BuiltIn(built_in) => {
                        this.lower_built_in_macro(built_in).map(|literal| {
                            let name = this.alloc_type_expr(TypeExpr::Literal(literal), None);
                            this.record_type_source(name, source);
                            CallTarget::Local { name }
                        })
                    }
                    MacroReplacement::Ast(_, ast::MacroDefReplacement::Expr(expr)) => {
                        Some(this.lower_type_call_target(Some(expr), arity))
                    }
                    MacroReplacement::Ast(_, _) => None,
                    // This would mean double parens in the call - invalid
                    MacroReplacement::BuiltInArgs(_, _) | MacroReplacement::AstArgs(_, _, _) => {
                        None
                    }
                })
                .flatten()
                .unwrap_or_else(|| {
                    // Record diagnostic for unresolved macro
                    self.record_unresolved_macro(call);

                    call.args()
                        .iter()
                        .flat_map(|args| args.args())
                        .for_each(|expr| {
                            let _ = self.lower_optional_type_expr(expr.expr());
                            let _ = self.lower_optional_type_expr(expr.guard());
                        });
                    CallTarget::Local {
                        name: self.alloc_type_expr(TypeExpr::Missing, expr.as_ref()),
                    }
                }),
            Some(expr) => {
                let name = self.lower_type_expr(expr);
                self.disambiguate_type_call_target(name, arity)
            }
            None => CallTarget::Local {
                name: self.alloc_type_expr(TypeExpr::Missing, None),
            },
        }
    }

    fn disambiguate_type_call_target(
        &mut self,
        name: TypeExprId,
        arity: ast::Arity,
    ) -> CallTarget<TypeExprId> {
        if let Some(module) = self.erlang_bif_type_module(name, arity) {
            CallTarget::Remote {
                module,
                name,
                parens: false,
                unqualified: true,
            }
        } else {
            CallTarget::Local { name }
        }
    }

    fn erlang_bif_type_module(
        &mut self,
        name_expr_id: TypeExprId,
        arity: ast::Arity,
    ) -> Option<TypeExprId> {
        let atom = self.body[name_expr_id].as_atom()?;
        if is_erlang_type(&atom.as_string(), arity?) {
            Some(self.erlang_type_expr_id())
        } else {
            None
        }
    }

    fn lower_type_expr_max(&mut self, expr_max: &ast::ExprMax, expr: &ast::Expr) -> TypeExprId {
        match expr_max {
            ast::ExprMax::AnonymousFun(_fun) => self.alloc_type_expr(TypeExpr::Missing, Some(expr)),
            ast::ExprMax::Atom(atom) => {
                let atom = Atom::new(&atom.as_name());
                self.alloc_type_expr(TypeExpr::Literal(Literal::Atom(atom)), Some(expr))
            }
            ast::ExprMax::Binary(bin) => {
                let (m, n) = lower_binary_type(bin);
                self.alloc_type_expr(TypeExpr::Bitstring { m, n }, Some(expr))
            }
            ast::ExprMax::BinaryComprehension(_bc) => {
                self.alloc_type_expr(TypeExpr::Missing, Some(expr))
            }
            ast::ExprMax::BlockExpr(_block) => self.alloc_type_expr(TypeExpr::Missing, Some(expr)),
            ast::ExprMax::CaseExpr(_case) => self.alloc_type_expr(TypeExpr::Missing, Some(expr)),
            ast::ExprMax::Char(char) => {
                let value = lower_char(char).map_or(TypeExpr::Missing, TypeExpr::Literal);
                self.alloc_type_expr(value, Some(expr))
            }
            ast::ExprMax::Concatables(_concat) => {
                self.alloc_type_expr(TypeExpr::Missing, Some(expr))
            }
            ast::ExprMax::ExternalFun(_fun) => self.alloc_type_expr(TypeExpr::Missing, Some(expr)),
            ast::ExprMax::Float(float) => {
                let value = lower_float(float).map_or(TypeExpr::Missing, TypeExpr::Literal);
                self.alloc_type_expr(value, Some(expr))
            }
            ast::ExprMax::FunType(fun) => match fun.sig() {
                None => self.alloc_type_expr(TypeExpr::Fun(FunType::Any), Some(expr)),
                Some(sig) => {
                    let result = self.lower_optional_type_expr(sig.ty());
                    let mut params = Vec::new();
                    let has_dot_dot_dot =
                        sig.args().iter().flat_map(|args| args.args()).any(|param| {
                            params.push(self.lower_type_expr(&param));
                            matches!(param, ast::Expr::Dotdotdot(_))
                        });
                    let fun = if has_dot_dot_dot {
                        FunType::AnyArgs { result }
                    } else {
                        FunType::Full { params, result }
                    };
                    self.alloc_type_expr(TypeExpr::Fun(fun), Some(expr))
                }
            },
            ast::ExprMax::IfExpr(_if_expr) => self.alloc_type_expr(TypeExpr::Missing, Some(expr)),
            ast::ExprMax::Integer(int) => {
                let value = lower_int(int).map_or(TypeExpr::Missing, TypeExpr::Literal);
                self.alloc_type_expr(value, Some(expr))
            }
            ast::ExprMax::InternalFun(_fun) => self.alloc_type_expr(TypeExpr::Missing, Some(expr)),
            ast::ExprMax::List(list) => {
                let ty = list.exprs().fold(ListType::Empty, |ty, expr| {
                    let elem = self.lower_type_expr(&expr);
                    match ty {
                        ListType::Empty => ListType::Regular(elem),
                        ListType::Regular(elem) if matches!(expr, ast::Expr::Dotdotdot(_)) => {
                            ListType::NonEmpty(elem)
                        }
                        other => other,
                    }
                });
                self.alloc_type_expr(TypeExpr::List(ty), Some(expr))
            }
            ast::ExprMax::ListComprehension(_lc) => {
                self.alloc_type_expr(TypeExpr::Missing, Some(expr))
            }
            ast::ExprMax::MacroCallExpr(call) => self
                .resolve_macro(call, |this, source, replacement| match replacement {
                    MacroReplacement::BuiltIn(built_in) => {
                        this.lower_built_in_macro(built_in).map(|literal| {
                            let type_id = this.alloc_type_expr(TypeExpr::Literal(literal), None);
                            this.record_type_source(type_id, source);
                            (None, type_id)
                        })
                    }
                    MacroReplacement::Ast(def_idx, ast::MacroDefReplacement::Expr(macro_expr)) => {
                        let type_id = this.lower_type_expr(&macro_expr);
                        this.record_type_source(type_id, source);
                        Some((Some(def_idx), type_id))
                    }
                    MacroReplacement::Ast(_, _) => None,
                    MacroReplacement::BuiltInArgs(built_in, args) => {
                        let name = this
                            .lower_built_in_macro(built_in)
                            .map(|literal| this.alloc_type_expr(TypeExpr::Literal(literal), None))
                            .unwrap_or_else(|| this.alloc_type_expr(TypeExpr::Missing, None));
                        let target = CallTarget::Local { name };
                        let args = args
                            .args()
                            .map(|expr| this.lower_optional_type_expr(expr.expr()))
                            .collect();
                        let type_id = this.alloc_type_expr(TypeExpr::Call { target, args }, None);
                        this.record_type_source(type_id, source);
                        Some((None, type_id))
                    }
                    MacroReplacement::AstArgs(
                        def_idx,
                        ast::MacroDefReplacement::Expr(replacement),
                        args,
                    ) => {
                        let target =
                            this.lower_type_call_target(Some(replacement), args.arity_value());
                        let args = args
                            .args()
                            .map(|expr| this.lower_optional_type_expr(expr.expr()))
                            .collect();
                        let type_id = this.alloc_type_expr(TypeExpr::Call { target, args }, None);
                        this.record_type_source(type_id, source);
                        Some((Some(def_idx), type_id))
                    }
                    MacroReplacement::AstArgs(_, _, _) => None,
                })
                .flatten()
                .map(|(macro_def, expansion)| {
                    let args = call
                        .args()
                        .iter()
                        .flat_map(|args| args.args())
                        .map(|expr| self.lower_optional_type_expr(expr.expr()))
                        .collect();
                    self.alloc_type_expr(
                        TypeExpr::MacroCall {
                            expansion,
                            args,
                            macro_def,
                            macro_name: self.macro_call_name(call.name()),
                        },
                        Some(expr),
                    )
                })
                .unwrap_or_else(|| {
                    // Record diagnostic for unresolved macro
                    self.record_unresolved_macro(call);

                    let expansion = self.alloc_type_expr(TypeExpr::Missing, Some(expr));
                    let args = call
                        .args()
                        .iter()
                        .flat_map(|args| args.args())
                        .map(|expr| self.lower_optional_type_expr(expr.expr()))
                        .collect();
                    self.alloc_type_expr(
                        TypeExpr::MacroCall {
                            expansion,
                            args,
                            macro_def: None,
                            macro_name: self.macro_call_name(call.name()),
                        },
                        Some(expr),
                    )
                }),
            ast::ExprMax::MacroString(_) => self.alloc_type_expr(TypeExpr::Missing, Some(expr)),
            ast::ExprMax::ParenExpr(paren_expr) => {
                if let Some(inner_expr) = paren_expr.expr() {
                    let type_expr_id = self.lower_type_expr(&inner_expr);
                    self.alloc_type_expr(TypeExpr::Paren { ty: type_expr_id }, Some(expr))
                } else {
                    self.alloc_type_expr(TypeExpr::Missing, Some(expr))
                }
            }
            ast::ExprMax::ReceiveExpr(_receive) => {
                self.alloc_type_expr(TypeExpr::Missing, Some(expr))
            }
            ast::ExprMax::String(str) => {
                let value = self
                    .lower_str_or_sigil(
                        str,
                        expr,
                        Self::lower_binary_string_literal_type_expr,
                        TypeExpr::Literal,
                    )
                    .unwrap_or(TypeExpr::Missing);
                self.alloc_type_expr(value, Some(expr))
            }
            ast::ExprMax::TryExpr(_try_expr) => self.alloc_type_expr(TypeExpr::Missing, Some(expr)),
            ast::ExprMax::Tuple(tup) => {
                let args = tup
                    .expr()
                    .flat_map(|expr| {
                        if let ast::Expr::ExprMax(ast::ExprMax::MacroCallExpr(ref call)) = expr
                            && let Some(types) = self.try_expand_type_tuple_macro(call)
                        {
                            return types;
                        }
                        vec![self.lower_type_expr(&expr)]
                    })
                    .collect();
                self.alloc_type_expr(TypeExpr::Tuple { args }, Some(expr))
            }
            ast::ExprMax::Var(var) => self
                .resolve_var(var, |this, expr| this.lower_optional_type_expr(expr.expr()))
                .unwrap_or_else(|var| self.alloc_type_expr(TypeExpr::Var(var), Some(expr))),
            ast::ExprMax::MaybeExpr(maybe) => {
                maybe.exprs().for_each(|expr| {
                    self.lower_expr(&expr);
                });

                maybe
                    .clauses()
                    .flat_map(|clause| self.lower_cr_clause(clause))
                    .last();
                self.alloc_type_expr(TypeExpr::Missing, Some(expr))
            }
            ExprMax::MapComprehension(_mc) => self.alloc_type_expr(TypeExpr::Missing, Some(expr)),
        }
    }

    fn lower_optional_term(&mut self, expr: Option<ast::Expr>) -> TermId {
        if let Some(expr) = &expr {
            self.lower_term(expr)
        } else {
            self.alloc_term(Term::Missing, None)
        }
    }

    fn lower_term(&mut self, expr: &ast::Expr) -> TermId {
        match expr {
            ast::Expr::ExprMax(expr_max) => self.lower_term_max(expr_max, expr),
            ast::Expr::AnnType(ann) => {
                let _ = self.lower_optional_term(ann.ty());
                self.alloc_term(Term::Missing, Some(expr))
            }
            ast::Expr::BinaryOpExpr(binary_op) => {
                // Interpreting foo/1 as {foo, 1}
                let lhs = self.lower_optional_term(binary_op.lhs());
                let rhs = self.lower_optional_term(binary_op.rhs());
                if matches!(
                    binary_op.op(),
                    Some((ast::BinaryOp::ArithOp(ast::ArithOp::FloatDiv), _))
                ) && matches!(self.body[lhs], Term::Literal(Literal::Atom(_)))
                    && matches!(self.body[rhs], Term::Literal(Literal::Integer(_)))
                {
                    let exprs = vec![lhs, rhs];
                    self.alloc_term(Term::Tuple { exprs }, Some(expr))
                } else {
                    self.alloc_term(Term::Missing, Some(expr))
                }
            }
            ast::Expr::Call(call) => {
                let _ = self.lower_optional_term(call.expr());
                call.args()
                    .iter()
                    .flat_map(|args| args.args())
                    .for_each(|expr| {
                        let _ = self.lower_term(&expr);
                    });
                self.alloc_term(Term::Missing, Some(expr))
            }
            ast::Expr::CatchExpr(catch) => {
                let _ = self.lower_optional_term(catch.expr());
                self.alloc_term(Term::Missing, Some(expr))
            }
            ast::Expr::Dotdotdot(_) => self.alloc_term(Term::Missing, Some(expr)),
            ast::Expr::MapExpr(map) => {
                let fields = map
                    .fields()
                    .flat_map(|field| {
                        let key = self.lower_optional_term(field.key());
                        let value = self.lower_optional_term(field.value());
                        if let Some((ast::MapOp::Assoc, _)) = field.op() {
                            Some((key, value))
                        } else {
                            None
                        }
                    })
                    .collect();
                self.alloc_term(Term::Map { fields }, Some(expr))
            }
            ast::Expr::MapExprUpdate(update) => {
                let _ = self.lower_optional_term(update.expr().map(Into::into));
                update.fields().for_each(|field| {
                    let _ = self.lower_optional_term(field.key());
                    let _ = self.lower_optional_term(field.value());
                });
                self.alloc_term(Term::Missing, Some(expr))
            }
            ast::Expr::MatchExpr(mat) => {
                let _ = self.lower_optional_term(mat.lhs());
                let _ = self.lower_optional_term(mat.rhs());
                self.alloc_term(Term::Missing, Some(expr))
            }
            ast::Expr::Pipe(pipe) => {
                let _ = self.lower_optional_term(pipe.lhs());
                let _ = self.lower_optional_term(pipe.rhs());
                self.alloc_term(Term::Missing, Some(expr))
            }
            ast::Expr::RangeType(range) => {
                let _ = self.lower_optional_term(range.lhs());
                let _ = self.lower_optional_term(range.rhs());
                self.alloc_term(Term::Missing, Some(expr))
            }
            ast::Expr::RecordExpr(record) => {
                record.fields().for_each(|field| {
                    let _ = self.lower_optional_term(field.ty().and_then(|expr| expr.expr()));
                });
                self.alloc_term(Term::Missing, Some(expr))
            }
            ast::Expr::RecordFieldExpr(field) => {
                let _ = self.lower_optional_term(field.expr().map(Into::into));
                self.alloc_term(Term::Missing, Some(expr))
            }
            ast::Expr::RecordIndexExpr(_index) => self.alloc_term(Term::Missing, Some(expr)),
            ast::Expr::RecordUpdateExpr(update) => {
                let _ = self.lower_optional_term(update.expr().map(Into::into));
                update.fields().for_each(|field| {
                    let _ = self.lower_optional_term(field.expr().and_then(|expr| expr.expr()));
                    let _ = self.lower_optional_term(field.ty().and_then(|expr| expr.expr()));
                });
                self.alloc_term(Term::Missing, Some(expr))
            }
            ast::Expr::Remote(remote) => {
                let _ =
                    self.lower_optional_term(remote.module().and_then(|module| module.module()));
                let _ = self.lower_optional_term(remote.fun());
                self.alloc_term(Term::Missing, Some(expr))
            }
            ast::Expr::UnaryOpExpr(unary_op) => {
                let term = self.lower_optional_term(unary_op.operand());
                match unary_op.op() {
                    Some((ast::UnaryOp::Plus, _)) => {
                        self.alloc_term(self.body[term].clone(), Some(expr))
                    }
                    Some((ast::UnaryOp::Minus, _)) => {
                        if let Term::Literal(literal) = &self.body[term] {
                            let value = literal.negate().map_or(Term::Missing, Term::Literal);
                            self.alloc_term(value, Some(expr))
                        } else {
                            self.alloc_term(Term::Missing, Some(expr))
                        }
                    }
                    _ => self.alloc_term(Term::Missing, Some(expr)),
                }
            }
            ast::Expr::CondMatchExpr(cond) => {
                let _ = self.lower_optional_term(cond.lhs());
                let _ = self.lower_optional_term(cond.rhs());
                self.alloc_term(Term::Missing, Some(expr))
            }
            // TODO(T262108365): lower native records (EEP 79) properly
            ast::Expr::AnonRecordExpr(_)
            | ast::Expr::AnonRecordFieldExpr(_)
            | ast::Expr::AnonRecordUpdateExpr(_)
            | ast::Expr::QualifiedRecordExpr(_)
            | ast::Expr::QualifiedRecordFieldExpr(_)
            | ast::Expr::QualifiedRecordUpdateExpr(_) => self.alloc_term(Term::Missing, Some(expr)),
        }
    }

    fn lower_term_max(&mut self, expr_max: &ast::ExprMax, expr: &ast::Expr) -> TermId {
        match expr_max {
            ast::ExprMax::AnonymousFun(_fun) => self.alloc_term(Term::Missing, Some(expr)),
            ast::ExprMax::Atom(atom) => {
                let atom = Atom::new(&atom.as_name());
                self.alloc_term(Term::Literal(Literal::Atom(atom)), Some(expr))
            }
            ast::ExprMax::Binary(bin) => {
                let value = bin
                    .elements()
                    .fold(Term::Binary(Vec::new()), |acc, element| {
                        if let Some(seg) =
                            self.lower_bin_element(&element, Self::lower_optional_term)
                        {
                            match acc {
                                Term::Binary(mut vec) => {
                                    // TODO: process size & unit & types
                                    if seg.size.is_none()
                                        && seg.unit.is_none()
                                        && seg.tys.is_empty()
                                    {
                                        match &self.body[seg.elem] {
                                            Term::Literal(Literal::Char(ch)) => {
                                                vec.push(*ch as u8);
                                                Term::Binary(vec)
                                            }
                                            Term::Literal(Literal::Integer(int)) => {
                                                vec.push(int.value as u8);
                                                Term::Binary(vec)
                                            }
                                            Term::Literal(Literal::String(str)) => {
                                                vec.extend(str.chars().map(|ch| ch as u8));
                                                Term::Binary(vec)
                                            }
                                            _ => Term::Missing,
                                        }
                                    } else {
                                        Term::Missing
                                    }
                                }
                                _ => Term::Missing,
                            }
                        } else {
                            acc
                        }
                    });

                self.alloc_term(value, Some(expr))
            }
            ast::ExprMax::BinaryComprehension(_bc) => self.alloc_term(Term::Missing, Some(expr)),
            ast::ExprMax::BlockExpr(_block) => self.alloc_term(Term::Missing, Some(expr)),
            ast::ExprMax::CaseExpr(_case) => self.alloc_term(Term::Missing, Some(expr)),
            ast::ExprMax::Char(char) => {
                let value = lower_char(char).map_or(Term::Missing, Term::Literal);
                self.alloc_term(value, Some(expr))
            }
            ast::ExprMax::Concatables(concat) => {
                let value = self
                    .lower_concat_with_macros(concat)
                    .map_or(Term::Missing, Term::Literal);
                self.alloc_term(value, Some(expr))
            }
            ast::ExprMax::ExternalFun(fun) => {
                let module = self.lower_optional_term(
                    fun.module()
                        .and_then(|module| module.name())
                        .map(Into::into),
                );
                let name = self.lower_optional_term(fun.fun().map(Into::into));
                let arity = self.lower_optional_term(
                    fun.arity().and_then(|arity| arity.value()).map(Into::into),
                );
                if let (
                    Term::Literal(Literal::Atom(module)),
                    Term::Literal(Literal::Atom(name)),
                    Term::Literal(Literal::Integer(arity)),
                ) = (&self.body[module], &self.body[name], &self.body[arity])
                {
                    if let Ok(arity) = (arity.value).try_into() {
                        let term = Term::CaptureFun {
                            module: *module,
                            name: *name,
                            arity,
                        };
                        self.alloc_term(term, Some(expr))
                    } else {
                        self.alloc_term(Term::Missing, Some(expr))
                    }
                } else {
                    self.alloc_term(Term::Missing, Some(expr))
                }
            }
            ast::ExprMax::Float(float) => {
                let value = lower_float(float).map_or(Term::Missing, Term::Literal);
                self.alloc_term(value, Some(expr))
            }
            ast::ExprMax::FunType(fun) => {
                if let Some(sig) = fun.sig() {
                    let _ = self.lower_optional_term(sig.ty());
                    sig.args()
                        .iter()
                        .flat_map(|args| args.args())
                        .for_each(|pat| {
                            let _ = self.lower_term(&pat);
                        });
                }
                self.alloc_term(Term::Missing, Some(expr))
            }
            ast::ExprMax::IfExpr(_if_expr) => self.alloc_term(Term::Missing, Some(expr)),
            ast::ExprMax::Integer(int) => {
                let value = lower_int(int).map_or(Term::Missing, Term::Literal);
                self.alloc_term(value, Some(expr))
            }
            ast::ExprMax::InternalFun(_fun) => self.alloc_term(Term::Missing, Some(expr)),
            ast::ExprMax::List(list) => {
                let (exprs, tail) = self.lower_list(
                    list,
                    |this| this.alloc_term(Term::Missing, None),
                    |this, expr| this.lower_term(expr),
                    |_this, _call| None,
                );
                self.alloc_term(Term::List { exprs, tail }, Some(expr))
            }
            ast::ExprMax::ListComprehension(_lc) => self.alloc_term(Term::Missing, Some(expr)),
            ast::ExprMax::MacroCallExpr(call) => self
                .resolve_macro(call, |this, source, replacement| match replacement {
                    MacroReplacement::BuiltIn(built_in) => {
                        this.lower_built_in_macro(built_in).map(|literal| {
                            let term_id = this.alloc_term(Term::Literal(literal), None);
                            this.record_term_source(term_id, source);
                            (None, term_id)
                        })
                    }
                    MacroReplacement::Ast(def_idx, ast::MacroDefReplacement::Expr(macro_expr)) => {
                        let term_id = this.lower_term(&macro_expr);
                        this.record_term_source(term_id, source);
                        Some((Some(def_idx), term_id))
                    }
                    _ => None,
                })
                .flatten()
                .map(|(macro_def, expansion)| {
                    let args = call
                        .args()
                        .iter()
                        .flat_map(|args| args.args())
                        .map(|expr| self.lower_optional_term(expr.expr()))
                        .collect();
                    self.alloc_term(
                        Term::MacroCall {
                            expansion,
                            args,
                            macro_def,
                            macro_name: self.macro_call_name(call.name()),
                        },
                        Some(expr),
                    )
                })
                .unwrap_or_else(|| {
                    // Record diagnostic for unresolved macro
                    self.record_unresolved_macro(call);

                    let expansion = self.alloc_term(Term::Missing, Some(expr));
                    let args = call
                        .args()
                        .iter()
                        .flat_map(|args| args.args())
                        .map(|expr| self.lower_optional_term(expr.expr()))
                        .collect();
                    self.alloc_term(
                        Term::MacroCall {
                            expansion,
                            args,
                            macro_def: None,
                            macro_name: self.macro_call_name(call.name()),
                        },
                        Some(expr),
                    )
                }),
            ast::ExprMax::MacroString(ms) => {
                if let Some(lit) = self.resolve_macro_string(ms) {
                    self.alloc_term(Term::Literal(lit), Some(expr))
                } else {
                    self.alloc_term(Term::Missing, Some(expr))
                }
            }
            ast::ExprMax::ParenExpr(paren_expr) => {
                if let Some(expr) = paren_expr.expr() {
                    self.lower_term(&expr)
                } else {
                    self.alloc_term(Term::Missing, Some(expr))
                }
            }
            ast::ExprMax::ReceiveExpr(_receive) => self.alloc_term(Term::Missing, Some(expr)),
            ast::ExprMax::String(str) => {
                let value = self
                    .lower_str_or_sigil(
                        str,
                        expr,
                        Self::lower_binary_string_literal_term,
                        Term::Literal,
                    )
                    .unwrap_or(Term::Missing);
                self.alloc_term(value, Some(expr))
            }
            ast::ExprMax::TryExpr(_try_expr) => self.alloc_term(Term::Missing, Some(expr)),
            ast::ExprMax::Tuple(tup) => {
                let exprs = tup.expr().map(|expr| self.lower_term(&expr)).collect();
                self.alloc_term(Term::Tuple { exprs }, Some(expr))
            }
            ast::ExprMax::Var(var) => self
                .resolve_var(var, |this, expr| this.lower_optional_term(expr.expr()))
                .unwrap_or_else(|_var| self.alloc_term(Term::Missing, Some(expr))),
            ast::ExprMax::MaybeExpr(maybe_expr) => {
                maybe_expr.exprs().for_each(|expr| {
                    self.lower_expr(&expr);
                });

                maybe_expr
                    .clauses()
                    .flat_map(|clause| self.lower_cr_clause(clause))
                    .last();
                self.alloc_term(Term::Missing, Some(expr))
            }
            ExprMax::MapComprehension(_mc) => self.alloc_term(Term::Missing, Some(expr)),
        }
    }

    fn lower_built_in_macro(&mut self, built_in: BuiltInMacro) -> Option<Literal> {
        match built_in {
            // This is a bit of a hack, but allows us not to depend on the file system
            // It somewhat replicates the behaviour of -deterministic option
            BuiltInMacro::FILE => {
                // First check for override (from including file)
                if let Some(ref name) = self.module_name_override {
                    return Some(Literal::String(StringVariant::Normal(format!(
                        "{}.erl",
                        name
                    ))));
                }
                // Fall back to current file
                let form_list = self.db.file_form_list(self.file_id());
                form_list.module_attribute().map(|attr| {
                    Literal::String(StringVariant::Normal(format!("{}.erl", attr.name)))
                })
            }
            BuiltInMacro::FUNCTION_NAME => self.function_info.map(|(name, _)| Literal::Atom(name)),
            BuiltInMacro::FUNCTION_ARITY => self.function_info.map(|(_, arity)| {
                let arity = self.epp_function_arity.unwrap_or(arity);
                Literal::Integer(arity.into())
            }),
            // Dummy value, we don't want to depend on the exact position
            BuiltInMacro::LINE => Some(Literal::Integer(0.into())),
            BuiltInMacro::MODULE => {
                // First check for override
                if let Some(ref name) = self.module_name_override {
                    return Some(Literal::Atom(Atom::new(name)));
                }
                let form_list = self.db.file_form_list(self.file_id());
                form_list
                    .module_attribute()
                    .map(|attr| Literal::Atom(Atom::new(&attr.name)))
            }
            BuiltInMacro::MODULE_STRING => {
                // First check for override
                if let Some(ref name) = self.module_name_override {
                    return Some(Literal::String(StringVariant::Normal(name.to_string())));
                }
                let form_list = self.db.file_form_list(self.file_id());
                form_list
                    .module_attribute()
                    .map(|attr| Literal::String(StringVariant::Normal(attr.name.to_string())))
            }
            BuiltInMacro::MACHINE => Some(Literal::Atom(Atom::new(&known::ELP))),
            BuiltInMacro::OTP_RELEASE => {
                // Use real OTP version if available, otherwise default to 2000
                let version = elp_base_db::Otp::version()
                    .and_then(|v| v.parse::<u32>().ok())
                    .unwrap_or(2000);
                Some(Literal::Integer(version.into()))
            }
        }
    }

    // With OTP 27 string sigils as syntactic sugar, we sometimes convert
    // them into a binary.  So use the same technique as in
    // `lower_bin_element`.
    fn lower_str_or_sigil<Id>(
        &mut self,
        str: &ast::String,
        expr: &ast::Expr,
        lower: fn(&mut Self, Literal, &ast::Expr) -> Option<Id>,
        literal: fn(Literal) -> Id,
    ) -> Option<Id> {
        let s = str.text();
        let contents: String = str.clone().into();
        if let Some(_rest) = s.strip_prefix("\"\"\"") {
            // Triple Quoted String.  Verbatim String
            Some(literal(Literal::String(StringVariant::Verbatim(
                str.text().to_string(),
            ))))
        } else if let Some(_rest) = s.strip_prefix("~\"\"\"") {
            // Verbatim Binary (default sigil)
            self.lower_verbatim_binary_sigil(str, expr, true, lower)
        } else if let Some(_rest) = s.strip_prefix("~B\"\"\"") {
            // Verbatim Binary (explicit sigil)
            self.lower_verbatim_binary_sigil(str, expr, true, lower)
        } else if let Some(_rest) = s.strip_prefix("~S\"\"\"") {
            // Verbatim string
            let contents: String = str.clone().into();
            Some(literal(Literal::String(StringVariant::Normal(contents))))
        } else if let Some(_rest) = s.strip_prefix("~s\"\"\"") {
            // Quoted String
            // contents is already unescaped by trim_quotes_and_sigils (via Into<String>)
            Some(literal(Literal::String(StringVariant::Normal(contents))))
        } else if let Some(_rest) = s.strip_prefix("~\"") {
            // Quoted binary, default when no additional char
            self.lower_quoted_binary_sigil(str, expr, lower)
        } else if let Some(_rest) = s.strip_prefix("~b\"") {
            // Quoted binary
            self.lower_quoted_binary_sigil(str, expr, lower)
        } else if let Some(_rest) = s.strip_prefix("~B\"") {
            // Verbatim Binary
            self.lower_verbatim_binary_sigil(str, expr, false, lower)
        } else if let Some(_rest) = s.strip_prefix("~s\"") {
            // Quoted String
            // contents is already unescaped by trim_quotes_and_sigils (via Into<String>)
            Some(literal(Literal::String(StringVariant::Normal(contents))))
        } else if let Some(_rest) = s.strip_prefix("~S\"") {
            // Verbatim string
            let contents: String = str.clone().into();
            Some(literal(Literal::String(StringVariant::Normal(contents))))
        } else if has_nonquote_sigil_prefix(&s, "~B") {
            // Verbatim Binary with non-quote delimiter
            self.lower_verbatim_binary_sigil(str, expr, false, lower)
        } else if has_nonquote_sigil_prefix(&s, "~b") {
            // Quoted binary with non-quote delimiter
            self.lower_quoted_binary_sigil(str, expr, lower)
        } else if has_nonquote_sigil_prefix(&s, "~S") {
            // Verbatim string with non-quote delimiter
            Some(literal(Literal::String(StringVariant::Normal(contents))))
        } else if has_nonquote_sigil_prefix(&s, "~s") {
            // Quoted String with non-quote delimiter
            // contents is already unescaped by trim_quotes_and_sigils (via Into<String>)
            Some(literal(Literal::String(StringVariant::Normal(contents))))
        } else if s
            .strip_prefix("~")
            .and_then(|r| r.chars().next())
            .is_some_and(is_nonquote_sigil_delimiter)
        {
            // Default sigil (~) with non-quote delimiter → quoted binary
            self.lower_quoted_binary_sigil(str, expr, lower)
        } else {
            // ordinary string
            Some(literal(Literal::String(StringVariant::Normal(
                unescape::unescape_string(&str.text())?.to_string(),
            ))))
        }
    }

    fn lower_verbatim_binary_sigil<Id>(
        &mut self,
        str: &ast::String,
        expr: &ast::Expr,
        is_not_tq: bool,
        lower: fn(&mut Self, Literal, &ast::Expr) -> Option<Id>,
    ) -> Option<Id> {
        let contents: String = str.clone().into();
        let string_variant = if is_not_tq {
            Literal::String(StringVariant::Normal(
                unescape::unescape_string(&contents)?.to_string(),
            ))
        } else {
            Literal::String(StringVariant::Normal(contents))
        };
        lower(self, string_variant, expr)
    }

    fn lower_quoted_binary_sigil<Id>(
        &mut self,
        str: &ast::String,
        expr: &ast::Expr,
        lower: fn(&mut Self, Literal, &ast::Expr) -> Option<Id>,
    ) -> Option<Id> {
        let contents: String = str.clone().into();
        let s = str.text();
        // For triple-quoted ~b sigils (e.g. ~b"""..."""), trim_quotes_and_sigils
        // returns verbatim content without escape processing, so we must
        // unescape here. For single-quoted sigils, trim_quotes_and_sigils
        // already unescapes, so we use contents directly to avoid
        // double-unescaping.
        let is_tq = s.starts_with("~b\"\"\"") || s.starts_with("~\"\"\"");
        let string_variant = if is_tq {
            Literal::String(StringVariant::Normal(
                unescape::unescape_string(&format!("\"{contents}\""))?.to_string(),
            ))
        } else {
            Literal::String(StringVariant::Normal(contents))
        };

        lower(self, string_variant, expr)
    }

    fn lower_binary_string_literal_expr(
        &mut self,
        string: Literal,
        expr: &ast::Expr,
    ) -> Option<Expr> {
        let elem = self.alloc_expr(Expr::Literal(string), Some(expr));
        let segs = vec![BinarySeg {
            elem,
            size: None,
            tys: vec![Atom::new(&known::utf8)],
            unit: None,
        }];
        Some(Expr::Binary { segs })
    }

    fn lower_binary_string_literal_pat(
        &mut self,
        string: Literal,
        expr: &ast::Expr,
    ) -> Option<Pat> {
        let elem = self.alloc_pat(Pat::Literal(string), Some(expr));
        let segs = vec![BinarySeg {
            elem,
            size: None,
            tys: vec![Atom::new(&known::utf8)],
            unit: None,
        }];
        Some(Pat::Binary { segs })
    }

    fn lower_binary_string_literal_type_expr(
        &mut self,
        _string: Literal,
        _expr: &ast::Expr,
    ) -> Option<TypeExpr> {
        None
    }

    // This function is passed in to another one, so needs to comply
    // with the expected signature.  Hence the unused `_expr` parameter.
    fn lower_binary_string_literal_term(
        &mut self,
        string: Literal,
        _expr: &ast::Expr,
    ) -> Option<Term> {
        if let Literal::String(str) = string {
            let segs = match str {
                StringVariant::Normal(s) => s.into(),
                StringVariant::Verbatim(s) => s.into(),
            };
            Some(Term::Binary(segs))
        } else {
            None
        }
    }

    fn resolve_name(&mut self, name: ast::Name) -> Option<NamedAtom> {
        let expr_id = self.lower_expr(&name.into());
        if let Expr::Literal(Literal::Atom(atom)) = self.body[expr_id] {
            Some(NamedAtom::new(atom, expr_id))
        } else {
            None
        }
    }

    fn resolve_arity(&mut self, arity: ast::ArityValue) -> Option<i128> {
        let expr_id = self.lower_expr(&arity.into());
        if let Expr::Literal(Literal::Integer(int)) = &self.body[expr_id] {
            Some(int.value)
        } else {
            None
        }
    }

    fn is_wildcard_field_name(name: &ast::Name) -> bool {
        matches!(name, ast::Name::Var(v) if v.text() == "_")
    }

    fn lower_record_fields(
        &mut self,
        fields: impl Iterator<Item = ast::RecordField>,
    ) -> (Vec<(NamedAtom, ExprId)>, Option<ExprId>) {
        let mut named = Vec::new();
        let mut default_field = None;
        for field in fields {
            let value = self.lower_optional_expr(field.expr().and_then(|expr| expr.expr()));
            if let Some(name) = field.name().and_then(|n| self.resolve_name(n)) {
                named.push((name, value));
            } else if field
                .name()
                .is_some_and(|n| Self::is_wildcard_field_name(&n))
            {
                default_field = Some(value);
            }
        }
        (named, default_field)
    }

    fn lower_record_fields_pat(
        &mut self,
        fields: impl Iterator<Item = ast::RecordField>,
    ) -> (Vec<(NamedAtom, PatId)>, Option<PatId>) {
        let mut named = Vec::new();
        let mut default_field = None;
        for field in fields {
            let value = self.lower_optional_pat(field.expr().and_then(|expr| expr.expr()));
            if let Some(name) = field.name().and_then(|n| self.resolve_name(n)) {
                named.push((name, value));
            } else if field
                .name()
                .is_some_and(|n| Self::is_wildcard_field_name(&n))
            {
                default_field = Some(value);
            }
        }
        (named, default_field)
    }

    fn lower_native_record_fields(
        &mut self,
        fields: impl Iterator<Item = ast::RecordField>,
    ) -> Vec<(NamedAtom, ExprId)> {
        fields
            .flat_map(|field| {
                let value = self.lower_optional_expr(field.expr().and_then(|expr| expr.expr()));
                let name = self.resolve_name(field.name()?)?;
                Some((name, value))
            })
            .collect()
    }

    fn lower_native_record_fields_pat(
        &mut self,
        fields: impl Iterator<Item = ast::RecordField>,
    ) -> Vec<(NamedAtom, PatId)> {
        fields
            .flat_map(|field| {
                let value = self.lower_optional_pat(field.expr().and_then(|expr| expr.expr()));
                let name = self.resolve_name(field.name()?)?;
                Some((name, value))
            })
            .collect()
    }

    fn lower_record_fields_type(
        &mut self,
        fields: impl Iterator<Item = ast::RecordField>,
    ) -> Vec<(NamedAtom, TypeExprId)> {
        fields
            .flat_map(|field| {
                let ty = self.lower_optional_type_expr(field.ty().and_then(|expr| expr.expr()));
                let name = self.resolve_name(field.name()?)?;
                Some((name, ty))
            })
            .collect()
    }

    fn lower_qualified_record_name(
        &mut self,
        qname: &ast::QualifiedRecordName,
    ) -> Option<NativeRecordName> {
        let module = self.resolve_name(qname.module()?.name()?)?;
        let name = self.resolve_name(qname.name()?)?;
        Some(NativeRecordName::Qualified { module, name })
    }

    fn resolve_macro<R>(
        &mut self,
        call: &ast::MacroCallExpr,
        cb: impl FnOnce(&mut Self, ExprSource, MacroReplacement) -> R,
    ) -> Option<R> {
        let name = macro_exp::macro_name(call)?;
        if self.macro_stack().any(|entry| entry.name == name) {
            return None;
        }

        let source = InFileAstPtr::new(
            self.curr_file_id(),
            AstPtr::new(call)
                .cast()
                .expect("MacroCallExpr should cast to Expr"),
        );

        match self.resolve_macro_name(&name) {
            Some(res @ ResolvedMacro::BuiltIn(built_in)) => {
                self.record_macro_resolution(call, res, name.clone());
                match built_in {
                    BuiltInMacro::FUNCTION_NAME => {
                        if let Some(args) = call.args() {
                            Some(cb(
                                self,
                                source,
                                MacroReplacement::BuiltInArgs(built_in, args),
                            ))
                        } else {
                            Some(cb(self, source, MacroReplacement::BuiltIn(built_in)))
                        }
                    }
                    _ => Some(cb(self, source, MacroReplacement::BuiltIn(built_in))),
                }
            }
            Some(res @ ResolvedMacro::User(def_idx)) => {
                self.record_macro_resolution(call, res, name.clone());
                self.enter_macro(name, def_idx, call.args(), |this, replacement| {
                    cb(this, source, MacroReplacement::Ast(def_idx, replacement))
                })
            }
            None => {
                let name = name.with_arity(None);
                let args = call.args()?;
                let res = self.resolve_macro_name(&name)?;
                self.record_macro_resolution(call, res, name.clone());
                match res {
                    ResolvedMacro::BuiltIn(built_in) => Some(cb(
                        self,
                        source,
                        MacroReplacement::BuiltInArgs(built_in, args),
                    )),
                    ResolvedMacro::User(def_idx) => {
                        self.enter_macro(name, def_idx, None, |this, replacement| {
                            cb(
                                this,
                                source,
                                MacroReplacement::AstArgs(def_idx, replacement, args),
                            )
                        })
                    }
                }
            }
        }
    }

    fn enter_macro<R>(
        &mut self,
        name: MacroName,
        def_idx: InFile<DefineId>,
        args: Option<ast::MacroCallArgs>,
        cb: impl FnOnce(&mut Self, ast::MacroDefReplacement) -> R,
    ) -> Option<R> {
        let form_list = self.db.file_form_list(def_idx.file_id);
        let define_form_id = form_list[def_idx.value].form_id;
        let source = self.db.parse(def_idx.file_id);
        let define = define_form_id.get(&source.tree());
        let replacement = define.replacement()?;

        let var_map = if let Some(args) = args {
            define
                .args()
                .zip(args.args())
                .map(|(var, arg)| (Var::new(&var.as_name()), arg))
                .collect()
        } else {
            FxHashMap::default()
        };
        let new_stack_id = self.macro_stack.len();
        self.macro_stack.push(MacroStackEntry {
            name,
            file_id: def_idx.file_id,
            var_map,
            parent_id: self.macro_stack_id,
        });
        self.macro_stack_id = new_stack_id;

        let ret = cb(self, replacement);

        let entry = self.macro_stack.pop().expect("BUG: missing stack entry");
        self.macro_stack_id = entry.parent_id;

        Some(ret)
    }

    fn macro_stack(&self) -> impl Iterator<Item = &MacroStackEntry> {
        iter::successors(Some(&self.macro_stack[self.macro_stack_id]), |entry| {
            if entry.parent_id != 0 {
                Some(&self.macro_stack[entry.parent_id])
            } else {
                None
            }
        })
    }

    fn resolve_var<R>(
        &mut self,
        var: &ast::Var,
        cb: impl FnOnce(&mut Self, ast::MacroExpr) -> R,
    ) -> Result<R, Var> {
        let var = Var::new(&var.as_name());
        let entry = &self.macro_stack[self.macro_stack_id];
        if let Some(expr) = entry.var_map.get(&var).cloned() {
            let curr_stack_id = self.macro_stack_id;
            self.macro_stack_id = entry.parent_id;

            let ret = cb(self, expr);

            self.macro_stack_id = curr_stack_id;

            Ok(ret)
        } else {
            Err(var)
        }
    }

    /// Resolve a `??Arg` macro stringification.
    /// If `Arg` is a macro parameter in the current expansion context,
    /// returns the stringified tokens of the argument as a string literal.
    /// Matches the Erlang preprocessor behavior: tokens are joined with
    /// single spaces and comments/whitespace are stripped.
    ///
    /// When the argument is a variable that corresponds to a parent macro's
    /// parameter, walks up the macro stack to find the original argument
    /// tokens. This handles nested macros like:
    ///   -define(OUTER(Pattern), ?INNER(??Pattern)).
    ///   -define(INNER(PatStr), ...PatStr...).
    /// where `?OUTER(?IQ_RESULT(X))` should stringify to `"? IQ_RESULT ( X )"`.
    fn resolve_macro_string(&self, macro_string: &ast::MacroString) -> Option<Literal> {
        const MAX_WALKUP_DEPTH: usize = 100;
        let name = macro_string.name()?;
        if let ast::MacroName::Var(ref var) = name {
            let hir_var = Var::new(&var.as_name());
            let mut current_stack_id = self.macro_stack_id;
            let mut current_var = hir_var;
            let mut depth = 0;
            loop {
                let entry = &self.macro_stack[current_stack_id];
                if let Some(macro_expr) = entry.var_map.get(&current_var)
                    && let Some(expr) = macro_expr.expr()
                {
                    // If the argument is a simple variable reference that
                    // corresponds to a parent macro's parameter, walk up
                    // the stack to find the original argument tokens.
                    if let ast::Expr::ExprMax(ast::ExprMax::Var(ref inner_var)) = expr {
                        let parent_id = entry.parent_id;
                        if parent_id != current_stack_id {
                            let parent_var = Var::new(&inner_var.as_name());
                            if self.macro_stack[parent_id]
                                .var_map
                                .contains_key(&parent_var)
                            {
                                depth += 1;
                                if depth > MAX_WALKUP_DEPTH {
                                    log::error!(
                                        "resolve_macro_string: exceeded max walk-up depth of {} for ??{}",
                                        MAX_WALKUP_DEPTH,
                                        var.as_name()
                                    );
                                    return None;
                                }
                                current_var = parent_var;
                                current_stack_id = parent_id;
                                continue;
                            }
                        }
                    }
                    // Stringify the expression tokens, substituting any
                    // variable tokens that are parameters of an outer macro
                    // with their actual argument text.
                    let text = self.stringify_macro_expr_tokens(&expr, entry.parent_id);
                    return Some(Literal::String(StringVariant::Normal(text)));
                }
                break;
            }
        }
        None
    }

    /// Collect non-trivia tokens from `expr` and join with spaces,
    /// matching the Erlang preprocessor's stringification behavior.
    ///
    /// `scope_stack_id` is the macro stack entry in whose context the
    /// expression was written. Any VAR token that corresponds to a
    /// parameter of that scope's macro is recursively substituted with
    /// the actual argument tokens.
    fn stringify_macro_expr_tokens(&self, expr: &ast::Expr, scope_stack_id: usize) -> String {
        expr.syntax()
            .descendants_with_tokens()
            .filter_map(|elem| elem.into_token())
            .filter(|token| !token.kind().is_trivia())
            .map(|token| {
                // Check if this VAR token is a macro parameter in the
                // enclosing scope, and if so substitute with the actual
                // argument tokens (recursively).
                if token.kind() == SyntaxKind::VAR
                    && let Some(parent) = token.parent()
                    && let Some(var_node) = ast::Var::cast(parent)
                {
                    let var = Var::new(&var_node.as_name());
                    let scope = &self.macro_stack[scope_stack_id];
                    if let Some(macro_expr) = scope.var_map.get(&var)
                        && let Some(inner_expr) = macro_expr.expr()
                    {
                        return self.stringify_macro_expr_tokens(&inner_expr, scope.parent_id);
                    }
                }
                // Atom tokens are normalized: the Erlang preprocessor
                // uses io_lib:write/1 which only quotes atoms when
                // necessary (e.g., reserved words, special chars),
                // so 'undefined' becomes undefined but 'and' stays 'and'.
                if token.kind() == SyntaxKind::ATOM && token.text().starts_with('\'') {
                    match unescape::unescape_string(token.text()) {
                        Some(unescaped) => crate::quote::escape_and_quote_atom(&unescaped),
                        None => token.text().to_string(),
                    }
                } else {
                    token.text().to_string()
                }
            })
            .collect::<Vec<_>>()
            .join(" ")
    }

    /// Lower a Concatables node, resolving macros that expand to strings.
    fn lower_concat_with_macros(&mut self, concat: &ast::Concatables) -> Option<Literal> {
        let mut buf = String::new();
        for concatable in concat.elems() {
            match concatable {
                ast::Concatable::MacroCallExpr(call) => {
                    let string_value = self.resolve_macro_concat_string(&call)?;
                    buf.push_str(&string_value);
                }
                ast::Concatable::MacroString(ms) => match self.resolve_macro_string(&ms)? {
                    Literal::String(StringVariant::Normal(s) | StringVariant::Verbatim(s)) => {
                        buf.push_str(&s);
                    }
                    _ => return None,
                },
                ast::Concatable::String(str) => {
                    let contents: String = str.clone().into();
                    buf.push_str(&contents);
                }
                ast::Concatable::Var(ref var) => {
                    let string_value = self.resolve_var_concat_string(var)?;
                    buf.push_str(&string_value);
                }
            }
        }
        Some(Literal::String(StringVariant::Normal(buf)))
    }

    /// Try to resolve a macro call to its string value.
    ///
    /// Returns `Some(string)` if the macro expands to a string literal
    /// or a Concatables that resolves to a string (possibly containing
    /// further macros that also resolve to strings).
    fn resolve_macro_concat_string(&mut self, call: &ast::MacroCallExpr) -> Option<String> {
        self.resolve_macro(call, |this, _source, replacement| match replacement {
            MacroReplacement::Ast(_, ast::MacroDefReplacement::Expr(ref expr)) => {
                if let ast::Expr::ExprMax(expr_max) = expr {
                    match expr_max {
                        ast::ExprMax::Concatables(concat) => this
                            .lower_concat_with_macros(concat)
                            .and_then(|lit| match lit {
                                Literal::String(
                                    StringVariant::Normal(s) | StringVariant::Verbatim(s),
                                ) => Some(s),
                                _ => None,
                            }),
                        ast::ExprMax::String(str) => {
                            let contents: std::string::String = str.clone().into();
                            Some(contents)
                        }
                        ast::ExprMax::MacroCallExpr(inner_call) => {
                            this.resolve_macro_concat_string(inner_call)
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            MacroReplacement::BuiltIn(built_in) => {
                this.lower_built_in_macro(built_in)
                    .and_then(|lit| match lit {
                        Literal::String(StringVariant::Normal(s) | StringVariant::Verbatim(s)) => {
                            Some(s)
                        }
                        _ => None,
                    })
            }
            _ => None,
        })
        .flatten()
    }

    /// Try to resolve a macro parameter variable to its string value.
    ///
    /// When a variable appears in a string concatenation position inside
    /// a macro body (e.g., `-define(FMT(Format), ?PREFIX Format).`),
    /// this resolves the variable through the macro stack. If the variable
    /// is a macro parameter bound to a string literal, concatables, or
    /// another macro parameter variable, it is recursively resolved.
    fn resolve_var_concat_string(&mut self, var: &ast::Var) -> Option<String> {
        self.resolve_var(var, |this, macro_expr| -> Option<String> {
            let expr = macro_expr.expr()?;
            match &expr {
                ast::Expr::ExprMax(ast::ExprMax::String(str)) => {
                    let contents: String = str.clone().into();
                    Some(contents)
                }
                ast::Expr::ExprMax(ast::ExprMax::Concatables(concat)) => this
                    .lower_concat_with_macros(concat)
                    .and_then(|lit| match lit {
                        Literal::String(StringVariant::Normal(s) | StringVariant::Verbatim(s)) => {
                            Some(s)
                        }
                        _ => None,
                    }),
                ast::Expr::ExprMax(ast::ExprMax::Var(inner_var)) => {
                    this.resolve_var_concat_string(inner_var)
                }
                _ => None,
            }
        })
        .ok()
        .flatten()
    }

    fn alloc_expr(&mut self, expr: Expr, source: Option<&ast::Expr>) -> ExprId {
        let expr_id = self.body.exprs.alloc(expr);
        if let Some(source) = source {
            let ptr = AstPtr::new(source);
            let source = InFileAstPtr::new(self.curr_file_id(), ptr);
            self.record_expr_source(expr_id, source);
        }
        expr_id
    }

    fn record_expr_source(&mut self, expr_id: ExprId, source: ExprSource) {
        self.source_map.expr_map.insert(source, expr_id);
        self.source_map.expr_map_back.insert(expr_id, source);
    }

    fn alloc_pat(&mut self, expr: Pat, source: Option<&ast::Expr>) -> PatId {
        let pat_id = self.body.pats.alloc(expr);
        if let Some(source) = source {
            let ptr = AstPtr::new(source);
            let source = InFileAstPtr::new(self.curr_file_id(), ptr);
            self.record_pat_source(pat_id, source);
        }
        pat_id
    }

    fn record_pat_source(&mut self, pat_id: PatId, source: ExprSource) {
        self.source_map.pat_map.insert(source, pat_id);
        self.source_map.pat_map_back.insert(pat_id, source);
    }

    fn alloc_type_expr(&mut self, type_expr: TypeExpr, source: Option<&ast::Expr>) -> TypeExprId {
        let type_expr_id = self.body.type_exprs.alloc(type_expr);
        if let Some(source) = source {
            let ptr = AstPtr::new(source);
            let source = InFileAstPtr::new(self.curr_file_id(), ptr);
            self.record_type_source(type_expr_id, source);
        }
        type_expr_id
    }

    fn record_type_source(&mut self, type_id: TypeExprId, source: ExprSource) {
        self.source_map.type_expr_map.insert(source, type_id);
        self.source_map.type_expr_map_back.insert(type_id, source);
    }

    fn alloc_term(&mut self, term: Term, source: Option<&ast::Expr>) -> TermId {
        let term_id = self.body.terms.alloc(term);
        if let Some(source) = source {
            let ptr = AstPtr::new(source);
            let source = InFileAstPtr::new(self.curr_file_id(), ptr);
            self.record_term_source(term_id, source);
        }
        term_id
    }

    fn record_term_source(&mut self, term_id: TermId, source: ExprSource) {
        self.source_map.term_map.insert(source, term_id);
        self.source_map.term_map_back.insert(term_id, source);
    }

    fn record_macro_resolution(
        &mut self,
        call: &ast::MacroCallExpr,
        res: ResolvedMacro,
        name: MacroName,
    ) {
        let ptr = AstPtr::new(call);
        let source = InFileAstPtr::new(self.curr_file_id(), ptr);
        self.source_map.macro_map.insert(source, res);
        self.macro_source_map.insert(name, source);
    }

    fn record_unresolved_macro(&mut self, call: &ast::MacroCallExpr) {
        // If we're in a macro RHS and this is ?FUNCTION_NAME or ?FUNCTION_ARITY, skip the diagnostic
        if self.in_macro_rhs {
            let macro_name = self.macro_call_name(call.name());
            if let MacroCallName::Var(var) = macro_name {
                let name_str = var.as_string();
                if name_str == "FUNCTION_NAME" || name_str == "FUNCTION_ARITY" {
                    return;
                }
            }
        }

        let source = InFileAstPtr::new(
            self.curr_file_id(),
            AstPtr::new(call)
                .cast()
                .expect("MacroCallExpr should cast to Expr"),
        );
        self.diagnostics
            .push(BodyDiagnostic::UnresolvedMacro(source));
    }

    fn curr_file_id(&self) -> FileId {
        self.macro_stack[self.macro_stack_id].file_id
    }

    fn macro_call_name(&self, name: Option<ast::MacroName>) -> MacroCallName {
        match name {
            Some(ast::MacroName::Atom(atom)) => {
                let atom = Atom::new(&atom.as_name());
                MacroCallName::Atom(atom)
            }
            Some(ast::MacroName::Var(var)) => {
                let var = Var::new(&var.as_name());
                MacroCallName::Var(var)
            }
            None => MacroCallName::Missing,
        }
    }
}

fn lower_char(char: &ast::Char) -> Option<Literal> {
    unescape::unescape_string(&char.text())
        .and_then(|str| str.chars().next())
        .map(Literal::Char)
}

fn lower_float(float: &ast::Float) -> Option<Literal> {
    let text = float.text().replace('_', "");
    let parts: Vec<_> = text.split("#").collect();
    match &parts[..] {
        [val] => {
            let float: f64 = val.parse().ok()?;
            Some(Literal::Float(float.to_bits()))
        }
        _ => {
            // We do not currently support processing based floating point literals
            None
        }
    }
}

fn lower_raw_int(int: &ast::Integer) -> Option<BasedInteger> {
    let text = int.text().replace('_', "");
    let parts: Vec<_> = text.split("#").collect();
    match &parts[..] {
        [val] => Some(BasedInteger {
            base: 10,
            value: val.parse().ok()?,
        }),
        [base, val] => {
            let base = base.parse().ok()?;
            Some(BasedInteger {
                base,
                value: parse_based(base, val)?,
            })
        }
        _ => None,
    }
}

/// Create a body for a condition expression with an optional module name override.
///
/// This variant allows specifying a module name to use for ?FILE, ?MODULE, etc.
/// when evaluating conditions in included header files. The override ensures that
/// built-in macros resolve to the including file's module name rather than failing
/// to resolve in the header context.
pub fn lower_condition_body(
    db: &dyn DefDatabase,
    file_id: FileId,
    cond_id: PPConditionId,
    expr: &ast::Expr,
    module_name_override: Option<Name>,
    macro_defs: Option<&FxHashMap<MacroName, InFile<DefineId>>>,
) -> (Arc<Body>, BodySourceMap, ExprId) {
    let origin = BodyOrigin::Condition { file_id, cond_id };
    let mut ctx = Ctx::new(db, origin);
    // Break the preprocessor–def_map salsa cycle (see skip_import_resolution field).
    ctx.skip_import_resolution = true;

    // Set local macro defs for condition body resolution. When defs are
    // provided (from the preprocessor or PreprocessorAnalysis), user-defined
    // macros resolve correctly. When None, empty defs still enable built-in
    // macro resolution (e.g., ?FILE, ?MODULE) without falling back to
    // db.resolve_macro().
    ctx.set_local_macro_defs(
        macro_defs
            .map(|defs| Arc::new(defs.clone()))
            .unwrap_or_else(|| Arc::new(FxHashMap::default())),
    );

    if module_name_override.is_some() {
        ctx.set_module_name_override(module_name_override);
    }
    let root_expr = ctx.lower_expr(expr);
    let (body, source_map) = ctx.finish();
    (body, source_map, root_expr)
}

fn parse_based(base: u32, str: &str) -> Option<i128> {
    let acc_base = base as i128;
    str.chars().try_fold(0, |acc, c| {
        let r: i128 = acc;
        let val = c.to_digit(base)?;
        // Erlang allows arbitrary length strings.  Fail conversion if
        // it would overflow.
        let shifted = r.checked_mul(acc_base)?;
        Some(shifted + val as i128)
    })
}

fn lower_int(int: &ast::Integer) -> Option<Literal> {
    lower_raw_int(int).map(Literal::Integer)
}

/// Lower a binary type expression like `<<>>`, `<<_:M>>`, `<<_:_*N>>`, or `<<_:M, _:_*N>>`
/// into (M, N) integer values.
///
/// In Erlang type syntax:
/// - `<<>>` means M=0, N=0
/// - `<<_:M>>` means a binary of exactly M bits
/// - `<<_:_*N>>` means a binary whose size is a multiple of N
/// - `<<_:M, _:_*N>>` means a binary of at least M bits, remainder multiple of N
fn lower_binary_type(bin: &ast::Binary) -> (u32, u32) {
    let mut m: u32 = 0;
    let mut n: u32 = 0;
    for element in bin.elements() {
        if let Some(size_expr) = element.size().and_then(|s| s.size()) {
            match &size_expr {
                // Simple integer size: <<_:M>> -- this is M (base size)
                ast::BitExpr::ExprMax(ast::ExprMax::Integer(int)) => {
                    m = int_text_to_u32(int);
                }
                // Multiplication expression: <<_:_*N>> -- this is N (unit size)
                ast::BitExpr::BinaryOpExpr(bin_op) => {
                    if let Some(ast::Expr::ExprMax(ast::ExprMax::Integer(int))) = bin_op.rhs() {
                        n = int_text_to_u32(&int);
                    }
                }
                _ => {}
            }
        }
    }
    (m, n)
}

fn int_text_to_u32(int: &ast::Integer) -> u32 {
    lower_raw_int(int)
        .and_then(|bi| u32::try_from(bi.value).ok())
        .unwrap_or(0)
}

/// Check if the first argument of a function clause consists entirely
/// of bracket tokens when tokenized by OTP's epp.  OTP's
/// `update_fun_name_1` classifies tokens as `left` (`(`, `[`, `{`,
/// `<<`), `right` (`)`, `]`, `}`, `>>`), `comma` (`,`), or `other`.
/// The first-argument flag is set only on encountering an `other`
/// token, so arguments made up exclusively of brackets/commas (e.g.
/// `[]`, `{}`, `<<>>`, `[[], {}]`) are never detected.  In a pattern
/// context `<`/`>` can only appear as part of `<<`/`>>`, so a
/// per-character check is sufficient.
/// <https://github.com/erlang/otp/issues/10705>
fn is_first_arg_bracket_only(clause: &ast::FunctionClause) -> bool {
    let args = match clause.args() {
        Some(args) => args,
        None => return false,
    };
    let first_arg = match args.args().next() {
        Some(arg) => arg,
        None => return false,
    };
    let text = first_arg.syntax().text().to_string();
    !text.is_empty()
        && text.chars().all(|c| {
            matches!(
                c,
                '[' | ']' | '{' | '}' | '(' | ')' | '<' | '>' | ',' | ' ' | '\t' | '\n' | '\r'
            )
        })
}

/// Check if a character is a non-quote sigil delimiter (EEP-66).
/// These are the opening delimiters for sigil strings other than `"`.
fn is_nonquote_sigil_delimiter(c: char) -> bool {
    matches!(c, '(' | '[' | '{' | '<' | '/' | '|' | '\'' | '`' | '#')
}

/// Check if a string starts with the given sigil prefix followed by
/// a non-quote delimiter character.
fn has_nonquote_sigil_prefix(s: &str, prefix: &str) -> bool {
    s.strip_prefix(prefix)
        .and_then(|r| r.chars().next())
        .is_some_and(is_nonquote_sigil_delimiter)
}

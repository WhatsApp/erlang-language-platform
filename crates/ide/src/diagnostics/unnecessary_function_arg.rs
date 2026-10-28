/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: unnecessary_function_arg
//!
//! Return a diagnostic if every clause of a non-exported function ignores
//! the argument in some position, and every reference to the function is a
//! local call in its own module.

use std::borrow::Cow;

use elp_ide_db::SearchScope;
use elp_ide_db::SymbolDefinition;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::helpers::get_call;
use elp_syntax::AstNode;
use elp_syntax::SyntaxKind;
use elp_syntax::ast;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::AnyExpr;
use hir::Body;
use hir::CallTarget;
use hir::Expr;
use hir::FormIdx;
use hir::FunctionDef;
use hir::InFile;
use hir::InFunctionClauseBody;
use hir::Literal;
use hir::NameArity;
use hir::Pat;
use hir::PatId;
use hir::Semantic;
use hir::Strategy;
use hir::Term;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::known;
use hir::resolver::Resolver;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::DiagnosticTag;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;

pub(crate) struct UnnecessaryFunctionArgLinter;

impl Linter for UnnecessaryFunctionArgLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnnecessaryFunctionArg
    }

    fn description(&self) -> &'static str {
        "this argument is ignored by every clause and can be removed"
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Context {
    name: String,
    arity: u32,
    position: usize,
}

impl GenericLinter for UnnecessaryFunctionArgLinter {
    type Context = Context;

    fn matches(&self, ctx: &LinterContext) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        if may_export_all_or_transform(sema, file_id) || is_parse_transform(sema, file_id) {
            return Some(Vec::new());
        }
        let named_in_attributes = functions_named_in_attributes(sema, file_id);
        let def_map = sema.def_map_local(file_id);
        let mut external_files: Option<Vec<FileId>> = None;
        let mut hir_calls: Option<FxHashMap<NameArity, HirUses>> = None;
        let mut res = Vec::new();
        for (name_arity, fun_def) in def_map.get_functions() {
            if fun_def.exported
                || def_map.is_deprecated(name_arity)
                || fun_def.file.file_id != file_id
                || name_arity.arity() == 0
                || fun_def
                    .function_clauses
                    .iter()
                    .any(|clause| clause.pp_ctx.cond().is_some())
            {
                // A function under conditional compilation may use the
                // argument in an inactive branch.
                continue;
            }
            let Some(clauses) = ast_clauses(sema, fun_def) else {
                continue;
            };
            let positions = ignored_positions(sema, file_id, fun_def, name_arity.arity());
            if positions.is_empty() {
                continue;
            }
            if named_in_attributes.contains(name_arity) {
                // E.g. `-nifs`, `-compile({inline, ...})`, or an `-export` in
                // an inactive preprocessor branch.
                continue;
            }
            let external_files =
                external_files.get_or_insert_with(|| files_mentioning_module(sema, file_id));
            let Some(calls) = local_call_count(sema, file_id, fun_def, external_files) else {
                continue;
            };
            let hir_calls = hir_calls.get_or_insert_with(|| hir_local_calls(sema, file_id));
            match hir_calls.get(name_arity) {
                // Called through a macro or `?FUNCTION_NAME`, or used as `fun f/N`.
                Some(HirUses::Calls(n)) if *n > calls => continue,
                Some(HirUses::Captured) => continue,
                _ => {}
            }
            for position in positions {
                let Some(arg) = clauses[0].args().and_then(|args| args.args().nth(position)) else {
                    continue;
                };
                res.push(GenericLinterMatchContext {
                    range: FileRange {
                        file_id,
                        range: arg.syntax().text_range(),
                    },
                    context: Context {
                        name: name_arity.name().to_string(),
                        arity: name_arity.arity(),
                        position,
                    },
                });
            }
        }
        Some(res)
    }

    fn match_description(&self, context: &Context) -> Cow<'_, str> {
        Cow::Owned(format!(
            "Argument {} of {}/{} is ignored by every clause and only passed by local calls, so it can be removed.",
            context.position + 1,
            context.name,
            context.arity
        ))
    }

    fn tag(&self, _context: &Context) -> Option<DiagnosticTag> {
        Some(DiagnosticTag::Unused)
    }
}

pub(crate) static LINTER: UnnecessaryFunctionArgLinter = UnnecessaryFunctionArgLinter;

/// The syntactic clauses of the function, if they are all plain clauses
/// (not generated by a macro).
fn ast_clauses(sema: &Semantic, fun_def: &FunctionDef) -> Option<Vec<ast::FunctionClause>> {
    fun_def
        .source(sema.db.upcast())
        .into_iter()
        .map(|fun_decl| match fun_decl.clause()? {
            ast::FunctionOrMacroClause::FunctionClause(clause) => Some(clause),
            ast::FunctionOrMacroClause::MacroCallExpr(_) => None,
        })
        .collect()
}

/// Whether a `-compile` attribute, possibly in an inactive preprocessor
/// branch, mentions `export_all` or `parse_transform`.
fn may_export_all_or_transform(sema: &Semantic, file_id: FileId) -> bool {
    sema.form_list(file_id).compile_attributes().any(|(id, _)| {
        let attribute = sema.db.compile_body(InFile::new(file_id, id));
        attribute.body.terms.iter().any(|(_, term)| match term {
            Term::Literal(Literal::Atom(atom)) => {
                let name = atom.as_name();
                name == *known::export_all || name == *known::parse_transform
            }
            _ => false,
        })
    })
}

/// Whether the module defines `parse_transform/2`. A parse transform can
/// copy the clauses of its own functions into the modules it compiles, where
/// their local calls bind to functions of that module.
fn is_parse_transform(sema: &Semantic, file_id: FileId) -> bool {
    sema.def_map_local(file_id)
        .get_function(&NameArity::new(known::parse_transform.clone(), 2))
        .is_some()
}

/// Functions named as `f/N` or `{f, N}` by an attribute, including
/// attributes in inactive preprocessor branches.
fn functions_named_in_attributes(sema: &Semantic, file_id: FileId) -> FxHashSet<NameArity> {
    let form_list = sema.form_list(file_id);
    form_list
        .forms()
        .iter()
        .flat_map(|form| match *form {
            FormIdx::Export(id) => form_list[id]
                .entries
                .clone()
                .map(|entry| form_list[entry].name.clone())
                .collect(),
            FormIdx::Attribute(id) => {
                name_arity_terms(&sema.db.attribute_body(InFile::new(file_id, id)).body)
            }
            FormIdx::CompileOption(id) => {
                name_arity_terms(&sema.db.compile_body(InFile::new(file_id, id)).body)
            }
            _ => Vec::new(),
        })
        .collect()
}

/// The `{f, N}` terms of an attribute body, where `f/N` is lowered as
/// `{f, N}`.
fn name_arity_terms(body: &Body) -> Vec<NameArity> {
    body.terms
        .iter()
        .filter_map(|(_, term)| {
            let Term::Tuple { exprs } = term else {
                return None;
            };
            let [name, arity] = exprs.as_slice() else {
                return None;
            };
            let (Term::Literal(Literal::Atom(name)), Term::Literal(Literal::Integer(arity))) =
                (&body[*name], &body[*arity])
            else {
                return None;
            };
            Some(NameArity::new(name.as_name(), arity.value.try_into().ok()?))
        })
        .collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HirUses {
    Calls(usize),
    Captured,
}

/// Unqualified calls and `fun f/N` references to each local function in the
/// lowered bodies of the module, where macros, including `?FUNCTION_NAME`,
/// are expanded.
fn hir_local_calls(sema: &Semantic, file_id: FileId) -> FxHashMap<NameArity, HirUses> {
    let def_map = sema.def_map_local(file_id);
    let mut uses: FxHashMap<NameArity, HirUses> = FxHashMap::default();
    for (_, clause_def) in def_map.get_function_clauses() {
        let in_clause = clause_def.in_clause(sema, clause_def);
        in_clause.fold_clause(
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            (),
            &mut |(), ctx| match ctx.item {
                AnyExpr::Expr(Expr::Call {
                    target: CallTarget::Local { name },
                    args,
                }) => {
                    if let Some(name) = in_clause.as_atom_name(&name) {
                        let entry = uses
                            .entry(NameArity::new(name, args.len() as u32))
                            .or_insert(HirUses::Calls(0));
                        if let HirUses::Calls(n) = entry {
                            *n += 1;
                        }
                    }
                }
                AnyExpr::Expr(Expr::CaptureFun {
                    target: CallTarget::Local { name },
                    arity,
                }) => {
                    if let Some(name) = in_clause.as_atom_name(&name)
                        && let Expr::Literal(Literal::Integer(arity)) = &in_clause[arity]
                        && let Ok(arity) = u32::try_from(arity.value)
                    {
                        uses.insert(NameArity::new(name, arity), HirUses::Captured);
                    }
                }
                _ => {}
            },
        );
    }
    uses
}

/// Argument positions that every clause of the function ignores: the
/// parameter is `_`, or a variable with no other occurrence in its clause.
fn ignored_positions(
    sema: &Semantic,
    file_id: FileId,
    fun_def: &FunctionDef,
    arity: u32,
) -> Vec<usize> {
    let def_map = sema.def_map_local(file_id);
    let mut ignored: Vec<usize> = (0..arity as usize).collect();
    for clause_id in &fun_def.function_clause_ids {
        let Some(clause_def) = def_map.function_clause(&InFile::new(file_id, *clause_id)) else {
            return Vec::new();
        };
        let in_clause = clause_def.in_clause(sema, clause_def);
        let pats = &in_clause.body.clause.pats;
        if pats.len() != arity as usize {
            return Vec::new();
        }
        let resolver = in_clause.resolver();
        let occurrences = var_occurrences(&in_clause, &resolver);
        ignored.retain(|position| {
            let pat_id = pats[*position];
            match &in_clause[pat_id] {
                Pat::Var(var) if var.as_string() == "_" => true,
                Pat::Var(var) => {
                    resolver.resolve_pat_id(var, pat_id).map(Vec::as_slice) == Some(&[pat_id])
                        && occurrences.by_binding.get(&pat_id) == Some(&1)
                        && !occurrences.unresolved.contains(var)
                }
                _ => false,
            }
        });
        if ignored.is_empty() {
            break;
        }
    }
    ignored
}

/// Other files of the application (including its tests) whose text
/// mentions the module name. A reference to a non-exported function from
/// another module, e.g. a test suite that exports it at runtime, can only
/// appear in one of them; other applications are not searched.
fn files_mentioning_module(sema: &Semantic, file_id: FileId) -> Vec<FileId> {
    let Some(module_name) = sema.module_name(file_id) else {
        return Vec::new();
    };
    let Some(app_data) = sema.db.file_app_data(file_id) else {
        return Vec::new();
    };
    // For Buck applications `dir` is the source directory; the tests live
    // next to it.
    let app_dir = match app_data.dir.file_name() {
        Some("src") => app_data.dir.parent().unwrap_or(&app_data.dir),
        _ => &app_data.dir,
    };
    let project_data = sema
        .db
        .project_data(app_data.project_id)
        .project_data(sema.db);
    let app_files = project_data.source_roots.iter().flat_map(|source_root_id| {
        let source_root = sema.db.source_root(*source_root_id).source_root(sema.db);
        source_root
            .iter()
            .filter(|other| {
                *other != file_id
                    && source_root
                        .path_for_file(other)
                        .and_then(|path| path.as_path())
                        .is_some_and(|path| path.starts_with(app_dir))
            })
            .collect::<Vec<_>>()
    });
    SearchScope::files(app_files)
        .retain_files_containing(sema.db, module_name.as_str())
        .file_ids()
        .collect()
}

/// Above this many textual occurrences of the function name in its file,
/// classifying each occurrence costs too much, and the function is skipped.
const MAX_NAME_OCCURRENCES: usize = 64;

/// The number of calls to the function, if every reference to it is an
/// unqualified call with the right number of arguments in its own file, or
/// its spec.
fn local_call_count(
    sema: &Semantic,
    file_id: FileId,
    fun_def: &FunctionDef,
    external_files: &[FileId],
) -> Option<usize> {
    let name_occurrences = sema
        .db
        .file(file_id)
        .text(sema.db)
        .matches(fun_def.name.name().as_str())
        .count();
    if name_occurrences > MAX_NAME_OCCURRENCES {
        return None;
    }
    let external_scope = SearchScope::files(external_files.iter().copied())
        .retain_files_containing(sema.db, fun_def.name.name().as_str());
    let def_map = sema.def_map(file_id);
    let scope = SearchScope::files(
        std::iter::once(file_id)
            .chain(def_map.get_included_files())
            .chain(external_scope.file_ids()),
    );
    let spec_range = fun_def
        .spec
        .as_ref()
        .map(|spec| spec.range(sema.db.upcast()));
    let mut usages = SymbolDefinition::Function(fun_def.clone()).usages(sema);
    usages.set_scope(&scope);
    let references = usages.all();
    let mut calls = 0;
    for (reference_file_id, names) in references.iter() {
        if reference_file_id != file_id {
            return None;
        }
        for name in names {
            let name_range = name.syntax().text_range();
            if spec_range.is_some_and(|range| range.contains_range(name_range)) {
                continue;
            }
            let call = get_call(name.syntax())?;
            let callee = call.expr()?;
            if callee.syntax().text_range() != name_range
                || call
                    .syntax()
                    .parent()
                    .is_some_and(|parent| parent.kind() == SyntaxKind::REMOTE)
                || call.args()?.args().count() != fun_def.name.arity() as usize
            {
                return None;
            }
            calls += 1;
        }
    }
    Some(calls)
}

#[derive(Default)]
struct VarOccurrences {
    /// For every pattern binding a variable, the number of variable
    /// occurrences in the clause, itself included, that resolve to it.
    by_binding: FxHashMap<PatId, usize>,
    /// Variables with an occurrence that does not resolve, e.g. inside
    /// a construct the lowering does not support. They may be uses.
    unresolved: FxHashSet<hir::Var>,
}

/// Macro expansions and macro call arguments are both part of the
/// lowered body, so occurrences in either count.
fn var_occurrences<T>(in_clause: &InFunctionClauseBody<T>, resolver: &Resolver) -> VarOccurrences {
    let mut occurrences = VarOccurrences::default();
    let mut record = |var: &hir::Var, resolution: Option<&Vec<PatId>>| match resolution {
        Some(pat_ids) => pat_ids
            .iter()
            .for_each(|pat_id| *occurrences.by_binding.entry(*pat_id).or_default() += 1),
        None => {
            occurrences.unresolved.insert(*var);
        }
    };
    in_clause.body_exprs().for_each(|(expr_id, expr)| {
        if let Expr::Var(var) = expr {
            record(var, resolver.resolve_expr_id(var, expr_id));
        }
    });
    in_clause.body_pats().for_each(|(pat_id, pat)| {
        if let Pat::Var(var) = pat {
            record(var, resolver.resolve_pat_id(var, pat_id));
        }
    });
    occurrences
}

#[cfg(test)]
mod tests {
    use elp_ide_db::DiagnosticCode;

    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;

    fn config() -> DiagnosticsConfig {
        DiagnosticsConfig::default()
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::UnexportedFunction)
            .disable(DiagnosticCode::UnusedRecordField)
    }

    #[test]
    fn ignored_by_every_clause() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            run(X) -> step(X, ctx).
            step(a, _Ctx) -> 1;
            %%      ^^^^ warning: W0088: Argument 2 of step/2 is ignored by every clause and only passed by local calls, so it can be removed.
            %%         | 💡 <suppression>
            step(_, _) -> 2.
            "#,
        );
    }

    #[test]
    fn used_in_one_clause() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            run(X) -> step(X, ctx).
            step(a, _Ctx) -> 1;
            step(_, Ctx) -> Ctx.
            "#,
        );
    }

    #[test]
    fn matched_in_one_clause() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            run(X) -> step(X, ctx).
            step(a, ctx) -> 1;
            step(_, _) -> 2.
            "#,
        );
    }

    #[test]
    fn exported_function() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([step/2]).
            step(X, _Ctx) -> X.
            "#,
        );
    }

    #[test]
    fn export_all() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -compile(export_all).
            step(X, _Ctx) -> X.
            "#,
        );
    }

    #[test]
    fn used_as_fun_value() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            run(L) -> lists:map(fun step/2, L).
            step(X, _Ctx) -> X.
            "#,
        );
    }

    #[test]
    fn called_qualified() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            run(X) -> ?MODULE:step(X, ctx).
            step(X, _Ctx) -> X.
            "#,
        );
    }

    #[test]
    fn referenced_from_another_module() {
        check_diagnostics_with_config(
            config(),
            r#"
            //- /src/main.erl
            -module(main).
            -export([run/1]).
            run(X) -> step(X, ctx).
            step(X, _Ctx) -> X.
            //- /test/main_SUITE.erl
            -module(main_SUITE).
            -export([t/0]).
            t() -> F = fun main:step/2, F(1, 2).
            "#,
        );
    }

    #[test]
    fn argument_used_through_macro() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            -define(LOG(X), io:format("~p~n", [{X, Ctx}])).
            run(X) -> step(X, ctx).
            step(X, Ctx) -> ?LOG(X).
            "#,
        );
    }

    #[test]
    fn nif_stub() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            -nifs([step/2]).
            run(X) -> step(X, ctx).
            step(_X, _Ctx) -> erlang:nif_error(not_loaded).
            "#,
        );
    }

    #[test]
    fn exported_in_inactive_branch() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            -ifdef(TEST).
            -export([step/2]).
            -endif.
            run(X) -> step(X, ctx).
            step(X, _Ctx) -> X.
            "#,
        );
    }

    #[test]
    fn conditional_compilation() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            run(X) -> step(X, ctx).
            -ifndef(TEST).
            step(X, _Ctx) -> X.
            -else.
            step(X, Ctx) -> {X, Ctx}.
            -endif.
            "#,
        );
    }

    #[test]
    fn called_through_function_name_macro() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            run(X) -> step(X, ctx).
            step(0, _) -> 0;
            step(N, _) -> ?FUNCTION_NAME(N - 1, ctx).
            "#,
        );
    }

    #[test]
    fn called_through_header_macro() {
        check_diagnostics_with_config(
            config(),
            r#"
            //- /src/main.hrl
            -define(S(A), step(A, ctx)).
            //- /src/main.erl
            -module(main).
            -include("main.hrl").
            -export([run/1]).
            run(X) -> ?S(X).
            step(X, _Ctx) -> X.
            "#,
        );
    }

    #[test]
    fn named_in_attribute_as_tuple() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            -compile({inline, [{step, 2}]}).
            run(X) -> step(X, ctx).
            step(X, _Ctx) -> X.
            "#,
        );
    }

    #[test]
    fn name_mentioned_in_prose() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            -doc "Calls do_step/2.".
            run(X) -> step(X, ctx).
            step(X, _Ctx) -> X.
            %%      ^^^^ warning: W0088: Argument 2 of step/2 is ignored by every clause and only passed by local calls, so it can be removed.
            %%         | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn named_in_string_attribute() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            -doc "Calls step/2.".
            run(X) -> step(X, ctx).
            step(X, _Ctx) -> X.
            %%      ^^^^ warning: W0088: Argument 2 of step/2 is ignored by every clause and only passed by local calls, so it can be removed.
            %%         | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn named_in_attribute_quoted() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            -compile({inline, ['step' / 2]}).
            run(X) -> step(X, ctx).
            step(X, _Ctx) -> X.
            "#,
        );
    }

    #[test]
    fn named_in_attribute_through_macro() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            -define(INLINED, [step/2]).
            -compile({inline, ?INLINED}).
            run(X) -> step(X, ctx).
            step(X, _Ctx) -> X.
            "#,
        );
    }

    #[test]
    fn parse_transform() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([parse_transform/2, template/1]).
            parse_transform(Forms, Options) -> rewrite(Forms, Options).
            rewrite(Forms, _Options) -> Forms.
            template(X) -> check(X, ctx).
            check(_, _) -> ok.
            "#,
        );
    }

    #[test]
    fn export_all_in_inactive_branch() {
        check_diagnostics_with_config(
            config(),
            r#"
            -module(main).
            -export([run/1]).
            -ifdef(TEST).
            -compile(export_all).
            -endif.
            run(X) -> step(X, ctx).
            step(X, _Ctx) -> X.
            "#,
        );
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_syntax::ast;
use elp_syntax::ast::in_erlang_module;
use elp_syntax::AstNode;
use elp_syntax::SmolStr;
use elp_syntax::SyntaxKind;
use elp_syntax::TextRange;
use elp_types_db::eqwalizer;
use fxhash::FxHashMap;
use hir::db::InternDatabase;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::Body;
use hir::CallTarget;
use hir::Expr;
use hir::ExprId;
use hir::FunctionDef;
use hir::HirIdx;
use hir::InFile;
use hir::InFunctionClauseBody;
use hir::Semantic;
use hir::Strategy;
use serde::Deserialize;
use serde::Serialize;

use crate::diagnostics::Diagnostic;
use crate::FileId;

// Given an expression that represents a statement, return a text range that covers
// the statement in full. This means:
//
// - If the expression is followed by a comma, include the comma in the range, and all
//   whitespace following the comma
// - If the expression is not followed by a comma, but is preceded by a comma, include
//   the preceding comma in the range, and all whitespace before the comma
// - Otherwise, we just use the range of the expression
//
// We typically want to have the statement range in order to be able to delete the statement,
// remove an element from a list, etc.
pub(crate) fn statement_range(expr: &ast::Expr) -> TextRange {
    let node = expr.syntax();
    let node_range = node.text_range();

    let mut right = node.last_token().and_then(|tok| tok.next_token());
    let mut searching_for_comma = true;
    let final_node_range;

    let mut node_range_right = node_range;
    while let Some(tok) = right {
        match tok.kind() {
            SyntaxKind::WHITESPACE => node_range_right = tok.text_range(),
            SyntaxKind::ANON_COMMA if searching_for_comma => {
                node_range_right = tok.text_range();
                searching_for_comma = false
            }
            _ => break,
        }
        right = tok.next_token();
    }

    if !searching_for_comma {
        final_node_range = node_range_right;
    } else {
        // We didn't find a trailing comma, so let's see if there is a preceding comma

        let mut left = node.first_token().and_then(|tok| tok.prev_token());

        let mut node_range_left = node_range;
        while let Some(tok) = left {
            match tok.kind() {
                SyntaxKind::WHITESPACE => node_range_left = tok.text_range(),
                SyntaxKind::ANON_COMMA if searching_for_comma => {
                    node_range_left = tok.text_range();
                    searching_for_comma = false
                }
                _ => break,
            }
            left = tok.prev_token();
        }

        if !searching_for_comma {
            final_node_range = node_range_left;
        } else {
            // We didn't find a trailing nor preceding comma, this is a singleton
            final_node_range = node_range_left.cover(node_range_right);
        }
    }

    node_range.cover(final_node_range)
}

pub(crate) fn var_name_starts_with_underscore(db: &dyn InternDatabase, var: &hir::Var) -> bool {
    var.as_string(db).starts_with('_')
}

pub(crate) fn is_only_place_where_var_is_defined(
    sema: &Semantic,
    var: &InFunctionClauseBody<AnyExprId>,
) -> bool {
    check_is_only_place_where_var_is_defined(sema, var).is_some()
}

pub(crate) fn var_has_no_references(
    sema: &Semantic,
    var: &InFunctionClauseBody<AnyExprId>,
) -> bool {
    check_var_has_no_references(sema, var).is_some()
}

pub(crate) fn check_is_only_place_where_var_is_defined_ast(
    sema: &Semantic,
    var: InFile<&ast::Var>,
) -> Option<()> {
    let usages = sema.find_local_usages_ast(var)?;
    let num_definitions = usages
        .iter()
        .filter(|v| sema.to_def(var.with_value(*v)).map_or(false, is_definition))
        .count();
    if num_definitions == 1 { Some(()) } else { None }
}

pub(crate) fn check_is_only_place_where_var_is_defined(
    sema: &Semantic,
    var: &InFunctionClauseBody<AnyExprId>,
) -> Option<()> {
    let usages = sema.find_local_usages(var)?;
    let num_definitions = usages
        .iter()
        .filter(|(id, _v)| var.to_var_def_any(*id).map_or(false, is_definition))
        .count();
    if num_definitions == 1 { Some(()) } else { None }
}

pub(crate) fn check_var_has_no_references_ast(
    sema: &Semantic,
    var: InFile<&ast::Var>,
) -> Option<()> {
    let usages = sema.find_local_usages_ast(var)?;
    let num_definitions = usages
        .iter()
        .filter(|v| {
            sema.to_def(var.with_value(*v))
                .map_or(false, |dor| !is_definition(dor))
        })
        .count();
    if num_definitions == 0 { Some(()) } else { None }
}

pub(crate) fn check_var_has_no_references(
    sema: &Semantic,
    var: &InFunctionClauseBody<AnyExprId>,
) -> Option<()> {
    let usages = sema.find_local_usages(var)?;
    let definition_found = usages.iter().any(|(id, _v)| {
        var.to_var_def_any(*id)
            .map_or(false, |dor| !is_definition(dor))
    });
    if !definition_found { Some(()) } else { None }
}

pub(crate) fn check_var_has_references(sema: &Semantic, var: InFile<&ast::Var>) -> Option<()> {
    match check_var_has_no_references_ast(sema, var) {
        Some(()) => None,
        None => Some(()),
    }
}

fn is_definition<D, R>(def: hir::DefinitionOrReference<D, R>) -> bool {
    match def {
        hir::DefinitionOrReference::Definition { .. } => true,
        hir::DefinitionOrReference::Reference { .. } => false,
    }
}

#[derive(Debug, Clone)]
pub struct FunctionMatcher<'a, T> {
    match_any: Option<(&'a FunctionMatch, &'a T)>,
    labels_full: FxHashMap<Option<SmolStr>, (&'a FunctionMatch, &'a T)>,
    labels_full_typed:
        FxHashMap<Option<SmolStr>, (&'a Vec<eqwalizer::types::Type>, &'a FunctionMatch, &'a T)>,
    labels_mf: FxHashMap<Option<SmolStr>, (&'a FunctionMatch, &'a T)>,
    labels_m: FxHashMap<Option<SmolStr>, (&'a FunctionMatch, &'a T)>,
}

impl<'a, T> FunctionMatcher<'a, T> {
    pub fn new(mfas: &'a [(&'a FunctionMatch, T)]) -> FunctionMatcher<'a, T> {
        let mut labels_full = FxHashMap::default();
        let mut labels_full_typed = FxHashMap::default();
        let mut labels_mf = FxHashMap::default();
        let mut labels_m = FxHashMap::default();
        let mut match_any = None;

        mfas.iter().for_each(|(c, t)| match c {
            FunctionMatch::Any => {
                match_any = Some((*c, t));
            }
            FunctionMatch::MFA(mfa) => {
                if mfa.module == "erlang" && in_erlang_module(&mfa.name, mfa.arity as usize) {
                    labels_full.insert(Some(mfa.short_label().into()), (*c, t));
                }
                labels_full.insert(Some(mfa.label().into()), (*c, t));
            }
            FunctionMatch::TypedMFA { mfa, types } => {
                if mfa.module == "erlang" && in_erlang_module(&mfa.name, mfa.arity as usize) {
                    labels_full_typed.insert(Some(mfa.short_label().into()), (types, *c, t));
                }
                labels_full_typed.insert(Some(mfa.label().into()), (types, *c, t));
            }
            FunctionMatch::MF { module, name } => {
                let label = format!("{}:{}", module, name);
                labels_mf.insert(Some(label.into()), (*c, t));
            }
            FunctionMatch::M { module } => {
                labels_m.insert(Some(module.into()), (*c, t));
            }
        });
        FunctionMatcher {
            match_any,
            labels_full,
            labels_full_typed,
            labels_mf,
            labels_m,
        }
    }

    pub fn get_match(
        &self,
        target: &CallTarget<ExprId>,
        arity: u32,
        args: Option<&[ExprId]>,
        sema: &Semantic,
        body: &Body,
    ) -> Option<(&'a FunctionMatch, &'a T)> {
        self.match_any
            .or_else(|| {
                self.labels_full
                    .get(&target.label(arity, sema, body))
                    .copied()
            })
            .or_else(|| {
                let (types, match_val, t) = self
                    .labels_full_typed
                    .get(&target.label(arity, sema, body))?;
                self.types_match(args?, types, sema, body)
                    .then_some((match_val, t))
            })
            .or_else(|| self.labels_mf.get(&target.label_short(sema, body)).copied())
            .or_else(|| self.labels_mf.get(&target.label_short(sema, body)).copied())
            .or_else(|| match target {
                CallTarget::Local { name: _ } => None,
                CallTarget::Remote { module, name: _ } => {
                    let name = sema.db.lookup_atom(body[*module].as_atom()?);
                    let label = Some(SmolStr::new(format!("{name}")));
                    self.labels_m.get(&label).copied()
                }
            })
    }

    fn types_match(
        &self,
        args: &[ExprId],
        types: &[eqwalizer::types::Type],
        sema: &Semantic,
        body: &Body,
    ) -> bool {
        args.len() == types.len()
            && args.iter().zip(types.iter()).all(|(expr_id, eq_type)| {
                sema.expr_type(body, expr_id)
                    .map(|t| &t == eq_type)
                    .unwrap_or(false)
            })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(tag = "type")]
#[allow(clippy::upper_case_acronyms)]
pub enum FunctionMatch {
    Any,
    MFA(MFA),
    MF {
        module: String,
        name: String,
    },
    M {
        module: String,
    },
    TypedMFA {
        mfa: MFA,
        types: Vec<eqwalizer::types::Type>,
    },
}

impl FunctionMatch {
    pub fn any() -> FunctionMatch {
        FunctionMatch::Any
    }

    pub fn mfa(m: &str, f: &str, arity: u32) -> FunctionMatch {
        FunctionMatch::MFA(MFA {
            module: m.into(),
            name: f.into(),
            arity,
        })
    }

    pub fn typed_mfa(m: &str, f: &str, types: Vec<eqwalizer::types::Type>) -> FunctionMatch {
        let mfa = MFA {
            module: m.into(),
            name: f.into(),
            arity: types.len() as u32,
        };
        FunctionMatch::TypedMFA { mfa, types }
    }

    pub fn mfas(m: &str, f: &str, arity: Vec<u32>) -> Vec<FunctionMatch> {
        arity
            .into_iter()
            .map(|a| {
                FunctionMatch::MFA(MFA {
                    module: m.into(),
                    name: f.into(),
                    arity: a,
                })
            })
            .collect()
    }

    pub fn mf(m: &str, f: &str) -> FunctionMatch {
        FunctionMatch::MF {
            module: m.into(),
            name: f.into(),
        }
    }

    pub fn m(m: &str) -> FunctionMatch {
        FunctionMatch::M { module: m.into() }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
#[allow(clippy::upper_case_acronyms)]
pub struct MFA {
    pub module: String,
    pub name: String,
    pub arity: u32,
}

impl MFA {
    pub fn from_call_target(
        target: &CallTarget<ExprId>,
        arity: u32,
        sema: &Semantic,
        body: &Body,
        file_id: FileId,
    ) -> Option<MFA> {
        let call_target = target.resolve_call(arity, sema, file_id, body)?;
        let call_module = call_target.module?;
        let na = call_target.name;
        Some(MFA {
            module: call_module.to_quoted_string(),
            name: na.name().to_quoted_string(),
            arity: na.arity(),
        })
    }

    #[cfg(test)] // @oss-only
    pub fn new(m: &str, f: &str, arity: u32) -> MFA {
        MFA {
            module: m.to_string(),
            name: f.to_string(),
            arity,
        }
    }

    pub fn label(&self) -> String {
        format!("{}:{}/{}", self.module, self.name, self.arity)
    }

    pub fn short_label(&self) -> String {
        format!("{}/{}", self.name, self.arity)
    }
}

pub struct CheckCallCtx<'a, T> {
    pub mfa: &'a FunctionMatch,
    pub parents: &'a Vec<HirIdx>,
    pub target: &'a CallTarget<ExprId>,
    pub t: &'a T,
    pub args: &'a [ExprId],
    pub in_clause: &'a InFunctionClauseBody<'a, &'a FunctionDef>,
}

/// Check a specific call instance, and return extra info for a
/// diagnostic and fix if needed.
pub type CheckCall<'a, T, U> = &'a dyn Fn(CheckCallCtx<T>) -> Option<U>;

pub struct MakeDiagCtx<'a, U> {
    pub sema: &'a Semantic<'a>,
    pub def_fb: &'a InFunctionClauseBody<'a, &'a FunctionDef>,
    pub target: &'a CallTarget<ExprId>,
    pub args: &'a [ExprId],
    pub range: TextRange,
    pub extra: &'a U,
}

pub type MakeDiag<'a, T> = &'a dyn Fn(MakeDiagCtx<T>) -> Option<Diagnostic>;

pub(crate) fn find_call_in_function<T, U>(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    def: &FunctionDef,
    mfas: &[(&FunctionMatch, T)],
    check_call: CheckCall<T, U>,
    make_diag: MakeDiag<U>,
) -> Option<()> {
    let def_fb = def.in_function_body(sema, def);
    let matcher = FunctionMatcher::new(mfas);
    def_fb
        .clone()
        .fold_function(Strategy::VisibleMacros, (), &mut |acc, clause_id, ctx| {
            if let AnyExpr::Expr(Expr::Call { target, args }) = ctx.item {
                if let Some((mfa, t)) = matcher.get_match(
                    &target,
                    args.len() as u32,
                    Some(&args),
                    sema,
                    &def_fb.body(clause_id),
                ) {
                    let in_clause = &def_fb.in_clause(clause_id);
                    let context = CheckCallCtx {
                        mfa,
                        parents: ctx.parents,
                        t,
                        target: &target,
                        args: &args,
                        in_clause,
                    };
                    if let Some(extra) = check_call(context) {
                        // Got one.
                        let call_expr_id = if let Some(expr_id) = ctx.in_macro {
                            expr_id.idx
                        } else {
                            ctx.item_id
                        };
                        if let Some(range) = &def_fb.range_for_any(clause_id, call_expr_id) {
                            if let Some(diag) = make_diag(MakeDiagCtx {
                                sema,
                                def_fb: in_clause,
                                target: &target,
                                args: &args,
                                extra: &extra,
                                range: *range,
                            }) {
                                diags.push(diag)
                            }
                        }
                    }
                }
            };
            acc
        });
    Some(())
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use elp_ide_db::elp_base_db::FileId;
    use elp_ide_db::elp_base_db::SourceDatabase;
    use elp_syntax::algo::find_node_at_offset;
    use elp_syntax::ast;
    use hir::FunctionDef;
    use hir::InFile;
    use hir::Semantic;

    use super::find_call_in_function;
    use super::FunctionMatch;
    use super::MakeDiagCtx;
    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::diagnostics::Severity;
    use crate::fixture;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix_with_config;
    use crate::AnalysisHost;

    fn check_functions(
        diags: &mut Vec<Diagnostic>,
        sema: &Semantic,
        file_id: FileId,
        match_spec: &Vec<Vec<FunctionMatch>>,
    ) {
        sema.def_map(file_id)
            .get_functions()
            .for_each(|(_, def)| check_function(diags, sema, def, match_spec.clone()));
    }

    fn check_function(
        diags: &mut Vec<Diagnostic>,
        sema: &Semantic,
        def: &FunctionDef,
        match_spec: Vec<Vec<FunctionMatch>>,
    ) {
        let matches = match_spec
            .into_iter()
            .flatten()
            .collect::<Vec<FunctionMatch>>();

        process_matches(diags, sema, def, &matches);
    }

    fn process_matches(
        diags: &mut Vec<Diagnostic>,
        sema: &Semantic,
        def: &FunctionDef,
        bad: &[FunctionMatch],
    ) {
        let mfas = bad.iter().map(|b| (b, ())).collect::<Vec<_>>();
        find_call_in_function(
            diags,
            sema,
            def,
            &mfas,
            &move |_ctx| Some("Diagnostic Message"),
            &move |MakeDiagCtx {
                       sema,
                       def_fb,
                       extra,
                       range,
                       ..
                   }: MakeDiagCtx<'_, &str>| {
                let diag =
                    Diagnostic::new(DiagnosticCode::AdHoc("test".to_string()), *extra, range)
                        .with_severity(Severity::Warning)
                        .with_ignore_fix(sema, def_fb.file_id());
                Some(diag)
            },
        );
    }

    #[track_caller]
    fn check_adhoc_function_match(match_spec: &Vec<Vec<FunctionMatch>>, fixture: &str) {
        check_diagnostics_with_config(
            DiagnosticsConfig::default()
                .disable(DiagnosticCode::MissingCompileWarnMissingSpec)
                .disable(DiagnosticCode::CrossNodeEval)
                .disable(DiagnosticCode::UndefinedFunction)
                .set_ad_hoc_semantic_diagnostics(vec![&|acc, sema, file_id, _ext| {
                    check_functions(acc, sema, file_id, match_spec)
                }]),
            fixture,
        );
    }

    #[track_caller]
    fn check_adhoc_function_fix(
        match_spec: &Vec<Vec<FunctionMatch>>,
        fixture_before: &str,
        fixture_after: &str,
    ) {
        check_fix_with_config(
            DiagnosticsConfig::default()
                .disable(DiagnosticCode::MissingCompileWarnMissingSpec)
                .disable(DiagnosticCode::CrossNodeEval)
                .disable(DiagnosticCode::UndefinedFunction)
                .set_ad_hoc_semantic_diagnostics(vec![&|acc, sema, file_id, _ext| {
                    check_functions(acc, sema, file_id, match_spec)
                }]),
            fixture_before,
            fixture_after,
        );
    }

    // -----------------------------------------------------------------

    #[test]
    fn find_call_in_function_1() {
        check_adhoc_function_match(
            &vec![FunctionMatch::mfas("foo", "fire_bombs", vec![1, 2])],
            r#"
            -module(main).

            bar(Config) ->
                foo:fire_bombs(Config),
            %%  ^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Diagnostic Message
                foo:fire_bombs(Config, zz).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Diagnostic Message
            "#,
        )
    }

    #[test]
    fn find_call_in_function_erlang_module() {
        check_adhoc_function_match(
            &vec![FunctionMatch::mfas("erlang", "spawn", vec![2, 4])],
            r#"
            -module(main).

            bar(Node) ->
                erlang:spawn(fun() -> ok end),
                spawn(fun() -> ok end),
                erlang:spawn(Node, fun() -> ok end),
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Diagnostic Message
                spawn(Node, fun() -> ok end),
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Diagnostic Message
                erlang:spawn(Node, mod, fff, []),
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Diagnostic Message
                spawn(Node, mod, fff, []).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Diagnostic Message
            "#,
        )
    }

    #[test]
    fn find_call_in_function_erlang_module_2() {
        check_adhoc_function_match(
            // Not actually in the erlang module
            &vec![FunctionMatch::mfas("erlang", "spawn", vec![0])],
            r#"
            -module(main).

            bar() ->
                erlang:spawn(),
            %%  ^^^^^^^^^^^^^^ 💡 warning: Diagnostic Message
                spawn().
            "#,
        )
    }

    #[test]
    fn find_call_module_function() {
        check_adhoc_function_match(
            &vec![vec![FunctionMatch::mf("foo", "bar")]],
            r#"
            -module(main).

            bar() ->
                foo:bar(),
            %%  ^^^^^^^^^ 💡 warning: Diagnostic Message
                foo:bar(x),
            %%  ^^^^^^^^^^ 💡 warning: Diagnostic Message
                foo:bar(x,y),
            %%  ^^^^^^^^^^^^ 💡 warning: Diagnostic Message
                baz:bar().
            "#,
        )
    }

    #[test]
    fn find_call_module_only() {
        check_adhoc_function_match(
            &vec![vec![FunctionMatch::m("foo")]],
            r#"
            -module(main).

            bar() ->
                foo:bar(),
            %%  ^^^^^^^^^ 💡 warning: Diagnostic Message
                baz:bar(x),
                foo:florgle(x,y).
            %%  ^^^^^^^^^^^^^^^^ 💡 warning: Diagnostic Message
            "#,
        )
    }

    #[test]
    fn find_call_any() {
        check_adhoc_function_match(
            &vec![vec![FunctionMatch::Any]],
            r#"
            -module(main).

            bar() ->
                foo:bar(),
            %%  ^^^^^^^^^ 💡 warning: Diagnostic Message
                baz:bar(x),
            %%  ^^^^^^^^^^ 💡 warning: Diagnostic Message
                local(x,y).
            %%  ^^^^^^^^^^ 💡 warning: Diagnostic Message
            local(A,B) -> {A,B}.
            "#,
        )
    }

    #[test]
    fn ignore_fix_1() {
        check_adhoc_function_fix(
            &vec![vec![FunctionMatch::m("foo")]],
            r#"
            -module(main).

            bar() ->
                fo~o:bar().
            %%  ^^^^^^^^^ 💡 warning: Diagnostic Message
             "#,
            r#"
            -module(main).

            bar() ->
                % elp:ignore ad-hoc: test (ad-hoc: test)
                foo:bar().
             "#,
        );
    }

    #[test]
    fn ignore_fix_2() {
        check_adhoc_function_fix(
            &vec![vec![FunctionMatch::m("rpc")]],
            r#"
            -module(main).
            foo(Node, M,F,A) ->
               rpc:c~all(Node, M, F, A).
            %% ^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Diagnostic Message
             "#,
            r#"
            -module(main).
            foo(Node, M,F,A) ->
               % elp:ignore ad-hoc: test (ad-hoc: test)
               rpc:call(Node, M, F, A).
             "#,
        );
    }

    #[test]
    fn ignore_fix_3() {
        check_adhoc_function_fix(
            &vec![vec![FunctionMatch::m("rpc")]],
            r#"
            -module(main).
            foo(Node, M,F,A) ->
               baz(rpc:c~all(Node, M, F, A)).
            %%     ^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Diagnostic Message
             "#,
            r#"
            -module(main).
            foo(Node, M,F,A) ->
               % elp:ignore ad-hoc: test (ad-hoc: test)
               baz(rpc:call(Node, M, F, A)).
             "#,
        );
    }

    // -------------------------------------------------------------------

    #[track_caller]
    fn check_type(fixture: &str) {
        let (db, position, _diagnostics_enabled, _guard, expected) =
            fixture::db_annotations(fixture);
        let host = AnalysisHost { db };
        let sema = Semantic::new(&host.db);
        if expected.len() != 1 {
            panic!("Expected exactly one annotation, got {:?}", &expected);
        }

        let analysis = host.analysis();
        let diagnostics = fixture::diagnostics_for(
            &analysis,
            position.file_id,
            &DiagnosticsConfig::default(),
            &_diagnostics_enabled,
        );
        assert!(diagnostics.is_empty());
        let file_syntax = host.db.parse(position.file_id).syntax_node();
        let val: ast::Expr = find_node_at_offset(&file_syntax, position.offset).unwrap();
        let type_info = sema
            .to_expr(InFile::new(position.file_id, &val))
            .map(|in_clause| sema.expr_type(&in_clause.body(), &in_clause.value).unwrap())
            .or_else(|| {
                sema.to_pat(InFile::new(position.file_id, &val))
                    .map(|in_clause| sema.pat_type(&in_clause.body(), &in_clause.value).unwrap())
            });
        if let Some(type_info) = type_info {
            let type_str = format!("{}", type_info);
            if type_str != expected[0].1 {
                panic!("Expected '{}', got '{}'", expected[0].1, type_str);
            }
        } else {
            panic!("could not get type info")
        }
    }

    // -----------------------------------------------------------------

    #[test]
    fn get_type_atom() {
        check_type(
            r#"
            //- eqwalizer
            //- /play/src/bar.erl app:play
                -module(bar).

                -spec baz(atom()) -> atom().
                baz(FF) -> F~F,
            %%             ^^ atom()
                  something_else.
            "#,
        )
    }

    #[test]
    fn get_type_custom() {
        check_type(
            r#"
            //- eqwalizer
            //- /play/src/bar.erl app:play
                -module(bar).

                -type foo() :: foo1 | foo2.

                -spec get_foo() -> foo().
                get_foo() -> foo1.

                baz() -> F~F = get_foo().
            %%           ^^ bar:foo()
            "#,
        )
    }

    #[test]
    fn get_type_string() {
        check_type(
            r#"
            //- eqwalizer
            //- /play/src/bar.erl app:play
                -module(bar).

                -spec get_foo() -> string().
                get_foo() -> "hello".

                baz() -> F~F = get_foo().
            %%           ^^ erlang:string()
            "#,
        )
    }

    #[test]
    fn include_file_tracking() {
        check_type(
            r#"
            //- eqwalizer
            //- /play/src/bar.erl app:play
                -module(bar).
                -include("level1.hrl").

                -spec get_foo() -> ?STRING().
                get_foo() -> "hello".

                baz() -> F~F = get_foo().
            %%           ^^ erlang:string()

            //- /play/src/level1.hrl app:play
            -include("level2.hrl").

            //- /play/src/level2.hrl app:play
            -define(STRING(), string()).
            "#,
        )
    }
}

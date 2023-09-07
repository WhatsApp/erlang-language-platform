/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::iter;

use elp_syntax::ast;
use elp_syntax::ast::in_erlang_module;
use elp_syntax::AstNode;
use elp_syntax::SmolStr;
use elp_syntax::SyntaxElement;
use elp_syntax::SyntaxKind;
use elp_syntax::TextRange;
use fxhash::FxHashMap;
use hir::Body;
use hir::CallTarget;
use hir::Expr;
use hir::ExprId;
use hir::FunctionDef;
use hir::InFile;
use hir::InFunctionBody;
use hir::Semantic;
use hir::Strategy;

use crate::diagnostics::Diagnostic;

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

    let elements_to_the_right = iter::successors(node.next_sibling_or_token(), |n| {
        (*n).next_sibling_or_token()
    });
    let mut searching_for_comma = true;
    let final_node_range;

    let mut node_range_right = node_range;
    for element in elements_to_the_right {
        if let Some(t) = &SyntaxElement::into_token(element) {
            match t.kind() {
                SyntaxKind::WHITESPACE => node_range_right = t.text_range(),
                SyntaxKind::ANON_COMMA if searching_for_comma => {
                    node_range_right = t.text_range();
                    searching_for_comma = false
                }
                _ => break,
            }
        } else {
            break;
        }
    }

    if !searching_for_comma {
        final_node_range = node_range_right;
    } else {
        // We didn't find a trailing comma, so let's see if there is a preceding comma

        let elements_to_the_left = iter::successors(node.prev_sibling_or_token(), |n| {
            (*n).prev_sibling_or_token()
        });

        let mut node_range_left = node_range;
        for element in elements_to_the_left {
            if let Some(t) = &SyntaxElement::into_token(element) {
                match t.kind() {
                    SyntaxKind::WHITESPACE => node_range_left = t.text_range(),
                    SyntaxKind::ANON_COMMA if searching_for_comma => {
                        node_range_left = t.text_range();
                        searching_for_comma = false
                    }
                    _ => break,
                }
            } else {
                break;
            }
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

pub(crate) fn var_name_starts_with_underscore(var: &ast::Var) -> bool {
    var.syntax().to_string().starts_with('_')
}

pub(crate) fn is_only_place_where_var_is_defined(sema: &Semantic, var: InFile<&ast::Var>) -> bool {
    check_is_only_place_where_var_is_defined(sema, var).is_some()
}

pub(crate) fn var_has_no_references(sema: &Semantic, var: InFile<&ast::Var>) -> bool {
    check_var_has_no_references(sema, var).is_some()
}

pub(crate) fn check_is_only_place_where_var_is_defined(
    sema: &Semantic,
    var: InFile<&ast::Var>,
) -> Option<()> {
    let usages = sema.find_local_usages(var)?;
    let num_definitions = usages
        .iter()
        .filter(|v| sema.to_def(var.with_value(*v)).map_or(false, is_definition))
        .count();
    if num_definitions == 1 { Some(()) } else { None }
}

pub(crate) fn check_var_has_no_references(sema: &Semantic, var: InFile<&ast::Var>) -> Option<()> {
    let usages = sema.find_local_usages(var)?;
    let num_definitions = usages
        .iter()
        .filter(|v| {
            sema.to_def(var.with_value(*v))
                .map_or(false, |dor| !is_definition(dor))
        })
        .count();
    if num_definitions == 0 { Some(()) } else { None }
}

pub(crate) fn check_var_has_references(sema: &Semantic, var: InFile<&ast::Var>) -> Option<()> {
    match check_var_has_no_references(sema, var) {
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
    labels_full: FxHashMap<Option<SmolStr>, (&'a FunctionMatch, &'a T)>,
    labels_mf: FxHashMap<Option<SmolStr>, (&'a FunctionMatch, &'a T)>,
    labels_m: FxHashMap<Option<SmolStr>, (&'a FunctionMatch, &'a T)>,
}

impl<'a, T> FunctionMatcher<'a, T> {
    pub fn new(call: &'a [(&'a FunctionMatch, T)]) -> FunctionMatcher<'a, T> {
        let mut labels_full: FxHashMap<Option<SmolStr>, (&FunctionMatch, &T)> =
            FxHashMap::default();
        let mut labels_mf: FxHashMap<Option<SmolStr>, (&FunctionMatch, &T)> = FxHashMap::default();
        let mut labels_m: FxHashMap<Option<SmolStr>, (&FunctionMatch, &T)> = FxHashMap::default();
        call.iter().for_each(|(c, t)| match c {
            FunctionMatch::MFA(mfa) => {
                if mfa.module == "erlang" && in_erlang_module(&mfa.name, mfa.arity as usize) {
                    labels_full.insert(Some(mfa.label().into()), (*c, t));
                    labels_full.insert(Some(mfa.short_label().into()), (*c, t));
                } else {
                    labels_full.insert(Some(mfa.label().into()), (*c, t));
                }
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
            labels_full,
            labels_mf,
            labels_m,
        }
    }

    pub fn get_match(
        &self,
        target: &CallTarget<ExprId>,
        arity: u32,
        sema: &Semantic,
        body: &Body,
    ) -> Option<(&'a FunctionMatch, &'a T)> {
        self.labels_full
            .get(&target.label(arity, sema, body))
            .copied()
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
pub enum FunctionMatch {
    MFA(MFA),
    MF { module: String, name: String },
    M { module: String },
}

impl FunctionMatch {
    pub fn mfa(m: &str, f: &str, arity: u32) -> FunctionMatch {
        FunctionMatch::MFA(MFA {
            module: m.into(),
            name: f.into(),
            arity,
        })
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

    pub fn module(self: &FunctionMatch) -> String {
        match self {
            FunctionMatch::MFA(mfa) => mfa.module.clone(),
            FunctionMatch::MF { module, name: _ } => module.to_string(),
            FunctionMatch::M { module } => module.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
pub struct MFA {
    pub module: String,
    pub name: String,
    pub arity: u32,
}

impl MFA {
    pub fn new(m: &str, n: &str, arity: u32) -> MFA {
        MFA {
            module: m.into(),
            name: n.into(),
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
    pub target: &'a CallTarget<ExprId>,
    pub t: &'a T,
    pub args: &'a [ExprId],
    pub def_fb: &'a InFunctionBody<&'a FunctionDef>,
}

/// Check a specific call instance, and return the contents of a
/// diagnostic if needed.
pub type CheckCall<'a, T> = &'a dyn Fn(CheckCallCtx<T>) -> Option<String>;

pub(crate) fn find_call_in_function<T>(
    diags: &mut Vec<Diagnostic>,
    sema: &Semantic,
    def: &FunctionDef,
    call: &[(&FunctionMatch, T)],
    check_call: CheckCall<T>,
    make_diag: impl FnOnce(
        &Semantic,
        &mut InFunctionBody<&FunctionDef>,
        &CallTarget<ExprId>,
        &[ExprId],
        &str,
        TextRange,
    ) -> Option<Diagnostic>
    + Copy,
) -> Option<()> {
    let mut def_fb = def.in_function_body(sema.db, def);
    let matcher = FunctionMatcher::new(call);
    def_fb.clone().fold_function_with_macros(
        Strategy::TopDown,
        (),
        &mut |acc, _, ctx| {
            if let Expr::Call { target, args } = ctx.expr {
                if let Some((mfa, t)) =
                    matcher.get_match(&target, args.len() as u32, sema, &def_fb.body())
                {
                    let context = CheckCallCtx {
                        mfa,
                        t,
                        target: &target,
                        args: &args,
                        def_fb: &def_fb,
                    };
                    if let Some(match_descr) = check_call(context) {
                        // Got one.
                        let call_expr_id = if let Some(expr_id) = ctx.in_macro {
                            expr_id
                        } else {
                            ctx.expr_id
                        };
                        if let Some(range) = &def_fb.range_for_expr(sema.db, call_expr_id) {
                            if let Some(diag) =
                                make_diag(sema, &mut def_fb, &target, &args, &match_descr, *range)
                            {
                                diags.push(diag)
                            }
                        }
                    }
                }
            };
            acc
        },
        &mut |acc, _, _| acc,
    );
    Some(())
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use elp_ide_db::elp_base_db::FileId;
    use fxhash::FxHashSet;
    use hir::FunctionDef;
    use hir::Semantic;

    use super::find_call_in_function;
    use super::FunctionMatch;
    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::diagnostics::Severity;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix_with_config;

    fn check_functions(
        diags: &mut Vec<Diagnostic>,
        sema: &Semantic,
        file_id: FileId,
        match_spec: &Vec<Vec<FunctionMatch>>,
    ) {
        sema.def_map(file_id)
            .get_functions()
            .iter()
            .for_each(|(_arity, def)| check_function(diags, sema, def, match_spec.clone()));
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
            &move |_ctx| Some("Diagnostic Message".to_string()),
            move |_sema, def_fb, __target, _args, extra_info, range| {
                let diag = Diagnostic::new(
                    DiagnosticCode::AdHoc("test".to_string()),
                    extra_info,
                    range.clone(),
                )
                .severity(Severity::Warning)
                .with_ignore_fix(sema, def_fb.file_id());
                Some(diag)
            },
        );
    }

    #[track_caller]
    fn check_adhoc_function_match(match_spec: &Vec<Vec<FunctionMatch>>, fixture: &str) {
        check_diagnostics_with_config(
            DiagnosticsConfig::new(
                false,
                FxHashSet::default(),
                vec![&|acc, sema, file_id, _ext| check_functions(acc, sema, file_id, match_spec)],
            )
            .disable(DiagnosticCode::MissingCompileWarnMissingSpec)
            .disable(DiagnosticCode::CrossNodeEval),
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
            DiagnosticsConfig::new(
                false,
                FxHashSet::default(),
                vec![&|acc, sema, file_id, _ext| check_functions(acc, sema, file_id, match_spec)],
            )
            .disable(DiagnosticCode::MissingCompileWarnMissingSpec)
            .disable(DiagnosticCode::CrossNodeEval),
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
                foo:bar(x,y).
            %%  ^^^^^^^^^^^^ 💡 warning: Diagnostic Message
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
            %%  ^^^^^^^^^ 💡 warning: Diagnostic Message
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
            %% ^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Diagnostic Message
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
            %%     ^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Diagnostic Message
             "#,
        );
    }
}

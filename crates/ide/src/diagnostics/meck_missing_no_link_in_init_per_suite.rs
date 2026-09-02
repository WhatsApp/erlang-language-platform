/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_ide_db::text_edit::TextRange;
use elp_ide_db::text_edit::TextSize;
use hir::AnyExprId;
use hir::Expr;
use hir::FunctionDef;
use hir::InFunctionClauseBody;
use hir::NameArity;
use hir::Semantic;
use hir::fold::ParentId;
use hir::known;

use crate::Assist;
use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::MatchCtx;
use crate::codemod_helpers::find_call_in_function;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::fix;

pub(crate) static LINTER: MeckMissingNoLinkLinter = MeckMissingNoLinkLinter;

pub(crate) struct MeckMissingNoLinkLinter;

#[derive(Clone, Debug)]
pub(crate) struct Context {
    end_of_list: TextSize,
    is_empty: bool,
    new_arg: bool,
    deletion_range: Option<TextRange>,
}

impl Linter for MeckMissingNoLinkLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::MeckMissingNoLinkInInitPerSuite
    }

    fn description(&self) -> &'static str {
        "Missing no_link option."
    }

    fn is_experimental(&self) -> bool {
        true
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        sema.db.file_kind(file_id) == FileKind::TestModule
    }
}

impl GenericLinter for MeckMissingNoLinkLinter {
    type Context = Context;

    fn matches(
        &self,
        ctx: &LinterContext,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let mut results = Vec::new();
        sema.def_map_local(file_id)
            .get_functions()
            .for_each(|(_arity, def)| {
                if def.name == NameArity::new(*known::init_per_suite, 1)
                    || def.name == NameArity::new(*known::init_per_group, 2)
                {
                    find_matches(&mut results, sema, def)
                }
            });
        if results.is_empty() {
            return None;
        }
        Some(
            results
                .into_iter()
                .map(|(range, context)| GenericLinterMatchContext {
                    range: FileRange { file_id, range },
                    context,
                })
                .collect(),
        )
    }

    fn fixes(
        &self,
        context: &Self::Context,
        range: TextRange,
        ctx: &LinterContext,
    ) -> Option<Vec<Assist>> {
        let file_id = ctx.file_id;
        Some(vec![make_fix(context, range, file_id)])
    }
}

fn in_anonymous_fun(def_fb: &InFunctionClauseBody<&FunctionDef>, parents: &[ParentId]) -> bool {
    parents.iter().any(|parent_id| match parent_id {
        ParentId::HirIdx(hir_idx) => match hir_idx.idx {
            AnyExprId::Expr(idx) => matches!(def_fb[idx], Expr::Closure { .. }),
            _ => false,
        },
        _ => false,
    })
}

fn find_matches(results: &mut Vec<(TextRange, Context)>, sema: &Semantic, def: &FunctionDef) {
    find_call_in_function(
        results,
        sema,
        def,
        &[(&FunctionMatch::mf("meck", "new"), ())],
        &[],
        &move |CheckCallCtx {
                   args,
                   in_clause: def_fb,
                   parents,
                   ..
               }: CheckCallCtx<'_, ()>| {
            if in_anonymous_fun(def_fb, parents) {
                return None;
            }
            match args.as_slice() {
                [_module] => Some(()),
                [_module, options] => {
                    let body = def_fb.body();
                    if let Some(false) =
                        &body[options].literal_list_contains_atom(def_fb, "no_link")
                    {
                        Some(())
                    } else {
                        None
                    }
                }
                _ => None,
            }
        },
        &move |MatchCtx {
                   def_fb,
                   args,
                   range,
                   ..
               }| match args.as_slice()[..] {
            [module] => {
                let module_range = def_fb.range_for_expr(module)?;
                Some((
                    range.range,
                    Context {
                        end_of_list: module_range.range.end(),
                        is_empty: true,
                        new_arg: true,
                        deletion_range: None,
                    },
                ))
            }
            [_module, options] => {
                let body = def_fb.body();
                match &body[options] {
                    hir::Expr::List { exprs, .. } => match exprs.last() {
                        Some(last_option) => {
                            let last_option_range = def_fb.range_for_expr(*last_option)?;
                            Some((
                                range.range,
                                Context {
                                    end_of_list: last_option_range.range.end(),
                                    is_empty: exprs.is_empty(),
                                    new_arg: false,
                                    deletion_range: None,
                                },
                            ))
                        }
                        None => {
                            let options_range = def_fb.range_for_expr(options)?;
                            Some((
                                range.range,
                                Context {
                                    end_of_list: options_range.range.end(),
                                    is_empty: true,
                                    new_arg: false,
                                    deletion_range: Some(options_range.range),
                                },
                            ))
                        }
                    },
                    _ => None,
                }
            }
            _ => None,
        },
    );
}

fn make_fix(context: &Context, range: TextRange, file_id: FileId) -> Assist {
    let mut builder = SourceChangeBuilder::new(file_id);
    if let Some(deletion_range) = context.deletion_range {
        builder.delete(deletion_range)
    }
    let text = if context.is_empty {
        if context.new_arg {
            ", [no_link]".to_string()
        } else {
            match context.deletion_range {
                Some(_) => "[no_link]".to_string(),
                None => "no_link".to_string(),
            }
        }
    } else {
        ", no_link".to_string()
    };
    builder.insert(context.end_of_list, text);
    fix(
        "meck_add_missing_no_link_option",
        "Add missing no_link option",
        builder.finish(),
        range,
    )
}

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::MeckMissingNoLinkInInitPerSuite
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default().set_experimental(true);
        tests::check_filtered_diagnostics_with_config(config, &vec![], fixture, &filter)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        let config = DiagnosticsConfig::default()
            .set_experimental(true)
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::UnusedFunctionArg);
        tests::check_fix_with_config(config, fixture_before, fixture_after)
    }

    #[test]
    fn test_missing_no_link_meck_new_1() {
        check_diagnostics(
            r#"
//- /my_app/test/missing_no_link1_SUITE.erl
   -module(missing_no_link1_SUITE).
   -export([all/0, init_per_suite/1]).
   -export([a/1]).
   all() -> [a].
   init_per_suite(Config) ->
     meck:new(my_module).
%%   ^^^^^^^^^^^^^^^^^^^ warning: W0022: Missing no_link option.
%%                     | 💡 Add missing no_link option
%%                     | 💡 <suppression>

   a(_Config) ->
     ok.
//- /my_app/src/meck.erl
   -module(meck).
   -export([new/1]).
   new(_Module) -> ok.
            "#,
        )
    }

    #[test]
    fn test_missing_no_link_init_per_group() {
        check_diagnostics(
            r#"
//- /my_app/test/missing_no_link2_SUITE.erl
   -module(missing_no_link2_SUITE).
   -export([all/0, init_per_group/2]).
   -export([a/1]).
   all() -> [a].
   init_per_group(_Group, Config) ->
     meck:new(my_module),
%%   ^^^^^^^^^^^^^^^^^^^ warning: W0022: Missing no_link option.
%%                     | 💡 Add missing no_link option
%%                     | 💡 <suppression>
     Config.

   a(_Config) ->
     ok.
//- /my_app/src/meck.erl
   -module(meck).
   -export([new/1]).
   new(_Module) -> ok.
            "#,
        )
    }

    #[test]
    fn test_missing_no_warning_outside_known_functions() {
        check_diagnostics(
            r#"
//- /my_app/test/missing_no_link3_SUITE.erl
   -module(missing_no_link3_SUITE).
   -export([all/0, init_per_suite/1]).
   -export([a/1]).
   all() -> [a].
   init_per_suite(Config) ->
     ok.

   a(_Config) ->
     meck:new(my_module),
     ok.
//- /my_app/src/meck.erl
   -module(meck).
   -export([new/1]).
   new(_Module) -> ok.
            "#,
        )
    }

    #[test]
    fn test_missing_no_link_meck_new_2() {
        check_diagnostics(
            r#"
//- /my_app/test/missing_no_link4_SUITE.erl
   -module(missing_no_link4_SUITE).
   -export([all/0, init_per_suite/1]).
   -export([a/1]).
   all() -> [a].
   init_per_suite(Config) ->
     meck:new(my_module, [passthrough, link]).
%%   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0022: Missing no_link option.
%%                                          | 💡 Add missing no_link option
%%                                          | 💡 <suppression>

   a(_Config) ->
     ok.
//- /my_app/src/meck.erl
   -module(meck).
   -export([new/2]).
   new(_Module, _Options) -> ok.
            "#,
        )
    }

    #[test]
    fn test_missing_no_link_in_fun() {
        check_diagnostics(
            r#"
//- /my_app/test/missing_no_link5_SUITE.erl
   -module(missing_no_link5_SUITE).
   -export([all/0, init_per_suite/1]).
   -export([a/1]).
   all() -> [a].
   init_per_suite(Config) ->
     F = fun() -> meck:new(my_module, [passthrough, link]) end,
     F().

   a(_Config) ->
     ok.
//- /my_app/src/meck.erl
   -module(meck).
   -export([new/2]).
   new(_Module, _Options) -> ok.
            "#,
        );
    }

    #[test]
    fn test_fix_missing_no_link_option_new_1() {
        check_fix(
            r#"
//- /my_app/test/missing_no_link6_SUITE.erl
-module(missing_no_link6_SUITE).
-export([all/0, init_per_suite/1]).
-export([a/1]).
all() -> [a].
init_per_suite(Config) ->
  m~eck:new(my_module).

a(_Config) ->
  ok.
//- /my_app/src/meck.erl
-module(meck).
-export([new/2]).
new(_Module, _Options) -> ok.
            "#,
            expect![[r#"
-module(missing_no_link6_SUITE).
-export([all/0, init_per_suite/1]).
-export([a/1]).
all() -> [a].
init_per_suite(Config) ->
  meck:new(my_module, [no_link]).

a(_Config) ->
  ok.
"#]],
        );
    }

    #[test]
    fn test_fix_missing_no_link_option_new_2() {
        check_fix(
            r#"
//- /my_app/test/missing_no_link7_SUITE.erl
-module(missing_no_link7_SUITE).
-export([all/0, init_per_suite/1]).
-export([a/1]).
all() -> [a].
init_per_suite(Config) ->
  m~eck:new(my_module, [passthrough, link]).

a(_Config) ->
  ok.
//- /my_app/src/meck.erl
-module(meck).
-export([new/2]).
new(_Module, _Options) -> ok.
            "#,
            expect![[r#"
-module(missing_no_link7_SUITE).
-export([all/0, init_per_suite/1]).
-export([a/1]).
all() -> [a].
init_per_suite(Config) ->
  meck:new(my_module, [passthrough, link, no_link]).

a(_Config) ->
  ok.
"#]],
        );
    }

    #[test]
    fn test_fix_missing_no_link_option_new_2_empty_list() {
        check_fix(
            r#"
//- /my_app/test/missing_no_link8_SUITE.erl
-module(missing_no_link8_SUITE).
-export([all/0, init_per_suite/1]).
-export([a/1]).
all() -> [a].
init_per_suite(Config) ->
  m~eck:new(my_module, []).

a(_Config) ->
  ok.
//- /my_app/src/meck.erl
-module(meck).
-export([new/2]).
new(_Module, _Options) -> ok.
            "#,
            expect![[r#"
-module(missing_no_link8_SUITE).
-export([all/0, init_per_suite/1]).
-export([a/1]).
all() -> [a].
init_per_suite(Config) ->
  meck:new(my_module, [no_link]).

a(_Config) ->
  ok.
"#]],
        );
    }
}

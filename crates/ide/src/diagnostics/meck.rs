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
use elp_ide_db::source_change::SourceChangeBuilder;
use elp_text_edit::TextRange;
use elp_text_edit::TextSize;
use hir::AnyExprId;
use hir::Expr;
use hir::FunctionDef;
use hir::InFunctionClauseBody;
use hir::NameArity;
use hir::Semantic;
use hir::fold::ParentId;
use hir::known;

use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::MatchCtx;
use crate::codemod_helpers::find_call_in_function;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::Severity;
use crate::fix;

pub fn missing_no_link_in_init_per_suite(
    res: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
) {
    sema.def_map(file_id)
        .get_functions()
        .for_each(|(_arity, def)| {
            if def.file.file_id == file_id && def.name == NameArity::new(known::init_per_suite, 1)
                || def.name == NameArity::new(known::init_per_group, 2)
            {
                check_function(res, sema, def)
            }
        });
}

fn in_anonymous_fun(def_fb: &InFunctionClauseBody<&FunctionDef>, parents: &[ParentId]) -> bool {
    parents.iter().any(|parent_id| match parent_id {
        ParentId::HirIdx(hir_idx) => match hir_idx.idx {
            AnyExprId::Expr(idx) => match def_fb[idx] {
                Expr::Closure { .. } => true,
                _ => false,
            },
            _ => false,
        },
        _ => false,
    })
}

pub(crate) fn check_function(diags: &mut Vec<Diagnostic>, sema: &Semantic, def: &FunctionDef) {
    find_call_in_function(
        diags,
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
                   sema,
                   def_fb,
                   args,
                   range,
                   ..
               }| match args.as_slice()[..] {
            [module] => {
                if let Some(module_range) = def_fb.range_for_expr(module) {
                    if def.file.file_id == range.file_id {
                        let diag = make_diagnostic(
                            sema,
                            def.file.file_id,
                            range.range,
                            module_range.range.end(),
                            true,
                            true,
                            None,
                        );
                        Some(diag)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            [_module, options] => {
                let body = def_fb.body();
                match &body[options] {
                    hir::Expr::List { exprs, .. } => match exprs.last() {
                        Some(last_option) => {
                            if let Some(last_option_range) = def_fb.range_for_expr(*last_option) {
                                if def.file.file_id == range.file_id {
                                    let diag = make_diagnostic(
                                        sema,
                                        def.file.file_id,
                                        range.range,
                                        last_option_range.range.end(),
                                        exprs.is_empty(),
                                        false,
                                        None,
                                    );
                                    Some(diag)
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                        None => {
                            // Empty list
                            if let Some(options_range) = def_fb.range_for_expr(options) {
                                if def.file.file_id == range.file_id {
                                    let diag = make_diagnostic(
                                        sema,
                                        def.file.file_id,
                                        range.range,
                                        options_range.range.end(),
                                        true,
                                        false,
                                        Some(options_range.range),
                                    );
                                    Some(diag)
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                    },
                    _ => None,
                }
            }
            _ => None,
        },
    );
}

fn make_diagnostic(
    sema: &Semantic,
    file_id: FileId,
    range: TextRange,
    end_of_list: TextSize,
    is_empty: bool,
    new_arg: bool,
    deletion_range: Option<TextRange>,
) -> Diagnostic {
    let message = "Missing no_link option.".to_string();
    let mut builder = SourceChangeBuilder::new(file_id);
    if let Some(deletion_range) = deletion_range {
        builder.delete(deletion_range)
    }
    let text = if is_empty {
        if new_arg {
            ", [no_link]".to_string()
        } else {
            match deletion_range {
                Some(_) => "[no_link]".to_string(),
                None => "no_link".to_string(),
            }
        }
    } else {
        ", no_link".to_string()
    };
    builder.insert(end_of_list, text);
    let fixes = vec![fix(
        "meck_add_missing_no_link_option",
        "Add missing no_link option",
        builder.finish(),
        range,
    )];
    Diagnostic::new(
        DiagnosticCode::MeckMissingNoLinkInInitPerSuite,
        message,
        range,
    )
    .experimental()
    .with_severity(Severity::Warning)
    .with_ignore_fix(sema, file_id)
    .with_fixes(Some(fixes))
}

#[cfg(test)]
mod tests {

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::MeckMissingNoLinkInInitPerSuite
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        tests::check_filtered_diagnostics(fixture, &filter)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: &str) {
        tests::check_filtered_ct_fix(
            fixture_before,
            fixture_after,
            &|d| d.code == DiagnosticCode::MeckMissingNoLinkInInitPerSuite,
            &|a| a.id.0 == "meck_add_missing_no_link_option",
        )
    }

    #[test]
    fn test_missing_no_link_meck_new_1() {
        check_diagnostics(
            r#"
//- common_test
//- /my_app/test/missing_no_link1_SUITE.erl
   -module(missing_no_link1_SUITE).
   -export([all/0, init_per_suite/1]).
   -export([a/1]).
   all() -> [a].
   init_per_suite(Config) ->
     meck:new(my_module).
%%   ^^^^^^^^^^^^^^^^^^^ 💡 warning: Missing no_link option.

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
//- common_test
//- /my_app/test/missing_no_link2_SUITE.erl
   -module(missing_no_link2_SUITE).
   -export([all/0, init_per_group/2]).
   -export([a/1]).
   all() -> [a].
   init_per_group(_Group, Config) ->
     meck:new(my_module),
%%   ^^^^^^^^^^^^^^^^^^^ 💡 warning: Missing no_link option.
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
//- common_test
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
//- common_test
//- /my_app/test/missing_no_link4_SUITE.erl
   -module(missing_no_link4_SUITE).
   -export([all/0, init_per_suite/1]).
   -export([a/1]).
   all() -> [a].
   init_per_suite(Config) ->
     meck:new(my_module, [passthrough, link]).
%%   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: Missing no_link option.

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
//- common_test
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
//- common_test
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
            r#"
-module(missing_no_link6_SUITE).
-export([all/0, init_per_suite/1]).
-export([a/1]).
all() -> [a].
init_per_suite(Config) ->
  meck:new(my_module, [no_link]).

a(_Config) ->
  ok.
"#,
        );
    }

    #[test]
    fn test_fix_missing_no_link_option_new_2() {
        check_fix(
            r#"
//- common_test
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
            r#"
-module(missing_no_link7_SUITE).
-export([all/0, init_per_suite/1]).
-export([a/1]).
all() -> [a].
init_per_suite(Config) ->
  meck:new(my_module, [passthrough, link, no_link]).

a(_Config) ->
  ok.
"#,
        );
    }

    #[test]
    fn test_fix_missing_no_link_option_new_2_empty_list() {
        check_fix(
            r#"
//- common_test
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
            r#"
-module(missing_no_link8_SUITE).
-export([all/0, init_per_suite/1]).
-export([a/1]).
all() -> [a].
init_per_suite(Config) ->
  meck:new(my_module, [no_link]).

a(_Config) ->
  ok.
"#,
        );
    }
}

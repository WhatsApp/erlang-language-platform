/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::FileId;
use elp_syntax::TextRange;
use hir::db::InternDatabase;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::AnyExpr;
use hir::Expr;
use hir::InFile;
use hir::ParamName;
use hir::Semantic;
use hir::Strategy;

use crate::InlayHint;
use crate::InlayHintLabel;
use crate::InlayHintsConfig;
use crate::InlayKind;

pub(super) fn hints(
    res: &mut Vec<InlayHint>,
    sema: &Semantic,
    config: &InlayHintsConfig,
    file_id: FileId,
    range_limit: Option<TextRange>,
) -> Option<()> {
    if !config.parameter_hints {
        return None;
    }
    let def_map = sema.def_map_local(file_id);
    for (_, def) in def_map.get_functions() {
        let function_id = InFile::new(file_id, def.function_id);
        let function_body = sema.to_function_body(function_id);
        function_body.fold_function(
            Strategy {
                macros: MacroStrategy::ExpandButIncludeMacroCall,
                parens: ParenStrategy::InvisibleParens,
            },
            (),
            &mut |acc, clause_id, ctx| {
                if let AnyExpr::Expr(Expr::Call { target, args }) = ctx.item {
                    // Do not produce hints if inside a macro
                    if ctx.in_macro.is_none() {
                        let arity = args.len() as u32;
                        let body = &function_body.body(clause_id);
                        if let Some(call_def) = target.resolve_call(arity, sema, file_id, body) {
                            let param_names = &call_def.function_clauses[0].param_names;
                            for (param_name, arg) in param_names.iter().zip(args) {
                                if should_hint(sema.db.upcast(), param_name, &body[arg]) {
                                    if let Some(arg_range) =
                                        function_body.range_for_expr(clause_id, arg)
                                    {
                                        if range_limit.is_none()
                                            || range_limit.unwrap().contains_range(arg_range)
                                        {
                                            if let ParamName::Name(param_name) = param_name {
                                                let hint = InlayHint {
                                                    range: arg_range,
                                                    kind: InlayKind::Parameter,
                                                    label: InlayHintLabel::simple(
                                                        param_name.as_str(),
                                                        None,
                                                        None,
                                                    ),
                                                };
                                                res.push(hint);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                acc
            },
        )
    }
    Some(())
}

fn should_hint(db: &dyn InternDatabase, param_name: &ParamName, expr: &Expr) -> bool {
    match param_name {
        ParamName::Name(name) => {
            if let Some(var) = expr.as_var() {
                let var_name = &var.as_string(db);
                let normalized_name = name.as_str().trim_start_matches('_');
                var_name != normalized_name
            } else {
                true
            }
        }
        ParamName::Default(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::inlay_hints::tests::check_with_config;
    use crate::inlay_hints::tests::DISABLED_CONFIG;
    use crate::inlay_hints::InlayHintsConfig;

    #[track_caller]
    fn check_params(fixture: &str) {
        check_with_config(
            InlayHintsConfig {
                parameter_hints: true,
                ..DISABLED_CONFIG
            },
            fixture,
        );
    }

    #[test]
    fn param_hints_basic() {
        check_params(
            r#"
-module(main).~
-compile(export_all).
sum(A, B) -> A + B.
main() -> sum(1,
           %% ^A
              2
           %% ^B
            ).
"#,
        );
    }

    #[test]
    fn param_hints_variables_same_name() {
        check_params(
            r#"
-module(main).~
-compile(export_all).
sum(A, B) -> A + B.
main() ->
  A = 1,
  B = 2,
  sum(A,
      B
     ).
"#,
        );
    }

    #[test]
    fn param_hints_variables_different_name() {
        check_params(
            r#"
-module(main).~
-compile(export_all).
sum(A, B) -> A + B.
main() ->
  A = 1,
  X = 2,
  sum(A,
      X
   %% ^B
     ).
"#,
        );
    }

    #[test]
    fn param_hints_variables_underscore_same_name() {
        check_params(
            r#"
-module(main).~
-compile(export_all).
sum(A, _X) -> A.
main() ->
  A = 1,
  X = 2,
  sum(A,
      X
     ).
"#,
        );
    }

    #[test]
    fn param_hints_variables_underscore_different_name() {
        check_params(
            r#"
-module(main).~
-compile(export_all).
sum(A, _B) -> A.
main() ->
  A = 1,
  X = 2,
  sum(A,
      X
   %% ^_B
     ).
"#,
        );
    }

    #[test]
    fn param_hints_variables_expression() {
        check_params(
            r#"
-module(main).~
-compile(export_all).
sum(A, B) -> A + B.
main() ->
  X = 2,
  sum(1 * (3 - 2),
   %% ^^^^^^^^^^^A
      X
   %% ^B
     ).
"#,
        );
    }

    #[test]
    fn param_hints_variables_multiple_calls() {
        check_params(
            r#"
-module(main).~
-compile(export_all).
sum(A, B) -> A + B.
main() ->
  X = 2,
  sum(1 * (3 - 2),
   %% ^^^^^^^^^^^A
      X
   %% ^B
     ),
  sum(X,
   %% ^A
      X
   %% ^B
    ).
"#,
        );
    }

    #[test]
    fn param_hints_variables_wrong_arity() {
        check_params(
            r#"
-module(main).~
-compile(export_all).
sum(A, B) -> A + B.
main() ->
  A = 1,
  sum(A).
"#,
        );
    }

    #[test]
    fn param_hints_variables_missing_param() {
        check_params(
            r#"
-module(main).~
-compile(export_all).
sum(A, B) -> A + B.
main() ->
  A = 1,
  B = 2,
  sum(A, ).
"#,
        );
    }

    #[test]
    fn param_hints_skip_in_macro() {
        check_params(
            r#"
//- /src/main.erl
-module(main).~
-compile(warn_missing_spec).
-include("header.hrl").
-export([read_file/0]).
read_file() ->
    ?LOG(warning,
         [],
         "Sample", []).
//- /src/dep.erl
-module(dep).
-compile(export_all).
call(One, Two, Three = {_, _, _}) ->
    ok.
//- /src/header.hrl
-define(LAZY(X), fun() -> X end).
-define(LOG(Level, Opts, Format, Args),
    ?LAZY(
        dep:call(Level, Opts, #{})
    )()
).
"#,
        );
    }

    #[test]
    fn param_hints_variables_skip_default_names() {
        check_params(
            r#"
-module(main).~
-compile(export_all).
do(X, Y, #{}) -> A + B.
main() ->
  A = 1,
  B = 2,
  C = 3,
  do(A, B, C).
%%   ^ X
%%      ^ Y
"#,
        );
    }
}

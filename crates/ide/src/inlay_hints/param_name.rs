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
use hir::db::MinInternDatabase;
use hir::Expr;
use hir::InFile;
use hir::Name;
use hir::Semantic;

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
    let def_map = sema.def_map(file_id);
    for (_name, def) in def_map.get_functions() {
        if def.file.file_id == file_id {
            let def_fb = def.in_function_body(sema.db, def);
            let function_id = InFile::new(file_id, def.function_id);
            let function_body = sema.to_function_body(function_id);
            sema.fold_function(
                function_id,
                (),
                &mut |acc, _clause_id, ctx| {
                    match ctx.expr {
                        Expr::Call { target, args } => {
                            let arity = args.len() as u32;
                            let body = &function_body.body();
                            if let Some(call_def) = target.resolve_call(arity, &sema, file_id, body)
                            {
                                let param_names = call_def.function.param_names;
                                for (param_name, arg) in param_names.iter().zip(args) {
                                    if should_hint(
                                        sema.db.upcast(),
                                        param_name,
                                        &function_body[arg],
                                    ) {
                                        if let Some(arg_range) = def_fb.range_for_expr(sema.db, arg)
                                        {
                                            if range_limit.is_none()
                                                || range_limit.unwrap().contains_range(arg_range)
                                            {
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
                        _ => {}
                    }
                    acc
                },
                &mut |acc, _, _| acc,
            );
        }
    }
    Some(())
}

fn should_hint(db: &dyn MinInternDatabase, param_name: &Name, expr: &Expr) -> bool {
    if let Some(var) = expr.as_var() {
        var.as_string(db) != param_name.as_str()
    } else {
        true
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
-module(main).
-compile(export_all).
sum(A, B) -> A + B.
main() -> sum(1,
           %% ^A
              2
           %% ^B
            ).
}"#,
        );
    }

    #[test]
    fn param_hints_variables_same_name() {
        check_params(
            r#"
-module(main).
-compile(export_all).
sum(A, B) -> A + B.
main() ->
  A = 1,
  B = 2,
  sum(A,
      B
     ).
}"#,
        );
    }

    #[test]
    fn param_hints_variables_different_name() {
        check_params(
            r#"
-module(main).
-compile(export_all).
sum(A, B) -> A + B.
main() ->
  A = 1,
  X = 2,
  sum(A,
      X
   %% ^B
     ).
}"#,
        );
    }

    #[test]
    fn param_hints_variables_expression() {
        check_params(
            r#"
-module(main).
-compile(export_all).
sum(A, B) -> A + B.
main() ->
  X = 2,
  sum(1 * (3 - 2),
   %% ^^^^^^^^^^^A
      X
   %% ^B
     ).
}"#,
        );
    }

    #[test]
    fn param_hints_variables_multiple_calls() {
        check_params(
            r#"
-module(main).
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
}"#,
        );
    }

    #[test]
    fn param_hints_variables_wrong_arity() {
        check_params(
            r#"
-module(main).
-compile(export_all).
sum(A, B) -> A + B.
main() ->
  A = 1,
  sum(A).
}"#,
        );
    }

    #[test]
    fn param_hints_variables_missing_param() {
        check_params(
            r#"
-module(main).
-compile(export_all).
sum(A, B) -> A + B.
main() ->
  A = 1,
  B = 2,
  sum(A, ).
}"#,
        );
    }
}

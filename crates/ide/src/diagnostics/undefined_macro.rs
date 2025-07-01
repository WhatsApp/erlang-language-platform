/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Add an assist to an erlang service diagnostic for an undefined macro.

use elp_ide_completion::get_include_file;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::path_for_file;
use elp_ide_db::source_change::SourceChange;
use elp_text_edit::TextEdit;
use hir::Semantic;
use lazy_static::lazy_static;
use regex::Regex;

use super::Diagnostic;
use crate::fix;

pub(crate) fn add_assist(
    sema: &Semantic,
    file_id: FileId,
    diagnostic: &mut Diagnostic,
) -> Option<()> {
    let (macro_name, macro_arity_str) = macro_undefined_from_message(&diagnostic.message)?;
    let project_id = sema.db.file_project_id(file_id)?;
    let index = sema.macro_define_index(project_id);
    let name = format!("{macro_name}/{macro_arity_str}");
    let includes: Vec<_> = index
        .complete(&macro_name)
        .iter()
        .flat_map(|(_chars, defines)| {
            defines.iter().flat_map(|define| {
                let include_path = path_for_file(sema.db, define.file_id);
                if let Some(include_path) = include_path {
                    get_include_file(sema.db, file_id, define.file_id, include_path.clone())
                } else {
                    None
                }
            })
        })
        .collect();
    let multiple = includes.len() > 1;
    for include in includes {
        if let Some(pos) = include.insert_position_if_needed(sema, file_id) {
            let mut builder = TextEdit::builder();
            builder.insert(pos.offset, include.as_attribute());
            let edit = builder.finish();
            let label = if multiple {
                format!("Add required include for '{name}' ({})", include.app_name)
            } else {
                format!("Add required include for '{name}'")
            };
            diagnostic.add_fix(fix(
                "add_macro_include",
                label.as_str(),
                SourceChange::from_text_edit(file_id, edit),
                diagnostic.range,
            ));
        }
    }
    Some(())
}

pub fn macro_undefined_from_message(s: &str) -> Option<(String, String)> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^undefined macro '([^/]+)/([^\s]+)'$").unwrap();
    }

    let captures = RE.captures(s)?;
    if RE.captures_len() == 3 {
        Some((captures[1].to_string(), captures[2].to_string()))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;
    use crate::tests::check_specific_fix;

    #[test]
    fn undefined_macro() {
        check_diagnostics(
            r#"
            //- erlang_service
            //- /app/src/main.erl
            -module(main).

            foo(X) -> ?assertEqual(X,2).
            %%        ^^^^^^^^^^^^ 💡 error: undefined macro 'assertEqual/2'
            //- /another-app/include/inc.hrl app:another include_path:/another-app/include
            -define(assertEqual(A,B), A =:= B).
           "#,
        );
    }

    #[test]
    fn undefined_macro_fix() {
        check_fix(
            r#"
            //- erlang_service
            //- /main/src/main.erl app:main
            -module(main).

            foo(X) -> ?assert~Equal(X,2).
            %%        ^^^^^^^^^^^^ 💡 error: undefined macro 'assertEqual/2'
            //- /another-app/include/inc.hrl app:another include_path:/another-app/include
            -define(assertEqual(A,B), A =:= B).
           "#,
            expect![[r#"
                -module(main).
                -include_lib("another/include/inc.hrl").

                foo(X) -> ?assertEqual(X,2).
            "#]],
        );
    }

    #[test]
    fn undefined_macro_fix_multiple_app_a() {
        check_specific_fix(
            "Add required include for 'assertEqual/2' (app_a)",
            r#"
            //- erlang_service
            //- /main/src/main.erl app:main
            -module(main).

            foo(X) -> ?assert~Equal(X,2).
            %%        ^^^^^^^^^^^^ 💡 error: undefined macro 'assertEqual/2'

            //- /app_a/include/inc.hrl app:app_a include_path:/app_a/include
            -define(assertEqual(A,B), A =:= B).

            //- /app_b/include/inc.hrl app:app_b include_path:/app_b/include
            -define(assertEqual(A,B), A =:= B).
           "#,
            expect![[r#"
                -module(main).
                -include_lib("app_a/include/inc.hrl").

                foo(X) -> ?assertEqual(X,2).

            "#]],
        );
    }

    #[test]
    fn undefined_macro_fix_multiple_app_b() {
        check_specific_fix(
            "Add required include for 'assertEqual/2' (app_b)",
            r#"
            //- erlang_service
            //- /main/src/main.erl app:main
            -module(main).

            foo(X) -> ?assert~Equal(X,2).
            %%        ^^^^^^^^^^^^ 💡 error: undefined macro 'assertEqual/2'

            //- /app_a/include/inc.hrl app:app_a include_path:/app_a/include
            -define(assertEqual(A,B), A =:= B).

            //- /app_b/include/inc.hrl app:app_b include_path:/app_b/include
            -define(assertEqual(A,B), A =:= B).
           "#,
            expect![[r#"
                -module(main).
                -include_lib("app_b/include/inc.hrl").

                foo(X) -> ?assertEqual(X,2).

            "#]],
        );
    }
}

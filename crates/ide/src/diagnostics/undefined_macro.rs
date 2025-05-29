/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Add an assist to an erlang service diagnostic for an undefined macro.

use elp_ide_completion::WELL_KNOWN_MACROS;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use hir::Semantic;
use lazy_static::lazy_static;
use regex::Regex;
use text_edit::TextEdit;

use super::Diagnostic;
use crate::fix;

pub(crate) fn add_assist(sema: &Semantic, file_id: FileId, diagnostic: &mut Diagnostic) {
    if let Some((macro_name, _macro_arity_str)) = macro_undefined_from_message(&diagnostic.message)
    {
        let well_known_macros = &WELL_KNOWN_MACROS;
        if let Some((name, include)) = well_known_macros
            .iter()
            .find(|(name, _import)| name.as_str() == macro_name)
        {
            if let Some(pos) = include.insert_position_if_needed(sema, file_id) {
                let mut builder = TextEdit::builder();
                builder.insert(pos.offset, include.as_attribute());
                let edit = builder.finish();
                diagnostic.add_fix(fix(
                    "add_macro_include",
                    format!("Add required include for '{name}'").as_str(),
                    SourceChange::from_text_edit(file_id, edit),
                    diagnostic.range,
                ));
            }
        }
    }
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

    #[test]
    fn undefined_macro() {
        check_diagnostics(
            r#"
            //- erlang_service
            //- /app_glean/src/main.erl
            -module(main).

            foo(X) -> ?assertEqual(X,2).
            %%        ^^^^^^^^^^^^ 💡 error: undefined macro 'assertEqual/2'
           "#,
        );
    }

    #[test]
    fn undefined_macro_fix() {
        check_fix(
            r#"
            //- erlang_service
            //- /app_glean/src/main.erl
            -module(main).

            foo(X) -> ?assert~Equal(X,2).
            %%        ^^^^^^^^^^^^ 💡 error: undefined macro 'assertEqual/2'
           "#,
            expect![[r#"
                -module(main).
                -include_lib("stdlib/include/assert.hrl").

                foo(X) -> ?assertEqual(X,2).
            "#]],
        );
    }
}

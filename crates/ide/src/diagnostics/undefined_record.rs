/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Add an assist to an erlang service diagnostic for an undefined record.

use std::sync::Arc;

use elp_ide_completion::get_include_file;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::path_for_file;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use fxhash::FxHashSet;
use hir::RecordDefineIndex;
use hir::Semantic;
use lazy_static::lazy_static;
use regex::Regex;

use super::Diagnostic;
use crate::fix;

pub(crate) fn add_assist(
    sema: &Semantic,
    file_id: FileId,
    index: &Arc<RecordDefineIndex>,
    diagnostic: &mut Diagnostic,
) -> Option<()> {
    let record_name = record_undefined_from_message(&diagnostic.message)?;
    let includes: FxHashSet<_> = index
        .complete(&record_name)
        .iter()
        .flat_map(|(_chars, records)| {
            records.iter().flat_map(|record| {
                let include_path = path_for_file(sema.db, record.file_id);
                if let Some(include_path) = include_path {
                    get_include_file(sema.db, file_id, record.file_id, include_path.clone())
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
                format!(
                    "Add required include for record '{record_name}' ({})",
                    include.app_name
                )
            } else {
                format!("Add required include for record '{record_name}'")
            };
            diagnostic.add_fix(fix(
                "add_record_include",
                label.as_str(),
                SourceChange::from_text_edit(file_id, edit),
                diagnostic.range,
            ));
        }
    }
    Some(())
}

fn record_undefined_from_message(s: &str) -> Option<String> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^record (\S+) undefined$").expect("valid regex");
    }
    let captures = RE.captures(s)?;
    if RE.captures_len() == 2 {
        Some(captures[1].to_string())
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
    fn undefined_record() {
        check_diagnostics(
            r#"
            //- erlang_service
            //- /app/src/main.erl
            -module(main).
            -export([foo/0]).

            foo() -> #my_record{}.
            %%       ^^^^^^^^^^^^ 💡 error: L1252: record my_record undefined
            //- /another-app/include/inc.hrl app:another include_path:/another-app/include
            -record(my_record, {field1, field2}).
           "#,
        );
    }

    #[test]
    fn undefined_record_fix() {
        check_fix(
            r#"
            //- erlang_service
            //- /main/src/main.erl app:main
            -module(main).
            -export([foo/0]).

            foo() -> #my_re~cord{}.
            %%       ^^^^^^^^^^^^ 💡 error: L1252: record my_record undefined
            //- /another-app/include/inc.hrl app:another include_path:/another-app/include
            -record(my_record, {field1, field2}).
           "#,
            expect![[r#"
                -module(main).
                -include_lib("another/include/inc.hrl").
                -export([foo/0]).

                foo() -> #my_record{}.
            "#]],
        );
    }

    #[test]
    fn undefined_record_fix_multiple_app_a() {
        check_specific_fix(
            "Add required include for record 'my_record' (app_a)",
            r#"
            //- erlang_service
            //- /main/src/main.erl app:main
            -module(main).
            -export([foo/0]).

            foo() -> #my_re~cord{}.
            %%       ^^^^^^^^^^^^ 💡 error: L1252: record my_record undefined

            //- /app_a/include/inc.hrl app:app_a include_path:/app_a/include
            -record(my_record, {field1, field2}).

            //- /app_b/include/inc.hrl app:app_b include_path:/app_b/include
            -record(my_record, {field1, field2}).
           "#,
            expect![[r#"
                -module(main).
                -include_lib("app_a/include/inc.hrl").
                -export([foo/0]).

                foo() -> #my_record{}.

            "#]],
        );
    }

    #[test]
    fn undefined_record_fix_multiple_app_b() {
        check_specific_fix(
            "Add required include for record 'my_record' (app_b)",
            r#"
            //- erlang_service
            //- /main/src/main.erl app:main
            -module(main).
            -export([foo/0]).

            foo() -> #my_re~cord{}.
            %%       ^^^^^^^^^^^^ 💡 error: L1252: record my_record undefined

            //- /app_a/include/inc.hrl app:app_a include_path:/app_a/include
            -record(my_record, {field1, field2}).

            //- /app_b/include/inc.hrl app:app_b include_path:/app_b/include
            -record(my_record, {field1, field2}).
           "#,
            expect![[r#"
                -module(main).
                -include_lib("app_b/include/inc.hrl").
                -export([foo/0]).

                foo() -> #my_record{}.

            "#]],
        );
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: unused-record-field
//
// Return a warning if a record field defined in an .erl file has no references to it

use std::borrow::Cow;

use elp_ide_assists::Assist;
use elp_ide_db::SymbolDefinition;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use hir::Semantic;

use crate::codemod_helpers::compute_comma_separated_delete_range;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::DiagnosticTag;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::fix;

pub(crate) struct UnusedRecordFieldLinter;

impl Linter for UnusedRecordFieldLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnusedRecordField
    }
    fn description(&self) -> &'static str {
        "Unused record field."
    }
    fn should_process_generated_files(&self) -> bool {
        true
    }
    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        sema.db.file_kind(file_id).is_module()
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context {
    name: String,
    delete_range: TextRange,
}

impl GenericLinter for UnusedRecordFieldLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let mut res = Vec::new();
        let def_map = sema.def_map(file_id);
        for (name, def) in def_map.get_records() {
            // Only run the check for records defined in the local module,
            // not in the included files.
            if def.file.file_id == file_id {
                // If the record itself is unused, there's little point in showing
                // warnings for each field
                if SymbolDefinition::Record(def.clone())
                    .usages(sema)
                    .at_least_one()
                {
                    for (field_name, field_def) in def.fields(sema.db) {
                        if !SymbolDefinition::RecordField(field_def.clone())
                            .usages(sema)
                            .at_least_one()
                        {
                            let combined_name = format!("{name}.{field_name}");
                            let source = field_def.source(sema.db.upcast());
                            let range = match source.name() {
                                Some(name) => name.syntax().text_range(),
                                None => source.syntax().text_range(),
                            };
                            let delete_range =
                                compute_comma_separated_delete_range(source.syntax());
                            let context = Context {
                                name: combined_name,
                                delete_range,
                            };
                            res.push(GenericLinterMatchContext {
                                range: FileRange { file_id, range },
                                context,
                            });
                        }
                    }
                }
            }
        }
        Some(res)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!("Unused record field ({})", context.name))
    }

    fn tag(&self, _context: &Self::Context) -> Option<DiagnosticTag> {
        Some(DiagnosticTag::Unused)
    }

    fn fixes(
        &self,
        context: &Context,
        _range: TextRange,
        _sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let edit = TextEdit::delete(context.delete_range);
        Some(vec![fix(
            "delete_unused_record_field",
            &format!("Remove unused record field ({})", context.name),
            SourceChange::from_text_edit(file_id, edit),
            context.delete_range,
        )])
    }
}

pub static LINTER: UnusedRecordFieldLinter = UnusedRecordFieldLinter;

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn test_unused_record_field() {
        check_diagnostics(
            r#"
-module(main).

-export([main/1]).

-record(used_field, {field_a, field_b = 42}).
-record(unused_field, {field_c, field_d}).
                             %% ^^^^^^^ 💡 warning: W0003: Unused record field (unused_field.field_d)

main(#used_field{field_a = A, field_b = B}) ->
    {A, B};
main(R) ->
    R#unused_field.field_c.
            "#,
        );
    }

    #[test]
    fn test_unused_record_field_with_type() {
        check_diagnostics(
            r#"
-module(main).

-export([main/1]).

-record(used_field, {field_a, field_b = 42}).
-record(unused_field, {field_c :: atom(), field_d :: number()}).
                                       %% ^^^^^^^ 💡 warning: W0003: Unused record field (unused_field.field_d)

main(#used_field{field_a = A, field_b = B}) ->
    {A, B};
main(R) ->
    R#unused_field.field_c.
            "#,
        );
    }

    #[test]
    fn test_unused_record_whole_record_unused() {
        check_diagnostics(
            r#"
-module(main).

-export([main/1]).

-record(used_field, {field_a, field_b = 42}).

main(_) -> ok.
            "#,
        );
    }

    #[test]
    fn test_unused_record_field_not_applicable() {
        check_diagnostics(
            r#"
-module(main).
-record(used_field, {field_a, field_b = 42}).

main(#used_field{field_a = A} = X) ->
  {A, X#used_field.field_b}.
            "#,
        );
    }

    #[test]
    fn test_unused_record_field_not_applicable_for_hrl_file() {
        check_diagnostics(
            r#"
//- /include/foo.hrl
-record(unused_record, {field_a, field_b}).
            "#,
        );
    }

    #[test]
    fn test_unused_record_field_include() {
        check_diagnostics(
            r#"
//- /include/foo.hrl
-record(unused_record, {field_a, field_b}).
//- /src/foo.erl
-module(foo).
-include("foo.hrl").
main(#used_field{field_a = A}) ->
    {A, B}.
        "#,
        );
    }

    #[test]
    fn test_unused_record_field_nested() {
        check_diagnostics(
            r#"
-module(main).
-record(a, {a1, a2}).
             %% ^^ 💡 warning: W0003: Unused record field (a.a2)
-record(b, {b1, b2}).
         %% ^^ 💡 warning: W0003: Unused record field (b.b1)
main(#a{a1 = #b{b2 = B2}} = A) ->
    {A, B2}.
        "#,
        );
    }

    #[test]
    fn test_unused_record_macro_name() {
        // https://github.com/WhatsApp/erlang-language-platform/issues/51
        check_diagnostics(
            r#"
-module(main).
-export([ test/0 ]).
-record(?MODULE, {queue :: term()}).
test() -> #?MODULE{queue = ok}.
            "#,
        );
    }

    #[test]
    fn fix_remove_last_field() {
        check_fix(
            r#"
-module(main).
-export([main/1]).
-record(my_rec, {field_a, fiel~d_b}).
main(R) ->
    R#my_rec.field_a.
            "#,
            expect![[r#"
-module(main).
-export([main/1]).
-record(my_rec, {field_a}).
main(R) ->
    R#my_rec.field_a.
            "#]],
        );
    }

    #[test]
    fn fix_remove_first_field() {
        check_fix(
            r#"
-module(main).
-export([main/1]).
-record(my_rec, {fiel~d_a, field_b}).
main(R) ->
    R#my_rec.field_b.
            "#,
            expect![[r#"
-module(main).
-export([main/1]).
-record(my_rec, {field_b}).
main(R) ->
    R#my_rec.field_b.
            "#]],
        );
    }

    #[test]
    fn fix_remove_middle_field() {
        check_fix(
            r#"
-module(main).
-export([main/1]).
-record(my_rec, {field_a, fiel~d_b, field_c}).
main(R) ->
    {R#my_rec.field_a, R#my_rec.field_c}.
            "#,
            expect![[r#"
-module(main).
-export([main/1]).
-record(my_rec, {field_a, field_c}).
main(R) ->
    {R#my_rec.field_a, R#my_rec.field_c}.
            "#]],
        );
    }

    #[test]
    fn fix_remove_last_field_with_type() {
        check_fix(
            r#"
-module(main).
-export([main/1]).
-record(my_rec, {field_a :: atom(), fiel~d_b :: number()}).
main(R) ->
    R#my_rec.field_a.
            "#,
            expect![[r#"
-module(main).
-export([main/1]).
-record(my_rec, {field_a :: atom()}).
main(R) ->
    R#my_rec.field_a.
            "#]],
        );
    }

    #[test]
    fn fix_remove_last_field_with_and_value() {
        check_fix(
            r#"
-module(main).
-export([main/1]).
-record(my_rec, {field_a = some_atom :: atom(), fiel~d_b = 42 :: number()}).
main(R) ->
    R#my_rec.field_a.
            "#,
            expect![[r#"
-module(main).
-export([main/1]).
-record(my_rec, {field_a = some_atom :: atom()}).
main(R) ->
    R#my_rec.field_a.
            "#]],
        );
    }
}

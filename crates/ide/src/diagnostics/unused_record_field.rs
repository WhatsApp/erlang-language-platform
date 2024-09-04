/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Diagnostic: unused-record-field
//
// Return a warning if a record field defined in an .erl file has no references to it

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::SymbolDefinition;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use hir::Semantic;

use super::DiagnosticConditions;
use super::DiagnosticDescriptor;
use crate::diagnostics::DiagnosticCode;
use crate::Diagnostic;

pub(crate) static DESCRIPTOR: DiagnosticDescriptor = DiagnosticDescriptor {
    conditions: DiagnosticConditions {
        experimental: false,
        include_generated: true,
        include_tests: true,
        default_disabled: false,
    },
    checker: &|diags, sema, file_id, file_kind| {
        unused_record_field(diags, sema, file_id, file_kind);
    },
};

fn unused_record_field(
    acc: &mut Vec<Diagnostic>,
    sema: &Semantic,
    file_id: FileId,
    file_kind: FileKind,
) -> Option<()> {
    if file_kind.is_module() {
        let def_map = sema.def_map(file_id);
        for (name, def) in def_map.get_records() {
            // Only run the check for records defined in the local module,
            // not in the included files.
            if def.file.file_id == file_id {
                for (field_name, field_def) in def.fields(sema.db) {
                    if !SymbolDefinition::RecordField(field_def.clone())
                        .usages(sema)
                        .at_least_one()
                    {
                        let combined_name = format!("{name}.{field_name}");
                        let range = field_def.source(sema.db.upcast()).syntax().text_range();
                        let d = make_diagnostic(range, &combined_name);
                        acc.push(d);
                    }
                }
            }
        }
    }
    Some(())
}

fn make_diagnostic(name_range: TextRange, name: &str) -> Diagnostic {
    Diagnostic::warning(
        DiagnosticCode::UnusedRecordField,
        name_range,
        format!("Unused record field ({name})"),
    )
}

#[cfg(test)]
mod tests {

    use crate::tests::check_diagnostics;

    #[test]
    fn test_unused_record_field() {
        check_diagnostics(
            r#"
-module(main).

-export([main/1]).

-record(used_field, {field_a, field_b = 42}).
-record(unused_field, {field_c, field_d}).
                             %% ^^^^^^^ warning: Unused record field (unused_field.field_d)

main(#used_field{field_a = A, field_b = B}) ->
    {A, B};
main(R) ->
    R#unused_field.field_c.
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
             %% ^^ warning: Unused record field (a.a2)
-record(b, {b1, b2}).
         %% ^^ warning: Unused record field (b.b1)
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
               %% ^^^^^^^^^^^^^^^ warning: Unused record field ([missing name].queue)
test() -> #?MODULE{queue = ok}.
            "#,
        );
    }
}

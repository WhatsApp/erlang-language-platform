/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: avoid_type_defs_in_header
//!
//! Return a diagnostic for type definitions (`-type`, `-opaque`, `-nominal`)
//! declared in a header file. A type defined in a header is textually copied
//! into every module that includes it, so it has no single owning module to
//! export it from.

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::elp_base_db::FileRange;
use hir::Semantic;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;

pub(crate) struct AvoidTypeDefsInHeaderLinter;

impl Linter for AvoidTypeDefsInHeaderLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::AvoidTypeDefsInHeader
    }

    fn description(&self) -> &'static str {
        "Type definitions should live in modules, not header files."
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        sema.db.file_kind(file_id) == FileKind::Header
    }
}

impl GenericLinter for AvoidTypeDefsInHeaderLinter {
    type Context = ();

    fn matches(
        &self,
        ctx: &LinterContext,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let def_map = sema.def_map_local(file_id);
        let mut res: Vec<_> = def_map
            .get_types()
            .values()
            .filter_map(|type_alias_def| {
                let range = type_alias_def.name_range(sema.db)?;
                Some(GenericLinterMatchContext {
                    range: FileRange { file_id, range },
                    context: (),
                })
            })
            .collect();
        // `get_types()` is a map, so impose a stable order on the diagnostics.
        res.sort_by_key(|matched| matched.range.range.start());
        Some(res)
    }
}

pub static LINTER: AvoidTypeDefsInHeaderLinter = AvoidTypeDefsInHeaderLinter;

#[cfg(test)]
mod tests {

    use crate::tests::check_diagnostics;

    #[test]
    fn type_defs_in_header() {
        check_diagnostics(
            r#"
            //- /src/main.hrl
            -type my_type() :: term().
            %%    ^^^^^^^ 💡 warning: W0083: Type definitions should live in modules, not header files.
            -opaque my_opaque() :: term().
            %%      ^^^^^^^^^ 💡 warning: W0083: Type definitions should live in modules, not header files.
            -record(my_record, {field :: term()}).
            -define(MY_MACRO, 42).
            "#,
        )
    }

    #[test]
    fn type_defs_in_module_not_reported() {
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -type my_type() :: term().
            -opaque my_opaque() :: term().
            -export_type([my_type/0, my_opaque/0]).
            "#,
        )
    }

    #[test]
    fn type_defs_included_from_header_reported_once() {
        // The diagnostic is attached to the header that defines the type,
        // not to every module that includes it.
        check_diagnostics(
            r#"
            //- /src/main.erl
            -module(main).
            -include("types.hrl").
            -spec test() -> my_type().
            test() -> ok.
            //- /src/types.hrl
            -type my_type() :: term().
            %%    ^^^^^^^ 💡 warning: W0083: Type definitions should live in modules, not header files.
            "#,
        )
    }

    #[test]
    fn empty_header_not_reported() {
        check_diagnostics(
            r#"
            //- /src/main.hrl
            -define(MY_MACRO, 42).
            "#,
        )
    }
}

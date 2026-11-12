/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: unused_header
//!
//! Return a diagnostic on a header file that no file of the project includes.

use elp_ide_db::SearchScope;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::elp_base_db::FileRange;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use hir::File;
use hir::InFile;
use hir::Semantic;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::DiagnosticTag;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;

pub(crate) struct UnusedHeaderLinter;

impl Linter for UnusedHeaderLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnusedHeader
    }

    fn description(&self) -> &'static str {
        "No file includes this header."
    }

    // Searches the whole project for includes.
    fn runs_on_save_only(&self) -> bool {
        true
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        sema.db.file_kind(file_id) == FileKind::Header
    }
}

impl GenericLinter for UnusedHeaderLinter {
    type Context = ();

    fn matches(&self, ctx: &LinterContext) -> Option<Vec<GenericLinterMatchContext<()>>> {
        if is_included(ctx.sema, ctx.file_id) {
            return None;
        }
        Some(vec![GenericLinterMatchContext {
            range: first_line(ctx.sema, ctx.file_id),
            context: (),
        }])
    }

    fn tag(&self, _context: &()) -> Option<DiagnosticTag> {
        Some(DiagnosticTag::Unused)
    }
}

pub(crate) static LINTER: UnusedHeaderLinter = UnusedHeaderLinter;

/// Whether another file of the project has an `-include` or `-include_lib`
/// attribute resolving to `file_id`, or to a header of the same application
/// with the same file name: the build system may map such an include to this
/// header, as Buck `includes` overrides do. A header outside any application
/// counts as included, since its project is unknown.
fn is_included(sema: &Semantic, file_id: FileId) -> bool {
    let db = sema.db;
    let Some(app) = db.file_app(file_id) else {
        return true;
    };
    let name = File { file_id }.name(db.upcast());
    let is_this_header = |included: FileId| {
        included == file_id
            || (db.file_app(included) == Some(app)
                && File { file_id: included }.name(db.upcast()) == name)
    };
    SearchScope::project(db, app.project(db))
        .exclude_file(file_id)
        .into_iter()
        .any(|(includer, _)| {
            db.file(includer).text(db).contains(name.as_str())
                && sema.form_list(includer).includes().any(|(idx, _)| {
                    db.resolve_include(db.file_app(includer), InFile::new(includer, idx))
                        .is_some_and(|included| included != includer && is_this_header(included))
                })
        })
}

/// The first line of the header that is neither blank nor a comment, where the
/// diagnostic is reported, so that an `elp:ignore` comment above it applies.
fn first_line(sema: &Semantic, file_id: FileId) -> FileRange {
    let text = sema.db.file(file_id).text(sema.db);
    let mut offset = 0;
    let range = text
        .split_inclusive('\n')
        .find_map(|line| {
            let start = offset;
            offset += line.len();
            let content = line.trim_end();
            let code = content.trim_start();
            (!code.is_empty() && !code.starts_with('%')).then(|| {
                TextRange::at(
                    TextSize::new(start as u32),
                    TextSize::new(content.len() as u32),
                )
            })
        })
        .unwrap_or_default();
    FileRange { file_id, range }
}

#[cfg(test)]
mod tests {
    use elp_ide_db::DiagnosticCode;

    use super::LINTER;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::diagnostics::Linter;
    use crate::tests::check_diagnostics_with_config;

    #[test]
    fn only_runs_on_save() {
        assert!(LINTER.runs_on_save_only());
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::UnusedInclude)
            .disable(DiagnosticCode::UnusedMacro);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn unused_header() {
        check_diagnostics(
            r#"
            //- /include/unused.hrl include_path:/include
            -define(A, 1).
            %%<^^^^^^^^^^^ warning: W0093: No file includes this header.
            %%           | 💡 <suppression>
            //- /src/main.erl
            -module(main).
            "#,
        )
    }

    #[test]
    fn header_included() {
        check_diagnostics(
            r#"
            //- /include/inner.hrl include_path:/include
            -define(A, 1).
            //- /include/outer.hrl include_path:/include
            -include("inner.hrl").
            //- /src/main.erl
            -module(main).
            -include("outer.hrl").
            -export([f/0]).
            f() -> ?A.
            "#,
        )
    }

    #[test]
    fn header_name_suffix_of_other_header() {
        check_diagnostics(
            r#"
            //- /include/types.hrl include_path:/include
            -define(A, 1).
            %%<^^^^^^^^^^^ warning: W0093: No file includes this header.
            %%           | 💡 <suppression>
            //- /include/app_types.hrl include_path:/include
            -define(B, 1).
            //- /src/main.erl
            -module(main).
            -include("app_types.hrl").
            -export([f/0]).
            f() -> ?B.
            "#,
        )
    }

    #[test]
    fn header_named_only_in_comment() {
        check_diagnostics(
            r#"
            //- /include/unused.hrl include_path:/include
            -define(A, 1).
            %%<^^^^^^^^^^^ warning: W0093: No file includes this header.
            %%           | 💡 <suppression>
            //- /src/main.erl
            -module(main).
            %% Used to include "unused.hrl".
            "#,
        )
    }

    #[test]
    fn header_same_name_in_other_app() {
        check_diagnostics(
            r#"
            //- /app_a/include/types.hrl include_path:/app_a/include app:app_a
            -define(A, 1).
            %%<^^^^^^^^^^^ warning: W0093: No file includes this header.
            %%           | 💡 <suppression>
            //- /app_b/include/types.hrl include_path:/app_b/include app:app_b
            -define(B, 1).
            //- /app_b/src/main.erl app:app_b
            -module(main).
            -include("types.hrl").
            -export([f/0]).
            f() -> ?B.
            "#,
        )
    }

    #[test]
    fn header_shadowed_by_same_name() {
        check_diagnostics(
            r#"
            //- /include/logger.hrl include_path:/include
            -define(LOG, 1).
            //- /overrides/logger.hrl
            -define(LOG, 2).
            //- /src/main.erl
            -module(main).
            -include("logger.hrl").
            -export([f/0]).
            f() -> ?LOG.
            "#,
        )
    }

    #[test]
    fn header_below_comments() {
        check_diagnostics(
            r#"
            //- /include/unused.hrl include_path:/include
            %% Copyright

            -define(A, 1).
            %%<^^^^^^^^^^^ warning: W0093: No file includes this header.
            %%           | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn header_suppressed() {
        check_diagnostics(
            r#"
            //- /include/unused.hrl include_path:/include
            %% Copyright
            % elp:ignore W0093
            -define(A, 1).
            "#,
        )
    }
}

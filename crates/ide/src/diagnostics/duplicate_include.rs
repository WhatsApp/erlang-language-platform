/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: duplicate include
//
// Return a warning if a header file is included more than once

use std::borrow::Cow;

use elp_ide_assists::helpers::extend_range;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_syntax::SmolStr;
use elp_syntax::TextRange;
use elp_syntax::ast::AstNode;
use fxhash::FxHashMap;
use hir::InFile;
use hir::IncludeAttribute;
use hir::IncludeAttributeId;
use hir::Semantic;

use crate::Assist;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::fix;

pub(crate) struct DuplicateIncludeLinter;

impl Linter for DuplicateIncludeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::DuplicateInclude
    }

    fn description(&self) -> &'static str {
        "Duplicate include file"
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        let file_kind = sema.db.file_kind(file_id);
        file_kind.is_module()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context {
    path: SmolStr,
    extended_range: TextRange,
}

impl Default for Context {
    fn default() -> Self {
        Context {
            path: SmolStr::new(""),
            extended_range: TextRange::default(),
        }
    }
}

struct IncludeInfo {
    include_idx: IncludeAttributeId,
    is_include_lib: bool,
    path: SmolStr,
}

impl GenericLinter for DuplicateIncludeLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let db = sema.db;
        let form_list = db.file_form_list(file_id);
        let source_file = db.parse(file_id);
        let source_app_name = db.file_app_name(file_id);

        // Pass 1: group includes by resolved file ID
        let mut groups: FxHashMap<FileId, Vec<IncludeInfo>> = FxHashMap::default();
        for (include_idx, attr) in form_list.includes() {
            let in_file = InFile::new(file_id, include_idx);
            if let Some(include_file_id) =
                db.resolve_include(db.app_data_id_by_file(file_id), in_file)
            {
                let (is_include_lib, path) = match attr {
                    IncludeAttribute::Include { path, .. } => (false, path.clone()),
                    IncludeAttribute::IncludeLib { path, .. } => (true, path.clone()),
                };
                groups
                    .entry(include_file_id)
                    .or_default()
                    .push(IncludeInfo {
                        include_idx,
                        is_include_lib,
                        path,
                    });
            }
        }

        // Pass 2: for each group with duplicates, decide which to keep
        let mut res = Vec::new();
        for (include_file_id, includes) in &groups {
            if includes.len() < 2 {
                continue;
            }

            // Determine preferred form based on whether header is from the same app
            let included_app_name = db.file_app_name(*include_file_id);
            let same_app = source_app_name.is_some() && source_app_name == included_app_name;
            // Same app: prefer -include; different app: prefer -include_lib
            let prefer_include_lib = !same_app;

            // Find the index of the include to keep: first match of
            // the preferred form, or else just the first include
            let keep_idx = includes
                .iter()
                .position(|inc| inc.is_include_lib == prefer_include_lib)
                .unwrap_or(0);

            // Flag all others as duplicates
            for (i, inc) in includes.iter().enumerate() {
                if i == keep_idx {
                    continue;
                }
                let attr = &form_list[inc.include_idx];
                let attribute = attr.form_id().get(&source_file.tree());
                let attribute_syntax = attribute.syntax();
                let attribute_range = attribute_syntax.text_range();
                let extended_attribute_range = extend_range(attribute_syntax);

                res.push(GenericLinterMatchContext {
                    range: FileRange {
                        file_id,
                        range: attribute_range,
                    },
                    context: Context {
                        path: inc.path.clone(),
                        extended_range: extended_attribute_range,
                    },
                });
            }
        }
        Some(res)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!("Duplicate include: {}", context.path))
    }

    fn fixes(
        &self,
        context: &Self::Context,
        _range: TextRange,
        _sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let mut edit_builder = TextEdit::builder();
        edit_builder.delete(context.extended_range);
        let edit = edit_builder.finish();

        Some(vec![fix(
            "remove_duplicate_include",
            "Remove duplicate include",
            SourceChange::from_text_edit(file_id, edit),
            context.extended_range,
        )])
    }
}

pub static LINTER: DuplicateIncludeLinter = DuplicateIncludeLinter;

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix;

    #[track_caller]
    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .disable(DiagnosticCode::UndefinedFunction)
            .disable(DiagnosticCode::UnusedInclude)
            .disable(DiagnosticCode::UnspecificInclude)
            .disable(DiagnosticCode::NoDialyzerAttribute);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn duplicate_include_detected() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  -define(FOO, 1).
//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
  -include("foo.hrl").
%%^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0078: Duplicate include: foo.hrl
  foo() -> ?FOO.
        "#,
        );
    }

    #[test]
    fn no_duplicate_single_include() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  -define(FOO, 1).
//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
  foo() -> ?FOO.
        "#,
        );
    }

    #[test]
    fn no_duplicate_different_files() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  -define(FOO, 1).
//- /include/bar.hrl include_path:/include
  -define(BAR, 2).
//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
  -include("bar.hrl").
  foo() -> {?FOO, ?BAR}.
        "#,
        );
    }

    #[test]
    fn duplicate_include_lib_detected() {
        check_diagnostics(
            r#"
//- /myapp/include/foo.hrl include_path:/myapp/include app:myapp
  -define(FOO, 1).
//- /src/foo.erl app:myapp
  -module(foo).
  -include_lib("myapp/include/foo.hrl").
  -include_lib("myapp/include/foo.hrl").
%%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0078: Duplicate include: myapp/include/foo.hrl
  foo() -> ?FOO.
        "#,
        );
    }

    #[test]
    fn duplicate_mixed_same_app_keeps_include() {
        // Same app: prefer -include, flag -include_lib
        check_diagnostics(
            r#"
//- /myapp/include/foo.hrl include_path:/myapp/include app:myapp
  -define(FOO, 1).
//- /src/foo.erl app:myapp
  -module(foo).
  -include("foo.hrl").
  -include_lib("myapp/include/foo.hrl").
%%^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0078: Duplicate include: myapp/include/foo.hrl
  foo() -> ?FOO.
        "#,
        );
    }

    #[test]
    fn duplicate_mixed_cross_app_keeps_include_lib() {
        // Different app: prefer -include_lib, flag -include
        check_diagnostics(
            r#"
//- /otherapp/include/foo.hrl include_path:/otherapp/include app:otherapp
  -define(FOO, 1).
//- /src/foo.erl app:myapp include_path:/otherapp/include
  -module(foo).
  -include("foo.hrl").
%%^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0078: Duplicate include: foo.hrl
  -include_lib("otherapp/include/foo.hrl").
  foo() -> ?FOO.
        "#,
        );
    }

    #[test]
    fn triple_duplicate_include() {
        check_diagnostics(
            r#"
//- /include/foo.hrl include_path:/include
  -define(FOO, 1).
//- /src/foo.erl
  -module(foo).
  -include("foo.hrl").
  -include("foo.hrl").
%%^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0078: Duplicate include: foo.hrl
  -include("foo.hrl").
%%^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0078: Duplicate include: foo.hrl
  foo() -> ?FOO.
        "#,
        );
    }

    #[test]
    fn fixes_duplicate_include() {
        check_fix(
            r#"
//- /include/foo.hrl include_path:/include
  -define(FOO, 1).
//- /src/main.erl
-module(main).
-include("foo.hrl").
-incl~ude("foo.hrl").
%%<^^^^^^^^^^^^^^^^^^^^ 💡 warning: Duplicate include: foo.hrl

foo() -> ?FOO.
"#,
            expect![[r#"
-module(main).
-include("foo.hrl").

foo() -> ?FOO.
"#]],
        )
    }
}

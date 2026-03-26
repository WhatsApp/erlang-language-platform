/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: unused-exported-function
//
// Return a warning if an exported function in an .erl file has no external usages

use std::borrow::Cow;

use elp_ide_assists::Assist;
use elp_ide_assists::helpers::extend_range;
use elp_ide_db::SearchScope;
use elp_ide_db::SymbolDefinition;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use hir::NameArity;
use hir::Semantic;
use hir::known;

use crate::codemod_helpers::compute_comma_separated_delete_range;
use crate::common_test;
use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::DiagnosticTag;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::fix;

pub(crate) struct UnusedExportedFunctionLinter;

impl Linter for UnusedExportedFunctionLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnusedExportedFunction
    }

    fn is_enabled(&self) -> bool {
        false
    }

    fn description(&self) -> &'static str {
        "Exported function is not used anywhere else in the codebase."
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        if !sema.db.file_kind(file_id).is_module() {
            return false;
        }
        // Test suites have too many false positives (exported test
        // callbacks, etc.), so skip them.  We still process test
        // helpers that don't end in _SUITE.
        if let Some(module_name) = sema.module_name(file_id)
            && common_test::is_suite(&module_name)
        {
            return false;
        }
        true
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context {
    name: String,
    arity: u32,
}

impl GenericLinter for UnusedExportedFunctionLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let mut res = Vec::new();
        let def_map = sema.def_map_local(file_id);

        let callbacks = sema.resolve_callbacks(file_id);

        let project_id = sema.db.file_app_data(file_id)?.project_id;
        let scope = SearchScope::project(sema.db, project_id).exclude_file(file_id);

        // Hoist the single-file scope before the loop to avoid allocating
        // a new FxHashMap on every iteration (file_id is loop-invariant).
        let self_scope = SearchScope::single_file(file_id, None);

        for name_arity in def_map.get_exported_functions() {
            if *name_arity.name() == known::module_info
                && (name_arity.arity() == 0 || name_arity.arity() == 1)
            {
                continue;
            }

            if *name_arity.name() == known::start_link {
                continue;
            }

            if callbacks.contains(name_arity) {
                continue;
            }

            let Some(fun_def) = def_map.get_function(name_arity) else {
                continue;
            };

            // Check for same-file call sites first (cheap, single-file search)
            // before falling back to the expensive project-wide search.
            // Since the function is exported, the export attribute itself
            // counts as one reference, and a spec adds another.
            // Any reference beyond that baseline is a real call site.
            let mut self_usages = SymbolDefinition::Function(fun_def.clone()).usages(sema);
            self_usages.set_scope(&self_scope);
            let baseline = if fun_def.spec.is_some() { 2 } else { 1 };
            if self_usages.at_least_n(baseline + 1) {
                continue;
            }

            let mut usages = SymbolDefinition::Function(fun_def.clone()).usages(sema);
            usages.set_scope(&scope);
            let has_external_usage = usages.at_least_one();

            if !has_external_usage && let Some(name_range) = fun_def.name_range(sema.db) {
                let context = Context {
                    name: name_arity.name().to_string(),
                    arity: name_arity.arity(),
                };
                res.push(GenericLinterMatchContext {
                    range: FileRange {
                        file_id,
                        range: name_range,
                    },
                    context,
                });
            }
        }
        Some(res)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!(
            "Exported function {}/{} is not used anywhere else in the codebase.",
            context.name, context.arity
        ))
    }

    fn tag(&self, _context: &Self::Context) -> Option<DiagnosticTag> {
        Some(DiagnosticTag::Unused)
    }

    fn fixes(
        &self,
        context: &Context,
        _range: TextRange,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let def_map = sema.def_map(file_id);
        let name_arity =
            NameArity::new(hir::Name::from_erlang_service(&context.name), context.arity);
        let fun_def = def_map.get_function(&name_arity)?;

        let mut builder = TextEdit::builder();

        // Delete all function clauses
        for fun_decl in fun_def.source(sema.db.upcast()) {
            builder.delete(extend_range(fun_decl.syntax()));
        }

        // Delete the spec if present
        if let Some(spec) = &fun_def.spec {
            let ast_spec = spec.source(sema.db.upcast());
            builder.delete(extend_range(ast_spec.syntax()));
        }

        // Delete the -doc attribute if present
        if let Some(doc_id) = fun_def.doc_id {
            let form_list = sema.form_list(file_id);
            let doc_attr = form_list[doc_id].form_id.get_ast(sema.db, file_id);
            builder.delete(extend_range(doc_attr.syntax()));
        }

        // Delete the -doc metadata attribute if present
        if let Some(doc_metadata_id) = fun_def.doc_metadata_id {
            let form_list = sema.form_list(file_id);
            let doc_metadata_attr = form_list[doc_metadata_id].form_id.get_ast(sema.db, file_id);
            builder.delete(extend_range(doc_metadata_attr.syntax()));
        }

        // Remove from export list
        let source = sema.parse(file_id).value;
        let form_list = sema.form_list(file_id);
        for (_idx, export) in form_list.exports() {
            let export_ast = export.form_id.get(&source);
            for fun in export_ast.funs() {
                if fun.syntax().text().to_string() == format!("{}/{}", context.name, context.arity)
                {
                    builder.delete(compute_comma_separated_delete_range(fun.syntax()));
                }
            }
        }

        let edit = builder.finish();
        let trigger_range = fun_def.range(sema.db.upcast())?;
        Some(vec![fix(
            "delete_unused_exported_function",
            &format!("Remove unused function {}/{}", context.name, context.arity),
            SourceChange::from_text_edit(file_id, edit),
            trigger_range,
        )])
    }
}

pub static LINTER: UnusedExportedFunctionLinter = UnusedExportedFunctionLinter;

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;
    use crate::tests::check_fix_with_config;

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::UnusedExportedFunction)
            .disable(DiagnosticCode::UndefinedFunction);
        check_diagnostics_with_config(config, fixture)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: expect_test::Expect) {
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::UnusedExportedFunction)
            .disable(DiagnosticCode::UndefinedFunction);
        check_fix_with_config(config, fixture_before, fixture_after)
    }

    #[test]
    fn test_unused_exported_function_basic() {
        check_diagnostics(
            r#"
-module(main).
-export([foo/0]).

   foo() -> ok.
%% ^^^ 💡 warning: W0077: Exported function foo/0 is not used anywhere else in the codebase.
            "#,
        );
    }

    #[test]
    fn test_used_exported_function() {
        check_diagnostics(
            r#"
//- /src/mod_a.erl
-module(mod_a).
-export([main/0]).

main() -> ok.

//- /src/mod_b.erl
-module(mod_b).

bar() -> mod_a:main().
            "#,
        );
    }

    #[test]
    fn test_used_exported_function_same_module() {
        check_diagnostics(
            r#"
//- /src/mod_a.erl
-module(mod_a).
-export([foo/0, bar/0]).

foo() -> bar().

bar() -> ok.

//- /src/mod_b.erl
-module(mod_b).

foo() -> mod_a:foo().
            "#,
        );
    }

    #[test]
    fn test_unexported_function_no_warning() {
        check_diagnostics(
            r#"
-module(main).

foo() -> ok.
            "#,
        );
    }

    #[test]
    fn test_behaviour_callback_no_warning() {
        check_diagnostics(
            r#"
//- /src/main.erl
-module(main).
-behaviour(gen_server).
-export([init/1]).

init(_Args) -> {ok, #{}}.

//- /src/gen_server.erl
-module(gen_server).
-callback init(term()) -> term().
            "#,
        );
    }

    #[test]
    fn test_module_info_no_warning() {
        check_diagnostics(
            r#"
-module(main).
-export([module_info/0, module_info/1]).

module_info() -> [].
module_info(_) -> [].
            "#,
        );
    }

    #[test]
    fn test_export_all_warns() {
        check_diagnostics(
            r#"
-module(main).
-compile([export_all]).

   foo() -> ok.
%% ^^^ 💡 warning: W0077: Exported function foo/0 is not used anywhere else in the codebase.
   bar() -> ok.
%% ^^^ 💡 warning: W0077: Exported function bar/0 is not used anywhere else in the codebase.
            "#,
        );
    }

    #[test]
    fn test_start_link_no_warning() {
        check_diagnostics(
            r#"
-module(main).
-export([start_link/0, start_link/1]).

start_link() -> ok.
start_link(_Args) -> ok.
            "#,
        );
    }

    #[test]
    fn test_hrl_file_no_warning() {
        check_diagnostics(
            r#"
//- /include/foo.hrl
-export([foo/0]).
foo() -> ok.
            "#,
        );
    }

    #[test]
    fn test_suite_no_warning() {
        check_diagnostics(
            r#"
//- common_test
//- /my_app/test/my_SUITE.erl
-module(my_SUITE).
-export([all/0, my_test/1]).

all() -> [my_test].
my_test(_Config) -> ok.
            "#,
        );
    }

    #[test]
    fn test_test_helper_still_warns() {
        check_diagnostics(
            r#"
//- /test/my_test_helper.erl
-module(my_test_helper).
-export([helper/0]).

   helper() -> ok.
%% ^^^^^^ 💡 warning: W0077: Exported function helper/0 is not used anywhere else in the codebase.
            "#,
        );
    }

    #[test]
    fn test_multi_module_used() {
        check_diagnostics(
            r#"
//- /src/mod_a.erl
-module(mod_a).
-export([helper/1]).

helper(X) -> X + 1.

//- /src/mod_b.erl
-module(mod_b).

main() -> mod_a:helper(42).
            "#,
        );
    }

    #[test]
    fn fix_remove_only_exported_function() {
        check_fix(
            r#"
-module(main).
-export([foo/0]).

fo~o() -> ok.
            "#,
            expect![[r#"
-module(main).
-export([]).

            "#]],
        );
    }

    #[test]
    fn fix_remove_one_of_two_exports() {
        check_fix(
            r#"
-module(main).
-export([foo/0, bar/0]).

fo~o() -> ok.
bar() -> baz().
baz() -> ok.
            "#,
            expect![[r#"
-module(main).
-export([bar/0]).

bar() -> baz().
baz() -> ok.
            "#]],
        );
    }

    #[test]
    fn fix_remove_with_spec() {
        check_fix(
            r#"
-module(main).
-export([foo/0]).

-spec foo() -> ok.
fo~o() -> ok.
            "#,
            expect![[r#"
-module(main).
-export([]).

            "#]],
        );
    }

    #[test]
    fn fix_remove_with_doc_attribute() {
        check_fix(
            r#"
-module(main).
-export([foo/0]).

-doc "Does foo things.".
fo~o() -> ok.
            "#,
            expect![[r#"
-module(main).
-export([]).

            "#]],
        );
    }

    #[test]
    fn fix_remove_with_doc_and_spec() {
        check_fix(
            r#"
-module(main).
-export([foo/0, bar/0]).

-doc "Does foo things.".
-spec foo() -> ok.
fo~o() -> ok.
bar() -> baz().
baz() -> ok.
            "#,
            expect![[r#"
-module(main).
-export([bar/0]).

bar() -> baz().
baz() -> ok.
            "#]],
        );
    }

    #[test]
    fn fix_remove_multi_clause() {
        check_fix(
            r#"
-module(main).
-export([foo/1]).

fo~o(1) -> one;
foo(_) -> other.
            "#,
            expect![[r#"
-module(main).
-export([]).

            "#]],
        );
    }
}

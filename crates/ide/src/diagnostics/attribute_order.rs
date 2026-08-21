/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Lint: attribute_order
//!
//! Return a diagnostic when module-level attributes do not follow the ordering
//! configured in the `[attribute_order]` section of `.elp_lint.toml`:
//!
//! ```toml
//! [attribute_order]
//! order = ["module", "oncall", "export", ["type", "opaque", "nominal", "define", "record"]]
//! ```
//!
//! Each entry is either a single attribute name or a group of interchangeable
//! names that share a rank. Attributes are matched by their literal name, so
//! custom (wild) attributes can be ordered too. The linter is a no-op unless an
//! `order` is configured. Only modules (`.erl`) are checked — headers (`.hrl`)
//! are skipped, so the include-guard idiom (a `-define` before `-include_lib`)
//! is never flagged. Ranking is per-file, so attributes pulled in from included
//! headers are not considered.

use std::borrow::Cow;

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_syntax::AstNode;
use hir::FormIdx;
use hir::FormList;
use hir::IncludeAttribute;
use hir::PPDirective;
use hir::Semantic;
use hir::TypeAlias;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;

/// The name identifying a top-level form for ordering purposes, or `None` for
/// forms that are not orderable attributes (function clauses, specs,
/// preprocessor conditionals, SSR definitions). Structured attributes map to
/// their canonical keyword (so both `-behaviour` and `-behavior` are
/// `behaviour`); wild attributes use their literal name, so any custom
/// attribute can be ordered.
fn attr_token(form_list: &FormList, form_idx: FormIdx) -> Option<&str> {
    let token = match form_idx {
        FormIdx::ModuleAttribute(_) => "module",
        FormIdx::Attribute(id) => form_list[id].name.as_str(),
        FormIdx::ModuleDocAttribute(_) | FormIdx::ModuleDocMetadataAttribute(_) => "moduledoc",
        FormIdx::DocAttribute(_) | FormIdx::DocMetadataAttribute(_) => "doc",
        FormIdx::CompileOption(_) => "compile",
        FormIdx::Behaviour(_) => "behaviour",
        FormIdx::Callback(_) => "callback",
        FormIdx::OptionalCallbacks(_) => "optional_callbacks",
        FormIdx::Export(_) => "export",
        FormIdx::TypeExport(_) => "export_type",
        FormIdx::Import(_) => "import",
        FormIdx::ImportRecord(_) => "import_record",
        FormIdx::ExportRecord(_) => "export_record",
        FormIdx::DeprecatedAttribute(_) => "deprecated",
        FormIdx::FeatureAttribute(_) => "feature",
        FormIdx::PPDirective(id) => match &form_list[id] {
            PPDirective::Include(include_id) => match &form_list[*include_id] {
                IncludeAttribute::Include { .. } => "include",
                IncludeAttribute::IncludeLib { .. } => "include_lib",
            },
            PPDirective::Define(_) => "define",
            PPDirective::Undef { .. } => return None,
        },
        FormIdx::TypeAlias(id) => match &form_list[id] {
            TypeAlias::Regular { .. } => "type",
            TypeAlias::Opaque { .. } => "opaque",
            TypeAlias::Nominal { .. } => "nominal",
        },
        FormIdx::Record(_) => "record",
        FormIdx::FunctionClause(_)
        | FormIdx::PPCondition(_)
        | FormIdx::Spec(_)
        | FormIdx::SsrDefinition(_) => return None,
    };
    Some(token)
}

pub(crate) struct AttributeOrderLinter;

/// Context carrying the message describing the specific ordering violation.
#[derive(Clone, Debug)]
pub(crate) struct Context {
    message: String,
}

impl Linter for AttributeOrderLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::AttributeOrder
    }

    fn description(&self) -> &'static str {
        "Attribute is out of order."
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        sema.db.file_kind(file_id).is_module()
    }
}

impl GenericLinter for AttributeOrderLinter {
    type Context = Context;

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(context.message.clone())
    }

    fn matches(
        &self,
        ctx: &LinterContext,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let config = &ctx.lint_config()?.attribute_order;
        if config.order.is_empty() {
            return None;
        }
        let rank_of = config.rank_of();

        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let form_list = sema.form_list(file_id);

        let mut res = Vec::new();
        let mut highest: Option<(usize, &str)> = None;

        for &form_idx in form_list.forms() {
            let Some(token) = attr_token(&form_list, form_idx) else {
                continue;
            };
            let Some(&rank) = rank_of.get(token) else {
                continue;
            };

            match highest {
                Some((prev_rank, prev_token)) if rank < prev_rank => {
                    let range = form_list
                        .get(form_idx)
                        .form_id(&form_list)
                        .get_ast(sema.db, file_id)
                        .syntax()
                        .text_range();
                    res.push(GenericLinterMatchContext {
                        range: FileRange { file_id, range },
                        context: Context {
                            message: format!("`-{token}` must appear before `-{prev_token}`."),
                        },
                    });
                }
                Some((prev_rank, _)) if rank > prev_rank => highest = Some((rank, token)),
                Some(_) => {}
                None => highest = Some((rank, token)),
            }
        }
        Some(res)
    }
}

pub static LINTER: AttributeOrderLinter = AttributeOrderLinter;

#[cfg(test)]
mod tests {
    use elp_ide_db::DiagnosticCode;

    use crate::diagnostics::AttributeOrderConfig;
    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::diagnostics::LintConfig;
    use crate::diagnostics::OrderEntry;
    use crate::tests::check_filtered_diagnostics_with_config;

    fn single(name: &str) -> OrderEntry {
        OrderEntry::Single(name.to_string())
    }

    fn group(names: &[&str]) -> OrderEntry {
        OrderEntry::Group(names.iter().map(|s| s.to_string()).collect())
    }

    fn canonical_order() -> Vec<OrderEntry> {
        vec![
            single("module"),
            single("author"),
            single("oncall"),
            single("moduledoc"),
            single("compile"),
            single("behaviour"),
            single("export"),
            single("export_type"),
            group(&["include", "include_lib"]),
            // `-type`/`-opaque`/`-nominal`, `-define` and `-record` share one
            // rank: Erlang requires a record or macro used inside a type to be
            // declared before it, so their relative order can't be enforced
            // without risking uncompilable code.
            group(&["type", "opaque", "nominal", "define", "record"]),
        ]
    }

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::AttributeOrder
    }

    fn config_with_order(order: Vec<OrderEntry>) -> DiagnosticsConfig {
        let lint_config = LintConfig {
            attribute_order: AttributeOrderConfig {
                order,
                ..Default::default()
            },
            ..Default::default()
        };
        DiagnosticsConfig {
            lint_config: Some(lint_config),
            ..Default::default()
        }
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        check_filtered_diagnostics_with_config(
            config_with_order(canonical_order()),
            &vec![],
            fixture,
            &filter,
        )
    }

    #[test]
    fn canonical_order_ok() {
        check_diagnostics(
            r#"
//- /src/main.erl
    -module(main).
    -oncall("an_oncall").
    -compile(warn_missing_spec_all).
    -behaviour(gen_server).
    -export([foo/0]).
    -export_type([bar/0]).
    -include("header.hrl").
    -type bar() :: term().
    -define(BAZ, 1).
    -record(rec, {field :: term()}).
    foo() -> ok.
//- /src/header.hrl
"#,
        )
    }

    #[test]
    fn oncall_after_compile() {
        check_diagnostics(
            r#"
//- /src/main.erl
    -module(main).
    -compile(warn_missing_spec_all).
    -oncall("an_oncall").
%%  ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0084: `-oncall` must appear before `-compile`.
"#,
        )
    }

    #[test]
    fn export_before_behaviour() {
        check_diagnostics(
            r#"
//- /src/main.erl
    -module(main).
    -export([foo/0]).
    -behaviour(gen_server).
%%  ^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0084: `-behaviour` must appear before `-export`.
    foo() -> ok.
"#,
        )
    }

    #[test]
    fn define_before_include() {
        check_diagnostics(
            r#"
//- /src/main.erl
    -module(main).
    -define(BAZ, 1).
    -include("header.hrl").
%%  ^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0084: `-include` must appear before `-define`.
//- /src/header.hrl
"#,
        )
    }

    #[test]
    fn grouped_definitions_are_interchangeable() {
        // Records and macros legitimately precede the types built on them;
        // grouping `type`/`define`/`record` keeps that valid, so no violation is
        // reported.
        check_diagnostics(
            r#"
//- /src/main.erl
    -module(main).
    -define(SIZE, 1).
    -record(rec, {field :: term()}).
    -type sized() :: #rec{}.
"#,
        )
    }

    #[test]
    fn headers_are_not_checked() {
        // Only `.erl` modules are checked. Headers wrap their body in an include
        // guard (`-ifndef`/`-define`/`-include_lib`), which would otherwise flag
        // the guard `-define` as preceding the includes.
        check_diagnostics(
            r#"
//- /src/main.hrl
    -ifndef(MAIN_HRL).
    -define(MAIN_HRL, 1).
    -include_lib("kernel/include/file.hrl").
    -define(SIZE, 1).
    -endif.
"#,
        )
    }

    #[test]
    fn included_header_does_not_affect_module_order() {
        check_diagnostics(
            r#"
//- /src/main.erl
    -module(main).
    -export([foo/0]).
    -include("header.hrl").
    foo() -> ok.
//- /src/header.hrl
    -define(BAZ, 1).
"#,
        )
    }

    #[test]
    fn no_op_without_config() {
        // Enabled by default, but with no `order` configured it reports nothing,
        // even for attributes that are clearly out of order.
        check_filtered_diagnostics_with_config(
            DiagnosticsConfig::default(),
            &vec![],
            r#"
//- /src/main.erl
    -module(main).
    -compile(warn_missing_spec_all).
    -oncall("an_oncall").
"#,
            &filter,
        )
    }

    #[test]
    fn partial_order_only_ranks_listed_attributes() {
        // Only `oncall` and `export` are configured; `-compile` and `-include`
        // are unranked and never trigger or anchor a violation.
        check_filtered_diagnostics_with_config(
            config_with_order(vec![single("oncall"), single("export")]),
            &vec![],
            r#"
//- /src/main.erl
    -module(main).
    -compile(warn_missing_spec_all).
    -export([foo/0]).
    -include("header.hrl").
    -oncall("an_oncall").
%%  ^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0084: `-oncall` must appear before `-export`.
    foo() -> ok.
//- /src/header.hrl
"#,
            &filter,
        )
    }

    #[test]
    fn grouped_attributes_are_interchangeable() {
        // `include` and `include_lib` share a rank, so either order is accepted.
        check_diagnostics(
            r#"
//- /src/main.erl
    -module(main).
    -include_lib("kernel/include/file.hrl").
    -include("header.hrl").
    foo() -> ok.
//- /src/header.hrl
"#,
        )
    }

    #[test]
    fn wild_attribute_can_be_ordered() {
        // A custom attribute is ranked by its literal name.
        check_filtered_diagnostics_with_config(
            config_with_order(vec![single("module"), single("my_attr"), single("export")]),
            &vec![],
            r#"
//- /src/main.erl
    -module(main).
    -export([foo/0]).
    -my_attr(something).
%%  ^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0084: `-my_attr` must appear before `-export`.
    foo() -> ok.
"#,
            &filter,
        )
    }

    #[test]
    fn structured_attribute_recognized_by_keyword() {
        // `-import` is a dedicated form (not a wild attribute); it is still
        // rankable by its keyword.
        check_filtered_diagnostics_with_config(
            config_with_order(vec![single("module"), single("import"), single("export")]),
            &vec![],
            r#"
//- /src/main.erl
    -module(main).
    -export([foo/0]).
    -import(lists, [map/2]).
%%  ^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0084: `-import` must appear before `-export`.
    foo() -> ok.
"#,
            &filter,
        )
    }
}

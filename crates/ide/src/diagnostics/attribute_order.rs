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
//! Return a diagnostic when module-level attributes do not follow the canonical
//! ordering. Ranking is per-file, so attributes pulled in from included headers
//! are not considered. Disabled by default.

use std::borrow::Cow;

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileKind;
use elp_ide_db::elp_base_db::FileRange;
use elp_syntax::AstNode;
use hir::FormIdx;
use hir::FormList;
use hir::PPDirective;
use hir::Semantic;
use hir::known;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum AttrRank {
    Module,
    Author,
    Oncall,
    ModuleDoc,
    Compile,
    Behaviour,
    Export,
    ExportType,
    Include,
    Type,
    Define,
    Record,
}

impl AttrRank {
    fn label(self) -> &'static str {
        match self {
            AttrRank::Module => "-module",
            AttrRank::Author => "-author",
            AttrRank::Oncall => "-oncall",
            AttrRank::ModuleDoc => "-moduledoc",
            AttrRank::Compile => "-compile",
            AttrRank::Behaviour => "-behaviour",
            AttrRank::Export => "-export",
            AttrRank::ExportType => "-export_type",
            AttrRank::Include => "-include",
            AttrRank::Type => "-type",
            AttrRank::Define => "-define",
            AttrRank::Record => "-record",
        }
    }
}

fn attr_rank(form_list: &FormList, form_idx: FormIdx) -> Option<AttrRank> {
    let rank = match form_idx {
        FormIdx::ModuleAttribute(_) => AttrRank::Module,
        FormIdx::Attribute(id) => {
            let name = &form_list[id].name;
            if *name == *known::author {
                AttrRank::Author
            } else if *name == *known::oncall {
                AttrRank::Oncall
            } else {
                return None;
            }
        }
        FormIdx::ModuleDocAttribute(_) | FormIdx::ModuleDocMetadataAttribute(_) => {
            AttrRank::ModuleDoc
        }
        FormIdx::CompileOption(_) => AttrRank::Compile,
        FormIdx::Behaviour(_) => AttrRank::Behaviour,
        FormIdx::Export(_) => AttrRank::Export,
        FormIdx::TypeExport(_) => AttrRank::ExportType,
        FormIdx::PPDirective(id) => match &form_list[id] {
            PPDirective::Include(_) => AttrRank::Include,
            PPDirective::Define(_) => AttrRank::Define,
            PPDirective::Undef { .. } => return None,
        },
        FormIdx::TypeAlias(_) => AttrRank::Type,
        FormIdx::Record(_) => AttrRank::Record,
        _ => return None,
    };
    Some(rank)
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

    fn is_enabled(&self) -> bool {
        false
    }

    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        let kind = sema.db.file_kind(file_id);
        kind.is_module() || kind == FileKind::Header
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
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let form_list = sema.form_list(file_id);

        let mut res = Vec::new();
        let mut highest: Option<AttrRank> = None;

        for &form_idx in form_list.forms() {
            let Some(rank) = attr_rank(&form_list, form_idx) else {
                continue;
            };
            match highest {
                Some(prev) if rank < prev => {
                    let range = form_list
                        .get(form_idx)
                        .form_id(&form_list)
                        .get_ast(sema.db, file_id)
                        .syntax()
                        .text_range();
                    res.push(GenericLinterMatchContext {
                        range: FileRange { file_id, range },
                        context: Context {
                            message: format!(
                                "`{}` must appear before `{}`.",
                                rank.label(),
                                prev.label()
                            ),
                        },
                    });
                }
                Some(prev) if rank > prev => highest = Some(rank),
                Some(_) => {}
                None => highest = Some(rank),
            }
        }
        Some(res)
    }
}

pub static LINTER: AttributeOrderLinter = AttributeOrderLinter;

#[cfg(test)]
mod tests {
    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_filtered_diagnostics_with_config;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::AttributeOrder
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default().enable(DiagnosticCode::AttributeOrder);
        check_filtered_diagnostics_with_config(config, &vec![], fixture, &filter)
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
    fn header_file_ranked_on_its_own() {
        check_diagnostics(
            r#"
//- /src/main.hrl
    -define(BAZ, 1).
    -type bar() :: term().
%%  ^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: W0084: `-type` must appear before `-define`.
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
    fn disabled_by_default() {
        let config = DiagnosticsConfig::default();
        check_filtered_diagnostics_with_config(
            config,
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
}

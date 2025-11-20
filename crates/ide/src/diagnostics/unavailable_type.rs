/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: unavailable-type
//
// Return a warning when referring to a type which is not defined in the module's dependencies.
// This diagnostic checks if a type referenced in specs, type definitions, or opaque types exists in:
// 1. The current module (local types)
// 2. Built-in Erlang types
// 3. Types from the module's application dependencies (including OTP apps)

use std::borrow::Cow;

use elp_ide_db::elp_base_db::AppData;
use elp_ide_db::elp_base_db::FileId;
use elp_project_model::AppName;
use hir::AnyExpr;
use hir::Callback;
use hir::InFile;
use hir::Semantic;
use hir::Spec;
use hir::Strategy;
use hir::TypeAlias;
use hir::TypeExpr;
use hir::fold::Fold;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use super::DiagnosticCode;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;

pub(crate) struct UnavailableTypeLinter;

impl Linter for UnavailableTypeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnavailableType
    }
    fn description(&self) -> &'static str {
        "Type is not available through dependencies."
    }
    fn is_enabled(&self) -> bool {
        false
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context {
    type_label: String,
    defining_app: String,
    referencing_app: String,
    referencing_target: String,
}

impl GenericLinter for UnavailableTypeLinter {
    type Context = Context;

    fn matches(
        &self,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        // Early return if we don't have app data - can't determine type availability
        let referencing_app_data = sema.db.file_app_data(file_id)?;

        // Extract target once here - if not available, can't create diagnostics anyway
        let referencing_target = referencing_app_data.buck_target_name.as_ref()?;

        let mut res = Vec::new();
        let form_list = sema.form_list(file_id);

        // Check -spec attributes
        for (spec_id, _spec) in form_list.specs() {
            check_spec(
                &mut res,
                sema,
                file_id,
                &referencing_app_data,
                referencing_target,
                spec_id,
            );
        }

        // Check -type and -opaque attributes
        for (type_alias_id, _type_alias) in form_list.type_aliases() {
            check_type_alias(
                &mut res,
                sema,
                file_id,
                &referencing_app_data,
                referencing_target,
                type_alias_id,
            );
        }

        // Check -callback attributes
        for (callback_id, _callback) in form_list.callback_attributes() {
            check_callback(
                &mut res,
                sema,
                file_id,
                &referencing_app_data,
                referencing_target,
                callback_id,
            );
        }

        Some(res)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!(
            "The type '{}' is defined in application '{}', but the application is not a dependency of '{}' (defined in '{}').",
            context.type_label,
            context.defining_app,
            context.referencing_app,
            context.referencing_target
        ))
    }
}

pub static LINTER: UnavailableTypeLinter = UnavailableTypeLinter;

fn check_spec(
    matches: &mut Vec<GenericLinterMatchContext<Context>>,
    sema: &Semantic,
    file_id: FileId,
    referencing_app_data: &AppData,
    referencing_target: &String,
    spec_id: hir::SpecId,
) {
    let spec_id = InFile::new(file_id, spec_id);
    let spec_body = sema.db.spec_body(spec_id);

    Spec::fold(
        sema,
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        spec_id,
        (),
        &mut |_acc, ctx| {
            check_type_call(
                matches,
                sema,
                file_id,
                referencing_app_data,
                referencing_target,
                &ctx,
                &spec_body.body,
            );
        },
    );
}

fn check_type_alias(
    matches: &mut Vec<GenericLinterMatchContext<Context>>,
    sema: &Semantic,
    file_id: FileId,
    referencing_app_data: &AppData,
    referencing_target: &String,
    type_alias_id: hir::TypeAliasId,
) {
    let type_alias_id = InFile::new(file_id, type_alias_id);
    let type_body = sema.db.type_body(type_alias_id);

    TypeAlias::fold(
        sema,
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        type_alias_id,
        (),
        &mut |_acc, ctx| {
            check_type_call(
                matches,
                sema,
                file_id,
                referencing_app_data,
                referencing_target,
                &ctx,
                &type_body.body,
            );
        },
    );
}

fn check_callback(
    matches: &mut Vec<GenericLinterMatchContext<Context>>,
    sema: &Semantic,
    file_id: FileId,
    referencing_app_data: &AppData,
    referencing_target: &String,
    callback_id: hir::CallbackId,
) {
    let callback_id = InFile::new(file_id, callback_id);
    let callback_body = sema.db.callback_body(callback_id);

    Callback::fold(
        sema,
        Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        },
        callback_id,
        (),
        &mut |_acc, ctx| {
            check_type_call(
                matches,
                sema,
                file_id,
                referencing_app_data,
                referencing_target,
                &ctx,
                &callback_body.body,
            );
        },
    );
}

fn is_type_available(
    sema: &Semantic,
    referencing_app_data: &AppData,
    referencing_target: &String,
    referencing_app_name: &AppName,
    defining_app_name: &AppName,
) -> bool {
    // Types from the same app are always available
    if referencing_app_name == defining_app_name {
        return true;
    }

    // Check if type is available through dependencies
    if let Some(include_mapping) = &sema
        .db
        .project_data(referencing_app_data.project_id)
        .include_mapping
    {
        include_mapping.is_dep(referencing_target, defining_app_name)
    } else {
        // Can't determine - be conservative and allow it
        true
    }
}

fn check_type_call(
    matches: &mut Vec<GenericLinterMatchContext<Context>>,
    sema: &Semantic,
    file_id: FileId,
    referencing_app_data: &AppData,
    referencing_target: &String,
    ctx: &hir::fold::AnyCallBackCtx<'_>,
    body: &hir::Body,
) -> Option<()> {
    if let AnyExpr::TypeExpr(TypeExpr::Call { target, args }) = &ctx.item {
        let arity = args.len() as u32;
        let target_label = target.label(arity, sema, body)?;
        let target_range = target.range(sema, body)?;
        let type_alias_def = target.resolve_call(arity, sema, file_id, body)?;
        let defining_file_id = type_alias_def.file.file_id;
        let defining_app_data = sema.db.file_app_data(defining_file_id)?;
        let defining_app_name = &defining_app_data.name;
        let referencing_app_name = &referencing_app_data.name;

        if !is_type_available(
            sema,
            referencing_app_data,
            referencing_target,
            referencing_app_name,
            defining_app_name,
        ) {
            matches.push(GenericLinterMatchContext {
                range: target_range.range,
                context: Context {
                    type_label: target_label.to_string(),
                    defining_app: defining_app_name.to_string(),
                    referencing_app: referencing_app_name.to_string(),
                    referencing_target: referencing_target.to_string(),
                },
            });
        }
    }
    Some(())
}

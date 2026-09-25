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
use elp_ide_db::elp_base_db::DepKind;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::any_owning_app;
use elp_ide_db::elp_base_db::is_app_reachable;
use elp_project_model::AppName;
use fxhash::FxHashMap;
use hir::AnyExpr;
use hir::Callback;
use hir::InFile;
use hir::Record;
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
use crate::diagnostics::LinterContext;

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
        ctx: &LinterContext,
    ) -> Option<Vec<GenericLinterMatchContext<Self::Context>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        // Early return if we don't have app data - can't determine type availability
        let referencing_app_data = sema.db.file_app_data(file_id)?;

        // Extract target once here - if not available, can't create diagnostics anyway
        let referencing_target = referencing_app_data.buck_target_name.as_ref()?;

        let mut checker = Checker {
            matches: Vec::new(),
            sema,
            file_id,
            referencing_app_data: &referencing_app_data,
            referencing_target,
            reachable: FxHashMap::default(),
        };
        let form_list = sema.form_list(file_id);

        // Check -spec attributes
        for (spec_id, _spec) in form_list.specs() {
            checker.check_spec(spec_id);
        }

        // Check -type and -opaque attributes
        for (type_alias_id, _type_alias) in form_list.type_aliases() {
            checker.check_type_alias(type_alias_id);
        }

        // Check -callback attributes
        for (callback_id, _callback) in form_list.callback_attributes() {
            checker.check_callback(callback_id);
        }

        // Check -record attributes
        for (record_id, _record) in form_list.records() {
            checker.check_record(record_id);
        }

        Some(checker.matches)
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

struct Checker<'a, 'db> {
    matches: Vec<GenericLinterMatchContext<Context>>,
    sema: &'a Semantic<'db>,
    file_id: FileId,
    referencing_app_data: &'a AppData,
    referencing_target: &'a String,
    /// One reachability query per defining application rather than one per
    /// reference. The source target and the [`DepKind`] are fixed for the
    /// file, so every reference into the same application asks the same
    /// question, and answering it walks the whole dependency closure.
    reachable: FxHashMap<AppName, bool>,
}

impl Checker<'_, '_> {
    fn check_spec(&mut self, spec_id: hir::SpecId) {
        let sema = self.sema;
        let spec_id = InFile::new(self.file_id, spec_id);

        Spec::fold(
            sema,
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            spec_id,
            (),
            &mut |_acc, ctx| {
                self.check_type_call(&ctx);
            },
        );
    }

    fn check_type_alias(&mut self, type_alias_id: hir::TypeAliasId) {
        let sema = self.sema;
        let type_alias_id = InFile::new(self.file_id, type_alias_id);

        TypeAlias::fold(
            sema,
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            type_alias_id,
            (),
            &mut |_acc, ctx| {
                self.check_type_call(&ctx);
            },
        );
    }

    fn check_callback(&mut self, callback_id: hir::CallbackId) {
        let sema = self.sema;
        let callback_id = InFile::new(self.file_id, callback_id);

        Callback::fold(
            sema,
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            callback_id,
            (),
            &mut |_acc, ctx| {
                self.check_type_call(&ctx);
            },
        );
    }

    fn check_record(&mut self, record_id: hir::RecordId) {
        let sema = self.sema;
        let record_id = InFile::new(self.file_id, record_id);

        Record::fold(
            sema,
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            record_id,
            (),
            &mut |_acc, ctx| {
                self.check_type_call(&ctx);
            },
        );
    }

    fn check_type_call(&mut self, ctx: &hir::fold::AnyCallBackCtx<'_>) -> Option<()> {
        if let AnyExpr::TypeExpr(TypeExpr::Call { target, args }) = &ctx.item {
            let body = &ctx.body_origin.get_body(self.sema)?;
            let arity = args.len() as u32;
            let target_label = target.label(arity, body)?;
            let target_range = target.range(self.sema, body)?;
            let type_alias_def = target.resolve_call(arity, self.sema, self.file_id, body)?;
            let defining_file_id = type_alias_def.file.file_id;
            let defining_app_data = self.sema.db.file_app_data(defining_file_id)?;
            let defining_app_name = &defining_app_data.name;

            if !self.is_reachable(defining_file_id) {
                self.matches.push(GenericLinterMatchContext {
                    range: target_range,
                    context: Context {
                        type_label: target_label.to_string(),
                        defining_app: defining_app_name.to_string(),
                        referencing_app: self.referencing_app_data.name.to_string(),
                        referencing_target: self.referencing_target.to_string(),
                    },
                });
            }
        }
        Some(())
    }

    /// A file compiled into several targets belongs to several applications,
    /// and reaching any one of them is enough.
    fn is_reachable(&mut self, defining_file_id: FileId) -> bool {
        let sema = self.sema;
        any_owning_app(sema.db.upcast(), defining_file_id, |app| {
            self.is_app_reachable(app)
        })
        .unwrap_or(true)
    }

    fn is_app_reachable(&mut self, defining_app: &AppName) -> bool {
        if let Some(reachable) = self.reachable.get(defining_app) {
            return *reachable;
        }
        // A type reference is erased at compile time, so nothing has to be
        // within reach of this code: an application talked to over the
        // network is close enough.
        let reachable = is_app_reachable(
            self.sema.db.upcast(),
            self.referencing_app_data,
            defining_app,
            DepKind::Extra,
        );
        self.reachable.insert(defining_app.clone(), reachable);
        reachable
    }
}

#[cfg(test)]
mod tests {

    use crate::DiagnosticsConfig;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests::check_diagnostics_with_config;

    pub(crate) fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default().enable(DiagnosticCode::UnavailableType);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn type_from_declared_dep_is_ok() {
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_b
  -module(main).
  -spec main() -> app_b:t().
  main() -> ok.
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib
  -module(app_b).
  -type t() :: ok.
  -export_type([t/0]).
            "#,
        )
    }

    #[test]
    fn type_from_file_shared_with_a_declared_dep_is_ok() {
        // `shared.erl` is compiled into both `app_b` and `app_c`; `app_a`
        // declares only `app_b`, which is enough for the reference to resolve.
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_b
  -module(main).
  -spec main() -> shared:t().
  main() -> ok.
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib
  -module(app_b).
//- /app_c/src/shared.erl app:app_c buck_target:cell//app_c:lib also_app:app_b
  -module(shared).
  -type t() :: ok.
  -export_type([t/0]).
            "#,
        )
    }

    /// Mirror of `type_from_file_shared_with_a_declared_dep_is_ok` with the
    /// owners swapped. Which owner ends up nominal depends on hash order, so
    /// one of the pair always reaches the type through a non-nominal owner.
    #[test]
    fn type_from_file_shared_with_a_declared_dep_is_ok_mirrored() {
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_c
  -module(main).
  -spec main() -> shared:t().
  main() -> ok.
//- /app_c/src/app_c.erl app:app_c buck_target:cell//app_c:lib
  -module(app_c).
//- /app_b/src/shared.erl app:app_b buck_target:cell//app_b:lib also_app:app_c
  -module(shared).
  -type t() :: ok.
  -export_type([t/0]).
            "#,
        )
    }

    #[test]
    fn type_from_undeclared_dep_is_reported() {
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib
  -module(main).
  -spec main() -> app_b:t().
%%                ^^^^^^^ warning: W0059: The type 'app_b:t/0' is defined in application 'app_b', but the application is not a dependency of 'app_a' (defined in 'cell//app_a:lib').
%%                      | 💡 <suppression>
  main() -> ok.
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib
  -module(app_b).
  -type t() :: ok.
  -export_type([t/0]).
            "#,
        )
    }

    #[test]
    fn type_from_distributed_dep_is_ok() {
        // `app_b` runs on another node, so no build-time dependency can
        // exist, but the spec that mentions its type is erased anyway.
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib distributed_deps:app_b
  -module(main).
  -spec main() -> app_b:t().
  main() -> ok.
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib
  -module(app_b).
  -type t() :: ok.
  -export_type([t/0]).
            "#,
        )
    }

    #[test]
    fn type_from_distributed_dep_of_a_dep_is_ok() {
        // The `foo_api` / `foo` split: `app_a` depends on the API app, which
        // is the one declaring the cross-node relationship to `app_c`.
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib deps:app_b
  -module(main).
  -spec main() -> app_c:t().
  main() -> ok.
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib distributed_deps:app_c
  -module(app_b).
//- /app_c/src/app_c.erl app:app_c buck_target:cell//app_c:lib
  -module(app_c).
  -type t() :: ok.
  -export_type([t/0]).
            "#,
        )
    }

    #[test]
    fn cyclic_distributed_deps_terminate() {
        // Unlike `applications`, this relation is allowed to be cyclic.
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib distributed_deps:app_b
  -module(main).
  -spec main() -> app_b:t().
  main() -> ok.
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib distributed_deps:app_a
  -module(app_b).
  -type t() :: ok.
  -export_type([t/0]).
            "#,
        )
    }

    #[test]
    fn distributed_dep_on_unknown_app_is_ignored() {
        check_diagnostics(
            r#"
//- /app_a/src/main.erl app:app_a buck_target:cell//app_a:lib distributed_deps:not_a_project_app
  -module(main).
  -spec main() -> app_b:t().
%%                ^^^^^^^ warning: W0059: The type 'app_b:t/0' is defined in application 'app_b', but the application is not a dependency of 'app_a' (defined in 'cell//app_a:lib').
%%                      | 💡 <suppression>
  main() -> ok.
//- /app_b/src/app_b.erl app:app_b buck_target:cell//app_b:lib
  -module(app_b).
  -type t() :: ok.
  -export_type([t/0]).
            "#,
        )
    }

    #[test]
    fn non_buck_project_is_not_reported() {
        // Without buck metadata there is no dependency graph to check against.
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  -spec main() -> dependency:t().
  main() -> ok.
//- /src/dependency.erl
  -module(dependency).
  -type t() :: ok.
  -export_type([t/0]).
            "#,
        )
    }
}

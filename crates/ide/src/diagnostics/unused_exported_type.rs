/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: unused-exported-type
//
// Return a warning if an exported type in an .erl file is not referenced
// anywhere, neither in its own module nor in any other module of the project.

use std::borrow::Cow;

use elp_eqwalizer::db::EQWALIZER_TYPES;
use elp_ide_db::TypeCall;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::files_naming_module;
use elp_ide_db::type_calls_in_functions;
use elp_ide_db::type_calls_in_type_forms;
use elp_ide_db::type_calls_to_module;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use hir::Atom;
use hir::NameArity;
use hir::Semantic;

use crate::diagnostics::DiagnosticCode;
use crate::diagnostics::DiagnosticTag;
use crate::diagnostics::GenericLinter;
use crate::diagnostics::GenericLinterMatchContext;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;

pub(crate) struct UnusedExportedTypeLinter;

impl Linter for UnusedExportedTypeLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::UnusedExportedType
    }

    fn description(&self) -> &'static str {
        "Exported type is not used anywhere in the codebase."
    }

    fn should_process_headers(&self) -> bool {
        false
    }

    // Searches the whole project for references.
    fn runs_on_save_only(&self) -> bool {
        true
    }

    // eqWAlizer reads the `'module:name'()` types of `eqwalizer_types` as
    // overrides of the types of other modules, so they need no reference.
    fn should_process_file_id(&self, sema: &Semantic, file_id: FileId) -> bool {
        sema.module_name(file_id)
            .is_none_or(|module| module != *EQWALIZER_TYPES)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Context {
    name: String,
    arity: u32,
}

impl GenericLinter for UnusedExportedTypeLinter {
    type Context = Context;

    fn matches(&self, ctx: &LinterContext) -> Option<Vec<GenericLinterMatchContext<Context>>> {
        let sema = ctx.sema;
        let file_id = ctx.file_id;
        let def_map = sema.def_map_local(file_id);
        let types = def_map.get_types();
        let exported_types: Vec<&NameArity> = types
            .iter()
            .filter(|(_, type_def)| type_def.exported)
            .map(|(name_arity, _)| name_arity)
            .collect();
        if exported_types.is_empty() {
            return Some(Vec::new());
        }
        let module = Atom::new(&sema.form_list(file_id).module_attribute()?.name);

        let is_local = |(target_module, name_arity): &TypeCall| {
            target_module.is_none_or(|m| m == module) && types.contains_key(name_arity)
        };
        let all_used =
            |used: &FxHashSet<NameArity>| exported_types.iter().all(|ty| used.contains(*ty));

        // The local types referenced by each type definition, and the ones
        // referenced by specs, callbacks and records, which are used.
        let mut type_refs: FxHashMap<NameArity, Vec<NameArity>> = FxHashMap::default();
        let mut roots = Vec::new();
        for (user, calls) in type_calls_in_type_forms(sema, file_id) {
            let refs = calls
                .into_iter()
                .filter(is_local)
                .map(|(_, name_arity)| name_arity);
            match user {
                Some(user) => type_refs.entry(user).or_default().extend(refs),
                None => roots.extend(refs),
            }
        }
        let mut used = FxHashSet::default();
        mark_used(&mut used, &type_refs, roots);
        if all_used(&used) {
            return Some(Vec::new());
        }

        // Type casts in function bodies.
        let refs = type_calls_in_functions(sema, file_id)
            .into_iter()
            .filter(is_local)
            .map(|(_, name_arity)| name_arity);
        mark_used(&mut used, &type_refs, refs);
        if all_used(&used) {
            return Some(Vec::new());
        }

        // Read the other files that name a type still unused, stopping once
        // every exported type is used.
        for (other_file_id, names) in files_naming_module(sema, file_id) {
            let names_pending_type = exported_types.iter().any(|ty| {
                !used.contains(*ty) && names.contains(ty.name().to_quoted_string().as_ref())
            });
            if !names_pending_type {
                continue;
            }
            let refs = type_calls_to_module(sema, other_file_id, module);
            mark_used(&mut used, &type_refs, refs);
            if all_used(&used) {
                break;
            }
        }

        let res = exported_types
            .into_iter()
            .filter(|name_arity| !used.contains(name_arity))
            .filter_map(|name_arity| {
                let range = types.get(name_arity)?.name_range(sema.db)?;
                Some(GenericLinterMatchContext {
                    range: FileRange { file_id, range },
                    context: Context {
                        name: name_arity.name().to_string(),
                        arity: name_arity.arity(),
                    },
                })
            })
            .collect();
        Some(res)
    }

    fn match_description(&self, context: &Self::Context) -> Cow<'_, str> {
        Cow::Owned(format!(
            "Exported type {}/{} is not used anywhere in the codebase.",
            context.name, context.arity
        ))
    }

    fn tag(&self, _context: &Self::Context) -> Option<DiagnosticTag> {
        Some(DiagnosticTag::Unused)
    }
}

/// Mark `roots` used, and the types their definitions reference.
fn mark_used(
    used: &mut FxHashSet<NameArity>,
    type_refs: &FxHashMap<NameArity, Vec<NameArity>>,
    roots: impl IntoIterator<Item = NameArity>,
) {
    let mut stack: Vec<NameArity> = roots.into_iter().collect();
    while let Some(name_arity) = stack.pop() {
        if let Some(refs) = type_refs.get(&name_arity)
            && !used.contains(&name_arity)
        {
            stack.extend(refs.iter().cloned());
        }
        used.insert(name_arity);
    }
}

pub static LINTER: UnusedExportedTypeLinter = UnusedExportedTypeLinter;

#[cfg(test)]
mod tests {

    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::tests::check_diagnostics_with_config;

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        let config = DiagnosticsConfig::default()
            .enable(DiagnosticCode::UnusedExportedType)
            .disable(DiagnosticCode::UnusedRecordField)
            .disable(DiagnosticCode::UnnecessaryFunctionArg)
            .disable(DiagnosticCode::UncheckedCast)
            .disable(DiagnosticCode::AvoidTypeDefsInHeader)
            .disable(DiagnosticCode::UnusedInclude);
        check_diagnostics_with_config(config, fixture)
    }

    #[test]
    fn unused_exported_type() {
        check_diagnostics(
            r#"
-module(main).
-export_type([foo/0]).

-type foo() :: ok.
%%    ^^^ warning: W0092: Exported type foo/0 is not used anywhere in the codebase.
%%      | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn unused_exported_opaque_and_nominal() {
        check_diagnostics(
            r#"
-module(main).
-export_type([foo/1, bar/0]).

-opaque foo(A) :: {A}.
%%      ^^^ warning: W0092: Exported type foo/1 is not used anywhere in the codebase.
%%        | 💡 <suppression>
-nominal bar() :: ok.
%%       ^^^ warning: W0092: Exported type bar/0 is not used anywhere in the codebase.
%%         | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn used_in_other_module_spec() {
        check_diagnostics(
            r#"
//- /src/mod_a.erl
-module(mod_a).
-export_type([foo/0]).
-type foo() :: ok.

//- /src/mod_b.erl
-module(mod_b).
-export([f/0]).
-spec f() -> mod_a:foo().
f() -> ok.
            "#,
        );
    }

    #[test]
    fn used_in_other_module_type_record_callback() {
        check_diagnostics(
            r#"
//- /src/mod_a.erl
-module(mod_a).
-export_type([t1/0, t2/0, t3/0]).
-type t1() :: ok.
-type t2() :: ok.
-type t3() :: ok.

//- /src/mod_b.erl
-module(mod_b).
-export_type([u/0]).
-type u() :: [mod_a:t1()].
-record(r, {f :: mod_a:t2()}).
-callback cb(mod_a:t3()) -> #r{}.
-spec g(u()) -> ok.
g(_) -> ok.
            "#,
        );
    }

    #[test]
    fn used_in_same_module() {
        check_diagnostics(
            r#"
-module(main).
-export([f/0]).
-export_type([foo/0, bar/0]).

-type foo() :: ok.
-type bar() :: foo().
-spec f() -> bar().
f() -> ok.
            "#,
        );
    }

    #[test]
    fn used_qualified_in_same_module() {
        check_diagnostics(
            r#"
-module(main).
-export([f/0]).
-export_type([foo/0]).

-type foo() :: ok.
-spec f() -> main:foo().
f() -> ok.
            "#,
        );
    }

    #[test]
    fn used_through_header_macro_in_other_module() {
        check_diagnostics(
            r#"
//- /src/mod_a.erl
-module(mod_a).
-export_type([foo/0]).
-type foo() :: ok.

//- /src/mod_b.hrl
-define(FOO, mod_a:foo()).

//- /src/mod_b.erl
-module(mod_b).
-include("mod_b.hrl").
-export([f/0]).
-spec f() -> ?FOO.
f() -> ok.
            "#,
        );
    }

    #[test]
    fn used_through_macro_in_other_module() {
        check_diagnostics(
            r#"
//- /src/mod_a.erl
-module(mod_a).
-export_type([foo/0]).
-type foo() :: ok.

//- /src/mod_b.erl
-module(mod_b).
-export([f/0]).
-define(FOO, mod_a:foo()).
-spec f() -> ?FOO.
f() -> ok.
            "#,
        );
    }

    #[test]
    fn used_in_type_casts() {
        check_diagnostics(
            r#"
//- /src/mod_a.erl
-module(mod_a).
-export([f/1]).
-export_type([foo/0, bar/0]).
-define(CHECKED_CAST(Expr, Type), (Expr :: Type)).
-type foo() :: ok.
-type bar() :: ok.
-spec f(term()) -> term().
f(X) -> ?CHECKED_CAST(X, foo()).

//- /src/mod_b.erl
-module(mod_b).
-export([g/1]).
-define(UNCHECKED_CAST(Expr, Type), (Expr :> Type)).
-spec g(term()) -> term().
g(X) -> ?UNCHECKED_CAST(X, mod_a:bar()).
            "#,
        );
    }

    #[test]
    fn arity_matters() {
        check_diagnostics(
            r#"
//- /src/mod_a.erl
-module(mod_a).
-export_type([foo/0, foo/1]).
-type foo() :: ok.
-type foo(A) :: {A}.
%%    ^^^ warning: W0092: Exported type foo/1 is not used anywhere in the codebase.
%%      | 💡 <suppression>

//- /src/mod_b.erl
-module(mod_b).
-export([f/0]).
-spec f() -> mod_a:foo().
f() -> ok.
            "#,
        );
    }

    #[test]
    fn header_type_no_warning() {
        check_diagnostics(
            r#"
//- /include/header.hrl include_path:/include
-type foo() :: ok.

//- /src/main.erl
-module(main).
-include("header.hrl").
-export_type([foo/0]).
            "#,
        );
    }

    #[test]
    fn eqwalizer_types_overrides_no_warning() {
        check_diagnostics(
            r#"
//- /src/eqwalizer_types.erl
-module(eqwalizer_types).
-export_type(['other:t'/0]).
-type 'other:t'() :: ok.
            "#,
        );
    }

    #[test]
    fn recursive_type() {
        check_diagnostics(
            r#"
-module(main).
-export_type([tree/0]).
-type tree() :: leaf | {node, tree(), tree()}.
%%    ^^^^ warning: W0092: Exported type tree/0 is not used anywhere in the codebase.
%%       | 💡 <suppression>
            "#,
        );
    }

    #[test]
    fn used_only_by_unused_types() {
        check_diagnostics(
            r#"
-module(main).
-export([f/0]).
-export_type([input/0, kind/0, used/0, used_kind/0]).
-type input() :: #{kind := kind(), helper := helper()}.
%%    ^^^^^ warning: W0092: Exported type input/0 is not used anywhere in the codebase.
%%        | 💡 <suppression>
-type kind() :: a | b.
%%    ^^^^ warning: W0092: Exported type kind/0 is not used anywhere in the codebase.
%%       | 💡 <suppression>
-type helper() :: kind().
-type used() :: used_kind().
-type used_kind() :: c.
-spec f() -> used().
f() -> c.
            "#,
        );
    }

    #[test]
    fn used_by_unused_unexported_type_and_spec() {
        check_diagnostics(
            r#"
-module(main).
-export([f/0]).
-export_type([kind/0]).
-type kind() :: a | b.
-type local() :: kind().
-spec f() -> kind().
f() -> a.
            "#,
        );
    }

    #[test]
    fn unexported_type_no_warning() {
        check_diagnostics(
            r#"
-module(main).
-type foo() :: ok.
            "#,
        );
    }
}

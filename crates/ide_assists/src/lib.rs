/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! `assists` crate provides a bunch of code assists, also known as code
//! actions (in LSP) or intentions (in IntelliJ).
//!
//! An assist is a micro-refactoring, which is automatically activated in
//! certain context. For example, if the cursor is over `,`, a "swap `,`" assist
//! becomes available.

#[allow(unused)]
macro_rules! eprintln {
    ($($tt:tt)*) => { stdx::eprintln!($($tt)*) };
}

mod assist_config;
mod assist_context;
pub mod helpers;
#[cfg(test)]
mod tests;

// use hir::Semantics;
pub use assist_config::AssistConfig;
use elp_ide_db::RootDatabase;
pub use elp_ide_db::assists::Assist;
use elp_ide_db::assists::AssistContextDiagnostic;
pub use elp_ide_db::assists::AssistId;
pub use elp_ide_db::assists::AssistKind;
pub use elp_ide_db::assists::AssistResolveStrategy;
use elp_ide_db::assists::AssistUserInput;
pub use elp_ide_db::assists::GroupLabel;
pub use elp_ide_db::assists::SingleResolve;
use elp_ide_db::elp_base_db::FileRange;

// use elp_syntax::TextRange;
pub(crate) use crate::assist_context::AssistContext;
pub(crate) use crate::assist_context::Assists;

/// Return all the assists applicable at the given position.
pub fn assists(
    db: &RootDatabase,
    config: &AssistConfig,
    resolve: AssistResolveStrategy,
    range: FileRange,
    context_diagnostics: &[AssistContextDiagnostic],
    user_input: Option<AssistUserInput>,
) -> Vec<Assist> {
    let ctx = AssistContext::new(db, config, range, context_diagnostics, user_input);
    let mut acc = Assists::new(&ctx, resolve);
    handlers::all().iter().for_each(|handler| {
        handler(&mut acc, &ctx);
    });
    acc.finish()
}

mod handlers {
    use crate::AssistContext;
    use crate::Assists;

    pub(crate) type Handler = fn(&mut Assists, &AssistContext) -> Option<()>;

    mod add_edoc;
    mod add_fixme;
    mod add_format;
    mod add_impl;
    mod add_spec;
    mod bump_variables;
    mod create_function;
    mod delete_function;
    mod export_function;
    mod export_type;
    mod extract_function;
    mod extract_variable;
    mod flip_sep;
    mod ignore_variable;
    mod implement_behaviour;
    mod inline_function;
    mod inline_local_variable;

    pub(crate) fn all() -> &'static [Handler] {
        &[
            // These are alphabetic for the foolish consistency
            add_edoc::add_edoc,
            add_fixme::add_fixme,
            add_format::add_format,
            add_impl::add_impl,
            add_spec::add_spec,
            bump_variables::bump_variables,
            create_function::create_function,
            delete_function::delete_function,
            export_function::export_function,
            export_type::export_type,
            extract_function::extract_function,
            extract_variable::extract_variable,
            flip_sep::flip_sep,
            ignore_variable::ignore_variable,
            implement_behaviour::implement_behaviour,
            inline_function::inline_function,
            inline_local_variable::inline_local_variable,
            // These are manually sorted for better priorities. By default,
            // priority is determined by the size of the target range (smaller
            // target wins). If the ranges are equal, position in this list is
            // used as a tie-breaker.
            // add_missing_impl_members::add_missing_impl_members,
            // add_missing_impl_members::add_missing_default_members,

            // Are you sure you want to add new assist here, and not to the
            // sorted list above?
        ]
    }
}

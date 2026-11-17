/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// Diagnostic: deprecated-function
//
// Return a warning if a function is marked as deprecated.
// This functionality is similar to the one provided by the XRef tool which comes with OTP.
// In fact, it leverages the same `-deprecated` attribute that XRef uses. The attribute
// allows the user to specify a "third" field to explain the reason of a deprecation, but
// XRef itself ignores that field, which is intended to be used by other tools.
// This diagnostic does just that and shows the message at the call-site.
//
// Like the Erlang compiler, it honours the `nowarn_deprecated_function`,
// `{nowarn_deprecated_function, MFAs}` and `warn_deprecated_function` options
// given via `-compile` attributes in the calling module.

use std::borrow::Cow;

use elp_ide_assists::Assist;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChange;
use elp_ide_db::text_edit::TextEdit;
use elp_ide_db::text_edit::TextSize;
use elp_syntax::AstNode;
use hir::Body;
use hir::InFile;
use hir::Literal;
use hir::Name;
use hir::NameArity;
use hir::PPConditionResult;
use hir::Semantic;
use hir::Term;
use hir::TermId;
use hir::known;

use super::DiagnosticCode;
use super::DiagnosticTag;
use crate::FunctionMatch;
use crate::codemod_helpers::CheckCallCtx;
use crate::codemod_helpers::MatchCtx;
use crate::diagnostics::FunctionCallLinter;
use crate::diagnostics::Linter;
use crate::diagnostics::LinterContext;
use crate::fix;
use crate::lazy_function_matches;

pub(crate) struct DeprecatedFunctionLinter;

#[derive(Clone, Debug, PartialEq, Default)]
pub(crate) struct DeprecatedContext {
    function_label: String,
    deprecated_desc: Option<String>,
    target_module: String,
    target_name: String,
    target_arity: u32,
}

impl Linter for DeprecatedFunctionLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::DeprecatedFunction
    }

    fn description(&self) -> &'static str {
        "Function is deprecated."
    }

    fn should_process_generated_files(&self) -> bool {
        true
    }
}

impl FunctionCallLinter for DeprecatedFunctionLinter {
    type Context = DeprecatedContext;

    fn matches_functions(&self) -> Vec<FunctionMatch> {
        lazy_function_matches![vec![FunctionMatch::any()]]
    }

    fn diagnostic_tag(&self, _ctx: &Self::Context) -> Option<DiagnosticTag> {
        Some(DiagnosticTag::Deprecated)
    }

    fn match_description(&self, ctx: &Self::Context) -> Cow<'_, str> {
        let base_message = format!("Function '{}' is deprecated.", ctx.function_label);
        match &ctx.deprecated_desc {
            Some(desc) => Cow::Owned(format!("{base_message}\n{desc}")),
            None => Cow::Owned(base_message),
        }
    }

    fn check_match(&self, ctx: &CheckCallCtx<'_, ()>) -> Option<Self::Context> {
        let arity = ctx.args.arity();
        let target_def = ctx.target.resolve_call(
            arity,
            ctx.sema,
            ctx.in_clause.file_id(),
            &ctx.in_clause.body(),
        )?;

        if !target_def.deprecated {
            return None;
        }

        let function_label = target_def.name.to_string();
        let deprecated_desc = target_def.deprecated_desc.as_ref().map(|desc| {
            let desc = desc.to_string();
            strip_quotes(&desc).to_string()
        });
        let target_module = ctx
            .sema
            .module_name(target_def.file.file_id)
            .expect("module should have a name")
            .as_str()
            .to_string();
        if DeprecatedWarningOptions::new(ctx.sema, ctx.in_clause.file_id())
            .is_suppressed(&target_module, &target_def.name)
        {
            return None;
        }
        let target_name = target_def.name.name().to_string();
        let target_arity = target_def.name.arity();

        Some(DeprecatedContext {
            function_label,
            deprecated_desc,
            target_module,
            target_name,
            target_arity,
        })
    }

    fn fixes(
        &self,
        match_ctx: &MatchCtx<Self::Context>,
        ctx: &LinterContext,
    ) -> Option<Vec<Assist>> {
        let source = match_ctx.sema.parse(ctx.file_id).value;
        let form_list = match_ctx.sema.form_list(ctx.file_id);

        let offset = if let Some(module_attr) = form_list.module_attribute() {
            let module_attr_range = module_attr.form_id.get(&source).syntax().text_range();
            module_attr_range.end() + TextSize::from(1)
        } else {
            TextSize::from(0)
        };

        let text = format!(
            "-ignore_xref([{{{}, {}, {}}}]).\n",
            match_ctx.extra.target_module,
            match_ctx.extra.target_name,
            match_ctx.extra.target_arity,
        );
        let mut edit_builder = TextEdit::builder();
        edit_builder.insert(offset, text);
        let edit = edit_builder.finish();
        let source_change = SourceChange::from_text_edit(ctx.file_id, edit);

        Some(vec![fix(
            "xref_ignore",
            "Add xref ignore for all calls to this function",
            source_change,
            match_ctx.range.range,
        )])
    }
}

pub static LINTER: DeprecatedFunctionLinter = DeprecatedFunctionLinter;

/// The deprecated-function warning options of a module's `-compile`
/// attributes, with the semantics of `erl_lint`: the last of
/// `nowarn_deprecated_function` and `warn_deprecated_function` decides whether
/// the warning is enabled, while `{nowarn_deprecated_function, MFAs}`
/// suppresses it for the listed functions either way.
#[derive(Debug, Default)]
struct DeprecatedWarningOptions {
    disabled: bool,
    /// Bare module atoms in `MFAs`, which `erl_lint` accepts and applies to
    /// every function of the module.
    modules: Vec<Name>,
    functions: Vec<(Name, NameArity)>,
}

impl DeprecatedWarningOptions {
    fn new(sema: &Semantic, file_id: FileId) -> Self {
        let form_list = sema.form_list(file_id);
        let mut options = Self::default();
        form_list
            .compile_attributes()
            .filter(|(_, attribute)| {
                form_list.is_form_active(sema.db, file_id, &attribute.pp_ctx, None)
                    != PPConditionResult::Inactive
            })
            .for_each(|(id, _)| {
                let attribute = sema.db.compile_body(InFile::new(file_id, id));
                flatten(&attribute.body, attribute.value)
                    .into_iter()
                    .for_each(|option| options.add_option(&attribute.body, option));
            });
        options
    }

    fn add_option(&mut self, body: &Body, option: TermId) {
        match expand(body, option) {
            Term::Literal(Literal::Atom(atom)) => {
                let name = atom.as_name();
                if name == *known::nowarn_deprecated_function {
                    self.disabled = true;
                } else if name == *known::warn_deprecated_function {
                    self.disabled = false;
                }
            }
            Term::Tuple { exprs } => {
                if let [tag, mfas] = exprs.as_slice()
                    && atom_name(body, *tag) == Some(*known::nowarn_deprecated_function)
                {
                    flatten(body, *mfas)
                        .into_iter()
                        .for_each(|mfa| self.add_mfa(body, mfa));
                }
            }
            _ => {}
        }
    }

    fn add_mfa(&mut self, body: &Body, mfa: TermId) {
        match expand(body, mfa) {
            Term::Literal(Literal::Atom(module)) => self.modules.push(module.as_name()),
            Term::Tuple { exprs } => {
                if let [module, name, arity] = exprs.as_slice()
                    && let Some(module) = atom_name(body, *module)
                    && let Some(name) = atom_name(body, *name)
                    && let Term::Literal(Literal::Integer(arity)) = expand(body, *arity)
                    && let Ok(arity) = arity.value.try_into()
                {
                    self.functions.push((module, NameArity::new(name, arity)));
                }
            }
            _ => {}
        }
    }

    fn is_suppressed(&self, module: &str, function: &NameArity) -> bool {
        self.disabled
            || self.modules.iter().any(|m| m == module)
            || self
                .functions
                .iter()
                .any(|(m, fa)| m == module && fa == function)
    }
}

/// The elements of a possibly deep list, as `lists:flatten/1` returns them.
/// Any other term is a single element.
fn flatten(body: &Body, term: TermId) -> Vec<TermId> {
    match &body[term] {
        Term::List { exprs, tail } => exprs
            .iter()
            .chain(tail)
            .flat_map(|e| flatten(body, *e))
            .collect(),
        Term::MacroCall { expansion, .. } => flatten(body, *expansion),
        _ => vec![term],
    }
}

fn expand(body: &Body, term: TermId) -> &Term {
    match &body[term] {
        Term::MacroCall { expansion, .. } => expand(body, *expansion),
        term => term,
    }
}

fn atom_name(body: &Body, term: TermId) -> Option<Name> {
    match expand(body, term) {
        Term::Literal(Literal::Atom(atom)) => Some(atom.as_name()),
        _ => None,
    }
}

fn strip_quotes(s: &str) -> &str {
    let quote = "\"";
    let s = s.strip_prefix(quote).unwrap_or(s);
    s.strip_suffix(quote).unwrap_or(s)
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::tests::check_diagnostics;
    use crate::tests::check_fix;

    #[test]
    fn test_deprecated_function_local() {
        check_diagnostics(
            r#"
  -module(main).
  -deprecated({not_ok_to_use, 0}).
  not_ok_to_use() ->
    ok.
  main() ->
    not_ok_to_use().
%%  ^^^^^^^^^^^^^ warning: W0016: Function 'not_ok_to_use/0' is deprecated.
%%              | 💡 Add xref ignore for all calls to this function
%%              | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_deprecated_function_fun_arity() {
        check_diagnostics(
            r#"
  -module(main).
  -deprecated([not_ok_to_use/0]).
  -deprecated(also_not_ok/1).
  not_ok_to_use() ->
    ok.
  also_not_ok(_) ->
    ok.
  main() ->
    not_ok_to_use(),
%%  ^^^^^^^^^^^^^ warning: W0016: Function 'not_ok_to_use/0' is deprecated.
%%              | 💡 Add xref ignore for all calls to this function
%%              | 💡 <suppression>
    also_not_ok(1).
%%  ^^^^^^^^^^^ warning: W0016: Function 'also_not_ok/1' is deprecated.
%%            | 💡 Add xref ignore for all calls to this function
%%            | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_deprecated_function_remote() {
        check_diagnostics(
            r#"
//- /src/b.erl
  -module(b).
  -export([not_ok_to_use/0]).
  -deprecated({not_ok_to_use, 0}).
  not_ok_to_use() ->
    ok.
//- /src/a.erl
  -module(a).

  main() ->
    b:not_ok_to_use().
%%  ^^^^^^^^^^^^^^^ warning: W0016: Function 'not_ok_to_use/0' is deprecated.
%%                | 💡 Add xref ignore for all calls to this function
%%                | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_deprecated_function_in_macro() {
        check_diagnostics(
            r#"
  -module(main).
  -define(LAZY, fun(X) -> X end).
  -deprecated({do, 0}).
  main() ->
    do(),
%%  ^^ warning: W0016: Function 'do/0' is deprecated.
%%   | 💡 Add xref ignore for all calls to this function
%%   | 💡 <suppression>
    ?LAZY(do()).
%%  ^^^^^^^^^^^ warning: W0016: Function 'do/0' is deprecated.
%%            | 💡 Add xref ignore for all calls to this function
%%            | 💡 <suppression>
  do() ->
    ok.
            "#,
        )
    }

    #[test]
    fn test_deprecated_function_description() {
        check_diagnostics(
            r#"
//- /src/b.erl
  -module(b).
  -export([not_ok_to_use/0]).
  -deprecated({not_ok_to_use, 0, "Cause I said so."}).
  not_ok_to_use() ->
    ok.
//- /src/a.erl
  -module(a).

  main() ->
    b:not_ok_to_use().
%%  ^^^^^^^^^^^^^^^ warning: W0016: Function 'not_ok_to_use/0' is deprecated.
%%                | Cause I said so.
%%                | 💡 Add xref ignore for all calls to this function
%%                | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_deprecated_function_no_auto_import() {
        check_diagnostics(
            r#"
//- /src/main.erl
  -module(main).
  -compile({no_auto_import, [now/0]}).
  -export([now/0, later/0]).
  now() ->
    really_now.
  later() ->
    {now(), but_later}.
            "#,
        )
    }

    #[test]
    fn test_nowarn_deprecated_function_all() {
        check_diagnostics(
            r#"
//- /src/b.erl
  -module(b).
  -export([f/0]).
  -deprecated([f/0]).
  f() -> ok.
//- /src/a.erl
  -module(a).
  -compile(nowarn_deprecated_function).
%%         ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
%%                                  | 💡 <suppression>
  -deprecated([local/0]).
  local() -> ok.
  main() ->
    local(),
    b:f().
            "#,
        )
    }

    #[test]
    fn test_nowarn_deprecated_function_mfa() {
        check_diagnostics(
            r#"
//- /src/b.erl
  -module(b).
  -export([f/0, f/1, g/0]).
  -deprecated([f/0, f/1, g/0]).
  f() -> ok.
  f(_) -> ok.
  g() -> ok.
//- /src/a.erl
  -module(a).
  -compile({nowarn_deprecated_function, {b, f, 0}}).
%%          ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
%%                                   | 💡 <suppression>
  main() ->
    b:f(),
    b:f(1),
%%  ^^^ warning: W0016: Function 'f/1' is deprecated.
%%    | 💡 Add xref ignore for all calls to this function
%%    | 💡 <suppression>
    b:g().
%%  ^^^ warning: W0016: Function 'g/0' is deprecated.
%%    | 💡 Add xref ignore for all calls to this function
%%    | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_nowarn_deprecated_function_mfa_list() {
        check_diagnostics(
            r#"
//- /src/b.erl
  -module(b).
  -export([f/0, g/0, h/0]).
  -deprecated([f/0, g/0, h/0]).
  f() -> ok.
  g() -> ok.
  h() -> ok.
//- /src/a.erl
  -module(a).
  -compile({nowarn_deprecated_function, [{b, f, 0}, {b, g, 0}, {c, h, 0}]}).
%%          ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
%%                                   | 💡 <suppression>
  main() ->
    b:f(),
    b:g(),
    b:h().
%%  ^^^ warning: W0016: Function 'h/0' is deprecated.
%%    | 💡 Add xref ignore for all calls to this function
%%    | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_nowarn_deprecated_function_in_option_list() {
        check_diagnostics(
            r#"
//- /src/b.erl
  -module(b).
  -export([f/0, g/0, h/0]).
  -deprecated([f/0, g/0, h/0]).
  f() -> ok.
  g() -> ok.
  h() -> ok.
//- /src/a.erl
  -module(a).
  -compile([debug_info, {nowarn_deprecated_function, [{b, f, 0}]}]).
%%                       ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
%%                                                | 💡 <suppression>
  -compile({nowarn_deprecated_function, {b, g, 0}}).
%%          ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
%%                                   | 💡 <suppression>
  main() ->
    b:f(),
    b:g(),
    b:h().
%%  ^^^ warning: W0016: Function 'h/0' is deprecated.
%%    | 💡 Add xref ignore for all calls to this function
%%    | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_nowarn_deprecated_function_list_tail() {
        check_diagnostics(
            r#"
//- /src/b.erl
  -module(b).
  -export([f/0, g/0, h/0, i/0]).
  -deprecated([f/0, g/0, h/0, i/0]).
  f() -> ok.
  g() -> ok.
  h() -> ok.
  i() -> ok.
//- /src/a.erl
  -module(a).
  -define(EXTRA, [{nowarn_deprecated_function, {b, f, 0}}]).
  -compile([debug_info | ?EXTRA]).
  -compile({nowarn_deprecated_function, [{b, g, 0} | [{b, h, 0}]]}).
%%          ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
%%                                   | 💡 <suppression>
  main() ->
    b:f(),
    b:g(),
    b:h(),
    b:i().
%%  ^^^ warning: W0016: Function 'i/0' is deprecated.
%%    | 💡 Add xref ignore for all calls to this function
%%    | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_nowarn_deprecated_function_macro() {
        check_diagnostics(
            r#"
  -module(main).
  -define(DEPRECATED, {?MODULE, local, 0}).
  -compile({nowarn_deprecated_function, [?DEPRECATED]}).
  %%        ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
  %%                                 | 💡 <suppression>
  -deprecated([local/0, other/0]).
  local() -> ok.
  other() -> ok.
  main() ->
    local(),
    other().
  %%^^^^^ warning: W0016: Function 'other/0' is deprecated.
  %%    | 💡 Add xref ignore for all calls to this function
  %%    | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_nowarn_deprecated_function_module() {
        check_diagnostics(
            r#"
//- /src/b.erl
  -module(b).
  -export([f/0]).
  -deprecated([f/0]).
  f() -> ok.
//- /src/c.erl
  -module(c).
  -export([f/0]).
  -deprecated([f/0]).
  f() -> ok.
//- /src/a.erl
  -module(a).
  -compile({nowarn_deprecated_function, [b]}).
%%          ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
%%                                   | 💡 <suppression>
  main() ->
    b:f(),
    c:f().
%%  ^^^ warning: W0016: Function 'f/0' is deprecated.
%%    | 💡 Add xref ignore for all calls to this function
%%    | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_warn_deprecated_function_last_wins() {
        check_diagnostics(
            r#"
//- /src/b.erl
  -module(b).
  -export([f/0, g/0]).
  -deprecated([f/0, g/0]).
  f() -> ok.
  g() -> ok.
//- /src/a.erl
  -module(a).
  -compile([nowarn_deprecated_function, {nowarn_deprecated_function, {b, f, 0}}]).
%%          ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
%%                                   | 💡 <suppression>
%%                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
%%                                                                | 💡 <suppression>
  -compile(warn_deprecated_function).
  main() ->
    b:f(),
    b:g().
%%  ^^^ warning: W0016: Function 'g/0' is deprecated.
%%    | 💡 Add xref ignore for all calls to this function
%%    | 💡 <suppression>
//- /src/c.erl
  -module(c).
  -compile(warn_deprecated_function).
  -compile(nowarn_deprecated_function).
%%         ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
%%                                  | 💡 <suppression>
  main() ->
    b:f(),
    b:g().
            "#,
        )
    }

    #[test]
    fn test_nowarn_deprecated_function_inactive() {
        check_diagnostics(
            r#"
  -module(main).
  -ifdef(UNDEFINED_MACRO).
  -compile(nowarn_deprecated_function).
  %%       ^^^^^^^^^^^^^^^^^^^^^^^^^^ warning: W0054: Do not suppress compiler warnings at module level.
  %%                                | 💡 <suppression>
  -endif.
  -deprecated([local/0]).
  local() -> ok.
  main() ->
    local().
  %%^^^^^ warning: W0016: Function 'local/0' is deprecated.
  %%    | 💡 Add xref ignore for all calls to this function
  %%    | 💡 <suppression>
            "#,
        )
    }

    #[test]
    fn test_xref_ignore_fix() {
        check_fix(
            r#"
//- /src/b.erl
-module(b).
-export([not_ok_to_use/0]).
-deprecated({not_ok_to_use, 0, "Cause I said so."}).
not_ok_to_use() ->
  ok.

//- /src/a.erl
-module(a).

main() ->
  b:no~t_ok_to_use().
"#,
            expect![[r#"
-module(a).
-ignore_xref([{b, not_ok_to_use, 0}]).

main() ->
  b:not_ok_to_use().
"#]],
        )
    }
}

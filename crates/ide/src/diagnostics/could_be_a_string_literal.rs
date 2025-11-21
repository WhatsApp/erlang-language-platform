/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::DiagnosticCode;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::source_change::SourceChangeBuilder;
use hir::Semantic;
use hir::Strategy;
use hir::StringVariant;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::quote::escape_and_quote_atom;
use hir::quote::escape_and_quote_binary_string;
use hir::quote::escape_and_quote_string;

use crate::Assist;
use crate::diagnostics::Linter;
use crate::diagnostics::Severity;
use crate::diagnostics::SsrPatternsLinter;
use crate::fix;

pub(crate) static LINTER: CouldBeAStringLiteralLinter = CouldBeAStringLiteralLinter;

pub(crate) struct CouldBeAStringLiteralLinter;

impl Linter for CouldBeAStringLiteralLinter {
    fn id(&self) -> DiagnosticCode {
        DiagnosticCode::CouldBeAStringLiteral
    }

    fn description(&self) -> &'static str {
        "Could be rewritten as a literal."
    }

    fn severity(&self, _sema: &Semantic, _file_id: FileId) -> Severity {
        Severity::Information
    }
}

const STRING_VAR: &str = "_@StringLike";

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) enum StringKind {
    List,
    Binary,
    Atom,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) struct StringRewrite {
    from: StringKind,
    to: StringKind,
}

impl SsrPatternsLinter for CouldBeAStringLiteralLinter {
    type Context = StringRewrite;

    fn patterns(&self) -> Vec<(String, Self::Context)> {
        vec![
            (
                format!("ssr: list_to_binary({STRING_VAR})."),
                StringRewrite {
                    from: StringKind::List,
                    to: StringKind::Binary,
                },
            ),
            (
                format!("ssr: list_to_atom({STRING_VAR})."),
                StringRewrite {
                    from: StringKind::List,
                    to: StringKind::Atom,
                },
            ),
            (
                format!("ssr: atom_to_list({STRING_VAR})."),
                StringRewrite {
                    from: StringKind::Atom,
                    to: StringKind::List,
                },
            ),
            (
                format!("ssr: atom_to_binary({STRING_VAR})."),
                StringRewrite {
                    from: StringKind::Atom,
                    to: StringKind::Binary,
                },
            ),
        ]
    }

    fn pattern_description(&self, context: &Self::Context) -> &'static str {
        match context.to {
            StringKind::List => "Could be rewritten as a string literal.",
            StringKind::Binary => "Could be rewritten as a binary string literal.",
            StringKind::Atom => "Could be rewritten as an atom literal.",
        }
    }

    fn is_match_valid(
        &self,
        context: &Self::Context,
        matched: &elp_ide_ssr::Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<bool> {
        if matched.range.file_id != file_id {
            // We've somehow ended up with a match in a different file - this means we've
            // accidentally expanded a macro from a different file, or some other complex case that
            // gets hairy, so bail out.
            return None;
        }
        if let Some(comments) = matched.comments(sema) {
            // Avoid clobbering comments in the original source code
            if !comments.is_empty() {
                return None;
            }
        }

        if let Some(true) = matched.placeholder_is_macro(sema, STRING_VAR) {
            None
        } else {
            match context.from {
                StringKind::List => Some(Option::is_some(
                    &matched.placeholder_is_string(sema, STRING_VAR),
                )),
                StringKind::Binary => None, // Possible future work
                StringKind::Atom => Some(Option::is_some(
                    &matched.placeholder_is_atom(sema, STRING_VAR),
                )),
            }
        }
    }

    fn fixes(
        &self,
        context: &Self::Context,
        matched: &elp_ide_ssr::Match,
        sema: &Semantic,
        file_id: FileId,
    ) -> Option<Vec<Assist>> {
        let unnecessary_non_literal_range = matched.range.range;
        let mut builder = SourceChangeBuilder::new(file_id);
        match *context {
            StringRewrite {
                from: StringKind::List,
                to: StringKind::Binary,
            } => {
                if let StringVariant::Normal(list_string_value) =
                    matched.placeholder_is_string(sema, STRING_VAR)?
                {
                    builder.replace(
                        unnecessary_non_literal_range,
                        escape_and_quote_binary_string(&list_string_value),
                    );
                    Some(vec![fix(
                        "rewrite_as_a_binary_string_literal",
                        "Rewrite as a binary string literal",
                        builder.finish(),
                        unnecessary_non_literal_range,
                    )])
                } else {
                    None
                }
            }
            StringRewrite {
                from: StringKind::List,
                to: StringKind::Atom,
            } => {
                if let StringVariant::Normal(list_string_value) =
                    matched.placeholder_is_string(sema, STRING_VAR)?
                {
                    builder.replace(
                        unnecessary_non_literal_range,
                        escape_and_quote_atom(&list_string_value),
                    );
                    Some(vec![fix(
                        "rewrite_as_an_atom_literal",
                        "Rewrite as an atom literal",
                        builder.finish(),
                        unnecessary_non_literal_range,
                    )])
                } else {
                    None
                }
            }
            StringRewrite {
                from: StringKind::Atom,
                to: StringKind::List,
            } => {
                let atom = matched.placeholder_is_atom(sema, STRING_VAR)?;
                builder.replace(
                    unnecessary_non_literal_range,
                    escape_and_quote_string(&atom.as_string(sema.db.upcast())),
                );
                Some(vec![fix(
                    "rewrite_as_a_string_literal",
                    "Rewrite as a string literal",
                    builder.finish(),
                    unnecessary_non_literal_range,
                )])
            }
            StringRewrite {
                from: StringKind::Atom,
                to: StringKind::Binary,
            } => {
                let atom = matched.placeholder_is_atom(sema, STRING_VAR)?;
                let list_string = escape_and_quote_binary_string(&atom.as_string(sema.db.upcast()));
                builder.replace(unnecessary_non_literal_range, list_string);
                Some(vec![fix(
                    "rewrite_as_a_binary_string_literal",
                    "Rewrite as a binary string literal",
                    builder.finish(),
                    unnecessary_non_literal_range,
                )])
            }
            _ => None,
        }
    }

    fn strategy(&self) -> Strategy {
        Strategy {
            // Macros are a form of documentation and normalization, so we should not inline them
            macros: MacroStrategy::DoNotExpand,
            parens: ParenStrategy::InvisibleParens,
        }
    }
}

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;

    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::tests;

    fn filter(d: &Diagnostic) -> bool {
        d.code == DiagnosticCode::CouldBeAStringLiteral
    }

    #[track_caller]
    fn check_diagnostics(fixture: &str) {
        tests::check_filtered_diagnostics(fixture, &filter)
    }

    #[track_caller]
    fn check_fix(fixture_before: &str, fixture_after: Expect) {
        tests::check_fix(fixture_before, fixture_after)
    }

    #[test]
    fn detects_list_to_binary() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> list_to_binary("foo").
         %%      ^^^^^^^^^^^^^^^^^^^^^ 💡 information: W0055: Could be rewritten as a binary string literal.

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_binary/1]).
         list_to_binary(_List) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn fixes_list_to_binary() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> li~st_to_binary("foo").

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_binary/1]).
         list_to_binary(_List) -> error(not_impl).
         "#,
            expect![[r#"
         -module(main).

         fn() -> ~"foo".

         "#]],
        )
    }

    #[test]
    fn detects_list_to_binary_fully_qualified() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> erlang:list_to_binary("foo").
         %%      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 information: W0055: Could be rewritten as a binary string literal.

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_binary/1]).
         list_to_binary(_List) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn fixes_list_to_binary_fully_qualified() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> erlang:li~st_to_binary("foo").

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_binary/1]).
         list_to_binary(_List) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> ~"foo".

         "#]],
        )
    }

    #[test]
    fn detects_list_to_atom() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> list_to_atom("foo").
         %%      ^^^^^^^^^^^^^^^^^^^ 💡 information: W0055: Could be rewritten as an atom literal.

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_atom/1]).
         list_to_atom(_Atom) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn fixes_list_to_atom() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> li~st_to_atom("foo").

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_atom/1]).
         list_to_atom(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> foo.

         "#]],
        )
    }

    #[test]
    fn detects_list_to_atom_fully_qualified() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> erlang:list_to_atom("foo").
         %%      ^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 information: W0055: Could be rewritten as an atom literal.

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_atom/1]).
         list_to_atom(_Atom) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn fixes_list_to_atom_fully_qualified() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> erlang:li~st_to_atom("foo").

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_atom/1]).
         list_to_atom(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> foo.

         "#]],
        )
    }

    #[test]
    fn detects_atom_to_binary() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> atom_to_binary(foo).
         %%      ^^^^^^^^^^^^^^^^^^^ 💡 information: W0055: Could be rewritten as a binary string literal.

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_binary/1]).
         atom_to_binary(_Atom) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn fixes_atom_to_binary() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> atom_to_b~inary(foo).

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_binary/1]).
         atom_to_binary(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> ~"foo".

         "#]],
        )
    }

    #[test]
    fn detects_atom_to_binary_fully_qualified() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> erlang:atom_to_binary(foo).
         %%      ^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 information: W0055: Could be rewritten as a binary string literal.

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_binary/1]).
         atom_to_binary(_Atom) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn fixes_atom_to_binary_fully_qualified() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> erlang:atom_to_~binary(foo).

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_binary/1]).
         atom_to_binary(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> ~"foo".

         "#]],
        )
    }

    #[test]
    fn detects_atom_to_list() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> atom_to_list(foo).
         %%      ^^^^^^^^^^^^^^^^^ 💡 information: W0055: Could be rewritten as a string literal.

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_list/1]).
         atom_to_list(_Atom) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn fixes_atom_to_list() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> atom_~to_list(foo).

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_list/1]).
         atom_to_list(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> "foo".

         "#]],
        )
    }

    #[test]
    fn detects_atom_to_list_fully_qualified() {
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> erlang:atom_to_list(foo).
         %%      ^^^^^^^^^^^^^^^^^^^^^^^^ 💡 information: W0055: Could be rewritten as a string literal.

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_list/1]).
         atom_to_list(_Atom) -> error(not_impl).
            "#,
        )
    }

    #[test]
    fn fixes_atom_to_list_fully_qualified() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> erlang:at~om_to_list(foo).

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_list/1]).
         atom_to_list(_Atom) -> error(not_impl).

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_list/1]).
         atom_to_list(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> "foo".

         "#]],
        )
    }

    #[test]
    fn ignores_potential_literals_behind_macros() {
        // Macros are a form of documentation and normalization, so we should not
        // effectively inline them by rewriting them to some function of their expansion.
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         -define(STR(X), X).

         fn() -> list_to_binary(?STR("foo")).

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_binary/1]).
         list_to_binary(_List) -> error(not_impl).
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         -define(ATOM(X), X).

         fn() -> atom_to_binary(?ATOM(foo)).

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_binary/1]).
         atom_to_binary(_Atom) -> error(not_impl).
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         -define(FOO(), "foo").

         fn() -> list_to_binary(?FOO()).

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_binary/1]).
         list_to_binary(_List) -> error(not_impl).
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         -define(FOO, "foo").

         fn() -> list_to_binary(?FOO).

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_binary/1]).
         list_to_binary(_List) -> error(not_impl).
            "#,
        );
        check_diagnostics(
            r#"
         //- /src/main.erl
         -module(main).

         -define(FOO, foo).

         fn() -> atom_to_list(?FOO).

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_list/1]).
         atom_to_list(_Atom) -> error(not_impl).
            "#,
        )
    }

    // ================= Dealing with special characters =================

    #[test]
    fn fixes_list_to_binary_emoji() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> li~st_to_binary("fo🥰o").

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_binary/1]).
         list_to_binary(_List) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> ~"fo🥰o".

         "#]],
        )
    }

    #[test]
    fn fixes_list_to_atom_emoji() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> li~st_to_atom("fo🦹🏻‍♂️o").

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_atom/1]).
         list_to_atom(_List) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> 'fo🦹🏻‍♂️o'.

         "#]],
        )
    }

    #[test]
    fn fixes_atom_to_binary_emoji() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> atom_to_bina~ry('fo🦹🏻‍♂️o').

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_binary/1]).
         atom_to_binary(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> ~"fo🦹🏻‍♂️o".

         "#]],
        )
    }

    #[test]
    fn fixes_atom_to_list_emoji() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> atom_~to_list('fo🦹🏻‍♂️o').

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_list/1]).
         atom_to_list(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> "fo🦹🏻‍♂️o".

         "#]],
        )
    }

    #[test]
    fn fixes_list_to_binary_quotes() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> li~st_to_binary("\"'").

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_binary/1]).
         list_to_binary(_List) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> ~"\"'".

         "#]],
        )
    }

    #[test]
    fn fixes_list_to_atom_quotes() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> li~st_to_atom("\"'").

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_atom/1]).
         list_to_atom(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> '"\''.

         "#]],
        )
    }

    #[test]
    fn fixes_atom_to_binary_quotes() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> atom_to_bin~ary('"\'').

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_binary/1]).
         atom_to_binary(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> ~"\"'".

         "#]],
        )
    }

    #[test]
    fn fixes_atom_to_list_quotes() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> atom_~to_list('"\'').

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_list/1]).
         atom_to_list(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> "\"'".

         "#]],
        )
    }

    #[test]
    fn fixes_list_to_binary_escapes() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> li~st_to_binary("A\sB\nC\tD").

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_binary/1]).
         list_to_binary(_List) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> ~"A B\nC\tD".

         "#]],
        )
    }

    #[test]
    fn fixes_list_to_atom_escapes() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> li~st_to_atom("A\sB\nC\tD").

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_atom/1]).
         list_to_atom(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> 'A B\nC\tD'.

         "#]],
        )
    }

    #[test]
    fn fixes_atom_to_binary_escapes() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> atom_to_bin~ary('A\sB\nC\tD').

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_binary/1]).
         atom_to_binary(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
                -module(main).

                fn() -> ~"A B\nC\tD".

                "#]],
        )
    }

    #[test]
    fn fixes_atom_to_list_escapes() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> atom_~to_list('A\sB\nC\tD').

         //- /src/erlang.erl
         -module(erlang).
         -export([atom_to_list/1]).
         atom_to_list(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> "A B\nC\tD".

         "#]],
        )
    }

    #[test]
    fn fixes_list_to_atom_keyword_is_quoted() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> li~st_to_atom("orelse").

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_atom/1]).
         list_to_atom(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> 'orelse'.

         "#]],
        )
    }

    #[test]
    fn fixes_list_to_atom_underscore_is_quoted() {
        check_fix(
            r#"
         //- /src/main.erl
         -module(main).

         fn() -> li~st_to_atom("_").

         //- /src/erlang.erl
         -module(erlang).
         -export([list_to_atom/1]).
         list_to_atom(_Atom) -> error(not_impl).
            "#,
            expect![[r#"
         -module(main).

         fn() -> '_'.

         "#]],
        )
    }
}

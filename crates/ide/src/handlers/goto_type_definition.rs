/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::RootDatabase;
use elp_ide_db::SymbolClass;
use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::find_best_token;
use elp_syntax::AstNode;
use elp_syntax::algo;
use elp_syntax::ast;
use hir::Semantic;

use crate::NavigationTarget;
use crate::RangeInfo;
use crate::navigation_target::ToNav;

pub(crate) fn goto_type_definition(
    db: &RootDatabase,
    position: FilePosition,
    refs: Vec<FileRange>,
) -> Option<RangeInfo<Vec<NavigationTarget>>> {
    let sema = Semantic::new(db);
    let token = find_best_token(&sema, position)?;

    let mut targets = Vec::new();
    refs.into_iter().for_each(|ref_range| {
        if let Some(class) = classify(&sema, ref_range) {
            class.iter().for_each(|def| targets.push(def.to_nav(db)));
        }
    });
    Some(RangeInfo::new(token.value.text_range(), targets))
}

fn classify(sema: &Semantic, range: FileRange) -> Option<SymbolClass> {
    let syntax = sema.parse(range.file_id).map(|file| file.syntax().clone());
    let type_alias = algo::find_node_at_range::<ast::TypeAlias>(&syntax.value, range.range)?;
    let type_name_range = type_alias.name()?.syntax().text_range();
    let position = FilePosition {
        file_id: range.file_id,
        offset: type_name_range.start(),
    };
    let token = find_best_token(&sema, position)?;
    SymbolClass::classify(&sema, token)
}

#[cfg(test)]
mod tests {

    use elp_project_model::otp::otp_supported_by_eqwalizer;

    use crate::fixture;
    use crate::tests::check_navs;
    use crate::tests::check_no_parse_errors;

    #[track_caller]
    fn check(fixture: &str) {
        check_worker(fixture, true)
    }

    #[track_caller]
    fn check_worker(fixture: &str, check_parse_error: bool) {
        let (analysis, fixture) = fixture::with_fixture(fixture);
        let expected = fixture.annotations();
        if check_parse_error {
            check_no_parse_errors(&analysis, fixture.file_id());
        }

        let navs = analysis
            .goto_type_definition(fixture.position())
            .unwrap()
            .expect("no type definition found")
            .info;

        if navs.is_empty() {
            panic!("got some with empty navs!");
        }

        check_navs(navs, expected);
    }

    #[test]
    fn local_type_alias() {
        if otp_supported_by_eqwalizer() {
            check(
                r#"
//- eqwalizer
//- /src/goto_type_def.erl
-module(goto_type_def).
-export([foo/1, bar/0]).

-type my_integer() :: integer().
%%    ^^^^^^^^^^^^

-spec foo(boolean()) -> my_integer().
foo(X) ->
  case X of
    true -> 42;
    false -> 0
  end.

-spec bar() -> ok | error.
bar() ->
  YYY = foo(true),
  case Y~YY of
    42 -> ok;
    _ -> error
  end.
"#,
            );
        }
    }

    #[test]
    fn multiple_type_aliases() {
        if otp_supported_by_eqwalizer() {
            check(
                r#"
//- eqwalizer
//- /src/goto_type_def_one.erl
-module(goto_type_def_one).
-export([foo/1, bar/0]).

-type my_integer() :: integer().
%%    ^^^^^^^^^^^^

-spec foo(boolean()) -> my_integer() | goto_type_def_two:my_other_integer().
foo(X) ->
  case X of
    true -> 42;
    false -> 0
  end.

-spec bar() -> ok | error.
bar() ->
  YYY = foo(true),
  case Y~YY of
    42 -> ok;
    _ -> error
  end.
//- /src/goto_type_def_two.erl
-module(goto_type_def_two).
-type my_other_integer() :: integer().
%%    ^^^^^^^^^^^^^^^^^^
-export_type([my_other_integer/0]).
"#,
            );
        }
    }
}

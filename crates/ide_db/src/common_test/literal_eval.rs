/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Literal-only fast path for the `all/0` and `groups/0` Common Test callbacks.
//!
//! The Erlang Service answers a `ct_info` request by shipping the suite's
//! entire abstract forms over a pipe and running `erl_eval` over them. The
//! evaluation itself is negligible; the transfer and the `binary_to_term` that
//! follows it are not, and they scale with module size rather than with the
//! callbacks being evaluated.
//!
//! Most suites declare both callbacks as plain data:
//!
//! ```erlang
//! all() -> [{group, nif}].
//! groups() -> [{nif, [], [encode_test, decode_test]}].
//! ```
//!
//! When that is the case the term the service would have returned can be built
//! here, and the request skipped entirely. Anything this module does not fully
//! recognise yields `None` so the caller falls back to the Erlang Service —
//! the fast path can remove work, never change an answer.

use std::iter;

use eetf::Term;
use elp_base_db::FileId;
use elp_erlang_service::common_test;
use elp_syntax::AstNode;
use elp_syntax::ast;
use hir::AsName;
use hir::Body;
use hir::DefMap;
use hir::Expr;
use hir::ExprId;
use hir::InFile;
use hir::Literal;
use hir::NameArity;
use hir::db::DefDatabase;
use hir::known;

use crate::common_test::CommonTestInfo;

/// Longest list `term_to_binary/1` will encode as `STRING_EXT`. Beyond this it
/// falls back to `LIST_EXT`, which decodes as a plain list of integers.
const MAX_STRING_EXT_LEN: usize = 65535;

/// Evaluate `all/0` and `groups/0` without consulting the Erlang Service, when
/// both are literal data.
///
/// `should_request_groups` must be the same flag the caller would have put on
/// the `CTInfoRequest`: the service only calls `groups/0` when it is exported,
/// and returns `[]` for the groups otherwise.
pub(crate) fn try_literal_ct_info(
    db: &dyn DefDatabase,
    file_id: FileId,
    def_map: &DefMap,
    should_request_groups: bool,
) -> Option<CommonTestInfo> {
    if has_unsafe_parse_transform(db, file_id, def_map) {
        return None;
    }

    let all_term = literal_callback(db, file_id, def_map, *known::all)?;
    let groups_term = if should_request_groups {
        literal_callback(db, file_id, def_map, *known::groups)?
    } else {
        eetf::List::from(vec![]).into()
    };

    // Reuse the very same converters the Erlang Service path uses, so the two
    // can only differ if the term itself differs.
    match (
        common_test::all(&all_term),
        common_test::groups(&groups_term),
    ) {
        (Ok(all), Ok(groups)) => Some(CommonTestInfo::Result { all, groups }),
        (Err(err), _) | (_, Err(err)) => Some(CommonTestInfo::ConversionError(err)),
    }
}

/// Parse transforms known not to touch `all/0` or `groups/0`.
///
/// `wa_assert_parse_trans` rewrites only *applications* of
/// `wa_assert:'$assert_match_error_info$'/1` and `wa_assert:'$expand_assert$'/1`,
/// the internal targets the `?assert*` macros expand to. A literal callback
/// body contains no applications at all, so the transform cannot reach it. It
/// is declared by `assert/include/assert.hrl`, which nearly every WhatsApp
/// Server suite includes, so treating it as unknown would disable the fast
/// path across the whole tree.
const CALLBACK_SAFE_PARSE_TRANSFORMS: &[&str] = &["wa_assert_parse_trans"];

/// Whether a parse transform we cannot vouch for applies to this module.
///
/// Parse transforms run inside the Erlang Service, in `erlang_service_lint:ast/2`,
/// *after* the point HIR is built from, so one is free to rewrite `all/0` or
/// `groups/0` and leave the two paths disagreeing. Three sources have to be
/// considered, and `erlang_service_lint:collect_parse_transforms/3` folds them
/// all into one list:
///
/// - transforms configured for the application;
/// - `-compile({parse_transform, _})` in the module itself;
/// - the same attribute in a header the module includes. `DefMap::merge` does
///   not propagate `parse_transform` from included files, so `def_map` alone
///   would miss this — and it is how WhatsApp Server declares its only
///   transform.
///
/// Anything whose shape is not recognised counts as unsafe.
fn has_unsafe_parse_transform(db: &dyn DefDatabase, file_id: FileId, def_map: &DefMap) -> bool {
    let app_transform_unsafe = db.file_app_data(file_id).is_some_and(|app_data| {
        app_data
            .parse_transforms
            .iter()
            .any(|transform| !is_safe_transform_term(transform))
    });
    if app_transform_unsafe {
        return true;
    }

    // `parse_transform` on the *local* def map is a cheap cached bool, so only
    // the handful of files that declare anything reach the syntax walk below.
    iter::once(file_id)
        .chain(def_map.get_included_files())
        .any(|file_id| {
            db.def_map_local(file_id).parse_transform
                && declares_unsafe_parse_transform(db, file_id)
        })
}

fn is_safe_transform_term(transform: &eetf::Term) -> bool {
    match transform {
        Term::Atom(atom) => CALLBACK_SAFE_PARSE_TRANSFORMS.contains(&atom.name.as_str()),
        _ => false,
    }
}

fn declares_unsafe_parse_transform(db: &dyn DefDatabase, file_id: FileId) -> bool {
    let form_list = db.file_form_list(file_id);
    let source = db.parse(file_id).tree();
    form_list.compile_attributes().any(|(_, option)| {
        option
            .form_id
            .get(&source)
            .options()
            .is_some_and(|options| {
                options
                    .syntax()
                    .descendants()
                    .filter_map(ast::Atom::cast)
                    .filter(|atom| atom.as_name() == *known::parse_transform)
                    .any(|tag| !names_only_safe_transforms(&tag))
            })
    })
}

/// Read the transform name out of the `{parse_transform, Module}` tuple the
/// given atom tags. Any other shape is treated as unsafe, since we cannot tell
/// what it will do.
fn names_only_safe_transforms(tag: &ast::Atom) -> bool {
    let Some(tuple) = tag.syntax().parent().and_then(ast::Tuple::cast) else {
        return false;
    };
    let mut elements = tuple.expr();
    // Guard against an atom that merely *mentions* `parse_transform` in a
    // value position, where the element after it means something else.
    if elements.next().as_ref().map(|first| first.syntax()) != Some(tag.syntax()) {
        return false;
    }
    let Some(name) = elements.next() else {
        return false;
    };
    if elements.next().is_some() {
        return false;
    }
    ast::Atom::cast(name.syntax().clone())
        .is_some_and(|name| CALLBACK_SAFE_PARSE_TRANSFORMS.contains(&name.as_name().as_str()))
}

/// The literal value of a zero-arity callback, if it has exactly one
/// unguarded clause whose body is a single literal expression.
fn literal_callback(
    db: &dyn DefDatabase,
    file_id: FileId,
    def_map: &DefMap,
    name: hir::Name,
) -> Option<Term> {
    let def = def_map.get_function(&NameArity::new(name, 0))?;

    // `def_map` resolves across includes, so the callback can come from a
    // header -- but a header body is lowered in that header's own macro
    // environment, not the includer's. A callback that reads a macro the suite
    // defines *before* the `-include` (a `-ifndef` default, say) would be
    // evaluated by the Erlang Service against the suite's definition and here
    // against the header's. Leave those to the service.
    if def.file.file_id != file_id {
        return None;
    }

    // `function_id` indexes the arena of the file that defines the function,
    // which the check above has pinned to `file_id`.
    let function_body = db.function_body(InFile::new(def.file.file_id, def.function_id));

    // A zero-arity clause has no patterns, so an unguarded first clause always
    // matches and any later clause is dead code -- which the Erlang compiler
    // warns about but accepts. That leaves the guard as the only thing that can
    // make the choice of clause depend on evaluation.
    let (_, clause) = function_body.clauses.iter().next()?;
    if !clause.clause.guards.is_empty() {
        return None;
    }
    let [expr_id] = clause.clause.exprs[..] else {
        return None;
    };
    expr_to_term(&clause.body, expr_id)
}

/// Build the term `erl_eval` would have produced for a literal expression.
///
/// `Body`'s `Index` impl already looks through macro expansions and parens, so
/// a macro expanding to literal data is handled here too.
fn expr_to_term(body: &Body, expr_id: ExprId) -> Option<Term> {
    match &body[expr_id] {
        Expr::Literal(literal) => literal_to_term(literal),
        Expr::Tuple { exprs } => Some(eetf::Tuple::from(exprs_to_terms(body, exprs)?).into()),
        Expr::List { exprs, tail } => {
            // `[Head | Tail]` needs an evaluation step we do not perform.
            if tail.is_some() {
                return None;
            }
            Some(list_term(exprs_to_terms(body, exprs)?))
        }
        _ => None,
    }
}

fn exprs_to_terms(body: &Body, exprs: &[ExprId]) -> Option<Vec<Term>> {
    exprs.iter().map(|expr| expr_to_term(body, *expr)).collect()
}

fn literal_to_term(literal: &Literal) -> Option<Term> {
    match literal {
        Literal::Atom(atom) => Some(eetf::Atom::from(atom.as_string()).into()),
        Literal::Integer(integer) => small_integer(integer.value),
        Literal::Char(char) => small_integer(i128::from(u32::from(*char))),
        Literal::String(string) => {
            let terms = string
                .chars()
                .map(|char| small_integer(i128::from(u32::from(char))))
                .collect::<Option<Vec<_>>>()?;
            Some(list_term(terms))
        }
        // Bigints and floats do not occur in Common Test callbacks, and
        // reproducing their encoding exactly is not worth the surface area.
        Literal::Float(_) => None,
    }
}

fn small_integer(value: i128) -> Option<Term> {
    i32::try_from(value)
        .ok()
        .map(|value| Term::from(eetf::FixInteger { value }))
}

/// `term_to_binary/1` encodes a non-empty list of byte-sized integers as
/// `STRING_EXT`, which `eetf` decodes as `ByteList` rather than `List`.
/// Mirroring that keeps the fast path's term equal to the service's, rather
/// than merely equivalent everywhere the Common Test converters happen to look.
fn list_term(elements: Vec<Term>) -> Term {
    let bytes: Option<Vec<u8>> = elements
        .iter()
        .map(|element| match element {
            Term::FixInteger(integer) => u8::try_from(integer.value).ok(),
            _ => None,
        })
        .collect();
    match bytes {
        Some(bytes) if !bytes.is_empty() && bytes.len() <= MAX_STRING_EXT_LEN => {
            eetf::ByteList { bytes }.into()
        }
        _ => eetf::List::from(elements).into(),
    }
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use elp_base_db::Upcast;
    use elp_base_db::assert_eq_expected;
    use elp_base_db::fixture::WithFixture;
    use elp_erlang_service::common_test::GroupDef;
    use elp_erlang_service::common_test::TestDef;

    use super::*;
    use crate::RootDatabase;

    const FALL_BACK: &str = "<fall back to the Erlang Service>";

    fn check(expected: &str, erlang: &str) {
        let (db, file_id) = RootDatabase::with_single_file(erlang);
        check_file(expected, &db, file_id);
    }

    /// Multi-file variant; the module under test must be declared first.
    fn check_fixture(expected: &str, fixture: &str) {
        let (db, fixture) = RootDatabase::with_fixture(fixture);
        check_file(expected, &db, fixture.files[0]);
    }

    fn check_file(expected: &str, db: &RootDatabase, file_id: FileId) {
        let def_map = db.def_map(file_id);
        let should_request_groups =
            def_map.is_function_exported(&NameArity::new(*known::groups, 0));
        let actual =
            match try_literal_ct_info(db.upcast(), file_id, &def_map, should_request_groups) {
                Some(info) => render(&info),
                None => FALL_BACK.to_string(),
            };
        assert_eq_expected!(expected, &actual);
    }

    fn render(info: &CommonTestInfo) -> String {
        match info {
            CommonTestInfo::Result { all, groups } => {
                format!(
                    "all: {} | groups: {}",
                    render_list(all.iter().map(render_test_def)),
                    render_list(groups.values().map(render_group_def)),
                )
            }
            other => format!("{other:?}"),
        }
    }

    /// `all` is a set and `groups` a map, so sort for a stable expectation.
    fn render_list(items: impl Iterator<Item = String>) -> String {
        let mut items: Vec<String> = items.collect();
        items.sort();
        format!("[{}]", items.join(","))
    }

    fn render_test_def(test_def: &TestDef) -> String {
        match test_def {
            TestDef::TestName(name) => name.to_string(),
            TestDef::GroupName(name) => format!("{{group,{name}}}"),
            TestDef::GroupDef(name, content) => {
                format!(
                    "{{{name},{}}}",
                    render_list(content.iter().map(render_test_def))
                )
            }
        }
    }

    fn render_group_def(group_def: &GroupDef) -> String {
        format!(
            "{{{},{}}}",
            group_def.name,
            render_list(group_def.content.iter().map(render_test_def))
        )
    }

    #[test]
    fn plain_list_of_test_names() {
        check(
            "all: [a,b] | groups: []",
            r#"
-module(my_SUITE).
-export([all/0]).
all() -> [a, b].
"#,
        );
    }

    #[test]
    fn empty_all() {
        check(
            "all: [] | groups: []",
            r#"
-module(my_SUITE).
-export([all/0]).
all() -> [].
"#,
        );
    }

    #[test]
    fn groups_with_properties() {
        check(
            "all: [{group,nif}] | groups: [{nif,[decode,encode]}]",
            r#"
-module(my_SUITE).
-export([all/0, groups/0]).
all() -> [{group, nif}].
groups() -> [{nif, [parallel, {repeat, 3}], [encode, decode]}].
"#,
        );
    }

    #[test]
    fn nested_group_definition() {
        check(
            "all: [{group,outer}] | groups: [{outer,[{inner,[a]}]}]",
            r#"
-module(my_SUITE).
-export([all/0, groups/0]).
all() -> [{group, outer}].
groups() -> [{outer, [], [{inner, [], [a]}]}].
"#,
        );
    }

    /// The Erlang Service only calls `groups/0` when it is exported, and
    /// reports no groups otherwise. The fast path must agree.
    #[test]
    fn unexported_groups_is_ignored() {
        check(
            "all: [a] | groups: []",
            r#"
-module(my_SUITE).
-export([all/0]).
all() -> [a].
groups() -> [{g, [], [a]}].
"#,
        );
    }

    /// `Body`'s `Index` impl looks through macro expansions, so a macro
    /// expanding to literal data stays on the fast path.
    #[test]
    fn macro_expanding_to_literals() {
        check(
            "all: [a,b] | groups: []",
            r#"
-module(my_SUITE).
-export([all/0]).
-define(TESTS, [a, b]).
all() -> ?TESTS.
"#,
        );
    }

    #[test]
    fn call_falls_back() {
        check(
            FALL_BACK,
            r#"
-module(my_SUITE).
-export([all/0]).
all() -> test_names().
test_names() -> [a].
"#,
        );
    }

    #[test]
    fn non_literal_groups_falls_back() {
        check(
            FALL_BACK,
            r#"
-module(my_SUITE).
-export([all/0, groups/0]).
all() -> [{group, g}].
groups() -> unit_groups() ++ server_groups().
unit_groups() -> [].
server_groups() -> [].
"#,
        );
    }

    #[test]
    fn list_comprehension_falls_back() {
        check(
            FALL_BACK,
            r#"
-module(my_SUITE).
-export([all/0]).
all() -> [T || T <- [a, b]].
"#,
        );
    }

    #[test]
    fn cons_tail_falls_back() {
        check(
            FALL_BACK,
            r#"
-module(my_SUITE).
-export([all/0]).
-define(REST, [b]).
all() -> [a | ?REST].
"#,
        );
    }

    /// `erlc` accepts this with a "cannot match" warning, so it does reach us.
    /// With no patterns to match, the first clause wins.
    #[test]
    fn later_clauses_are_dead_code() {
        check(
            "all: [a] | groups: []",
            r#"
-module(my_SUITE).
-export([all/0]).
all() -> [a];
all() -> [b].
"#,
        );
    }

    /// A guard is the only way the choice of clause can depend on evaluation.
    #[test]
    fn guarded_clause_falls_back() {
        check(
            FALL_BACK,
            r#"
-module(my_SUITE).
-export([all/0]).
all() when node() =:= nonode@nohost -> [a];
all() -> [b].
"#,
        );
    }

    #[test]
    fn several_body_expressions_fall_back() {
        check(
            FALL_BACK,
            r#"
-module(my_SUITE).
-export([all/0]).
all() -> _ = ok, [a].
"#,
        );
    }

    #[test]
    fn missing_all_falls_back() {
        check(
            FALL_BACK,
            r#"
-module(my_SUITE).
-export([groups/0]).
groups() -> [].
"#,
        );
    }

    /// A well-formed literal that is not a valid Common Test shape converts to
    /// the same error the Erlang Service path would report, so there is
    /// nothing to gain by asking the service.
    #[test]
    fn invalid_shape_reports_the_conversion_error() {
        check(
            "ConversionError(InvalidTestDefinition)",
            r#"
-module(my_SUITE).
-export([all/0]).
all() -> [{a, b, c, d}].
"#,
        );
    }

    /// `erlang_service_lint:collect_parse_transforms/3` picks up transforms a
    /// module declares itself and applies them before the service evaluates
    /// the callbacks, so HIR may not be looking at the same `all/0` we would
    /// be answering from.
    #[test]
    fn file_local_parse_transform_falls_back() {
        check(
            FALL_BACK,
            r#"
-module(my_SUITE).
-compile({parse_transform, my_transform}).
-export([all/0]).
all() -> [a].
"#,
        );
    }

    /// The same attribute written in the list form the Erlang Service also
    /// accepts.
    #[test]
    fn file_local_parse_transform_in_a_list_falls_back() {
        check(
            FALL_BACK,
            r#"
-module(my_SUITE).
-compile([debug_info, {parse_transform, my_transform}]).
-export([all/0]).
all() -> [a].
"#,
        );
    }

    /// A header body is lowered in the header's own macro environment rather
    /// than the includer's, so a header-defined callback can evaluate
    /// differently here than in the Erlang Service. We decline those.
    ///
    /// `helper/0` is here on purpose: its body is what an earlier revision
    /// returned, having indexed the suite's arena with the header's
    /// `FunctionDefId`. Seeing `[wrong]` again would mean that has come back.
    #[test]
    fn callback_defined_in_an_included_header_falls_back() {
        check_fixture(
            FALL_BACK,
            r#"
//- /my_SUITE.erl
-module(my_SUITE).
-include("callbacks.hrl").
helper() -> [wrong].
//- /callbacks.hrl
-export([all/0]).
all() -> [a].
"#,
        );
    }

    /// The concrete divergence the previous test guards against: the service
    /// preprocesses in context and sees the suite's `?TESTS`, while a
    /// standalone lowering of the header sees the `-ifndef` default.
    #[test]
    fn header_callback_reading_an_includer_macro_falls_back() {
        check_fixture(
            FALL_BACK,
            r#"
//- /my_SUITE.erl
-module(my_SUITE).
-define(TESTS, [real_test]).
-include("callbacks.hrl").
//- /callbacks.hrl
-ifndef(TESTS).
-define(TESTS, [header_default]).
-endif.
-export([all/0]).
all() -> ?TESTS.
"#,
        );
    }

    /// `DefMap::merge` does not carry `parse_transform` up from included
    /// files, so this case has to be found by walking the includes.
    #[test]
    fn parse_transform_from_an_include_falls_back() {
        check_fixture(
            FALL_BACK,
            r#"
//- /my_SUITE.erl
-module(my_SUITE).
-include("header.hrl").
-export([all/0]).
all() -> [a].
//- /header.hrl
-compile({parse_transform, my_transform}).
"#,
        );
    }

    /// The WhatsApp Server shape: `assert.hrl` declares `wa_assert_parse_trans`
    /// and nearly every suite includes it. That transform only rewrites
    /// `wa_assert` applications, so it cannot reach a literal callback.
    #[test]
    fn known_safe_parse_transform_from_an_include_stays_on_the_fast_path() {
        check_fixture(
            "all: [a] | groups: []",
            r#"
//- /my_SUITE.erl
-module(my_SUITE).
-include("assert.hrl").
-export([all/0]).
all() -> [a].
//- /assert.hrl
-compile({parse_transform, wa_assert_parse_trans}).
"#,
        );
    }

    /// A safe transform alongside one we know nothing about still falls back.
    #[test]
    fn safe_and_unknown_parse_transforms_fall_back() {
        check_fixture(
            FALL_BACK,
            r#"
//- /my_SUITE.erl
-module(my_SUITE).
-include("assert.hrl").
-compile({parse_transform, my_transform}).
-export([all/0]).
all() -> [a].
//- /assert.hrl
-compile({parse_transform, wa_assert_parse_trans}).
"#,
        );
    }

    /// An unrelated `-compile` attribute must not disable the fast path.
    #[test]
    fn unrelated_compile_attribute_stays_on_the_fast_path() {
        check(
            "all: [a] | groups: []",
            r#"
-module(my_SUITE).
-compile([debug_info, export_all]).
-export([all/0]).
all() -> [a].
"#,
        );
    }

    #[test]
    fn float_falls_back() {
        check(
            FALL_BACK,
            r#"
-module(my_SUITE).
-export([all/0, groups/0]).
all() -> [{group, g}].
groups() -> [{g, [{seconds, 1.5}], [a]}].
"#,
        );
    }
}

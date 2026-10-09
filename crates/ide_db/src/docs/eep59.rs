/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Native extraction of EEP-59 documentation attributes (`-moduledoc` and
//! `-doc`), following the rules of OTP's `beam_doc`. Each rule has a test
//! of the same name:
//!
//! - **Module doc**: a `-moduledoc` string documents the module.
//! - **Attachment**: a `-doc` documents the next function or type definition,
//!   and only that one. Forms in between that are not definitions (`-spec`,
//!   `-export`, `-export_type`, `-record`, ...) do not interrupt it.
//! - **Last doc wins**: when several `-doc` (or `-moduledoc`) attributes
//!   precede a definition (or the module), the last one is used.
//! - **Callbacks**: a `-callback` takes the preceding `-doc`, which is not
//!   passed on to the next definition. Callback docs are not reported.
//! - **Hidden**: `-doc false.` and `-doc hidden.` hide the next definition,
//!   even if it has `equiv` metadata. `-moduledoc false.` and
//!   `-moduledoc hidden.` hide the module.
//! - **Unexported definitions**: functions and types are documented whether
//!   or not they are exported.
//! - **Signature line**: a first line that is a call to the documented
//!   function with the matching arity is its signature, and is removed from
//!   the text.
//! - **Equiv metadata**: without a doc string, `equiv` metadata produces
//!   "equivalent to `...`", using the source text of the value. A doc string
//!   takes precedence.
//! - **Repeated definitions**: a function defined by several `.`-terminated
//!   forms keeps the doc of the first one, unless a later form has its own
//!   `-doc`, which replaces it.
//! - **Inactive branches**: forms in inactive preprocessor branches,
//!   definitions included, are skipped.
//! - **String decoding**: doc strings are decoded like Erlang string
//!   literals (escapes, triple-quoted, sigils, adjacent literals, optional
//!   parentheses) and trimmed.
//! - **Macros**: doc attributes whose value contains a macro call are not
//!   expanded, and are ignored.
//! - **EDoc comments**: `%% @doc` comments are not read.

use elp_base_db::FileId;
use elp_syntax::AstNode;
use elp_syntax::SourceFile;
use elp_syntax::ast;
use fxhash::FxHashMap;
use hir::AsName;
use hir::FormIdx;
use hir::FormList;
use hir::NameArity;
use hir::PPConditionResult;
use hir::db::DefDatabase;
use hir::known;

use super::Doc;
use super::FileDoc;

#[derive(Debug, Clone, PartialEq, Eq)]
enum DocValue {
    Text(String),
    Hidden,
}

#[derive(Debug, Default)]
struct PendingDoc {
    value: Option<DocValue>,
    equiv: Option<String>,
}

impl PendingDoc {
    fn has_doc(&self) -> bool {
        self.value.is_some() || self.equiv.is_some()
    }

    fn to_doc(&self, name: &NameArity) -> Option<Doc> {
        let text = match &self.value {
            Some(DocValue::Hidden) => return None,
            Some(DocValue::Text(text)) => strip_signature(text.trim(), name).to_string(),
            None => format!("equivalent to `{}`", self.equiv.as_ref()?),
        };
        Some(Doc::new(text))
    }
}

pub(super) fn file_docs(db: &dyn DefDatabase, file_id: FileId) -> FileDoc {
    let form_list = db.file_form_list(file_id);
    let source = db.parse(file_id).tree();

    let mut module_doc = None;
    let mut function_docs = FxHashMap::default();
    let mut type_docs = FxHashMap::default();
    let mut pending = PendingDoc::default();

    for &form in form_list.forms() {
        if !is_active(db, file_id, &form_list, form) {
            continue;
        }
        match form {
            FormIdx::ModuleDocAttribute(idx) => {
                let attribute = form_list[idx].form_id.get(&source);
                module_doc = match doc_value(&attribute) {
                    Some(DocValue::Text(text)) => Some(Doc::new(text.trim().to_string())),
                    Some(DocValue::Hidden) | None => None,
                };
            }
            FormIdx::DocAttribute(idx) => {
                let attribute = form_list[idx].form_id.get(&source);
                pending.value = doc_value(&attribute);
            }
            FormIdx::DocMetadataAttribute(idx) => {
                let attribute = form_list[idx].form_id.get(&source);
                if let Some(equiv) = equiv_metadata(&attribute) {
                    pending.equiv = Some(equiv);
                }
            }
            FormIdx::FunctionClause(idx) => {
                record_doc(&mut function_docs, &form_list[idx].name, &pending);
                pending = PendingDoc::default();
            }
            FormIdx::TypeAlias(idx) => {
                record_doc(&mut type_docs, form_list[idx].name(), &pending);
                pending = PendingDoc::default();
            }
            FormIdx::Callback(_) => {
                pending = PendingDoc::default();
            }
            _ => {}
        }
    }

    FileDoc {
        module_doc,
        function_docs,
        type_docs,
    }
}

fn is_active(db: &dyn DefDatabase, file_id: FileId, form_list: &FormList, form: FormIdx) -> bool {
    if matches!(form, FormIdx::PPCondition(_)) {
        return false;
    }
    let Some(pp_ctx) = form_list.get(form).pp_ctx(form_list) else {
        return true;
    };
    form_list.is_form_active(db, file_id, pp_ctx, None) != PPConditionResult::Inactive
}

/// A function with several clauses separated by `.` (or a function
/// defined more than once) keeps its first doc unless a later one
/// provides a new doc, as `beam_doc` does.
fn record_doc(docs: &mut FxHashMap<NameArity, Doc>, name: &NameArity, pending: &PendingDoc) {
    if !pending.has_doc() && docs.contains_key(name) {
        return;
    }
    match pending.to_doc(name) {
        Some(doc) => {
            docs.insert(name.clone(), doc);
        }
        None => {
            docs.remove(name);
        }
    }
}

fn doc_value(attribute: &ast::WildAttribute) -> Option<DocValue> {
    expr_doc_value(attribute.value()?)
}

fn expr_doc_value(expr: ast::Expr) -> Option<DocValue> {
    let ast::Expr::ExprMax(expr_max) = expr else {
        return None;
    };
    match expr_max {
        ast::ExprMax::ParenExpr(paren) => expr_doc_value(paren.expr()?),
        ast::ExprMax::String(string) => Some(DocValue::Text(String::from(string))),
        ast::ExprMax::Concatables(concatables) => concatables
            .elems()
            .map(|elem| match elem {
                ast::Concatable::String(string) => Some(String::from(string)),
                _ => None,
            })
            .collect::<Option<String>>()
            .map(DocValue::Text),
        ast::ExprMax::Atom(atom) => {
            let name = atom.as_name();
            (name == *known::false_name || name == *known::hidden).then_some(DocValue::Hidden)
        }
        _ => None,
    }
}

fn equiv_metadata(attribute: &ast::WildAttribute) -> Option<String> {
    let map = metadata_map(attribute.value()?)?;
    map.fields().find_map(|field| {
        let ast::Expr::ExprMax(ast::ExprMax::Atom(key)) = field.key()? else {
            return None;
        };
        if key.as_name() != *known::equiv {
            return None;
        }
        Some(field.value()?.syntax().text().to_string())
    })
}

fn metadata_map(expr: ast::Expr) -> Option<ast::MapExpr> {
    match expr {
        ast::Expr::MapExpr(map) => Some(map),
        ast::Expr::ExprMax(ast::ExprMax::ParenExpr(paren)) => metadata_map(paren.expr()?),
        _ => None,
    }
}

/// If the first line of the doc is a call to the documented function
/// with the right arity (e.g. `foo(Bar, Baz)`), `beam_doc` uses it as the
/// signature and drops it from the doc text.
fn strip_signature<'a>(text: &'a str, name: &NameArity) -> &'a str {
    let (first_line, rest) = text.split_once('\n').unwrap_or((text, ""));
    if is_signature(first_line, name) {
        rest.trim()
    } else {
        text
    }
}

fn is_signature(line: &str, name: &NameArity) -> bool {
    let line = line.trim();
    if !line.ends_with(')') || !line.contains('(') {
        return false;
    }
    let parse = SourceFile::parse_text(&format!("f() -> {line}."));
    if !parse.errors().is_empty() {
        return false;
    }
    let Some(call) = parse
        .tree()
        .syntax()
        .descendants()
        .find_map(ast::Call::cast)
    else {
        return false;
    };
    if call.syntax().text() != line {
        return false;
    }
    let Some(ast::Expr::ExprMax(ast::ExprMax::Atom(atom))) = call.expr() else {
        return false;
    };
    let arity = call.args().map_or(0, |args| args.args().count());
    atom.as_name() == *name.name() && arity == name.arity() as usize
}

#[cfg(test)]
mod tests {
    use elp_base_db::fixture::WithFixture;
    use expect_test::Expect;
    use expect_test::expect;
    use itertools::Itertools;

    use super::file_docs;
    use crate::RootDatabase;

    #[track_caller]
    fn check(fixture: &str, expected: Expect) {
        let (db, file_id) = RootDatabase::with_single_file(fixture);
        let docs = file_docs(&db, file_id);
        let mut actual = String::new();
        if let Some(doc) = &docs.module_doc {
            actual.push_str(&format!("module: {:?}\n", doc.markdown_text()));
        }
        for (name, doc) in docs
            .function_docs
            .iter()
            .sorted_by_key(|(na, _)| na.to_string())
        {
            actual.push_str(&format!("function {name}: {:?}\n", doc.markdown_text()));
        }
        for (name, doc) in docs
            .type_docs
            .iter()
            .sorted_by_key(|(na, _)| na.to_string())
        {
            actual.push_str(&format!("type {name}: {:?}\n", doc.markdown_text()));
        }
        expected.assert_eq(&actual);
    }

    #[test]
    fn module_doc() {
        check(
            r#"
-module(main).
-moduledoc "The module doc".
"#,
            expect![[r#"
                module: "The module doc"
            "#]],
        );
    }

    #[test]
    fn attachment() {
        check(
            r#"
-module(main).
-export([one/0]).
-doc "Before a spec".
-spec one() -> ok.
one() -> ok.
two() -> ok.
-doc "Before an export_type".
-export_type([t/0]).
-type t() :: ok.
-doc "Before a record".
-record(r, {f}).
three() -> ok.
"#,
            expect![[r#"
                function one/0: "Before a spec"
                function three/0: "Before a record"
                type t/0: "Before an export_type"
            "#]],
        );
    }

    #[test]
    fn last_doc_wins() {
        check(
            r#"
-module(main).
-moduledoc "First module doc".
-moduledoc "Second module doc".
-doc "First".
-doc "Second".
one() -> 1.
-doc #{equiv => a()}.
-doc #{equiv => b()}.
two() -> 2.
"#,
            expect![[r#"
                module: "Second module doc"
                function one/0: "Second"
                function two/0: "equivalent to `b()`"
            "#]],
        );
    }

    #[test]
    fn callbacks() {
        check(
            r#"
-module(main).
-doc "Callback doc".
-callback cb() -> ok.
one() -> 1.
-doc "Function doc".
two() -> 2.
"#,
            expect![[r#"
                function two/0: "Function doc"
            "#]],
        );
    }

    #[test]
    fn hidden() {
        check(
            r#"
-module(main).
-moduledoc hidden.
-doc false.
one() -> 1.
-doc hidden.
two() -> 2.
-doc false.
-type t() :: ok.
-doc hidden.
-type u() :: ok.
-doc hidden.
-doc #{equiv => one()}.
three() -> 3.
-doc "Visible".
four() -> 4.
"#,
            expect![[r#"
                function four/0: "Visible"
            "#]],
        );
    }

    #[test]
    fn unexported_definitions() {
        check(
            r#"
-module(main).
-export([public/0]).
-doc "Public".
public() -> private().
-doc "Private".
private() -> ok.
-doc "Unexported type, unused in specs".
-type t() :: ok.
"#,
            expect![[r#"
                function private/0: "Private"
                function public/0: "Public"
                type t/0: "Unexported type, unused in specs"
            "#]],
        );
    }

    #[test]
    fn signature_line() {
        check(
            r#"
-module(main).
-doc """
add(A, B)

Adds A and B.
""".
add(A, B) -> A + B.

-doc """
add(A)
Wrong arity: kept.
""".
add(A, B, C) -> A + B + C.

-doc """
add(A, B)
Other function: kept.
""".
mul(A, B) -> A * B.

-doc """
sub(A, B) and more
Not only a call: kept.
""".
sub(A, B) -> A - B.

-doc """
zero()
Zero arity.
""".
zero() -> 0.

-doc "neg(A)".
neg(A) -> -A.
"#,
            expect![[r#"
                function add/2: "Adds A and B."
                function add/3: "add(A)\nWrong arity: kept."
                function mul/2: "add(A, B)\nOther function: kept."
                function neg/1: ""
                function sub/2: "sub(A, B) and more\nNot only a call: kept."
                function zero/0: "Zero arity."
            "#]],
        );
    }

    #[test]
    fn equiv_metadata() {
        check(
            r#"
-module(main).
-doc #{equiv => one(ok)}.
one() -> one(ok).
-doc #{equiv => one/1}.
alias() -> ok.
-doc(#{since => "1.0", equiv => two(ok)}).
two() -> two(ok).
-doc "Doc string".
-doc #{equiv => one()}.
one(_) -> ok.
-doc #{since => "1.0"}.
three() -> ok.
"#,
            expect![[r#"
                function alias/0: "equivalent to `one/1`"
                function one/0: "equivalent to `one(ok)`"
                function one/1: "Doc string"
                function two/0: "equivalent to `two(ok)`"
            "#]],
        );
    }

    #[test]
    fn repeated_definitions() {
        check(
            r#"
-module(main).
-doc "First".
f() -> 1.
f() -> 2.
-doc "First".
g(1) -> a.
-doc "Second".
g(2) -> b.
-doc "First".
h() -> 1.
-doc false.
h() -> 2.
"#,
            expect![[r#"
                function f/0: "First"
                function g/1: "Second"
            "#]],
        );
    }

    #[test]
    fn inactive_branches() {
        check(
            r#"
-module(main).
-ifdef(NOT_DEFINED).
-moduledoc "Inactive module doc".
-doc "Inactive doc".
-endif.
one() -> 1.
-ifndef(NOT_DEFINED).
-doc "Active doc".
-endif.
two() -> 2.
-doc "Skips the inactive definition".
-ifdef(NOT_DEFINED).
three() -> 3.
-endif.
four() -> 4.
"#,
            expect![[r#"
                function four/0: "Skips the inactive definition"
                function two/0: "Active doc"
            "#]],
        );
    }

    #[test]
    fn string_decoding() {
        check(
            r#"
-module(main).
-doc "Tab:\tend".
one() -> 1.
-doc """
  Triple-quoted,
    indented.
  """.
two() -> 2.
-doc \~"Sigil \"quoted\"".
three() -> 3.
-doc \~"""
  Verbatim \n
  """.
four() -> 4.
-doc("Adjacent" " " "literals").
five() -> 5.
-doc "  Trimmed  ".
six() -> 6.
"#,
            expect![[r#"
                function five/0: "Adjacent literals"
                function four/0: "Verbatim \\n"
                function one/0: "Tab:\tend"
                function six/0: "Trimmed"
                function three/0: "Sigil \"quoted\""
                function two/0: "Triple-quoted,\n  indented."
            "#]],
        );
    }

    #[test]
    fn macros() {
        check(
            r#"
-module(main).
-define(DOC, "From a macro").
-moduledoc ?DOC.
-doc ?DOC.
one() -> 1.
-doc "Prefix " ?DOC.
two() -> 2.
-doc "No macro".
three() -> 3.
"#,
            expect![[r#"
                function three/0: "No macro"
            "#]],
        );
    }

    #[test]
    fn edoc_comments() {
        check(
            r#"
%% @doc Old-style module doc.
-module(main).
%% @doc Old-style function doc.
one() -> 1.
-doc "Attribute doc".
%% @doc Old-style doc, not read.
two() -> 2.
"#,
            expect![[r#"
                function two/0: "Attribute doc"
            "#]],
        );
    }
}

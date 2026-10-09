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
//! `-doc`), following the attachment rules of OTP's `beam_doc`:
//!
//! - `-doc` attributes are accumulated until the next function, type or
//!   callback definition, which they then document. Other forms (e.g.
//!   `-spec` or `-export`) in between do not interrupt the accumulation.
//! - `-doc false.` and `-doc hidden.` hide the next definition.
//! - A doc whose first line is a call to the documented function with the
//!   matching arity provides the signature, and is removed from the text.
//! - Without a doc string, an `equiv` metadata entry produces a short
//!   "equivalent to" description.
//!
//! Doc attributes whose value is a macro call are not expanded, and are
//! ignored.

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
    fn module_and_function_docs() {
        check(
            r#"
-module(main).
-moduledoc "This is the module doc".
-export([one/0, two/0, three/0]).

%% @doc This is function one, with old style docs
one() -> 1.

-doc "This is function two".
two() -> 2.

-doc "This is function three".
% @doc Old style doc, ignored.
three() -> 3.
"#,
            expect![[r#"
                module: "This is the module doc"
                function three/0: "This is function three"
                function two/0: "This is function two"
            "#]],
        );
    }

    #[test]
    fn triple_quoted_and_sigil_strings() {
        check(
            r#"
-module(main).
-moduledoc """
  Module doc.

  With a second paragraph.
  """.

-doc \~"Sigil \"doc\"".
one() -> 1.

-doc \~"""
  Verbatim \n doc
  """.
two() -> 2.

-doc("Paren" " " "concatenated").
three() -> 3.
"#,
            expect![[r#"
                module: "Module doc.\n\nWith a second paragraph."
                function one/0: "Sigil \"doc\""
                function three/0: "Paren concatenated"
                function two/0: "Verbatim \\n doc"
            "#]],
        );
    }

    #[test]
    fn doc_skips_spec_and_attaches_to_next_definition() {
        check(
            r#"
-module(main).
-doc "Documented".
-spec one() -> ok.
one() -> ok.

two() -> ok.

-doc "Type doc".
-export_type([t/0]).
-type t() :: ok.

-doc "Callback doc".
-callback cb() -> ok.
three() -> ok.
"#,
            expect![[r#"
                function one/0: "Documented"
                type t/0: "Type doc"
            "#]],
        );
    }

    #[test]
    fn private_functions_are_documented() {
        check(
            r#"
-module(main).
-export([public/0]).
-doc "Public".
public() -> private().
-doc "Private".
private() -> ok.
"#,
            expect![[r#"
                function private/0: "Private"
                function public/0: "Public"
            "#]],
        );
    }

    #[test]
    fn hidden_docs() {
        check(
            r#"
-module(main).
-moduledoc false.
-doc false.
one() -> 1.
-doc hidden.
-type t() :: ok.
-doc "Visible".
two() -> 2.
"#,
            expect![[r#"
                function two/0: "Visible"
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
-doc "Has a doc".
-doc #{equiv => one/0}.
one(_) -> ok.
-doc(#{since => "1.0", equiv => two/1}).
two() -> two(ok).
"#,
            expect![[r#"
                function one/0: "equivalent to `one(ok)`"
                function one/1: "Has a doc"
                function two/0: "equivalent to `two/1`"
            "#]],
        );
    }

    #[test]
    fn signature_line_is_stripped() {
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
The arity does not match, so this line is kept.
""".
add(A, B, C) -> A + B + C.

-doc """
sub(A, B) and more
Not a call on its own.
""".
sub(A, B) -> A - B.

-doc "neg(A)".
neg(A) -> -A.
"#,
            expect![[r#"
                function add/2: "Adds A and B."
                function add/3: "add(A)\nThe arity does not match, so this line is kept."
                function neg/1: ""
                function sub/2: "sub(A, B) and more\nNot a call on its own."
            "#]],
        );
    }

    #[test]
    fn inactive_forms_are_ignored() {
        check(
            r#"
-module(main).
-ifdef(NOT_DEFINED).
-moduledoc "Inactive".
-doc "Inactive".
-endif.
one() -> 1.
"#,
            expect![""],
        );
    }

    #[test]
    fn no_docs() {
        check(
            r#"
-module(main).
-spec one() -> ok.
one() -> ok.
"#,
            expect![""],
        );
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Links to a module's or function's page under its app's docs URL.

use std::sync::Arc;

use elp_ide_db::elp_base_db::FileId;
use hir::Module;
use hir::Semantic;
use url::Url;

use crate::DocLink;
use crate::elp_ide_db::SymbolDefinition;

/// The page of `module`, if it has a `-moduledoc` and its app a docs URL.
pub fn module_doc_link(sema: &Semantic, module: &Module) -> Option<String> {
    if !module.has_moduledoc(sema.db) {
        return None;
    }
    let docs_url = docs_url(sema, module.file.file_id)?;
    let page = module_page(&docs_url, &module.name(sema.db).to_string())?;
    Some(page.into())
}

pub(crate) fn links(res: &mut Vec<DocLink>, sema: &Semantic, def: &SymbolDefinition) {
    match def {
        SymbolDefinition::Module(module) => {
            if let Some(uri) = module_doc_link(sema, module) {
                let title = module.name(sema.db).to_string();
                res.push(DocLink { title, uri });
            }
        }
        SymbolDefinition::Function(function_def) => {
            let file_id = function_def.file.file_id;
            if function_def.exported
                && function_def.has_doc_attribute()
                && let Some(docs_url) = docs_url(sema, file_id)
                && let Some(module_name) = sema.module_name(file_id)
                && let Some(mut page) = module_page(&docs_url, module_name.as_str())
            {
                let name = function_def.name.name();
                let arity = function_def.name.arity();
                page.set_fragment(Some(&format!("{name}/{arity}")));
                res.push(DocLink {
                    title: format!("{}:{name}/{arity}", module_name.as_str()),
                    uri: page.into(),
                });
            }
        }
        _ => (),
    }
}

fn docs_url(sema: &Semantic, file_id: FileId) -> Option<Arc<Url>> {
    sema.db.file_app(file_id)?.docs_url(sema.db).clone()
}

/// `<docs_url>/<module>.html`, whether or not `docs_url` ends with a slash.
fn module_page(docs_url: &Url, module: &str) -> Option<Url> {
    let mut page = docs_url.clone();
    page.path_segments_mut()
        .ok()?
        .pop_if_empty()
        .push(&format!("{module}.html"));
    Some(page)
}

#[cfg(test)]
mod tests {
    use crate::doc_links::tests::check_links;

    #[test]
    fn a_documented_otp_module_links_to_its_erlang_org_page() {
        check_links(
            r#"
 //- /opt/lib/stdlib-3.17/src/lists.erl otp_app:/opt/lib/stdlib-3.17
 -module(lists).
 -moduledoc "List processing functions.".
 -export([reverse/1]).
 reverse([]) -> [].
 //- /src/two.erl
 -module(two).
 a() ->
   list~s:reverse([]).
         "#,
            vec!["https://erlang.org/doc/man/lists.html"],
        )
    }

    #[test]
    fn a_documented_exported_otp_function_links_to_its_erlang_org_page() {
        check_links(
            r#"
 //- /opt/lib/stdlib-3.17/src/lists.erl otp_app:/opt/lib/stdlib-3.17
 -module(lists).
 -export([reverse/1]).
 -doc "Reverses a list.".
 reverse([]) -> [].
 //- /src/two.erl
 -module(two).
 a() ->
   lists:rev~erse([]).
         "#,
            vec!["https://erlang.org/doc/man/lists.html#reverse/1"],
        )
    }

    #[test]
    fn an_undocumented_module_has_no_link() {
        check_links(
            r#"
 //- /opt/lib/stdlib-3.17/src/lists.erl otp_app:/opt/lib/stdlib-3.17
 -module(lists).
 -export([reverse/1]).
 reverse([]) -> [].
 //- /src/two.erl
 -module(two).
 a() ->
   list~s:reverse([]).
         "#,
            vec![],
        )
    }

    #[test]
    fn an_undocumented_function_has_no_link() {
        check_links(
            r#"
 //- /opt/lib/stdlib-3.17/src/lists.erl otp_app:/opt/lib/stdlib-3.17
 -module(lists).
 -moduledoc "List processing functions.".
 -export([reverse/1]).
 reverse([]) -> [].
 //- /src/two.erl
 -module(two).
 a() ->
   lists:rev~erse([]).
         "#,
            vec![],
        )
    }

    #[test]
    fn a_documented_function_that_is_not_exported_has_no_link() {
        check_links(
            r#"
 //- /src/one.erl docs_url:https://example.com/docs
 -module(one).
 -doc "Says hello.".
 hello() -> ok.
 a() -> hel~lo().
         "#,
            vec![],
        )
    }

    #[test]
    fn a_module_links_under_its_apps_docs_url() {
        check_links(
            r#"
 //- /src/one.erl docs_url:https://example.com/docs
 -module(one).
 -moduledoc "Says hello.".
 -export([hello/0]).
 -doc "Says hello.".
 hello() -> ok.
 //- /src/two.erl
 -module(two).
 a() ->
   on~e:hello().
         "#,
            vec!["https://example.com/docs/one.html"],
        )
    }

    #[test]
    fn a_function_links_under_its_apps_docs_url() {
        check_links(
            r#"
 //- /src/one.erl docs_url:https://example.com/docs
 -module(one).
 -export([hello/0]).
 -doc "Says hello.".
 hello() -> ok.
 //- /src/two.erl
 -module(two).
 a() ->
   one:hel~lo().
         "#,
            vec!["https://example.com/docs/one.html#hello/0"],
        )
    }

    #[test]
    fn a_docs_url_with_a_trailing_slash_links_without_a_double_slash() {
        check_links(
            r#"
 //- /src/one.erl docs_url:https://example.com/docs/
 -module(one).
 -moduledoc "Says hello.".
 -export([hello/0]).
 hello() -> ok.
 //- /src/two.erl
 -module(two).
 a() ->
   on~e:hello().
         "#,
            vec!["https://example.com/docs/one.html"],
        )
    }

    #[test]
    fn an_app_without_a_docs_url_has_no_links() {
        check_links(
            r#"
 //- /src/one.erl
 -module(one).
 -moduledoc "Says hello.".
 -export([hello/0]).
 hello() -> ok.
 //- /src/two.erl
 -module(two).
 a() ->
   on~e:hello().
         "#,
            vec![],
        )
    }
}

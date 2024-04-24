/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use hir::Semantic;

use crate::elp_ide_db::SymbolDefinition;
use crate::DocLink;

const OTP_BASE_URL: &str = "https://erlang.org";

pub(crate) fn links(res: &mut Vec<DocLink>, sema: &Semantic, def: &SymbolDefinition) {
    match def {
        SymbolDefinition::Module(module) => {
            if module.is_in_otp(sema.db) {
                let name = module.name(sema.db);
                let uri = format!("{}/doc/man/{}.html", OTP_BASE_URL, name);
                let link = DocLink {
                    title: name.to_string(),
                    uri,
                };
                res.push(link);
            }
        }
        SymbolDefinition::Function(function_def) => {
            if function_def.is_in_otp(sema.db) {
                if let Some(module_name) = sema.module_name(function_def.file.file_id) {
                    let module_name = module_name.to_string();
                    let function_name = function_def.name.name();
                    let function_arity = function_def.name.arity();
                    let title = format!("{module_name}:{function_name}/{function_arity}");
                    let uri = format!(
                        "{}/doc/man/{}.html#{}-{}",
                        OTP_BASE_URL, module_name, function_name, function_arity
                    );
                    let link = DocLink { title, uri };
                    res.push(link);
                }
            }
        }
        _ => (),
    }
}

#[cfg(test)]
mod tests {
    use crate::doc_links::tests::check_links;

    #[test]
    fn otp_module_doc_links() {
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
            vec!["https://erlang.org/doc/man/lists.html"],
        )
    }

    #[test]
    fn non_otp_module_doc_links() {
        check_links(
            r#"
 //- /src/one.erl
 -module(one).
 -export([reverse/1]).
 reverse([]) -> [].

 //- /src/two.erl
 -module(two).
 a() ->
   on~e:reverse([]).
         "#,
            vec![],
        )
    }

    #[test]
    fn otp_function_doc_links() {
        check_links(
            r#"
 //- /opt/lib/stdlib-3.17/src/lists.erl otp_app:/opt/lib/stdlib-3.17
 -module(lists).
 -export([reverse/1]).
 reverse([]) -> [].

 //- /src/two.erl
 -module(two).
 a() ->
   lists:rev~erse([]).
         "#,
            vec!["https://erlang.org/doc/man/lists.html#reverse-1"],
        )
    }

    #[test]
    fn non_otp_function_doc_links() {
        check_links(
            r#"
 //- /src/one.erl
 -module(one).
 -export([reverse/1]).
 reverse([]) -> [].

 //- /src/two.erl
 -module(two).
 a() ->
   one:rev~erse([]).
         "#,
            vec![],
        )
    }
}

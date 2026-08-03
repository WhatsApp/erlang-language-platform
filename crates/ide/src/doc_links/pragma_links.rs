/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::elp_base_db::FilePosition;
use elp_syntax::SyntaxNode;
use elp_syntax::algo;
use elp_syntax::ast;
use hir::known;

use crate::DocLink;

const COMPILE_BASE_URL: &str = "https://www.erlang.org/doc/apps/compiler/compile.html#:~:text=";
// N.B. we prepend a W to the option name so we go to the documentation in the "Warnings" section
const DIALYZER_BASE_URL: &str = "https://www.erlang.org/doc/apps/dialyzer/dialyzer.html#:~:text=W";

pub(crate) fn links(res: &mut Vec<DocLink>, node: &SyntaxNode, position: &FilePosition) {
    if let Some(atom) = algo::find_node_at_offset::<ast::Atom>(node, position.offset)
        && let Some(atom_text) = atom.text()
    {
        if algo::find_node_at_offset::<ast::CompileOptionsAttribute>(node, position.offset)
            .is_some()
        {
            let uri = format!("{COMPILE_BASE_URL}{atom_text}");
            let title = format!("Compile option ({atom_text})");
            res.push(DocLink { title, uri });
        } else if let Some(attr) =
            algo::find_node_at_offset::<ast::WildAttribute>(node, position.offset)
            && is_wild_attribute_named(&attr, *known::dialyzer) == Some(true)
        {
            let uri = format!("{DIALYZER_BASE_URL}{atom_text}");
            let title = format!("Dialyzer option ({atom_text})");
            res.push(DocLink { title, uri });
        }
    }
}

fn is_wild_attribute_named(attr: &ast::WildAttribute, name: hir::Name) -> Option<bool> {
    let attr_name = attr.name()?.name()?.to_string();
    Some(attr_name == name.to_string())
}

#[cfg(test)]
mod tests {
    use crate::doc_links::tests::check_links;

    #[test]
    fn compile_option_simple() {
        check_links(
            r#"
//- /src/foo.erl
-module(foo).
-compile(warn_missing_spec_a~ll).
         "#,
            vec![
                "https://www.erlang.org/doc/apps/compiler/compile.html#:~:text=warn_missing_spec_all",
            ],
        )
    }

    #[test]
    fn compile_option_in_list() {
        check_links(
            r#"
//- /src/foo.erl
-module(foo).
-compile([warn_missing_spec_a~ll]).
         "#,
            vec![
                "https://www.erlang.org/doc/apps/compiler/compile.html#:~:text=warn_missing_spec_all",
            ],
        )
    }

    #[test]
    fn dialyzer_option_simple() {
        check_links(
            r#"
//- /src/foo.erl
-module(foo).
-dialyzer(no_improper_lis~ts).
         "#,
            vec![
                "https://www.erlang.org/doc/apps/dialyzer/dialyzer.html#:~:text=Wno_improper_lists",
            ],
        )
    }

    #[test]
    fn dialyzer_option_in_tuple() {
        check_links(
            r#"
//- /src/foo.erl
-module(foo).
f() -> ok.
-dialyzer({nowarn_functi~on, f/0}).
         "#,
            vec!["https://www.erlang.org/doc/apps/dialyzer/dialyzer.html#:~:text=Wnowarn_function"],
        )
    }

    #[test]
    fn dialyzer_option_complex() {
        check_links(
            r#"
//- /src/foo.erl
-module(foo).
-dialyzer([{nowar~n_function, [f/0]}, no_improper_lists]).
f() -> ok.
         "#,
            vec!["https://www.erlang.org/doc/apps/dialyzer/dialyzer.html#:~:text=Wnowarn_function"],
        );
        check_links(
            r#"
//- /src/foo.erl
-module(foo).
-dialyzer([{nowarn_function, [f/0]}, no_im~proper_lists]).
f() -> ok.
         "#,
            vec![
                "https://www.erlang.org/doc/apps/dialyzer/dialyzer.html#:~:text=Wno_improper_lists",
            ],
        )
    }

    #[test]
    fn cursor_not_on_atom() {
        check_links(
            r#"
//- /src/foo.erl
-module(foo).
-compile~(warn_missing_spec_all).
         "#,
            vec![],
        )
    }
}

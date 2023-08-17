/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::min;

use elp_ide_db::assists::AssistId;
use elp_ide_db::assists::AssistKind;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::TextSize;

use crate::helpers::prev_form_nodes;
use crate::AssistContext;
use crate::Assists;

const PRAGMA: &str = "@format";
// The following prefix is used to prevent being recognized as an EDoc tag.
const PRAGMA_PREFIX: &str = "%% % ";

// Assist: add_format
//
// Adds a @format pragma above the module attribute line, if the file doesn't contain a pragma already.
// The pragma is used to indicate that the current file is formatted using the
// default formatter (erlfmt).
//
// ```
// -module(my_module).
// ```
// ->
// ```
// %% % @format
// -module(my_module).
// ```
pub(crate) fn add_format(acc: &mut Assists, ctx: &AssistContext) -> Option<()> {
    let module = ctx.find_node_at_offset::<ast::ModuleAttribute>()?;
    let already_has_format = prev_form_nodes(module.syntax())
        .filter_map(|comment| {
            let text = comment.text();
            let at = text.find_char('@')?;
            Some((text, at))
        })
        .any(|(text, at)| text.slice(at..min(text.len(), at + TextSize::of(PRAGMA))) == PRAGMA);
    if already_has_format {
        return None;
    }

    let insert = module.syntax().text_range().start();
    let target = module.syntax().text_range();

    acc.add(
        AssistId("add_format", AssistKind::Generate),
        "Add @format pragma",
        target,
        None,
        |builder| {
            let text = format!("{}{}\n", PRAGMA_PREFIX, PRAGMA);
            builder.edit_file(ctx.frange.file_id);
            builder.insert(insert, text)
        },
    )
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::tests::*;

    #[test]
    fn test_add_format_pragma() {
        check_assist(
            add_format,
            "Add @format pragma",
            r#"
%% LICENSE
%% LICENSE
%% LICENSE
%%
-modu~le(my_module).
"#,
            expect![[r#"
                %% LICENSE
                %% LICENSE
                %% LICENSE
                %%
                %% % @format
                -module(my_module).
            "#]],
        )
    }

    #[test]
    fn test_module_already_has_format_pragma() {
        check_assist_not_applicable(
            add_format,
            r#"
%% LICENSE
%% LICENSE
%% LICENSE
%%
%% % @format
%%
-modu~le(my_module).
"#,
        );
    }

    #[test]
    fn test_module_with_edoc() {
        check_assist(
            add_format,
            "Add @format pragma",
            r#"
%%%-------------------------------------------------------------------
%%% @doc
%%% My custom server
-mo~dule(custom_server).
"#,
            expect![[r#"
                %%%-------------------------------------------------------------------
                %%% @doc
                %%% My custom server
                %% % @format
                -module(custom_server).
            "#]],
        )
    }
}

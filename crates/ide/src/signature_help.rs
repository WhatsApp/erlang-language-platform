/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This module provides primitives for showing type and function parameter information when editing
//! a call or use-site.

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::find_best_token;
use elp_ide_db::RootDatabase;
use elp_syntax::algo;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use elp_syntax::TextSize;
use fxhash::FxHashMap;
use hir::CallTarget;
use hir::FunctionDef;
use hir::InFile;
use hir::Name;
use hir::Semantic;
use itertools::Itertools;
use stdx::format_to;

use crate::handlers::get_docs::get_doc_at_position;

/// Contains information about an item signature as seen from a use site.
///
/// This includes the "active parameter", which is the parameter whose value is currently being
/// edited.
#[derive(Debug)]
pub struct SignatureHelp {
    pub function_doc: Option<String>,
    pub parameters_doc: FxHashMap<String, String>,
    pub signature: String,
    pub active_parameter: Option<usize>,
    parameters: Vec<TextRange>,
}

impl SignatureHelp {
    pub fn parameter_labels(&self) -> impl Iterator<Item = &str> + '_ {
        self.parameters.iter().map(move |&it| &self.signature[it])
    }

    pub fn parameter_ranges(&self) -> &[TextRange] {
        &self.parameters
    }

    fn push_param(&mut self, param: &str) {
        if !self.signature.ends_with('(') {
            self.signature.push_str(", ");
        }
        let start = TextSize::of(&self.signature);
        self.signature.push_str(param);
        let end = TextSize::of(&self.signature);
        // Temporary for T148094436
        let _pctx = stdx::panic_context::enter(format!("\nSignatureHelp::push_param"));
        self.parameters.push(TextRange::new(start, end))
    }
}

/// Computes parameter information for the given position.
pub(crate) fn signature_help(
    db: &RootDatabase,
    position: FilePosition,
) -> Option<(Vec<SignatureHelp>, Option<usize>)> {
    let sema = Semantic::new(db);
    let source_file = sema.parse(position.file_id);
    let syntax = source_file.value.syntax();
    let token = find_best_token(&sema, position)?.value;
    let call = algo::find_node_at_offset::<ast::Call>(syntax, position.offset)?;
    let call_expr = sema.to_expr(InFile::new(
        position.file_id,
        &ast::Expr::Call(call.clone()),
    ))?;
    let active_parameter = match call.args() {
        Some(args) => {
            let param = args
                .args()
                .take_while(|arg| arg.syntax().text_range().end() <= token.text_range().start())
                .count();
            Some(param)
        }
        None => None,
    };

    let mut res = Vec::new();

    match &call_expr[call_expr.value] {
        hir::Expr::Call { target, args } => {
            let arity = args.len() as u32;
            match target {
                CallTarget::Local { name } => {
                    let fun_atom = &call_expr[name.clone()].as_atom()?;
                    let fun_name = sema.db.lookup_atom(*fun_atom);
                    signature_help_for_call(
                        &mut res,
                        sema,
                        db,
                        position.file_id,
                        None,
                        fun_name,
                        arity,
                        active_parameter,
                    )
                }
                CallTarget::Remote { module, name } => {
                    let module_atom = &call_expr[module.clone()].as_atom()?;
                    let module_name = sema.db.lookup_atom(*module_atom);
                    let fun_atom = &call_expr[name.clone()].as_atom()?;
                    let fun_name = sema.db.lookup_atom(*fun_atom);
                    let module =
                        sema.resolve_module_name(position.file_id, module_name.as_str())?;
                    signature_help_for_call(
                        &mut res,
                        sema,
                        db,
                        module.file.file_id,
                        Some(module_name),
                        fun_name,
                        arity,
                        active_parameter,
                    )
                }
            }
        }
        _ => (),
    };

    Some((res, active_parameter))
}

fn signature_help_for_call(
    res: &mut Vec<SignatureHelp>,
    sema: Semantic,
    db: &RootDatabase,
    file_id: FileId,
    module_name: Option<Name>,
    fun_name: Name,
    arity: u32,
    active_parameter: Option<usize>,
) {
    let def_map = sema.def_map(file_id);
    let functions = def_map
        .get_functions_in_scope()
        .filter(|&name_arity| {
            *name_arity.name() == fun_name
                && name_arity.arity() >= arity
                && (module_name.is_none() || def_map.is_function_exported(name_arity))
        })
        .sorted();
    for name_arity in functions {
        match def_map.get_function(name_arity) {
            Some(def) => {
                let help = build_signature_help(
                    db,
                    &sema,
                    file_id,
                    def,
                    active_parameter,
                    module_name.clone(),
                    &fun_name,
                );
                res.push(help);
            }
            None => {
                // Function could be imported
                if let Some(module_name) = def_map.get_imports().get(name_arity) {
                    if let Some(module) = sema.resolve_module_name(file_id, module_name) {
                        let def_map = sema.def_map(module.file.file_id);
                        if let Some(def) = def_map.get_function(name_arity) {
                            let help = build_signature_help(
                                db,
                                &sema,
                                module.file.file_id,
                                def,
                                active_parameter,
                                Some(module_name.clone()),
                                &fun_name,
                            );
                            res.push(help)
                        }
                    }
                }
            }
        }
    }
}

fn build_signature_help(
    db: &RootDatabase,
    sema: &Semantic,
    file_id: FileId,
    def: &FunctionDef,
    active_parameter: Option<usize>,
    module_name: Option<Name>,
    fun_name: &Name,
) -> SignatureHelp {
    let function_doc = get_function_doc(db, &sema, file_id, def);
    let parameters_doc = get_parameters_doc(db, def);
    let mut help = SignatureHelp {
        function_doc,
        parameters_doc,
        signature: String::new(),
        parameters: vec![],
        active_parameter,
    };
    match &module_name {
        Some(m) => format_to!(help.signature, "{m}:{fun_name}("),
        None => format_to!(help.signature, "{fun_name}("),
    }
    let parameters = &def.function.param_names;
    for parameter in parameters {
        help.push_param(parameter);
    }
    help.signature.push(')');
    help
}

fn get_parameters_doc(db: &RootDatabase, def: &FunctionDef) -> FxHashMap<String, String> {
    match def.edoc_comments(db) {
        Some(edoc_header) => edoc_header.params(),
        None => FxHashMap::default(),
    }
}

fn get_function_doc(
    db: &RootDatabase,
    sema: &Semantic,
    file_id: FileId,
    def: &FunctionDef,
) -> Option<String> {
    let position = FilePosition {
        file_id,
        offset: def.source(sema.db.upcast()).syntax().text_range().start(),
    };
    let (doc, _file_range) = get_doc_at_position(db, position)?;
    Some(doc.markdown_text().to_string())
}

#[cfg(test)]
mod tests {
    use std::iter;

    use elp_ide_db::elp_base_db::fixture::WithFixture;
    use expect_test::expect;
    use expect_test::Expect;
    use stdx::format_to;

    use crate::RootDatabase;

    fn check(fixture: &str, expect: Expect) {
        let (db, position) = RootDatabase::with_position(fixture);
        let sig_help = crate::signature_help::signature_help(&db, position);
        let actual = match sig_help {
            Some((sig_help, _active_parameter)) => {
                let mut rendered = String::new();
                for sh in sig_help {
                    if let Some(spec) = &sh.function_doc {
                        format_to!(rendered, "{}\n------\n", spec.as_str());
                    }
                    format_to!(rendered, "{}\n", sh.signature);
                    let mut offset = 0;
                    for (i, range) in sh.parameter_ranges().iter().enumerate() {
                        let is_active = sh.active_parameter == Some(i);

                        let start = u32::from(range.start());
                        let gap = start.checked_sub(offset).unwrap_or_else(|| {
                            panic!("parameter ranges out of order: {:?}", sh.parameter_ranges())
                        });
                        rendered.extend(iter::repeat(' ').take(gap as usize));
                        let param_text = &sh.signature[*range];
                        let width = param_text.chars().count();
                        let marker = if is_active { '^' } else { '-' };
                        rendered.extend(iter::repeat(marker).take(width));
                        offset += gap + u32::from(range.len());
                    }
                    if !sh.parameter_ranges().is_empty() {
                        format_to!(rendered, "\n");
                    }
                    if !sh.parameters_doc.is_empty() {
                        format_to!(rendered, "------\n");
                        for (param_name, param_desc) in &sh.parameters_doc {
                            format_to!(rendered, "{param_name}: {param_desc}\n");
                        }
                    }
                    format_to!(rendered, "======\n");
                }
                rendered
            }
            None => String::new(),
        };
        expect.assert_eq(&actual);
    }

    #[test]
    fn test_fn_signature_local_two_args() {
        check(
            r#"
-module(main).

-spec add(integer(), integer()) -> integer().
add(This, That) ->
  add(This, That, 0).

-spec add(integer(), integer(), integer()) -> integer().
add(This, That, Extra) ->
  This + That + Extra.

main() ->
  add(~, That).
"#,
            expect![[r#"
                ```erlang
                -spec add(integer(), integer()) -> integer().
                ```
                ------
                add(This, That)
                    ^^^^  ----
                ======
                ```erlang
                -spec add(integer(), integer(), integer()) -> integer().
                ```
                ------
                add(This, That, Extra)
                    ^^^^  ----  -----
                ======
            "#]],
        );
        check(
            r#"
-module(main).

-spec add(integer(), integer()) -> integer().
add(This, That) ->
  add(This, That, 0).

-spec add(integer(), integer(), integer()) -> integer().
add(This, That, Extra) ->
  This + That + Extra.

main() ->
  add(This~).
"#,
            expect![[r#"
                ```erlang
                -spec add(integer(), integer()) -> integer().
                ```
                ------
                add(This, That)
                    ^^^^  ----
                ======
                ```erlang
                -spec add(integer(), integer(), integer()) -> integer().
                ```
                ------
                add(This, That, Extra)
                    ^^^^  ----  -----
                ======
            "#]],
        );
        check(
            r#"
-module(main).

-spec add(integer(), integer()) -> integer().
add(This, That) ->
  add(This, That, 0).

-spec add(integer(), integer(), integer()) -> integer().
add(This, That, Extra) ->
  This + That + Extra.

main() ->
  add(This, ~).
"#,
            expect![[r#"
                ```erlang
                -spec add(integer(), integer()) -> integer().
                ```
                ------
                add(This, That)
                    ----  ^^^^
                ======
                ```erlang
                -spec add(integer(), integer(), integer()) -> integer().
                ```
                ------
                add(This, That, Extra)
                    ----  ^^^^  -----
                ======
            "#]],
        );
    }

    #[test]
    fn test_fn_signature_remote_two_args() {
        check(
            r#"
//- /one.erl
-module(one).

-compile(export_all).

-spec add(integer(), integer()) -> integer().
add(This, That) ->
  add(This, That, 0).

-spec add(integer(), integer(), integer()) -> integer().
add(This, That, Extra) ->
  This + That + Extra.

//- /two.erl
-module(two).

main() ->
  one:add(~, That).
"#,
            expect![[r#"
                ```erlang
                -spec add(integer(), integer()) -> integer().
                ```
                ------
                one:add(This, That)
                        ^^^^  ----
                ======
                ```erlang
                -spec add(integer(), integer(), integer()) -> integer().
                ```
                ------
                one:add(This, That, Extra)
                        ^^^^  ----  -----
                ======
            "#]],
        );
        check(
            r#"
//- /one.erl
-module(one).

-compile(export_all).

-spec add(integer(), integer()) -> integer().
add(This, That) ->
  add(This, That, 0).

-spec add(integer(), integer(), integer()) -> integer().
add(This, That, Extra) ->
  This + That + Extra.

//- /two.erl
-module(two).

main() ->
  one:add(This~).
"#,
            expect![[r#"
                ```erlang
                -spec add(integer(), integer()) -> integer().
                ```
                ------
                one:add(This, That)
                        ^^^^  ----
                ======
                ```erlang
                -spec add(integer(), integer(), integer()) -> integer().
                ```
                ------
                one:add(This, That, Extra)
                        ^^^^  ----  -----
                ======
            "#]],
        );
        check(
            r#"
//- /one.erl
-module(one).

-compile(export_all).

-spec add(integer(), integer()) -> integer().
add(This, That) ->
  add(This, That, 0).

-spec add(integer(), integer(), integer()) -> integer().
add(This, That, Extra) ->
  This + That + Extra.

//- /two.erl
-module(two).

main() ->
  one:add(This, ~).
"#,
            expect![[r#"
                ```erlang
                -spec add(integer(), integer()) -> integer().
                ```
                ------
                one:add(This, That)
                        ----  ^^^^
                ======
                ```erlang
                -spec add(integer(), integer(), integer()) -> integer().
                ```
                ------
                one:add(This, That, Extra)
                        ----  ^^^^  -----
                ======
            "#]],
        );
    }

    // Due to the way the current grammar currently works, this is
    // currently not returning any results since the cursor is not
    // identified as part of the EXPR_ARGS.
    // In practice, this should not be an issue since VS Code
    // auto-completes parentheses.
    #[test]
    fn test_fn_signature_unclosed_call() {
        check(
            r#"
-module(main).

-compile(export_all).

-spec add(integer(), integer()) -> integer().
add(This, That) ->
  add(This, That, 0).

-spec add(integer(), integer(), integer()) -> integer().
add(This, That, Extra) ->
  This + That + Extra.

main() ->
  main:add(~
"#,
            expect![""],
        );
    }

    #[test]
    fn test_fn_signature_doc() {
        check(
            r#"
-module(main).

-compile(export_all).

%% @doc
%% Add This to That
%% @param This The first thing
%% @param That The second thing
%% @returns The sum of This and That plus 0
%% @end
-spec add(integer(), integer()) -> integer().
add(This, That) ->
  add(This, That, 0).

%% @doc
%% Add This to That, including an extra
%% @param This The first thing
%% @param That The second thing
%% @param Extra Something more
%% @returns The sum of This and That plus the Extra
%% @end
-spec add(integer(), integer(), integer()) -> integer().
add(This, That, Extra) ->
  This + That + Extra.

main() ->
  main:add(This, ~)
"#,
            expect![[r#"
                ```erlang
                -spec add(integer(), integer()) -> integer().
                ```
                ------
                main:add(This, That)
                         ----  ^^^^
                ------
                That: The second thing
                This: The first thing
                ======
                ```erlang
                -spec add(integer(), integer(), integer()) -> integer().
                ```
                ------
                main:add(This, That, Extra)
                         ----  ^^^^  -----
                ------
                Extra: Something more
                That: The second thing
                This: The first thing
                ======
            "#]],
        );
    }

    #[test]
    fn test_fn_signature_local_imported() {
        check(
            r#"
//- /one.erl
-module(one).
-compile(export_all).

-spec add(integer(), integer()) -> integer().
add(This, That) ->
  add(This, That, 0).

-spec add(integer(), integer(), integer()) -> integer().
add(This, That, Extra) ->
  This + That + Extra.

//- /two.erl
-module(two).
-import(one, [add/2, add/3]).
main() ->
  add(~, That).
"#,
            expect![[r#"
                ```erlang
                -spec add(integer(), integer()) -> integer().
                ```
                ------
                one:add(This, That)
                        ^^^^  ----
                ======
                ```erlang
                -spec add(integer(), integer(), integer()) -> integer().
                ```
                ------
                one:add(This, That, Extra)
                        ^^^^  ----  -----
                ======
            "#]],
        );
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::FilePosition;
use elp_ide_db::FxIndexMap;
use elp_ide_db::RootDatabase;
use elp_syntax::algo;
use elp_syntax::ast::{self};
use elp_syntax::AstNode;
use elp_syntax::SmolStr;
use elp_syntax::TextRange;
use hir::AnyExpr;
use hir::Expr;
use hir::InFile;
use hir::Semantic;

use crate::handlers::goto_definition;
use crate::handlers::references;
use crate::navigation_target::ToNav;
use crate::NavigationTarget;
use crate::RangeInfo;

#[derive(Debug, Clone)]
pub struct CallItem {
    pub target: NavigationTarget,
    pub ranges: Vec<TextRange>,
}

pub(crate) fn call_hierarchy_prepare(
    db: &RootDatabase,
    position: FilePosition,
) -> Option<RangeInfo<Vec<NavigationTarget>>> {
    goto_definition::goto_definition(db, position)
}

pub(crate) fn incoming_calls(db: &RootDatabase, position: FilePosition) -> Option<Vec<CallItem>> {
    let sema = Semantic::new(db);
    let mut calls = CallLocations::default();
    let search_result = references::find_all_refs(&sema, position);
    let references = search_result?.first()?.references.clone();

    for (file_id, ranges) in references {
        let source_file = sema.parse(file_id);
        let syntax = source_file.value.syntax();
        let form_list = sema.form_list(file_id);

        for range in ranges {
            if let Some(call) = algo::find_node_at_offset::<ast::Call>(syntax, range.start()) {
                let enclosing_function_id = sema.find_enclosing_function(file_id, call.syntax())?;
                let enclosing_function_name = &form_list[enclosing_function_id].name;
                let def_map = sema.def_map(file_id);
                let enclosing_function_def = def_map.get_function(enclosing_function_name)?;
                let mut enclosing_function_nav = enclosing_function_def.to_nav(db);
                if file_id != position.file_id {
                    if let Some(module_name) = sema.module_name(file_id) {
                        enclosing_function_nav.name = SmolStr::new(format!(
                            "{}:{}",
                            module_name.as_str(),
                            enclosing_function_nav.name
                        ))
                    }
                }
                calls.add(enclosing_function_nav, range);
            }
        }
    }

    Some(calls.into_items())
}

pub(crate) fn outgoing_calls(db: &RootDatabase, position: FilePosition) -> Option<Vec<CallItem>> {
    let sema = Semantic::new(db);
    let mut calls = CallLocations::default();
    let file_id = position.file_id;
    let source_file = sema.parse(file_id);
    let syntax = source_file.value.syntax();
    if let Some(function) = algo::find_node_at_offset::<ast::FunDecl>(syntax, position.offset) {
        let function_id_idx = sema.find_enclosing_function(file_id, function.syntax())?;
        let function_id = InFile::new(file_id, function_id_idx);
        let function_body = sema.to_function_body(function_id);
        sema.fold_function(function_id, (), &mut |acc, _clause_id, ctx| {
            if let AnyExpr::Expr(Expr::Call { target, args }) = &ctx.item {
                let arity = args.len() as u32;
                let body = &function_body.body();
                if let Some(call_def) = target.resolve_call(arity, &sema, file_id, body) {
                    let mut nav = call_def.to_nav(db);
                    if let Some(label) = target.label(arity, &sema, body) {
                        nav.name = label
                    }
                    if let Some(expr) = &function_body.get_body_map(db).any(ctx.item_id) {
                        if let Some(node) = expr.to_node(&source_file) {
                            if let Some(call) = algo::find_node_at_offset::<ast::Call>(
                                node.syntax(),
                                node.syntax().text_range().start(),
                            ) {
                                if let Some(expr) = call.expr() {
                                    let range = expr.syntax().text_range();
                                    calls.add(nav.clone(), range);
                                }
                            }
                        }
                    }
                }
            }
            acc
        })
    }
    Some(calls.into_items())
}

#[derive(Default)]
struct CallLocations {
    funcs: FxIndexMap<NavigationTarget, Vec<TextRange>>,
}

impl CallLocations {
    fn add(&mut self, target: NavigationTarget, range: TextRange) {
        self.funcs.entry(target).or_default().push(range);
    }

    fn into_items(self) -> Vec<CallItem> {
        self.funcs
            .into_iter()
            .map(|(target, ranges)| CallItem { target, ranges })
            .collect()
    }
}

#[cfg(test)]
mod tests {

    use crate::tests::check_call_hierarchy;

    #[test]
    fn test_call_hierarchy_on_ref() {
        check_call_hierarchy(
            r#"
   callee() ->
%% ^^^^^^
     ok.
   caller() ->
     cal~lee().
   "#,
            r#"
   cal~lee() ->
     ok.
   caller() ->
%% ^^^^^^ from: caller/0
     callee().
  %% ^^^^^^ from_range: caller/0
   "#,
            r#"
   cal~lee() ->
     ok.
   caller() ->
     callee().
   "#,
        );
    }

    #[test]
    fn test_call_hierarchy_on_def() {
        check_call_hierarchy(
            r#"
    call~ee() ->
 %% ^^^^^^
      ok.
    caller() ->
      callee().
    "#,
            r#"
    call~ee() ->
      ok.
    caller() ->
 %% ^^^^^^ from: caller/0
      callee().
   %% ^^^^^^ from_range: caller/0
    "#,
            r#"
    call~ee() ->
      ok.
    caller() ->
      callee().
      "#,
        )
    }

    #[test]
    fn test_call_hierarchy_multiple_calls_same_function() {
        check_call_hierarchy(
            r#"
    callee() -> ok.
 %% ^^^^^^
    caller() ->
      call~ee(),
      callee().
    "#,
            r#"
    cal~lee() -> ok.
    caller() ->
 %% ^^^^^^ from: caller/0
      callee(),
   %% ^^^^^^ from_range: caller/0
      callee().
   %% ^^^^^^ from_range: caller/0
    "#,
            r#"
    cal~lee() -> ok.
    caller() ->
      callee(),
      callee().
    "#,
        );
    }

    #[test]
    fn test_call_hierarchy_multiple_calls_different_function() {
        check_call_hierarchy(
            r#"
    callee() -> ok.
 %% ^^^^^^
    caller1() ->
      call~ee().
    caller2() ->
      callee().
    "#,
            r#"
    cal~lee() -> ok.
    caller1() ->
 %% ^^^^^^^ from: caller1/0
      callee().
   %% ^^^^^^ from_range: caller1/0
    caller2() ->
 %% ^^^^^^^ from: caller2/0
      callee().
   %% ^^^^^^ from_range: caller2/0
    "#,
            r#"
    cal~lee() -> ok.
    caller1() ->
      callee().
    caller2() ->
      callee().
    "#,
        );
    }

    #[test]
    fn test_call_hierarchy_recursive() {
        check_call_hierarchy(
            r#"
    fact(1) -> 1;
 %% ^^^^
    fact(N) -> N * fa~ct(N-1).
    "#,
            r#"
    f~act(1) -> 1;
 %% ^^^^ from: fact/1
        fact(N) -> N * fact(N-1).
                    %% ^^^^ from_range: fact/1
    "#,
            r#"
    f~act(1) -> 1;
 %% ^^^^ to: fact/1
    fact(N) -> N * fact(N-1).
                %% ^^^^ from_range: fact/1
      "#,
        )
    }

    #[test]
    fn test_call_hierarchy_different_files() {
        check_call_hierarchy(
            r#"
 //- /src/a.erl
    -module(a).
    caller() ->
      b:calle~e().
 //- /src/b.erl
    -module(b).
    -export([callee/0]).
    callee() -> ok.
 %% ^^^^^^
    "#,
            r#"
 //- /src/a.erl
    -module(a).
    caller() ->
 %% ^^^^^^ from: a:caller/0
      b:callee().
 %%     ^^^^^^ from_range: a:caller/0
 //- /src/b.erl
    -module(b).
    -export([callee/0]).
    cal~lee() -> ok.
    "#,
            r#"
 //- /src/a.erl
    -module(a).
    caller() ->
      b:callee().
 //- /src/b.erl
    -module(b).
    -export([callee/0]).
    cal~lee() -> ok.
    "#,
        );
    }

    #[test]
    fn test_call_hierarchy_outgoing() {
        check_call_hierarchy(
            r#"
    -module(main).
    callee() ->
      ok.
    call~er() ->
 %% ^^^^^^
      callee(),
      callee().
    "#,
            r#"
    -module(main).
    callee() ->
      ok.
    call~er() ->
      callee(),
      callee().
    "#,
            r#"
    -module(main).
    callee() ->
 %% ^^^^^^ to: callee/0
      ok.
    call~er() ->
      callee(),
 %%   ^^^^^^ from_range: callee/0
      callee().
 %%   ^^^^^^ from_range: callee/0
    "#,
        );
    }

    #[test]
    fn test_call_hierarchy_outgoing_fully_qualified() {
        check_call_hierarchy(
            r#"
 //- /src/a.erl
    -module(a).
    cal~ler() ->
 %% ^^^^^^
      b:callee().
 //- /src/b.erl
    -module(b).
    -export([callee/0]).
    callee() -> ok.
    "#,
            r#"
 //- /src/a.erl
    -module(a).
    cal~ler() ->
      b:callee().
 //- /src/b.erl
    -module(b).
    -export([callee/0]).
    callee() -> ok.
    "#,
            r#"
 //- /src/a.erl
    -module(a).
    ca~ller() ->
      b:callee().
   %% ^^^^^^^^ from_range: b:callee/0
 //- /src/b.erl
    -module(b).
    -export([callee/0]).
    callee() -> ok.
 %% ^^^^^^ to: b:callee/0
    "#,
        );
    }
}

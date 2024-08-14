/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod highlights;
pub(crate) mod tags;

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::RootDatabase;
use elp_ide_db::SymbolKind;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::NodeOrToken;
use elp_syntax::TextRange;
use hir::AnyExpr;
use hir::CallTarget;
use hir::DefMap;
use hir::Expr;
use hir::ExprId;
use hir::InFile;
use hir::InFunctionClauseBody;
use hir::NameArity;
use hir::Semantic;
use hir::Strategy;

use self::highlights::Highlights;
use self::tags::Highlight;
use crate::HlMod;
use crate::HlTag;

#[derive(Debug, Clone, Copy)]
pub struct HlRange {
    pub range: TextRange,
    pub highlight: Highlight,
    pub binding_hash: Option<u64>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct HighlightConfig {}

// Feature: Semantic Syntax Highlighting
//
// ELP highlights some code semantically.
//
// Initially this is just used for bound variables in patterns

pub(crate) fn highlight(
    db: &RootDatabase,
    file_id: FileId,
    range_to_highlight: Option<TextRange>,
) -> Vec<HlRange> {
    let _p = tracing::info_span!("highlight").entered();
    let sema = Semantic::new(db);

    // Determine the root based on the given range.
    let (root, range_to_highlight) = {
        let source_file = sema.parse(file_id);
        let source_file = source_file.value.syntax();
        match range_to_highlight {
            Some(range) => {
                let node = match source_file.covering_element(range) {
                    NodeOrToken::Node(it) => it,
                    NodeOrToken::Token(it) => it.parent().unwrap_or_else(|| source_file.clone()),
                };
                (node, range)
            }
            None => (source_file.clone(), source_file.text_range()),
        }
    };

    let mut hl = highlights::Highlights::new(root.text_range());
    bound_vars_in_pattern_highlight(&sema, file_id, range_to_highlight, &mut hl);
    functions_highlight(&sema, file_id, range_to_highlight, &mut hl);
    deprecated_func_highlight(&sema, file_id, range_to_highlight, &mut hl);
    hl.to_vec()
}

fn bound_vars_in_pattern_highlight(
    sema: &Semantic,
    file_id: FileId,
    range_to_highlight: TextRange,
    hl: &mut Highlights,
) {
    let highlight_bound = HlTag::Symbol(SymbolKind::Variable) | HlMod::Bound;

    let bound_var_ranges = sema.bound_vars_in_pattern_diagnostic(file_id);
    bound_var_ranges.iter().for_each(|(_, _, var)| {
        let range = var.syntax().text_range();
        // Element inside the viewport, need to highlight
        if range_to_highlight.intersect(range).is_some() {
            hl.add(HlRange {
                range,
                highlight: highlight_bound,
                binding_hash: None,
            });
        }
    });
}

fn deprecated_func_highlight(
    sema: &Semantic,
    file_id: FileId,
    range_to_highlight: TextRange,
    hl: &mut Highlights,
) {
    let def_map = sema.def_map(file_id);
    let highlight = HlTag::Symbol(SymbolKind::Function) | HlMod::DeprecatedFunction;
    for (_, def) in def_map.get_functions() {
        if def.file.file_id == file_id {
            let function_id = InFile::new(file_id, def.function_id);
            let function_body = sema.to_function_body(function_id);
            sema.fold_function(
                Strategy::InvisibleMacros,
                function_id,
                (),
                &mut |acc, clause_id, ctx| {
                    let clause_body = function_body.in_clause(clause_id);
                    if let AnyExpr::Expr(Expr::Call { target, args }) = ctx.item {
                        let arity = args.len() as u32;
                        match target {
                            CallTarget::Local { name } => {
                                if let Some(range) = find_deprecated_range(
                                    sema,
                                    &def_map,
                                    &name,
                                    arity,
                                    range_to_highlight,
                                    clause_body,
                                ) {
                                    hl.add(HlRange {
                                        range,
                                        highlight,
                                        binding_hash: None,
                                    })
                                }
                            }
                            CallTarget::Remote { module, name } => {
                                if let Some(file_id) =
                                    find_remote_module_file_id(sema, file_id, &module, clause_body)
                                {
                                    let def_map = sema.def_map(file_id);
                                    if let Some(range) = find_deprecated_range(
                                        sema,
                                        &def_map,
                                        &name,
                                        arity,
                                        range_to_highlight,
                                        clause_body,
                                    ) {
                                        hl.add(HlRange {
                                            range,
                                            highlight,
                                            binding_hash: None,
                                        })
                                    }
                                }
                            }
                        }
                    }
                    acc
                },
            );
        }
    }
}

fn find_deprecated_range(
    sema: &Semantic,
    def_map: &DefMap,
    name: &ExprId,
    arity: u32,
    range_to_highlight: TextRange,
    function_body: &InFunctionClauseBody<()>,
) -> Option<TextRange> {
    let fun_atom = &function_body[*name].as_atom()?;
    let range = function_body.range_for_expr(*name)?;
    if range_to_highlight.intersect(range).is_some() {
        let name = sema.db.lookup_atom(*fun_atom);
        if def_map.is_deprecated(&NameArity::new(name, arity)) {
            return Some(range);
        }
    }
    None
}

fn find_remote_module_file_id(
    sema: &Semantic,
    file_id: FileId,
    module: &ExprId,
    function_body: &InFunctionClauseBody<()>,
) -> Option<FileId> {
    let module_atom = &function_body[*module].as_atom()?;
    let module_name = sema.db.lookup_atom(*module_atom);
    let module = sema.resolve_module_name(file_id, module_name.as_str())?;
    Some(module.file.file_id)
}

fn functions_highlight(
    sema: &Semantic,
    file_id: FileId,
    range_to_highlight: TextRange,
    hl: &mut Highlights,
) {
    let def_map = sema.def_map(file_id);
    for (_, def) in def_map.get_functions() {
        if def.file.file_id == file_id && (def.exported || def.deprecated) {
            let fun_decl_ast = def.source(sema.db.upcast());

            fun_decl_ast
                .iter()
                .for_each(|fun_clause| match fun_clause.clause() {
                    Some(ast::FunctionOrMacroClause::FunctionClause(clause)) => {
                        if let Some(n) = clause.name() {
                            let range = n.syntax().text_range();

                            let highlight = match (def.exported, def.deprecated) {
                                (true, true) => {
                                    HlTag::Symbol(SymbolKind::Function)
                                        | HlMod::ExportedFunction
                                        | HlMod::DeprecatedFunction
                                }
                                (false, true) => {
                                    HlTag::Symbol(SymbolKind::Function) | HlMod::DeprecatedFunction
                                }
                                (true, false) => {
                                    HlTag::Symbol(SymbolKind::Function) | HlMod::ExportedFunction
                                }
                                (false, false) => unreachable!("checked above"),
                            };

                            // Element inside the viewport, need to highlight
                            if range_to_highlight.intersect(range).is_some() {
                                hl.add(HlRange {
                                    range,
                                    highlight,
                                    binding_hash: None,
                                })
                            }
                        };
                    }
                    Some(ast::FunctionOrMacroClause::MacroCallExpr(_)) => {}
                    None => {}
                })
        }
    }
}

#[cfg(test)]
mod tests {
    use elp_base_db::fixture::WithFixture;
    use elp_ide_db::elp_base_db;
    use elp_ide_db::elp_base_db::fixture::extract_tags;
    use elp_ide_db::RootDatabase;
    use itertools::Itertools;

    use crate::syntax_highlighting::highlight;
    use crate::HlTag;

    // These are tests of the specific modifier functionality.  When
    // we go all-in with semantic tokens, we can consider bringing
    // over the RA test mechanism which compares an HTML file.
    #[track_caller]
    fn check_highlights(fixture: &str) {
        let (ranges, fixture) = extract_tags(fixture.trim_start(), "tag");
        let range = if !ranges.is_empty() {
            Some(ranges[0].0)
        } else {
            None
        };

        let (db, fixture) = RootDatabase::with_fixture(&fixture);
        let annotations = fixture.annotations(&db);
        let expected: Vec<_> = annotations
            .into_iter()
            .map(|(fr, tag)| (fr.range, tag))
            .sorted_by(|a, b| a.0.start().cmp(&b.0.start()))
            .collect();

        let file_id = fixture.files[0];
        let highlights = highlight(&db, file_id, range);
        let ranges: Vec<_> = highlights
            .iter()
            .filter(|h| h.highlight != HlTag::None.into()) // Means with no modifiers either
            .map(|h| {
                let mods: Vec<_> = h.highlight.mods.iter().map(|m| format!("{m}")).collect();
                (h.range, mods.join(","))
            })
            .sorted_by(|a, b| a.0.start().cmp(&b.0.start()))
            .collect();
        assert_eq!(expected, ranges);
    }

    #[test]
    fn highlights_1() {
        check_highlights(
            r#"
              f(Var1) ->
                Var1 = 1.
           %%   ^^^^bound "#,
        )
    }

    #[test]
    fn highlights_2() {
        check_highlights(
            r#"
              -export([f/1]).
              f(Var1) ->
           %% ^exported_function
                Var1 = 1.
           %%   ^^^^bound "#,
        )
    }

    #[test]
    fn deprecated_exported_highlight() {
        check_highlights(
            r#"
              -deprecated([{f, 1}, {g, 1}]).
              -export([g/1]).
              f(1) -> 1;
           %% ^deprecated_function
              f(2) -> 2.
           %% ^deprecated_function
              g(3) -> 3.
           %% ^exported_function,deprecated_function"#,
        )
    }

    #[test]
    fn deprecated_highlight() {
        check_highlights(
            r#"
              //- /src/deprecated_highlight.erl
              -module(deprecated_highlight).
              -deprecated([{f, 1}]).
              f(1) -> 1.
           %% ^deprecated_function
              ga(Num) -> f(Num).
           %%            ^deprecated_function"#,
        )
    }

    #[test]
    fn highlights_in_range() {
        check_highlights(
            r#"
              -export([f/1]).
              foo(X) -> ok.
              f(Var1) ->
           %% Not exported_function
              <tag range>
                Var1 = 1.
           %%   ^^^^bound
              </tag>
              bar(Y) -> ok.
              "#,
        )
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

mod highlights;
pub(crate) mod tags;

use std::sync::Arc;

use elp_eqwalizer::ast::Pos;
use elp_ide_db::RootDatabase;
use elp_ide_db::SymbolKind;
use elp_ide_db::elp_base_db::FileId;
use elp_syntax::AstNode;
use elp_syntax::NodeOrToken;
use elp_syntax::TextRange;
use elp_syntax::ast;
use elp_types_db::eqwalizer::types::Type;
use hir::AnyExpr;
use hir::CallTarget;
use hir::DefMap;
use hir::Expr;
use hir::ExprId;
use hir::InFile;
use hir::InFunctionClauseBody;
use hir::Literal;
use hir::NameArity;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;

use self::highlights::Highlights;
use self::tags::Highlight;
use crate::HlMod;
use crate::HlTag;
use crate::format_string;

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
    types: Option<Arc<Vec<(Pos, Type)>>>,
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
    types_highlight(&sema, file_id, range_to_highlight, &mut hl);
    deprecated_func_highlight(&sema, file_id, range_to_highlight, &mut hl);
    dynamic_usages_highlight(types, range_to_highlight, &mut hl);
    format_specifier_highlight(&sema, file_id, range_to_highlight, &mut hl);
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
    let def_map = sema.def_map_local(file_id);
    let highlight = HlTag::Symbol(SymbolKind::Function) | HlMod::DeprecatedFunction;
    for (_, def) in def_map.get_functions() {
        let function_id = InFile::new(file_id, def.function_id);
        let function_body = sema.to_function_body(function_id);
        sema.fold_function(
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
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
                                file_id,
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
                        CallTarget::Remote { module, name, .. } => {
                            if let Some(file_id) =
                                find_remote_module_file_id(sema, file_id, &module, clause_body)
                            {
                                let def_map = sema.def_map(file_id);
                                if let Some(range) = find_deprecated_range(
                                    sema,
                                    file_id,
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
        )
    }
}

fn find_deprecated_range(
    sema: &Semantic,
    file_id: FileId,
    def_map: &DefMap,
    name: &ExprId,
    arity: u32,
    range_to_highlight: TextRange,
    function_body: &InFunctionClauseBody<()>,
) -> Option<TextRange> {
    let fun_atom = &function_body[*name].as_atom()?;
    let range = function_body.range_for_expr(*name)?;
    if range.file_id != file_id {
        return None;
    }
    if range_to_highlight.intersect(range.range).is_some() {
        let name = sema.db.lookup_atom(*fun_atom);
        if def_map.is_deprecated(&NameArity::new(name, arity)) {
            return Some(range.range);
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
    let def_map = sema.def_map_local(file_id);
    for (_, def) in def_map.get_functions() {
        if def.exported || def.deprecated {
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

fn types_highlight(
    sema: &Semantic,
    file_id: FileId,
    range_to_highlight: TextRange,
    hl: &mut Highlights,
) {
    let def_map = sema.def_map_local(file_id);
    for def in def_map.get_types().values() {
        if def.exported {
            let type_alias_source = def.source(sema.db.upcast());

            if let Some(type_name) = type_alias_source.type_name()
                && let Some(name) = type_name.name()
            {
                let range = name.syntax().text_range();

                // The base highlighing currently uses SymbolKind::Function.
                // We cannot set a modifier only. so must repeat it here.
                let highlight = HlTag::Symbol(SymbolKind::Function) | HlMod::ExportedType;

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
    }
}

/// Highlight usages of eqwalizer dynamic() type
fn dynamic_usages_highlight(
    types: Option<Arc<Vec<(Pos, Type)>>>,
    range_to_highlight: TextRange,
    hl: &mut Highlights,
) {
    let highlight_dynamic = HlTag::Symbol(SymbolKind::Variable) | HlMod::TypeDynamic;

    if let Some(types) = types {
        types.iter().for_each(|(r, t)| {
            if is_dynamic(t)
                && let Pos::TextRange(eq_range) = r
            {
                let range: TextRange = (eq_range.clone()).into();
                if range_to_highlight.intersect(range).is_some() {
                    hl.add(HlRange {
                        range,
                        highlight: highlight_dynamic,
                        binding_hash: None,
                    });
                }
            }
        });
    }
}

fn is_dynamic(t: &Type) -> bool {
    match t {
        Type::DynamicType => true,
        Type::BoundedDynamicType(_) => true,
        _ => false,
    }
}

/// Highlight format specifiers (`~p`, `~w`, etc.) inside calls to
/// `io:format`, `io_lib:format` and friends.
///
/// ## Architecture: HIR fold for discovery, syntax for ranges
///
/// We use `sema.fold_function` with `MacroStrategy::Expand` to find
/// format calls.  This handles macro expansions correctly: when a
/// format string is passed as a **macro argument** (e.g.
/// `?LOG("~p", [X])`), the HIR lowerer's `resolve_var` restores the
/// call-site `FileId`, so `range_for_expr` returns a range in the
/// file the user is editing and highlighting works normally.
///
/// We deliberately stay at the **source-text** level for computing
/// per-specifier `TextRange`s.  A pure-HIR approach (parsing
/// specifiers from `Literal::String` content) would not help here
/// because escape sequences, sigils and triple-quoting mean that
/// content byte offsets do not map 1:1 to source byte offsets — and
/// visual highlighting needs exact source positions.
///
/// Format strings embedded in a **macro body** (not passed as an
/// argument) will have a `FileId` pointing to the macro definition
/// file, so the `file_id` guard in `find_format_highlight_info`
/// correctly skips them — there is no visible source location to
/// highlight in the file using the macro.
fn format_specifier_highlight(
    sema: &Semantic,
    file_id: FileId,
    range_to_highlight: TextRange,
    hl: &mut Highlights,
) {
    let highlight_spec = HlTag::Symbol(SymbolKind::Variable) | HlMod::FormatSpecifier;
    let def_map = sema.def_map_local(file_id);
    let file_text = sema.db.file_text(file_id);

    for (_, def) in def_map.get_functions() {
        // Skip functions entirely outside the visible range.
        if let Some(fun_range) = def.range(sema.db.upcast())
            && fun_range.intersect(range_to_highlight).is_none()
        {
            continue;
        }
        let function_id = InFile::new(file_id, def.function_id);
        let function_body = sema.to_function_body(function_id);
        sema.fold_function(
            Strategy {
                macros: MacroStrategy::Expand,
                parens: ParenStrategy::InvisibleParens,
            },
            function_id,
            (),
            &mut |acc, clause_id, ctx| {
                let clause_body = function_body.in_clause(clause_id);
                if let AnyExpr::Expr(Expr::Call {
                    target: CallTarget::Remote { module, name, .. },
                    args,
                }) = ctx.item
                    && let Some(module_atom) = clause_body[module].as_atom()
                    && let Some(name_atom) = clause_body[name].as_atom()
                    && let Some(info) = format_string::match_format_function(
                        &sema.db.lookup_atom(module_atom),
                        &sema.db.lookup_atom(name_atom),
                        args.len(),
                    )
                    && let Some(fmt_info) = find_format_highlight_info(
                        file_id,
                        &file_text,
                        info,
                        &args,
                        range_to_highlight,
                        clause_body,
                    )
                {
                    for range in fmt_info.specifier_ranges {
                        hl.add(HlRange {
                            range,
                            highlight: highlight_spec,
                            binding_hash: None,
                        });
                    }
                }
                acc
            },
        )
    }
}

struct FormatHighlightInfo {
    specifier_ranges: Vec<TextRange>,
}

fn find_format_highlight_info(
    file_id: FileId,
    file_text: &str,
    info: format_string::FormatFunctionInfo,
    args: &[ExprId],
    range_to_highlight: TextRange,
    clause_body: &InFunctionClauseBody<()>,
) -> Option<FormatHighlightInfo> {
    let fmt_expr_id = args[info.format_arg_index];

    // Only handle literal strings (not variables or concatenated strings)
    match &clause_body[fmt_expr_id] {
        Expr::Literal(Literal::String(_)) => {}
        _ => return None,
    }

    let fmt_range = clause_body.range_for_expr(fmt_expr_id)?;
    if fmt_range.file_id != file_id {
        return None;
    }

    let fmt_src = format_string::parse_format_source(file_text, fmt_range.range)?;

    let mut specifier_ranges = Vec::new();

    for spec in &fmt_src.parsed.specifiers {
        let range = format_string::specifier_source_range(
            fmt_src.string_range,
            fmt_src.content_offset,
            spec,
        );
        if range_to_highlight.intersect(range).is_some() {
            specifier_ranges.push(range);
        }
    }

    if specifier_ranges.is_empty() {
        None
    } else {
        Some(FormatHighlightInfo { specifier_ranges })
    }
}

#[cfg(test)]
mod tests {
    use elp_base_db::fixture::WithFixture;
    use elp_ide_db::EqwalizerDatabase;
    use elp_ide_db::RootDatabase;
    use elp_ide_db::elp_base_db;
    use elp_ide_db::elp_base_db::FileLoader;
    use elp_ide_db::elp_base_db::assert_eq_expected;
    use elp_project_model::otp::otp_supported_by_eqwalizer;
    use itertools::Itertools;
    use stdx::trim_indent;

    use crate::HlMod;
    use crate::HlTag;
    use crate::syntax_highlighting::highlight;

    // These are tests of the specific modifier functionality.  When
    // we go all-in with semantic tokens, we can consider bringing
    // over the RA test mechanism which compares an HTML file.
    #[track_caller]
    fn check_highlights(fixture: &str) {
        do_check_highlights(fixture, false);
    }
    #[track_caller]
    fn check_highlights_eqwalizer(fixture: &str) {
        do_check_highlights(fixture, true);
    }

    #[track_caller]
    fn do_check_highlights(fixture: &str, provide_types: bool) {
        let fixture = trim_indent(fixture);
        let (db, fixture) = RootDatabase::with_fixture(&fixture);
        let ranges = fixture.tags.get(&fixture.file_id()).unwrap();
        let range = if !ranges.is_empty() {
            Some(ranges[0].0)
        } else {
            None
        };
        let annotations = fixture.annotations();
        let expected: Vec<_> = annotations
            .into_iter()
            .map(|(fr, tag)| (fr.range, tag))
            .sorted_by(|a, b| a.0.start().cmp(&b.0.start()))
            .collect();

        let file_id = fixture.files[0];
        let types = if provide_types {
            db.types_for_file(file_id)
        } else {
            None
        };
        let highlights = highlight(&db, file_id, types, range);
        let ranges: Vec<_> = highlights
            .iter()
            .filter(|h| h.highlight != HlTag::None.into()) // Means with no modifiers either
            .map(|h| {
                let mods: Vec<_> = h.highlight.mods.iter().map(|m| format!("{m}")).collect();
                (h.range, mods.join(","))
            })
            .sorted_by(|a, b| a.0.start().cmp(&b.0.start()))
            .collect();
        assert_eq_expected!(expected, ranges);
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
           //- /src/highlights_in_range.erl tag:tag
              -module(highlights_in_range).
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

    #[test]
    fn eqwalizer_dynamic_highlight() {
        if otp_supported_by_eqwalizer() {
            check_highlights_eqwalizer(
                r#"
            //- eqwalizer
            //- /app_a/src/a_file.erl
              -module(a_file).
              -spec f(dynamic()) -> ok.
              f(AAA) -> ok.
            %%  ^^^type_dynamic
              "#,
            )
        }
    }

    #[test]
    fn exported_type() {
        check_highlights(
            r#"
              -export_type([foo/0]).
              -type foo() :: integer().
           %%       ^^^exported_type
              -type bar() :: integer().
              "#,
        )
    }

    // Helper for format specifier tests. Uses \~ in fixture text so the
    // fixture parser converts them back to ~ without consuming them as
    // cursor markers. Verifies that the highlighted specifier texts match.
    #[track_caller]
    fn check_format_specifiers(fixture: &str, expected_specifiers: &[&str]) {
        let fixture_str = trim_indent(fixture);
        let (db, fixture) = RootDatabase::with_fixture(&fixture_str);
        let file_id = fixture.files[0];
        let highlights = highlight(&db, file_id, None, None);
        let file_text = db.file_text(file_id);
        let mut format_highlights: Vec<_> = highlights
            .iter()
            .filter(|h| h.highlight.mods.contains(HlMod::FormatSpecifier))
            .collect();
        format_highlights.sort_by_key(|h| h.range.start());
        let actual_texts: Vec<&str> = format_highlights
            .iter()
            .map(|h| &file_text[usize::from(h.range.start())..usize::from(h.range.end())])
            .collect();
        assert_eq!(
            actual_texts, expected_specifiers,
            "Format specifier mismatch"
        );
    }

    #[test]
    fn format_specifier_simple() {
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("hello \~p world \~s done", [A, B]).
            "#,
            &["~p", "~s"],
        );
    }

    #[test]
    fn format_specifier_non_consuming() {
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~p\~n\~\~", [A]).
            "#,
            &["~p", "~n", "~~"],
        );
    }

    #[test]
    fn format_specifier_with_width() {
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~10.5f", [A]).
            "#,
            &["~10.5f"],
        );
    }

    #[test]
    fn format_specifier_io_format_3() {
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format(user, "\~w \~s", [A, B]).
            "#,
            &["~w", "~s"],
        );
    }

    #[test]
    fn format_specifier_io_lib_format() {
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io_lib:format("\~p", [A]).
            "#,
            &["~p"],
        );
    }

    #[test]
    fn format_specifier_multi_arg() {
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~W \~P", [A, 10, B, 20]).
            "#,
            &["~W", "~P"],
        );
    }

    #[test]
    fn format_specifier_fwrite() {
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:fwrite("\~p\~n", [A]),
                io_lib:fwrite("\~s", [B]).
            "#,
            &["~p", "~n", "~s"],
        );
    }

    #[test]
    fn format_specifier_not_literal() {
        // Format string is a variable, not a literal - should produce no highlights
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            foo(Fmt) ->
                io:format(Fmt, [A]).
            "#,
            &[],
        );
    }

    #[test]
    fn format_specifier_not_format_call() {
        // Not a format function - should produce no highlights
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                lists:map("\~p", [A]).
            "#,
            &[],
        );
    }

    #[test]
    fn format_specifier_exact_match() {
        // Exact match: ~p→A, ~s→B
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            foo() ->
                io:format("\~p \~s", [A, B]).
            "#,
            &["~p", "~s"],
        );
    }

    #[test]
    fn format_specifier_variable_args_list() {
        // Args list is a variable - specifiers still highlighted
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            foo(Args) ->
                io:format("\~p \~s", Args).
            "#,
            &["~p", "~s"],
        );
    }

    #[test]
    fn format_specifier_macro_wrapping_format() {
        // Macro wraps io:format - format string passed as argument
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            -define(LOG(Fmt, Args), io:format(Fmt, Args)).
            foo() ->
                ?LOG("\~p \~s", [A, B]).
            "#,
            &["~p", "~s"],
        );
    }

    #[test]
    fn format_specifier_macro_from_header() {
        // Macro defined in a header file, format string passed as argument at call site
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            -include("log.hrl").
            foo() ->
                ?LOG("\~p \~w", [A, B]).
            //- /include/log.hrl include_path:/include
            -define(LOG(Fmt, Args), io:format(Fmt, Args)).
            "#,
            &["~p", "~w"],
        );
    }

    #[test]
    fn format_specifier_macro_io_format_3() {
        // Macro wraps io:format/3 with a device argument
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            -define(LOG(Fmt, Args), io:format(user, Fmt, Args)).
            foo() ->
                ?LOG("\~s \~p", [A, B]).
            "#,
            &["~s", "~p"],
        );
    }

    #[test]
    fn format_specifier_macro_io_lib_format() {
        // Macro wraps io_lib:format
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            -define(FMT(Fmt, Args), io_lib:format(Fmt, Args)).
            foo() ->
                ?FMT("\~p", [A]).
            "#,
            &["~p"],
        );
    }

    #[test]
    fn non_trivial_format_specifier_macro_io_lib_format() {
        // Macro wraps io_lib:format
        check_format_specifiers(
            r#"
            //- /src/main.erl
            -module(main).
            -define(FMT(Fmt, Args), begin io_lib:format(Fmt, Args) ++ "~n" end).
            foo() ->
                ?FMT("\~p", [A]).
            "#,
            &["~p"],
        );
    }
}

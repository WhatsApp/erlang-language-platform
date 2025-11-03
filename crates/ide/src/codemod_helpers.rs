/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_ide_db::elp_base_db::FileRange;
use elp_syntax::AstNode;
use elp_syntax::SmolStr;
use elp_syntax::SyntaxKind;
use elp_syntax::TextRange;
use elp_syntax::ast;
use elp_syntax::ast::in_erlang_module;
use elp_types_db::eqwalizer;
use fxhash::FxHashMap;
use hir::AnyExpr;
use hir::AnyExprId;
use hir::Body;
use hir::CallTarget;
use hir::Expr;
use hir::ExprId;
use hir::FunctionDef;
use hir::InFile;
use hir::InFunctionClauseBody;
use hir::Literal;
use hir::Semantic;
use hir::Strategy;
use hir::db::InternDatabase;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use hir::fold::ParentId;
use lazy_static::lazy_static;
use regex::Regex;
use serde::Deserialize;
use serde::Serialize;

use crate::FileId;

// Given an expression that represents a statement, return a text range that covers
// the statement in full. This means:
//
// - If the expression is followed by a comma, include the comma in the range, and all
//   whitespace following the comma
// - If the expression is not followed by a comma, but is preceded by a comma, include
//   the preceding comma in the range, and all whitespace before the comma
// - Otherwise, we just use the range of the expression
//
// We typically want to have the statement range in order to be able to delete the statement,
// remove an element from a list, etc.
pub(crate) fn statement_range(expr: &ast::Expr) -> TextRange {
    let node = expr.syntax();
    let node_range = node.text_range();

    let mut right = node.last_token().and_then(|tok| tok.next_token());
    let mut searching_for_comma = true;
    let final_node_range;

    let mut node_range_right = node_range;
    while let Some(tok) = right {
        match tok.kind() {
            SyntaxKind::WHITESPACE => node_range_right = tok.text_range(),
            SyntaxKind::ANON_COMMA if searching_for_comma => {
                node_range_right = tok.text_range();
                searching_for_comma = false
            }
            _ => break,
        }
        right = tok.next_token();
    }

    if !searching_for_comma {
        final_node_range = node_range_right;
    } else {
        // We didn't find a trailing comma, so let's see if there is a preceding comma

        let mut left = node.first_token().and_then(|tok| tok.prev_token());

        let mut node_range_left = node_range;
        while let Some(tok) = left {
            match tok.kind() {
                SyntaxKind::WHITESPACE => node_range_left = tok.text_range(),
                SyntaxKind::ANON_COMMA if searching_for_comma => {
                    node_range_left = tok.text_range();
                    searching_for_comma = false
                }
                _ => break,
            }
            left = tok.prev_token();
        }

        if !searching_for_comma {
            final_node_range = node_range_left;
        } else {
            // We didn't find a trailing nor preceding comma, this is a singleton
            final_node_range = node_range_left.cover(node_range_right);
        }
    }

    node_range.cover(final_node_range)
}

pub(crate) fn var_name_starts_with_underscore(db: &dyn InternDatabase, var: &hir::Var) -> bool {
    var.as_string(db).starts_with('_')
}

pub(crate) fn is_only_place_where_var_is_defined(
    sema: &Semantic,
    var: &InFunctionClauseBody<AnyExprId>,
) -> bool {
    check_is_only_place_where_var_is_defined(sema, var).is_some()
}

pub(crate) fn var_has_no_references(
    sema: &Semantic,
    var: &InFunctionClauseBody<AnyExprId>,
) -> bool {
    check_var_has_no_references(sema, var).is_some()
}

pub(crate) fn check_is_only_place_where_var_is_defined_ast(
    sema: &Semantic,
    var: InFile<&ast::Var>,
) -> Option<()> {
    let usages = sema.find_local_usages_ast(var)?;
    let num_definitions = usages
        .iter()
        .filter(|v| sema.to_def(var.with_value(*v)).is_some_and(is_definition))
        .count();
    if num_definitions == 1 { Some(()) } else { None }
}

pub(crate) fn check_is_only_place_where_var_is_defined(
    sema: &Semantic,
    var: &InFunctionClauseBody<AnyExprId>,
) -> Option<()> {
    let usages = sema.find_local_usages(var)?;
    let num_definitions = usages
        .iter()
        .filter(|(id, _v)| var.to_var_def_any(*id).is_some_and(is_definition))
        .count();
    if num_definitions == 1 { Some(()) } else { None }
}

pub(crate) fn check_var_has_no_references_ast(
    sema: &Semantic,
    var: InFile<&ast::Var>,
) -> Option<()> {
    let usages = sema.find_local_usages_ast(var)?;
    let num_definitions = usages
        .iter()
        .filter(|v| {
            sema.to_def(var.with_value(*v))
                .is_some_and(|dor| !is_definition(dor))
        })
        .count();
    if num_definitions == 0 { Some(()) } else { None }
}

pub(crate) fn check_var_has_no_references(
    sema: &Semantic,
    var: &InFunctionClauseBody<AnyExprId>,
) -> Option<()> {
    let usages = sema.find_local_usages(var)?;
    let definition_found = usages.iter().any(|(id, _v)| {
        var.to_var_def_any(*id)
            .is_some_and(|dor| !is_definition(dor))
    });
    if !definition_found { Some(()) } else { None }
}

pub(crate) fn check_var_has_references(sema: &Semantic, var: InFile<&ast::Var>) -> Option<()> {
    match check_var_has_no_references_ast(sema, var) {
        Some(()) => None,
        None => Some(()),
    }
}

fn is_definition<D, R>(def: hir::DefinitionOrReference<D, R>) -> bool {
    match def {
        hir::DefinitionOrReference::Definition { .. } => true,
        hir::DefinitionOrReference::Reference { .. } => false,
    }
}

#[derive(Debug, Clone)]
pub struct FunctionMatcher<'a, T> {
    match_any: Option<(&'a FunctionMatch, &'a T)>,
    labels_full: FxHashMap<Option<SmolStr>, (&'a FunctionMatch, &'a T)>,
    labels_full_typed:
        FxHashMap<Option<SmolStr>, (&'a Vec<eqwalizer::types::Type>, &'a FunctionMatch, &'a T)>,
    labels_mf: FxHashMap<Option<SmolStr>, (&'a FunctionMatch, &'a T)>,
    labels_m: FxHashMap<Option<SmolStr>, (&'a FunctionMatch, &'a T)>,
}

impl<'a, T> FunctionMatcher<'a, T> {
    pub fn new(mfas: &'a [(&'a FunctionMatch, T)]) -> FunctionMatcher<'a, T> {
        let mut labels_full = FxHashMap::default();
        let mut labels_full_typed = FxHashMap::default();
        let mut labels_mf = FxHashMap::default();
        let mut labels_m = FxHashMap::default();
        let mut match_any = None;

        mfas.iter().for_each(|(c, t)| match c {
            FunctionMatch::Any => {
                match_any = Some((*c, t));
            }
            FunctionMatch::MFA { mfa } => {
                if mfa.module == "erlang" && in_erlang_module(&mfa.name, mfa.arity as usize) {
                    labels_full.insert(Some(mfa.short_label().into()), (*c, t));
                }
                labels_full.insert(Some(mfa.label().into()), (*c, t));
            }
            FunctionMatch::TypedMFA { mfa, types } => {
                if mfa.module == "erlang" && in_erlang_module(&mfa.name, mfa.arity as usize) {
                    labels_full_typed.insert(Some(mfa.short_label().into()), (types, *c, t));
                }
                labels_full_typed.insert(Some(mfa.label().into()), (types, *c, t));
            }
            FunctionMatch::MF { module, name } => {
                let label = format!("{module}:{name}");
                labels_mf.insert(Some(label.into()), (*c, t));
            }
            FunctionMatch::M { module } => {
                labels_m.insert(Some(module.into()), (*c, t));
            }
        });
        FunctionMatcher {
            match_any,
            labels_full,
            labels_full_typed,
            labels_mf,
            labels_m,
        }
    }

    pub fn get_match(
        &self,
        target: &CallTarget<ExprId>,
        arity: u32,
        args: Option<&[ExprId]>,
        sema: &Semantic,
        body: &Body,
    ) -> Option<(&'a FunctionMatch, &'a T)> {
        self.match_any
            .or_else(|| {
                self.labels_full
                    .get(&target.label(arity, sema, body))
                    .copied()
            })
            .or_else(|| {
                let (types, match_val, t) = self
                    .labels_full_typed
                    .get(&target.label(arity, sema, body))?;
                self.types_match(args?, types, sema, body)
                    .then_some((match_val, t))
            })
            .or_else(|| self.labels_mf.get(&target.label_short(sema, body)).copied())
            .or_else(|| self.labels_mf.get(&target.label_short(sema, body)).copied())
            .or_else(|| match target {
                CallTarget::Local { name: _ } => None,
                CallTarget::Remote { module, .. } => {
                    let name = sema.db.lookup_atom(body[*module].as_atom()?);
                    let label = Some(SmolStr::new(format!("{name}")));
                    self.labels_m.get(&label).copied()
                }
            })
    }

    fn types_match(
        &self,
        args: &[ExprId],
        types: &[eqwalizer::types::Type],
        sema: &Semantic,
        body: &Body,
    ) -> bool {
        args.len() == types.len()
            && args.iter().zip(types.iter()).all(|(expr_id, eq_type)| {
                sema.expr_type(body, expr_id)
                    .map(|t| &t == eq_type)
                    .unwrap_or(false)
            })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(tag = "type")]
#[allow(clippy::upper_case_acronyms)]
pub enum FunctionMatch {
    Any,
    MFA {
        mfa: MFA,
    },
    MF {
        module: String,
        name: String,
    },
    M {
        module: String,
    },
    TypedMFA {
        mfa: MFA,
        types: Vec<eqwalizer::types::Type>,
    },
}

impl FunctionMatch {
    pub fn any() -> FunctionMatch {
        FunctionMatch::Any
    }

    pub fn mfa(m: &str, f: &str, arity: u32) -> FunctionMatch {
        FunctionMatch::MFA {
            mfa: MFA {
                module: m.into(),
                name: f.into(),
                arity,
            },
        }
    }

    pub fn typed_mfa(m: &str, f: &str, types: Vec<eqwalizer::types::Type>) -> FunctionMatch {
        let mfa = MFA {
            module: m.into(),
            name: f.into(),
            arity: types.len() as u32,
        };
        FunctionMatch::TypedMFA { mfa, types }
    }

    pub fn mfas(m: &str, f: &str, arity: Vec<u32>) -> Vec<FunctionMatch> {
        arity
            .into_iter()
            .map(|a| FunctionMatch::MFA {
                mfa: MFA {
                    module: m.into(),
                    name: f.into(),
                    arity: a,
                },
            })
            .collect()
    }

    pub fn mf(m: &str, f: &str) -> FunctionMatch {
        FunctionMatch::MF {
            module: m.into(),
            name: f.into(),
        }
    }

    pub fn m(m: &str) -> FunctionMatch {
        FunctionMatch::M { module: m.into() }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
#[allow(clippy::upper_case_acronyms)]
#[serde(try_from = "String", into = "String")]
pub struct MFA {
    pub module: String,
    pub name: String,
    pub arity: u32,
}

impl MFA {
    pub fn from_call_target(
        target: &CallTarget<ExprId>,
        arity: u32,
        sema: &Semantic,
        body: &Body,
        file_id: FileId,
    ) -> Option<MFA> {
        let call_target = target.resolve_call(arity, sema, file_id, body)?;
        let call_module = call_target.module?;
        let na = call_target.name;
        Some(MFA {
            module: call_module.to_quoted_string().into_owned(),
            name: na.name().to_quoted_string().into_owned(),
            arity: na.arity(),
        })
    }

    #[cfg(test)] // @oss-only
    pub fn new(m: &str, f: &str, arity: u32) -> MFA {
        MFA {
            module: m.to_string(),
            name: f.to_string(),
            arity,
        }
    }

    pub fn label(&self) -> String {
        format!("{}:{}/{}", self.module, self.name, self.arity)
    }

    pub fn short_label(&self) -> String {
        format!("{}/{}", self.name, self.arity)
    }
}

// Serde serialization via String
impl From<MFA> for String {
    fn from(val: MFA) -> Self {
        format!("{}:{}/{}", val.module, val.name, val.arity)
    }
}

// Serde serialization via String
impl TryFrom<String> for MFA {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        TryFrom::try_from(value.as_str())
    }
}
impl TryFrom<&str> for MFA {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^([^:]+):([^:]+)\/(\d+)$").unwrap();
        }
        if let Some(captures) = RE.captures(value)
            && captures.len() > 3
        {
            let arity = captures[3]
                .parse::<u32>()
                .unwrap_or_else(|_| panic!("bad arity field for `{value}'"));
            return Ok(MFA {
                module: captures[1].to_string(),
                name: captures[2].to_string(),
                arity,
            });
        }
        Err(format!("invalid MFA '{value}'"))
    }
}

#[allow(unused)]
pub struct CheckCallCtx<'a, T> {
    pub sema: &'a Semantic<'a>,
    pub mfa: &'a FunctionMatch,
    pub parents: &'a Vec<ParentId>,
    pub target: &'a CallTarget<ExprId>,
    pub t: &'a T,
    pub args: &'a Args,
    pub in_clause: &'a InFunctionClauseBody<'a, &'a FunctionDef>,
}

#[derive(Debug, Clone)]
pub enum Args {
    Args(Vec<ExprId>),
    Arity(u32),
}

impl Args {
    pub fn get(&self, i: usize) -> Option<ExprId> {
        match self {
            Args::Args(args) => args.get(i).copied(),
            Args::Arity(_) => None,
        }
    }

    pub fn arity(&self) -> u32 {
        match self {
            Args::Args(args) => args.len() as u32,
            Args::Arity(arity) => *arity,
        }
    }

    pub fn as_slice(&self) -> &[ExprId] {
        const EMPTY_SLICE: &[ExprId] = &[];
        match self {
            Args::Args(args) => args,
            Args::Arity(_arity) => EMPTY_SLICE,
        }
    }
}

/// Check a specific call instance, and return extra info for the call
pub type CheckCall<'a, Ctx, Res> = &'a dyn Fn(CheckCallCtx<Ctx>) -> Option<Res>;

pub struct MatchCtx<'a, Extra> {
    pub sema: &'a Semantic<'a>,
    pub def_fb: &'a InFunctionClauseBody<'a, &'a FunctionDef>,
    pub target: &'a CallTarget<ExprId>,
    pub args: &'a Args,
    /// Range of the module:fun part of an MFA, if not defined in a
    /// macro.
    pub range_surface_mf: Option<FileRange>,
    pub range: FileRange,
    pub extra: &'a Extra,
}

impl<U> MatchCtx<'_, U> {
    /// Range of the module:fun part of an MFA, if not defined in a
    /// macro, in which case the macro call location is used.
    pub fn range_mf_or_macro(&self) -> FileRange {
        self.range_surface_mf.unwrap_or(self.range)
    }

    pub fn range(&self, use_range: &UseRange) -> FileRange {
        match use_range {
            UseRange::WithArgs => self.range,
            UseRange::NameOnly => self.range_mf_or_macro(),
        }
    }
}

pub type Make<'a, Ctx, Res> = &'a dyn Fn(MatchCtx<Ctx>) -> Option<Res>;

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum UseRange {
    #[allow(dead_code)]
    WithArgs,
    NameOnly,
}

pub(crate) fn find_call_in_function<CallCtx, MakeCtx, Res>(
    res: &mut Vec<Res>,
    sema: &Semantic,
    def: &FunctionDef,
    mfas: &[(&FunctionMatch, CallCtx)],
    excluded_mfas: &[(&FunctionMatch, CallCtx)],
    check_call: CheckCall<CallCtx, MakeCtx>,
    make: Make<MakeCtx, Res>,
) -> Option<()> {
    let def_fb = def.in_function_body(sema, def);
    let matcher = FunctionMatcher::new(mfas);
    let excluded_matcher = FunctionMatcher::new(excluded_mfas);
    def_fb.clone().fold_function(
        Strategy {
            macros: MacroStrategy::ExpandButIncludeMacroCall,
            parens: ParenStrategy::InvisibleParens,
        },
        (),
        &mut |acc, clause_id, ctx| {
            let def_fc = &def_fb[clause_id];
            if let Some((target, args)) = match ctx.item {
                AnyExpr::Expr(Expr::CaptureFun { target, arity }) => match &def_fc.body[arity] {
                    Expr::Literal(Literal::Integer(arity)) => {
                        Some((target, Args::Arity(arity.value as u32)))
                    }
                    _ => None,
                },
                AnyExpr::Expr(Expr::Call { target, args }) => Some((target, Args::Args(args))),
                _ => None,
            } && let None = excluded_matcher.get_match(
                &target,
                args.arity(),
                Some(args.as_slice()),
                sema,
                &def_fb.body(clause_id),
            ) && let Some((mfa, t)) = matcher.get_match(
                &target,
                args.arity(),
                Some(args.as_slice()),
                sema,
                &def_fb.body(clause_id),
            ) {
                let in_clause = &def_fb.in_clause(clause_id);
                let context = CheckCallCtx {
                    sema,
                    mfa,
                    parents: ctx.parents,
                    t,
                    target: &target,
                    args: &args,
                    in_clause,
                };
                if let Some(extra) = check_call(context) {
                    // Got one.
                    let call_expr_id = if let Some(expr_id) = ctx.in_macro {
                        expr_id.idx
                    } else {
                        ctx.item_id
                    };
                    if let Some(range) = &def_fb.range_for_any(clause_id, call_expr_id) {
                        let range_surface_mf = if ctx.in_macro.is_none() {
                            target.range(in_clause)
                        } else {
                            // We need to rather use the full range, of the macro
                            None
                        };
                        if let Some(diag) = make(MatchCtx {
                            sema,
                            def_fb: in_clause,
                            target: &target,
                            args: &args,
                            extra: &extra,
                            range_surface_mf,
                            range: *range,
                        }) {
                            res.push(diag)
                        }
                    }
                }
            };
            acc
        },
    );
    Some(())
}

/// Helper function to create a fix that replaces the module name in a remote call.
/// This is useful for linters that need to rename module references (e.g., replacing
/// `old_module:function()` with `new_module:function()`).
///
/// # Arguments
/// * `match_context` - The match context from the FunctionCallLinter
/// * `file_id` - The file being analyzed
/// * `new_module_name` - The new module name to replace with
/// * `fix_id` - A unique identifier for this fix (e.g., "rename_module_to_new_name")
/// * `fix_label` - A human-readable label for the fix (e.g., "Rename to new_module")
///
/// # Returns
/// An `Assist` that replaces just the module name, or `None` if the target is not a
/// remote call or if the module range is in a different file.
pub fn make_module_rename_fix<T>(
    match_context: &MatchCtx<T>,
    file_id: FileId,
    new_module_name: &str,
    fix_id: &'static str,
    fix_label: &str,
) -> Option<elp_ide_assists::Assist> {
    use elp_ide_db::source_change::SourceChangeBuilder;

    let module_range = match_context.target.module_range(match_context.def_fb)?;

    let mut builder = SourceChangeBuilder::new(file_id);
    builder.replace(module_range.range, new_module_name.to_string());

    Some(crate::fix(
        fix_id,
        fix_label,
        builder.finish(),
        module_range.range,
    ))
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use elp_ide_db::RootDatabase;
    use elp_ide_db::elp_base_db::FileId;
    use elp_ide_db::elp_base_db::SourceDatabase;
    use elp_ide_db::elp_base_db::fixture::WithFixture;
    use elp_project_model::otp::otp_supported_by_eqwalizer;
    use elp_syntax::algo::find_node_at_offset;
    use elp_syntax::ast;
    use expect_test::Expect;
    use expect_test::expect;
    use hir::FunctionDef;
    use hir::InFile;
    use hir::Semantic;

    use super::FunctionMatch;
    use super::MatchCtx;
    use super::UseRange;
    use super::find_call_in_function;
    use crate::AnalysisHost;
    use crate::diagnostics::Diagnostic;
    use crate::diagnostics::DiagnosticCode;
    use crate::diagnostics::DiagnosticsConfig;
    use crate::diagnostics::Severity;
    use crate::fixture;
    use crate::tests::check_diagnostics_with_config_and_ad_hoc;
    use crate::tests::check_fix_with_config_and_adhoc;

    fn check_functions(
        diags: &mut Vec<Diagnostic>,
        sema: &Semantic,
        file_id: FileId,
        match_spec: &[Vec<FunctionMatch>],
        use_range: UseRange,
    ) {
        sema.def_map(file_id).get_functions().for_each(|(_, def)| {
            check_function(diags, sema, def, match_spec.to_owned(), use_range)
        });
    }

    fn check_function(
        diags: &mut Vec<Diagnostic>,
        sema: &Semantic,
        def: &FunctionDef,
        match_spec: Vec<Vec<FunctionMatch>>,
        use_range: UseRange,
    ) {
        let matches = match_spec
            .into_iter()
            .flatten()
            .collect::<Vec<FunctionMatch>>();

        process_matches(diags, sema, def, &matches, use_range);
    }

    fn process_matches(
        diags: &mut Vec<Diagnostic>,
        sema: &Semantic,
        def: &FunctionDef,
        bad: &[FunctionMatch],
        use_range: UseRange,
    ) {
        let mfas = bad.iter().map(|b| (b, ())).collect::<Vec<_>>();
        find_call_in_function(
            diags,
            sema,
            def,
            &mfas,
            &[],
            &move |_ctx| Some("Diagnostic Message"),
            &move |ctx @ MatchCtx {
                       sema,
                       def_fb,
                       extra,
                       ..
                   }: MatchCtx<'_, &str>| {
                let diag_range = ctx.range(&use_range);
                if diag_range.file_id == def_fb.file_id() {
                    let diag = Diagnostic::new(
                        DiagnosticCode::AdHoc("test".to_string()),
                        *extra,
                        diag_range.range,
                    )
                    .with_severity(Severity::Warning)
                    .with_ignore_fix(sema, def_fb.file_id());
                    Some(diag)
                } else {
                    None
                }
            },
        );
    }

    #[track_caller]
    fn check_adhoc_function_match(match_spec: &[Vec<FunctionMatch>], fixture: &str) {
        check_diagnostics_with_config_and_ad_hoc(
            DiagnosticsConfig::default()
                .disable(DiagnosticCode::CrossNodeEval)
                .disable(DiagnosticCode::UndefinedFunction),
            &vec![&|acc, sema, file_id, _ext| {
                check_functions(acc, sema, file_id, match_spec, UseRange::WithArgs)
            }],
            fixture,
        );
    }

    #[track_caller]
    fn check_adhoc_function_match_range_mf(match_spec: &[Vec<FunctionMatch>], fixture: &str) {
        check_diagnostics_with_config_and_ad_hoc(
            DiagnosticsConfig::default()
                .disable(DiagnosticCode::CrossNodeEval)
                .disable(DiagnosticCode::UndefinedFunction),
            &vec![&|acc, sema, file_id, _ext| {
                check_functions(acc, sema, file_id, match_spec, UseRange::NameOnly)
            }],
            fixture,
        );
    }

    #[track_caller]
    fn check_adhoc_function_fix(
        match_spec: &[Vec<FunctionMatch>],
        fixture_before: &str,
        fixture_after: Expect,
    ) {
        check_fix_with_config_and_adhoc(
            DiagnosticsConfig::default()
                .disable(DiagnosticCode::CrossNodeEval)
                .disable(DiagnosticCode::UndefinedFunction),
            &vec![&|acc, sema, file_id, _ext| {
                check_functions(acc, sema, file_id, match_spec, UseRange::WithArgs)
            }],
            fixture_before,
            fixture_after,
        );
    }

    // -----------------------------------------------------------------

    #[test]
    fn find_call_in_function_1() {
        check_adhoc_function_match(
            &[FunctionMatch::mfas("foo", "fire_bombs", vec![1, 2])],
            r#"
            -module(main).

            bar(Config) ->
                foo:fire_bombs(Config),
            %%  ^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
                foo:fire_bombs(Config, zz).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
            "#,
        )
    }

    #[test]
    fn find_call_in_function_1a() {
        check_adhoc_function_match_range_mf(
            &[FunctionMatch::mfas("foo", "fire_bombs", vec![1, 2])],
            r#"
            -module(main).

            bar(Config) ->
                foo:fire_bombs(Config),
            %%  ^^^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
                foo:fire_bombs(Config, zz).
            %%  ^^^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
            "#,
        )
    }

    #[test]
    fn find_call_in_function_full_range_for_macro() {
        check_adhoc_function_match_range_mf(
            &[FunctionMatch::mfas("foo", "fire_bombs", vec![1])],
            r#"
            -module(main).

            -define(MY_MACRO(A), fun() -> foo:fire_bombs(A) end).

            bar(Config) ->
                ?MY_MACRO(Config).
            %%  ^^^^^^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
            "#,
        )
    }

    #[test]
    fn find_call_in_function_full_range_for_remote_macro() {
        check_adhoc_function_match_range_mf(
            &[FunctionMatch::mfas("foo", "fire_bombs", vec![1])],
            r#"
            //- /src/main.erl
            -module(main).

            -include("inc.hrl").

            bar(Config) ->
                ?MY_MACRO(Config).
            %%  ^^^^^^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message

            //- /src/inc.hrl
            -define(MY_MACRO(A), fun() -> foo:fire_bombs(A) end).
            "#,
        )
    }

    #[test]
    fn find_call_in_function_full_mf_auto_erlang() {
        check_adhoc_function_match_range_mf(
            &[FunctionMatch::mfas("erlang", "spawn", vec![2, 4])],
            r#"
            -module(main).

            bar(Node) ->
                spawn(Node, mod, fff, []).
            %%  ^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
            "#,
        )
    }

    #[test]
    fn find_call_in_function_erlang_module() {
        check_adhoc_function_match(
            &[FunctionMatch::mfas("erlang", "spawn", vec![2, 4])],
            r#"
            -module(main).

            bar(Node) ->
                erlang:spawn(fun() -> ok end),
                spawn(fun() -> ok end),
                erlang:spawn(Node, fun() -> ok end),
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
                spawn(Node, fun() -> ok end),
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
                erlang:spawn(Node, mod, fff, []),
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
                spawn(Node, mod, fff, []).
            %%  ^^^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
            "#,
        )
    }

    #[test]
    fn find_call_in_function_erlang_module_2() {
        check_adhoc_function_match(
            // Not actually in the erlang module
            &[FunctionMatch::mfas("erlang", "spawn", vec![0])],
            r#"
            -module(main).

            bar() ->
                erlang:spawn(),
            %%  ^^^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
                spawn().
            "#,
        )
    }

    #[test]
    fn find_call_module_function() {
        check_adhoc_function_match(
            &[vec![FunctionMatch::mf("foo", "bar")]],
            r#"
            -module(main).

            bar() ->
                foo:bar(),
            %%  ^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
                foo:bar(x),
            %%  ^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
                foo:bar(x,y),
            %%  ^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
                baz:bar().
            "#,
        )
    }

    #[test]
    fn find_call_module_only() {
        check_adhoc_function_match(
            &[vec![FunctionMatch::m("foo")]],
            r#"
            -module(main).

            bar() ->
                foo:bar(),
            %%  ^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
                baz:bar(x),
                foo:florgle(x,y).
            %%  ^^^^^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
            "#,
        )
    }

    #[test]
    fn find_call_any() {
        check_adhoc_function_match(
            &[vec![FunctionMatch::Any]],
            r#"
            -module(main).

            bar() ->
                foo:bar(),
            %%  ^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
                baz:bar(x),
            %%  ^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
                local(x,y).
            %%  ^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
            local(A,B) -> {A,B}.
            "#,
        )
    }

    #[test]
    fn ignore_fix_1() {
        check_adhoc_function_fix(
            &[vec![FunctionMatch::m("foo")]],
            r#"
            -module(main).

            bar() ->
                fo~o:bar().
            %%  ^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
             "#,
            expect![[r#"
            -module(main).

            bar() ->
                % elp:ignore ad-hoc: test (ad-hoc: test)
                foo:bar().
             "#]],
        );
    }

    #[test]
    fn ignore_fix_2() {
        check_adhoc_function_fix(
            &[vec![FunctionMatch::m("rpc")]],
            r#"
            -module(main).
            foo(Node, M,F,A) ->
               rpc:c~all(Node, M, F, A).
            %% ^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
             "#,
            expect![[r#"
            -module(main).
            foo(Node, M,F,A) ->
               % elp:ignore ad-hoc: test (ad-hoc: test)
               rpc:call(Node, M, F, A).
             "#]],
        );
    }

    #[test]
    fn ignore_fix_3() {
        check_adhoc_function_fix(
            &[vec![FunctionMatch::m("rpc")]],
            r#"
            -module(main).
            foo(Node, M,F,A) ->
               baz(rpc:c~all(Node, M, F, A)).
            %%     ^^^^^^^^^^^^^^^^^^^^^^^ 💡 warning: ad-hoc: test: Diagnostic Message
             "#,
            expect![[r#"
            -module(main).
            foo(Node, M,F,A) ->
               % elp:ignore ad-hoc: test (ad-hoc: test)
               baz(rpc:call(Node, M, F, A)).
             "#]],
        );
    }

    // -------------------------------------------------------------------

    #[track_caller]
    fn check_type(fixture: &str) {
        let (db, fixture) = RootDatabase::with_fixture(fixture);
        let expected = fixture.annotations();
        let file_id = fixture.file_id();
        let host = AnalysisHost { db };
        let sema = Semantic::new(&host.db);
        if expected.len() != 1 {
            panic!("Expected exactly one annotation, got {:?}", &expected);
        }

        let analysis = host.analysis();
        let diagnostics = fixture::diagnostics_for(
            &analysis,
            file_id,
            &DiagnosticsConfig::default(),
            &vec![],
            &fixture.diagnostics_enabled,
        );
        assert!(diagnostics.is_empty());
        let file_syntax = host.db.parse(file_id).syntax_node();
        let val: ast::Expr = find_node_at_offset(&file_syntax, fixture.position().offset).unwrap();
        let type_info = sema
            .to_expr(InFile::new(file_id, &val))
            .map(|in_clause| sema.expr_type(&in_clause.body(), &in_clause.value).unwrap())
            .or_else(|| {
                sema.to_pat(InFile::new(file_id, &val))
                    .map(|in_clause| sema.pat_type(&in_clause.body(), &in_clause.value).unwrap())
            });
        if let Some(type_info) = type_info {
            let type_str = format!("{type_info}");
            if type_str != expected[0].1 {
                panic!("Expected '{}', got '{}'", expected[0].1, type_str);
            }
        } else {
            panic!("could not get type info")
        }
    }

    // -----------------------------------------------------------------

    #[test]
    fn get_type_atom() {
        if otp_supported_by_eqwalizer() {
            check_type(
                r#"
            //- eqwalizer
            //- /play/src/bar1.erl app:play
                -module(bar1).

                -spec baz(atom()) -> atom().
                baz(FF) -> F~F,
            %%             ^^ atom()
                  something_else.
            "#,
            )
        }
    }

    #[test]
    fn get_type_custom() {
        if otp_supported_by_eqwalizer() {
            check_type(
                r#"
            //- eqwalizer
            //- /play/src/bar2.erl app:play
                -module(bar2).

                -type foo() :: foo1 | foo2.

                -spec get_foo() -> foo().
                get_foo() -> foo1.

                baz() -> F~F = get_foo().
            %%           ^^ bar2:foo()
            "#,
            )
        }
    }

    #[test]
    fn get_type_string() {
        if otp_supported_by_eqwalizer() {
            check_type(
                r#"
            //- eqwalizer
            //- /play/src/bar3.erl app:play
                -module(bar3).

                -spec get_foo() -> string().
                get_foo() -> "hello".

                baz() -> F~F = get_foo().
            %%           ^^ erlang:string()
            "#,
            )
        }
    }

    #[test]
    fn include_file_tracking() {
        if otp_supported_by_eqwalizer() {
            check_type(
                r#"
            //- eqwalizer
            //- /play/src/bar4.erl app:play
                -module(bar4).
                -include("level1.hrl").

                -spec get_foo() -> ?STRING().
                get_foo() -> "hello".

                baz() -> F~F = get_foo().
            %%           ^^ erlang:string()

            //- /play/src/level1.hrl app:play
            -include("level2.hrl").

            //- /play/src/level2.hrl app:play
            -define(STRING(), string()).
            "#,
            )
        }
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use anyhow::Context;
use anyhow::Result;
use anyhow::bail;
use elp_syntax::SmolStr;
use elp_syntax::ast;
use fxhash::FxHashMap;

/// Signature key used to index dynamic call patterns by
/// `(caller_module, caller_function, caller_arity)`.
///
/// Uses `SmolStr` so that cloning and constructing keys at lookup time
/// is cheap (short module/function names are inlined, longer ones share
/// an `Arc`).
pub type PatternKey = (Option<SmolStr>, SmolStr, u32);

/// Returns the `(module, function, arity)` signature key for a pattern.
pub fn pattern_key(p: &DynamicCallPatternInput) -> PatternKey {
    (
        p.caller_module.clone(),
        p.caller_function.clone(),
        p.caller_arity,
    )
}

/// Build the (signature → pattern) index for a list of patterns.
///
/// Two patterns sharing the same `(module, function, arity)` signature
/// but with opposite `module_arg_shape` (one `Atom`, one `List`) are
/// merged into a single entry whose shape is `Either`.
pub fn build_index(
    patterns: Vec<DynamicCallPatternInput>,
) -> FxHashMap<PatternKey, DynamicCallPatternInput> {
    let mut index: FxHashMap<PatternKey, DynamicCallPatternInput> = FxHashMap::default();
    for p in patterns {
        let key = pattern_key(&p);
        match index.get_mut(&key) {
            None => {
                index.insert(key, p);
            }
            Some(existing) => {
                if existing.module_arg_shape != p.module_arg_shape {
                    existing.module_arg_shape = ModuleArgShape::Either;
                }
            }
        }
    }
    index
}

/// How the module argument is expressed at a pattern's call site.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleArgShape {
    /// The module is a bare atom (e.g. `meck:new(Module)`).
    Atom,
    /// The module is a list of atoms (e.g. `meck:new([Module])`).
    List,
    /// Either form is accepted (e.g. `meck:new` accepts both an atom and
    /// a list at runtime).
    Either,
}

impl ModuleArgShape {
    /// Returns `true` if a list-form call site (`[Module]`) is accepted.
    pub fn allows_list(self) -> bool {
        matches!(self, ModuleArgShape::List | ModuleArgShape::Either)
    }

    /// Returns `true` if an atom-form call site (`Module`) is accepted.
    pub fn allows_atom(self) -> bool {
        matches!(self, ModuleArgShape::Atom | ModuleArgShape::Either)
    }
}

/// A parsed representation of a dynamic call pattern (e.g., `rpc:call(Node, Module, Function, Args)`).
/// Used to describe both built-in OTP patterns and user-configured patterns from `.elp_lint.toml`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DynamicCallPatternInput {
    /// Module part of the caller (`None` for unqualified calls like `apply/2`)
    pub caller_module: Option<SmolStr>,
    /// Function name of the caller
    pub caller_function: SmolStr,
    /// Arity of the caller (inferred from arg count)
    pub caller_arity: u32,
    /// 0-based index of the `Module` argument
    pub module_arg_index: Option<u32>,
    /// Whether the module argument is expressed as a bare atom, a list,
    /// or both (`Either`). The parser only produces `Atom` or `List`
    /// from a single pattern string; `Either` is constructed when two
    /// patterns with opposite shapes are merged for the same signature.
    pub module_arg_shape: ModuleArgShape,
    /// 0-based index of the `Function` argument (`None` for module-arg-only patterns)
    pub function_arg_index: Option<u32>,
    /// 0-based index of the `Args`/`Arity` argument (`None` for module-arg-only patterns)
    pub args_list_index: Option<u32>,
    /// `true` if `Arity` was used (direct integer), `false` if `Args` (list length)
    pub direct_arity: bool,
}

/// Parse the input string as a (possibly qualified) call expression using ELP's
/// tree-sitter parser. Returns the optional module name (for qualified calls
/// like `erlang:apply(...)`) and the inner `Call` node containing the function
/// atom and arguments.
///
/// Note: ELP's grammar nests `Module:Function(Args)` as `Remote { module, fun:
/// Call { atom, args } }` — i.e. the outer node is `Remote`, not `Call`. So we
/// have to peel the `Remote` here to get at the inner `Call`.
fn parse_call_expr(input: &str) -> Result<(Option<SmolStr>, ast::Call)> {
    let parse = ast::SourceFile::parse_text(input);

    if !parse.errors().is_empty() {
        let msgs: Vec<String> = parse.errors().iter().map(|e| format!("{e}")).collect();
        bail!("syntax error: {}", msgs.join(", "));
    }

    let expr = parse
        .tree()
        .exprs()
        .next()
        .context("failed to parse expression")?;

    match expr {
        ast::Expr::Call(call) => Ok((None, call)),
        ast::Expr::Remote(remote) => {
            let module_expr = remote
                .module()
                .and_then(|rm| rm.module())
                .context("expected atom for module name")?;
            let module_name = match module_expr {
                ast::Expr::ExprMax(ast::ExprMax::Atom(ref a)) => atom_smol(a)?,
                _ => bail!("expected atom for module name"),
            };
            match remote.fun() {
                Some(ast::Expr::Call(call)) => Ok((Some(module_name), call)),
                _ => bail!("expected a function call after module qualifier"),
            }
        }
        _ => bail!("expected a function call"),
    }
}

fn atom_smol(atom: &ast::Atom) -> Result<SmolStr> {
    atom.text()
        .map(SmolStr::new)
        .context("failed to read atom text")
}

/// Parse a dynamic call pattern string like `"rpc:call(_, Module, Function, Args)"`.
///
/// Format: `[module:]function(Arg1, Arg2, ...)`
/// - `Module` → argument position containing the target module name
/// - `[Module]` → argument position containing a list of target module names
/// - `Function` → argument position containing the target function name
/// - `Args` → argument position containing the list of arguments (arity = list length)
/// - `Arity` → argument position containing the arity as a direct integer
/// - `_` (or any `_`-prefixed variable) → ignored placeholder
///
/// Any other variable name or expression form is rejected as invalid.
///
/// Validation:
/// - `Function` and `Args`/`Arity` must both be present or both absent
/// - If both absent, at least `Module` or `[Module]` must be present (module-arg-only pattern)
/// - At most one `Module`/`[Module]`, one `Function`, one of `Args`/`Arity`
pub fn parse_dynamic_call_pattern(input: &str) -> Result<DynamicCallPatternInput> {
    let input = input.trim();
    let (caller_module, call) = parse_call_expr(input)?;

    let caller_function = match call.expr() {
        Some(ast::Expr::ExprMax(ast::ExprMax::Atom(ref a))) => atom_smol(a)?,
        _ => bail!("expected a function call with atom name"),
    };

    let args_node = call.args().context("missing argument list")?;
    let args: Vec<ast::Expr> = args_node.args().collect();

    if args.is_empty() {
        bail!("pattern must have at least one argument");
    }

    let caller_arity = args.len() as u32;

    let mut module_arg_index: Option<u32> = None;
    let mut module_arg_shape = ModuleArgShape::Atom;
    let mut function_arg_index: Option<u32> = None;
    let mut args_list_index: Option<u32> = None;
    let mut direct_arity = false;

    for (i, arg) in args.iter().enumerate() {
        match arg {
            ast::Expr::ExprMax(ast::ExprMax::Var(var)) => {
                let name = var.text();
                match name.as_str() {
                    "Module" => {
                        if module_arg_index.is_some() {
                            bail!("multiple 'Module' arguments");
                        }
                        module_arg_index = Some(i as u32);
                        module_arg_shape = ModuleArgShape::Atom;
                    }
                    "Function" => {
                        if function_arg_index.is_some() {
                            bail!("multiple 'Function' arguments");
                        }
                        function_arg_index = Some(i as u32);
                    }
                    "Args" => {
                        if args_list_index.is_some() {
                            bail!("both 'Args' and 'Arity' present");
                        }
                        args_list_index = Some(i as u32);
                        direct_arity = false;
                    }
                    "Arity" => {
                        if args_list_index.is_some() {
                            bail!("both 'Args' and 'Arity' present");
                        }
                        args_list_index = Some(i as u32);
                        direct_arity = true;
                    }
                    other => {
                        if !other.starts_with('_') {
                            bail!(
                                "unexpected variable '{}' (expected 'Module', 'Function', 'Args', 'Arity', or a '_'-prefixed placeholder)",
                                other
                            );
                        }
                    }
                }
            }
            ast::Expr::ExprMax(ast::ExprMax::List(list)) => {
                let mut elems = list.exprs();
                if let Some(ast::Expr::ExprMax(ast::ExprMax::Var(var))) = elems.next()
                    && elems.next().is_none()
                    && var.text().as_str() == "Module"
                {
                    if module_arg_index.is_some() {
                        bail!("multiple 'Module' arguments");
                    }
                    module_arg_index = Some(i as u32);
                    module_arg_shape = ModuleArgShape::List;
                } else {
                    bail!("unexpected list argument (only '[Module]' is supported)");
                }
            }
            _ => bail!("unexpected argument (expected a variable or '[Module]' list)"),
        }
    }

    // Validate: Function and Args/Arity must both be present or both absent
    match (function_arg_index, args_list_index) {
        (Some(_), Some(_)) => {}
        (None, None) => {
            if module_arg_index.is_none() {
                bail!("pattern must have at least 'Module', '[Module]', or 'Function'");
            }
        }
        (Some(_), None) => {
            bail!("missing 'Args' or 'Arity' argument (required when 'Function' is present)");
        }
        (None, Some(_)) => {
            bail!("missing 'Function' argument (required when 'Args'/'Arity' is present)");
        }
    }

    Ok(DynamicCallPatternInput {
        caller_module,
        caller_function,
        caller_arity,
        module_arg_index,
        module_arg_shape,
        function_arg_index,
        args_list_index,
        direct_arity,
    })
}

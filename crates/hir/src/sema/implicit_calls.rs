/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Calls that a framework makes to a function named by atoms in the code.
//!
//! Some OTP and library behaviours call a function whose name only appears as
//! atoms, such as the `start` MFA of a supervisor child spec. Unlike a dynamic
//! call (see [`super::dynamic_calls`]), the code has no call to a dispatcher:
//! the framework makes the call. Each such convention implements
//! [`ImplicitCallPattern`] and is listed in [`PATTERNS`].
//!
//! The patterns run on the lowered body of each function clause, so macros
//! are expanded: `{?MODULE, start_link, []}` and child specs built by a macro
//! are found too. The result is cached per clause. They also run on the
//! replacement of a `-define`, for a child spec written out in a macro. An
//! implicit call names its target with a [`CallTarget`], like a dynamic call,
//! and is resolved the same way.

use std::slice;
use std::sync::Arc;

use elp_base_db::FileId;
use elp_syntax::AstNode;
use elp_syntax::AstPtr;
use elp_syntax::ast;

use crate::Body;
use crate::CallTarget;
use crate::Expr;
use crate::ExprId;
use crate::FormIdx;
use crate::FunctionClauseId;
use crate::FunctionDef;
use crate::InFile;
use crate::InFileAstPtr;
use crate::Module;
use crate::Name;
use crate::NameArity;
use crate::PPDirective;
use crate::Semantic;
use crate::db::DefDatabase;
use crate::known;
use crate::sema::to_def::resolve_call_target;

/// A call that a framework makes to a function named by atoms in a function
/// clause.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplicitCall {
    /// The called function. A local target is a function of the module that
    /// contains the clause.
    pub target: CallTarget<ExprId>,
    pub arity: u32,
}

impl ImplicitCall {
    /// The called function, if it exists. `body` is the body of the clause
    /// that contains the call, in `file_id`.
    pub fn resolve(&self, sema: &Semantic, file_id: FileId, body: &Body) -> Option<FunctionDef> {
        resolve_call_target(sema, &self.target, Some(self.arity), file_id, body)
    }
}

/// What an atom names in an implicit call.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImplicitCallReference {
    Module(Module),
    Function(FunctionDef),
}

/// A convention under which a framework calls a function named by atoms.
trait ImplicitCallPattern {
    /// The behaviour a module must declare for the pattern to apply to its
    /// functions. `None` applies the pattern to every module.
    fn behaviour(&self) -> Option<&'static Name> {
        None
    }

    /// Add the implicit calls named by `expr`, the expression `expr_id` of the
    /// body of `context`.
    fn find_calls(
        &self,
        context: &BodyContext,
        expr_id: ExprId,
        expr: &Expr,
        calls: &mut Vec<ImplicitCall>,
    );
}

/// The body that the patterns run on: a function clause, or the replacement
/// of a `-define`.
struct BodyContext<'a> {
    db: &'a dyn DefDatabase,
    file_id: FileId,
    /// The function of the clause; `None` for a `-define`.
    function: Option<&'a NameArity>,
    /// The expressions of the clause, or the replacement of a `-define`.
    exprs: &'a [ExprId],
    body: &'a Body,
}

/// The patterns, tried in order.
const PATTERNS: &[&dyn ImplicitCallPattern] = &[&SupervisorChildSpec, &GenStatemState];

pub(crate) fn function_clause_implicit_calls_dispatch(
    db: &dyn DefDatabase,
    clause_id: InFile<FunctionClauseId>,
) -> Arc<Vec<ImplicitCall>> {
    db.function_clause_implicit_calls_interned(crate::InternedInFileFunctionClause::new(
        db, clause_id,
    ))
}

pub(crate) fn function_clause_implicit_calls_inner(
    db: &dyn DefDatabase,
    fid: crate::InternedInFileFunctionClause,
) -> Arc<Vec<ImplicitCall>> {
    let clause_id = fid.value(db);
    let clause = db.function_clause_body(clause_id);
    let Some(function) = &clause.name else {
        return Arc::default();
    };
    let context = BodyContext {
        db,
        file_id: clause_id.file_id,
        function: Some(function),
        exprs: &clause.clause.exprs,
        body: &clause.body,
    };
    Arc::new(find_calls(&context))
}

/// The implicit calls in the body of `context`.
fn find_calls(context: &BodyContext) -> Vec<ImplicitCall> {
    let def_map = context.db.def_map(context.file_id);
    let patterns: Vec<_> = PATTERNS
        .iter()
        .filter(|pattern| {
            pattern
                .behaviour()
                .is_none_or(|behaviour| def_map.get_behaviours().contains(behaviour))
        })
        .collect();
    let mut calls = Vec::new();
    for (expr_id, expr) in context.body.exprs.iter() {
        for pattern in &patterns {
            pattern.find_calls(context, expr_id, expr, &mut calls);
        }
    }
    calls
}

/// The module or function that `atom` names in an implicit call of the
/// function clause or `-define` that contains it, if the called function
/// exists.
pub(crate) fn reference(
    sema: &Semantic,
    file_id: FileId,
    atom: &ast::Atom,
) -> Option<ImplicitCallReference> {
    let form = atom.syntax().ancestors().find_map(ast::Form::cast)?;
    let form_list = sema.db.file_form_list(file_id);
    let (calls, body, source_map) = match form_list.find_form(&form)? {
        FormIdx::FunctionClause(clause_id) => {
            let clause_id = InFile::new(file_id, clause_id);
            let calls = sema.db.function_clause_implicit_calls(clause_id);
            if calls.is_empty() {
                return None;
            }
            let (clause, source_map) = sema.db.function_clause_body_with_source(clause_id);
            (calls, clause.body.clone(), source_map)
        }
        FormIdx::PPDirective(directive) => {
            let PPDirective::Define(define_id) = form_list[directive] else {
                return None;
            };
            let (define, source_map) = sema
                .db
                .define_body_with_source(InFile::new(file_id, define_id));
            let context = BodyContext {
                db: sema.db,
                file_id,
                function: None,
                exprs: slice::from_ref(&define.expr),
                body: &define.body,
            };
            let calls = Arc::new(find_calls(&context));
            (calls, define.body.clone(), source_map)
        }
        _ => return None,
    };

    let atom_expr = ast::Expr::ExprMax(ast::ExprMax::Atom(atom.clone()));
    let atom_source = InFileAstPtr::new(file_id, AstPtr::new(&atom_expr));
    let names_atom = |expr: ExprId| source_map.expr(expr) == Some(atom_source);
    calls.iter().find_map(|call| {
        let (module, name) = match call.target {
            CallTarget::Local { name } => (None, name),
            CallTarget::Remote { module, name, .. } => (Some(module), name),
        };
        if let Some(module) = module.filter(|module| names_atom(*module)) {
            call.resolve(sema, file_id, &body)?;
            let module = sema.resolve_module_expr(file_id, &body[module])?;
            Some(ImplicitCallReference::Module(module))
        } else if names_atom(name) {
            let function = call.resolve(sema, file_id, &body)?;
            Some(ImplicitCallReference::Function(function))
        } else {
            None
        }
    })
}

/// MFA tuples in supervisor child specs.
/// Pattern: `#{id => Id, start => {Module, Function, Args}, ...}`, where every
/// key is a child spec key.
/// The framework calls `Module:Function/length(Args)`.
struct SupervisorChildSpec;

impl ImplicitCallPattern for SupervisorChildSpec {
    fn find_calls(
        &self,
        context: &BodyContext,
        _expr_id: ExprId,
        expr: &Expr,
        calls: &mut Vec<ImplicitCall>,
    ) {
        let body = context.body;
        let Expr::Map { fields } = expr else {
            return;
        };
        let start = fields
            .iter()
            .find(|(key, _)| atom_name(body, *key) == Some(*known::start));
        let Some((module, function, arity)) = start.and_then(|(_, value)| mfa(body, *value)) else {
            return;
        };
        if is_child_spec(body, fields) {
            calls.push(ImplicitCall {
                target: CallTarget::Remote {
                    module,
                    name: function,
                    parens: false,
                    unqualified: false,
                },
                arity,
            });
        }
    }
}

/// Whether the map with `fields` has an `id` key and only keys that a child
/// spec can have.
fn is_child_spec(body: &Body, fields: &[(ExprId, ExprId)]) -> bool {
    static CHILD_SPEC_KEYS: std::sync::LazyLock<Vec<Name>> = std::sync::LazyLock::new(|| {
        vec![
            *known::id,
            *known::start,
            *known::restart,
            *known::significant,
            *known::shutdown,
            *known::type_name,
            *known::modules,
        ]
    });
    let mut has_id = false;
    for (key, _) in fields {
        match atom_name(body, *key) {
            Some(key) if CHILD_SPEC_KEYS.contains(&key) => has_id |= key == *known::id,
            _ => return false,
        }
    }
    has_id
}

/// The module, function and arity of a `{Module, Function, Args}` tuple.
fn mfa(body: &Body, expr: ExprId) -> Option<(ExprId, ExprId, u32)> {
    let Expr::Tuple { exprs } = &body[unwrap(body, expr)] else {
        return None;
    };
    let [module, function, args] = exprs.as_slice() else {
        return None;
    };
    let arity = body[unwrap(body, *args)].list_length()?;
    Some((unwrap(body, *module), unwrap(body, *function), arity as u32))
}

/// State names of a `gen_statem` callback module in the `state_functions`
/// callback mode, where `gen_statem` calls `State(EventType, Event, Data)` for
/// the current state.
/// Pattern: the `State` atom in `{next_state, State, Data}` or
/// `{next_state, State, Data, Actions}` returned by a function, or in
/// `{ok, State, Data}` or `{ok, State, Data, Actions}` returned by `init/1`.
/// The framework calls `State/3` in the same module.
struct GenStatemState;

impl ImplicitCallPattern for GenStatemState {
    fn behaviour(&self) -> Option<&'static Name> {
        Some(&*known::gen_statem)
    }

    fn find_calls(
        &self,
        context: &BodyContext,
        expr_id: ExprId,
        expr: &Expr,
        calls: &mut Vec<ImplicitCall>,
    ) {
        let body = context.body;
        let Expr::Tuple { exprs } = expr else {
            return;
        };
        let ([tag, state, _] | [tag, state, _, _]) = exprs.as_slice() else {
            return;
        };
        let is_transition = match atom_name(body, *tag) {
            Some(tag) if tag == *known::next_state => true,
            Some(tag) if tag == *known::ok => {
                context.function == Some(&NameArity::new(*known::init, 1))
            }
            _ => false,
        };
        if !is_transition
            || !body.returned_exprs(context.exprs).contains(&expr_id)
            || atom_name(body, *state).is_none()
            || !uses_state_functions(context.db, context.file_id)
        {
            return;
        }
        calls.push(ImplicitCall {
            target: CallTarget::Local {
                name: unwrap(body, *state),
            },
            arity: 3,
        });
    }
}

/// Whether `callback_mode/0` in `file_id` returns `state_functions`, alone or
/// in a list.
fn uses_state_functions(db: &dyn DefDatabase, file_id: FileId) -> bool {
    let def_map = db.def_map(file_id);
    let Some(callback_mode) = def_map.get_function(&NameArity::new(*known::callback_mode, 0))
    else {
        return false;
    };
    let function = db.function_body(InFile::new(file_id, callback_mode.function_id));
    function.clauses.iter().any(|(_, clause)| {
        let body = &clause.body;
        let is_state_functions =
            |expr| atom_name(body, expr).is_some_and(|name| name == *known::state_functions);
        body.returned_exprs(&clause.clause.exprs)
            .into_iter()
            .any(|result| match &body[result] {
                Expr::List { exprs, .. } => exprs.iter().any(|&expr| is_state_functions(expr)),
                _ => is_state_functions(result),
            })
    })
}

/// The expression that `expr` stands for, through parentheses and macro
/// calls.
fn unwrap(body: &Body, mut expr: ExprId) -> ExprId {
    loop {
        match &body[expr] {
            Expr::Paren { expr: inner }
            | Expr::MacroCall {
                expansion: inner, ..
            } => expr = *inner,
            _ => return expr,
        }
    }
}

fn atom_name(body: &Body, expr: ExprId) -> Option<Name> {
    body[unwrap(body, expr)]
        .as_atom()
        .map(|atom| atom.as_name())
}

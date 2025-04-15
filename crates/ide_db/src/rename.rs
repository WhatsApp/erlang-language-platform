/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Rename infrastructure for ELP. It is used primarily for the
//! literal "rename" in the ide (look for tests there), but it is also
//! available as a general-purpose service.

use std::fmt;
use std::iter::once;

use elp_base_db::FileId;
use elp_base_db::FileRange;
use elp_syntax::AstNode;
use elp_syntax::ast;
use elp_syntax::ast::in_erlang_module;
use hir::InFile;
use hir::Semantic;
use text_edit::TextEdit;

use crate::SymbolDefinition;
use crate::helpers::get_call;
use crate::search::NameLike;
use crate::source_change::SourceChange;

pub type RenameResult<T> = Result<T, RenameError>;

#[derive(Debug)]
pub struct RenameError(pub String);

impl fmt::Display for RenameError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

#[macro_export]
macro_rules! _format_err {
    ($fmt:expr) => { RenameError(format!($fmt)) };
    ($fmt:expr, $($arg:tt)+) => { RenameError(format!($fmt, $($arg)+)) }
}
pub use _format_err as format_err;

#[macro_export]
macro_rules! _rename_error {
    ($($tokens:tt)*) => { return Err(format_err!($($tokens)*)) }
}
pub use _rename_error as rename_error;

// ---------------------------------------------------------------------

// Delegate checking name validity to the parser
pub fn is_valid_var_name(new_name: &String) -> bool {
    let parse = ast::SourceFile::parse_text(format!("foo() -> {} = 1.", new_name).as_str());
    parse.tree().syntax().descendants().any(|node| {
        if let Some(var) = ast::Var::cast(node) {
            var.syntax().text().to_string() == *new_name
        } else {
            false
        }
    })
}

// Delegate checking name validity to the parser
pub fn is_valid_function_name(new_name: &String) -> bool {
    let parse = ast::SourceFile::parse_text(format!("{}() -> ok.", new_name).as_str());
    match parse.tree().forms().next() {
        Some(ast::Form::FunDecl(fun)) => match fun.name() {
            Some(ast::Name::Atom(atom)) => atom.syntax().text().to_string() == *new_name,
            _ => false,
        },
        _ => false,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SafetyChecks {
    Yes,
    No, // May be more specific at a future date.
}

impl SymbolDefinition {
    pub fn rename(
        &self,
        sema: &Semantic,
        new_name: &String,
        parens_needed_in_context: &dyn Fn(&ast::Name) -> bool,
        safety_check: SafetyChecks,
    ) -> RenameResult<SourceChange> {
        match self.clone() {
            SymbolDefinition::Module(_) => {
                rename_error!("Cannot rename module")
            }
            SymbolDefinition::Function(fun) => {
                if safety_check == SafetyChecks::Yes && !is_valid_function_name(new_name) {
                    rename_error!("Invalid new function name: '{}'", new_name);
                }

                let arity = fun.name.arity();
                if safety_check == SafetyChecks::Yes
                    && !is_safe_function(sema, fun.file.file_id, new_name, arity)
                {
                    rename_error!("Function '{}/{}' already in scope", new_name, arity);
                } else {
                    self.rename_reference(sema, new_name, parens_needed_in_context, safety_check)
                }
            }
            SymbolDefinition::Record(_) => {
                rename_error!("Cannot rename record")
            }
            SymbolDefinition::RecordField(_) => {
                rename_error!("Cannot rename record field")
            }
            SymbolDefinition::Type(_) => {
                rename_error!("Cannot rename type")
            }
            SymbolDefinition::Callback(_) => {
                rename_error!("Cannot rename callback")
            }
            SymbolDefinition::Define(_) => {
                rename_error!("Cannot rename define")
            }
            SymbolDefinition::Header(_) => {
                rename_error!("Cannot rename header")
            }
            SymbolDefinition::Var(_) => {
                if safety_check == SafetyChecks::Yes && !is_valid_var_name(new_name) {
                    rename_error!("Invalid new variable name: '{}'", new_name);
                }

                self.rename_reference(sema, new_name, parens_needed_in_context, safety_check)
            }
        }
    }

    /// Textual range of the identifier which will change when
    /// renaming this `Definition`. Note that some definitions, like
    /// builtin types, can't be renamed.
    pub fn range_for_rename(self, sema: &Semantic) -> Option<FileRange> {
        let res = match self {
            SymbolDefinition::Var(v) => {
                let range = v.source(sema.db.upcast()).syntax().text_range();
                Some(FileRange {
                    file_id: v.file.file_id,
                    range,
                })
            }
            _ => None,
        };
        res
    }

    fn rename_reference(
        &self,
        sema: &Semantic,
        new_name: &String,
        parens_needed_in_context: &dyn Fn(&ast::Name) -> bool,
        safety_check: SafetyChecks,
    ) -> RenameResult<SourceChange> {
        let file_id = self.file().file_id;
        let mut source_change = SourceChange::default();
        match self {
            SymbolDefinition::Function(function) => {
                let usages = self.clone().usages(sema).all();
                let mut def_usages = Vec::default();
                let funs = function.source(sema.db.upcast());
                for fun in funs {
                    if let Some(ast::FunctionOrMacroClause::FunctionClause(fc)) = fun.clause() {
                        if let Some(name) = fc.name() {
                            def_usages.push(NameLike::Name(name));
                        }
                    };
                }
                if safety_check == SafetyChecks::Yes {
                    // We have already checked the function safe in
                    // its defining file, check remote references
                    // now.
                    let arity = function.name.arity();
                    let mut problems = usages.iter().filter(|(file_id, refs)| {
                        (file_id != &function.file.file_id)
                            && !is_safe_remote_function(sema, *file_id, new_name, arity, *refs)
                    });
                    // Report the first one only, an existence proof of problems
                    if let Some((file_id, _)) = problems.next() {
                        {
                            if let Some(module_name) = sema.module_name(file_id) {
                                rename_error!(
                                    "Function '{}/{}' already in scope in module '{}'",
                                    new_name,
                                    arity,
                                    module_name.as_str()
                                );
                            } else {
                                rename_error!("Function '{}/{}' already in scope", new_name, arity);
                            }
                        }
                    };
                }

                let usages: Vec<_> = usages
                    .iter()
                    .chain(once((file_id, &def_usages[..])))
                    .collect();

                source_edit_from_usages(
                    &mut source_change,
                    usages,
                    new_name,
                    parens_needed_in_context,
                );
                Ok(source_change)
            }
            SymbolDefinition::Var(var) => {
                let usages = sema
                    .find_local_usages_ast(InFile {
                        file_id,
                        value: &var.source(sema.db.upcast()),
                    })
                    .unwrap_or_default();
                let def_usages = usages.iter().map(|v| v.clone().into()).collect::<Vec<_>>();
                let usages = vec![(file_id, &def_usages[..])];

                let infile_var = InFile {
                    file_id: var.file.file_id,
                    value: &var.source(sema.db.upcast()),
                };
                if safety_check == SafetyChecks::Yes {
                    if !is_safe_var_usages(sema, infile_var, &usages, &new_name) {
                        rename_error!("Name '{}' already in scope", new_name);
                    }

                    if !is_safe_var_anonymous(infile_var) {
                        rename_error!("Cannot rename '_'");
                    }
                }

                let (file_id, edit) = source_edit_from_def(sema, self.clone(), new_name)?;
                source_change.insert_source_edit(file_id, edit);
                source_edit_from_usages(
                    &mut source_change,
                    usages,
                    new_name,
                    parens_needed_in_context,
                );
                Ok(source_change)
            }
            // Note: This is basically an internal error, this function is called from
            // SymbolDefinition::rename which already weeds them out
            _ => {
                rename_error!("rename reference not supported for {:?}", self);
            }
        }
    }
}

fn source_edit_from_usages(
    source_change: &mut SourceChange,
    usages: Vec<(FileId, &[NameLike])>,
    new_name: &String,
    parens_needed_in_context: &dyn Fn(&ast::Name) -> bool,
) {
    source_change.extend(usages.into_iter().map(|(file_id, references)| {
        (
            file_id,
            source_edit_from_references(
                &references
                    .iter()
                    .filter_map(|n| match n {
                        NameLike::Name(n) => Some(n.clone()),
                        NameLike::String(_) => None,
                    })
                    .collect::<Vec<_>>(),
                new_name,
                parens_needed_in_context,
            ),
        )
    }));
}

pub fn source_edit_from_references(
    references: &[ast::Name],
    new_name: &String,
    parens_needed_in_context: &dyn Fn(&ast::Name) -> bool,
) -> TextEdit {
    let mut edit = TextEdit::builder();
    let mut edited_ranges = Vec::new();
    for name in references {
        let new_name = if parens_needed_in_context(name) {
            format!("({})", new_name)
        } else {
            new_name.clone()
        };
        let range = name.syntax().text_range();
        edit.replace(range, new_name.to_string());
        edited_ranges.push(range.start());
    }

    edit.finish()
}

fn source_edit_from_def(
    sema: &Semantic,
    def: SymbolDefinition,
    new_name: &String,
) -> RenameResult<(FileId, TextEdit)> {
    let FileRange { file_id, range } = def
        .range_for_rename(sema)
        .ok_or_else(|| format_err!("No identifier available to rename"))?;

    let mut edit = TextEdit::builder();
    if edit.is_empty() {
        edit.replace(range, new_name.clone());
    }
    Ok((file_id, edit.finish()))
}

/// Check that the new variable name is not in scope already at any
/// of the usage locations.
fn is_safe_var_usages(
    sema: &Semantic,
    var_in: InFile<&ast::Var>,
    usages: &[(FileId, &[NameLike])],
    new_name: &str,
) -> bool {
    usages.iter().all(|(file_id, name_like)| {
        if var_in.file_id == *file_id {
            let vars: Vec<ast::Var> = name_like
                .iter()
                .filter_map(|name_like| match name_like {
                    NameLike::Name(ast::Name::Var(var)) => Some(var.clone()),
                    _ => None,
                })
                .collect();
            is_safe_var(sema, var_in, &vars, new_name)
        } else {
            false
        }
    })
}

fn is_safe_var(
    sema: &Semantic,
    var_in: InFile<&ast::Var>,
    usages: &[ast::Var],
    new_name: &str,
) -> bool {
    usages.iter().all(|var: &ast::Var| {
        let var_in = InFile {
            file_id: var_in.file_id,
            value: var,
        };
        if let Some((resolver_var, scope_var)) = sema.scope_for(var_in) {
            let name_ok = resolver_var
                .all_vars_in_scope(scope_var)
                .iter()
                .all(|&var| sema.db.lookup_var(var) != new_name);
            name_ok
        } else {
            false
        }
    })
}

fn is_safe_var_anonymous(var_in: InFile<&ast::Var>) -> bool {
    if var_in.value.text() == "_" {
        // Do not rename `_`
        // TODO: This test will not use text when T135585863 is done
        false
    } else {
        true
    }
}

/// Check that the new function name is not in scope already.  This
/// includes checking for auto-included functions from the `erlang`
/// module.
pub fn is_safe_function(sema: &Semantic, file_id: FileId, new_name: &str, arity: u32) -> bool {
    let scope_ok = sema
        .db
        .def_map_local(file_id)
        .get_functions_in_scope()
        .all(|(name, _)| !(&name.name().to_string() == new_name && name.arity() == arity));

    scope_ok && !in_erlang_module(new_name, arity as usize)
}

/// Check that the new function name is not in scope already in the
/// module via an explicit import.
pub fn is_safe_remote_function(
    sema: &Semantic,
    file_id: FileId,
    new_name: &str,
    arity: u32,
    refs: &[NameLike],
) -> bool {
    // Problem occurs if the usage is not qualified with a module name
    let all_remote = refs.iter().all(|name_like| {
        if let Some(call) = get_call(&name_like.syntax()) {
            if let Some(ast::Expr::Remote(_)) = call.expr() {
                true
            } else {
                false
            }
        } else {
            false
        }
    });

    all_remote || is_safe_function(sema, file_id, new_name, arity)
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Rename infrastructure for ELP. It is used primarily for the
//! literal "rename" in the ide (look for tests there), but it is also
//! available as a general-purpose service.

use std::fmt;
use std::iter::once;

use elp_base_db::AnchoredPathBuf;
use elp_base_db::FileId;
use elp_base_db::FileRange;
use elp_base_db::ModuleName;
use elp_syntax::AstNode;
use elp_syntax::ast;
use elp_syntax::ast::in_erlang_module;
use hir::InFile;
use hir::Semantic;

use crate::SymbolDefinition;
use crate::helpers::get_call;
use crate::search::NameLike;
use crate::source_change::FileSystemEdit;
use crate::source_change::SourceChange;
use crate::text_edit::TextEdit;
use crate::text_edit::TextEditBuilder;

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
    let parse = ast::SourceFile::parse_text(format!("foo() -> {new_name} = 1.").as_str());
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
    let parse = ast::SourceFile::parse_text(format!("{new_name}() -> ok.").as_str());
    match parse.tree().forms().next() {
        Some(ast::Form::FunDecl(fun)) => match fun.name() {
            Some(ast::Name::Atom(atom)) => atom.syntax().text().to_string() == *new_name,
            _ => false,
        },
        _ => false,
    }
}

// Delegate checking macro name validity to the parser
// Macros can be either atoms or variables
pub fn is_valid_macro_name(new_name: &String) -> bool {
    let parse = ast::SourceFile::parse_text(format!("-define({new_name}, value).").as_str());
    matches!(
        parse.tree().forms().next(),
        Some(ast::Form::PreprocessorDirective(
            ast::PreprocessorDirective::PpDefine(_)
        ))
    )
}

// Delegate checking type name validity to the parser
pub fn is_valid_type_name(new_name: &String) -> bool {
    let parse = ast::SourceFile::parse_text(format!("-type {new_name}() :: ok.").as_str());
    // Check that we got a TypeAlias form
    if let Some(ast::Form::TypeAlias(type_alias)) = parse.tree().forms().next() {
        // Check that the name is an atom (not a variable)
        if let Some(type_name) = type_alias.name()
            && let Some(ast::Name::Atom(_)) = type_name.name()
        {
            return true;
        }
    }
    false
}

// Delegate checking module name validity to the parser
pub fn is_valid_module_name(new_name: &String) -> bool {
    let parse = ast::SourceFile::parse_text(format!("-module({}).", new_name).as_str());
    match parse.tree().forms().next() {
        Some(ast::Form::ModuleAttribute(ma)) => match ma.name() {
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
                if safety_check == SafetyChecks::Yes && !is_valid_module_name(new_name) {
                    rename_error!("Invalid new module name: '{}'", new_name);
                }
                self.rename_module(sema, new_name, safety_check)
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
            SymbolDefinition::Type(type_alias) => {
                if safety_check == SafetyChecks::Yes && !is_valid_type_name(new_name) {
                    rename_error!("Invalid new type name: '{}'", new_name);
                }

                let arity = type_alias.name().arity();
                if safety_check == SafetyChecks::Yes {
                    // Check safety in the file where the type is defined
                    if !is_safe_type(sema, type_alias.file.file_id, new_name, arity) {
                        rename_error!("Type '{}/{}' already in scope", new_name, arity);
                    }

                    // Also check safety in all files where the type is used
                    let usages = self.clone().usages(sema).all();
                    for (file_id, _refs) in usages.iter() {
                        if file_id != type_alias.file.file_id
                            && !is_safe_type(sema, file_id, new_name, arity)
                        {
                            rename_error!("Type '{}/{}' already in scope", new_name, arity);
                        }
                    }
                }

                self.rename_reference(sema, new_name, parens_needed_in_context, safety_check)
            }
            SymbolDefinition::Callback(_) => {
                rename_error!("Cannot rename callback")
            }
            SymbolDefinition::Define(define) => {
                if safety_check == SafetyChecks::Yes && !is_valid_macro_name(new_name) {
                    rename_error!("Invalid new macro name: '{}'", new_name);
                }

                let arity = define.define.name.arity();
                if safety_check == SafetyChecks::Yes {
                    // Check safety in the file where the macro is defined
                    if !is_safe_macro(sema, define.file.file_id, new_name, arity) {
                        rename_error!("Macro '{}' already in scope", new_name);
                    }

                    // Also check safety in all files where the macro is used
                    let usages = self.clone().usages(sema).all();
                    for (file_id, _refs) in usages.iter() {
                        if file_id != define.file.file_id
                            && !is_safe_macro(sema, file_id, new_name, arity)
                        {
                            rename_error!("Macro '{}' already in scope", new_name);
                        }
                    }
                }

                self.rename_reference(sema, new_name, parens_needed_in_context, safety_check)
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
        match self {
            SymbolDefinition::Var(v) => {
                let range = v.source(sema.db.upcast()).syntax().text_range();
                Some(FileRange {
                    file_id: v.file.file_id,
                    range,
                })
            }
            SymbolDefinition::Define(d) => {
                // Get the macro definition location
                let source = d.source(sema.db.upcast());
                if let Some(name) = source.name() {
                    let range = name.syntax().text_range();
                    Some(FileRange {
                        file_id: d.file.file_id,
                        range,
                    })
                } else {
                    None
                }
            }
            SymbolDefinition::Type(t) => {
                // Get the type definition location
                let range = t.name_range(sema.db.upcast())?;
                Some(FileRange {
                    file_id: t.file.file_id,
                    range,
                })
            }
            _ => None,
        }
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
                    if let Some(ast::FunctionOrMacroClause::FunctionClause(fc)) = fun.clause()
                        && let Some(name) = fc.name()
                    {
                        def_usages.push(NameLike::Name(name));
                    };
                }
                if safety_check == SafetyChecks::Yes {
                    // We have already checked the function safe in
                    // its defining file, check remote references
                    // now.
                    let arity = function.name.arity();
                    let mut problems = usages.iter().filter(|(file_id, refs)| {
                        (file_id != &function.file.file_id)
                            && !is_safe_remote_function(sema, *file_id, new_name, arity, refs)
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
            SymbolDefinition::Define(_define) => {
                // Find all usages of the macro
                let usages = self.clone().usages(sema).all();

                // Also need to rename the definition itself
                // Get the macro definition location
                let (file_id, def_edit) = source_edit_from_def(sema, self.clone(), new_name)?;
                source_change.insert_source_edit(file_id, def_edit);

                source_edit_from_usages(
                    &mut source_change,
                    usages.iter().collect(),
                    new_name,
                    parens_needed_in_context,
                );
                Ok(source_change)
            }
            SymbolDefinition::Type(_type_alias) => {
                // Find all usages of the type
                let usages = self.clone().usages(sema).all();

                // Also need to rename the definition itself
                // Get the type definition location
                let (file_id, def_edit) = source_edit_from_def(sema, self.clone(), new_name)?;
                source_change.insert_source_edit(file_id, def_edit);

                source_edit_from_usages(
                    &mut source_change,
                    usages.iter().collect(),
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
                    if !is_safe_var_usages(sema, infile_var, &usages, new_name) {
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

    fn rename_module(
        &self,
        sema: &Semantic,
        new_name: &str,
        safety_check: SafetyChecks,
    ) -> RenameResult<SourceChange> {
        let file_id = self.file().file_id;
        if let Some(project_id) = sema.db.file_project_id(file_id) {
            let module_index = sema.db.module_index(project_id);
            if safety_check == SafetyChecks::Yes {
                let new_name_module = ModuleName::new(new_name);
                if module_index
                    .all_modules()
                    .iter()
                    .any(|name| name == &new_name_module)
                {
                    rename_error!("module '{}' already exists", new_name);
                }
            }

            let mut source_change = SourceChange::default();
            // Step 1, rename all references
            let def_map = sema.def_map(file_id);
            // process anything that could be used. functions, types, ??
            def_map.get_functions().for_each(|(_name, f)| {
                if f.exported {
                    let usages = SymbolDefinition::Function(f.clone()).usages(sema).all();
                    rename_remote_module_call_refs(usages, file_id, new_name, &mut source_change);
                }
            });
            def_map.get_types().iter().for_each(|(_name, t)| {
                if t.exported {
                    let usages = SymbolDefinition::Type(t.clone()).usages(sema).all();
                    rename_remote_module_call_refs(usages, file_id, new_name, &mut source_change);
                }
            });

            // Make changes in the module being renamed
            let mut renamed_module_edit: TextEdit = TextEdit::default();
            let form_list = sema.form_list(file_id);
            if let Some(module_attribute) = form_list.module_attribute() {
                let ast = module_attribute.form_id.get_ast(sema.db, file_id);
                if let Some(name) = ast.name() {
                    let range = name.syntax().text_range();
                    let mut builder = TextEdit::builder();
                    builder.replace(range, new_name.to_string());
                    renamed_module_edit
                        .union(builder.finish())
                        .expect("Could not combine TextEdits");
                }
                def_map.get_functions().for_each(|(_name, f)| {
                    let usages = SymbolDefinition::Function(f.clone()).usages(sema).all();
                    rename_own_module_call_refs(
                        usages,
                        file_id,
                        new_name,
                        &mut renamed_module_edit,
                    );
                });
                def_map.get_types().iter().for_each(|(_name, t)| {
                    let usages = SymbolDefinition::Type(t.clone()).usages(sema).all();
                    rename_own_module_call_refs(
                        usages,
                        file_id,
                        new_name,
                        &mut renamed_module_edit,
                    );
                });
            }

            let anchor = file_id;
            let path = format!("{new_name}.erl");
            let dst = AnchoredPathBuf { anchor, path };
            let mut initial_contents = sema.db.file_text(anchor).to_string();
            renamed_module_edit.apply(&mut initial_contents);
            source_change.push_file_system_edit(FileSystemEdit::CreateFile {
                dst,
                initial_contents,
            });

            Ok(source_change)
        } else {
            rename_error!(
                "Could not find project for '{:?}'",
                self.file().name(sema.db.upcast())
            )
        }
    }
}

fn rename_remote_module_call_refs(
    usages: crate::UsageSearchResult,
    file_id: FileId,
    new_name: &str,
    source_change: &mut SourceChange,
) {
    usages.iter().for_each(|(usage_file_id, refs)| {
        if usage_file_id != file_id
            && let Some(edit) = rename_call_module_in_refs(refs, new_name)
        {
            source_change.insert_source_edit(usage_file_id, edit);
        };
    });
}

fn rename_own_module_call_refs(
    usages: crate::UsageSearchResult,
    file_id: FileId,
    new_name: &str,
    renamed_module_edit: &mut TextEdit,
) {
    usages.iter().for_each(|(usage_file_id, refs)| {
        if usage_file_id == file_id
            && let Some(edit) = rename_call_module_in_refs(refs, new_name)
        {
            renamed_module_edit
                .union(edit)
                .expect("Could not combine TextEdits");
        }
    });
}

fn rename_call_module_in_refs(refs: &[NameLike], new_name: &str) -> Option<TextEdit> {
    let mut builder = TextEdit::builder();
    for usage in refs {
        let _ = rename_call_module_in_ref(usage, &mut builder, new_name);
    }
    Some(builder.finish())
}

fn rename_call_module_in_ref(
    usage: &NameLike,
    builder: &mut TextEditBuilder,
    new_name: &str,
) -> Option<()> {
    let call = get_call(usage.syntax())?;
    // Note: `ast` uses the same syntax for a function call and a type
    let _: () = if let Some(ast::Expr::Remote(remote)) = call.expr() {
        let module = remote.module()?.module()?;
        builder.replace(module.syntax().text_range(), new_name.to_string());
    };
    Some(())
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
            format!("({new_name})")
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
    new_name: &str,
) -> RenameResult<(FileId, TextEdit)> {
    let FileRange { file_id, range } = def
        .range_for_rename(sema)
        .ok_or_else(|| format_err!("No identifier available to rename"))?;

    let mut edit = TextEdit::builder();
    if edit.is_empty() {
        edit.replace(range, new_name.to_string());
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
            resolver_var
                .all_vars_in_scope(scope_var)
                .iter()
                .all(|&var| sema.db.lookup_var(var) != new_name)
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
        .all(|(name, _)| !(*name.name().to_string() == *new_name && name.arity() == arity));

    scope_ok && !in_erlang_module(new_name, arity as usize)
}

/// Check that the new macro name is not already defined in scope.
/// Macros are identified by both name and arity, similar to functions.
pub fn is_safe_macro(sema: &Semantic, file_id: FileId, new_name: &str, arity: Option<u32>) -> bool {
    sema.db
        .def_map(file_id)
        .get_macros()
        .iter()
        .all(|(name, _)| {
            // A macro is considered different if either the name or arity differs
            *name.name().to_string() != *new_name || name.arity() != arity
        })
}

/// Check that the new type name is not already defined in scope.
/// Types are identified by both name and arity, similar to functions and macros.
pub fn is_safe_type(sema: &Semantic, file_id: FileId, new_name: &str, arity: u32) -> bool {
    sema.db
        .def_map(file_id)
        .get_types()
        .iter()
        .all(|(name, _)| {
            // A type is considered different if either the name or arity differs
            *name.name().to_string() != *new_name || name.arity() != arity
        })
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
        if let Some(call) = get_call(name_like.syntax()) {
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

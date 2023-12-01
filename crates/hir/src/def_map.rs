/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A lookup map for definitions in a module/file.
//!
//! DefMap represents definitions of various constructs in a file -
//! functions, records, types, in a way that is easy to lookup.
//! It represents the state as Erlang compiler sees it - after include resolution.
//!
//! They are constructed recursively and separately for all headers and modules -
//! this makes sure that we need to do a minimal amount of re-computation on changes.

use std::sync::Arc;

use elp_base_db::module_name;
use elp_base_db::FileId;
use elp_syntax::ast;
use elp_syntax::match_ast;
use elp_syntax::AstNode;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use profile::Count;

use crate::db::MinDefDatabase;
use crate::form_list::DeprecatedAttribute;
use crate::form_list::DeprecatedDesc;
use crate::form_list::DeprecatedFa;
use crate::known;
use crate::module_data::SpecDef;
use crate::module_data::SpecdFunctionDef;
use crate::name::AsName;
use crate::CallbackDef;
use crate::DefineDef;
use crate::File;
use crate::FormIdx;
use crate::FunctionClauseDef;
use crate::FunctionDef;
use crate::FunctionId;
use crate::InFile;
use crate::MacroName;
use crate::Name;
use crate::NameArity;
use crate::OptionalCallbacks;
use crate::PPDirective;
use crate::RecordDef;
use crate::TypeAliasDef;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct DefMap {
    _c: Count<Self>,
    included: FxHashSet<FileId>,

    function_clauses: FxHashMap<FunctionId, FunctionClauseDef>,
    functions: FxHashMap<InFile<FunctionDefId>, FunctionDef>,

    function_clauses_by_fa: FxHashMap<NameArity, FunctionId>,
    functions_by_fa: FxHashMap<NameArity, InFile<FunctionDefId>>,
    function_by_function_id: FxHashMap<FunctionId, FunctionDefId>,

    specs: FxHashMap<NameArity, SpecDef>,
    exported_functions: FxHashSet<NameArity>,
    deprecated: Deprecated,
    optional_callbacks: FxHashSet<NameArity>,
    imported_functions: FxHashMap<NameArity, Name>,
    types: FxHashMap<NameArity, TypeAliasDef>,
    exported_types: FxHashSet<NameArity>,
    records: FxHashMap<Name, RecordDef>,
    callbacks: FxHashMap<NameArity, CallbackDef>,
    macros: FxHashMap<MacroName, DefineDef>,
    export_all: bool,
    pub parse_transform: bool,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionDefId(FunctionId);

impl FunctionDefId {
    /// A `FunctionDefId` wraps the `FunctionId` of the first fun_decl
    /// with its clause.
    /// Use with care, if the clause being processed matters.
    pub fn as_form_id(&self) -> FormIdx {
        FormIdx::Function(self.0)
    }

    // #[cfg(test)]
    pub fn new(function_id: FunctionId) -> FunctionDefId {
        FunctionDefId(function_id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Deprecated {
    all: bool,
    functions: FxHashMap<Name, Option<DeprecatedDesc>>,
    fa: FxHashMap<NameArity, Option<DeprecatedDesc>>,
}

impl Deprecated {
    fn is_deprecated(&self, fa: &NameArity) -> bool {
        if self.all {
            return true;
        }
        if self.functions.contains_key(fa.name()) {
            return true;
        }
        if self.fa.contains_key(fa) {
            return true;
        }
        false
    }

    fn deprecation_desc(&self, fa: &NameArity) -> Option<DeprecatedDesc> {
        if let Some(desc) = self.functions.get(fa.name()) {
            return desc.clone();
        }
        if let Some(desc) = self.fa.get(fa) {
            return desc.clone();
        }
        None
    }

    fn shrink_to_fit(&mut self) {
        self.functions.shrink_to_fit();
        self.fa.shrink_to_fit();
    }
}

impl DefMap {
    pub(crate) fn local_def_map_query(db: &dyn MinDefDatabase, file_id: FileId) -> Arc<DefMap> {
        let mut def_map = Self::default();
        let file = File { file_id };
        let module = module_name(db.upcast(), file_id);

        // Type (and to some extent function) definitions & export attributes
        // can come in any order, we initially store types/functions as unexported, non-deprecated
        // and later fix it up.
        // We do this both for the local & complete query to have some export information
        // in case we fall back to local-only data in cycles.

        let form_list = db.file_form_list(file_id);
        for &form in form_list.forms() {
            match form {
                FormIdx::Function(idx) => {
                    let function = form_list[idx].clone();
                    let fun_name = &function.name.clone();
                    let function_def = FunctionClauseDef {
                        file,
                        module: module.clone(),
                        function,
                        function_id: idx,
                    };
                    def_map.function_clauses.insert(idx, function_def);
                    let function_clause_id = idx;
                    def_map
                        .function_clauses_by_fa
                        .insert(fun_name.clone(), function_clause_id);
                }
                FormIdx::Export(idx) => {
                    for export_id in form_list[idx].entries.clone() {
                        def_map
                            .exported_functions
                            .insert(form_list[export_id].name.clone());
                    }
                }
                FormIdx::Import(idx) => {
                    for import_id in form_list[idx].entries.clone() {
                        let module = form_list[idx].from.clone();
                        def_map
                            .imported_functions
                            .insert(form_list[import_id].name.clone(), module);
                    }
                }
                FormIdx::TypeAlias(idx) => {
                    let type_alias = form_list[idx].clone();
                    def_map.types.insert(
                        type_alias.name().clone(),
                        TypeAliasDef {
                            file,
                            exported: false,
                            type_alias,
                        },
                    );
                }
                FormIdx::TypeExport(idx) => {
                    for export_id in form_list[idx].entries.clone() {
                        def_map
                            .exported_types
                            .insert(form_list[export_id].name.clone());
                    }
                }
                FormIdx::Callback(idx) => {
                    let callback = form_list[idx].clone();
                    def_map.callbacks.insert(
                        callback.name.clone(),
                        CallbackDef {
                            file,
                            callback,
                            optional: false,
                        },
                    );
                }
                FormIdx::Record(idx) => {
                    let record = form_list[idx].clone();
                    def_map
                        .records
                        .insert(record.name.clone(), RecordDef { file, record });
                }
                FormIdx::PPDirective(idx) => {
                    if let PPDirective::Define(define) = &form_list[idx] {
                        let define = form_list[*define].clone();
                        def_map
                            .macros
                            .insert(define.name.clone(), DefineDef { file, define });
                    }
                }
                FormIdx::CompileOption(idx) => {
                    let option = &form_list[idx];
                    let source = db.parse(file_id);
                    let ast_option = option.form_id.get(&source.tree());
                    if let Some(options) = ast_option.options() {
                        // Blindly search for any atom with value `export_all`, or `parse_transform`.
                        options.syntax().descendants().for_each(|n| {
                            match_ast! {
                                match n {
                                    ast::Atom(a) => {
                                        if a.as_name() == known::export_all {
                                            def_map.export_all = true;
                                        }
                                        if a.as_name() == known::parse_transform {
                                            def_map.parse_transform = true;
                                        }
                                    },
                                    _ => {},
                                }
                            }
                        });
                    }
                }
                FormIdx::Spec(idx) => {
                    let spec = form_list[idx].clone();
                    def_map.specs.insert(
                        spec.name.clone(),
                        SpecDef {
                            file,
                            spec,
                            spec_id: idx,
                        },
                    );
                }
                //https://github.com/erlang/otp/blob/69aa665f3f48a59f83ad48dea63fdf1476d1d46a/lib/stdlib/src/erl_lint.erl#L1123
                FormIdx::DeprecatedAttribute(idx) => match &form_list[idx] {
                    DeprecatedAttribute::Module { .. } => {
                        def_map.deprecated.all = true;
                    }
                    DeprecatedAttribute::Fa { fa, .. } => {
                        Self::def_map_deprecated_attr(&mut def_map, fa);
                    }
                    DeprecatedAttribute::Fas { fas, .. } => {
                        for fa in fas {
                            Self::def_map_deprecated_attr(&mut def_map, fa);
                        }
                    }
                },
                FormIdx::OptionalCallbacks(idx) => {
                    let OptionalCallbacks {
                        entries,
                        cond: _,
                        form_id: _,
                    } = &form_list[idx];
                    entries.clone().for_each(|fa| {
                        def_map
                            .optional_callbacks
                            .insert(form_list[fa].name.clone());
                    });
                }
                _ => {}
            }
        }

        def_map.fixup_functions();
        def_map.fixup_exports();
        def_map.fixup_deprecated();
        def_map.fixup_callbacks();
        def_map.shrink_to_fit();

        Arc::new(def_map)
    }

    fn def_map_deprecated_attr(def_map: &mut DefMap, fa: &DeprecatedFa) {
        if fa.name == "_" {
            def_map.deprecated.all = true;
        }
        let desc = fa.desc.clone();
        match fa.arity {
            Some(arity) => def_map
                .deprecated
                .fa
                .insert(NameArity::new(fa.name.clone(), arity), desc),
            None => def_map.deprecated.functions.insert(fa.name.clone(), desc),
        };
    }

    pub(crate) fn def_map_query(db: &dyn MinDefDatabase, file_id: FileId) -> Arc<DefMap> {
        let local = db.local_def_map(file_id);
        let form_list = db.file_form_list(file_id);

        let mut remote = Self::default();

        form_list
            .includes()
            .filter_map(|(idx, _)| db.resolve_include(InFile::new(file_id, idx)))
            // guard against naive cycles of headers including themselves
            .filter(|&included_file_id| included_file_id != file_id)
            .map(|included_file_id| (included_file_id, db.def_map(included_file_id)))
            .for_each(|(file_id, def_map)| {
                remote.included.insert(file_id);
                remote.merge(&def_map)
            });

        // Small optimisation for a case where we have no headers or headers don't contain defintitions
        // we're inrested in - should be hit frequently in headers themselves
        if remote.is_empty() {
            local
        } else {
            remote.merge(&local);
            remote.fixup_exports();
            remote.fixup_deprecated();
            Arc::new(remote)
        }
    }

    // This handles the case of headers accidentally forming other cycles.
    // Return just the local def map in such cases, not resolving nested includes at all
    pub(crate) fn recover_cycle(
        db: &dyn MinDefDatabase,
        _cycle: &[String],
        file_id: &FileId,
    ) -> Arc<DefMap> {
        db.local_def_map(*file_id)
    }

    pub fn get_by_function_id(&self, function_id: &InFile<FunctionDefId>) -> Option<&FunctionDef> {
        self.functions.get(function_id)
    }

    pub fn function_def_id(&self, function_id: &FunctionId) -> Option<&FunctionDefId> {
        self.function_by_function_id.get(function_id)
    }

    pub fn is_deprecated(&self, name: &NameArity) -> bool {
        self.deprecated.is_deprecated(name)
    }

    pub fn get_spec(&self, name: &NameArity) -> Option<&SpecDef> {
        self.specs.get(name)
    }

    pub fn get_specd_function(&self, name: &NameArity) -> Option<SpecdFunctionDef> {
        let (spec_def, function_def) =
            Option::zip(self.get_spec(name).cloned(), self.get_function(name))?;
        Some(SpecdFunctionDef {
            spec_def,
            function_def: function_def.clone(),
        })
    }

    pub fn get_exported_functions(&self) -> &FxHashSet<NameArity> {
        &self.exported_functions
    }

    pub fn is_function_exported(&self, name: &NameArity) -> bool {
        self.exported_functions.contains(name)
    }

    pub fn get_function(&self, name: &NameArity) -> Option<&FunctionDef> {
        self.functions_by_fa
            .get(name)
            .and_then(|idx| self.functions.get(idx))
    }

    pub fn get_functions(&self) -> impl Iterator<Item = (&NameArity, &FunctionDef)> {
        self.functions_by_fa.iter().filter_map(|(k, _)| {
            if let Some(def) = self.get_function(k) {
                Some((k, def))
            } else {
                None
            }
        })
    }

    pub fn get_function_clauses(&self) -> impl Iterator<Item = (&FunctionId, &FunctionClauseDef)> {
        self.function_clauses.iter()
    }

    pub fn get_function_clauses_ordered(&self) -> Vec<(FunctionId, FunctionClauseDef)> {
        // We can't use a BTreeMap for this because of the lack of Ord for Idx<_>.
        let mut v: Vec<(FunctionId, FunctionClauseDef)> = Vec::from_iter(
            self.function_clauses
                .iter()
                .map(|(k, v)| ((*k).clone(), (*v).clone())),
        );
        v.sort_by(|(ka, _), (kb, _)| ka.into_raw().cmp(&kb.into_raw()));
        v
    }

    pub fn get_specs(&self) -> &FxHashMap<NameArity, SpecDef> {
        &self.specs
    }

    pub fn get_specd_functions(&self) -> FxHashMap<NameArity, SpecdFunctionDef> {
        let specs = self.get_specs();
        let name_arities = self.functions_by_fa.keys().chain(specs.keys());
        name_arities
            .filter_map(|na| {
                specs.get(na).zip(self.get_function(na)).map(|(s, f)| {
                    (
                        na.clone(),
                        SpecdFunctionDef {
                            spec_def: s.clone(),
                            function_def: f.clone(),
                        },
                    )
                })
            })
            .collect()
    }

    pub fn get_imports(&self) -> &FxHashMap<NameArity, Name> {
        &self.imported_functions
    }

    pub fn get_functions_in_scope(&self) -> impl Iterator<Item = &NameArity> {
        self.get_imports().keys().chain(self.functions_by_fa.keys())
    }

    // TODO: tweak API T127375780
    pub fn get_types(&self) -> &FxHashMap<NameArity, TypeAliasDef> {
        &self.types
    }

    pub fn get_type(&self, name: &NameArity) -> Option<&TypeAliasDef> {
        self.types.get(name)
    }

    // TODO: tweak API T127375780
    pub fn get_exported_types(&self) -> &FxHashSet<NameArity> {
        &self.exported_types
    }

    pub fn get_records(&self) -> &FxHashMap<Name, RecordDef> {
        &self.records
    }

    pub fn get_record(&self, name: &Name) -> Option<&RecordDef> {
        self.records.get(name)
    }

    pub fn get_macros(&self) -> &FxHashMap<MacroName, DefineDef> {
        &self.macros
    }

    pub fn get_callbacks(&self) -> &FxHashMap<NameArity, CallbackDef> {
        &self.callbacks
    }

    pub fn get_callback(&self, name: &NameArity) -> Option<&CallbackDef> {
        self.callbacks.get(name)
    }

    pub fn is_callback_optional(&self, name: &NameArity) -> bool {
        self.optional_callbacks.contains(name)
    }

    pub fn get_included_files(&self) -> impl Iterator<Item = FileId> + '_ {
        self.included.iter().copied()
    }

    fn is_empty(&self) -> bool {
        self.included.is_empty()
            && self.functions.is_empty()
            && self.function_clauses.is_empty()
            && self.exported_functions.is_empty()
            && self.types.is_empty()
            && self.exported_types.is_empty()
            && self.records.is_empty()
            && self.callbacks.is_empty()
            && self.macros.is_empty()
    }

    fn merge(&mut self, other: &Self) {
        self.included.extend(other.included.iter().cloned());
        self.function_clauses.extend(
            other
                .function_clauses
                .iter()
                .map(|(name, def)| (name.clone(), def.clone())),
        );
        self.functions.extend(
            other
                .functions
                .iter()
                .map(|(name, def)| (name.clone(), def.clone())),
        );
        self.function_clauses_by_fa.extend(
            other
                .function_clauses_by_fa
                .iter()
                .map(|(name, def)| (name.clone(), def.clone())),
        );
        self.functions_by_fa.extend(
            other
                .functions_by_fa
                .iter()
                .map(|(name, def)| (name.clone(), def.clone())),
        );
        self.function_by_function_id.extend(
            other
                .function_by_function_id
                .iter()
                .map(|(name, def)| (name.clone(), def.clone())),
        );
        self.specs.extend(
            other
                .specs
                .iter()
                .map(|(name, def)| (name.clone(), def.clone())),
        );
        self.exported_functions
            .extend(other.exported_functions.iter().cloned());
        self.types.extend(
            other
                .types
                .iter()
                .map(|(name, def)| (name.clone(), def.clone())),
        );
        self.exported_types
            .extend(other.exported_types.iter().cloned());
        self.records.extend(
            other
                .records
                .iter()
                .map(|(name, def)| (name.clone(), def.clone())),
        );
        self.callbacks.extend(
            other
                .callbacks
                .iter()
                .map(|(name, def)| (name.clone(), def.clone())),
        );
        self.macros.extend(
            other
                .macros
                .iter()
                .map(|(name, def)| (name.clone(), def.clone())),
        );
        self.deprecated.all |= other.deprecated.all;
        self.deprecated.functions.extend(
            other
                .deprecated
                .functions
                .iter()
                .map(|(name, desc)| (name.clone(), desc.clone())),
        );
        self.deprecated.fa.extend(
            other
                .deprecated
                .fa
                .iter()
                .map(|(name, desc)| (name.clone(), desc.clone())),
        );
    }

    fn fixup_functions(&mut self) {
        // We parse each function clause independently as a top level
        // form, to improve error recovery.
        // These have been lowered into self.function_clauses.
        // Work through them and construct the top level function
        // definitions by combining the ones that belong together.
        let mut current: Vec<(NameArity, FunctionClauseDef)> = Vec::default();
        let mut prior_separator = None;
        self.get_function_clauses_ordered()
            .iter()
            .for_each(|(_next_id, next_def)| {
                if let Some((current_na, current_def)) = current.get(0) {
                    if current_na == &next_def.function.name
                        || (next_def.function.is_macro
                            && prior_separator == Some(ast::ClauseSeparator::Semi))
                    {
                        current.push((next_def.function.name.clone(), next_def.clone()));
                    } else {
                        // We have a new one, create a FunctionDef with
                        // the ones in current.
                        self.insert_fun(&current, current_na, current_def);
                        current = Vec::default();
                        current.push((next_def.function.name.clone(), next_def.clone()));
                    }
                } else {
                    current.push((next_def.function.name.clone(), next_def.clone()));
                }
                prior_separator = next_def.function.separator.clone().map(|s| s.0);
            });
        if let Some((current_na, current_def)) = current.get(0) {
            self.insert_fun(&current, current_na, current_def);
        }
    }

    fn insert_fun(
        &mut self,
        current: &Vec<(NameArity, FunctionClauseDef)>,
        current_na: &NameArity,
        current_def: &FunctionClauseDef,
    ) {
        let function = current
            .iter()
            .map(|(_, clause)| clause.function.clone())
            .collect::<Vec<_>>();
        let function_ids = current
            .iter()
            .map(|(_, clause)| clause.function_id)
            .collect::<Vec<_>>();
        let function_id = FunctionDefId(function_ids[0]);
        let na = (current_na).clone();
        let fun = FunctionDef {
            file: current_def.file,
            exported: false,
            deprecated: false,
            deprecated_desc: None,
            module: current_def.module.clone(),
            name: (current_na).clone(),
            function,
            function_ids,
            function_id,
        };
        let function_def_id = FunctionDefId(fun.function_ids[0]);
        self.function_by_function_id.extend(
            fun.function_ids
                .iter()
                .map(|id| (*id, function_def_id.clone())),
        );
        let id = InFile::new(current_def.file.file_id, function_def_id.clone());
        self.functions.insert(id, fun.clone());
        self.functions_by_fa.insert(na, id);
    }

    fn is_exported(&self, fun_def_id: &InFile<FunctionDefId>) -> bool {
        if let Some(fun) = self.functions.get(fun_def_id) {
            self.exported_functions.contains(&fun.function[0].name)
        } else {
            false
        }
    }

    fn is_deprecated_fun_def(&self, fun_def_id: &InFile<FunctionDefId>) -> bool {
        if let Some(fun) = self.functions.get(fun_def_id) {
            self.deprecated.is_deprecated(&fun.name)
        } else {
            false
        }
    }

    fn deprecation_desc(&self, fun_def_id: &InFile<FunctionDefId>) -> Option<DeprecatedDesc> {
        self.functions
            .get(fun_def_id)
            .and_then(|fun| self.deprecated.deprecation_desc(&fun.function[0].name))
    }

    fn fixup_exports(&mut self) {
        if self.export_all {
            self.exported_functions = self.functions_by_fa.keys().cloned().collect()
        }

        // Hack around mutable borrow issue below
        let exported: FxHashMap<FunctionDefId, bool> = self
            .functions
            .clone()
            .iter()
            .map(|(fun_def_id, _)| (fun_def_id.value.clone(), self.is_exported(fun_def_id)))
            .collect();

        for (fun_def_id, fun_def) in self.functions.iter_mut() {
            fun_def.exported |=
                self.export_all || *exported.get(&fun_def_id.value).unwrap_or(&false)
        }

        for (name, type_def) in self.types.iter_mut() {
            type_def.exported |= self.exported_types.contains(name)
        }
    }

    fn fixup_deprecated(&mut self) {
        // Hack around mutable borrow issue below
        let deprecated: FxHashMap<FunctionDefId, bool> = self
            .functions
            .clone()
            .iter()
            .map(|(fun_def_id, _)| {
                (
                    fun_def_id.value.clone(),
                    self.is_deprecated_fun_def(fun_def_id),
                )
            })
            .collect();

        let deprecation_desc: FxHashMap<FunctionDefId, Option<DeprecatedDesc>> = self
            .functions
            .clone()
            .iter()
            .map(|(fun_def_id, _)| (fun_def_id.value.clone(), self.deprecation_desc(fun_def_id)))
            .collect();

        for (fun_def_id, fun_def) in self.functions.iter_mut() {
            let is_deprecated = *deprecated.get(&fun_def_id.value).unwrap_or(&false);
            fun_def.deprecated |= is_deprecated;
            if is_deprecated {
                fun_def.deprecated_desc = deprecation_desc
                    .get(&fun_def_id.value)
                    .unwrap_or(&None)
                    .clone();
            }
        }
    }

    fn fixup_callbacks(&mut self) {
        for (name, callback) in self.callbacks.iter_mut() {
            callback.optional |= self.optional_callbacks.contains(name);
        }
    }

    fn shrink_to_fit(&mut self) {
        // Exhaustive match to require handling new fields.
        let Self {
            _c: _,
            included,
            function_clauses,
            functions,
            specs,
            deprecated,
            exported_functions,
            imported_functions,
            types,
            exported_types,
            records,
            callbacks,
            macros,
            export_all: _,
            parse_transform: _,
            optional_callbacks,
            function_by_function_id: function_by_form_id,
            function_clauses_by_fa,
            functions_by_fa,
        } = self;

        included.shrink_to_fit();
        function_clauses.shrink_to_fit();
        functions.shrink_to_fit();
        specs.shrink_to_fit();
        exported_functions.shrink_to_fit();
        imported_functions.shrink_to_fit();
        types.shrink_to_fit();
        exported_types.shrink_to_fit();
        optional_callbacks.shrink_to_fit();
        records.shrink_to_fit();
        callbacks.shrink_to_fit();
        macros.shrink_to_fit();
        deprecated.shrink_to_fit();
        function_by_form_id.shrink_to_fit();
        function_clauses_by_fa.shrink_to_fit();
        functions_by_fa.shrink_to_fit();
    }
}

#[cfg(test)]
mod tests {
    use elp_base_db::fixture::WithFixture;
    use expect_test::expect;
    use expect_test::Expect;

    use super::*;
    use crate::test_db::TestDB;
    use crate::TypeAlias;

    fn check_functions(fixture: &str, expect: Expect) {
        let (db, files) = TestDB::with_many_files(fixture);
        let file_id = files[0];
        let def_map = db.def_map(file_id);
        let mut resolved = def_map
            .functions
            .values()
            .map(|def| {
                format!(
                    "fun {} exported: {}",
                    def.name,
                    def.exported && def_map.exported_functions.contains(&def.name),
                )
            })
            .chain(def_map.types.values().map(|def| match &def.type_alias {
                TypeAlias::Regular { name, .. } => {
                    format!("-type {} exported: {}", name, def.exported)
                }
                TypeAlias::Opaque { name, .. } => {
                    format!("-opaque {} exported: {}", name, def.exported)
                }
            }))
            .collect::<Vec<_>>()
            .join("\n");
        resolved.push('\n');
        expect.assert_eq(&resolved);
    }

    fn check_functions_by_id(fixture: &str, expect: Expect) {
        let (db, files) = TestDB::with_many_files(fixture);
        let file_id = files[0];
        let def_map = db.def_map(file_id);
        let mut resolved = def_map
            .functions
            .values()
            .map(|def| {
                format!(
                    "fun {} exported: {}",
                    def.name,
                    def.exported && def_map.exported_functions.contains(&def.name),
                )
            })
            .chain(def_map.types.values().map(|def| match &def.type_alias {
                TypeAlias::Regular { name, .. } => {
                    format!("-type {} exported: {}", name, def.exported)
                }
                TypeAlias::Opaque { name, .. } => {
                    format!("-opaque {} exported: {}", name, def.exported)
                }
            }))
            .collect::<Vec<_>>()
            .join("\n");
        resolved.push('\n');
        expect.assert_eq(&resolved);
    }

    fn check_callbacks(fixture: &str, expect: Expect) {
        let (db, files) = TestDB::with_many_files(fixture);
        let file_id = files[0];
        let def_map = db.def_map(file_id);
        let mut resolved = def_map
            .callbacks
            .values()
            .map(|def| {
                format!(
                    "callback {} optional: {}",
                    def.callback.name,
                    def.optional && def_map.optional_callbacks.contains(&def.callback.name)
                )
            })
            .collect::<Vec<_>>()
            .join("\n");
        resolved.push('\n');
        expect.assert_eq(&resolved);
    }

    #[test]
    fn exported_functions() {
        check_functions(
            r#"
-export([foo/1]).

foo(_) -> ok.
bar() -> ok.
"#,
            expect![[r#"
                fun foo/1 exported: true
                fun bar/0 exported: false
            "#]],
        )
    }

    #[test]
    fn export_all_1() {
        check_functions(
            r#"
-compile(export_all).

foo(_) -> ok.
bar() -> ok.
"#,
            expect![[r#"
                fun foo/1 exported: true
                fun bar/0 exported: true
            "#]],
        )
    }

    #[test]
    fn export_all_2() {
        check_functions(
            r#"
-compile({export_all}).

foo(_) -> ok.
bar() -> ok.
"#,
            expect![[r#"
                fun foo/1 exported: true
                fun bar/0 exported: true
            "#]],
        )
    }

    #[test]
    fn export_all_3() {
        check_functions(
            r#"
-compile([export_all]).

foo(_) -> ok.
bar() -> ok.
"#,
            expect![[r#"
                fun foo/1 exported: true
                fun bar/0 exported: true
            "#]],
        )
    }

    #[test]
    fn export_all_4() {
        check_functions(
            r#"
-compile([brief,export_all]).

foo(_) -> ok.
bar() -> ok.
"#,
            expect![[r#"
                fun foo/1 exported: true
                fun bar/0 exported: true
            "#]],
        )
    }

    #[test]
    fn exported_types() {
        check_functions(
            r#"
-export_type([foo/1]).

-type foo(A) :: ok.
-opaque bar() :: ok.
"#,
            expect![[r#"
                -opaque bar/0 exported: false
                -type foo/1 exported: true
            "#]],
        )
    }

    #[test]
    fn exported_types_post_definition() {
        check_functions(
            r#"
-opaque foo(A) :: ok.
-type bar() :: ok.

-export_type([foo/1]).
"#,
            expect![[r#"
                -type bar/0 exported: false
                -opaque foo/1 exported: true
            "#]],
        )
    }

    #[test]
    fn exported_types_from_header() {
        check_functions(
            r#"
//- /module.erl
-include("header.hrl").

-export_type([foo/1]).
//- /header.hrl
-type foo(A) :: ok.
-type bar() :: ok.
"#,
            expect![[r#"
                -type bar/0 exported: false
                -type foo/1 exported: true
            "#]],
        )
    }

    #[test]
    fn export_functions_in_header() {
        check_functions(
            r#"
//- /module.erl
-include("header.hrl").

foo(_) -> ok.
bar() -> ok.
//- /header.hrl
-export([foo/1]).
"#,
            expect![[r#"
                fun foo/1 exported: true
                fun bar/0 exported: false
            "#]],
        )
    }

    #[test]
    fn optional_callback() {
        check_callbacks(
            r#"
            //- /module.erl
            -callback optional() -> ok.
            -callback init(term()) -> ok.
            -optional_callbacks([
                init/1
            ])."#,
            expect![[r#"
                callback optional/0 optional: false
                callback init/1 optional: true
            "#]],
        )
    }

    #[test]
    fn multiple_mfas() {
        check_functions(
            r#"
foo(A,B,C) -> {A,B,C}.
bar() -> ok.
foo(X,Y, Z) -> ok.
"#,
            expect![[r#"
                fun foo/3 exported: false
                fun bar/0 exported: false
                fun foo/3 exported: false
            "#]],
        )
    }

    #[test]
    fn multiple_mfas_by_id() {
        check_functions_by_id(
            r#"
foo(A,B,C) -> {A,B,C}.
bar() -> ok.
foo(X,Y, Z) -> ok.
"#,
            expect![[r#"
                fun foo/3 exported: false
                fun bar/0 exported: false
                fun foo/3 exported: false
            "#]],
        )
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::sync::Arc;

use elp_types_db::eqwalizer::form::Callback;
use elp_types_db::eqwalizer::form::FunSpec;
use elp_types_db::eqwalizer::form::OverloadedFunSpec;
use elp_types_db::eqwalizer::form::RecDecl;
use elp_types_db::eqwalizer::form::TypeDecl;
use elp_types_db::eqwalizer::invalid_diagnostics::Invalid;
use elp_types_db::StringId;
use serde::Serialize;

use super::Id;

#[derive(Serialize, Debug, Clone, PartialEq, Eq, Default)]
pub struct ModuleStub {
    pub module: StringId,
    pub exports: BTreeSet<Id>,
    pub imports: BTreeMap<Id, StringId>,
    pub export_types: BTreeSet<Id>,
    pub opaques: BTreeMap<Id, TypeDecl>,
    pub types: BTreeMap<Id, TypeDecl>,
    pub specs: BTreeMap<Id, FunSpec>,
    pub overloaded_specs: BTreeMap<Id, OverloadedFunSpec>,
    pub records: BTreeMap<StringId, RecDecl>,
    pub callbacks: Vec<Callback>,
    pub optional_callbacks: BTreeSet<Id>,
    pub invalids: Vec<Invalid>,
}

// The result of type validation checking:
// - invalid_forms: a list of invalid forms
// - invalid_types: a set of invalid types/opaque types
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct VStub {
    stub: Arc<ModuleStub>,
    pub invalids: Vec<Invalid>,
    pub invalid_ids: BTreeSet<Id>,
}

impl VStub {
    pub fn new(stub: Arc<ModuleStub>) -> Self {
        Self {
            stub,
            invalids: vec![],
            invalid_ids: BTreeSet::default(),
        }
    }

    pub fn get_type(&self, id: &Id) -> Option<&TypeDecl> {
        if !self.invalid_ids.contains(id) {
            return self.stub.types.get(id);
        }
        None
    }

    pub fn types(&self) -> impl Iterator<Item = &TypeDecl> {
        self.stub
            .types
            .values()
            .filter(|decl| !self.invalid_ids.contains(&decl.id))
    }

    pub fn get_opaque(&self, id: &Id) -> Option<&TypeDecl> {
        if !self.invalid_ids.contains(id) {
            return self.stub.opaques.get(id);
        }
        None
    }

    pub fn opaques(&self) -> impl Iterator<Item = &TypeDecl> {
        self.stub
            .opaques
            .values()
            .filter(|decl| !self.invalid_ids.contains(&decl.id))
    }

    pub fn get_record(&self, name: StringId) -> Option<&RecDecl> {
        self.stub.records.get(&name)
    }

    pub fn records(&self) -> impl Iterator<Item = &RecDecl> {
        self.stub.records.values()
    }

    pub fn specs(&self) -> impl Iterator<Item = &FunSpec> {
        self.stub.specs.values()
    }

    pub fn overloaded_specs(&self) -> impl Iterator<Item = &OverloadedFunSpec> {
        self.stub.overloaded_specs.values()
    }

    pub fn into_normalized_stub(&self) -> ModuleStub {
        let mut stub = (*self.stub).clone();
        stub.invalids.extend_from_slice(&self.invalids);
        stub.types.retain(|id, _| !self.invalid_ids.contains(id));
        stub.opaques.retain(|id, _| !self.invalid_ids.contains(id));
        stub
    }
}

impl ModuleStub {
    pub fn to_bytes(&self) -> Vec<u8> {
        serde_json::to_vec(self).unwrap()
    }
}

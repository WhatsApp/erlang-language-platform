/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Borrow;
use std::borrow::Cow;
use std::fmt;
use std::hash::Hash;
use std::ops::Deref;
use std::sync::Arc;

use elp_syntax::SmolStr;
use fxhash::FxHashMap;
use fxhash::FxHashSet;

use crate::FileId;
use crate::to_quoted_string;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModuleName(SmolStr);

impl ModuleName {
    pub fn new(name: &str) -> Self {
        ModuleName(SmolStr::new(name))
    }

    pub fn as_str(&self) -> &str {
        self
    }

    pub fn to_quoted_string(&self) -> Cow<'_, str> {
        to_quoted_string(self.as_str())
    }
}

impl Deref for ModuleName {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Borrow<str> for ModuleName {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

pub type Modules = Vec<ModuleName>;

#[derive(Clone, PartialEq, Eq)]
pub struct ModuleIndex {
    /// The index of OTP's modules, which the project's modules shadow.
    otp: Option<Arc<ModuleIndex>>,
    mod2file: FxHashMap<ModuleName, FileId>,
    file2mod: FxHashMap<FileId, ModuleName>,
    duplicates: FxHashMap<ModuleName, FxHashSet<FileId>>,
}

impl fmt::Debug for ModuleIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("ModuleIndex(")?;
        let mut map = f.debug_map();
        for entry in &self.mod2file {
            map.entry(&entry.0, &entry.1);
        }
        map.finish()?;
        f.write_str(")")
    }
}

impl ModuleIndex {
    pub fn builder() -> Builder {
        Builder::default()
    }

    pub fn file_for_module<Q>(&self, name: &Q) -> Option<FileId>
    where
        ModuleName: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.mod2file
            .get(name)
            .copied()
            .or_else(|| self.otp.as_ref()?.file_for_module(name))
    }

    pub fn module_for_file(&self, file_id: FileId) -> Option<&ModuleName> {
        self.file2mod
            .get(&file_id)
            .or_else(|| self.otp.as_ref()?.module_for_file(file_id))
    }

    /// Iterate over project-owned modules, without OTP
    pub fn iter_own(&self) -> impl ExactSizeIterator<Item = (&ModuleName, FileId)> + '_ {
        self.mod2file.iter().map(|(name, id)| (name, *id))
    }

    /// Number of project-owned modules, without OTP
    pub fn len_own(&self) -> usize {
        self.mod2file.len()
    }

    /// All project-owned modules and OTP modules
    pub fn all_modules(&self) -> Modules {
        self.mod2file
            .keys()
            .chain(self.otp.iter().flat_map(|otp| otp.mod2file.keys()))
            .cloned()
            .collect()
    }

    pub fn duplicates(&self, module_name: &ModuleName) -> Option<FxHashSet<FileId>> {
        self.duplicates.get(module_name).cloned()
    }
}

#[derive(Default)]
pub struct Builder {
    mod2file: FxHashMap<ModuleName, FileId>,
    otp: Option<Arc<ModuleIndex>>,
    duplicates: FxHashMap<ModuleName, FxHashSet<FileId>>,
}

impl Builder {
    pub fn insert(&mut self, file_id: FileId, name: ModuleName) {
        if let Some(dup_file_id) = self.mod2file.insert(name.clone(), file_id) {
            self.duplicates
                .entry(name)
                .or_default()
                .extend([dup_file_id, file_id]);
        }
    }

    /// Use a given, existing index as OTP
    pub fn set_otp(&mut self, otp: Arc<ModuleIndex>) {
        self.otp = Some(otp)
    }

    pub fn build(self) -> Arc<ModuleIndex> {
        let file2mod = self
            .mod2file
            .iter()
            .map(|(name, file)| (*file, name.clone()))
            .collect::<FxHashMap<_, _>>();

        Arc::new(ModuleIndex {
            otp: self.otp,
            mod2file: self.mod2file,
            file2mod,
            duplicates: self.duplicates,
        })
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::borrow::Cow;
use std::fmt;
use std::hash::Hash;
use std::ops::Deref;
use std::sync::Arc;

use elp_syntax::SmolStr;
use fxhash::FxHashMap;

use crate::FileId;
use crate::FileSource;
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

    pub fn to_quoted_string(&self) -> Cow<str> {
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
    /// - None: No OTP being tracked
    /// - Some(There(_)): There's OTP's module index
    /// - Some(Here): This index is itself OTP
    otp: Option<OtpModuleIndex>,
    mod2file: FxHashMap<ModuleName, (FileSource, FileId)>,
    file2mod: FxHashMap<FileId, ModuleName>,
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
            .map(|(_source, id)| *id)
            .or_else(|| {
                self.otp.as_ref().and_then(|otp| match otp {
                    OtpModuleIndex::There(index) => index.file_for_module(name),
                    OtpModuleIndex::Here => None,
                })
            })
    }

    pub fn file_source_for_file(&self, file_id: FileId) -> Option<FileSource> {
        self.file2mod
            .get(&file_id)
            .and_then(|name| self.mod2file.get(name).map(|(source, _id)| *source))
            .or_else(|| {
                self.otp.as_ref().and_then(|otp| match otp {
                    OtpModuleIndex::There(index) => index.file_source_for_file(file_id),
                    OtpModuleIndex::Here => None,
                })
            })
    }

    pub fn module_for_file(&self, file_id: FileId) -> Option<&ModuleName> {
        self.file2mod.get(&file_id).map_or_else(
            || {
                self.otp.as_ref().and_then(|otp| match otp {
                    OtpModuleIndex::There(index) => index.module_for_file(file_id),
                    OtpModuleIndex::Here => None,
                })
            },
            Some,
        )
    }

    /// Iterate over project-owned modules, without OTP
    pub fn iter_own(
        &self,
    ) -> impl ExactSizeIterator<Item = (&ModuleName, FileSource, FileId)> + '_ {
        self.mod2file
            .iter()
            .map(|(name, (source, id))| (name, *source, *id))
    }

    /// Number of project-owned modules, without OTP
    pub fn len_own(&self) -> usize {
        self.mod2file.len()
    }

    /// All project-owned modules and OTP modules
    pub fn all_modules(&self) -> Modules {
        match &self.otp {
            Some(OtpModuleIndex::There(otp)) => self
                .mod2file
                .iter()
                .chain(otp.mod2file.iter())
                .map(|(m, _f)| m.clone())
                .collect::<Vec<_>>(),
            Some(_) | None => self.mod2file.keys().cloned().collect::<Vec<_>>(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum OtpModuleIndex {
    /// "Use this index as the OTP index"
    There(Arc<ModuleIndex>),
    /// "We are currently indexing OTP, but it needs to refer to OTP's index - we need to tie the knot!"
    Here,
}

#[derive(Default)]
pub struct Builder(
    FxHashMap<ModuleName, (FileSource, FileId)>,
    Option<OtpModuleIndex>,
);

impl Builder {
    pub fn insert(&mut self, file_id: FileId, source: FileSource, name: ModuleName) {
        self.0.insert(name, (source, file_id));
    }

    /// Use a given, existing index as OTP
    pub fn set_otp(&mut self, otp: Arc<ModuleIndex>) {
        self.1 = Some(OtpModuleIndex::There(otp))
    }

    /// You are OTP, so use yourself as your OTP index
    pub fn is_otp(&mut self) {
        self.1 = Some(OtpModuleIndex::Here)
    }

    pub fn build(self) -> Arc<ModuleIndex> {
        let file2mod = self
            .0
            .iter()
            .map(|(name, (_source, file))| (*file, name.clone()))
            .collect::<FxHashMap<_, _>>();

        Arc::new(ModuleIndex {
            otp: self.1,
            mod2file: self.0,
            file2mod,
        })
    }
}

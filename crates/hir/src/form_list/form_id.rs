/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! `FormId` allows to create stable IDs for forms in a file.
//!
//! Specifically, it uses the sequential position of the form in the file
//! as the ID. That way, ids don't change unless the set of forms itself changes.

use std::any::type_name;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use std::marker::PhantomData;

use elp_base_db::FileId;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::SyntaxNodePtr;
use fxhash::FxHashMap;

use crate::db::MinDefDatabase;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
struct RawId(u32);

/// `FormId` points to an AST node encoding type & position of the node.
/// Use `FormId::get()` to reconstitute the AST node
pub struct FormId<N> {
    raw: RawId,
    _ty: PhantomData<fn() -> N>,
}

impl<N> Clone for FormId<N> {
    fn clone(&self) -> FormId<N> {
        *self
    }
}
impl<N> Copy for FormId<N> {}

impl<N> PartialEq for FormId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}
impl<N> Eq for FormId<N> {}
impl<N> Hash for FormId<N> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.raw.hash(hasher);
    }
}

impl<N> fmt::Debug for FormId<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FormId::<{}>({})", type_name::<N>(), self.raw.0)
    }
}

impl<N: AstNode> FormId<N> {
    pub fn get(&self, source_file: &ast::SourceFile) -> N {
        source_file
            .forms()
            .nth(self.raw.0 as usize)
            .and_then(|form| N::cast(form.syntax().clone()))
            .unwrap()
    }

    pub fn get_ast(&self, db: &dyn MinDefDatabase, file_id: FileId) -> N {
        let parsed = db.parse(file_id);
        self.get(&parsed.tree())
    }
}

impl<T: Into<ast::Form>> FormId<T> {
    pub fn upcast(self) -> FormId<ast::Form> {
        FormId {
            raw: self.raw,
            _ty: PhantomData,
        }
    }
}

// Temporary struct used during form lowering for resolving ids
#[derive(Debug, Default, Clone)]
pub struct FormIdMap {
    arena: FxHashMap<SyntaxNodePtr, RawId>,
}

impl FormIdMap {
    pub fn from_source_file(source_file: &ast::SourceFile) -> Self {
        let mut map = Self::default();
        for (id, form) in source_file.forms().enumerate() {
            let ptr = SyntaxNodePtr::new(form.syntax());
            let raw_id = RawId(id.try_into().unwrap());
            map.arena.insert(ptr, raw_id);
        }
        map
    }

    pub fn get_id<N: AstNode>(&self, node: &N) -> FormId<N> {
        let raw_id = self.arena.get(&SyntaxNodePtr::new(node.syntax())).unwrap();
        FormId {
            raw: *raw_id,
            _ty: PhantomData,
        }
    }
}

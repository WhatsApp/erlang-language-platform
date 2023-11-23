/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! See [`NavigationTarget`].

use std::fmt;

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::elp_base_db::FileRange;
use elp_ide_db::SymbolDefinition;
use elp_ide_db::SymbolKind;
use elp_syntax::ast;
use elp_syntax::AstNode;
use elp_syntax::SmolStr;
use elp_syntax::TextRange;
use hir::db::MinDefDatabase;

/// `NavigationTarget` represents an element in the editor's UI which you can
/// click on to navigate to a particular piece of code.
///
/// Typically, a `NavigationTarget` corresponds to some element in the source
/// code, like a function or a record, but this is not strictly required.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct NavigationTarget {
    pub file_id: FileId,
    /// Range which encompasses the whole element.
    ///
    /// Should include body, doc comments, attributes, etc.
    ///
    /// Clients should use this range to answer "is the cursor inside the
    /// element?" question.
    pub full_range: TextRange,
    /// A "most interesting" range within the `full_range`.
    ///
    /// Typically, `full_range` is the whole syntax node, including doc
    /// comments, and `focus_range` is the range of the identifier.
    ///
    /// Clients should place the cursor on this range when navigating to this target.
    pub focus_range: Option<TextRange>,
    pub name: SmolStr,
    pub kind: SymbolKind,
}

impl fmt::Debug for NavigationTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut f = f.debug_struct("NavigationTarget");
        f.field("file_id", &self.file_id);
        f.field("full_range", &self.full_range);
        f.field("name", &self.name);
        if let Some(focus_range) = &self.focus_range {
            f.field("focus_range", focus_range);
        }
        f.finish()
    }
}

impl NavigationTarget {
    /// Returns either the focus range, or the full range, if not available
    /// anchored to the file_id
    pub fn range(&self) -> TextRange {
        self.focus_range.unwrap_or(self.full_range)
    }

    /// Returns either the focus range, or the full range, if not available
    /// anchored to the file_id
    pub fn file_range(&self) -> FileRange {
        FileRange {
            file_id: self.file_id,
            range: self.range(),
        }
    }
}

pub trait ToNav {
    fn to_nav(&self, db: &dyn MinDefDatabase) -> NavigationTarget;
}

impl ToNav for SymbolDefinition {
    fn to_nav(&self, db: &dyn MinDefDatabase) -> NavigationTarget {
        match self {
            SymbolDefinition::Module(it) => it.to_nav(db),
            SymbolDefinition::Function(it) => it.to_nav(db),
            SymbolDefinition::Record(it) => it.to_nav(db),
            SymbolDefinition::RecordField(it) => it.to_nav(db),
            SymbolDefinition::Type(it) => it.to_nav(db),
            SymbolDefinition::Callback(it) => it.to_nav(db),
            SymbolDefinition::Define(it) => it.to_nav(db),
            SymbolDefinition::Header(it) => it.to_nav(db),
            SymbolDefinition::Var(it) => it.to_nav(db),
        }
    }
}

impl ToNav for hir::Module {
    fn to_nav(&self, db: &dyn MinDefDatabase) -> NavigationTarget {
        let file_id = self.file.file_id;
        let source = self.file.source(db.upcast());
        let full_range = source.syntax().text_range();
        let attr = self.module_attribute(db);
        let focus_range = attr
            .as_ref()
            .map(|attr| attr.form_id.get(&source).syntax().text_range());
        NavigationTarget {
            file_id,
            full_range,
            focus_range,
            name: self.name(db).raw(),
            kind: SymbolKind::Module,
        }
    }
}

impl ToNav for hir::FunctionDef {
    fn to_nav(&self, db: &dyn MinDefDatabase) -> NavigationTarget {
        let file_id = self.file.file_id;
        let source = self.source(db.upcast());
        let full_range = self.range(db.upcast()).unwrap_or_default(); // Should always succeed, but play safe
        let focus_range = source
            .iter()
            .find_map(|fun_clause| match fun_clause.clause() {
                Some(ast::FunctionOrMacroClause::FunctionClause(clause)) => {
                    clause.name().map(|name| name.syntax().text_range())
                }
                _ => None,
            });
        let arity = self.name.arity();
        let name = self.name.name().raw();
        NavigationTarget {
            file_id,
            full_range,
            focus_range,
            name: SmolStr::new(format!("{name}/{arity}")),
            kind: SymbolKind::Function,
        }
    }
}

impl ToNav for hir::RecordDef {
    fn to_nav(&self, db: &dyn MinDefDatabase) -> NavigationTarget {
        let file_id = self.file.file_id;
        let source = self.source(db.upcast());
        let full_range = source.syntax().text_range();
        let focus_range = source.name().map(|name| name.syntax().text_range());
        NavigationTarget {
            file_id,
            full_range,
            focus_range,
            name: self.record.name.raw(),
            kind: SymbolKind::Record,
        }
    }
}

impl ToNav for hir::RecordFieldDef {
    fn to_nav(&self, db: &dyn MinDefDatabase) -> NavigationTarget {
        let file_id = self.record.file.file_id;
        let source = self.source(db.upcast());
        let full_range = source.syntax().text_range();
        let focus_range = source.name().map(|name| name.syntax().text_range());
        NavigationTarget {
            file_id,
            full_range,
            focus_range,
            name: self.field.name.raw(),
            kind: SymbolKind::RecordField,
        }
    }
}

impl ToNav for hir::TypeAliasDef {
    fn to_nav(&self, db: &dyn MinDefDatabase) -> NavigationTarget {
        let file_id = self.file.file_id;
        let source = self.source(db.upcast());
        let full_range = source.syntax().text_range();
        let focus_range = source.type_name().map(|name| name.syntax().text_range());
        NavigationTarget {
            file_id,
            full_range,
            focus_range,
            name: self.type_alias.name().name().raw(),
            kind: SymbolKind::Type,
        }
    }
}

impl ToNav for hir::CallbackDef {
    fn to_nav(&self, db: &dyn MinDefDatabase) -> NavigationTarget {
        let file_id = self.file.file_id;
        let source = self.source(db.upcast());
        let full_range = source.syntax().text_range();
        let focus_range = source.fun().map(|name| name.syntax().text_range());
        NavigationTarget {
            file_id,
            full_range,
            focus_range,
            name: self.callback.name.name().raw(),
            kind: SymbolKind::Callback,
        }
    }
}

impl ToNav for hir::DefineDef {
    fn to_nav(&self, db: &dyn MinDefDatabase) -> NavigationTarget {
        let file_id = self.file.file_id;
        let source = self.source(db.upcast());
        let full_range = source.syntax().text_range();
        let focus_range = source.lhs().map(|lhs| lhs.syntax().text_range());
        NavigationTarget {
            file_id,
            full_range,
            focus_range,
            name: self.define.name.name().raw(),
            kind: SymbolKind::Define,
        }
    }
}

impl ToNav for hir::File {
    fn to_nav(&self, db: &dyn MinDefDatabase) -> NavigationTarget {
        let source = self.source(db.upcast());
        let full_range = source.syntax().text_range();
        NavigationTarget {
            file_id: self.file_id,
            full_range,
            focus_range: None,
            name: self.name(db.upcast()),
            kind: SymbolKind::File,
        }
    }
}

impl ToNav for hir::VarDef {
    fn to_nav(&self, db: &dyn MinDefDatabase) -> NavigationTarget {
        let source = self.source(db.upcast());
        let full_range = source.syntax().text_range();
        NavigationTarget {
            file_id: self.file.file_id,
            full_range,
            focus_range: None,
            name: self.name(db.upcast()).raw(),
            kind: SymbolKind::Variable,
        }
    }
}

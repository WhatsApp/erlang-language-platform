/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;

use elp_ide_db::RootDatabase;
use elp_ide_db::SymbolKind;
use elp_ide_db::elp_base_db::FileId;
use elp_syntax::TextRange;
use hir::DefMap;
use hir::DefineDef;
use hir::FunctionClauseDef;
use hir::FunctionDef;
use hir::RecordDef;
use hir::Semantic;
use hir::TypeAliasDef;
use hir::db::DefDatabase;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct DocumentSymbol {
    pub name: String,
    pub kind: SymbolKind,
    pub range: TextRange,
    pub selection_range: TextRange,
    pub deprecated: bool,
    pub detail: Option<String>,
    pub children: Option<Vec<DocumentSymbol>>,
}

impl fmt::Display for DocumentSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = format!("{:?} | {}", self.kind, self.name);
        match &self.detail {
            None => (),
            Some(detail) => s.push_str(format!(" | {detail}").as_str()),
        };
        if self.deprecated {
            s.push_str(" | deprecated")
        }
        write!(f, "{s}")
    }
}

pub trait ToDocumentSymbol {
    fn to_document_symbol(&self, db: &dyn DefDatabase, def_map: &DefMap) -> Option<DocumentSymbol>;
}

impl ToDocumentSymbol for FunctionDef {
    fn to_document_symbol(&self, db: &dyn DefDatabase, def_map: &DefMap) -> Option<DocumentSymbol> {
        let mut children = Vec::new();
        if self.function_clause_ids.len() > 1 {
            for clause_id in &self.function_clause_ids {
                if let Some(function_clause_def) = def_map.function_clauses.get(clause_id)
                    && let Some(child) = function_clause_def.to_document_symbol(db, def_map)
                {
                    children.push(child);
                }
            }
        }
        let children = if !children.is_empty() {
            Some(children)
        } else {
            None
        };
        let symbol = DocumentSymbol {
            name: self.name.to_string(),
            kind: SymbolKind::Function,
            range: self.range(db)?,
            selection_range: self.name_range(db)?,
            deprecated: self.deprecated,
            detail: None,
            children,
        };
        Some(symbol)
    }
}

impl ToDocumentSymbol for FunctionClauseDef {
    fn to_document_symbol(&self, db: &dyn DefDatabase, def_map: &DefMap) -> Option<DocumentSymbol> {
        let deprecated = def_map.is_deprecated(&self.function_clause.name);
        let range = self.range(db);
        let selection_range = self.name_range(db).unwrap_or(range);
        let symbol = DocumentSymbol {
            name: self.label(db),
            kind: SymbolKind::Function,
            range,
            selection_range,
            deprecated,
            detail: Some(self.function_clause.name.to_string()),
            children: None,
        };
        Some(symbol)
    }
}

impl ToDocumentSymbol for TypeAliasDef {
    fn to_document_symbol(
        &self,
        db: &dyn DefDatabase,
        _def_map: &DefMap,
    ) -> Option<DocumentSymbol> {
        let range = self.range(db)?;
        let selection_range = self.name_range(db).unwrap_or(range);
        let symbol = DocumentSymbol {
            name: self.name().to_string(),
            kind: SymbolKind::Type,
            range,
            selection_range,
            deprecated: false,
            detail: None,
            children: None,
        };
        Some(symbol)
    }
}

impl ToDocumentSymbol for RecordDef {
    fn to_document_symbol(
        &self,
        db: &dyn DefDatabase,
        _def_map: &DefMap,
    ) -> Option<DocumentSymbol> {
        let range = self.range(db);
        let selection_range = self.name_range(db).unwrap_or(range);
        let symbol = DocumentSymbol {
            name: self.record.name.to_string(),
            kind: SymbolKind::Record,
            range,
            selection_range,
            deprecated: false,
            detail: None,
            children: None,
        };
        Some(symbol)
    }
}

impl ToDocumentSymbol for DefineDef {
    fn to_document_symbol(
        &self,
        db: &dyn DefDatabase,
        _def_map: &DefMap,
    ) -> Option<DocumentSymbol> {
        let range = self.range(db);
        let selection_range = self.name_range(db).unwrap_or(range);
        let symbol = DocumentSymbol {
            name: self.define.name.to_string(),
            kind: SymbolKind::Define,
            range,
            selection_range,
            deprecated: false,
            detail: None,
            children: None,
        };
        Some(symbol)
    }
}

// Feature: Document Symbols
//
// Provides a list of the symbols defined in the file. Can be used to
//
// * fuzzy search symbol in a file (super useful)
// * draw breadcrumbs to describe the context around the cursor
// * draw outline of the file
//
// |===
// | Editor  | Shortcut
//
// | VS Code | kbd:[Ctrl+Shift+O]
// |===
pub(crate) fn document_symbols(db: &RootDatabase, file_id: FileId) -> Vec<DocumentSymbol> {
    let sema = Semantic::new(db);
    let def_map = sema.def_map_local(file_id);

    let mut res = Vec::new();

    for (_name, def) in def_map.get_functions() {
        if let Some(symbol) = def.to_document_symbol(db, &def_map) {
            res.push(symbol);
        }
    }
    for def in def_map.get_records().values() {
        if let Some(symbol) = def.to_document_symbol(db, &def_map) {
            res.push(symbol);
        }
    }
    for def in def_map.get_macros().values() {
        if let Some(symbol) = def.to_document_symbol(db, &def_map) {
            res.push(symbol);
        }
    }
    for def in def_map.get_types().values() {
        if let Some(symbol) = def.to_document_symbol(db, &def_map) {
            res.push(symbol);
        }
    }

    res.sort_by(|a, b| a.range.start().cmp(&b.range.start()));

    res
}

#[cfg(test)]
mod tests {

    use elp_ide_db::elp_base_db::FileRange;

    use crate::fixture;

    fn check(fixture: &str) {
        let (analysis, fixture) = fixture::with_fixture(fixture);
        let file_id = fixture.file_id();
        let expected = fixture.annotations();

        let symbols = analysis.document_symbols(file_id).unwrap();

        let mut actual = Vec::new();
        for symbol in symbols {
            actual.push((
                FileRange {
                    file_id,
                    range: symbol.selection_range,
                },
                symbol.to_string(),
            ));
            if let Some(children) = symbol.children {
                for child in children {
                    actual.push((
                        FileRange {
                            file_id,
                            range: child.selection_range,
                        },
                        child.to_string(),
                    ))
                }
            }
        }
        actual.sort_by_key(|(file_range, _)| file_range.range.start());

        assert_eq!(
            expected, actual,
            "\nExpected:\n{expected:#?}\n\nActual:\n{actual:#?}"
        )
    }

    #[test]
    fn test_file_structure() {
        check(
            r#"~
   -module(file_structure_test).

   -export([ a/1, b/0, c/0]).

   -record(my_first_record, {my_integer :: my_integer(), my_atom :: atom() }).
%%         ^^^^^^^^^^^^^^^ Record | my_first_record
   -record(my_second_record, {my_list :: [] }).
%%         ^^^^^^^^^^^^^^^^ Record | my_second_record
   -type my_integer() :: integer().
%%       ^^^^^^^^^^ Type | my_integer/0

   -define(MEANING_OF_LIFE, 42).
%%         ^^^^^^^^^^^^^^^ Define | MEANING_OF_LIFE
   -define(MEANING_OF_LIFE(X), X). % You are the owner of your own destiny.
%%         ^^^^^^^^^^^^^^^ Define | MEANING_OF_LIFE/1

   a(_) -> a.
%% ^ Function | a/1
   b() -> b.
%% ^ Function | b/0

   c() ->
%% ^ Function | c/0
     a(),
     b(),
     ok.

   ?MEANING_OF_LIFE(X, Y) ->
%% ^^^^^^^^^^^^^^^^ Function | [missing name]/2
     X + Y.
"#,
        );
    }

    #[test]
    fn test_deprecated_function() {
        check(
            r#"~
   -module(main).
   -export([ a/1, b/0]).
   -deprecated({a, 1}).
   a(_) -> a.
%% ^ Function | a/1 | deprecated
   b() -> b.
%% ^ Function | b/0
"#,
        );
    }

    #[test]
    fn test_multiple_clauses() {
        check(
            r#"~
   -module(main).
   -export([ a/1, b/0]).
   -deprecated({a, 1}).
   a(1) -> 1;
%% ^ Function | a/1 | deprecated
%% ^ Function | a(1) | a/1 | deprecated
   a(2) -> 2.
%% ^ Function | a(2) | a/1 | deprecated
   b() -> b.
%% ^ Function | b/0
"#,
        );
    }

    #[test]
    fn test_header_file() {
        check(
            r#"
//- /header.hrl
    -define(INCLUDED_MACRO, included).
    -record(included_record, {my_field :: integer()}).
    -type included_type() :: integer().
    included_function() -> ok.

//- /main.erl
    -module(main).~
    -include("header.hrl").
    -define(LOCAL_MACRO, local).
%%          ^^^^^^^^^^^ Define | LOCAL_MACRO
    -record(included_record, {my_field :: integer()}).
%%          ^^^^^^^^^^^^^^^ Record | included_record
    -type local_type() :: integer().
%%        ^^^^^^^^^^ Type | local_type/0
    local_function() -> ok.
%%  ^^^^^^^^^^^^^^ Function | local_function/0
"#,
        );
    }
}

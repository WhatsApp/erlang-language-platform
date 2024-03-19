/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::RootDatabase;
use elp_ide_db::SymbolKind;
use elp_syntax::ast::FunctionOrMacroClause;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use hir::db::DefDatabase;
use hir::DefineDef;
use hir::FunctionDef;
use hir::Name;
use hir::RecordDef;
use hir::Semantic;
use hir::TypeAliasDef;

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
            Some(detail) => s.push_str(format!(" | {}", detail).as_str()),
        };
        if self.deprecated {
            s.push_str(" | deprecated")
        }
        write!(f, "{s}")
    }
}

pub trait ToDocumentSymbol {
    fn to_document_symbol(&self, db: &dyn DefDatabase) -> DocumentSymbol;
}

impl ToDocumentSymbol for FunctionDef {
    fn to_document_symbol(&self, db: &dyn DefDatabase) -> DocumentSymbol {
        let source = self.source(db.upcast());
        let range = self
            .range(db.upcast())
            .unwrap_or(TextRange::new(0.into(), 0.into())); // default should never be needed
        let mut children = Vec::new();
        for fun_clause in source.iter() {
            if let Some(clause) = fun_clause.clause() {
                let function_name = self.name.to_string();
                let function_name_no_arity = self.name.name().to_string();
                let clause_name = match &clause {
                    FunctionOrMacroClause::FunctionClause(clause) => match clause.args() {
                        None => Name::MISSING.to_string(),
                        Some(args) => args.to_string(),
                    },
                    FunctionOrMacroClause::MacroCallExpr(_) => Name::MISSING.to_string(),
                };
                let range = clause.syntax().text_range();
                let selection_range = match &clause {
                    FunctionOrMacroClause::FunctionClause(clause) => match clause.name() {
                        None => range,
                        Some(name) => name.syntax().text_range(),
                    },
                    FunctionOrMacroClause::MacroCallExpr(_) => range,
                };
                let symbol = DocumentSymbol {
                    name: format!("{function_name_no_arity}{clause_name}"),
                    kind: SymbolKind::Function,
                    range,
                    selection_range,
                    deprecated: self.deprecated,
                    detail: Some(function_name),
                    children: None,
                };
                children.push(symbol);
            }
        }
        let selection_range = children.first().map_or(range, |c| c.selection_range);
        let children = if !children.is_empty() {
            Some(children)
        } else {
            None
        };
        DocumentSymbol {
            name: self.name.to_string(),
            kind: SymbolKind::Function,
            range,
            selection_range,
            deprecated: self.deprecated,
            detail: None,
            children,
        }
    }
}

impl ToDocumentSymbol for TypeAliasDef {
    fn to_document_symbol(&self, db: &dyn DefDatabase) -> DocumentSymbol {
        let source = self.source(db.upcast());
        let range = source.syntax().text_range();
        let selection_range = match &source.type_name() {
            None => range,
            Some(name) => name.syntax().text_range(),
        };
        DocumentSymbol {
            name: self.name().to_string(),
            kind: SymbolKind::Type,
            range,
            selection_range,
            deprecated: false,
            detail: None,
            children: None,
        }
    }
}

impl ToDocumentSymbol for RecordDef {
    fn to_document_symbol(&self, db: &dyn DefDatabase) -> DocumentSymbol {
        let source = self.source(db.upcast());
        let range = source.syntax().text_range();
        let selection_range = match &source.name() {
            None => range,
            Some(name) => name.syntax().text_range(),
        };
        DocumentSymbol {
            name: self.record.name.to_string(),
            kind: SymbolKind::Record,
            range,
            selection_range,
            deprecated: false,
            detail: None,
            children: None,
        }
    }
}

impl ToDocumentSymbol for DefineDef {
    fn to_document_symbol(&self, db: &dyn DefDatabase) -> DocumentSymbol {
        let source = self.source(db.upcast());
        let range = source.syntax().text_range();
        let selection_range = if let Some(lhs) = &source.lhs() {
            lhs.syntax().text_range()
        } else {
            range
        };
        DocumentSymbol {
            name: self.define.name.to_string(),
            kind: SymbolKind::Define,
            range,
            selection_range,
            deprecated: false,
            detail: None,
            children: None,
        }
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
    let def_map = sema.def_map(file_id);

    let mut res = Vec::new();

    for (name, def) in def_map.get_functions() {
        if def.file.file_id == file_id {
            let mut symbol = def.to_document_symbol(db);
            if def_map.is_deprecated(name) {
                symbol.deprecated = true;
            }
            res.push(symbol);
        }
    }
    for def in def_map.get_records().values() {
        if def.file.file_id == file_id {
            res.push(def.to_document_symbol(db));
        }
    }
    for def in def_map.get_macros().values() {
        if def.file.file_id == file_id {
            res.push(def.to_document_symbol(db));
        }
    }
    for def in def_map.get_types().values() {
        if def.file.file_id == file_id {
            res.push(def.to_document_symbol(db));
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
        let (analysis, pos, _diagnostics_enabled, _guard, mut expected) =
            fixture::annotations(fixture);
        let file_id = pos.file_id;
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
        expected.sort_by_key(|(file_range, _)| file_range.range.start());

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
%%       ^^^^^^^^^^^^ Type | my_integer/0

   -define(MEANING_OF_LIFE, 42).
%%         ^^^^^^^^^^^^^^^ Define | MEANING_OF_LIFE
   -define(MEANING_OF_LIFE(X), X). % You are the owner of your own destiny.
%%         ^^^^^^^^^^^^^^^^^^ Define | MEANING_OF_LIFE/1

   a(_) -> a.
%% ^ Function | a/1
%% ^ Function | a(_) | a/1
   b() -> b.
%% ^ Function | b/0
%% ^ Function | b() | b/0

   c() ->
%% ^ Function | c/0
%% ^ Function | c() | c/0
     a(),
     b(),
     ok.

   ?MEANING_OF_LIFE(X, Y) ->
%% ^^^^^^^^^^^^^^^^ Function | [missing name]/2
%% ^^^^^^^^^^^^^^^^ Function | [missing name](X, Y) | [missing name]/2
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
%% ^ Function | a(_) | a/1 | deprecated
   b() -> b.
%% ^ Function | b/0
%% ^ Function | b() | b/0
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
%% ^ Function | b() | b/0
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
%%        ^^^^^^^^^^^^ Type | local_type/0
    local_function() -> ok.
%%  ^^^^^^^^^^^^^^ Function | local_function/0
%%  ^^^^^^^^^^^^^^ Function | local_function() | local_function/0
"#,
        );
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::RootDatabase;
use elp_syntax::AstNode;
use elp_syntax::TextRange;
use hir::Semantic;

#[derive(Debug, PartialEq, Eq)]
pub enum FoldKind {
    Function,
    Record,
}

#[derive(Debug)]
pub struct Fold {
    pub range: TextRange,
    pub kind: FoldKind,
}

// Feature: Folding
//
// Defines folding regions for functions.
pub(crate) fn folding_ranges(db: &RootDatabase, file_id: FileId) -> Vec<Fold> {
    let mut folds = Vec::new();
    let sema = Semantic::new(db);
    let def_map = sema.def_map(file_id);
    // Functions
    for def in def_map.get_functions().values() {
        folds.push(Fold {
            kind: FoldKind::Function,
            range: def.source(db).syntax().text_range(),
        })
    }
    // Records
    for def in def_map.get_records().values() {
        folds.push(Fold {
            kind: FoldKind::Record,
            range: def.source(db).syntax().text_range(),
        })
    }
    folds
}

#[cfg(test)]
mod tests {
    use elp_ide_db::elp_base_db::fixture::extract_tags;

    use super::*;
    use crate::fixture;

    fn check(fixture: &str) {
        let (ranges, fixture) = extract_tags(fixture.trim_start(), "fold");
        let (analysis, file_id) = fixture::single_file(&fixture);
        let mut folds = analysis.folding_ranges(file_id).unwrap_or_default();
        folds.sort_by_key(|fold| (fold.range.start(), fold.range.end()));

        assert_eq!(
            folds.len(),
            ranges.len(),
            "The amount of folds is different than the expected amount"
        );

        for (fold, (range, attr)) in folds.iter().zip(ranges.into_iter()) {
            assert_eq!(
                fold.range.start(),
                range.start(),
                "mismatched start of folding ranges"
            );
            assert_eq!(
                fold.range.end(),
                range.end(),
                "mismatched end of folding ranges"
            );

            let kind = match fold.kind {
                FoldKind::Function | FoldKind::Record => "region",
            };
            assert_eq!(kind, &attr.unwrap());
        }
    }

    #[test]
    fn test_function() {
        check(
            r#"
-module(my_module).
<fold region>one() ->
  ok.</fold>
"#,
        )
    }

    #[test]
    fn test_record() {
        check(
            r#"
-module(my_module).
<fold region>-record(my_record, {a :: integer(), b :: binary()}).</fold>
"#,
        )
    }

    #[test]
    fn test_records_and_functions() {
        check(
            r#"
-module(my_module).

<fold region>-record(my_record, {a :: integer(),
                                 b :: binary()}).</fold>

<fold region>one() ->
  ok.</fold>

<fold region>two() ->
  ok,
  ok.</fold>
"#,
        );
    }
}

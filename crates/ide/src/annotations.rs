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
use elp_syntax::TextRange;

use crate::runnables::runnables;
use crate::runnables::Runnable;

// Feature: Annotations
//
// Provides user with annotations above items (e.g. for running tests)
//
#[derive(Debug)]
pub struct Annotation {
    pub range: TextRange,
    pub kind: AnnotationKind,
}

#[derive(Debug)]
pub enum AnnotationKind {
    Runnable(Runnable),
}

pub(crate) fn annotations(db: &RootDatabase, file_id: FileId) -> Vec<Annotation> {
    let mut annotations = Vec::default();

    for runnable in runnables(db, file_id) {
        let range = runnable.nav.range();
        annotations.push(Annotation {
            range,
            kind: AnnotationKind::Runnable(runnable),
        });
    }
    annotations
}

#[cfg(test)]
mod tests {
    use elp_ide_db::elp_base_db::FileRange;
    use stdx::trim_indent;

    use crate::fixture;
    use crate::AnnotationKind;

    #[track_caller]
    fn check(fixture: &str) {
        let (analysis, pos, mut annotations) = fixture::annotations(trim_indent(fixture).as_str());
        let actual_annotations = analysis.annotations(pos.file_id).unwrap();
        let mut actual = Vec::new();
        for annotation in actual_annotations {
            match annotation.kind {
                AnnotationKind::Runnable(runnable) => {
                    let file_id = runnable.nav.file_id;
                    let range = runnable.nav.focus_range.unwrap();
                    let text = runnable.nav.name;
                    actual.push((FileRange { file_id, range }, text.to_string()));
                }
            }
        }
        let cmp = |(frange, text): &(FileRange, String)| {
            (frange.file_id, frange.range.start(), text.clone())
        };
        actual.sort_by_key(cmp);
        annotations.sort_by_key(cmp);
        assert_eq!(actual, annotations);
    }

    #[test]
    fn annotations_no_suite() {
        check(
            r#"
-module(main).
~
main() ->
  ok.
            "#,
        );
    }

    #[test]
    fn annotations_suite() {
        check(
            r#"
//- /main_SUITE.erl
   ~
   -module(main_SUITE).
%% ^^^^^^^^^^^^^^^^^^^^ main_SUITE
main() ->
  ok.
            "#,
        );
    }
}

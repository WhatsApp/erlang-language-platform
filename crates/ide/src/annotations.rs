/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use elp_erlang_service::common_test::GroupDef;
use elp_erlang_service::TestDef;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_db::RootDatabase;
use elp_syntax::SmolStr;
use elp_syntax::TextRange;
use fxhash::FxHashMap;
use fxhash::FxHashSet;

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

pub(crate) fn annotations(_db: &RootDatabase, _file_id: FileId) -> Vec<Annotation> {
    Vec::default()
}

pub(crate) fn ct_annotations(
    db: &RootDatabase,
    file_id: FileId,
    all: FxHashSet<TestDef>,
    groups: FxHashMap<SmolStr, GroupDef>,
) -> Vec<Annotation> {
    let mut ct_annotations = Vec::default();

    for runnable in runnables(db, file_id, all, groups) {
        let range = runnable.nav.range();
        ct_annotations.push(Annotation {
            range,
            kind: AnnotationKind::Runnable(runnable),
        });
    }

    let annotations = annotations(db, file_id);

    annotations.into_iter().chain(ct_annotations).collect()
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
        let project_id = analysis.project_id(pos.file_id).unwrap().unwrap();
        let _ = analysis.db.ensure_erlang_service(project_id);
        let mut actual = Vec::new();
        for annotation in analysis.annotations(pos.file_id).unwrap() {
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
//- /main.erl scratch_buffer:true
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
//- /main_SUITE.erl scratch_buffer:true
   ~
   -module(main_SUITE).
%% ^^^^^^^^^^^^^^^^^^^^ main_SUITE
    -export([all/0]).
    all() -> [].
    main() ->
      ok.
            "#,
        );
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_erlang_service::TestDef;
use elp_erlang_service::common_test::GroupDef;
use elp_ide_db::RootDatabase;
use elp_ide_db::elp_base_db::FileId;
use elp_syntax::SmolStr;
use elp_syntax::TextRange;
use fxhash::FxHashMap;
use fxhash::FxHashSet;

// @fb-only
use crate::runnables::Runnable;
use crate::runnables::runnables;

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
    Link(Link),
}

#[derive(Debug)]
pub enum LinkKind {
    Configerator,
    ExDocs,
    LogView,
    ODS,
    Scuba,
    WAM,
}

#[derive(Debug)]
pub struct Link {
    pub kind: LinkKind,
    pub file_id: FileId,
    pub text_range: TextRange,
    pub url: String,
    pub text: String,
}

#[rustfmt::skip]
// @fb-only
pub(crate) fn annotations(_db: &RootDatabase, _file_id: FileId) -> Vec<Annotation> { // @oss-only
    // @fb-only
    let annotations = Vec::default(); // @oss-only
    // @fb-only
    annotations
}

pub(crate) fn ct_annotations(
    db: &RootDatabase,
    file_id: FileId,
    all: &FxHashSet<TestDef>,
    groups: &FxHashMap<SmolStr, GroupDef>,
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

    use crate::AnnotationKind;
    use crate::fixture;

    #[track_caller]
    fn check(fixture: &str) {
        let trimmed_fixture = trim_indent(fixture);
        let (analysis, fixture) = fixture::with_fixture(trimmed_fixture.as_str());
        let mut actual = Vec::new();
        for annotation in analysis.annotations(fixture.file_id()).unwrap() {
            if let AnnotationKind::Runnable(runnable) = annotation.kind {
                let file_id = runnable.nav.file_id;
                let range = runnable.nav.focus_range.unwrap();
                let text = runnable.nav.name;
                actual.push((FileRange { file_id, range }, text.to_string()));
            }
        }
        let cmp = |(frange, text): &(FileRange, String)| {
            (frange.file_id, frange.range.start(), text.clone())
        };
        actual.sort_by_key(cmp);
        assert_eq!(actual, fixture.annotations());
    }

    #[test]
    fn annotations_no_suite() {
        check(
            r#"
//- erlang_service
//- /main.erl
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
//- erlang_service
//- /main_SUITE.erl
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

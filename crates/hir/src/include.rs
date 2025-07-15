/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_base_db::FileId;
use elp_base_db::IncludeCtx;

use crate::InFile;
use crate::IncludeAttribute;
use crate::IncludeAttributeId;
use crate::db::DefDatabase;

pub(crate) fn resolve(
    db: &dyn DefDatabase,
    include_id: InFile<IncludeAttributeId>,
) -> Option<FileId> {
    let ctx = &IncludeCtx::new(db.upcast(), include_id.file_id);
    let form_list = db.file_form_list(ctx.file_id);
    let (path, file_id) = match &form_list[include_id.value] {
        IncludeAttribute::Include { path, .. } => (path, ctx.resolve_include(path)),
        IncludeAttribute::IncludeLib { path, .. } => (path, ctx.resolve_include_lib(path)),
    };
    if file_id.is_none() {
        let module_str = if let Some(module_attribute) = form_list.module_attribute() {
            module_attribute.name.as_str().to_string()
        } else {
            format!("{:?}", ctx.file_id)
        };
        log::warn!("Unable to resolve \"{path}\" in '{module_str}'");
    }
    file_id
}

#[cfg(test)]
mod tests {
    use elp_base_db::SourceDatabase;
    use elp_base_db::fixture::WithFixture;
    use expect_test::Expect;
    use expect_test::expect;

    use super::*;
    use crate::test_db::TestDB;

    fn check(ra_fixture: &str, expect: Expect) {
        let (db, files, _) = TestDB::with_many_files(ra_fixture);
        let file_id = files[0];
        let form_list = db.file_form_list(file_id);
        let mut resolved = form_list
            .includes()
            .map(|(idx, include)| {
                let resolved = db
                    .resolve_include(InFile::new(file_id, idx))
                    .unwrap_or_else(|| panic!("unresolved include: {include:?}"));
                let resolved_path = db
                    .source_root(db.file_source_root(resolved).source_root_id(&db))
                    .source_root(&db)
                    .path_for_file(&resolved)
                    .unwrap()
                    .clone();
                (include, resolved_path)
            })
            .map(|(include, resolved)| match include {
                IncludeAttribute::Include { path, .. } => {
                    format!("-include({path:?}). % => {resolved}")
                }
                IncludeAttribute::IncludeLib { path, .. } => {
                    format!("-include_lib({path:?}). % => {resolved}")
                }
            })
            .collect::<Vec<_>>()
            .join("\n");
        resolved.push('\n');
        expect.assert_eq(&resolved);
    }

    #[test]
    fn relative() {
        check(
            r#"
//- /src/module.erl
-include("header.hrl").
-include_lib("header.hrl").
//- /src/header.hrl
"#,
            expect![[r#"
                -include("header.hrl"). % => /src/header.hrl
                -include_lib("header.hrl"). % => /src/header.hrl
            "#]],
        )
    }

    #[test]
    fn include_path() {
        check(
            r#"
//- /src/module.erl include_path:/include
-include("header.hrl").
-include_lib("header.hrl").
//- /include/header.hrl
"#,
            expect![[r#"
                -include("header.hrl"). % => /include/header.hrl
                -include_lib("header.hrl"). % => /include/header.hrl
            "#]],
        )
    }

    #[test]
    fn lib() {
        check(
            r#"
//- /main/src/module.erl app:main
-include_lib("another/include/header.hrl").
//- /another-app/include/header.hrl app:another
"#,
            expect![[r#"
                -include_lib("another/include/header.hrl"). % => /another-app/include/header.hrl
            "#]],
        )
    }
}

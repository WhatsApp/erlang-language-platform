/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use elp_base_db::FileId;
use elp_base_db::SourceRoot;
use elp_base_db::SourceRootId;

use crate::db::DefDatabase;
use crate::InFile;
use crate::IncludeAttribute;
use crate::IncludeAttributeId;

struct IncludeCtx<'a> {
    db: &'a dyn DefDatabase,
    source_root_id: SourceRootId,
    source_root: Arc<SourceRoot>,
    file_id: FileId,
}

pub(crate) fn resolve(
    db: &dyn DefDatabase,
    include_id: InFile<IncludeAttributeId>,
) -> Option<FileId> {
    resolve_in_ctx(&IncludeCtx::new(db, include_id.file_id), include_id.value)
}

fn resolve_in_ctx(ctx: &IncludeCtx, id: IncludeAttributeId) -> Option<FileId> {
    let form_list = ctx.db.file_form_list(ctx.file_id);
    let (path, file_id) = match &form_list[id] {
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

impl<'a> IncludeCtx<'a> {
    fn new(db: &'a dyn DefDatabase, file_id: FileId) -> Self {
        // Context for T171541590
        let _ = stdx::panic_context::enter(format!("\nIncludeCtx::new: {:?}", file_id));
        let source_root_id = db.file_source_root(file_id);
        let source_root = db.source_root(source_root_id);
        Self {
            db,
            file_id,
            source_root_id,
            source_root,
        }
    }

    fn resolve_include(&self, path: &str) -> Option<FileId> {
        self.resolve_relative(path)
            .or_else(|| self.resolve_local(path))
    }

    fn resolve_include_lib(&self, path: &str) -> Option<FileId> {
        self.resolve_include(path)
            .or_else(|| self.resolve_remote(path))
    }

    fn resolve_relative(&self, path: &str) -> Option<FileId> {
        self.source_root.relative_path(self.file_id, path)
    }

    fn resolve_local(&self, path: &str) -> Option<FileId> {
        let app_data = self.db.app_data(self.source_root_id)?;
        app_data.include_path.iter().find_map(|include| {
            let name = include.join(path);
            self.source_root.file_for_path(&name.into())
        })
    }

    fn resolve_remote(&self, path: &str) -> Option<FileId> {
        let app_data = self.db.app_data(self.source_root_id)?;
        let project_data = self.db.project_data(app_data.project_id);

        let (app_name, path) = path.split_once('/')?;
        let source_root_id = project_data.app_roots.get(app_name)?;
        let source_root = self.db.source_root(source_root_id);
        let target_app_data = self.db.app_data(source_root_id)?;
        let path = target_app_data.dir.join(path);
        source_root.file_for_path(&path.into())
    }
}

#[cfg(test)]
mod tests {
    use elp_base_db::fixture::WithFixture;
    use elp_base_db::SourceDatabase;
    use expect_test::expect;
    use expect_test::Expect;

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
                    .unwrap_or_else(|| panic!("unresolved include: {:?}", include));
                let resolved_path = db
                    .source_root(db.file_source_root(resolved))
                    .path_for_file(&resolved)
                    .unwrap()
                    .clone();
                (include, resolved_path)
            })
            .map(|(include, resolved)| match include {
                IncludeAttribute::Include { path, .. } => {
                    format!("-include({:?}). % => {}", path, resolved)
                }
                IncludeAttribute::IncludeLib { path, .. } => {
                    format!("-include_lib({:?}). % => {}", path, resolved)
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

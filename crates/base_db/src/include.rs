/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use elp_project_model::AppName;
use elp_project_model::buck::DepKind;
use elp_project_model::buck::IncludeMappingScope;
use elp_syntax::SmolStr;
use vfs::FileId;
use vfs::VfsPath;

use crate::App;
use crate::Project;
use crate::RootQueryDb;
use crate::SourceRoot;

pub struct IncludeCtx<'a> {
    db: &'a dyn RootQueryDb,
    source_root: Arc<SourceRoot>,
    /// The app of the originating `.erl` file when resolving includes.
    /// The dependency graph for includes is calculated based on the
    /// originating app's data, if set.
    pub orig_app: Option<App>,
    /// The current `FileId`. This starts out the same as the
    /// originating file, but will change if a nested include file is
    /// processed.
    pub current_file_id: FileId,
}

impl<'a> IncludeCtx<'a> {
    pub fn new(db: &'a dyn RootQueryDb, orig_app: Option<App>, current_file_id: FileId) -> Self {
        // Context for T171541590
        let _ = stdx::panic_context::enter(format!(
            "\nIncludeCtx::new: {orig_app:?} {current_file_id:?}"
        ));
        let source_root_id = db.file_source_root(current_file_id).source_root_id(db);
        let source_root = db.source_root(source_root_id).source_root(db);
        Self {
            db,
            orig_app,
            current_file_id,
            source_root,
        }
    }

    pub fn resolve_include(&self, path: &str) -> Option<FileId> {
        // Note, from https://www.erlang.org/doc/apps/erts/erlc_cmd#generally-useful-flags
        // When encountering an `-include` or `-include_lib` directive,
        // the compiler searches for header files in the following directories:
        //
        //   - ".", the current working directory of the file server
        //   - The base name of the compiled file
        //   - The directories specified using option -I; the directory
        //     specified last is searched first
        self.resolve_relative(path)
            .or_else(|| self.db.resolve_local(self.current_file_id, path.into()))
    }

    pub fn resolve_include_lib(&self, path: &str) -> Option<FileId> {
        self.resolve_include(path).or_else(|| {
            self.db
                .resolve_remote(self.orig_app, self.current_file_id, path.into())
        })
    }

    pub fn resolve_include_doc(&self, path: &str) -> Option<FileId> {
        self.resolve_relative(path)
    }

    fn resolve_relative(&self, path: &str) -> Option<FileId> {
        self.source_root.relative_path(self.current_file_id, path)
    }

    pub(crate) fn resolve_local_dispatch(
        db: &dyn RootQueryDb,
        file_id: FileId,
        path: SmolStr,
    ) -> Option<FileId> {
        db.resolve_local_interned(crate::InternedFileId::new(db, file_id), path)
    }

    /// Called via salsa for inserting in the graph.  We are looking
    /// for a base filename in the includes of the current app (from
    /// the `file_id`) or any of its dependencies
    pub(crate) fn resolve_local_inner(
        db: &dyn RootQueryDb,
        fid: crate::InternedFileId,
        path: SmolStr,
    ) -> Option<FileId> {
        let file_id = fid.file_id(db);
        let project = db.file_project(file_id)?;
        let app = db.file_app(file_id)?;
        if let Some(file_id) = db.mapped_include_file(
            project,
            IncludeMappingScope::Local(app.name(db).clone()),
            path.clone(),
        ) {
            Some(file_id)
        } else {
            // Not in the current app, look in the dependencies
            let include_file_index = db.include_file_index(project);
            if let Some(file_path) = include_file_index
                .buck_index
                .find_local(app.name(db), &path)
            {
                include_file_index
                    .path_to_file_id
                    .get(&VfsPath::from(file_path.clone()))
                    .copied()
            } else {
                // Fallback for non-buck2 projects
                let path: &str = &path;
                app.include_path(db).iter().find_map(|include| {
                    let name = include.join(path);
                    db.include_file_id(app.project(db), VfsPath::from(name.clone()))
                })
            }
        }
    }

    /// Called via salsa for inserting in the graph
    /// When processing a .erl file, it can include other files, and so on recursively.
    /// In this case, the originating app is identified by `orig_app`, and the current file is
    /// the one being processed.
    pub(crate) fn resolve_remote_query(
        db: &dyn RootQueryDb,
        orig_app: Option<App>,
        current_file_id: FileId,
        path: SmolStr,
    ) -> Option<FileId> {
        let project = db.file_project(current_file_id)?;

        // Gather app data from both the originating app and the current
        // file's app.  For the dep check we accept the include if
        // *either* app has the target as a dependency.  This handles:
        //   - A→B→C where A deps C but B doesn't (orig check passes)
        //   - A→B→C where B deps C but A doesn't (current check passes)
        let current_app = db.file_app(current_file_id);

        // Need at least one valid app to proceed.
        orig_app.or(current_app)?;

        let (app_name, include_path) = path.split_once('/')?;
        let target_app = project.app_named(db, AppName(app_name.to_string()))?;
        if let Some(buck_index) = project.buck_index(db) {
            if let Some(p) = buck_index
                .includes
                .get(IncludeMappingScope::Remote, &path)
                .map(|path| db.include_file_id(project, VfsPath::from(path.clone())))
            {
                if p.is_some() {
                    // We have an entry in the include mapping, and it maps to a FileId.
                    // Accept if the target is a dep of either the originating app
                    // or the current file's app.
                    let target = AppName(app_name.to_string());
                    let is_dep = [orig_app, current_app].into_iter().flatten().any(|app| {
                        app.buck_target_name(db).as_ref().is_some_and(|t| {
                            db.reachable_targets(project, t.clone(), DepKind::Runtime)
                                .is_some_and(|reachable| {
                                    buck_index.app_deps.is_reachable_in(&reachable, &target)
                                })
                        })
                    });

                    if is_dep {
                        p
                    } else {
                        // We have a lookup value, but neither the
                        // originating app nor the current file's app
                        // has it as a dependency.
                        None
                    }
                } else {
                    // We do have an entry in the include mapping, but
                    // it does not resolve to a valid FileId.
                    // This should also not happen.
                    let name = orig_app.or(current_app).map(|app| app.name(db).clone());
                    log::warn!(
                        "include mapping does not resolve to FileId: app:{:?}, path:{}, p:{:?}",
                        name,
                        path,
                        p
                    );
                    None
                }
            } else {
                // We did not find an entry in the include mapping.
                None
            }
        } else {
            // There is no include mapping.
            // This is the path followed when it is not a buck2
            // project, as those are currently the only ones that
            // populate the buck index.
            let path = target_app.dir_path(db).join(include_path);
            db.include_file_id(project, VfsPath::from(path.clone()))
                .or_else(|| find_generated_include_lib(db, project, include_path, target_app))
        }
    }
}

fn find_generated_include_lib(
    db: &dyn RootQueryDb,
    project: Project,
    include_path: &str,
    target_app: App,
) -> Option<FileId> {
    // buck2 builds create an include file mapping when
    // invoking the OTP compiler, by manipulating symlinks
    // in the output directory.
    //
    // Since we are only generating some files, not
    // building them, and we prefer to work with the files
    // in their canonical locations, we deal with this
    // here.
    //

    // The target app has an `include_path` field. An "include/"
    // prefix on the path should be replaced with an entry from the
    // set of include paths/
    // Note: we use the more relaxed include_path here, as the thrift
    // includes are not all for direct dependencies at present.
    let path = include_path.strip_prefix("include/")?;
    target_app.include_path(db).iter().find_map(|dir| {
        let path = dir.join(path);
        db.include_file_id(project, VfsPath::from(path.clone()))
    })
}

pub fn generated_file_include_lib(
    db: &dyn RootQueryDb,
    file_id: FileId,
    included_file_id: FileId,
    include_path: VfsPath,
) -> Option<String> {
    // In `find_generated_include_lib`, the processing does
    // - split the path into app and rest
    // - get the app based on the app name
    // - if the rest starts with "include/"
    //   - strip it
    //   - find a dir in the app include_path list such that the
    //     concatenation gives a valid file
    //
    // We need to do the reverse here.

    let inc_app = db.file_app(included_file_id)?;
    let candidate_path = inc_app
        // Note: we use include_dirs here, as it keeps the include dirs local to the app only.
        // include_path is the search path including all dependency include_dir`s
        .include_dirs(db)
        .iter()
        .find_map(|dir| include_path.as_path()?.strip_prefix(dir))?;
    let candidate = format!("{}/include/{}", inc_app.name(db), candidate_path.as_str());
    let resolved_file_id =
        IncludeCtx::new(db, db.file_app(file_id), file_id).resolve_include_lib(&candidate)?;
    if resolved_file_id == included_file_id {
        // We have an equivalent include
        Some(candidate)
    } else {
        None
    }
}

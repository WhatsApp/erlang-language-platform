/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cmp;
use std::hash::BuildHasherDefault;
use std::sync::Arc;

use elp_ide::elp_ide_db::RootDatabase;
use elp_ide::elp_ide_db::elp_base_db::ChangeKind;
use elp_ide::elp_ide_db::elp_base_db::FileMap;
use elp_ide::elp_ide_db::elp_base_db::FileSetConfig;
use elp_ide::elp_ide_db::elp_base_db::LineEndings;
use elp_ide::elp_ide_db::elp_base_db::ProjectApps;
use elp_ide::elp_ide_db::elp_base_db::ProjectId;
use elp_ide::elp_ide_db::elp_base_db::SourceDatabase;
use elp_ide::elp_ide_db::elp_base_db::SourceRoot;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use elp_ide::elp_ide_db::elp_base_db::loader;
use elp_ide::elp_ide_db::elp_base_db::set_source_roots;
use elp_project_model::ProjectAppData;
use elp_project_model::ProjectBuildData;
use elp_project_model::buck::BuckProject;
use elp_project_model::eqwalizer_support;
use fxhash::FxHashSet;
use fxhash::FxHasher;
use include_dir::Dir;
use include_dir::include_dir;
use indexmap::IndexMap;
use vfs::AbsPathBuf;
use vfs::FileId;

use crate::document::Document;

static EQWALIZER_SUPPORT: Dir = include_dir!("$EQWALIZER_SUPPORT_DIR");

#[derive(Debug)]
pub struct ProjectFolders {
    pub load: Vec<loader::Entry>,
    pub watch: Vec<lsp_types::FileSystemWatcher>,
    pub file_set_config: FileSetConfig,
}

pub fn watch_paths_for_app(
    app: &ProjectAppData,
    project_id: ProjectId,
    otp_project_id: Option<ProjectId>,
) -> Vec<String> {
    let mut paths = Vec::new();
    for root in app.all_dirs_to_watch() {
        if Some(project_id) != otp_project_id {
            paths.push(format!("{root}/**/*.{{e,h}}rl"));
        }
    }
    for file in app.all_files_to_watch() {
        paths.push(file.to_string());
    }
    paths
}

pub fn project_root_watch_paths(root: &AbsPathBuf) -> Vec<String> {
    vec![
        format!("{}/**/BUCK", root),
        format!("{}/**/TARGETS", root),
        format!("{}/**/TARGETS.v2", root),
        format!("{}/**/.elp.toml", root),
        format!("{}/**/.elp_lint.toml", root),
        format!("{}/**/rebar.{{config,config.script,lock}}", root),
    ]
}

fn buck_project_watch_paths(buck_project: &BuckProject) -> Vec<String> {
    let dirs = buck_project.buck_conf.included_target_dirs();
    if dirs.is_empty() {
        let root = buck_project.buck_conf.source_root();
        return project_root_watch_paths(&root);
    }
    let mut paths: Vec<String> = dirs
        .iter()
        .flat_map(|dir| {
            [
                format!("{}/**/BUCK", dir),
                format!("{}/**/TARGETS", dir),
                format!("{}/**/TARGETS.v2", dir),
            ]
        })
        .collect();
    if let Some(config_dir) = buck_project.buck_conf.config_dir() {
        paths.push(format!("{}/**/.elp.toml", config_dir));
        paths.push(format!("{}/**/.elp_lint.toml", config_dir));
    }
    paths
}

impl ProjectFolders {
    pub fn new(project_apps: &ProjectApps) -> ProjectFolders {
        let file_set_config = project_apps
            .all_apps
            .iter()
            .fold(
                FileSetConfig::builder(),
                |mut builder, (_project_id, app)| {
                    let mut file_sets: FxHashSet<VfsPath> = app
                        .abs_src_dirs
                        .iter()
                        .map(|src| VfsPath::from(src.clone()))
                        .collect();
                    let dir = VfsPath::from(app.dir.clone());
                    file_sets.insert(dir);
                    builder.add_file_set(file_sets.into_iter().collect());
                    builder
                },
            )
            .build();

        let load = loader_config(project_apps);

        let otp_project_id = project_apps.otp_project_id;

        let mut watch_paths: FxHashSet<_> = project_apps
            .all_apps
            .iter()
            .flat_map(|(project_id, app)| watch_paths_for_app(app, *project_id, otp_project_id))
            .collect();

        for project in &project_apps.projects {
            match &project.project_build_data {
                ProjectBuildData::Buck(buck_project) => {
                    watch_paths.extend(buck_project_watch_paths(buck_project));
                }
                _ => {
                    let root = project.root();
                    watch_paths.extend(project_root_watch_paths(&root));
                }
            }
        }

        // LSP spec says "If omitted it defaults to
        // WatchKind.Create | WatchKind.Change | WatchKind.Delete
        // which is 7".
        let kind = None;
        let watch: Vec<_> = watch_paths
            .into_iter()
            .map(|glob_pattern| lsp_types::FileSystemWatcher {
                glob_pattern: lsp_types::GlobPattern::Pattern(glob_pattern),
                kind,
            })
            .collect();

        ProjectFolders {
            load,
            watch,
            file_set_config,
        }
    }
}

/// The files in the `src` directory of the `eqwalizer_support` app bundled with
/// ELP, to be set in the VFS directly, since the loader never sees them. Like the
/// OTP apps, the app is shared by all projects.
pub(crate) fn bundled_files(
    project_apps: &ProjectApps<'_>,
) -> impl Iterator<Item = (VfsPath, Vec<u8>)> + use<> {
    let dir = project_apps.otp_project_id.map(|ProjectId(idx)| {
        eqwalizer_support::bundled_dir(&project_apps.projects[idx as usize].otp)
    });
    dir.into_iter().flat_map(|dir| {
        EQWALIZER_SUPPORT
            .get_dir("src")
            .into_iter()
            .flat_map(Dir::files)
            .filter_map(move |file| {
                let Some(path) = file.path().to_str() else {
                    log::warn!(
                        "Skipping bundled file with a non-UTF-8 path: {:?}",
                        file.path()
                    );
                    return None;
                };
                Some((VfsPath::from(dir.join(path)), file.contents().to_vec()))
            })
    })
}

fn loader_config(project_apps: &ProjectApps<'_>) -> Vec<loader::Entry> {
    let mut app_dirs = FxHashSet::default();
    let mut include_dirs = FxHashSet::default();
    let mut files = FxHashSet::default();
    project_apps.all_apps.iter().for_each(|(_, app)| {
        app_dirs.extend(app.all_source_dirs());
        if let Some(applicable_files) = app.applicable_files.as_ref() {
            files.extend(applicable_files.clone());
        }
        include_dirs.extend(app.include_dirs.clone());
    });

    let app_dirs_vec: Vec<AbsPathBuf> = app_dirs.into_iter().collect();
    let include_dirs_vec: Vec<AbsPathBuf> = include_dirs.into_iter().collect();
    // Create chunks so our loader spinner shows progress. And to
    // interleave loading and storing
    // There is no particular significance to the numbers chosen, the
    // intent is to have a reasonable progress bar, with 100 as the total.
    // We split the chunks arbitrarily between apps and include dirs, leaving
    // room for the files too.
    // For some reason our progress reporter lists this as 100, even
    // though the total seems to be 80 + 16 + 1 = 97

    let apps_chunk_size = cmp::max(app_dirs_vec.len() / 80, 1);
    let include_dirs_chunk_size = cmp::max(include_dirs_vec.len() / 16, 1);

    let mut load: Vec<_> = vec![];
    load.extend(
        app_dirs_vec
            .chunks(apps_chunk_size)
            .map(|chunk| -> Vec<AbsPathBuf> { chunk.into() })
            .map(|include| {
                loader::Entry::Directories(loader::Directories {
                    extensions: vec!["erl".to_string(), "hrl".to_string(), "escript".to_string()],
                    include,
                    exclude: vec![],
                })
            }),
    );
    load.extend(
        include_dirs_vec
            .chunks(include_dirs_chunk_size)
            .map(|chunk| -> Vec<AbsPathBuf> { chunk.into() })
            .map(|include| {
                loader::Entry::Directories(loader::Directories {
                    extensions: vec!["hrl".to_string()],
                    include,
                    exclude: vec![],
                })
            }),
    );
    // Put the files last to catch anything missed in the prior loads.
    // TODO: consider removing files that will already be selected
    // from the directory load entries. This needs a many-many
    // correlation though.
    load.push(loader::Entry::Files(files.into_iter().collect()));
    load
}

/// New file contents by path, `None` for a deleted file. The last write to a
/// path wins, and paths are processed in the order they were first written.
pub type PendingChanges = IndexMap<VfsPath, Option<Vec<u8>>, BuildHasherDefault<FxHasher>>;

pub struct FileChange {
    pub file_id: FileId,
    pub kind: ChangeKind,
    /// `None` for a deleted file.
    contents: Option<(Arc<str>, LineEndings)>,
}

/// The pending changes whose text or line endings differ from what is held.
/// Reads the database without writing it, so it can run before snapshots are
/// drained. Interns new paths.
pub fn file_changes(
    db: &RootDatabase,
    file_map: &FileMap,
    pending: PendingChanges,
) -> Vec<FileChange> {
    pending
        .into_iter()
        .filter_map(|(path, contents)| {
            let file_id = file_map.intern(db, path);
            let file = db.file(file_id);
            let existed = file.exists(db);
            let Some(contents) = contents else {
                return existed.then_some(FileChange {
                    file_id,
                    kind: ChangeKind::Delete,
                    contents: None,
                });
            };
            // Text held with Unix line endings has no `\r`, so bytes equal to it
            // decode and normalise to themselves: an unchanged file is
            // recognised without allocating.
            if existed
                && file.line_endings(db) == LineEndings::Unix
                && file.text(db).as_bytes() == contents.as_slice()
            {
                return None;
            }
            let (text, line_endings) = Document::from_bytes(&contents).vfs_to_salsa();
            if existed && **file.text(db) == *text && file.line_endings(db) == line_endings {
                return None;
            }
            Some(FileChange {
                file_id,
                kind: if existed {
                    ChangeKind::Modify
                } else {
                    ChangeKind::Create
                },
                contents: Some((Arc::from(text), line_endings)),
            })
        })
        .collect()
}

/// Applies changes to the salsa database. The first write waits for every
/// outstanding snapshot to drop, so the LSP server drains them first, with
/// `RootDatabase::request_cancellation`, to time that wait on its own.
pub fn write_file_changes(db: &mut RootDatabase, changes: Vec<FileChange>) {
    for change in changes {
        db.file(change.file_id).set_contents(db, change.contents);
    }
}

pub fn apply_source_roots(
    db: &mut RootDatabase,
    file_map: &FileMap,
    file_set_config: &FileSetConfig,
) {
    let roots = file_set_config
        .partition(file_map, db)
        .into_iter()
        .map(SourceRoot::new)
        .collect();
    set_source_roots(db, roots);
}

#[cfg(test)]
mod tests {
    use elp_ide::elp_ide_db::RootDatabase;
    use elp_ide::elp_ide_db::elp_base_db::ChangeKind;
    use elp_ide::elp_ide_db::elp_base_db::FileMap;
    use elp_ide::elp_ide_db::elp_base_db::IncludeOtp;
    use elp_ide::elp_ide_db::elp_base_db::LineEndings;
    use elp_ide::elp_ide_db::elp_base_db::ProjectApps;
    use elp_ide::elp_ide_db::elp_base_db::RootQueryDb;
    use elp_ide::elp_ide_db::elp_base_db::SourceDatabase;
    use elp_ide::elp_ide_db::elp_base_db::VfsPath;
    use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;
    use elp_ide::elp_ide_db::elp_base_db::lower;
    use elp_project_model::AppName;
    use elp_project_model::Project;
    use elp_project_model::ProjectAppData;
    use elp_project_model::otp::Otp;
    use fxhash::FxHashSet;
    use vfs::AbsPathBuf;
    use vfs::FileId;

    use super::file_changes;
    use super::write_file_changes;

    #[derive(Default)]
    struct Files {
        db: RootDatabase,
        file_map: FileMap,
    }

    impl Files {
        /// Applies `changes` as one batch, and returns what changed.
        fn apply(&mut self, changes: &[(&str, Option<&str>)]) -> Vec<(FileId, ChangeKind)> {
            let pending = changes
                .iter()
                .map(|(path, text)| (path_of(path), text.map(|text| text.as_bytes().to_vec())))
                .collect();
            let changes = file_changes(&self.db, &self.file_map, pending);
            let changed = changes
                .iter()
                .map(|change| (change.file_id, change.kind))
                .collect();
            write_file_changes(&mut self.db, changes);
            changed
        }

        /// The id of `path`, if the file exists.
        fn file_id(&self, path: &VfsPath) -> Option<FileId> {
            self.file_map
                .file_id(path)
                .filter(|file_id| self.db.file(*file_id).exists(&self.db))
        }

        fn text(&self, file_id: FileId) -> String {
            self.db.file(file_id).text(&self.db).to_string()
        }

        fn line_endings(&self, file_id: FileId) -> LineEndings {
            self.db.file(file_id).line_endings(&self.db)
        }
    }

    fn path_of(path: &str) -> VfsPath {
        VfsPath::new_real_path(path.to_string())
    }

    #[test]
    fn identical_contents_record_no_change() {
        let mut files = Files::default();
        let created = files.apply(&[("/a.erl", Some("-module(a)."))]);
        let file_id = files.file_id(&path_of("/a.erl")).unwrap();
        assert_eq_expected!(vec![(file_id, ChangeKind::Create)], created);

        let reloaded = files.apply(&[("/a.erl", Some("-module(a)."))]);
        assert_eq_expected!(Vec::<(FileId, ChangeKind)>::new(), reloaded);

        let modified = files.apply(&[("/a.erl", Some("-module(b)."))]);
        assert_eq_expected!(vec![(file_id, ChangeKind::Modify)], modified);
        assert_eq_expected!("-module(b).", files.text(file_id));
    }

    #[test]
    fn the_last_write_to_a_path_in_a_batch_wins() {
        let mut files = Files::default();
        let changed = files.apply(&[
            ("/a.erl", Some("-module(first).")),
            ("/b.erl", None),
            ("/a.erl", None),
            ("/a.erl", Some("-module(last).")),
        ]);

        // `/b.erl` never existed, so deleting it is no change, though it is
        // interned after `/a.erl`, in the order the paths were first written.
        let file_id = FileId::from_raw(0);
        assert_eq_expected!(vec![(file_id, ChangeKind::Create)], changed);
        assert_eq_expected!("-module(last).", files.text(file_id));
        assert_eq_expected!(None, files.file_id(&path_of("/b.erl")));
        assert_eq_expected!(
            FileId::from_raw(1),
            files.file_map.intern(&files.db, path_of("/b.erl"))
        );
    }

    #[test]
    fn a_deleted_file_is_emptied_and_keeps_its_id() {
        let mut files = Files::default();
        files.apply(&[("/a.erl", Some("-module(a)."))]);
        let file_id = files.file_id(&path_of("/a.erl")).unwrap();

        let deleted = files.apply(&[("/a.erl", None)]);
        assert_eq_expected!(vec![(file_id, ChangeKind::Delete)], deleted);
        assert_eq_expected!("", files.text(file_id));
        assert_eq_expected!(None, files.file_id(&path_of("/a.erl")));

        let recreated = files.apply(&[("/a.erl", Some("-module(a)."))]);
        assert_eq_expected!(vec![(file_id, ChangeKind::Create)], recreated);
        assert_eq_expected!(Some(file_id), files.file_id(&path_of("/a.erl")));
    }

    #[test]
    fn a_recreated_file_keeps_its_file() {
        let mut files = Files::default();
        files.apply(&[("/a.erl", Some("-module(a)."))]);
        let file_id = files.file_id(&path_of("/a.erl")).unwrap();
        let file = files.db.file(file_id);

        files.apply(&[("/a.erl", None)]);
        assert_eq_expected!(false, file.exists(&files.db));

        files.apply(&[("/a.erl", Some("-module(b)."))]);
        assert_eq!(
            file,
            files.db.file(file_id),
            "the recreated file should have the same File"
        );
        assert_eq_expected!(true, file.exists(&files.db));
        assert_eq_expected!("-module(b).", &**file.text(&files.db));
    }

    #[test]
    fn a_change_of_line_endings_alone_is_a_modification() {
        let mut files = Files::default();
        files.apply(&[("/a.erl", Some("-module(a).\n"))]);
        let file_id = files.file_id(&path_of("/a.erl")).unwrap();

        let modified = files.apply(&[("/a.erl", Some("-module(a).\r\n"))]);
        assert_eq_expected!(vec![(file_id, ChangeKind::Modify)], modified);
        assert_eq_expected!("-module(a).\n", files.text(file_id));
        assert_eq_expected!(LineEndings::Dos, files.line_endings(file_id));
    }

    #[test]
    fn a_change_back_to_unix_line_endings_is_a_modification() {
        let mut files = Files::default();
        files.apply(&[("/a.erl", Some("-module(a).\r\n"))]);
        let file_id = files.file_id(&path_of("/a.erl")).unwrap();

        let modified = files.apply(&[("/a.erl", Some("-module(a).\n"))]);
        assert_eq_expected!(vec![(file_id, ChangeKind::Modify)], modified);
        assert_eq_expected!(LineEndings::Unix, files.line_endings(file_id));
    }

    #[test]
    fn a_listed_file_has_its_app_before_it_loads() {
        let mut files = Files::default();
        let listed = AbsPathBuf::assert("/app/src/listed.erl".into());
        let mut app = ProjectAppData::fixture_app_data(
            AppName("app".to_string()),
            AbsPathBuf::assert("/app".into()),
            vec![],
            vec![AbsPathBuf::assert("/app/src".into())],
            vec![],
        );
        app.applicable_files = Some(FxHashSet::from_iter([listed.clone()]));
        let mut project = Project::empty(Otp {
            lib_dir: AbsPathBuf::assert("/otp".into()),
        });
        project.add_apps(vec![app]);
        let projects = [project];
        let project_apps = ProjectApps::new(&projects, IncludeOtp::No);
        lower(&mut files.db, &files.file_map, &project_apps);

        let file_id = files.file_map.intern(&files.db, VfsPath::from(listed));
        let app_name = |files: &Files| {
            files
                .db
                .file_app_data(file_id)
                .map(|app_data| app_data.name.clone())
        };
        let expected_app = Some(AppName("app".to_string()));
        assert_eq_expected!(None, files.file_id(&path_of("/app/src/listed.erl")));
        assert_eq_expected!(expected_app, app_name(&files), "before the file loads");

        files.apply(&[("/app/src/listed.erl", Some("-module(listed)."))]);
        assert_eq_expected!(expected_app, app_name(&files), "once the file has loaded");
    }
}

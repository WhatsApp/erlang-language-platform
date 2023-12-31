/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::iter;
use std::mem;
use std::ops::Deref;
use std::ops::DerefMut;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;
use std::vec;

use anyhow::bail;
use anyhow::Context;
use anyhow::Result;
use buck::EqwalizerConfig;
use elp_log::timeit;
use parking_lot::MutexGuard;
use paths::AbsPath;
use paths::AbsPathBuf;
use tempfile::NamedTempFile;
use tempfile::TempPath;
use thiserror::Error;

use crate::buck::BuckProject;
use crate::json::JsonConfig;
use crate::otp::Otp;
use crate::rebar::Profile;
use crate::rebar::RebarConfig;
use crate::rebar::RebarProject;

pub mod buck;
pub mod eqwalizer_support;
pub mod json;
pub mod no_manifest;
pub mod otp;
pub mod rebar;
pub mod test_fixture;

pub struct CommandProxy<'a>(MutexGuard<'a, ()>, Command);

impl<'a> Deref for CommandProxy<'a> {
    type Target = Command;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<'a> DerefMut for CommandProxy<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.1
    }
}

#[derive(Debug, Clone)]
pub struct DiscoverConfig {
    pub rebar: bool,
    pub rebar_profile: Profile,
}

impl DiscoverConfig {
    pub fn new(rebar: bool, profile: &str) -> Self {
        if rebar {
            Self::rebar(Some(profile.to_owned()))
        } else {
            Self::buck()
        }
    }

    pub fn rebar(profile: Option<String>) -> Self {
        let rebar_profile = profile.map(Profile).unwrap_or_default();
        Self {
            rebar: true,
            rebar_profile,
        }
    }

    pub fn buck() -> DiscoverConfig {
        Self {
            rebar: false,
            rebar_profile: Default::default(),
        }
    }
}

impl Display for DiscoverConfig {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.rebar {
            write!(f, "rebar --profile {}", self.rebar_profile.0)
        } else {
            write!(f, "buck")
        }
    }
}

#[derive(Error, Debug)]
pub enum ProjectModelError {
    #[error("Buck2 was not found. Try to run buck2 --help from command line")]
    MissingBuck(#[source] std::io::Error),
    #[error("Not in buck project")]
    NotInBuckProject,
    #[error("Rebar3 was not found. Try to run rebar3 --help from command line")]
    MissingRebar(#[source] std::io::Error),
    #[error(
        "build-info plugin was not installed. Please follow the instructions on https://github.com/WhatsApp/eqwalizer"
    )]
    NoBuildInfo,
    #[error("Failed to parse build info json")]
    InvalidBuildInfoJson(#[source] anyhow::Error),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProjectManifest {
    Rebar(RebarConfig),
    Toml(buck::ElpConfig),
    Json(JsonConfig),
    NoManifest(no_manifest::NoManifestConfig),
}

impl ProjectManifest {
    pub fn root(&self) -> &AbsPath {
        match self {
            ProjectManifest::Rebar(conf) => conf.config_path(),
            ProjectManifest::Toml(conf) => conf.config_path(),
            ProjectManifest::Json(conf) => conf.config_path(),
            ProjectManifest::NoManifest(conf) => conf.config_path(),
        }
    }

    fn find_in_dir<'a>(
        path: &'a Path,
        manifests: &'a [&str],
    ) -> impl Iterator<Item = AbsPathBuf> + 'a {
        let mut curr: Option<&Path> = Some(path);
        let ancestors = iter::from_fn(move || {
            let next = curr.and_then(|path| path.parent());
            mem::replace(&mut curr, next)
        });
        ancestors.flat_map(|path| {
            manifests
                .iter()
                .map(|file| path.join(file))
                .filter(|file| file.exists())
                .map(AbsPathBuf::assert)
        })
    }

    pub fn discover_rebar(
        path: &AbsPath,
        profile: Option<Profile>,
    ) -> Result<Option<ProjectManifest>> {
        let _timer = timeit!("discover rebar");
        let path =
            Self::find_in_dir(path.as_ref(), &["rebar.config", "rebar.config.script"]).last();
        if let Some(path) = path {
            let rebar = RebarConfig::from_config_path(path, profile.unwrap_or_default())?;
            Ok(Some(ProjectManifest::Rebar(rebar)))
        } else {
            Ok(None)
        }
    }

    fn discover_toml(path: &AbsPath) -> Result<Option<ProjectManifest>> {
        let _timer = timeit!("discover toml");
        let toml_path = Self::find_in_dir(path.as_ref(), &[".elp.toml"]).next();
        if let Some(path) = toml_path {
            let toml = buck::ElpConfig::try_parse(&path)?;
            Ok(Some(ProjectManifest::Toml(toml)))
        } else {
            Ok(None)
        }
    }

    fn discover_static(path: &AbsPath) -> Result<Option<ProjectManifest>> {
        let _timer = timeit!("discover static");
        let json_path = Self::find_in_dir(path.as_ref(), &["build_info.json"]).next();
        if let Some(path) = json_path {
            let json = json::JsonConfig::try_parse(&path)?;
            Ok(Some(ProjectManifest::Json(json)))
        } else {
            Ok(None)
        }
    }

    pub fn discover_no_manifest(path: &AbsPath) -> ProjectManifest {
        let _timer = timeit!("discover simple");
        let src_path = Self::find_in_dir(path.as_ref(), &["src"]).next();
        let root_path = if let Some(src_path) = &src_path {
            src_path.parent().map(|path| path.to_path_buf())
        } else {
            path.parent().map(|path| path.to_path_buf())
        };
        let root_path =
            root_path.unwrap_or_else(|| panic!("Error getting parent from {:?}", &path));
        let name = AppName(
            root_path
                .file_name()
                .and_then(|name| name.to_str())
                .map(|name| name.to_string())
                .unwrap_or_else(|| "generic".to_string()),
        );
        let abs_src_dirs = vec![src_path.unwrap_or_else(|| root_path.clone())];
        let no_manifest = no_manifest::NoManifestConfig::new(root_path, name, abs_src_dirs);
        ProjectManifest::NoManifest(no_manifest)
    }

    pub fn discover(path: &AbsPath) -> Result<ProjectManifest> {
        let _timer = timeit!("discover all projects");
        if cfg!(feature = "buck") {
            if let Some(t) = Self::discover_toml(path)? {
                return Ok(t);
            }
        }
        if let Some(r) = Self::discover_rebar(path, None)? {
            return Ok(r);
        }
        if let Some(s) = Self::discover_static(path)? {
            return Ok(s);
        }
        Ok(Self::discover_no_manifest(path))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProjectBuildData {
    Otp,
    Rebar(RebarProject),
    Buck(BuckProject),
    Static(StaticProject),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StaticProject {
    pub apps: Vec<ProjectAppData>,
    pub deps: Vec<ProjectAppData>,
    pub config_path: AbsPathBuf,
}

#[derive(Clone)]
pub struct Project {
    build_info_file: Option<BuildInfoFile>,
    pub otp: Otp,
    pub project_build_data: ProjectBuildData,
}

#[derive(Clone)]
pub struct BuildInfoFile(Arc<TempPath>);

impl BuildInfoFile {
    pub fn build_info_file(&self) -> AbsPathBuf {
        let BuildInfoFile(loaded) = &self;
        AbsPathBuf::assert(loaded.to_path_buf())
    }
}

impl PartialEq for Project {
    fn eq(&self, other: &Self) -> bool {
        // Explicitly ignore build_info field - if we inferred the same data after loading
        // from rebar, it's enough for equality comparison
        self.otp == other.otp && self.project_build_data == other.project_build_data
    }
}

impl Project {
    pub fn otp(otp: Otp) -> Self {
        Self {
            build_info_file: None,
            otp,
            project_build_data: ProjectBuildData::Otp,
        }
    }

    pub fn empty(otp: Otp) -> Self {
        Self {
            build_info_file: None,
            otp,
            project_build_data: ProjectBuildData::Rebar(Default::default()),
        }
    }

    pub fn all_apps(&self) -> Vec<&ProjectAppData> {
        match &self.project_build_data {
            ProjectBuildData::Otp => self.otp.apps.iter().collect(),
            ProjectBuildData::Rebar(rebar) => rebar.apps.iter().chain(rebar.deps.iter()).collect(),
            ProjectBuildData::Buck(buck) => buck.project_app_data.iter().collect(),
            ProjectBuildData::Static(stat) => stat.apps.iter().chain(stat.deps.iter()).collect(),
        }
    }

    pub fn root(&self) -> Cow<AbsPathBuf> {
        match &self.project_build_data {
            ProjectBuildData::Otp => Cow::Borrowed(&self.otp.lib_dir),
            ProjectBuildData::Rebar(rebar) => Cow::Borrowed(&rebar.root),
            ProjectBuildData::Buck(buck) => buck.buck_conf.source_root(),
            ProjectBuildData::Static(stat) => match stat.config_path.parent() {
                Some(parent) => Cow::Owned(parent.to_path_buf()),
                None => Cow::Owned(AbsPathBuf::assert(PathBuf::from("/"))),
            },
        }
    }

    pub fn name(&self) -> String {
        let root = self.root();
        root.file_name()
            .map(|name| name.to_string_lossy().to_string())
            .unwrap_or_else(move || root.as_os_str().to_string_lossy().to_string())
    }

    pub fn build_info_file(&self) -> Option<AbsPathBuf> {
        self.build_info_file
            .as_ref()
            .map(|info| info.build_info_file())
    }

    pub fn deps_ebins(&self) -> Vec<AbsPathBuf> {
        match &self.project_build_data {
            ProjectBuildData::Otp => vec![],
            ProjectBuildData::Rebar(rebar) => {
                rebar.deps.iter().flat_map(|app| app.ebin.clone()).collect()
            }
            ProjectBuildData::Buck(buck) => buck
                .target_info
                .targets
                .values()
                .flat_map(|target| &target.ebin)
                .cloned()
                .collect(),
            ProjectBuildData::Static(stat) => stat
                .deps
                .iter()
                .flat_map(|app| &app.ebin)
                .cloned()
                .collect(),
        }
    }

    pub fn eqwalizer_config(&self) -> EqwalizerConfig {
        match &self.project_build_data {
            ProjectBuildData::Buck(buck) => buck.eqwalizer_conf.clone(),
            ProjectBuildData::Otp => EqwalizerConfig::default(),
            ProjectBuildData::Rebar(_) => EqwalizerConfig::default(),
            ProjectBuildData::Static(_) => EqwalizerConfig::default(),
        }
    }
}

impl fmt::Debug for Project {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Project")
            .field("otp", &self.otp)
            .field("project_build_info", &self.project_build_data)
            .finish_non_exhaustive()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AppName(pub String);

impl AppName {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Display for AppName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Borrow<str> for AppName {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl From<&str> for AppName {
    fn from(value: &str) -> Self {
        AppName(value.to_string())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AppType {
    App,
    Dep,
    Otp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectAppData {
    pub name: AppName,
    pub dir: AbsPathBuf,
    pub ebin: Option<AbsPathBuf>,
    pub extra_src_dirs: Vec<String>,
    //usually single include dir
    pub include_dirs: Vec<AbsPathBuf>,
    pub abs_src_dirs: Vec<AbsPathBuf>,
    pub macros: Vec<eetf::Term>,
    pub parse_transforms: Vec<eetf::Term>,
    pub app_type: AppType,
    //list of directories required by module to compile
    //usually includes all dependencies include paths and otp
    pub include_path: Vec<AbsPathBuf>,
}

impl ProjectAppData {
    pub fn fixture_app_data(
        name: AppName,
        dir: AbsPathBuf,
        include_dirs: Vec<AbsPathBuf>,
        src_dirs: Vec<AbsPathBuf>,
        extra_src_dirs: Vec<String>,
    ) -> ProjectAppData {
        ProjectAppData {
            name,
            ebin: Some(dir.join("ebin")),
            extra_src_dirs,
            include_dirs,
            dir,
            macros: vec![],
            parse_transforms: vec![],
            app_type: AppType::App,
            include_path: vec![],
            abs_src_dirs: src_dirs,
        }
    }

    pub fn otp_app_data(versioned_name: &str, dir: AbsPathBuf) -> Self {
        let name = versioned_name
            .split_once('-')
            .map_or(versioned_name, |(base, _version)| base);
        let parent = dir.parent().unwrap_or(dir.as_path()).to_path_buf();
        let src = dir.join(PathBuf::from("src"));
        let include = dir.join("include");
        let abs_src_dir = dir.join("src");
        Self {
            name: AppName(name.to_string()),
            ebin: Some(dir.join("ebin")),
            extra_src_dirs: vec![],
            // This makes sure files in ./include are loaded into VFS
            include_dirs: vec![include.clone()],
            dir,
            macros: vec![],
            parse_transforms: vec![],
            app_type: AppType::Otp,
            include_path: vec![include, src, parent],
            abs_src_dirs: vec![abs_src_dir],
        }
    }

    pub fn include_dirs(&self) -> Vec<AbsPathBuf> {
        self.include_dirs
            .iter()
            .chain(self.abs_src_dirs.iter())
            .cloned()
            .collect()
    }

    /// Source directories for the application including the extra
    /// sources and includes
    pub fn all_source_dirs(&self) -> Vec<AbsPathBuf> {
        self.extra_src_dirs
            .iter()
            .map(|src_dir| self.dir.join(src_dir))
            .chain(self.abs_src_dirs.iter().cloned())
            .chain(self.include_dirs.iter().cloned())
            .collect()
    }

    /// Combine the info from the other ProjectAppData into this one
    pub fn combine(&mut self, other: ProjectAppData) {
        self.abs_src_dirs.extend(other.abs_src_dirs);
        self.abs_src_dirs.dedup();
        self.extra_src_dirs.extend(other.extra_src_dirs);
        self.extra_src_dirs.dedup();
        self.include_dirs.extend(other.include_dirs);
        self.include_dirs.dedup();
        self.macros.extend(other.macros);
        self.macros.dedup();
        self.parse_transforms.extend(other.parse_transforms);
        self.parse_transforms.dedup();
    }
}

impl Project {
    pub fn compile_deps(&self) -> Result<()> {
        let _timer = timeit!("compile deps");
        match &self.project_build_data {
            ProjectBuildData::Otp => Ok(()),
            ProjectBuildData::Rebar(rebar) => {
                let mut cmd = rebar.rebar_config.rebar3_command();
                cmd.arg("compile");
                cmd.arg("--deps_only");

                let _ = utf8_stdout(&mut cmd)?;
                Ok(())
            }
            ProjectBuildData::Buck(_) => Ok(()),
            ProjectBuildData::Static(_) => Ok(()),
        }
    }

    pub fn load(manifest: &ProjectManifest) -> Result<Project> {
        let (project_build_info, build_info, otp_root) = match manifest {
            ProjectManifest::Rebar(rebar_setting) => {
                let _timer = timeit!(
                    "load project from rebar config {}",
                    rebar_setting.config_file.display()
                );
                let rebar_version = {
                    let mut cmd = Command::new("rebar3");
                    cmd.arg("version");
                    utf8_stdout(&mut cmd)?
                };

                let loaded = Project::load_rebar_build_info(rebar_setting).with_context(|| {
                    format!(
                        "Failed to read rebar build info for config file {}, {}",
                        rebar_setting.config_file.display(),
                        rebar_version
                    )
                })?;
                let (rebar_project, otp_root) =
                    RebarProject::from_rebar_build_info(&loaded, rebar_setting.clone())
                        .with_context(|| {
                            format!(
                                "Failed to decode rebar build info for config file {:?}",
                                manifest
                            )
                        })?;
                (
                    ProjectBuildData::Rebar(rebar_project),
                    Some(BuildInfoFile(Arc::new(loaded))),
                    otp_root,
                )
            }
            ProjectManifest::Toml(config) => match &config.buck {
                Some(buck) => {
                    let (project, build_info, otp_root) =
                        BuckProject::load_from_config(buck, &config.eqwalizer)?;
                    (ProjectBuildData::Buck(project), Some(build_info), otp_root)
                }
                None => {
                    let otp_root = Otp::find_otp()?;
                    let config_path = config.config_path().to_path_buf();
                    let project = StaticProject {
                        apps: vec![],
                        deps: vec![],
                        config_path,
                    };
                    (ProjectBuildData::Static(project), None, otp_root)
                }
            },
            ProjectManifest::Json(config) => {
                let otp_root = Otp::find_otp()?;
                let config_path = config.config_path().to_path_buf();
                let (apps, deps, terms, deps_terms) =
                    json::gen_app_data(config, AbsPath::assert(&otp_root));
                let build_info_term =
                    buck::make_build_info(terms, deps_terms, &otp_root, &config_path);
                let build_info = buck::save_build_info(build_info_term)?;
                let project = StaticProject {
                    apps,
                    deps,
                    config_path,
                };
                (
                    ProjectBuildData::Static(project),
                    Some(build_info),
                    otp_root,
                )
            }
            ProjectManifest::NoManifest(config) => {
                let otp_root = Otp::find_otp()?;
                let abs_otp_root = AbsPath::assert(&otp_root);
                let config_path = config.config_path().to_path_buf();
                let (apps, terms) = config.to_project_app_data(abs_otp_root);
                let (eqwalizer_support_app, eqwalizer_support_term) =
                    eqwalizer_support::eqwalizer_suppport_data(abs_otp_root);
                let build_info_term = buck::make_build_info(
                    terms,
                    vec![eqwalizer_support_term],
                    abs_otp_root,
                    &config_path,
                );
                let build_info = buck::save_build_info(build_info_term)?;
                let project = StaticProject {
                    apps,
                    deps: vec![eqwalizer_support_app],
                    config_path,
                };
                (
                    ProjectBuildData::Static(project),
                    Some(build_info),
                    otp_root,
                )
            }
        };

        Ok(Project {
            build_info_file: build_info,
            otp: Otp::discover(otp_root),
            project_build_data: project_build_info,
        })
    }

    fn load_rebar_build_info(build: &RebarConfig) -> Result<TempPath> {
        let out_file = NamedTempFile::new()?.into_temp_path();
        let mut cmd = build.rebar3_command();
        cmd.arg("build_info");
        cmd.arg("--to");
        cmd.arg(&out_file);

        let _ = utf8_stdout(&mut cmd)?;

        Ok(out_file)
    }
}

pub fn utf8_stdout(cmd: &mut Command) -> Result<String> {
    let output = cmd.output().with_context(|| format!("{:?} failed", cmd))?;
    let stdout = String::from_utf8(output.stdout)?;
    if !output.status.success() {
        match String::from_utf8(output.stderr) {
            Ok(stderr) if !stderr.is_empty() => {
                bail!(
                    "{:?} failed, {}\nstdout:\n{}\nstderr:\n{}",
                    cmd,
                    output.status,
                    stdout,
                    stderr
                )
            }
            _ => bail!("{:?} failed, {}\nstdout:{}", cmd, output.status, stdout),
        }
    }
    Ok(stdout.trim().to_string())
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;
    use crate::json::JsonProjectAppData;
    use crate::no_manifest::NoManifestConfig;
    use crate::test_fixture::Fixture;

    #[test]
    fn test_discover_rebar() {
        let spec = r#"
        //- /rebar.config
            {checkouts_dir, ["."]}.
            {project_app_dirs, [
                "app_a",
                "app_b"
            ]}.
            {erl_opts, [debug_info]}.
            {deps, []}.
            {alias, [
                {build_info, [help]}
            ]}.
        //- /app_a/src/app.erl
        -module(app).
        //- /app_b/src/app.erl
        -module(app).
        //- /app_b/include/app.hrl
        %% comment
        "#;
        let dir = Fixture::gen_project(spec);
        let manifest =
            ProjectManifest::discover(&AbsPathBuf::assert(dir.path().join("app_a/src/app.erl")));
        if let Ok(ProjectManifest::Rebar(config)) = manifest {
            let expected_config = RebarConfig {
                config_file: AbsPathBuf::assert(dir.path().join("rebar.config")),
                profile: Profile("test".to_string()),
            };
            assert_eq!(expected_config, config)
        } else {
            panic!("Expected Ok(Some(RebarManifest)), got {:?}", manifest)
        }
    }

    #[test]
    fn test_json() {
        let spec = r#"
        //- /build_info.json
        {
            "apps": [
                {
                    "name": "app_a",
                    "dir": "app_a",
                    "src_dirs": ["src"]
                },
                {
                    "name": "app_b",
                    "dir": "app_b",
                    "src_dirs": ["src"],
                    "extra_src_dirs": ["test"],
                    "include_dirs": ["include"]
                }
            ]
        }
        //- /app_a/src/app.erl
        -module(app).
        //- /app_b/src/app.erl
        -module(app).
        //- /app_b/include/app.hrl
        %% comment
        //- /app_b/test/suite.erl
        %% test
        "#;
        let dir = Fixture::gen_project(spec);
        let dir_path = AbsPathBuf::assert(fs::canonicalize(dir.path()).unwrap());
        let manifest = ProjectManifest::discover(&dir_path.join("app_b/src/app.erl"));
        if let Ok(ProjectManifest::Json(config)) = manifest {
            let json_app_a = JsonProjectAppData {
                name: "app_a".into(),
                dir: "app_a".to_string(),
                src_dirs: vec!["src".to_string()],
                ebin: None,
                extra_src_dirs: vec![],
                include_dirs: vec![],
                macros: vec![],
            };
            let json_app_b = JsonProjectAppData {
                name: "app_b".into(),
                dir: "app_b".to_string(),
                src_dirs: vec!["src".to_string()],
                ebin: None,
                extra_src_dirs: vec!["test".to_string()],
                include_dirs: vec!["include".to_string()],
                macros: vec![],
            };
            let build_info_path = dir_path.join("build_info.json");
            let expected_config = JsonConfig::new(
                vec![json_app_a, json_app_b],
                vec![],
                build_info_path.clone(),
            );
            //discover only app_b since path was from app_b
            assert_eq!(expected_config, config);
            let project =
                Project::load(&ProjectManifest::Json(config)).expect("project must be ok");
            assert!(project.build_info_file.is_some());
            let otp_root = AbsPathBuf::assert(Otp::find_otp().unwrap());
            let (eqwalizer_support, _) = eqwalizer_support::eqwalizer_suppport_data(&otp_root);
            if let ProjectBuildData::Static(build_data) = &project.project_build_data {
                let app_a_path = dir_path.join("app_a");
                let app_a_src = app_a_path.join("src");
                let app_data_a = ProjectAppData {
                    name: "app_a".into(),
                    dir: app_a_path,
                    ebin: None,
                    extra_src_dirs: vec![],
                    include_dirs: vec![],
                    abs_src_dirs: vec![app_a_src.clone()],
                    macros: vec![],
                    parse_transforms: vec![],
                    app_type: AppType::App,
                    include_path: vec![project.otp.lib_dir.clone(), dir_path.clone(), app_a_src],
                };
                let app_b_path = dir_path.join("app_b");
                let app_b_src = app_b_path.join("src");
                let app_b_include = app_b_path.join("include");
                let app_data_b = ProjectAppData {
                    name: "app_b".into(),
                    dir: app_b_path,
                    ebin: None,
                    extra_src_dirs: vec!["test".to_string()],
                    include_dirs: vec![app_b_include.clone()],
                    abs_src_dirs: vec![app_b_src.clone()],
                    macros: vec![],
                    parse_transforms: vec![],
                    app_type: AppType::App,
                    include_path: vec![
                        project.otp.lib_dir.clone(),
                        dir_path,
                        app_b_include,
                        app_b_src,
                    ],
                };
                let expected_build_data = StaticProject {
                    apps: vec![app_data_a, app_data_b],
                    deps: vec![eqwalizer_support],
                    config_path: build_info_path,
                };
                assert_eq!(&expected_build_data, build_data)
            } else {
                panic!(
                    "expected staticproject got {:?}",
                    project.project_build_data
                )
            }
        } else {
            panic!("Expected Ok(Some(NoManifest)), got {:?}", manifest)
        }
    }

    #[test]
    fn test_err_on_invalid_json() {
        let spec = r#"
        //- /build_info.json
        {
            invalid_apps": [
                {
                    "name": "app_a",
                    "dir": "app_a",
                    "src_dirs": ["src"]
                },
            ]
        }
        //- /app_a/src/app.erl
        -module(app).
        "#;
        let dir = Fixture::gen_project(spec);
        let dir_path = AbsPathBuf::assert(fs::canonicalize(dir.path()).unwrap());
        let manifest = ProjectManifest::discover(&dir_path.join("app_b/src/app.erl"));
        match manifest
            .expect_err("Must be err")
            .downcast_ref::<ProjectModelError>()
        {
            Some(ProjectModelError::InvalidBuildInfoJson(_)) => (),
            Some(err) => panic!("Wrong err {:?}", err),
            None => panic!("Must be error"),
        };
    }

    #[test]
    fn test_err_on_no_buck_root() {
        if cfg!(feature = "buck") {
            let spec = r#"
            //- /.elp.toml
            [buck]
            enabled = true
            build_deps = false
            included_targets = ["//..."]
            //- /app_a/src/app.erl
            -module(app).
            "#;
            let dir = Fixture::gen_project(spec);
            let manifest = ProjectManifest::discover(&AbsPathBuf::assert(
                dir.path().join("app_a/src/app.erl"),
            ));
            match manifest
                .expect_err("Must be err")
                .downcast_ref::<ProjectModelError>()
            {
                Some(ProjectModelError::NotInBuckProject) => (),
                Some(err) => panic!("Wrong err {:?}", err),
                None => panic!("Wrong error"),
            };
        }
    }

    #[test]
    fn test_err_on_no_rebar_build_info() {
        let spec = r#"
        //- /rebar.config
            {checkouts_dir, ["."]}.
            {project_app_dirs, [
                "app_a"
            ]}.
            {erl_opts, [debug_info]}.
            {deps, []}.
        //- /app_a/src/app.erl
        -module(app).
        "#;
        let dir = Fixture::gen_project(spec);
        let manifest =
            ProjectManifest::discover(&AbsPathBuf::assert(dir.path().join("app_a/src/app.erl")));
        match manifest
            .expect_err("Must be err")
            .downcast_ref::<ProjectModelError>()
        {
            Some(ProjectModelError::NoBuildInfo) => (),
            Some(err) => panic!("Wrong err {:?}", err),
            None => panic!("Wrong error"),
        };
    }
    #[test]
    fn test_no_manifesto() {
        let spec = r#"
        //- /app_a/src/app.erl
        -module(app).
        //- /app_b/src/app.erl
        -module(app).
        //- /app_b/include/app.hrl
        %% comment
        //- /app_b/test/suite.erl
        %% test
        "#;
        let dir = Fixture::gen_project(spec);
        let manifest =
            ProjectManifest::discover(&AbsPathBuf::assert(dir.path().join("app_b/src/app.erl")));
        if let Ok(ProjectManifest::NoManifest(config)) = manifest {
            let path = AbsPathBuf::assert(dir.path().join("app_b"));
            let name: AppName = "app_b".into();
            let abs_src = AbsPathBuf::assert(dir.path().join("app_b/src"));
            let includes = AbsPathBuf::assert(dir.path().join("app_b/include"));
            let expected_config =
                NoManifestConfig::new(path.clone(), name.clone(), vec![abs_src.clone()]);
            //discover only app_b since path was from app_b
            assert_eq!(&expected_config, &config);

            let project =
                Project::load(&ProjectManifest::NoManifest(config)).expect("project must be ok");
            assert!(project.build_info_file.is_some());
            let otp_root = AbsPathBuf::assert(Otp::find_otp().unwrap());
            let (eqwalizer_support, _) = eqwalizer_support::eqwalizer_suppport_data(&otp_root);
            if let ProjectBuildData::Static(build_data) = &project.project_build_data {
                let app_data = ProjectAppData {
                    name,
                    dir: path.clone(),
                    ebin: None,
                    extra_src_dirs: vec!["test".to_string()],
                    include_dirs: vec![includes.clone()],
                    abs_src_dirs: vec![abs_src.clone()],
                    macros: vec![],
                    parse_transforms: vec![],
                    app_type: AppType::App,
                    include_path: vec![
                        project.otp.lib_dir.clone(),
                        includes,
                        abs_src,
                        AbsPathBuf::assert(dir.path().to_path_buf()),
                    ],
                };
                let expected_build_data = StaticProject {
                    apps: vec![app_data],
                    deps: vec![eqwalizer_support],
                    config_path: path.join(".static"),
                };
                assert_eq!(&expected_build_data, build_data)
            } else {
                panic!(
                    "expected staticproject got {:?}",
                    project.project_build_data
                )
            }
        } else {
            panic!("Expected Ok(Some(NoManifest)), got {:?}", manifest)
        }
    }

    #[test]
    fn test_toml_empty() {
        if cfg!(feature = "buck") {
            let spec = r#"
        //- /.elp.toml
        //- /app_a/src/app.erl
        -module(app).
        "#;
            let dir = Fixture::gen_project(spec);
            let manifest = ProjectManifest::discover(&AbsPathBuf::assert(
                dir.path().join("app_a/src/app.erl"),
            ));
            if let Ok(ProjectManifest::Toml(toml)) = manifest {
                let expected_config = buck::ElpConfig::new(
                    AbsPathBuf::assert(dir.path().join(".elp.toml")),
                    None,
                    EqwalizerConfig::default(),
                );
                assert_eq!(expected_config, toml)
            } else {
                panic!("Expected Ok(Some(Toml)), got {:?}", manifest)
            }
        }
    }

    #[test]
    fn test_discover() {
        let root = AbsPathBuf::assert(Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures"));
        let manifest = ProjectManifest::discover_rebar(&root.join("nested"), None);
        match manifest {
            Ok(Some(ProjectManifest::Rebar(RebarConfig {
                config_file: actual,
                profile: _,
            }))) => {
                let expected = root.join("rebar.config.script");
                assert_eq!(actual, expected);
            }
            _ => {
                unimplemented!()
            }
        }
    }
}

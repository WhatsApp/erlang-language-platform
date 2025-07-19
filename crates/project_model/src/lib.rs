/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Borrow;
use std::borrow::Cow;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fs;
use std::iter;
use std::mem;
use std::ops::Deref;
use std::ops::DerefMut;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;
use std::vec;

use anyhow::Context;
use anyhow::Result;
use anyhow::bail;
use buck::BuckConfig;
use buck::BuckQueryConfig;
use buck::IncludeMapping;
use buck::TargetFullName;
use elp_log::timeit;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use glob::glob;
use itertools::Either;
use itertools::Itertools;
use json::JsonProjectAppData;
use parking_lot::MutexGuard;
use paths::AbsPath;
use paths::AbsPathBuf;
use paths::RelPath;
use paths::Utf8Path;
use paths::Utf8PathBuf;
use serde::Deserialize;
use serde::Serialize;
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
pub mod temp_dir;
pub mod test_fixture;

pub const ELP_CONFIG_FILE: &str = ".elp.toml";
pub const BUILD_INFO_FILE: &str = "build_info.json";

pub struct CommandProxy<'a> {
    _guard: MutexGuard<'a, ()>,
    command: Command,
}

impl<'a> CommandProxy<'a> {
    pub fn new(guard: MutexGuard<'a, ()>, command: Command) -> Self {
        CommandProxy {
            _guard: guard,
            command,
        }
    }
}

impl Deref for CommandProxy<'_> {
    type Target = Command;

    fn deref(&self) -> &Self::Target {
        &self.command
    }
}

impl DerefMut for CommandProxy<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.command
    }
}

impl fmt::Display for CommandProxy<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let program = self.command.get_program().to_string_lossy();
        let args = self
            .command
            .get_args()
            .map(|a| a.to_string_lossy())
            .join(" ");
        write!(f, "{program} {args}")
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
    #[error("Could not extract version information from rebar3. Error: {error:?}")]
    RebarVersionError { error: String },
    #[error("rebar3 version too old (expected {expected:?}, actual {actual:?})")]
    RebarVersionTooOld { expected: String, actual: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProjectManifest {
    Rebar(RebarConfig),
    TomlBuck(BuckConfig),
    Json(JsonConfig),
    NoManifest(no_manifest::NoManifestConfig),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IncludeParentDirs {
    Yes,
    No,
}

impl ProjectManifest {
    pub fn root(&self) -> &AbsPath {
        match self {
            ProjectManifest::Rebar(conf) => conf.config_path(),
            ProjectManifest::TomlBuck(conf) => conf.config_path(),
            ProjectManifest::Json(conf) => conf.config_path(),
            ProjectManifest::NoManifest(conf) => conf.config_path(),
        }
    }

    fn find_in_dir<'a>(
        path: &'a Path,
        manifests: &'a [&str],
        include_parents: IncludeParentDirs,
    ) -> impl Iterator<Item = AbsPathBuf> + 'a {
        let ancestors = if include_parents == IncludeParentDirs::Yes {
            let mut curr: Option<&Path> = Some(path);
            Either::Left(iter::from_fn(move || {
                let next = curr.and_then(|path| path.parent());
                mem::replace(&mut curr, next)
            }))
        } else {
            Either::Right(iter::once(path))
        };
        ancestors.flat_map(|path| {
            manifests
                .iter()
                .map(|file| path.join(file))
                .filter(|file| file.exists())
                .map(AbsPathBuf::assert_utf8)
        })
    }

    pub fn discover_rebar(
        path: &AbsPath,
        profile: Option<Profile>,
        include_parents: IncludeParentDirs,
    ) -> Result<Option<ProjectManifest>> {
        let _timer = timeit!("discover rebar");
        let path = Self::find_in_dir(
            path.as_ref(),
            &["rebar.config", "rebar.config.script"],
            include_parents,
        )
        .last();
        if let Some(path) = path {
            let rebar = RebarConfig::from_config_path(path, profile.unwrap_or_default())?;
            Ok(Some(ProjectManifest::Rebar(rebar)))
        } else {
            Ok(None)
        }
    }

    fn discover_toml(path: &AbsPath) -> Result<Option<ElpConfig>> {
        let _timer = timeit!("discover toml");
        let toml_path =
            Self::find_in_dir(path.as_ref(), &[ELP_CONFIG_FILE], IncludeParentDirs::Yes).next();
        if let Some(path) = toml_path {
            let toml = ElpConfig::try_parse(&path)?;
            Ok(Some(toml))
        } else {
            Ok(None)
        }
    }

    fn discover_static(
        path: &AbsPath,
        include_parents: IncludeParentDirs,
    ) -> Result<Option<ProjectManifest>> {
        let _timer = timeit!("discover static");
        let json_path =
            Self::find_in_dir(path.as_ref(), &[BUILD_INFO_FILE], include_parents).next();
        if let Some(path) = json_path {
            let json = json::JsonConfig::try_parse(&path)?;
            Ok(Some(ProjectManifest::Json(json)))
        } else {
            Ok(None)
        }
    }

    pub fn discover_no_manifest(
        path: &AbsPath,
        include_parents: IncludeParentDirs,
    ) -> ProjectManifest {
        let _timer = timeit!("discover simple");
        let src_path = Self::find_in_dir(path.as_ref(), &["src"], include_parents).next();
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
                .map(|name| name.to_string())
                .unwrap_or_else(|| "generic".to_string()),
        );
        let abs_src_dirs = vec![src_path.unwrap_or_else(|| root_path.clone())];
        let no_manifest = no_manifest::NoManifestConfig::new(root_path, name, abs_src_dirs);
        ProjectManifest::NoManifest(no_manifest)
    }

    /// Given the path of a file in a project, discover its
    /// configuration.
    pub fn discover(path: &AbsPath) -> Result<(ElpConfig, ProjectManifest)> {
        let _timer = timeit!("discover all projects");
        // First check for a json config file as the path.
        if let Some("json") = path.extension() {
            let json = json::JsonConfig::try_parse(path)?;
            return Ok((ElpConfig::default(), ProjectManifest::Json(json)));
        }

        if let Some(elp_config) = Self::discover_toml(path)? {
            if elp_config.buck_enabled() {
                let buck = elp_config.clone().buck.unwrap(); // Safe from prior line
                return Ok((elp_config.clone(), ProjectManifest::TomlBuck(buck)));
            } else {
                // Not a buck project, check if explicit build info given
                if let Some(absolute_path) = elp_config.build_info_path() {
                    match json::JsonConfig::try_parse(&absolute_path) {
                        Ok(json) => return Ok((elp_config.clone(), ProjectManifest::Json(json))),
                        Err(err) => {
                            bail!(
                                "Reading build_info file {}, found in {}: {err}",
                                absolute_path.as_path().as_os_str().to_string_lossy(),
                                elp_config.config_path().as_os_str().to_string_lossy()
                            );
                        }
                    }
                } else if elp_config.generate_build_info() {
                    let apps = elp_config.json_project_app_data(elp_config.build_info_apps());
                    let deps = elp_config.json_project_app_data(elp_config.build_info_deps());
                    let config_path = elp_config.clone().config_path.expect("Missing config path");
                    let json_config = json::JsonConfig::new(apps, deps, config_path);
                    return Ok((elp_config.clone(), ProjectManifest::Json(json_config)));
                } else {
                    let manifest = ProjectManifest::discover_in_place(
                        elp_config.config_path(),
                        elp_config.rebar_profile(),
                    )?;
                    return Ok((elp_config.clone(), manifest));
                }
            }
        }
        if let Some(r) = Self::discover_rebar(path, None, IncludeParentDirs::Yes)? {
            return Ok((ElpConfig::default(), r));
        }
        if let Some(s) = Self::discover_static(path, IncludeParentDirs::Yes)? {
            return Ok((ElpConfig::default(), s));
        }
        Ok((
            ElpConfig::default(),
            Self::discover_no_manifest(path, IncludeParentDirs::Yes),
        ))
    }

    /// Given the path of the `ELP_CONFIG_FILE` file, discover its configuration.
    pub fn discover_in_place(path: &AbsPath, rebar_profile: Profile) -> Result<ProjectManifest> {
        let _timer = timeit!("discover projects in place");
        // We skip looking for the TOML file since we have already found it.
        if let Some(r) = Self::discover_rebar(path, Some(rebar_profile), IncludeParentDirs::No)? {
            return Ok(r);
        }
        if let Some(s) = Self::discover_static(path, IncludeParentDirs::No)? {
            return Ok(s);
        }
        Ok(Self::discover_no_manifest(path, IncludeParentDirs::No))
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
    pub config_path: AbsPathBuf,
}

// Sample config:
// ```
// [buck]
// enabled = true
// deps_target = "waserver//third-party/..."
// build_deps = true
// included_targets = [ "waserver//erl/..." ]
// source_root = "erl"
//
// [eqwalizer]
// enable_all = true
//```
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize, Default)]
pub struct ElpConfig {
    #[serde(skip_deserializing)]
    #[serde(skip_serializing)]
    config_path: Option<AbsPathBuf>,
    pub build_info: Option<BuildInfoConfig>,
    pub buck: Option<BuckConfig>,
    #[serde(default)]
    pub eqwalizer: EqwalizerConfig,
    #[serde(default)]
    pub rebar: ElpRebarConfig,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    Ord,
    PartialOrd,
    Deserialize,
    Serialize
)]
pub struct EqwalizerConfig {
    #[serde(default = "eqwalizer_enable_all_default")]
    pub enable_all: bool,
    #[serde(default = "eqwalizer_max_tasks_default")]
    pub max_tasks: usize,
}

fn eqwalizer_enable_all_default() -> bool {
    true
}

fn eqwalizer_max_tasks_default() -> usize {
    4
}

impl Default for EqwalizerConfig {
    fn default() -> Self {
        Self {
            enable_all: eqwalizer_enable_all_default(),
            max_tasks: eqwalizer_max_tasks_default(),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct BuildInfoConfig {
    pub file: Option<PathBuf>,
    pub apps: Option<SingleOrMulti>,
    pub deps: Option<SingleOrMulti>,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(untagged)]
pub enum StringOrApp {
    String(String),
    App(JsonProjectAppData),
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(untagged)]
pub enum SingleOrMulti {
    Single(StringOrApp),
    Multi(Vec<StringOrApp>),
}

impl From<SingleOrMulti> for Vec<StringOrApp> {
    fn from(val: SingleOrMulti) -> Self {
        match val {
            SingleOrMulti::Single(single) => vec![single],
            SingleOrMulti::Multi(multi) => multi,
        }
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    Ord,
    PartialOrd,
    Deserialize,
    Serialize
)]
pub struct ElpRebarConfig {
    #[serde(default = "rebar_profile_default")]
    pub profile: String,
}

fn rebar_profile_default() -> String {
    "test".to_string()
}

impl Default for ElpRebarConfig {
    fn default() -> Self {
        Self {
            profile: rebar_profile_default(),
        }
    }
}

impl ElpConfig {
    pub fn new(
        config_path: AbsPathBuf,
        buck: Option<BuckConfig>,
        build_info: Option<BuildInfoConfig>,
        eqwalizer: EqwalizerConfig,
        rebar: ElpRebarConfig,
    ) -> Self {
        Self {
            config_path: Some(config_path),
            buck,
            build_info,
            eqwalizer,
            rebar,
        }
    }
    pub fn try_parse(path: &AbsPath) -> Result<ElpConfig> {
        let p = Utf8Path::new(ELP_CONFIG_FILE);
        let path = if !path.ends_with(RelPath::new_unchecked(p)) {
            path.join(p)
        } else {
            path.to_path_buf()
        };
        let config_content = fs::read_to_string(&path)?;
        match toml::from_str(config_content.as_str()) {
            Ok(mut config) => {
                BuckConfig::make_config(&path, &mut config)?;
                config.config_path = Some(path);

                Ok(config)
            }
            Err(err) => bail!(
                "unable to read {}: {err}",
                path.as_path().as_os_str().to_string_lossy()
            ),
        }
    }

    pub fn buck_enabled(&self) -> bool {
        if let Some(buck) = &self.buck {
            buck.enabled
        } else {
            false
        }
    }

    pub fn generate_build_info(&self) -> bool {
        if let Some(build_info) = &self.build_info {
            build_info.file.is_none()
        } else {
            false
        }
    }

    pub fn build_info_apps(&self) -> Option<Vec<StringOrApp>> {
        self.build_info.clone()?.apps.map(|apps| apps.into())
    }

    pub fn build_info_deps(&self) -> Option<Vec<StringOrApp>> {
        self.build_info.clone()?.deps.map(|deps| deps.into())
    }

    pub fn config_path(&self) -> &AbsPath {
        self.config_path.as_ref().unwrap()
    }

    pub fn build_info_path(&self) -> Option<AbsPathBuf> {
        let build_info = self.build_info.clone()?;
        let build_info_file = build_info.file?;
        let build_info_file = Utf8PathBuf::from_path_buf(build_info_file.to_path_buf())
            .expect("UTF8 conversion failed");
        let absolute_path = if build_info_file.is_absolute() {
            AbsPathBuf::assert(build_info_file)
        } else {
            self.config_path()
                .parent()
                .unwrap()
                .to_path_buf()
                .join(build_info_file)
        };
        Some(absolute_path)
    }

    pub fn json_project_app_data(
        &self,
        patterns: Option<Vec<StringOrApp>>,
    ) -> Vec<JsonProjectAppData> {
        let mut res = FxHashMap::default();

        if let Some(patterns) = patterns {
            for pattern in patterns {
                match pattern {
                    StringOrApp::String(pattern) => {
                        let pattern = self
                            .config_path()
                            .parent()
                            .unwrap()
                            .to_path_buf()
                            .join(pattern);
                        for entry in glob(&pattern.as_os_str().to_string_lossy())
                            .expect("Failed to read glob pattern")
                        {
                            match entry {
                                Ok(path) => {
                                    if let Some(app_data) = app_data_from_path(&path) {
                                        res.insert(app_data.clone().name, app_data);
                                    }
                                }
                                Err(e) => {
                                    log::warn!("Glob Error: {e:?}")
                                }
                            }
                        }
                    }
                    StringOrApp::App(mut app) => {
                        let app_dir = app.dir.to_string();
                        app.dir = self
                            .config_path()
                            .parent()
                            .unwrap()
                            .to_path_buf()
                            .join(app_dir)
                            .to_string();
                        res.insert(app.name.clone(), app);
                    }
                }
            }
        }

        res.values().cloned().collect()
    }

    pub fn rebar_profile(&self) -> Profile {
        Profile(self.rebar.profile.clone())
    }
}

fn app_data_from_path(path: &Path) -> Option<JsonProjectAppData> {
    let name = path.file_name()?.to_string_lossy().to_string();
    let dir = path.to_string_lossy().to_string();
    let src_dirs = vec!["src".to_string()];
    // See <https://github.com/erlang/rebar3/blob/9ca36c6a6dbe674d405df959051b46d1bb71e367/apps/rebar/src/rebar_compiler_erl.erl#L28-L37>:
    let mut include_dirs = vec!["include".to_string()];
    include_dirs.extend(src_dirs.iter().cloned());
    include_dirs.push(".".to_string());
    let app_data = JsonProjectAppData {
        name,
        dir,
        src_dirs,
        ebin: None,
        extra_src_dirs: vec!["test".to_string()],
        include_dirs,
        macros: FxHashMap::default(),
    };
    Some(app_data)
}

/// This is the key data structure related to project discovery and
/// loading.
///
/// It can be populated using any mechanism at all, and currently has
/// support for loading from a rebar3 config (using the
/// eqwalizer_rebar3 plugin to give build_info), buck2, a JSON config
/// file, or just deducing it from the directory structure.  We also
/// generate them from declarative test configurations using
/// `WithFixture`.
///
/// Any novel project discovery/representation schemes should aim to
/// produce one of these, and it will be able to work with ELP.
///
/// Once we have a `Vec<Project>`, we use it in the test runners, LSP
/// server, and CLI invocations to set up ELP for use.
#[derive(Clone)]
pub struct Project {
    pub otp: Otp,
    pub project_build_data: ProjectBuildData,
    pub project_apps: Vec<ProjectAppData>,
    pub eqwalizer_config: EqwalizerConfig,
    pub include_mapping: Arc<IncludeMapping>,
}

#[derive(Clone, Debug)]
pub enum BuildInfoFile {
    TempPath(Arc<TempPath>),
    Path(AbsPathBuf),
}

impl BuildInfoFile {
    pub fn build_info_file(&self) -> AbsPathBuf {
        match &self {
            BuildInfoFile::TempPath(loaded) => AbsPathBuf::assert(
                Utf8PathBuf::from_path_buf(loaded.to_path_buf()).expect("could not decode UTF8"),
            ),
            BuildInfoFile::Path(path) => path.clone(),
        }
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
    pub fn otp(otp: Otp, project_apps: Vec<ProjectAppData>) -> Self {
        Self {
            otp,
            project_build_data: ProjectBuildData::Otp,
            project_apps,
            eqwalizer_config: EqwalizerConfig::default(),
            include_mapping: Arc::new(IncludeMapping::default()),
        }
    }

    pub fn empty(otp: Otp) -> Self {
        Self {
            otp,
            project_build_data: ProjectBuildData::Rebar(Default::default()),
            project_apps: Vec::default(),
            eqwalizer_config: EqwalizerConfig::default(),
            include_mapping: Arc::new(IncludeMapping::default()),
        }
    }

    pub fn add_apps(&mut self, apps: Vec<ProjectAppData>) {
        self.project_apps.extend(apps);
    }

    pub fn all_apps(&self) -> impl Iterator<Item = &ProjectAppData> + '_ {
        match &self.project_build_data {
            ProjectBuildData::Otp => Either::Left(self.otp_apps()),
            _ => Either::Right(self.non_otp_apps()),
        }
    }

    pub fn non_otp_apps(&self) -> impl Iterator<Item = &ProjectAppData> + '_ {
        self.project_apps
            .iter()
            .filter(|app| app.app_type != AppType::Otp)
    }

    fn deps(&self) -> impl Iterator<Item = &ProjectAppData> + '_ {
        self.project_apps
            .iter()
            .filter(|app| app.app_type == AppType::Dep)
    }

    pub fn otp_apps(&self) -> impl Iterator<Item = &ProjectAppData> + '_ {
        self.project_apps
            .iter()
            .filter(|app| app.app_type == AppType::Otp)
    }

    pub fn root(&self) -> Cow<AbsPathBuf> {
        match &self.project_build_data {
            ProjectBuildData::Otp => Cow::Borrowed(&self.otp.lib_dir),
            ProjectBuildData::Rebar(rebar) => Cow::Borrowed(&rebar.root),
            ProjectBuildData::Buck(buck) => buck.buck_conf.source_root(),
            ProjectBuildData::Static(stat) => match stat.config_path.parent() {
                Some(parent) => Cow::Owned(parent.to_path_buf()),
                None => Cow::Owned(AbsPathBuf::assert(Utf8PathBuf::from("/"))),
            },
        }
    }

    pub fn name(&self) -> String {
        let root = self.root();
        root.file_name()
            .map(|name| name.to_string())
            .unwrap_or_else(move || root.as_os_str().to_string_lossy().to_string())
    }

    pub fn deps_ebins(&self) -> Vec<AbsPathBuf> {
        self.deps().flat_map(|app| app.ebin.clone()).collect()
    }

    pub fn as_json(&self, root: AbsPathBuf) -> JsonConfig {
        let project_app_data = self.non_otp_apps().cloned().collect::<Vec<_>>();
        let root_without_file = if <AbsPath as AsRef<Utf8Path>>::as_ref(&root).is_file() {
            root.parent().unwrap_or(&root).to_path_buf()
        } else {
            root
        };
        let json_app_data: Vec<_> = project_app_data
            .iter()
            .filter_map(|project_app_data| {
                if project_app_data.name == AppName("eqwalizer_support".to_string()) {
                    // This is derived from OTP when the project is loaded again
                    None
                } else {
                    Some(JsonProjectAppData::from_project_app_data(
                        &root_without_file,
                        project_app_data,
                    ))
                }
            })
            .collect();
        JsonConfig {
            apps: json_app_data,
            deps: vec![],
            config_path: None,
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
    /// Target name if this application originates from a buck target
    pub buck_target_name: Option<TargetFullName>,
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
    // Originating from buck2 model, the set of specific files this
    // data applies to.
    pub applicable_files: Option<ApplicableFiles>,
    pub is_test_target: Option<bool>,
}

pub type ApplicableFiles = FxHashSet<AbsPathBuf>;

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
            buck_target_name: None,
            ebin: None,
            extra_src_dirs,
            include_dirs,
            dir,
            macros: vec![],
            parse_transforms: vec![],
            app_type: AppType::App,
            include_path: vec![],
            abs_src_dirs: src_dirs,
            applicable_files: None,
            is_test_target: None,
        }
    }

    pub fn otp_app_data(versioned_name: &str, dir: &AbsPathBuf) -> Self {
        let name = versioned_name
            .split_once('-')
            .map_or(versioned_name, |(base, _version)| base);
        let parent = dir.parent().unwrap_or(dir.as_path()).to_path_buf();
        let src = dir.join(Utf8PathBuf::from("src"));
        let include = dir.join("include");
        let abs_src_dir = dir.join("src");
        Self {
            name: AppName(name.to_string()),
            buck_target_name: None,
            ebin: Some(dir.join("ebin")),
            extra_src_dirs: vec![],
            // This makes sure files in ./include are loaded into VFS
            include_dirs: vec![include.clone()],
            dir: dir.clone(),
            macros: vec![],
            parse_transforms: vec![],
            app_type: AppType::Otp,
            include_path: vec![include, src, parent],
            abs_src_dirs: vec![abs_src_dir],
            applicable_files: None,
            is_test_target: None,
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
    pub fn all_source_and_include_dirs(&self) -> FxHashSet<AbsPathBuf> {
        self.extra_src_dirs
            .iter()
            .map(|src_dir| self.dir.join(src_dir))
            .chain(self.abs_src_dirs.iter().cloned())
            .chain(self.include_dirs.iter().cloned())
            .chain(self.include_path.iter().cloned())
            .collect()
    }
    /// Source directories for the application
    pub fn all_source_dirs(&self) -> FxHashSet<AbsPathBuf> {
        self.extra_src_dirs
            .iter()
            .map(|src_dir| self.dir.join(src_dir))
            .chain(self.abs_src_dirs.iter().cloned())
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

    pub fn load(
        manifest: &ProjectManifest,
        eqwalizer_config: EqwalizerConfig,
        query_config: &BuckQueryConfig,
        report_progress: &impl Fn(&str),
    ) -> Result<Project> {
        let (project_build_info, mut project_apps, otp_root, include_mapping) = match manifest {
            ProjectManifest::Rebar(rebar_setting) => {
                let _timer = timeit!(
                    "load project from rebar config {}",
                    rebar_setting.config_file
                );
                let rebar_version = {

                    let mut cmd = if cfg!(target_os = "windows") {
                        let mut cmd = Command::new("cmd");
                        cmd.args(["/C", "rebar3"]);
                        cmd
                    } else {
                        Command::new("rebar3")
                    };
                    
                    cmd.arg("version");
                    utf8_stdout(&mut cmd)?
                };

                let loaded = Project::load_rebar_build_info(rebar_setting).with_context(|| {
                    format!(
                        "Failed to read rebar build info for config file {}, {}",
                        rebar_setting.config_file, rebar_version
                    )
                })?;
                let (rebar_project, otp_root, apps) =
                    RebarProject::from_rebar_build_info(&loaded, rebar_setting.clone())
                        .with_context(|| {
                            format!(
                                "Failed to decode rebar build info for config file {manifest:?}"
                            )
                        })?;
                (
                    ProjectBuildData::Rebar(rebar_project),
                    apps,
                    otp_root,
                    Arc::new(IncludeMapping::default()),
                )
            }
            ProjectManifest::TomlBuck(buck) => {
                // We only select this manifest if buck is actually enabled
                let (project, apps, otp_root, include_mapping) =
                    BuckProject::load_from_config(buck, query_config, report_progress)?;
                (
                    ProjectBuildData::Buck(project),
                    apps,
                    otp_root,
                    include_mapping,
                )
            }
            ProjectManifest::Json(config) => {
                let otp_root = Otp::find_otp()?;
                let config_path = config.config_path().to_path_buf();
                let (mut apps, deps) = json::gen_app_data(config, AbsPath::assert(&otp_root));
                let project = StaticProject { config_path };
                apps.extend(deps);
                (
                    ProjectBuildData::Static(project),
                    apps,
                    otp_root,
                    Arc::new(IncludeMapping::default()),
                )
            }
            ProjectManifest::NoManifest(config) => {
                let otp_root = Otp::find_otp()?;
                let abs_otp_root = AbsPath::assert(&otp_root);
                let config_path = config.config_path().to_path_buf();
                let mut apps = config.to_project_app_data(abs_otp_root);
                let eqwalizer_support_app =
                    eqwalizer_support::eqwalizer_suppport_data(abs_otp_root);
                let project = StaticProject { config_path };
                apps.push(eqwalizer_support_app);
                (
                    ProjectBuildData::Static(project),
                    apps,
                    otp_root,
                    Arc::new(IncludeMapping::default()),
                )
            }
        };

        let (otp, otp_project_apps) = Otp::discover(otp_root);
        project_apps.extend(otp_project_apps);
        report_progress("Project info loaded");
        Ok(Project {
            otp,
            project_build_data: project_build_info,
            project_apps,
            eqwalizer_config,
            include_mapping,
        })
    }

    fn load_rebar_build_info(build: &RebarConfig) -> Result<TempPath> {
        let out_file = NamedTempFile::new()?.into_temp_path();
        let mut cmd = build.rebar3_command();
        cmd.arg("experimental");
        cmd.arg("manifest");
        cmd.arg("--format");
        cmd.arg("eetf");
        cmd.arg("--to");
        cmd.arg(&out_file);

        let _ = utf8_stdout(&mut cmd)?;

        Ok(out_file)
    }
}

pub fn utf8_stdout(cmd: &mut Command) -> Result<String> {
    let output = cmd.output().with_context(|| format!("{cmd:?} failed"))?;
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

pub fn to_abs_path_buf(path: &Path) -> Result<AbsPathBuf> {
    let path = Utf8PathBuf::from_path_buf(path.to_path_buf()).expect("UTF8 conversion failed");
    Ok(AbsPathBuf::assert(path))
}

#[cfg(test)]
mod tests {
    use std::fs;

    use expect_test::expect;

    use self::temp_dir::TempDir;
    use super::*;
    use crate::test_fixture::FixtureWithProjectMeta;

    fn debug_normalise_temp_dir(dir: TempDir, actual: &impl fmt::Debug) -> String {
        let dir_str = dir.path().as_os_str().to_string_lossy().to_string();
        let actual_debug = format!("{actual:#?}\n");
        actual_debug.replace(dir_str.as_str(), "TMPDIR")
    }

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
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let manifest = ProjectManifest::discover(
            &to_abs_path_buf(&dir.path().join("app_a/src/app.erl")).unwrap(),
        );

        expect![[r#"
            Ok(
                (
                    ElpConfig {
                        config_path: None,
                        build_info: None,
                        buck: None,
                        eqwalizer: EqwalizerConfig {
                            enable_all: true,
                            max_tasks: 4,
                        },
                        rebar: ElpRebarConfig {
                            profile: "test",
                        },
                    },
                    Rebar(
                        RebarConfig {
                            config_file: AbsPathBuf(
                                "TMPDIR/rebar.config",
                            ),
                            profile: Profile(
                                "test",
                            ),
                        },
                    ),
                ),
            )
        "#]]
        .assert_eq(&debug_normalise_temp_dir(dir, &manifest));
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
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let dir_path = to_abs_path_buf(dir.path()).unwrap();
        let manifest = ProjectManifest::discover(&dir_path.join("app_b/src/app.erl"));
        expect![[r#"
            Ok(
                (
                    ElpConfig {
                        config_path: None,
                        build_info: None,
                        buck: None,
                        eqwalizer: EqwalizerConfig {
                            enable_all: true,
                            max_tasks: 4,
                        },
                        rebar: ElpRebarConfig {
                            profile: "test",
                        },
                    },
                    Json(
                        JsonConfig {
                            apps: [
                                JsonProjectAppData {
                                    name: "app_a",
                                    dir: "app_a",
                                    src_dirs: [
                                        "src",
                                    ],
                                    ebin: None,
                                    extra_src_dirs: [],
                                    include_dirs: [],
                                    macros: {},
                                },
                                JsonProjectAppData {
                                    name: "app_b",
                                    dir: "app_b",
                                    src_dirs: [
                                        "src",
                                    ],
                                    ebin: None,
                                    extra_src_dirs: [
                                        "test",
                                    ],
                                    include_dirs: [
                                        "include",
                                    ],
                                    macros: {},
                                },
                            ],
                            deps: [],
                            config_path: Some(
                                AbsPathBuf(
                                    "TMPDIR/build_info.json",
                                ),
                            ),
                        },
                    ),
                ),
            )
        "#]]
        .assert_eq(&debug_normalise_temp_dir(dir, &manifest));
    }

    #[test]
    fn test_json_project() {
        let spec = r#"
        //- /build_info.json
        {
          "apps": [
            {
              "name": "app_a",
              "dir": "app_a",
              "ebin": "../_build/test/lib/app_a/ebin",
              "extra_src_dirs": ["test"],
              "include_dirs": ["include"],
              "macros": {"TEST": "true"},
              "src_dirs": ["src"]
            },
            {
              "name": "app_b",
              "dir": "app_b",
              "ebin": "../_build/test/lib/app_b/ebin",
              "macros": {"TEST": "true"},
              "src_dirs": ["src"]
            },
            {
              "name": "eqwalizer",
              "dir": "eqwalizer",
              "ebin": "../_build/test/lib/eqwalizer/ebin",
              "macros": {"TEST": "true"},
              "src_dirs": ["src"]
            }
          ],
          "deps": [

          ],
          "root": ""
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
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let dir_path = AbsPathBuf::assert_utf8(fs::canonicalize(dir.path()).unwrap());
        if let Ok((elp_config, ProjectManifest::Json(mut manifest))) =
            ProjectManifest::discover(&dir_path.join("build_info.json"))
        {
            manifest.config_path = Some(AbsPathBuf::assert("/tmp/foo".into()));
            expect![[r#"
                (
                    ElpConfig {
                        config_path: None,
                        build_info: None,
                        buck: None,
                        eqwalizer: EqwalizerConfig {
                            enable_all: true,
                            max_tasks: 4,
                        },
                        rebar: ElpRebarConfig {
                            profile: "test",
                        },
                    },
                    JsonConfig {
                        apps: [
                            JsonProjectAppData {
                                name: "app_a",
                                dir: "app_a",
                                src_dirs: [
                                    "src",
                                ],
                                ebin: Some(
                                    "../_build/test/lib/app_a/ebin",
                                ),
                                extra_src_dirs: [
                                    "test",
                                ],
                                include_dirs: [
                                    "include",
                                ],
                                macros: {
                                    "TEST": "true",
                                },
                            },
                            JsonProjectAppData {
                                name: "app_b",
                                dir: "app_b",
                                src_dirs: [
                                    "src",
                                ],
                                ebin: Some(
                                    "../_build/test/lib/app_b/ebin",
                                ),
                                extra_src_dirs: [],
                                include_dirs: [],
                                macros: {
                                    "TEST": "true",
                                },
                            },
                            JsonProjectAppData {
                                name: "eqwalizer",
                                dir: "eqwalizer",
                                src_dirs: [
                                    "src",
                                ],
                                ebin: Some(
                                    "../_build/test/lib/eqwalizer/ebin",
                                ),
                                extra_src_dirs: [],
                                include_dirs: [],
                                macros: {
                                    "TEST": "true",
                                },
                            },
                        ],
                        deps: [],
                        config_path: Some(
                            AbsPathBuf(
                                "/tmp/foo",
                            ),
                        ),
                    },
                )
            "#]]
            .assert_debug_eq(&(elp_config, manifest));
        } else {
            panic!("bad manifest");
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
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let dir_path = to_abs_path_buf(&fs::canonicalize(dir.path()).unwrap()).unwrap();
        let manifest = ProjectManifest::discover(&dir_path.join("app_b/src/app.erl"));
        expect![[r#"
            Err(
                Error("key must be a string", line: 2, column: 5),
            )
        "#]]
        .assert_debug_eq(&manifest)
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
            let dir = FixtureWithProjectMeta::gen_project(spec);
            let manifest = ProjectManifest::discover(
                &to_abs_path_buf(&dir.path().join("app_a/src/app.erl")).unwrap(),
            );
            match manifest
                .expect_err("Must be err")
                .downcast_ref::<ProjectModelError>()
            {
                Some(ProjectModelError::NotInBuckProject) => (),
                Some(err) => panic!("Wrong err {err:?}"),
                None => panic!("Wrong error"),
            };
        }
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
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let manifest = ProjectManifest::discover(
            &to_abs_path_buf(&dir.path().join("app_b/src/app.erl")).unwrap(),
        );

        expect![[r#"
            Ok(
                (
                    ElpConfig {
                        config_path: None,
                        build_info: None,
                        buck: None,
                        eqwalizer: EqwalizerConfig {
                            enable_all: true,
                            max_tasks: 4,
                        },
                        rebar: ElpRebarConfig {
                            profile: "test",
                        },
                    },
                    NoManifest(
                        NoManifestConfig {
                            root_path: AbsPathBuf(
                                "TMPDIR/app_b",
                            ),
                            config_path: AbsPathBuf(
                                "TMPDIR/app_b/.static",
                            ),
                            name: AppName(
                                "app_b",
                            ),
                            abs_src_dirs: [
                                AbsPathBuf(
                                    "TMPDIR/app_b/src",
                                ),
                            ],
                            include_dirs: [
                                AbsPathBuf(
                                    "TMPDIR/app_b/include",
                                ),
                            ],
                            extra_src_dirs: [
                                "test",
                            ],
                        },
                    ),
                ),
            )
        "#]]
        .assert_eq(&debug_normalise_temp_dir(dir, &manifest));
    }

    #[test]
    fn test_toml_empty() {
        // This one is a real worst-case. We force discovery to happen in
        // the directory where .elp.toml is found, so it does not find the
        // app structure below. Perhaps it can be improved in future, but
        // it is very low priority, and an unlikely scenario.
        if cfg!(feature = "buck") {
            let spec = r#"
        //- /root/.elp.toml
        //- /root/app_a/src/app.erl
        -module(app).
        "#;
            let dir = FixtureWithProjectMeta::gen_project(spec);
            let discovered = ProjectManifest::discover(
                &to_abs_path_buf(&dir.path().join("root/app_a/src/app.erl")).unwrap(),
            );
            expect![[r#"
                Ok(
                    (
                        ElpConfig {
                            config_path: Some(
                                AbsPathBuf(
                                    "TMPDIR/root/.elp.toml",
                                ),
                            ),
                            build_info: None,
                            buck: None,
                            eqwalizer: EqwalizerConfig {
                                enable_all: true,
                                max_tasks: 4,
                            },
                            rebar: ElpRebarConfig {
                                profile: "test",
                            },
                        },
                        NoManifest(
                            NoManifestConfig {
                                root_path: AbsPathBuf(
                                    "TMPDIR/root",
                                ),
                                config_path: AbsPathBuf(
                                    "TMPDIR/root/.static",
                                ),
                                name: AppName(
                                    "root",
                                ),
                                abs_src_dirs: [
                                    AbsPathBuf(
                                        "TMPDIR/root",
                                    ),
                                ],
                                include_dirs: [],
                                extra_src_dirs: [],
                            },
                        ),
                    ),
                )
            "#]]
            .assert_eq(&debug_normalise_temp_dir(dir, &discovered));
        }
    }

    #[test]
    fn test_toml_syntax_error() {
        let spec = r#"
        //- /.elp.toml
        buggy stuff, oops
        //- /app_a/src/app.erl
        -module(app).
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let manifest = ProjectManifest::discover(
            &to_abs_path_buf(&dir.path().join("app_a/src/app.erl")).unwrap(),
        );

        expect![[r#"
            Err(
                "unable to read TMPDIR/.elp.toml: expected an equals, found an identifier at line 1 column 7",
            )
        "#]]
            // .assert_debug_eq(&res);

        .assert_eq(&debug_normalise_temp_dir(dir, &manifest));
    }

    #[test]
    fn test_toml_incorrect_build_info_path() {
        let spec = r#"
        //- /.elp.toml
        [build_info]
        file = "nonexistent_file"
        //- /app_a/src/app.erl
        -module(app).
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let manifest = ProjectManifest::discover(
            &to_abs_path_buf(&dir.path().join("app_a/src/app.erl")).unwrap(),
        );
        if let Err(err) = manifest {
            let res = normalise_temp_dir_in_err(dir, err);
            expect![[r#"
                "Reading build_info file TMPDIR//nonexistent_file, found in TMPDIR//.elp.toml: No such file or directory (os error 2)"
            "#]]
            .assert_debug_eq(&res);
        } else {
            panic!("Expected syntax error, got {manifest:?}")
        }
    }

    fn normalise_temp_dir_in_err(dir: TempDir, err: anyhow::Error) -> String {
        let dir_str = dir.path().as_os_str().to_string_lossy().to_string();
        let err_str = format!("{err}");
        err_str.replace(dir_str.as_str(), "TMPDIR/")
    }

    #[test]
    fn test_toml_build_info_parse_error() {
        let spec = r#"
        //- /.elp.toml
        [build_info]
        file = "info.json"
        //- /info.json
        {
        syntax error!
        }
        //- /app_a/src/app.erl
        -module(app).
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let manifest = ProjectManifest::discover(
            &to_abs_path_buf(&dir.path().join("app_a/src/app.erl")).unwrap(),
        );
        if let Err(err) = manifest {
            let res = normalise_temp_dir_in_err(dir, err);
            expect![[r#"
                "Reading build_info file TMPDIR//info.json, found in TMPDIR//.elp.toml: key must be a string at line 2 column 1"
            "#]]
            .assert_debug_eq(&res);
        } else {
            panic!("Expected syntax error, got {manifest:?}")
        }
    }

    #[test]
    fn test_toml_build_info_manifest() {
        let spec = r#"
        //- /.elp.toml
        [build_info]
        file = "info.json"
        //- /info.json
        {
            "apps": [
                {
                    "name": "app_a",
                    "dir": "app_a",
                    "src_dirs": ["src"]
                }
            ]
        }
        //- /app_a/src/app.erl
        -module(app).
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        if let Ok((_elp_config, ProjectManifest::Json(mut json_config))) = ProjectManifest::discover(
            &to_abs_path_buf(&dir.path().join("app_a/src/app.erl")).unwrap(),
        ) {
            json_config.config_path = Some(AbsPathBuf::assert("/tmp/dummy".into()));
            expect![[r#"
                JsonConfig {
                    apps: [
                        JsonProjectAppData {
                            name: "app_a",
                            dir: "app_a",
                            src_dirs: [
                                "src",
                            ],
                            ebin: None,
                            extra_src_dirs: [],
                            include_dirs: [],
                            macros: {},
                        },
                    ],
                    deps: [],
                    config_path: Some(
                        AbsPathBuf(
                            "/tmp/dummy",
                        ),
                    ),
                }
            "#]]
            .assert_debug_eq(&json_config);
        } else {
            panic!()
        }
    }

    #[test]
    fn test_toml_rebar_profile() {
        let spec = r#"
        //- /.elp.toml
        [rebar]
        profile = "other"
        //- /app_a/src/app.erl
        -module(app).
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        if let Ok((elp_config, ProjectManifest::NoManifest(_))) = ProjectManifest::discover(
            &to_abs_path_buf(&dir.path().join("app_a/src/app.erl")).unwrap(),
        ) {
            expect![[r#"
                ElpConfig {
                    config_path: Some(
                        AbsPathBuf(
                            "TMPDIR/.elp.toml",
                        ),
                    ),
                    build_info: None,
                    buck: None,
                    eqwalizer: EqwalizerConfig {
                        enable_all: true,
                        max_tasks: 4,
                    },
                    rebar: ElpRebarConfig {
                        profile: "other",
                    },
                }
            "#]]
            .assert_eq(&debug_normalise_temp_dir(dir, &elp_config));
        } else {
            panic!()
        }
    }

    #[test]
    fn serde_serialize_elp_toml() {
        let result = toml::to_string::<ElpConfig>(&ElpConfig {
            config_path: None,
            build_info: Some(BuildInfoConfig {
                file: Some(PathBuf::from("path/to/file")),
                apps: None,
                deps: None,
            }),
            buck: Some(BuckConfig {
                config_path: None,
                buck_root: None,
                enabled: true,
                deps_target: Some("root//target/deps".to_string()),
                deps_targets: vec!["root//dep/one".to_string(), "root//dep/two".to_string()],
                build_deps: false,
                included_targets: vec![
                    "root//target/one".to_string(),
                    "root//target/two".to_string(),
                ],
                excluded_targets: vec![
                    "root//target/three".to_string(),
                    "root//target/four".to_string(),
                ],
                source_root: Some(PathBuf::from("path/to/root")),
            }),
            eqwalizer: EqwalizerConfig {
                enable_all: true,
                max_tasks: 34,
            },
            rebar: ElpRebarConfig {
                profile: "my_profile".to_string(),
            },
        })
        .unwrap();
        expect![[r#"
            [build_info]
            file = "path/to/file"

            [buck]
            enabled = true
            deps_target = "root//target/deps"
            deps_targets = ["root//dep/one", "root//dep/two"]
            build_deps = false
            included_targets = ["root//target/one", "root//target/two"]
            excluded_targets = ["root//target/three", "root//target/four"]
            source_root = "path/to/root"

            [eqwalizer]
            enable_all = true
            max_tasks = 34

            [rebar]
            profile = "my_profile"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_deserialize_elp_toml() {
        let lints: ElpConfig = toml::from_str(
            r#"
            [build_info]
            file = "path/to/file"

            [buck]
            enabled = true
            deps_target = "root//target/deps"
            build_deps = false
            included_targets = ["root//target/one", "root//target/two"]
            excluded_targets = ["root//target/three", "root//target/four"]
            source_root = "path/to/root"

            [eqwalizer]
            enable_all = true
            max_tasks = 34

            [rebar]
            profile = "my_profile"
             "#,
        )
        .unwrap();

        expect![[r#"
            ElpConfig {
                config_path: None,
                build_info: Some(
                    BuildInfoConfig {
                        file: Some(
                            "path/to/file",
                        ),
                        apps: None,
                        deps: None,
                    },
                ),
                buck: Some(
                    BuckConfig {
                        config_path: None,
                        buck_root: None,
                        enabled: true,
                        deps_target: Some(
                            "root//target/deps",
                        ),
                        deps_targets: [],
                        build_deps: false,
                        included_targets: [
                            "root//target/one",
                            "root//target/two",
                        ],
                        excluded_targets: [
                            "root//target/three",
                            "root//target/four",
                        ],
                        source_root: Some(
                            "path/to/root",
                        ),
                    },
                ),
                eqwalizer: EqwalizerConfig {
                    enable_all: true,
                    max_tasks: 34,
                },
                rebar: ElpRebarConfig {
                    profile: "my_profile",
                },
            }
        "#]]
        .assert_debug_eq(&lints);
    }

    #[test]
    fn test_discover() {
        let root = AbsPathBuf::assert(Utf8Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures"));
        let manifest =
            ProjectManifest::discover_rebar(&root.join("nested"), None, IncludeParentDirs::Yes);
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

    #[test]
    fn test_toml_apps_as_array() {
        let spec = r#"
        //- /.elp.toml
        [build_info]
        apps = ["app_a"]
        //- /app_a/src/app.erl
        -module(app).
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let (_elp_config, manifest) = ProjectManifest::discover(
            &to_abs_path_buf(&dir.path().join("app_a/src/app.erl")).unwrap(),
        )
        .unwrap();
        match manifest {
            ProjectManifest::Json(json_config) => {
                assert_eq!(json_config.apps.len(), 1);
            }
            _ => {
                panic!("Expected json config")
            }
        }
    }

    #[test]
    fn test_toml_apps_as_glob() {
        let spec = r#"
        //- /.elp.toml
        [build_info]
        apps = ["lib/*", "other_lib/*"]
        //- /lib/app_a/src/app_a.erl
        -module(app_a).
        //- /lib/app_b/src/app_b.erl
        -module(app_b).
        //- /other_lib/app_c/src/app_c.erl
        -module(app_c).
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let (_elp_config, manifest) = ProjectManifest::discover(
            &to_abs_path_buf(&dir.path().join("lib/app_a/src/app.erl")).unwrap(),
        )
        .unwrap();
        match manifest {
            ProjectManifest::Json(json_config) => {
                assert_eq!(json_config.apps.len(), 3);
            }
            _ => {
                panic!("Expected json config")
            }
        }
    }

    #[test]
    fn test_toml_apps_as_glob_struct() {
        let spec = r#"
        //- /.elp.toml
        [build_info]
        apps = [ "lib/*",
                 {"name" = "app_a", "dir" = "lib/app_a", "src_dirs" = ["src", "gen"]},
                 "other_lib/*"
               ]
        //- /lib/app_a/src/app_a.erl
        -module(app_a).
        //- /lib/app_a/src/gen/app_a_gen.erl
        -module(app_a_gen).
        //- /lib/app_b/src/app_b.erl
        -module(app_b).
        //- /other_lib/app_c/src/app_c.erl
        -module(app_c).
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let (_elp_config, manifest) = ProjectManifest::discover(
            &to_abs_path_buf(&dir.path().join("lib/app_a/src/app.erl")).unwrap(),
        )
        .unwrap();
        match manifest {
            ProjectManifest::Json(json_config) => {
                let mut apps = FxHashMap::default();
                json_config.apps.iter().for_each(|app| {
                    apps.insert(app.name.clone(), app);
                });
                let app_a = apps.get("app_a");
                let app_b = apps.get("app_b");
                assert_eq!(app_a.unwrap().src_dirs, ["src", "gen"]);
                assert_eq!(app_b.unwrap().src_dirs, ["src"]);
            }
            _ => {
                panic!("Expected json config")
            }
        }
    }
}

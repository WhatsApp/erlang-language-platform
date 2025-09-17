/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use core::str;
use std::borrow::Cow;
use std::convert::TryFrom;
use std::fmt;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;

use anyhow::Result;
use anyhow::bail;
use eetf::Term::Atom;
use elp_log::timeit;
use elp_log::timeit_with_telemetry;
use elp_syntax::SmolStr;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use indexmap::indexset;
use lazy_static::lazy_static;
use parking_lot::Mutex;
use paths::AbsPath;
use paths::AbsPathBuf;
use paths::RelPathBuf;
use paths::Utf8PathBuf;
use regex::Regex;
use serde::Deserialize;
use serde::Serialize;
use thiserror::Error;

use crate::AppName;
use crate::AppType;
use crate::CommandProxy;
use crate::ElpConfig;
use crate::ProjectAppData;
use crate::ProjectModelError;
use crate::otp::Otp;

pub type TargetFullName = String;

lazy_static! {
    static ref DIRS: Vec<RelPathBuf> = vec!["src", "test", "include"]
        .into_iter()
        .flat_map(|dir| dir.try_into())
        .collect();
}

const ERL_EXT: &str = "erl";

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    Ord,
    PartialOrd,
    Deserialize,
    Serialize,
    Default
)]
pub struct BuckConfig {
    #[serde(skip_deserializing)]
    #[serde(skip_serializing)]
    /// Location of ELP_CONFIG_FILE this config was loaded from
    pub(crate) config_path: Option<AbsPathBuf>,
    #[serde(skip_deserializing)]
    #[serde(skip_serializing)]
    pub buck_root: Option<AbsPathBuf>,
    pub enabled: bool,
    pub deps_target: Option<String>,
    #[serde(default)]
    pub deps_targets: Vec<String>,
    pub build_deps: bool,
    pub included_targets: Vec<String>,
    #[serde(default)]
    pub excluded_targets: Vec<String>,
    pub(crate) source_root: Option<PathBuf>,
}

impl BuckConfig {
    pub fn buck_command(&self) -> CommandProxy<'_> {
        static BUCK_GLOBAL_LOCK: Mutex<()> = Mutex::new(());
        let guard = BUCK_GLOBAL_LOCK.lock();
        let mut cmd = Command::new("buck2");
        // buck2 doesn't handle the RUST_BACKTRACE option well and can become extremely
        // slow. Until that is fixed ensure the option is not propagated to buck2.
        cmd.env_remove("RUST_BACKTRACE")
            .env_remove("RUST_LIB_BACKTRACE");
        cmd.arg("--isolation-dir");
        cmd.arg("lsp");
        cmd.current_dir(self.buck_root());
        CommandProxy::new(guard, cmd)
    }

    pub fn buck_root(&self) -> &AbsPathBuf {
        self.buck_root.as_ref().unwrap()
    }

    pub fn source_root(&self) -> Cow<'_, AbsPathBuf> {
        let buck_root = self.buck_root();
        match self.source_root {
            None => Cow::Borrowed(buck_root),
            Some(ref dir) => Cow::Owned(buck_root.join(
                Utf8PathBuf::from_path_buf(dir.to_path_buf()).expect("UTF8 conversion failed"),
            )),
        }
    }

    pub(crate) fn make_config(path: &AbsPath, config: &mut ElpConfig) -> Result<()> {
        let buck_conf = match &mut config.buck {
            Some(conf) => conf,
            None => return Ok(()),
        };
        if buck_conf.enabled {
            //assign any file from buck monorepo in order to find root
            buck_conf.buck_root = Some(path.parent().unwrap().to_path_buf());
            buck_conf.config_path = Some(path.to_path_buf());

            let root = find_root(buck_conf)?;
            buck_conf.buck_root = Some(root);

            for excluded in buck_conf.excluded_targets.iter_mut() {
                let pat = "/...";
                if excluded.ends_with(pat) {
                    excluded.truncate(excluded.len() - pat.len());
                }
            }
        }
        Ok(())
    }

    pub(crate) fn config_path(&self) -> &AbsPath {
        self.config_path.as_ref().unwrap()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct TargetInfo {
    pub targets: FxHashMap<TargetFullName, Target>,
    pub path_to_target_name: FxHashMap<AbsPathBuf, TargetFullName>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuckProject {
    pub target_info: TargetInfo,
    pub buck_conf: BuckConfig,
}

#[derive(Default, Clone, Debug, PartialEq, Eq)]
pub struct IncludeMapping {
    includes: FxHashMap<SmolStr, AbsPathBuf>,
    deps: FxHashMap<TargetFullName, FxHashSet<TargetFullName>>,
    /// A buck target can have an alternative app name in case the
    /// last part of its `TargetFullName` is ambiguous.  Keep a
    /// mapping from these to the corresponding `TargetFullName`.
    app_names: FxHashMap<AppName, TargetFullName>,
    app_names_rev: FxHashMap<TargetFullName, AppName>,
    /// The OTP apps are always available as dependencies, keep track
    /// of what they are
    otp_apps: FxHashSet<AppName>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IncludeMappingScope {
    Local(AppName),
    Remote,
}

/// We add a prefix to the local and remote lookup strings in the
/// `IncludeMapping`, so we can do dependency checking on the remote
/// one via `include_lib`
const LOCAL_LOOKUP_INCLUDE_PREFIX: &str = "L";
const REMOTE_LOOKUP_INCLUDE_PREFIX: &str = "R";

impl IncludeMapping {
    pub fn add_otp(&mut self, otp_apps: &[ProjectAppData]) {
        for app in otp_apps {
            self.otp_apps.insert(app.name.clone());
            for inc in &app.include_dirs {
                self.update_mapping_from_path(&app.name.0, inc.clone());
            }
        }
    }

    fn update_mapping_from_path(&mut self, app_name: &str, path: AbsPathBuf) {
        if let Ok(paths) = Utf8PathBuf::from(path).read_dir_utf8() {
            for path in paths.flatten() {
                let path = path.into_path();
                if let Some("hrl") = path.extension()
                    && let Some(basename) = path.file_name()
                {
                    // We encode the local include string as follows
                    // - First an indicator to flag that it is local
                    // - Next the app name that it is local to
                    // - Then the base file name.
                    // We always do the local lookup in the
                    // context of a specific app, this ensures the
                    // app matches, and allows different apps to
                    // use the same include file base name.
                    let local_include = SmolStr::new(format!(
                        "{LOCAL_LOOKUP_INCLUDE_PREFIX}:{app_name}:{basename}"
                    ));
                    let trimmed_app_name =
                        app_name.strip_suffix("_includes_only").unwrap_or(app_name);
                    let remote_include = SmolStr::new(format!(
                        "{REMOTE_LOOKUP_INCLUDE_PREFIX}:{trimmed_app_name}/include/{basename}"
                    ));
                    let path = AbsPathBuf::assert(path);
                    // TODO: remove clone()
                    self.insert(local_include, path.clone());
                    self.insert(remote_include, path);
                }
            }
        }
    }

    pub fn get(&self, scope: IncludeMappingScope, path: &SmolStr) -> Option<&AbsPathBuf> {
        let lookup_path = match scope {
            IncludeMappingScope::Local(app_name) => {
                SmolStr::new(format!("{LOCAL_LOOKUP_INCLUDE_PREFIX}:{app_name}:{path}"))
            }
            IncludeMappingScope::Remote => {
                SmolStr::new(format!("{REMOTE_LOOKUP_INCLUDE_PREFIX}:{path}"))
            }
        };
        self.includes.get(&lookup_path)
    }

    pub fn insert(&mut self, path: SmolStr, abs_path: AbsPathBuf) -> Option<AbsPathBuf> {
        self.includes.insert(path, abs_path)
    }

    /// Local lookup of `path` in the dependencies of `app_name`
    pub fn find_local(&self, app_name: &AppName, path: &SmolStr) -> Option<AbsPathBuf> {
        self.get(IncludeMappingScope::Local(app_name.clone()), path);
        if let Some(current) = self.app_names.get(app_name) {
            let mut visited = FxHashSet::default();
            self.dfs_local(current, path, &mut visited)
        } else {
            None
        }
    }

    fn dfs_local(
        &self,
        current: &TargetFullName,
        path: &SmolStr,
        visited: &mut FxHashSet<TargetFullName>,
    ) -> Option<AbsPathBuf> {
        if let Some(app_name) = self.app_names_rev.get(current)
            && let Some(abs_path) = self.get(IncludeMappingScope::Local(app_name.clone()), path)
        {
            return Some(abs_path.clone());
        }

        if visited.contains(current) {
            return None;
        }

        visited.insert(current.clone());

        if let Some(deps) = self.deps.get(current) {
            for dep in deps {
                if let Some(abs_path) = self.dfs_local(dep, path, visited) {
                    return Some(abs_path);
                }
            }
        }

        None
    }

    /// For each buck TargetFullName we have a set of immediate dependencies.
    /// Check for the `source` one if the target is in the graph rooted at it.
    /// If the target is an OTP app then it is always a dependency.
    pub fn is_dep(&self, source: &TargetFullName, target_app: &AppName) -> bool {
        if self.otp_apps.contains(target_app) {
            return true;
        }
        if let Some(target) = self.app_names.get(target_app) {
            let mut visited = FxHashSet::default();
            self.dfs(source, target, &mut visited)
        } else {
            false
        }
    }

    fn dfs(
        &self,
        current: &TargetFullName,
        target: &TargetFullName,
        visited: &mut FxHashSet<TargetFullName>,
    ) -> bool {
        if current == target {
            return true;
        }

        if visited.contains(current) {
            return false;
        }

        visited.insert(current.clone());

        if let Some(deps) = self.deps.get(current) {
            for dep in deps {
                if self.dfs(dep, target, visited) {
                    return true;
                }
            }
        }

        false
    }
}

impl BuckProject {
    pub fn load_from_config(
        buck_conf: &BuckConfig,
        query_config: &BuckQueryConfig,
        report_progress: &impl Fn(&str),
    ) -> Result<
        (
            BuckProject,
            Vec<ProjectAppData>,
            Utf8PathBuf,
            Arc<IncludeMapping>,
        ),
        anyhow::Error,
    > {
        let _timer = timeit_with_telemetry!("BuckProject::load_from_config");
        let target_info = load_buck_targets_bxl(buck_conf, query_config, report_progress)?;
        report_progress("Making project app data");
        let (project_app_data, mut include_mapping) =
            targets_to_project_data_bxl(&target_info.targets);
        let project = BuckProject {
            target_info,
            buck_conf: buck_conf.clone(),
        };
        let otp_root = Otp::find_otp()?;
        // TODO: we now get these twice. Perhaps they should be cached?
        let (_otp, otp_project_apps) = Otp::discover(otp_root.clone());
        include_mapping.add_otp(&otp_project_apps);
        Ok((
            project,
            project_app_data,
            otp_root,
            Arc::new(include_mapping),
        ))
    }

    pub fn target(&self, file_path: &AbsPathBuf) -> Option<String> {
        self.target_info.path_to_target_name.get(file_path).cloned()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
#[derive(Default)]
enum BuckTargetOrigin {
    #[default]
    App,
    Dep,
    Prelude,
}

// Serde serialization via String
impl From<BuckTargetOrigin> for String {
    fn from(val: BuckTargetOrigin) -> Self {
        match val {
            BuckTargetOrigin::App => "app".to_string(),
            BuckTargetOrigin::Dep => "dep".to_string(),
            BuckTargetOrigin::Prelude => "prelude".to_string(),
        }
    }
}

// Serde serialization via String
impl TryFrom<String> for BuckTargetOrigin {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        TryFrom::try_from(value.as_str())
    }
}
impl TryFrom<&str> for BuckTargetOrigin {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "app" => Ok(BuckTargetOrigin::App),
            "dep" => Ok(BuckTargetOrigin::Dep),
            "prelude" => Ok(BuckTargetOrigin::Prelude),
            _ => Err(format!(
                "bad origin value '{value}', expected 'app', 'dep', or 'prelude'."
            )),
        }
    }
}

/// The interface type used in reading the output of running `elp.bxl`
#[derive(Deserialize, Debug)]
pub struct BuckTarget {
    name: String,
    /// Alternative application name, if the `name` field above is
    /// ambiguous. If set, is used instead of `name`.
    app_name: Option<String>,
    /// Some if target is test, in which case srcs will be empty
    suite: Option<String>,
    #[serde(default)]
    srcs: Vec<String>,
    #[serde(default)]
    includes: Vec<String>,
    #[serde(default)]
    labels: FxHashSet<String>,
    #[serde(default)]
    deps: Vec<TargetFullName>,
    #[serde(default)]
    apps: Vec<TargetFullName>,
    /// A target may require include files from other targets, but
    /// making the target a direct dependency (app) causes a loop. The
    /// workaround is to declare it as an `included_application`. They
    /// are populated here, and used only for populating the include
    /// path.
    #[serde(default)]
    included_apps: Vec<TargetFullName>,
    /// extra_includes are a similar but slightly different
    /// hack. buck2 creates a shadow target with only the include path
    /// in it, as a proxy for the real target when building the graph.
    /// These do not have dependencies, so make it possible for buck2
    /// to build the graph.
    #[serde(default)]
    extra_includes: Vec<TargetFullName>,
    #[serde(default)]
    origin: BuckTargetOrigin,
}

impl BuckTarget {
    fn name(&self) -> AppName {
        if let Some(name) = self.app_name.clone() {
            AppName(name)
        } else {
            AppName(self.name.clone())
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Target {
    // full-name, like cell//path/to/target/...
    pub name: TargetFullName,
    pub app_name: AppName,
    pub dir: AbsPathBuf,
    pub src_files: Vec<AbsPathBuf>,
    pub include_files: Vec<AbsPathBuf>,
    pub deps: Vec<TargetFullName>,
    pub apps: Vec<TargetFullName>,
    pub included_apps: Vec<TargetFullName>,
    pub extra_includes: Vec<TargetFullName>,
    pub ebin: Option<AbsPathBuf>,
    pub target_type: TargetType,
    /// true if there are .hrl files in the src dir
    pub private_header: bool,
}

impl Target {
    fn app_type(&self) -> AppType {
        match self.target_type {
            TargetType::ErlangApp => AppType::App,
            TargetType::ErlangTest => AppType::App,
            TargetType::ErlangTestUtils => AppType::App,
            TargetType::ThirdParty => AppType::Dep,
        }
    }

    fn include_files(&self) -> Vec<AbsPathBuf> {
        let mut include_files = self.include_files.clone();
        if self.private_header {
            self.src_files.iter().for_each(|path| {
                if Some("hrl") == path.extension() {
                    include_files.push(include_path_from_file(path));
                }
            });
        }
        include_files
    }
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq)]
pub enum TargetType {
    ErlangApp,
    ErlangTest,
    //rebar3's deps
    ThirdParty,
    ErlangTestUtils,
}

fn load_buck_targets_bxl(
    buck_config: &BuckConfig,
    build: &BuckQueryConfig,
    report_progress: &impl Fn(&str),
) -> Result<TargetInfo> {
    let _timer = timeit!("loading info from buck");
    let root = buck_config.buck_root();

    let mut dep_path = if buck_config.build_deps {
        report_progress("Building third party targets");
        build_third_party_targets(buck_config)?
    } else {
        FxHashMap::default()
    };

    match build {
        BuckQueryConfig::BuildGeneratedCode => {
            report_progress("Querying and generating buck targets")
        }
        BuckQueryConfig::NoBuildGeneratedCode => report_progress("Querying buck targets"),
    }

    let buck_targets = query_buck_targets(buck_config, build)?;

    report_progress("Making target info");
    let mut target_info = TargetInfo::default();

    let mut used_deps = FxHashSet::default();

    for (name, buck_target) in &buck_targets {
        if buck_target.origin != BuckTargetOrigin::Prelude
            && let Ok(target) = make_buck_target(
                root,
                name,
                buck_target,
                buck_config.build_deps,
                &mut dep_path,
                &mut target_info,
            )
        {
            for target_name in &target.apps {
                used_deps.insert(target_name.clone());
            }
            for target_name in &target.deps {
                used_deps.insert(target_name.clone());
            }
            target_info.targets.insert(name.clone(), target);
        }
    }
    // Insert used prelude values too
    for name in &used_deps {
        if let Some(buck_target) = &buck_targets.get(name)
            && buck_target.origin == BuckTargetOrigin::Prelude
            && let Ok(target) = make_buck_target(
                root,
                name,
                buck_target,
                buck_config.build_deps,
                &mut dep_path,
                &mut target_info,
            )
        {
            target_info.targets.insert(name.clone(), target);
        }
    }
    Ok(target_info)
}

fn make_buck_target(
    root: &AbsPathBuf,
    name: &String,
    target: &BuckTarget,
    build_deps: bool,
    dep_path: &mut FxHashMap<String, AbsPathBuf>,
    target_info: &mut TargetInfo,
) -> Result<Target> {
    let dir = find_app_root_bxl(root, name, target).expect("could not find app root");

    let (src_files, include_files, target_type, private_header, ebin) =
        if let Some(ref suite) = target.suite {
            let src_file = buck_path_to_abs_path(root, suite)?;
            let src = vec![src_file.clone()];
            target_info
                .path_to_target_name
                .insert(src_file, name.clone());
            let mut include_files = vec![];
            for include in &target.includes {
                if let Ok(inc) = AbsPathBuf::try_from(include.as_str()) {
                    include_files.push(inc);
                }
            }
            (src, include_files, TargetType::ErlangTest, false, None)
        } else {
            let mut private_header = false;
            let target_type = compute_target_type(name, target);
            let mut src_files = vec![];
            for src in &target.srcs {
                let src = buck_path_to_abs_path(root, src).unwrap();
                if Some("hrl") == src.extension() {
                    private_header = true;
                }
                src_files.push(src);
            }
            let mut include_files = vec![];
            for include in &target.includes {
                let mut inc = buck_path_to_abs_path(root, include).unwrap();
                if inc.extension().is_some() {
                    inc.pop();
                }
                include_files.push(inc);
            }

            let ebin = match target_type {
                TargetType::ThirdParty if build_deps => dep_path
                    .remove(name)
                    .map(|dir| dir.join(Utf8PathBuf::from("ebin"))),
                TargetType::ThirdParty => Some(dir.clone()),
                _ => None,
            };
            (src_files, include_files, target_type, private_header, ebin)
        };
    Ok(Target {
        name: name.clone(),
        app_name: target.name(),
        dir,
        src_files,
        include_files,
        deps: target.deps.clone(),
        apps: target.apps.clone(),
        included_apps: target.included_apps.clone(),
        extra_includes: target.extra_includes.clone(),
        ebin,
        target_type,
        private_header,
    })
}

fn compute_target_type(name: &TargetFullName, target: &BuckTarget) -> TargetType {
    if target.origin != BuckTargetOrigin::App && (name.starts_with("prelude//"))
        || name.contains("//third-party")
    {
        TargetType::ThirdParty
    } else {
        let test_utils = target.labels.contains("test_utils");
        let test_application = target.labels.contains("test_application");
        let elp_enabled = target.labels.contains("elp_enabled");
        match (elp_enabled, test_application, test_utils) {
            (true, _, _) => TargetType::ErlangApp,
            (false, false, false) => TargetType::ErlangApp,
            (_, _, _) => TargetType::ErlangTestUtils,
        }
    }
}

/// finds buck root directory based on buck config, executing `buck2 root`
fn find_root(buck_config: &BuckConfig) -> Result<AbsPathBuf> {
    let _timer = timeit!("loading root");
    let output = match buck_config.buck_command().arg("root").output() {
        Ok(out) => out,
        Err(err) => {
            log::error!("Err executing buck2 root: {err:?}");
            bail!(ProjectModelError::MissingBuck(err))
        }
    };
    if !output.status.success() {
        if output.status.code() == Some(1)
            && String::from_utf8_lossy(&output.stderr)
                .contains("not in a Buck project, are you missing a .buckconfig file?")
        {
            bail!(ProjectModelError::NotInBuckProject)
        } else {
            bail!(
                "Failed to get buck2 root, error code: {:?}, stderr: {:?}",
                output.status.code(),
                String::from_utf8(output.stderr)
            )
        }
    }
    let path = String::from_utf8(output.stdout)?;
    match AbsPathBuf::try_from(path.trim()) {
        Ok(path) => Ok(path),
        Err(path) => bail!("expected absolute path, got {}", path),
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BuckQueryConfig {
    BuildGeneratedCode,
    NoBuildGeneratedCode,
}

fn query_buck_targets(
    buck_config: &BuckConfig,
    query_config: &BuckQueryConfig,
) -> Result<FxHashMap<TargetFullName, BuckTarget>> {
    let _timer = timeit!("load buck targets");
    let result = query_buck_targets_bxl(buck_config, query_config)?;

    let result = result
        .into_iter()
        .filter(|(name, _)| {
            !buck_config
                .excluded_targets
                .iter()
                .any(|excluded| name.starts_with(excluded))
        })
        .filter(|(_, target)| {
            target.suite.is_some()
                || !target.srcs.is_empty()
                || !target.includes.is_empty()
                || !target.apps.is_empty()
                || !target.deps.is_empty()
        })
        .collect();
    Ok(result)
}

pub fn query_buck_targets_bxl(
    buck_config: &BuckConfig,
    build: &BuckQueryConfig,
) -> Result<FxHashMap<String, BuckTarget>> {
    let mut targets = Vec::default();
    for target in &buck_config.included_targets {
        targets.push("--included_targets");
        targets.push(target);
    }
    let build_args = if build == &BuckQueryConfig::BuildGeneratedCode {
        vec!["--build_generated_code", "true"]
    } else {
        vec![]
    };
    let mut command = buck_config.buck_command();
    command
        .arg("bxl")
        .arg("--config=client.id=elp")
        .arg("prelude//erlang/elp.bxl:elp_config")
        .arg("--")
        .args(build_args)
        .args(targets);
    let output = command.output()?;
    if !output.status.success() {
        let reason = match output.status.code() {
            Some(code) => format!("Exited with status code: {code}"),
            None => "Process terminated by signal".to_string(),
        };
        let error = match String::from_utf8(output.stderr) {
            Ok(err) => BuckQueryError::new(format!("{command}"), reason.clone(), err.clone()),
            Err(_) => BuckQueryError::new(format!("{command}"), reason.clone(), "".to_string()),
        };
        bail!(error);
    }
    let string = String::from_utf8(output.stdout)?;
    let result: FxHashMap<TargetFullName, BuckTarget> = serde_json::from_str(&string)?;
    Ok(result)
}

#[derive(Error, Debug)]
pub struct BuckQueryError {
    pub command: String,
    pub reason: String,
    pub details: String,
    pub buck_ui_url: Option<String>,
}

impl BuckQueryError {
    pub fn new(command: String, reason: String, details: String) -> BuckQueryError {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"Buck UI: (http\S+)").unwrap();
        }
        let buck_ui_url = RE
            .captures(&details)
            .and_then(|m| m.get(1))
            .map(|m| m.as_str().to_string());
        BuckQueryError {
            command,
            reason,
            details,
            buck_ui_url,
        }
    }
}

impl fmt::Display for BuckQueryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(url) = &self.buck_ui_url {
            write!(
                f,
                "Project Initialisation Failed: invalid or missing buck 2 configuration. See {url} for details"
            )
        } else {
            write!(
                f,
                "Project Initialisation Failed: invalid or missing buck 2 configuration"
            )
        }
    }
}

fn build_third_party_targets(
    buck_config: &BuckConfig,
) -> Result<FxHashMap<TargetFullName, AbsPathBuf>> {
    let deps_target = match &buck_config.deps_target {
        None => return Ok(FxHashMap::default()),
        Some(_) if !buck_config.build_deps => return Ok(FxHashMap::default()),
        Some(target) => target,
    };
    let _timer = timeit!("building third party deps");
    let output = buck_config
        .buck_command()
        .arg("build")
        .arg("--config=client.id=elp")
        .arg("--prefer-local")
        .arg("--show-full-json-output")
        .arg(deps_target)
        .output()?;
    if !output.status.success() {
        bail!(
            "Failed to get buck2 build target output, error code: {:?}, stderr: {:?}",
            output.status.code(),
            String::from_utf8(output.stderr)
        );
    }
    let string = String::from_utf8(output.stdout)?;
    let result: FxHashMap<String, String> = serde_json::from_str(&string)?;
    // yaws_priv and piqi_priv are buck hacks, we get empty path for it
    Ok(result
        .into_iter()
        .filter(|(_, v)| !v.is_empty())
        .map(|(k, v)| (k, AbsPathBuf::assert(v.into())))
        .collect())
}

/// Convert cell//path/to/project_file.erl to /Users/$USER/buckroot/path/to/project_file.erl
fn buck_path_to_abs_path(root: &AbsPath, target: &str) -> Result<AbsPathBuf> {
    // TODO: remove this function once the BXL query is used instead.
    //       It is an approximation, targets may be in different cells.
    //       T188371274
    if target.contains("//") {
        let mut split = target.split("//");
        let _ = split.next(); // "cell" or empty in case of //...
        match split.next() {
            None => bail!("couldn't find a path for target {:?}", target),
            Some(path) => Ok(root.join(path)),
        }
    } else {
        Ok(root.join(target))
    }
}

/// convert call//path/target:tgt_name into abs path ~/buckroot/path/target
fn find_buck_file_base_target_dir(
    root: &AbsPath,
    target_name: &TargetFullName,
) -> Result<AbsPathBuf> {
    let path: String = target_name.chars().take_while(|ch| *ch != ':').collect();
    buck_path_to_abs_path(root, &path)
}

fn find_app_root_bxl(
    root: &AbsPath,
    target_name: &TargetFullName,
    target: &BuckTarget,
) -> Option<AbsPathBuf> {
    let dir_based_on_buck_file = find_buck_file_base_target_dir(root, target_name).ok()?;
    let mut set = indexset![];
    let paths = target
        .srcs
        .iter()
        .chain(target.includes.iter())
        .chain(target.suite.iter());

    for path in paths {
        if let Ok(path) = AbsPathBuf::try_from(path.as_str()) {
            let parent = path.parent();
            if let Some(parent) = parent {
                set.insert(parent.to_path_buf());
            }
        }
    }

    if set.len() > 1 {
        // Find the common prefix for all the src directory paths.
        // We will always get a `Some` result, due to the length check
        set.into_iter().reduce(|a, b| common_prefix(&a, &b))
    } else {
        Some(set.into_iter().next().unwrap_or(
            // Otherwise, we just return the directory containing the BUCK file,
            dir_based_on_buck_file,
        ))
    }
}

/// Find the common prefix between the two paths
fn common_prefix(a: &AbsPathBuf, b: &AbsPathBuf) -> AbsPathBuf {
    let mut common = Utf8PathBuf::new();
    for (c1, c2) in a.components().zip(b.components()) {
        if c1 != c2 {
            break;
        }
        common.push(c1);
    }
    AbsPathBuf::assert(common)
}

fn targets_to_project_data_bxl(
    targets: &FxHashMap<TargetFullName, Target>,
) -> (Vec<ProjectAppData>, IncludeMapping) {
    let mut include_mapping = IncludeMapping::default();
    let mut result: Vec<ProjectAppData> = vec![];

    for target in targets.values() {
        target.include_files.iter().for_each(|inc: &AbsPathBuf| {
            // TODO: make a FXHashSet of include paths, and use that to update the mapping
            let include_path = include_path_from_file(inc);
            include_mapping.update_mapping_from_path(&target.app_name.0, include_path);
        });

        if target.private_header {
            target.src_files.iter().for_each(|path: &AbsPathBuf| {
                if Some("hrl") == path.extension() {
                    let include_path = include_path_from_file(path);
                    include_mapping.update_mapping_from_path(&target.app_name.0, include_path);
                }
            });
        }
    }

    // Track target dependencies, as a check in the include_mapping, which is global.
    // The app being looked up must be in the dependency relation of the app.
    // Note: This step must be fast, so we do not build out the graph.
    for (target_name, target) in targets {
        let all_deps: FxHashSet<TargetFullName> = FxHashSet::from_iter(
            target
                .deps
                .iter()
                .chain(target.apps.iter().chain(target.included_apps.iter()))
                .chain(target.apps.iter().chain(target.extra_includes.iter()))
                .cloned(),
        );
        include_mapping.deps.insert(target_name.clone(), all_deps);
        include_mapping
            .app_names
            .insert(target.app_name.clone(), target_name.clone());
        include_mapping
            .app_names_rev
            .insert(target_name.clone(), target.app_name.clone());
    }

    for target in targets.values() {
        let macros = if target.target_type != TargetType::ThirdParty {
            vec![Atom("TEST".into()), Atom("COMMON_TEST".into())]
        } else {
            vec![]
        };
        // Note: For BUCK targets, it is either a single test file in
        // a test suite, or one or more files as an app target.
        // This is indicated by TargetType::ErlangTest for the former
        let (abs_src_dirs, extra_src_dirs) = match target.target_type {
            TargetType::ErlangApp | TargetType::ThirdParty => (
                target
                    .src_files
                    .iter()
                    .filter(|src| src.extension() == Some(ERL_EXT))
                    .filter_map(|src| src.parent())
                    .map(|dir| dir.to_path_buf())
                    .collect(),
                FxHashSet::default(),
            ),
            TargetType::ErlangTest | TargetType::ErlangTestUtils => (
                FxHashSet::default(),
                target
                    .src_files
                    .iter()
                    .filter(|src| src.extension() == Some(ERL_EXT))
                    .filter_map(|src| src.parent())
                    .map(|dir| dir.to_path_buf())
                    .filter_map(|d| {
                        d.strip_prefix(&target.dir.to_path_buf())
                            .map(|r| r.to_path_buf())
                    })
                    .map(|d| d.as_str().to_string())
                    .collect(),
            ),
        };
        let project_app_data = ProjectAppData {
            name: target.app_name.clone(),
            buck_target_name: Some(target.name.clone()),
            dir: target.dir.clone(),
            ebin: None,
            extra_src_dirs: extra_src_dirs.into_iter().collect(),
            include_dirs: target.include_files(),
            abs_src_dirs: abs_src_dirs.into_iter().collect(),
            macros,
            parse_transforms: vec![],
            app_type: target.app_type(),
            include_path: vec![],
            applicable_files: Some(FxHashSet::from_iter(target.src_files.clone())),
            is_test_target: Some(target.target_type == TargetType::ErlangTest),
        };
        // Do not bother with applications that exist purely as a
        // handle on dependencies for include_path.
        if !((target.src_files.is_empty() && project_app_data.extra_src_dirs.is_empty())
            && project_app_data.include_dirs.is_empty()
            && project_app_data.abs_src_dirs.is_empty())
        {
            result.push(project_app_data);
        }
    }

    (result, include_mapping)
}

fn include_path_from_file(path: &AbsPath) -> AbsPathBuf {
    // The bxl query returns directories already, do not parent unless it is a file
    let parent = if path.extension().is_some() {
        path.parent()
    } else {
        Some(path)
    };
    if let Some(parent) = parent {
        AbsPathBuf::assert_utf8(parent.into())
    } else {
        AbsPathBuf::assert_utf8(path.into())
    }
}

/// This is used in tests
pub fn get_prelude_cell(buck_config: &BuckConfig) -> Result<String> {
    let output = buck_config
        .buck_command()
        .arg("audit")
        .arg("cell")
        .arg("prelude")
        .output()?;
    if !output.status.success() {
        let reason = match output.status.code() {
            Some(code) => format!("Exited with status code: {code}"),
            None => "Process terminated by signal".to_string(),
        };
        let details = match String::from_utf8(output.stderr) {
            Ok(err) => err,
            Err(_) => "".to_string(),
        };
        bail!("Error evaluating Buck2 query Reason: {reason}. Details: {details}",);
    }
    let raw_output = String::from_utf8(output.stdout)?;

    lazy_static! {
        static ref RE: Regex = Regex::new(r"^prelude: ([^\s]+)").unwrap();
    }
    let string = RE
        .captures_iter(&raw_output)
        .next()
        .map(|c| c[1].to_string())
        .unwrap();
    Ok(string)
}

#[cfg(test)]
mod tests {

    use expect_test::Expect;
    use expect_test::expect;
    use paths::Utf8Path;

    use super::*;
    use crate::temp_dir::TempDir;
    use crate::test_fixture::FixtureWithProjectMeta;
    use crate::to_abs_path_buf;

    fn as_absolute_string(dir: &TempDir, path: &str) -> String {
        dir.path().join(path).to_string_lossy().to_string()
    }

    #[test]
    fn test_find_app_root_src() {
        let spec = r#"
        //- /app_a/src/app.erl
        //- /app_a/BUCK
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let root = AbsPath::assert(Utf8Path::from_path(dir.path()).unwrap());
        let target_name = "cell//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            app_name: None,
            suite: None,
            srcs: vec![as_absolute_string(&dir, "app_a/src/app.erl")],
            includes: vec![],
            labels: FxHashSet::default(),
            deps: vec![],
            apps: vec![],
            included_apps: vec![],
            extra_includes: vec![],
            origin: BuckTargetOrigin::App,
        };

        let actual = find_app_root_bxl(root, &target_name, &target);
        let expected = Some(to_abs_path_buf(&dir.path().join("app_a/src").to_path_buf()).unwrap());
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_find_app_root_include() {
        let spec = r#"
        //- /app_a/include/app.hrl
        //- /app_a/BUCK
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let root = AbsPath::assert(Utf8Path::from_path(dir.path()).unwrap());
        let target_name = "cell//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            app_name: None,
            suite: None,
            srcs: vec![],
            includes: vec![as_absolute_string(&dir, "app_a/include/app.hrl")],
            labels: FxHashSet::default(),
            deps: vec![],
            apps: vec![],
            included_apps: vec![],
            extra_includes: vec![],
            origin: BuckTargetOrigin::App,
        };

        let actual = find_app_root_bxl(root, &target_name, &target);
        let expected = Some(to_abs_path_buf(&dir.path().join("app_a/include")).unwrap());
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_find_app_root_suite() {
        let spec = r#"
        //- /app_a/test/app_SUITE.erl
        //- /app_a/BUCK
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let root = AbsPath::assert(Utf8Path::from_path(dir.path()).unwrap());
        let target_name = "cell//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            app_name: None,
            suite: Some(as_absolute_string(&dir, "app_a/test/app_SUITE.erl")),
            srcs: vec![],
            includes: vec![],
            labels: FxHashSet::default(),
            deps: vec![],
            apps: vec![],
            included_apps: vec![],
            extra_includes: vec![],
            origin: BuckTargetOrigin::App,
        };

        let actual = find_app_root_bxl(root, &target_name, &target);
        let expected = Some(to_abs_path_buf(&dir.path().join("app_a/test")).unwrap());
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_find_app_root_multiple_src() {
        let spec = r#"
        //- /app_a/src/app.erl
        //- /app_a/entity/entity.erl
        //- /app_a/BUCK
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let root = AbsPath::assert(Utf8Path::from_path(dir.path()).unwrap());
        let target_name = "cell//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            app_name: None,
            suite: None,
            srcs: vec![
                "cell//app_a/entity/entity.erl".to_string(),
                "cell//app_a/src/app.erl".to_string(),
            ],
            includes: vec![],
            labels: FxHashSet::default(),
            deps: vec![],
            apps: vec![],
            included_apps: vec![],
            extra_includes: vec![],
            origin: BuckTargetOrigin::App,
        };

        let actual = find_app_root_bxl(root, &target_name, &target);
        let expected = Some(to_abs_path_buf(&dir.path().join("app_a")).unwrap());
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_find_app_root_no_structure_buck_inside() {
        let spec = r#"
        //- /app_a/app.erl
        //- /app_a/app.hrl
        //- /app_a/BUCK
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let root = AbsPath::assert(Utf8Path::from_path(dir.path()).unwrap());
        let target_name = "cell//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            app_name: None,
            suite: None,
            srcs: vec!["cell//app_a/app.erl".to_string()],
            includes: vec!["cell//app_a/app.hrl".to_string()],
            labels: FxHashSet::default(),
            deps: vec![],
            apps: vec![],
            included_apps: vec![],
            extra_includes: vec![],
            origin: BuckTargetOrigin::App,
        };

        let actual = find_app_root_bxl(root, &target_name, &target);
        let expected = Some(to_abs_path_buf(&dir.path().join("app_a")).unwrap());
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_find_app_root_no_structure_buck_outside() {
        let spec = r#"
        //- /app_a/sub/app.erl
        //- /app_a/sub/app.hrl
        //- /app_a/BUCK
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let root = AbsPath::assert(Utf8Path::from_path(dir.path()).unwrap());
        let target_name = "cell//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            app_name: None,
            suite: None,
            srcs: vec![as_absolute_string(&dir, "app_a/sub/app.erl")],
            includes: vec![as_absolute_string(&dir, "app_a/sub/app.hrl")],
            labels: FxHashSet::default(),
            deps: vec![],
            apps: vec![],
            included_apps: vec![],
            extra_includes: vec![],
            origin: BuckTargetOrigin::App,
        };

        let actual = find_app_root_bxl(root, &target_name, &target);
        let expected = Some(to_abs_path_buf(&dir.path().join("app_a/sub")).unwrap());
        assert_eq!(expected, actual)
    }

    // TODO: enable when buck is properly set up on github project
    // @fb-only
    const BUCK_TESTS_ENABLED: bool = false; // @oss-only

    #[track_caller]
    fn check_buck_bxl_query(build_generated: bool, expect: Expect) {
        if BUCK_TESTS_ENABLED {
            let buck_root = to_abs_path_buf(&std::env::current_dir().unwrap()).unwrap();
            // We only need buck_config to get the buck command, everything but the buck root is ignored.
            let buck_config = BuckConfig {
                config_path: None,
                buck_root: Some(buck_root),
                enabled: true,
                deps_target: None,
                deps_targets: vec![],
                build_deps: false,
                included_targets: vec![],
                excluded_targets: vec![],
                source_root: None,
            };
            let generated_args = if build_generated {
                vec!["--build_generated_code", "true"]
            } else {
                vec![]
            };
            let output = buck_config
                .buck_command()
                .arg("bxl")
                .arg("prelude//erlang/elp.bxl:elp_config")
                .arg("--")
                .args(generated_args)
                .arg("--included_targets")
                .arg("fbcode//whatsapp/elp/test_projects/buck_tests_2/auto_gen/...")
                .output()
                .unwrap();
            if !output.status.success() {
                panic!("{output:#?}");
            }
            let string = String::from_utf8(output.stdout).unwrap();
            let prelude_cell = get_prelude_cell(&buck_config).expect("could not get prelude");
            let string = string.replace(&prelude_cell, "/[prelude]/");

            let to_replace = env!("CARGO_WORKSPACE_DIR");
            let string = string.replace(to_replace, "/[..]/");
            expect.assert_eq(&string);
        }
    }

    #[test]
    #[ignore]
    fn build_info_buck_bxl_query() {
        if BUCK_TESTS_ENABLED {
            check_buck_bxl_query(
                false,
                expect![[r#"
                {
                  "fbcode//whatsapp/elp/test_projects/buck_tests_2/auto_gen/auto_gen_a:auto_gen_a": {
                    "name": "auto_gen_a",
                    "app_name": null,
                    "suite": null,
                    "srcs": [
                      "/[..]/test_projects/buck_tests_2/auto_gen/auto_gen_a/src/auto_gen_a.erl"
                    ],
                    "includes": [
                      "/[..]/test_projects/buck_tests_2/auto_gen/auto_gen_a/include"
                    ],
                    "labels": [
                      "user_application"
                    ],
                    "deps": [],
                    "apps": [],
                    "included_apps": [],
                    "origin": "app"
                  },
                  "fbcode//whatsapp/elp/test_projects/buck_tests_2/auto_gen/auto_gen_a:generated_srcs": {
                    "name": "generated_srcs",
                    "app_name": null,
                    "suite": null,
                    "srcs": [],
                    "includes": [],
                    "labels": [
                      "user_application"
                    ],
                    "deps": [],
                    "apps": [],
                    "included_apps": [],
                    "origin": "app"
                  },
                  "prelude//erlang/common_test/cth_hooks:cth_hooks": {
                    "name": "cth_hooks",
                    "app_name": null,
                    "suite": null,
                    "srcs": [
                      "/[prelude]//erlang/common_test/cth_hooks/src/cth_tpx.erl",
                      "/[prelude]//erlang/common_test/cth_hooks/src/cth_tpx_role.erl",
                      "/[prelude]//erlang/common_test/cth_hooks/src/cth_tpx_server.erl",
                      "/[prelude]//erlang/common_test/cth_hooks/src/cth_tpx_test_tree.erl",
                      "/[prelude]//erlang/common_test/cth_hooks/src/method_ids.hrl"
                    ],
                    "includes": [],
                    "labels": [],
                    "deps": [],
                    "apps": [
                      "prelude//erlang/applications:kernel",
                      "prelude//erlang/applications:stdlib",
                      "prelude//erlang/applications:common_test"
                    ],
                    "included_apps": [],
                    "origin": "dep"
                  },
                  "prelude//erlang/common_test/common:common": {
                    "name": "common",
                    "app_name": null,
                    "suite": null,
                    "srcs": [
                      "/[prelude]//erlang/common_test/common/src/artifact_annotations.erl",
                      "/[prelude]//erlang/common_test/common/src/bounded_buffer.erl",
                      "/[prelude]//erlang/common_test/common/src/buck_ct_parser.erl",
                      "/[prelude]//erlang/common_test/common/src/buck_ct_provider.erl",
                      "/[prelude]//erlang/common_test/common/src/ct_error_printer.erl",
                      "/[prelude]//erlang/common_test/common/src/io_buffer.erl",
                      "/[prelude]//erlang/common_test/common/src/test_artifact_directory.erl",
                      "/[prelude]//erlang/common_test/common/src/test_logger.erl"
                    ],
                    "includes": [
                      "/[prelude]//erlang/common_test/common/include"
                    ],
                    "labels": [],
                    "deps": [],
                    "apps": [
                      "prelude//erlang/applications:kernel",
                      "prelude//erlang/applications:stdlib"
                    ],
                    "included_apps": [],
                    "origin": "dep"
                  },
                  "prelude//erlang/common_test/test_exec:test_exec": {
                    "name": "test_exec",
                    "app_name": null,
                    "suite": null,
                    "srcs": [
                      "/[prelude]//erlang/common_test/test_exec/src/ct_daemon.erl",
                      "/[prelude]//erlang/common_test/test_exec/src/ct_daemon_core.erl",
                      "/[prelude]//erlang/common_test/test_exec/src/ct_daemon_hooks.erl",
                      "/[prelude]//erlang/common_test/test_exec/src/ct_daemon_logger.erl",
                      "/[prelude]//erlang/common_test/test_exec/src/ct_daemon_node.erl",
                      "/[prelude]//erlang/common_test/test_exec/src/ct_daemon_printer.erl",
                      "/[prelude]//erlang/common_test/test_exec/src/ct_daemon_runner.erl",
                      "/[prelude]//erlang/common_test/test_exec/src/ct_executor.erl",
                      "/[prelude]//erlang/common_test/test_exec/src/ct_runner.erl",
                      "/[prelude]//erlang/common_test/test_exec/src/epmd_manager.erl",
                      "/[prelude]//erlang/common_test/test_exec/src/test_exec.erl",
                      "/[prelude]//erlang/common_test/test_exec/src/test_exec_sup.erl"
                    ],
                    "includes": [],
                    "labels": [],
                    "deps": [],
                    "apps": [
                      "prelude//erlang/applications:kernel",
                      "prelude//erlang/applications:stdlib",
                      "prelude//erlang/applications:debugger",
                      "prelude//erlang/common_test/common:common",
                      "prelude//erlang/common_test/cth_hooks:cth_hooks"
                    ],
                    "included_apps": [],
                    "origin": "dep"
                  },
                  "prelude//erlang/toolchain:toolchain_json": {
                    "name": "toolchain_json",
                    "app_name": null,
                    "suite": null,
                    "srcs": [
                      "/[prelude]//erlang/toolchain/json.erl"
                    ],
                    "includes": [],
                    "labels": [
                      "otp_compatibility_polyfill_application"
                    ],
                    "deps": [],
                    "apps": [
                      "prelude//erlang/applications:kernel",
                      "prelude//erlang/applications:stdlib"
                    ],
                    "included_apps": [],
                    "origin": "dep"
                  },
                  "prelude//erlang/shell:buck2_shell_utils": {
                    "name": "buck2_shell_utils",
                    "app_name": null,
                    "suite": null,
                    "srcs": [
                      "/[prelude]//erlang/shell/src/shell_buck2_module_search.erl",
                      "/[prelude]//erlang/shell/src/shell_buck2_utils.erl",
                      "/[prelude]//erlang/shell/src/user_default.erl"
                    ],
                    "includes": [],
                    "labels": [],
                    "deps": [],
                    "apps": [
                      "prelude//erlang/applications:kernel",
                      "prelude//erlang/applications:stdlib",
                      "prelude//erlang/toolchain:toolchain_json"
                    ],
                    "included_apps": [
                      "prelude//erlang/common_test/test_exec:test_exec"
                    ],
                    "origin": "dep"
                  }
                }
            "#]],
            );
        }
    }

    #[test]
    #[ignore]
    fn build_info_buck_bxl_generated_query() {
        if BUCK_TESTS_ENABLED {
            // Note that there is now a value for `srcs` in the
            // "fbcode//whatsapp/elp/test_projects/buck_tests_2/auto_gen/auto_gen_a:generated_srcs"
            // target
            check_buck_bxl_query(
                true,
                expect![[r#"
                    {
                      "fbcode//whatsapp/elp/test_projects/buck_tests_2/auto_gen/auto_gen_a:auto_gen_a": {
                        "name": "auto_gen_a",
                        "app_name": null,
                        "suite": null,
                        "srcs": [
                          "/[..]/test_projects/buck_tests_2/auto_gen/auto_gen_a/src/auto_gen_a.erl"
                        ],
                        "includes": [
                          "/[..]/test_projects/buck_tests_2/auto_gen/auto_gen_a/include"
                        ],
                        "labels": [
                          "user_application"
                        ],
                        "deps": [],
                        "apps": [],
                        "included_apps": [],
                        "origin": "app"
                      },
                      "fbcode//whatsapp/elp/test_projects/buck_tests_2/auto_gen/auto_gen_a:generated_srcs": {
                        "name": "generated_srcs",
                        "app_name": null,
                        "suite": null,
                        "srcs": [
                          "/[..]/test_projects/buck_tests_2/auto_gen/auto_gen_a/out/pretend_generated.erl"
                        ],
                        "includes": [],
                        "labels": [
                          "user_application"
                        ],
                        "deps": [],
                        "apps": [],
                        "included_apps": [],
                        "origin": "app"
                      },
                      "prelude//erlang/common_test/cth_hooks:cth_hooks": {
                        "name": "cth_hooks",
                        "app_name": null,
                        "suite": null,
                        "srcs": [
                          "/[prelude]//erlang/common_test/cth_hooks/src/cth_tpx.erl",
                          "/[prelude]//erlang/common_test/cth_hooks/src/cth_tpx_role.erl",
                          "/[prelude]//erlang/common_test/cth_hooks/src/cth_tpx_server.erl",
                          "/[prelude]//erlang/common_test/cth_hooks/src/cth_tpx_test_tree.erl",
                          "/[prelude]//erlang/common_test/cth_hooks/src/method_ids.hrl"
                        ],
                        "includes": [],
                        "labels": [],
                        "deps": [],
                        "apps": [
                          "prelude//erlang/applications:kernel",
                          "prelude//erlang/applications:stdlib",
                          "prelude//erlang/applications:common_test"
                        ],
                        "included_apps": [],
                        "origin": "dep"
                      },
                      "prelude//erlang/common_test/common:common": {
                        "name": "common",
                        "app_name": null,
                        "suite": null,
                        "srcs": [
                          "/[prelude]//erlang/common_test/common/src/artifact_annotations.erl",
                          "/[prelude]//erlang/common_test/common/src/bounded_buffer.erl",
                          "/[prelude]//erlang/common_test/common/src/buck_ct_parser.erl",
                          "/[prelude]//erlang/common_test/common/src/buck_ct_provider.erl",
                          "/[prelude]//erlang/common_test/common/src/ct_error_printer.erl",
                          "/[prelude]//erlang/common_test/common/src/io_buffer.erl",
                          "/[prelude]//erlang/common_test/common/src/test_artifact_directory.erl",
                          "/[prelude]//erlang/common_test/common/src/test_logger.erl"
                        ],
                        "includes": [
                          "/[prelude]//erlang/common_test/common/include"
                        ],
                        "labels": [],
                        "deps": [],
                        "apps": [
                          "prelude//erlang/applications:kernel",
                          "prelude//erlang/applications:stdlib"
                        ],
                        "included_apps": [],
                        "origin": "dep"
                      },
                      "prelude//erlang/common_test/test_exec:test_exec": {
                        "name": "test_exec",
                        "app_name": null,
                        "suite": null,
                        "srcs": [
                          "/[prelude]//erlang/common_test/test_exec/src/ct_daemon.erl",
                          "/[prelude]//erlang/common_test/test_exec/src/ct_daemon_core.erl",
                          "/[prelude]//erlang/common_test/test_exec/src/ct_daemon_hooks.erl",
                          "/[prelude]//erlang/common_test/test_exec/src/ct_daemon_logger.erl",
                          "/[prelude]//erlang/common_test/test_exec/src/ct_daemon_node.erl",
                          "/[prelude]//erlang/common_test/test_exec/src/ct_daemon_printer.erl",
                          "/[prelude]//erlang/common_test/test_exec/src/ct_daemon_runner.erl",
                          "/[prelude]//erlang/common_test/test_exec/src/ct_executor.erl",
                          "/[prelude]//erlang/common_test/test_exec/src/ct_runner.erl",
                          "/[prelude]//erlang/common_test/test_exec/src/epmd_manager.erl",
                          "/[prelude]//erlang/common_test/test_exec/src/test_exec.erl",
                          "/[prelude]//erlang/common_test/test_exec/src/test_exec_sup.erl"
                        ],
                        "includes": [],
                        "labels": [],
                        "deps": [],
                        "apps": [
                          "prelude//erlang/applications:kernel",
                          "prelude//erlang/applications:stdlib",
                          "prelude//erlang/applications:debugger",
                          "prelude//erlang/common_test/common:common",
                          "prelude//erlang/common_test/cth_hooks:cth_hooks"
                        ],
                        "included_apps": [],
                        "origin": "dep"
                      },
                      "prelude//erlang/toolchain:toolchain_json": {
                        "name": "toolchain_json",
                        "app_name": null,
                        "suite": null,
                        "srcs": [
                          "/[prelude]//erlang/toolchain/json.erl"
                        ],
                        "includes": [],
                        "labels": [
                          "otp_compatibility_polyfill_application"
                        ],
                        "deps": [],
                        "apps": [
                          "prelude//erlang/applications:kernel",
                          "prelude//erlang/applications:stdlib"
                        ],
                        "included_apps": [],
                        "origin": "dep"
                      },
                      "prelude//erlang/shell:buck2_shell_utils": {
                        "name": "buck2_shell_utils",
                        "app_name": null,
                        "suite": null,
                        "srcs": [
                          "/[prelude]//erlang/shell/src/shell_buck2_module_search.erl",
                          "/[prelude]//erlang/shell/src/shell_buck2_utils.erl",
                          "/[prelude]//erlang/shell/src/user_default.erl"
                        ],
                        "includes": [],
                        "labels": [],
                        "deps": [],
                        "apps": [
                          "prelude//erlang/applications:kernel",
                          "prelude//erlang/applications:stdlib",
                          "prelude//erlang/toolchain:toolchain_json"
                        ],
                        "included_apps": [
                          "prelude//erlang/common_test/test_exec:test_exec"
                        ],
                        "origin": "dep"
                      }
                    }
                "#]],
            );
        }
    }

    #[test]
    fn test_buck_path_to_abs_path() {
        let root = AbsPath::assert(Utf8Path::new("/blah"));
        expect![[r#"
            Ok(
                AbsPathBuf(
                    "/blah/foo/bar",
                ),
            )
        "#]]
        .assert_debug_eq(&buck_path_to_abs_path(root, "cell//foo/bar"));

        expect![[r#"
            Ok(
                AbsPathBuf(
                    "/blah/foo/bar",
                ),
            )
        "#]]
        .assert_debug_eq(&buck_path_to_abs_path(root, "//foo/bar"));

        expect![[r#"
            Ok(
                AbsPathBuf(
                    "/blah/foo/bar",
                ),
            )
        "#]]
        .assert_debug_eq(&buck_path_to_abs_path(root, "foo/bar"))
    }

    #[test]
    fn new_buck_query_error() {
        expect![[r#"
            BuckQueryError {
                command: "command",
                reason: "reason",
                details: "Buck UI: https://a.b.com/buck2/ref-hash\nblah",
                buck_ui_url: Some(
                    "https://a.b.com/buck2/ref-hash",
                ),
            }
        "#]]
        .assert_debug_eq(&BuckQueryError::new(
            "command".to_string(),
            "reason".to_string(),
            "Buck UI: https://a.b.com/buck2/ref-hash\nblah".to_string(),
        ));
    }
}

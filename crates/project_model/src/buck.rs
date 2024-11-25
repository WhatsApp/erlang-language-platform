/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

extern crate serde;
extern crate serde_json;

use core::str;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::convert::TryFrom;
use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;

use anyhow::bail;
use anyhow::Result;
use eetf::Term;
use eetf::Term::Atom;
use elp_log::timeit;
use fxhash::FxHashMap;
use fxhash::FxHashSet;
use indexmap::indexset;
use itertools::Itertools;
use lazy_static::lazy_static;
use parking_lot::Mutex;
use paths::AbsPath;
use paths::AbsPathBuf;
use paths::RelPathBuf;
use paths::Utf8Path;
use paths::Utf8PathBuf;
use serde::Deserialize;
use serde::Serialize;

use crate::otp::Otp;
use crate::AppName;
use crate::AppType;
use crate::CommandProxy;
use crate::ElpConfig;
use crate::ProjectAppData;
use crate::ProjectModelError;

pub type TargetFullName = String;

lazy_static! {
    static ref DIRS: Vec<RelPathBuf> = vec!["src", "test", "include"]
        .into_iter()
        .flat_map(|dir| dir.try_into())
        .collect();
}

const ERL_EXT: &str = "erl";
const TEST_DIR: &str = "test";

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
    pub(crate) buck_root: Option<AbsPathBuf>,
    pub enabled: bool,
    pub deps_target: Option<String>,
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

    pub fn source_root(&self) -> Cow<AbsPathBuf> {
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

impl BuckProject {
    pub fn load_from_config(
        buck_conf: &BuckConfig,
        query_config: &BuckQueryConfig,
    ) -> Result<(BuckProject, Vec<ProjectAppData>, Utf8PathBuf), anyhow::Error> {
        let (otp_root, project_app_data, project) = if query_config == &BuckQueryConfig::Original {
            load_from_config_orig(buck_conf)?
        } else {
            load_from_config_bxl(buck_conf)?
        };
        Ok((project, project_app_data, otp_root))
    }

    pub fn target(&self, file_path: &AbsPathBuf) -> Option<String> {
        self.target_info.path_to_target_name.get(file_path).cloned()
    }
}

fn load_from_config_orig(
    buck_conf: &BuckConfig,
) -> Result<(Utf8PathBuf, Vec<ProjectAppData>, BuckProject), anyhow::Error> {
    let target_info = load_buck_targets_orig(buck_conf)?;
    let otp_root = Otp::find_otp()?;
    let project_app_data = targets_to_project_data_orig(&target_info.targets, &otp_root);
    let project = BuckProject {
        target_info,
        buck_conf: buck_conf.clone(),
    };
    Ok((otp_root, project_app_data, project))
}

fn load_from_config_bxl(
    buck_conf: &BuckConfig,
) -> Result<(Utf8PathBuf, Vec<ProjectAppData>, BuckProject), anyhow::Error> {
    let target_info = load_buck_targets_bxl(buck_conf)?;
    let otp_root = Otp::find_otp()?;
    let project_app_data = targets_to_project_data_bxl(&target_info.targets, &otp_root);
    let project = BuckProject {
        target_info,
        buck_conf: buck_conf.clone(),
    };
    Ok((otp_root, project_app_data, project))
}

#[derive(Deserialize, Debug)]
pub struct BuckTarget {
    name: String,
    //Some if target is test, in which case srcs will be empty
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
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Target {
    // full-name, like cell//path/to/target/...
    pub name: TargetFullName,
    pub app_name: String,
    pub dir: AbsPathBuf,
    pub src_files: Vec<AbsPathBuf>,
    pub include_files: Vec<AbsPathBuf>,
    pub deps: Vec<TargetFullName>,
    pub apps: Vec<TargetFullName>,
    pub included_apps: Vec<TargetFullName>,
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

fn load_buck_targets_bxl(buck_config: &BuckConfig) -> Result<TargetInfo> {
    let _timer = timeit!("loading info from buck");
    let root = buck_config.buck_root();

    let mut dep_path = if buck_config.build_deps {
        build_third_party_targets(buck_config)?
    } else {
        FxHashMap::default()
    };
    let buck_targets = query_buck_targets(buck_config, &BuckQueryConfig::Bxl)?;

    let mut target_info = TargetInfo::default();
    for (name, target) in buck_targets {
        let mut private_header = false;

        let dir = match find_app_root_bxl(root, &name, &target) {
            None => continue,
            Some(dir) => dir,
        };

        let (src_files, include_files, target_type, private_header, ebin) =
            if let Some(ref suite) = target.suite {
                let src_file = buck_path_to_abs_path(root, &suite)?;
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
                let target_type = compute_target_type(&name, &target);
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
                    let inc = buck_path_to_abs_path(root, include).unwrap();
                    include_files.push(inc);
                }

                let ebin = match target_type {
                    TargetType::ThirdParty if buck_config.build_deps => dep_path
                        .remove(&name)
                        .map(|dir| dir.join(Utf8PathBuf::from("ebin"))),
                    TargetType::ThirdParty => Some(dir.clone()),
                    _ => None,
                };
                (src_files, include_files, target_type, private_header, ebin)
            };
        let target = Target {
            name: name.clone(),
            app_name: target.name,
            dir,
            src_files,
            include_files,
            deps: target.deps,
            apps: target.apps,
            included_apps: target.included_apps,
            ebin,
            target_type,
            private_header,
        };
        target_info.targets.insert(name, target);
    }
    Ok(target_info)
}

fn load_buck_targets_orig(buck_config: &BuckConfig) -> Result<TargetInfo> {
    let _timer = timeit!("loading info from buck");
    let root = buck_config.buck_root();

    let mut dep_path = if buck_config.build_deps {
        build_third_party_targets(buck_config)?
    } else {
        FxHashMap::default()
    };
    let buck_targets = query_buck_targets(buck_config, &BuckQueryConfig::Original)?;

    let mut target_info = TargetInfo::default();
    for (name, target) in buck_targets {
        let mut private_header = false;

        let dir = match find_app_root_orig(root, &name, &target) {
            None => continue,
            Some(dir) => dir,
        };

        let (src_files, include_files, target_type, private_header, ebin) =
            if let Some(suite) = target.suite {
                let src_file = buck_path_to_abs_path(root, &suite)?;
                let src = vec![src_file.clone()];
                target_info
                    .path_to_target_name
                    .insert(src_file, name.clone());
                (src, vec![], TargetType::ErlangTest, false, None)
            } else {
                let target_type = compute_target_type(&name, &target);
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
                    let inc = buck_path_to_abs_path(root, include).unwrap();
                    include_files.push(inc);
                }

                let ebin = match target_type {
                    TargetType::ThirdParty if buck_config.build_deps => dep_path
                        .remove(&name)
                        .map(|dir| dir.join(Utf8PathBuf::from("ebin"))),
                    TargetType::ThirdParty => Some(dir.clone()),
                    _ => None,
                };
                (src_files, include_files, target_type, private_header, ebin)
            };
        let target = Target {
            name: name.clone(),
            app_name: target.name,
            dir,
            src_files,
            include_files,
            apps: target.apps,
            deps: target.deps,
            included_apps: target.included_apps,
            ebin,
            target_type,
            private_header,
        };
        target_info.targets.insert(name, target);
    }
    Ok(target_info)
}

fn compute_target_type(name: &TargetFullName, target: &BuckTarget) -> TargetType {
    if name.contains("//third-party") {
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
            log::error!("Err executing buck2 root: {:?}", err);
            bail!(crate::ProjectModelError::MissingBuck(err))
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
    Original,
    Bxl,
}

fn query_buck_targets(
    buck_config: &BuckConfig,
    query_config: &BuckQueryConfig,
) -> Result<FxHashMap<TargetFullName, BuckTarget>> {
    let _timer = timeit!("load buck targets");
    let result = query_buck_targets_raw(buck_config, query_config)?;

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

pub fn query_buck_targets_raw(
    buck_config: &BuckConfig,
    query_config: &BuckQueryConfig,
) -> Result<FxHashMap<String, BuckTarget>> {
    match query_config {
        BuckQueryConfig::Original => query_buck_targets_orig(buck_config),
        BuckQueryConfig::Bxl => query_buck_targets_bxl(buck_config),
    }
}

fn query_buck_targets_orig(buck_config: &BuckConfig) -> Result<FxHashMap<String, BuckTarget>> {
    let mut kinds = String::new();
    for target in &buck_config.included_targets {
        if kinds.is_empty() {
            kinds = format!(
                "kind(erlang_test$, {}) + kind(erlang_app$, {})",
                target, target
            );
        } else {
            kinds.push_str(
                format!(
                    "+ kind(erlang_test$, {}) + kind(erlang_app$, {})",
                    target, target
                )
                .as_str(),
            );
        }
    }
    if let Some(deps_target) = &buck_config.deps_target {
        if kinds.is_empty() {
            kinds = format!("kind(erlang_app$, {})", deps_target);
        } else {
            kinds.push_str(format!("+ kind(erlang_app$, {})", deps_target).as_str());
        }
    }
    let output = buck_config
        .buck_command()
        .arg("uquery")
        .arg("--config=client.id=elp")
        .arg("--json")
        .arg(kinds)
        .arg("--output-attribute")
        .arg("suite")
        .arg("--output-attribute")
        .arg("^includes")
        .arg("--output-attribute")
        .arg("srcs")
        .arg("--output-attribute")
        .arg("name")
        .arg("--output-attribute")
        .arg("labels")
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
        bail!(
            "Error evaluating Buck2 query. This is often due to an incorrect BUCK file. Reason: {reason}. Details: {details}",
        );
    }
    let string = String::from_utf8(output.stdout)?;
    let result: FxHashMap<TargetFullName, BuckTarget> = serde_json::from_str(&string)?;
    Ok(result)
}

fn query_buck_targets_bxl(buck_config: &BuckConfig) -> Result<FxHashMap<String, BuckTarget>> {
    let mut targets = Vec::default();
    for target in &buck_config.included_targets {
        targets.push("--included_targets");
        targets.push(target);
    }
    if let Some(deps_target) = &buck_config.deps_target {
        targets.push("--deps_target");
        targets.push(deps_target);
    }
    let output = buck_config
        .buck_command()
        .arg("bxl")
        .arg("--config=client.id=elp")
        .arg("prelude//erlang/elp.bxl:elp_config")
        .arg("--")
        .args(targets)
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
        bail!(
            "Error evaluating Buck2 query. This is often due to an incorrect BUCK file. Reason: {reason}. Details: {details}",
        );
    }
    let string = String::from_utf8(output.stdout)?;
    let result: FxHashMap<TargetFullName, BuckTarget> = serde_json::from_str(&string)?;
    Ok(result)
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
        for path in set {
            return Some(path);
        }
        // Otherwise, we just return the directory containing the BUCK file,
        Some(dir_based_on_buck_file)
    }
}

fn find_app_root_orig(
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
        if let Ok(path) = buck_path_to_abs_path(root, path) {
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
        for path in set {
            if let Some(path) = examine_path(&path, dir_based_on_buck_file.as_path()) {
                // We found an src/, test/, or include/ directory before the BUCK file,
                // so we just return the parent of this directory.
                return Some(path);
            }
        }
        // Otherwise, we just return the directory containing the BUCK file,
        Some(dir_based_on_buck_file)
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

fn examine_path(path: &AbsPath, dir_based_on_buck_file: &AbsPath) -> Option<AbsPathBuf> {
    let mut parent_it = Some(path);
    while let Some(parent_in_dirs) = parent_it {
        for dir in DIRS.iter() {
            if parent_in_dirs.ends_with(dir) {
                return parent_in_dirs.parent().map(|p| p.to_path_buf());
            }
        }
        //optimization to stop on BUCK file location and not go to the /
        if dir_based_on_buck_file == parent_in_dirs {
            break;
        }
        parent_it = parent_in_dirs.parent();
    }
    None
}

fn targets_to_project_data_orig(
    targets: &FxHashMap<TargetFullName, Target>,
    otp_root: &Utf8Path,
) -> Vec<ProjectAppData> {
    let it = targets
        .values()
        .sorted_by(|a, b| match (a.target_type, b.target_type) {
            (TargetType::ErlangTest, TargetType::ErlangTest) => Ordering::Equal,
            (TargetType::ErlangTest, _) => Ordering::Greater,
            _ => Ordering::Equal,
        });

    let mut accs: FxHashMap<AbsPathBuf, ProjectAppDataAcc> = Default::default();
    for target in it {
        let mut target_dir = target.dir.clone();
        let acc = accs.remove(&target_dir);
        let mut acc = match acc {
            Some(acc) => acc,
            None => {
                let search_first = match target.target_type {
                    TargetType::ErlangApp | TargetType::ThirdParty => false,
                    _ => true,
                };
                let mut result = None;
                if search_first {
                    let dir = accs.keys().find(|dir| target.dir.starts_with(dir));
                    if let Some(dir) = dir {
                        let dir = dir.clone();
                        result = accs.remove(&dir);
                        target_dir = dir;
                    }
                }
                result.unwrap_or_else(ProjectAppDataAcc::new)
            }
        };

        acc.add(target);
        accs.insert(target_dir, acc);
    }
    let mut result: Vec<ProjectAppData> = vec![];
    let mut global_inc: Vec<AbsPathBuf> = targets
        .values()
        .filter(|target| target.target_type != TargetType::ErlangTest)
        .filter_map(|target| target.dir.parent().map(|p| p.to_path_buf()))
        .collect();
    global_inc.push(AbsPathBuf::assert(otp_root.to_path_buf()));
    for (_, mut acc) in accs {
        acc.add_global_includes(global_inc.clone());
        result.push(acc.into());
    }

    result
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum IsCached {
    Yes,
    No,
}

fn targets_to_project_data_bxl(
    targets: &FxHashMap<TargetFullName, Target>,
    otp_root: &Utf8Path,
) -> Vec<ProjectAppData> {
    let mut result: Vec<ProjectAppData> = vec![];
    let mut includes_cache: FxHashMap<TargetFullName, (IsCached, &Target, FxHashSet<AbsPathBuf>)> =
        FxHashMap::default();
    for (_target_full_name, target) in targets {
        let mut includes = FxHashSet::from_iter(
            target
                .include_files
                .iter()
                .map(|inc| include_path_from_file(inc)),
        );
        if target.private_header {
            target.src_files.iter().for_each(|path| {
                if Some("hrl") == path.extension() {
                    includes.insert(include_path_from_file(path));
                }
            });
        }
        includes_cache.insert(target.name.clone(), (IsCached::No, &target, includes));
    }
    let otp_includes = FxHashSet::from_iter(vec![AbsPathBuf::assert(otp_root.to_path_buf())]);
    for (target_full_name, target) in targets {
        let includes = apps_and_deps_includes(&mut includes_cache, target_full_name, &otp_includes);

        let macros = if target.target_type != TargetType::ThirdParty {
            vec![Atom("TEST".into()), Atom("COMMON_TEST".into())]
        } else {
            vec![]
        };
        let abs_src_dirs: FxHashSet<_> = target
            .src_files
            .iter()
            .filter(|src| src.extension() == Some(&ERL_EXT))
            .filter_map(|src| src.parent())
            .map(|dir| dir.to_path_buf())
            .filter(|dir| dir.file_name() != Some(&TEST_DIR))
            .collect();
        let extra_src_dirs: FxHashSet<String> = target
            .src_files
            .iter()
            .filter(|src| src.extension() == Some(&ERL_EXT))
            .filter_map(|src| src.parent())
            .map(|dir| dir.to_path_buf())
            .filter(|dir| dir.file_name() == Some(&TEST_DIR))
            .filter_map(|d| {
                d.strip_prefix(&target.dir.to_path_buf())
                    .map(|r| r.to_path_buf())
            })
            .map(|d| d.as_str().to_string())
            .collect();
        let project_app_data = ProjectAppData {
            name: AppName(target.app_name.clone()),
            dir: target.dir.clone(),
            ebin: None,
            extra_src_dirs: extra_src_dirs.into_iter().collect(),
            include_dirs: target.include_files(),
            abs_src_dirs: abs_src_dirs.into_iter().collect(),
            macros,
            parse_transforms: vec![],
            app_type: target.app_type(),
            include_path: includes.into_iter().collect(),
            applicable_files: Some(FxHashSet::from_iter(
                target
                    .src_files
                    .clone()
                    .into_iter()
                    .chain(target.include_files.clone()),
            )),
            is_test_target: Some(target.target_type == TargetType::ErlangTest),
        };
        result.push(project_app_data);
    }

    result
}

/// We have a map of the initial direct dependencies of each target,
/// each with a flag as to whether the extended dependency includes
/// have been calculated yet.  Look up the full includes for a target,
/// populating the cache with the recursive closure of its dependent
/// includes on the way.
fn apps_and_deps_includes(
    includes_cache: &mut FxHashMap<TargetFullName, (IsCached, &Target, FxHashSet<AbsPathBuf>)>,
    target_name: &TargetFullName,
    otp_include: &FxHashSet<AbsPathBuf>,
) -> FxHashSet<AbsPathBuf> {
    if let Some((is_cached, target, mut includes)) = includes_cache.get(target_name).cloned() {
        if is_cached == IsCached::Yes {
            includes.clone()
        } else {
            target
                .apps
                .iter()
                .chain(target.deps.iter())
                .chain(target.included_apps.iter())
                .for_each(|sub_target| {
                    let sub_includes =
                        apps_and_deps_includes(includes_cache, sub_target, otp_include);
                    includes.extend(sub_includes.into_iter())
                });
            includes_cache.insert(
                target_name.to_string(),
                (IsCached::Yes, target, includes.clone()),
            );
            includes
        }
    } else {
        otp_include.clone()
    }
}

fn include_path_from_file(path: &AbsPath) -> AbsPathBuf {
    // The bxl query returns directories already, do not parent unless it is a file
    let parent = if <AbsPath as AsRef<Utf8Path>>::as_ref(path).is_file() {
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

#[derive(Debug)]
struct ProjectAppDataAcc {
    // Fields directly for `ProjectAppData`
    pub name: Option<AppName>,
    pub dir: Option<AbsPathBuf>,
    pub ebin: Option<AbsPathBuf>,
    pub extra_src_dirs: FxHashSet<String>,
    pub include_dirs: FxHashSet<String>,
    pub abs_src_dirs: FxHashSet<AbsPathBuf>,
    pub macros: Vec<Term>,
    pub app_type: Option<AppType>,
    pub include_path: FxHashSet<AbsPathBuf>,

    // Fields not in `ProjectAppData`
    pub abs_extra_src_dirs: FxHashSet<AbsPathBuf>,
}

impl ProjectAppDataAcc {
    fn new() -> ProjectAppDataAcc {
        ProjectAppDataAcc {
            name: None,
            dir: None,
            ebin: None,
            extra_src_dirs: Default::default(),
            abs_extra_src_dirs: Default::default(),
            include_dirs: Default::default(),
            macros: vec![],
            app_type: None,
            include_path: Default::default(),
            abs_src_dirs: Default::default(),
        }
    }

    fn set_ebin(&mut self, target: &Target) {
        if let (Some(ebin), None) = (&target.ebin, &self.ebin) {
            self.ebin = Some(ebin.clone());
        }
    }

    fn set_app_type(&mut self, target: &Target) {
        if self.app_type.is_none() {
            self.app_type = Some(target.app_type())
        }
    }

    fn set_name(&mut self, target: &Target) {
        let dir_name = target.dir.file_name().map(|f| AppName(f.to_string()));
        let target_name = Some(AppName(target.app_name.clone()));
        if dir_name == target_name {
            self.name = target_name;
        } else {
            match (target.target_type, &self.name) {
                (TargetType::ErlangApp | TargetType::ThirdParty, None) => self.name = target_name,
                (_, None) => self.name = dir_name,
                _ => (),
            };
        }
    }

    fn set_dir(&mut self, target: &Target) {
        if self.dir.is_none() {
            self.dir = Some(target.dir.clone());
        }
    }

    fn add_src(&mut self, target: &Target) {
        let app_dir = match &self.dir {
            None => return,
            Some(dir) => dir,
        };
        match target.target_type {
            TargetType::ErlangApp | TargetType::ThirdParty => {
                let abs_src_dirs: FxHashSet<AbsPathBuf> = target
                    .src_files
                    .iter()
                    .filter(|src| src.extension() == Some(&ERL_EXT))
                    .filter_map(|src| src.parent())
                    .map(|dir| dir.to_path_buf())
                    .filter(|dir| dir.file_name() != Some(&TEST_DIR))
                    .collect();

                self.abs_src_dirs.extend(abs_src_dirs);
            }
            TargetType::ErlangTest | TargetType::ErlangTestUtils => {
                let abs_extra_dirs: FxHashSet<AbsPathBuf> = target
                    .src_files
                    .iter()
                    .filter(|src| src.extension() == Some(&ERL_EXT))
                    .filter_map(|extra| extra.parent())
                    .map(|extra| extra.to_path_buf())
                    .collect();

                let extra_src_dirs: Vec<String> = abs_extra_dirs
                    .iter()
                    .filter_map(|dir| dir.strip_prefix(app_dir))
                    .map(|dir| dir.as_str().to_string())
                    .collect();

                for abs_extra in &abs_extra_dirs {
                    self.add_include_if_not_exist(abs_extra);
                }

                self.extra_src_dirs.extend(extra_src_dirs);
                self.abs_extra_src_dirs.extend(abs_extra_dirs);
            }
        }
    }

    fn add_include_path(&mut self, target: &Target) {
        //we need it because some applications, like erlclient has tree structure inside the /src
        //and includes the files from top into the bottom files
        //for example erlclient/src/wrapper_handlers/erlclient_web_device_login_cookie.erl includes the
        //erlclient/src/erlclient.hrl
        if target.private_header {
            for src in &target.src_files {
                self.add_include_if_not_exist(src);
            }
        }

        for inc in &target.include_files {
            self.add_include_if_not_exist(inc);
        }
    }

    fn add_include_if_not_exist(&mut self, path: &AbsPath) {
        // The bxl query returns directories already, do not parent unless it is a file
        let include_dir = if <AbsPath as AsRef<Utf8Path>>::as_ref(path).is_file() {
            path.parent()
        } else {
            Some(path)
        };
        if let Some(include_dir) = include_dir {
            if !self.include_path.contains(include_dir) {
                self.include_path.insert(include_dir.to_path_buf());
            }
        }
    }

    fn add_global_includes(&mut self, includes: Vec<AbsPathBuf>) {
        self.include_path.extend(includes);
    }

    fn add_include(&mut self, target: &Target) {
        let it = target
            .include_files
            .iter()
            .filter_map(|inc| {
                let path: &Path = inc.as_ref();
                if path.is_file() {
                    inc.parent()
                } else {
                    Some(inc.deref())
                }
            })
            //in case of .*_test_utils target it is possible to have *.hrl file in test dir
            //which buck treats as include. This check prevents it in case we already added
            //the folder as test folder
            .filter(|inc| !self.abs_extra_src_dirs.contains(*inc))
            .filter_map(|inc| inc.strip_prefix(&target.dir))
            .map(|inc| inc.as_str().to_string());

        self.include_dirs.extend(it);
    }

    fn set_macro(&mut self, target: &Target) {
        if target.target_type != TargetType::ThirdParty && self.macros.is_empty() {
            self.macros.push(Atom("TEST".into()));
            self.macros.push(Atom("COMMON_TEST".into()));
        }
    }

    fn add(&mut self, target: &Target) {
        self.set_name(target);
        self.set_dir(target);
        self.set_app_type(target);
        self.set_ebin(target);
        self.set_macro(target);
        self.add_src(target);
        self.add_include(target);
        self.add_include_path(target);
    }
}

impl From<ProjectAppDataAcc> for ProjectAppData {
    fn from(acc: ProjectAppDataAcc) -> Self {
        let dir = acc.dir.unwrap();

        ProjectAppData {
            name: acc.name.unwrap(),
            dir: dir.clone(),
            ebin: acc.ebin,
            extra_src_dirs: acc.extra_src_dirs.into_iter().collect(),
            include_dirs: acc
                .include_dirs
                .into_iter()
                .map(|include| dir.join(include))
                .collect(),
            macros: acc.macros,
            parse_transforms: vec![],
            abs_src_dirs: acc.abs_src_dirs.into_iter().collect(),
            app_type: acc.app_type.unwrap(),
            // we sort to speed up parse-server
            // local folders goes first, than common, like /erl
            // TODO:AZ why only for buck?
            include_path: acc
                .include_path
                .into_iter()
                .sorted_by(|a, b| {
                    if a.starts_with(&dir) && !b.starts_with(&dir) {
                        return Ordering::Less;
                    }
                    if b.starts_with(&dir) && !a.starts_with(&dir) {
                        return Ordering::Greater;
                    }
                    let len1 = a.as_os_str().len();
                    let len2 = b.as_os_str().len();
                    len1.cmp(&len2)
                })
                .collect(),
            applicable_files: None,
            is_test_target: None,
        }
    }
}

#[cfg(test)]
mod tests {

    use expect_test::expect;
    use expect_test::Expect;

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
        let root = AbsPath::assert(&Utf8Path::from_path(dir.path()).unwrap());
        let target_name = "cell//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            suite: None,
            srcs: vec![as_absolute_string(&dir, "app_a/src/app.erl")],
            includes: vec![],
            labels: FxHashSet::default(),
            deps: vec![],
            apps: vec![],
            included_apps: vec![],
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
        let root = AbsPath::assert(&Utf8Path::from_path(dir.path()).unwrap());
        let target_name = "cell//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            suite: None,
            srcs: vec![],
            includes: vec![as_absolute_string(&dir, "app_a/include/app.hrl")],
            labels: FxHashSet::default(),
            deps: vec![],
            apps: vec![],
            included_apps: vec![],
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
        let root = AbsPath::assert(&Utf8Path::from_path(dir.path()).unwrap());
        let target_name = "cell//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            suite: Some(as_absolute_string(&dir, "app_a/test/app_SUITE.erl")),
            srcs: vec![],
            includes: vec![],
            labels: FxHashSet::default(),
            deps: vec![],
            apps: vec![],
            included_apps: vec![],
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
        let root = AbsPath::assert(&Utf8Path::from_path(dir.path()).unwrap());
        let target_name = "cell//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
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
        let root = AbsPath::assert(&Utf8Path::from_path(dir.path()).unwrap());
        let target_name = "cell//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            suite: None,
            srcs: vec!["cell//app_a/app.erl".to_string()],
            includes: vec!["cell//app_a/app.hrl".to_string()],
            labels: FxHashSet::default(),
            deps: vec![],
            apps: vec![],
            included_apps: vec![],
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
        let root = AbsPath::assert(&Utf8Path::from_path(dir.path()).unwrap());
        let target_name = "cell//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            suite: None,
            srcs: vec![as_absolute_string(&dir, "app_a/sub/app.erl")],
            includes: vec![as_absolute_string(&dir, "app_a/sub/app.hrl")],
            labels: FxHashSet::default(),
            deps: vec![],
            apps: vec![],
            included_apps: vec![],
        };

        let actual = find_app_root_bxl(root, &target_name, &target);
        let expected = Some(to_abs_path_buf(&dir.path().join("app_a/sub")).unwrap());
        assert_eq!(expected, actual)
    }

    // TODO: enable when buck is properly set up on github project
    // @fb-only
    const BUCK_TESTS_ENABLED: bool = false; // @oss-only

    #[track_caller]
    fn check_buck_bxl_query(expect: Expect) {
        if BUCK_TESTS_ENABLED {
            let buck_root = to_abs_path_buf(&std::env::current_dir().unwrap()).unwrap();
            let buck_config = BuckConfig {
                config_path: None,
                buck_root: Some(buck_root),
                enabled: true,
                deps_target: None,
                build_deps: false,
                included_targets: vec![
                    "fbcode//whatsapp/elp/test_projects/buck_tests_2/util/app_a/...".to_string(),
                ],
                excluded_targets: vec![],
                source_root: Some(PathBuf::from("whatsapp/elp/test_projects/buck_tests_2")),
            };
            let output = buck_config
                .buck_command()
                .arg("bxl")
                .arg("prelude//erlang/elp.bxl:elp_config")
                .arg("--")
                .arg("--included_targets")
                .arg("fbcode//whatsapp/elp/test_projects/buck_tests_2/util/app_a/...")
                .output()
                .unwrap();
            if !output.status.success() {
                panic!("{:#?}", output);
            }
            let string = String::from_utf8(output.stdout).unwrap();

            let to_replace = env!("CARGO_WORKSPACE_DIR");
            let string = string.replace(to_replace, "/[..]/");
            expect.assert_eq(&string);
        }
    }

    #[test]
    fn build_info_buck_bxl_query() {
        check_buck_bxl_query(expect![[r#"
            {
              "fbcode//whatsapp/elp/test_projects/buck_tests_2/util/app_a:app_a": {
                "name": "app_a",
                "suite": null,
                "srcs": [
                  "/[..]/test_projects/buck_tests_2/util/app_a/src/app_a.erl"
                ],
                "includes": [],
                "labels": [],
                "deps": [],
                "apps": [
                  "fbcode//whatsapp/elp/test_projects/buck_tests_2/auto_gen/auto_gen_a:auto_gen_a"
                ],
                "included_apps": []
              }
            }
        "#]]);
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
        .assert_debug_eq(&buck_path_to_abs_path(&root, "cell//foo/bar"));

        expect![[r#"
            Ok(
                AbsPathBuf(
                    "/blah/foo/bar",
                ),
            )
        "#]]
        .assert_debug_eq(&buck_path_to_abs_path(&root, "//foo/bar"));

        expect![[r#"
            Ok(
                AbsPathBuf(
                    "/blah/foo/bar",
                ),
            )
        "#]]
        .assert_debug_eq(&buck_path_to_abs_path(&root, "foo/bar"))
    }
}

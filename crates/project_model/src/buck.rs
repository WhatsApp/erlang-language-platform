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

use std::borrow::Cow;
use std::cmp::Ordering;
use std::convert::TryFrom;
use std::ffi::OsStr;
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
use serde::Deserialize;
use serde::Serialize;

use crate::make_build_info;
use crate::otp::Otp;
use crate::path_to_binary;
use crate::save_build_info;
use crate::str_to_binary;
use crate::AppName;
use crate::AppType;
use crate::BuildInfoFile;
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

lazy_static! {
    static ref ERL_EXT: &'static OsStr = OsStr::new("erl");
}

lazy_static! {
    static ref SRC_EXT: &'static OsStr = OsStr::new("src");
}

lazy_static! {
    static ref TEST_EXT: &'static OsStr = OsStr::new("test");
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
            Some(ref dir) => Cow::Owned(buck_root.join(dir)),
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
    ) -> Result<(BuckProject, Vec<ProjectAppData>, BuildInfoFile, PathBuf), anyhow::Error> {
        let target_info = load_buck_targets(buck_conf)?;
        let otp_root = Otp::find_otp()?;
        let project_app_data = targets_to_project_data(&target_info.targets, &otp_root);
        let build_info_term = build_info(buck_conf, &project_app_data, &otp_root);
        let build_info = save_build_info(build_info_term)?;
        let project = BuckProject {
            target_info,
            buck_conf: buck_conf.clone(),
        };
        Ok((project, project_app_data, build_info, otp_root))
    }

    pub fn target(&self, file_path: &AbsPathBuf) -> Option<String> {
        self.target_info.path_to_target_name.get(file_path).cloned()
    }
}

#[derive(Deserialize, Debug)]
struct BuckTarget {
    name: String,
    //Some if target is test
    suite: Option<String>,
    #[serde(default)]
    srcs: Vec<String>,
    #[serde(default)]
    includes: Vec<String>,
    #[serde(default)]
    labels: FxHashSet<String>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Target {
    //full-name, like  waserver//erl/chatd:chatd
    pub name: TargetFullName,
    pub app_name: String,
    pub dir: AbsPathBuf,
    pub src_files: Vec<AbsPathBuf>,
    pub include_files: Vec<AbsPathBuf>,
    pub ebin: Option<AbsPathBuf>,
    pub target_type: TargetType,
    pub private_header: bool,
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq)]
pub enum TargetType {
    ErlangApp,
    ErlangTest,
    //rebar3's deps
    ThirdParty,
    ErlangTestUtils,
}

pub fn load_buck_targets(buck_config: &BuckConfig) -> Result<TargetInfo> {
    let _timer = timeit!("loading info from buck");
    let root = buck_config.buck_root();

    let mut dep_path = if buck_config.build_deps {
        build_third_party_targets(buck_config)?
    } else {
        FxHashMap::default()
    };
    let buck_targets = query_buck_targets(buck_config)?;

    let mut target_info = TargetInfo::default();
    for (name, target) in buck_targets {
        let mut private_header = false;

        let dir = match find_app_root(root, &name, &target) {
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
                    if Some(OsStr::new("hrl")) == src.extension() {
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
                        .map(|dir| dir.join(PathBuf::from("ebin"))),
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
        Err(path) => bail!("expected absolute path, got {}", path.display()),
    }
}

fn query_buck_targets(buck_config: &BuckConfig) -> Result<FxHashMap<TargetFullName, BuckTarget>> {
    let _timer = timeit!("load buck targets");
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
    let result = result
        .into_iter()
        .filter(|(name, _)| {
            !buck_config
                .excluded_targets
                .iter()
                .any(|excluded| name.starts_with(excluded))
        })
        .filter(|(_, target)| {
            target.suite.is_some() || !target.srcs.is_empty() || !target.includes.is_empty()
        })
        .collect();
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

/// Convert waserver//erl/util/thrift/src/thrift_compiler.erl to /Users/$USER/local/whatsapp/server/erl/util/thrift/src/thrift_compiler.erl
fn buck_path_to_abs_path(root: &AbsPath, target: &str) -> Result<AbsPathBuf> {
    let mut split = target.split("//");
    let _ = split.next(); //waserver or empty in case of //...
    match split.next() {
        None => bail!("couldn't find a path for target {:?}", target),
        Some(path) => Ok(root.join(path)),
    }
}

/// creates erlang term for an app in a format required by eqwalizer
pub fn build_info_app(project_data: &ProjectAppData, ebin: impl AsRef<Path>) -> Term {
    let dir = path_to_binary(&project_data.dir);
    let ebin = path_to_binary(ebin);

    let extra_src_dirs = Term::List(
        project_data
            .extra_src_dirs
            .iter()
            .map(|s| str_to_binary(s.as_ref()))
            .collect::<Vec<Term>>()
            .into(),
    );

    let include_dirs = Term::List(
        project_data
            .include_dirs
            .iter()
            .map(path_to_binary)
            .collect::<Vec<Term>>()
            .into(),
    );

    let macros = Term::List(project_data.macros.clone().into());
    let name = str_to_binary(&project_data.name.0);
    let parse_transforms = Term::List(project_data.parse_transforms.clone().into());

    let src_dirs = Term::List(
        project_data
            .abs_src_dirs
            .iter()
            .filter_map(|src| src.strip_prefix(project_data.dir.as_ref()))
            .map(|path| path_to_binary(path.as_ref()))
            .collect::<Vec<Term>>()
            .into(),
    );

    Term::Map(
        vec![
            (Atom("dir".into()), dir),
            (Atom("ebin".into()), ebin),
            (Atom("extra_src_dirs".into()), extra_src_dirs),
            (Atom("include_dirs".into()), include_dirs),
            (Atom("macros".into()), macros),
            (Atom("name".into()), name),
            (Atom("parse_transforms".into()), parse_transforms),
            (Atom("src_dirs".into()), src_dirs),
        ]
        .into(),
    )
}

pub fn build_info(config: &BuckConfig, project_apps: &[ProjectAppData], otp_root: &Path) -> Term {
    let mut apps = vec![];
    let mut deps = vec![];
    let default_ebin = config.buck_root().join("buck-out/ebins");
    for project in project_apps {
        let ebin = match &project.ebin {
            None => default_ebin.as_path(),
            Some(ebin) => ebin.as_path(),
        };

        match project.app_type {
            AppType::App => apps.push(build_info_app(project, ebin)),
            AppType::Dep if !config.build_deps => apps.push(build_info_app(project, ebin)),
            AppType::Dep => deps.push(build_info_app(project, ebin)),
            _ => {}
        };
    }
    let path = &config.source_root();
    make_build_info(apps, deps, otp_root, path.as_ref())
}

/// convert waserver//erl/chatd:chatd into abs path ~/local/whatsapp/server/erl/chatd
fn find_buck_file_base_target_dir(
    root: &AbsPath,
    target_name: &TargetFullName,
) -> Result<AbsPathBuf> {
    let path: String = target_name.chars().take_while(|ch| *ch != ':').collect();
    buck_path_to_abs_path(root, &path)
}

fn find_app_root(
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
                if !set.contains(parent) {
                    set.insert(parent.to_path_buf());
                }
            }
        }
    }

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

pub fn targets_to_project_data(
    targets: &FxHashMap<TargetFullName, Target>,
    otp_root: &Path,
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

#[derive(Debug)]
struct ProjectAppDataAcc {
    pub name: Option<AppName>,
    pub dir: Option<AbsPathBuf>,
    pub ebin: Option<AbsPathBuf>,
    pub abs_src_dirs: FxHashSet<AbsPathBuf>,
    pub extra_src_dirs: FxHashSet<String>,
    pub abs_extra_src_dirs: FxHashSet<AbsPathBuf>,
    pub include_dirs: FxHashSet<String>,
    pub macros: Vec<Term>,
    pub app_type: Option<AppType>,
    pub include_path: FxHashSet<AbsPathBuf>,
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
            let app_type = match target.target_type {
                TargetType::ErlangApp => AppType::App,
                TargetType::ErlangTest => AppType::App,
                TargetType::ErlangTestUtils => AppType::App,
                TargetType::ThirdParty => AppType::Dep,
            };
            self.app_type = Some(app_type)
        }
    }

    fn set_name(&mut self, target: &Target) {
        let dir_name = target
            .dir
            .file_name()
            .map(|f| AppName(f.to_string_lossy().to_string()));
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
                    .filter(|dir| dir.file_name() != Some(&TEST_EXT))
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
                    .filter(|dir| dir.file_name() != Some(&SRC_EXT))
                    .collect();

                let extra_src_dirs: Vec<String> = abs_extra_dirs
                    .iter()
                    .filter_map(|dir| dir.strip_prefix(app_dir))
                    .map(|dir| dir.as_ref().as_os_str().to_string_lossy().to_string())
                    .collect();

                for abs_extra in &abs_extra_dirs {
                    self.add_parent_if_not_exist(abs_extra);
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
                self.add_parent_if_not_exist(src);
            }
        }

        for inc in &target.include_files {
            self.add_parent_if_not_exist(inc);
        }
    }

    fn add_parent_if_not_exist(&mut self, path: &AbsPath) {
        if let Some(parent) = path.parent() {
            if !self.include_path.contains(parent) {
                self.include_path.insert(parent.to_path_buf());
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
            .filter_map(|inc| inc.parent())
            //in case of .*_test_utils target it is possible to have *.hrl file in test dir
            //which buck treats as include. This check prevents it in case we already added
            //the folder as test folder
            .filter(|inc| !self.abs_extra_src_dirs.contains(*inc))
            .filter_map(|inc| inc.strip_prefix(&target.dir))
            .map(|inc| inc.as_ref().to_string_lossy().to_string());

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
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_fixture::FixtureWithProjectMeta;

    #[test]
    fn test_find_app_root_src() {
        let spec = r#"
        //- /app_a/src/app.erl
        //- /app_a/BUCK
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let root = AbsPath::assert(dir.path());
        let target_name = "waserver//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            suite: None,
            srcs: vec!["waserver//app_a/src/app.erl".to_string()],
            includes: vec![],
            labels: FxHashSet::default(),
        };

        let actual = find_app_root(root, &target_name, &target);
        let expected = Some(AbsPathBuf::assert(dir.path().join("app_a")));
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_find_app_root_include() {
        let spec = r#"
        //- /app_a/include/app.hrl
        //- /app_a/BUCK
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let root = AbsPath::assert(dir.path());
        let target_name = "waserver//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            suite: None,
            srcs: vec![],
            includes: vec!["waserver//app_a/include/app.hrl".to_string()],
            labels: FxHashSet::default(),
        };

        let actual = find_app_root(root, &target_name, &target);
        let expected = Some(AbsPathBuf::assert(dir.path().join("app_a")));
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_find_app_root_suite() {
        let spec = r#"
        //- /app_a/test/app_SUITE.erl
        //- /app_a/BUCK
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let root = AbsPath::assert(dir.path());
        let target_name = "waserver//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            suite: Some("waserver//app_a/test/app_SUITE.erl".to_string()),
            srcs: vec![],
            includes: vec![],
            labels: FxHashSet::default(),
        };

        let actual = find_app_root(root, &target_name, &target);
        let expected = Some(AbsPathBuf::assert(dir.path().join("app_a")));
        assert_eq!(expected, actual)
    }

    #[test]
    //aka wa_zippy case
    fn test_find_app_root_multiple_src() {
        let spec = r#"
        //- /app_a/src/app.erl
        //- /app_a/entity/entity.erl
        //- /app_a/BUCK
        "#;
        let dir = FixtureWithProjectMeta::gen_project(spec);
        let root = AbsPath::assert(dir.path());
        let target_name = "waserver//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            suite: None,
            srcs: vec![
                "waserver//app_a/entity/entity.erl".to_string(),
                "waserver//app_a/src/app.erl".to_string(),
            ],
            includes: vec![],
            labels: FxHashSet::default(),
        };

        let actual = find_app_root(root, &target_name, &target);
        let expected = Some(AbsPathBuf::assert(dir.path().join("app_a")));
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
        let root = AbsPath::assert(dir.path());
        let target_name = "waserver//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            suite: None,
            srcs: vec!["waserver//app_a/app.erl".to_string()],
            includes: vec!["waserver//app_a/app.hrl".to_string()],
            labels: FxHashSet::default(),
        };

        let actual = find_app_root(root, &target_name, &target);
        let expected = Some(AbsPathBuf::assert(dir.path().join("app_a")));
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
        let root = AbsPath::assert(dir.path());
        let target_name = "waserver//app_a:app_a".to_string();
        let target = BuckTarget {
            name: "app_a".to_string(),
            suite: None,
            srcs: vec!["waserver//app_a/sub/app.erl".to_string()],
            includes: vec!["waserver//app_a/sub/app.hrl".to_string()],
            labels: FxHashSet::default(),
        };

        let actual = find_app_root(root, &target_name, &target);
        let expected = Some(AbsPathBuf::assert(dir.path().join("app_a")));
        assert_eq!(expected, actual)
    }
}

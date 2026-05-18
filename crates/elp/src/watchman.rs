/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::process::Stdio;

use anyhow::Result;
use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use elp_ide::elp_ide_db::elp_base_db::VfsPath;
use paths::Utf8PathBuf;
use serde::Deserialize;

use crate::build::types::LoadResult;

pub struct Watchman {
    watch: PathBuf,
    clock: String,
    expression: serde_json::Value,
}

#[derive(Deserialize)]
struct WatchProjectResponse {
    watch: PathBuf,
}

#[derive(Debug, Clone, Deserialize)]
struct WatchmanQueryResult {
    #[serde(default)]
    is_fresh_instance: bool,
    clock: String,
    #[serde(default)]
    files: Vec<WatchmanFile>,
}

#[derive(Debug, Clone, Deserialize)]
struct WatchmanFile {
    name: String,
    exists: bool,
    #[serde(default)]
    new: bool,
}

impl Watchman {
    pub fn new(project: &Path) -> Result<Self> {
        let mut cmd = Command::new("watchman");
        cmd.arg("watch-project").arg(project.as_os_str());
        let output = cmd.output().map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                anyhow::Error::msg(
                    "`watchman` command not found. install it from \
                     https://facebook.github.io/watchman/ to use `elp shell`.",
                )
            } else {
                e.into()
            }
        })?;
        let resp: WatchProjectResponse = serde_json::from_slice(&output.stdout).map_err(|_| {
            anyhow::Error::msg(
                "Could not find project. Are you in an Erlang project directory, \
                     or is one specified using --project?",
            )
        })?;
        let clock = Self::get_clock_for(&resp.watch)?;
        let expression = build_watchman_expression(&[]);
        Ok(Watchman {
            watch: resp.watch,
            clock,
            expression,
        })
    }

    pub fn set_project_dirs(&mut self, loaded: &LoadResult) {
        let project_dirs = collect_project_dirs(loaded, &self.watch);
        self.expression = build_watchman_expression(&project_dirs);
        // Best-effort clock refresh; keep old clock on failure
        if let Ok(clock) = Self::get_clock_for(&self.watch) {
            self.clock = clock;
        }
    }

    pub fn poll_and_apply_changes(&mut self, loaded: &mut LoadResult) -> Result<UpdateResult> {
        let result = self.query()?;

        if result.is_fresh_instance {
            return Ok(UpdateResult::NeedsFullReload {
                reason: "Watchman clock invalidated (large change detected), reloading project...",
            });
        }

        if result.files.is_empty() {
            self.clock = result.clock;
            return Ok(UpdateResult::Updated);
        }

        // Branches that throw away the in-memory project state can return
        // without applying VFS changes — the daemon will rebuild from scratch
        // (or exit). Order matters: project-config changes shadow lint-config
        // and suite-file changes, which shadow ordinary source changes.

        if result
            .files
            .iter()
            .any(|f| is_elp_proj_config_file(&f.name))
        {
            return Ok(UpdateResult::NeedsRestart {
                reason: "ELP config change detected, restart required",
            });
        }

        if result.files.iter().any(|f| is_config_file(&f.name)) {
            return Ok(UpdateResult::NeedsFullReload {
                reason: "Project change detected, reloading project",
            });
        }

        if result
            .files
            .iter()
            .any(|f| is_suite_file(&f.name) && (f.new || !f.exists))
        {
            return Ok(UpdateResult::NeedsFullReload {
                reason: "Suite file change detected, reloading project",
            });
        }

        // `.elp_lint.toml` change is hot-reloaded by the caller; the daemon
        // keeps its loaded project, so we MUST still apply any non-config
        // changes from the same batch to VFS before advancing the clock —
        // otherwise batched source-file changes are silently lost.
        let lint_config_changed = result
            .files
            .iter()
            .any(|f| is_elp_lint_config_file(&f.name));

        let vfs = &mut loaded.vfs;
        for file in &result.files {
            // .elp_lint.toml itself is not tracked in VFS; the daemon's
            // reload path reads it via read_lint_config_file directly.
            if is_elp_lint_config_file(&file.name) {
                continue;
            }
            let path = self.watch.join(&file.name);
            let vfs_path = VfsPath::from(AbsPathBuf::assert(
                Utf8PathBuf::from_path_buf(path.clone()).expect("UTF8 conversion failed"),
            ));
            if !file.exists {
                vfs.set_file_contents(vfs_path, None);
            } else {
                match fs::read(&path) {
                    Ok(contents) => {
                        vfs.set_file_contents(vfs_path, Some(contents));
                    }
                    Err(err) => {
                        log::warn!("Cannot read file {path:?}: {err}, treating as deleted");
                        vfs.set_file_contents(vfs_path, None);
                    }
                }
            }
        }
        loaded.apply_vfs_changes();
        self.clock = result.clock;

        if lint_config_changed {
            Ok(UpdateResult::NeedsLintConfigReload {
                reason: "Lint config change detected, reloading",
            })
        } else {
            Ok(UpdateResult::Updated)
        }
    }

    fn get_clock_for(watch: &Path) -> Result<String> {
        let mut cmd = Command::new("watchman");
        cmd.arg("clock");
        cmd.arg(watch.as_os_str());
        let result: serde_json::Value = serde_json::from_slice(&cmd.output()?.stdout)?;
        result["clock"]
            .as_str()
            .map(|s| s.to_string())
            .ok_or_else(|| anyhow::anyhow!("Missing clock in watchman response"))
    }

    fn query(&self) -> Result<WatchmanQueryResult> {
        let query = serde_json::json!([
            "query",
            self.watch.to_string_lossy(),
            {
                "since": self.clock,
                "expression": self.expression,
                "fields": ["name", "exists", "new"]
            }
        ]);
        let mut cmd = Command::new("watchman");
        cmd.arg("-j")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        let mut child = cmd.spawn()?;
        if let Some(stdin) = child.stdin.take() {
            serde_json::to_writer(stdin, &query)?;
        }
        let output = child.wait_with_output()?;
        Ok(serde_json::from_slice(&output.stdout)?)
    }
}

pub enum UpdateResult {
    Updated,
    NeedsFullReload { reason: &'static str },
    NeedsRestart { reason: &'static str },
    NeedsLintConfigReload { reason: &'static str },
}

fn collect_project_dirs(loaded: &LoadResult, watch_root: &Path) -> Vec<PathBuf> {
    let prefix = format!("{}/", watch_root.display());
    loaded
        .project
        .non_otp_apps()
        .flat_map(|app| app.all_dirs_to_watch())
        .filter_map(|dir| {
            let dir_str = dir.to_string();
            dir_str.strip_prefix(&prefix).map(PathBuf::from)
        })
        .collect()
}

fn build_watchman_expression(project_dirs: &[PathBuf]) -> serde_json::Value {
    let config_files = serde_json::json!([
        "anyof",
        ["match", "BUCK", "basename"],
        ["match", "TARGETS", "basename"],
        ["match", "TARGETS.v2", "basename"],
        ["match", "rebar.config", "basename"],
        ["match", "rebar.config.script", "basename"],
        ["match", ".elp.toml", "basename"],
        ["match", ".elp_lint.toml", "basename"]
    ]);

    let source_suffix = serde_json::json!(["anyof", ["suffix", "erl"], ["suffix", "hrl"]]);

    let source_files = if project_dirs.is_empty() || project_dirs.len() > 100 {
        source_suffix
    } else {
        let mut dir_parts: Vec<serde_json::Value> = vec![serde_json::json!("anyof")];
        for dir in project_dirs {
            dir_parts.push(serde_json::json!(["dirname", dir.to_string_lossy()]));
        }
        let dir_filter = serde_json::Value::Array(dir_parts);
        serde_json::json!(["allof", source_suffix, dir_filter])
    };

    serde_json::json!([
        "allof",
        ["type", "f"],
        ["anyof", source_files, config_files]
    ])
}

fn file_basename(name: &str) -> &str {
    Path::new(name)
        .file_name()
        .and_then(|f| f.to_str())
        .unwrap_or("")
}

fn is_elp_proj_config_file(name: &str) -> bool {
    file_basename(name) == ".elp.toml"
}

fn is_elp_lint_config_file(name: &str) -> bool {
    file_basename(name) == ".elp_lint.toml"
}

fn is_config_file(name: &str) -> bool {
    matches!(
        file_basename(name),
        "BUCK" | "TARGETS" | "TARGETS.v2" | "rebar.config" | "rebar.config.script"
    )
}

fn is_suite_file(name: &str) -> bool {
    name.ends_with("_SUITE.erl")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn elp_proj_config_matches_only_elp_toml() {
        assert!(is_elp_proj_config_file(".elp.toml"));
        assert!(is_elp_proj_config_file("path/to/.elp.toml"));
        assert!(!is_elp_proj_config_file(".elp_lint.toml"));
        assert!(!is_elp_proj_config_file("path/to/.elp_lint.toml"));
        assert!(!is_elp_proj_config_file("BUCK"));
        assert!(!is_elp_proj_config_file("src/foo.erl"));
    }

    #[test]
    fn elp_lint_config_matches_only_elp_lint_toml() {
        assert!(is_elp_lint_config_file(".elp_lint.toml"));
        assert!(is_elp_lint_config_file("path/to/.elp_lint.toml"));
        assert!(!is_elp_lint_config_file(".elp.toml"));
        assert!(!is_elp_lint_config_file("path/to/.elp.toml"));
        assert!(!is_elp_lint_config_file("rebar.config"));
        assert!(!is_elp_lint_config_file("src/foo.erl"));
    }

    #[test]
    fn project_and_lint_config_predicates_are_disjoint() {
        // Ensures a single file change can't trigger both restart and reload —
        // important because handle_connection's match treats them as separate
        // branches with different recovery actions.
        for name in [".elp.toml", ".elp_lint.toml", "deep/nested/.elp_lint.toml"] {
            assert!(
                !(is_elp_proj_config_file(name) && is_elp_lint_config_file(name)),
                "{name} matched both predicates"
            );
        }
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::fs;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Result;
use anyhow::anyhow;
use anyhow::bail;
use elp_ide::Analysis;
use elp_ide::diagnostics::LintConfig;
use elp_ide::elp_ide_db::elp_base_db::FileId;
use elp_syntax::SmolStr;
use fxhash::FxHashSet;
use lazy_static::lazy_static;
use serde::de::DeserializeOwned;
pub use server::setup::ServerSetup;

pub mod arc_types;
pub mod build;
pub mod cli;
pub mod config;
pub mod convert;
pub mod document;
mod from_proto;
mod handlers;
pub mod line_endings;
pub mod lsp_ext;
mod mem_docs;
pub mod memory_usage;
// @fb-only
mod op_queue;
mod project_loader;
pub mod reload;
mod semantic_tokens;
pub mod server;
mod snapshot;
mod task_pool;
mod to_proto;

pub fn from_json<T: DeserializeOwned>(what: &'static str, json: serde_json::Value) -> Result<T> {
    let res = serde_path_to_error::deserialize(&json)
        .map_err(|e| anyhow!("Failed to deserialize {}: {}; {}", what, e, json))?;
    Ok(res)
}

#[derive(Debug)]
struct LspError {
    code: i32,
    message: String,
}

impl LspError {
    fn new(code: i32, message: String) -> LspError {
        LspError { code, message }
    }
}

impl fmt::Display for LspError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Language Server request failed with {}. ({})",
            self.code, self.message
        )
    }
}

impl std::error::Error for LspError {}

/// Gets ELP version.
///
/// This uses the version as set in the Cargo.toml file
/// as well as either `+local` for locally-build versions
/// or `+build-YEAR-MONTH-DAY` for versions built on CI.
///
/// To check for CI a `CI` env var is inspected.
/// It's possible to override the date with
/// [`SOURCE_DATE_EPOCH`](https://reproducible-builds.org/docs/source-date-epoch/).
/// See the `build.rs` file for more details.
pub fn version() -> String {
    format!("{}+{}", env!("CARGO_PKG_VERSION"), env!("BUILD_ID"))
}

/// Some modules use a macro such as `-define(CATCH, catch).`.
/// Our grammar cannot handle it at the moment, so we keep a list of
/// these modules to skip when doing elp parsing for CI.
pub fn otp_file_to_ignore(db: &Analysis, file_id: FileId) -> bool {
    lazy_static! {
        static ref SET: FxHashSet<SmolStr> =
            ["ttb",
                // Not all files in the dependencies compile with ELP,
                // also using unusual macros. Rather than skip
                // checking deps, we list the known bad ones.
                 "jsone", "jsone_decode", "jsone_encode",
                 "piqirun_props",
                 "yaws_server", "yaws_appmod_dav", "yaws_runmod_lock",
                 "jsonrpc",
                 "redbug_dtop",
                 ]
                .iter()
                // @fb-only
                .map(SmolStr::new)
                .collect();
    }
    if let Some(module_name) = db.module_name(file_id).unwrap() {
        SET.contains(module_name.as_str())
    } else {
        false
    }
}

// ---------------------------------------------------------------------

pub const LINT_CONFIG_FILE: &str = ".elp_lint.toml";

pub fn read_lint_config_file(project: &Path, config_file: &Option<String>) -> Result<LintConfig> {
    if let Some(file_name) = config_file {
        let file_path: PathBuf = file_name.into();
        match fs::read_to_string(file_path.clone()) {
            Ok(content) => match toml::from_str::<LintConfig>(&content) {
                Ok(config) => return Ok(config),
                Err(err) => bail!("errors parsing {:?}: {err}", file_path),
            },
            Err(err) => {
                bail!("unable to read {:?}: {err}", file_path)
            }
        }
    } else {
        let mut potential_path = Some(project);
        while let Some(path) = potential_path {
            let file_path = path.join(LINT_CONFIG_FILE);

            if !file_path.is_file() {
                potential_path = path.parent();
                continue;
            } else if let Ok(content) = fs::read_to_string(file_path.clone()) {
                match toml::from_str::<LintConfig>(&content) {
                    Ok(config) => return Ok(config),
                    Err(err) => bail!("failed to read {:?}:{err}", file_path),
                }
            }
            break;
        }
    }
    Ok(LintConfig::default())
}

#[cfg(test)]
mod tests {
    use elp_ide::FunctionMatch;
    use elp_ide::diagnostics::DiagnosticCode;
    use elp_ide::diagnostics::ErlangServiceConfig;
    use elp_ide::diagnostics::Lint;
    use elp_ide::diagnostics::LintsFromConfig;
    use elp_ide::diagnostics::MatchSsr;
    use elp_ide::diagnostics::ReplaceCall;
    use elp_ide::diagnostics::ReplaceCallAction;
    use elp_ide::diagnostics::Replacement;
    use elp_ide::diagnostics::Severity;
    use expect_test::expect;
    use fxhash::FxHashMap;

    use crate::LintConfig;

    #[test]
    fn serde_serialize_lint_config() {
        let lint_config = LintConfig {
            enabled_lints: vec![DiagnosticCode::ApplicationGetEnv],
            disabled_lints: vec![],
            ad_hoc_lints: LintsFromConfig {
                lints: vec![
                    Lint::ReplaceCall(ReplaceCall {
                        matcher: FunctionMatch::mf("mod_a", "func"),
                        action: ReplaceCallAction::Replace(Replacement::UseOk),
                    }),
                    Lint::ReplaceCall(ReplaceCall {
                        matcher: FunctionMatch::m("foo"),
                        action: ReplaceCallAction::Replace(Replacement::ArgsPermutation {
                            perm: vec![1, 2],
                        }),
                    }),
                    Lint::LintMatchSsr(MatchSsr {
                        ssr_pattern: "ssr: _@A = 10.".to_string(),
                        message: None,
                        strategy: None,
                        severity: None,
                    }),
                ],
            },
            linters: FxHashMap::default(),
            erlang_service: ErlangServiceConfig::default(),
        };
        expect![[r#"
            enabled_lints = ["W0011"]
            disabled_lints = []

            [erlang_service]
            warnings_as_errors = false
            [[ad_hoc_lints.lints]]
            type = "ReplaceCall"

            [ad_hoc_lints.lints.matcher]
            type = "MF"
            module = "mod_a"
            name = "func"

            [ad_hoc_lints.lints.action]
            action = "Replace"
            type = "UseOk"

            [[ad_hoc_lints.lints]]
            type = "ReplaceCall"

            [ad_hoc_lints.lints.matcher]
            type = "M"
            module = "foo"

            [ad_hoc_lints.lints.action]
            action = "Replace"
            type = "ArgsPermutation"
            perm = [1, 2]

            [[ad_hoc_lints.lints]]
            type = "LintMatchSsr"
            ssr_pattern = "ssr: _@A = 10."

            [linters]
        "#]]
        .assert_eq(&toml::to_string::<LintConfig>(&lint_config).unwrap());
    }

    #[test]
    fn serde_read_lint_config_empty_enabled() {
        let content = r#"
            disabled_lints = []
            "#;
        let config = toml::from_str::<LintConfig>(content).unwrap();
        assert_eq!(config.enabled_lints, vec![]);
        assert_eq!(config.disabled_lints, vec![]);
    }

    #[test]
    fn serde_read_lint_config_linters_overrides() {
        let content = r#"
            [linters.no_garbage_collect]
            severity = "error"
            "#;
        let config = toml::from_str::<LintConfig>(content).unwrap();
        assert_eq!(
            config
                .linters
                .get(&DiagnosticCode::NoGarbageCollect)
                .unwrap()
                .severity,
            Some(Severity::Error)
        );
    }
}

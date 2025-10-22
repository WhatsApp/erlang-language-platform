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
use std::path::PathBuf;
use std::process::Command;
use std::sync::RwLock;

use anyhow::Result;
use anyhow::bail;
use elp_log::timeit;
use lazy_static::lazy_static;
use paths::AbsPathBuf;
use paths::Utf8Path;
use paths::Utf8PathBuf;

use crate::AppName;
use crate::OtpConfig;
use crate::ProjectAppData;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Otp {
    pub lib_dir: AbsPathBuf,
}

lazy_static! {
    pub static ref ERL: RwLock<String> = RwLock::new("erl".to_string());
}

lazy_static! {
    pub static ref OTP_ROOT: Utf8PathBuf =
        Otp::find_otp().expect("tests should always be able to find OTP");
    pub static ref OTP_ERTS_DIR: AbsPathBuf = get_erts_dir();
    pub static ref OTP_ERLANG_MODULE: (PathBuf, String) = get_erlang_module();
    pub static ref OTP_ERLANG_APP: ProjectAppData = ProjectAppData::fixture_app_data(
        AppName("erts".to_string()),
        OTP_ERTS_DIR.clone(),
        Vec::default(),
        vec![OTP_ERTS_DIR.join("src")],
        Vec::default(),
    );
    pub static ref OTP_VERSION: Option<String> = Otp::otp_release().ok();
}

pub fn otp_supported_by_eqwalizer() -> bool {
    OTP_VERSION
        .as_ref()
        .map(|v| v.as_str() > "25")
        .unwrap_or(true)
}

pub fn supports_eep59_doc_attributes() -> bool {
    OTP_VERSION
        .as_ref()
        .map(|v| v.as_str() >= "27")
        .unwrap_or(true)
}

pub fn supports_eep66_sigils() -> bool {
    OTP_VERSION
        .as_ref()
        .map(|v| v.as_str() >= "27")
        .unwrap_or(true)
}

fn get_erts_dir() -> AbsPathBuf {
    let (_otp, apps) = Otp::discover(OTP_ROOT.to_path_buf(), &OtpConfig::default());
    for app in apps {
        if app.name == AppName("erts".to_string()) {
            return app.dir;
        }
    }
    panic!()
}

fn get_erlang_module() -> (PathBuf, String) {
    let erlang_path = OTP_ERTS_DIR.join("src/erlang.erl");
    let contents = std::fs::read_to_string(&erlang_path).unwrap();
    (erlang_path.into(), contents)
}

impl Otp {
    pub fn find_otp() -> Result<Utf8PathBuf> {
        let _timer = timeit!("find otp");
        let erl = ERL.read().unwrap();
        let output = Command::new(&*erl)
            .arg("-noshell")
            .arg("-eval")
            .arg("io:format('~s', [code:root_dir()])")
            .arg("-s")
            .arg("erlang")
            .arg("halt")
            .output()?;

        if !output.status.success() {
            bail!(
                "Failed to get OTP dir, error code: {:?}, stderr: {:?}",
                output.status.code(),
                String::from_utf8(output.stderr)
            );
        }
        let path = String::from_utf8(output.stdout)?;
        let result: Utf8PathBuf = format!("{path}/lib").into();
        let result = fs::canonicalize(result)?;
        Ok(Utf8PathBuf::from_path_buf(result).expect("Could not create Utf8PathBuf"))
    }

    pub fn otp_release() -> Result<String> {
        let _timer = timeit!("otp_version");
        let erl = ERL.read().unwrap();
        let output = Command::new(&*erl)
            .arg("-noshell")
            .arg("-eval")
            .arg("io:format('~s', [erlang:system_info(otp_release)])")
            .arg("-s")
            .arg("erlang")
            .arg("halt")
            .output()?;

        if !output.status.success() {
            bail!(
                "Failed to get OTP release, error code: {:?}, stderr: {:?}",
                output.status.code(),
                String::from_utf8(output.stderr)
            );
        }
        let val = String::from_utf8(output.stdout)?;
        Ok(val)
    }

    pub fn system_version() -> Result<String> {
        let _timer = timeit!("system_version");
        let erl = ERL.read().unwrap();
        let output = Command::new(&*erl)
            .arg("-noshell")
            .arg("-eval")
            .arg("io:format('~s', [erlang:system_info(system_version)])")
            .arg("-s")
            .arg("erlang")
            .arg("halt")
            .output()?;

        if !output.status.success() {
            bail!(
                "Failed to get OTP system version, error code: {:?}, stderr: {:?}",
                output.status.code(),
                String::from_utf8(output.stderr)
            );
        }
        let val = String::from_utf8(output.stdout)?;
        Ok(val)
    }

    pub fn discover(path: Utf8PathBuf, otp_config: &OtpConfig) -> (Otp, Vec<ProjectAppData>) {
        let apps = Self::discover_otp_apps(&path, otp_config);
        (
            Otp {
                lib_dir: AbsPathBuf::assert(path),
            },
            apps,
        )
    }

    fn discover_otp_apps(path: &Utf8Path, otp_config: &OtpConfig) -> Vec<ProjectAppData> {
        let exclude_apps = &otp_config.exclude_apps;
        log::info!("Loading OTP apps from {path:?}");
        if let Ok(entries) = fs::read_dir(path) {
            entries
                .into_iter()
                .filter_map(|entry| {
                    let entry = entry.ok()?;
                    let name = entry.file_name();
                    let name_str = name.to_str()?;

                    // Extract app name from versioned directory name (e.g., "megaco-4.5" -> "megaco")
                    let app_name = name_str
                        .split_once('-')
                        .map_or(name_str, |(base, _version)| base);

                    // Skip excluded applications
                    if exclude_apps.contains(&app_name.to_string()) {
                        log::info!("Excluding OTP app: {}", app_name);
                        return None;
                    }

                    let path = fs::canonicalize(entry.path()).expect("Could not canonicalize path");
                    let dir = AbsPathBuf::assert(
                        Utf8PathBuf::from_path_buf(path).expect("Could not convert to Utf8PathBuf"),
                    );
                    Some(ProjectAppData::otp_app_data(name_str, &dir))
                })
                .collect()
        } else {
            vec![]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_discover_with_excluded_apps() {
        // Create a mock OTP directory structure
        let temp_dir = tempfile::tempdir().unwrap();
        let otp_lib_dir = temp_dir.path().join("lib");
        std::fs::create_dir_all(&otp_lib_dir).unwrap();

        // Create mock OTP app directories
        let apps = vec![
            "kernel-9.2",
            "stdlib-5.2",
            "megaco-4.5",
            "eunit-2.8.2",
            "common_test-1.25.1",
        ];

        for app in &apps {
            let app_dir = otp_lib_dir.join(app);
            std::fs::create_dir_all(app_dir.join("src")).unwrap();
            std::fs::create_dir_all(app_dir.join("include")).unwrap();
            std::fs::create_dir_all(app_dir.join("ebin")).unwrap();
        }

        let otp_path = Utf8PathBuf::from_path_buf(otp_lib_dir).unwrap();

        // Test without exclusions
        let (_otp, all_apps) = Otp::discover(otp_path.clone(), &OtpConfig::default());
        let app_names: Vec<&str> = all_apps.iter().map(|app| app.name.as_str()).collect();
        assert!(app_names.contains(&"kernel"));
        assert!(app_names.contains(&"stdlib"));
        assert!(app_names.contains(&"megaco"));
        assert!(app_names.contains(&"eunit"));
        assert!(app_names.contains(&"common_test"));

        // Test with exclusions
        let otp_config = OtpConfig {
            exclude_apps: vec!["megaco".to_string(), "eunit".to_string()],
        };
        let (_otp, filtered_apps) = Otp::discover(otp_path, &otp_config);
        let filtered_names: Vec<&str> = filtered_apps.iter().map(|app| app.name.as_str()).collect();
        assert!(filtered_names.contains(&"kernel"));
        assert!(filtered_names.contains(&"stdlib"));
        assert!(filtered_names.contains(&"common_test"));
        assert!(!filtered_names.contains(&"megaco"));
        assert!(!filtered_names.contains(&"eunit"));

        // Verify the filtered list is smaller
        assert!(filtered_apps.len() < all_apps.len());
        assert_eq!(filtered_apps.len(), all_apps.len() - 2);
    }
}

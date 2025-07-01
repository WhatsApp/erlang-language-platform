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
    let (_otp, apps) = Otp::discover(OTP_ROOT.to_path_buf());
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
        let result: Utf8PathBuf = format!("{}/lib", path).into();
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

    pub fn discover(path: Utf8PathBuf) -> (Otp, Vec<ProjectAppData>) {
        let apps = Self::discover_otp_apps(&path);
        (
            Otp {
                lib_dir: AbsPathBuf::assert(path),
            },
            apps,
        )
    }

    fn discover_otp_apps(path: &Utf8Path) -> Vec<ProjectAppData> {
        log::info!("Loading OTP apps from {:?}", path);
        if let Ok(entries) = fs::read_dir(path) {
            entries
                .into_iter()
                .filter_map(|entry| {
                    let entry = entry.ok()?;
                    let name = entry.file_name();
                    let path = fs::canonicalize(entry.path()).expect("Could not canonicalize path");
                    let dir = AbsPathBuf::assert(
                        Utf8PathBuf::from_path_buf(path).expect("Could not convert to Utf8PathBuf"),
                    );
                    Some(ProjectAppData::otp_app_data(name.to_str()?, &dir))
                })
                .collect()
        } else {
            vec![]
        }
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fs;
use std::process::Command;
use std::sync::RwLock;

use anyhow::bail;
use anyhow::Result;
use elp_log::timeit;
use lazy_static::lazy_static;
use paths::AbsPathBuf;
use paths::Utf8Path;
use paths::Utf8PathBuf;

use crate::ProjectAppData;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Otp {
    pub lib_dir: AbsPathBuf,
}

lazy_static! {
    pub static ref ERL: RwLock<String> = RwLock::new("erl".to_string());
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
    pub fn otp_version() -> Result<String> {
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
                "Failed to get OTP version, error code: {:?}, stderr: {:?}",
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

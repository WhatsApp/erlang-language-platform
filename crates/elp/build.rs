/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::env;
use std::str::FromStr;

use time::format_description;
use time::OffsetDateTime;

const CI: &str = "CI";
const SOURCE_DATE_EPOCH: &str = "SOURCE_DATE_EPOCH";
const EQWALIZER_DIR: &str = "EQWALIZER_DIR";
const EQWALIZER_SUPPORT_DIR: &str = "EQWALIZER_SUPPORT_DIR";
const CARGO_MANIFEST_DIR: &str = "CARGO_MANIFEST_DIR";

fn main() {
    let date_format =
        format_description::parse("build-[year]-[month]-[day]").expect("wrong format");

    let is_ci = env::var(CI).is_ok();
    let epoch = env::var(SOURCE_DATE_EPOCH);
    let build_id = if is_ci || epoch.is_ok() {
        let date = match epoch {
            Ok(v) => {
                let timestamp = i64::from_str(&v).expect("parsing SOURCE_DATE_EPOCH");
                OffsetDateTime::from_unix_timestamp(timestamp).expect("parsing SOURCE_DATE_EPOCH")
            }
            Err(std::env::VarError::NotPresent) => OffsetDateTime::now_utc(),
            Err(e) => panic!("Error getting SOURCE_DATE_EPOCH: {}", e),
        };
        date.format(&date_format).expect("formatting date")
    } else {
        "local".to_string()
    };
    let eqwalizer_dir = env::var(EQWALIZER_DIR);
    let cargo_manifest_dir = env::var(CARGO_MANIFEST_DIR)
        .expect("CARGO_MANIFEST_DIR should be set automatically by cargo");
    let eqwalizer_support_dir = match eqwalizer_dir {
        Ok(eqwalizer_support_dir) => format!("{}/../eqwalizer_support", eqwalizer_support_dir),
        Err(_) => format!(
            "{}/../../../eqwalizer/eqwalizer_support",
            cargo_manifest_dir
        ),
    };

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed={}", SOURCE_DATE_EPOCH);
    println!("cargo:rerun-if-env-changed={}", CI);
    println!("cargo:rustc-env=BUILD_ID={}", build_id);
    println!(
        "cargo:rustc-env={}={}",
        EQWALIZER_SUPPORT_DIR, eqwalizer_support_dir
    );
    println!("cargo:rerun-if-env-changed={}", EQWALIZER_DIR);
}

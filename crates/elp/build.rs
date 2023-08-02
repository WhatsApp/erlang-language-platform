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

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed={}", SOURCE_DATE_EPOCH);
    println!("cargo:rerun-if-env-changed={}", CI);
    println!("cargo:rustc-env=BUILD_ID={}", build_id);
}

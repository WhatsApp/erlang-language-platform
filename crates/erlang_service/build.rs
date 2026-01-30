/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    let source_directory = "../../erlang_service";
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_dir = Path::new(&out_dir).join("erlang_service");
    fs::create_dir_all(&dest_dir).unwrap();

    let dest_path = dest_dir.join("erlang_service");

    // CARGO_MANIFEST_DIR points to .../crates/erlang_service
    // Go up 2 levels to reach ELP root for buck2 target resolution
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let elp_root = Path::new(&manifest_dir).join("../..");

    let output = Command::new("buck2")
        .arg("build")
        .arg("--out")
        .arg(&dest_path)
        .arg("erlang_service:erlang_service_escript")
        .current_dir(&elp_root)
        .output()
        .expect("failed to execute buck2 build");

    if !output.status.success() {
        let stdout = String::from_utf8(output.stdout).expect("valid utf8 output from buck2 build");
        let stderr = String::from_utf8(output.stderr).expect("valid utf8 output from buck2 build");
        panic!("buck2 build failed with stdout:\n{stdout}\n\nstderr:\n{stderr}");
    }

    println!("cargo:rerun-if-changed={source_directory}/BUCK");
    println!("cargo:rerun-if-changed={source_directory}/src");
}

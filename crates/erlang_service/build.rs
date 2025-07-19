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

    if let Some(path) = env::var_os("ELP_PARSE_SERVER_ESCRIPT_PATH") {
        fs::copy(path, dest_dir.join("erlang_service"))
            .expect("Copying precompiled erlang service escript failed");
    } else {
        let profile = env::var("PROFILE").unwrap();

        let mut cmd = if cfg!(target_os = "windows") {
            let mut cmd = Command::new("cmd");
            cmd.args(["/C", "rebar3"]);
            cmd
        } else {
           Command::new("rebar3")
        };

        let output = cmd
            .arg("escriptize")
            .env("REBAR_PROFILE", &profile)
            .env("REBAR_BASE_DIR", &dest_dir)
            .current_dir(source_directory)
            .output()
            .expect("failed to execute rebar3 escriptize");

        if !output.status.success() {
            let stdout =
                String::from_utf8(output.stdout).expect("valid utf8 output from rebar3 escriptize");
            let stderr =
                String::from_utf8(output.stderr).expect("valid utf8 output from rebar3 escriptize");
            panic!("rebar3 escriptize failed with stdout:\n{stdout}\n\nstderr:\n{stderr}");
        }

        let source = dest_dir.join(profile).join("bin").join("erlang_service");
        fs::copy(source, dest_dir.join("erlang_service")).unwrap();

        println!("cargo:rerun-if-changed={source_directory}/rebar.config");
        println!("cargo:rerun-if-changed={source_directory}/src");
    }

    println!("cargo:rerun-if-env-changed=ELP_PARSE_SERVER_ESCRIPT_PATH");
}

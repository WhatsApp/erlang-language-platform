/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::path::PathBuf;

use expect_test::ExpectFile;
use expect_test::expect_file;

#[cfg(unix)]
use crate::daemon::DaemonCli;

// Snapshot dir from `ELP_RESOURCE_DIR`, set by both the Buck and cargo pipelines.
// Buck points it at a copy of the snapshots, so a run that updates them takes the
// checked-in directory from `ELP_RESOURCE_SOURCE_DIR` instead. That value is
// repo-relative, so absolutize it against CWD.
pub fn resource_dir() -> PathBuf {
    let source_dir = std::env::var("ELP_RESOURCE_SOURCE_DIR")
        .ok()
        .filter(|_| std::env::var_os("UPDATE_EXPECT").is_some());
    let dir = PathBuf::from(
        source_dir
            .or_else(|| std::env::var("ELP_RESOURCE_DIR").ok())
            .expect("ELP_RESOURCE_DIR not set"),
    );
    if dir.is_absolute() {
        dir
    } else {
        std::env::current_dir().unwrap().join(dir)
    }
}

pub fn resource_file(name: &str) -> ExpectFile {
    expect_file!(resource_dir().join(name))
}

pub fn project_path(project: &str) -> PathBuf {
    #[cfg(buck_build)]
    let test_projects = buck_resources::get("whatsapp/elp/crates/elp/test_projects")
        .unwrap()
        .join("test/test_projects");

    #[cfg(not(buck_build))]
    let test_projects = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../test/test_projects");

    let project_path = test_projects.join(project);
    dunce::canonicalize(&project_path).unwrap_or_else(|_| {
        panic!(
            "Failed to canonicalize project path: {}",
            project_path.display()
        )
    })
}

#[cfg(unix)]
pub fn capture_daemon_output<T>(run: impl FnOnce(&mut DaemonCli) -> T) -> (T, String, String) {
    use std::io::Read;
    use std::os::unix::net::UnixStream;
    use std::thread;

    use crate::daemon_protocol::DaemonResponse;

    let (server, mut client) = UnixStream::pair().unwrap();
    let reader = thread::spawn(move || {
        let mut wire = String::new();
        client
            .read_to_string(&mut wire)
            .expect("daemon output should be readable");
        wire
    });
    let mut cli = DaemonCli::client(server);
    let result = run(&mut cli);
    drop(cli);
    let wire = reader.join().expect("daemon output reader should finish");
    let mut stdout = String::new();
    let mut info = String::new();
    for line in wire.lines() {
        let response: DaemonResponse<serde_json::Value> =
            serde_json::from_str(line).expect("daemon response should be a complete record");
        match response {
            DaemonResponse::Info { message } => {
                info.push_str(&message);
                info.push('\n');
            }
            DaemonResponse::Progress { .. } => {}
            response => {
                stdout.push_str(&serde_json::to_string(&response).unwrap());
                stdout.push('\n');
            }
        }
    }
    (result, stdout, info)
}

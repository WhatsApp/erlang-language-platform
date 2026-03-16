/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! The most high-level integrated tests for elp.
//! Based on rust-analyzer at 2021-02-08 revision.
//!
//! This tests run a full LSP event loop, spawn cargo and process stdlib from
//! sysroot. For this reason, the tests here are very slow, and should be
//! avoided unless absolutely necessary.
//!
//! In particular, it's fine *not* to test that client & server agree on
//! specific JSON shapes here -- there's little value in such tests, as we can't
//! be sure without a real client anyway.

#[cfg(test)]
mod support;

use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use expect_test::expect;
use lsp_types::Position;
use lsp_types::Range;
use paths::Utf8Path;

use crate::support::code_action_project;
use crate::support::diagnostic_project;

#[test]
fn test_run_mock_lsp() {
    // Skip this test in GitHub CI - it fails there. See T251247921
    if std::env::var("GITHUB_ACTIONS").is_ok() {
        return;
    }
    if cfg!(feature = "buck") {
        let workspace_root = AbsPathBuf::assert(
            Utf8Path::new(env!("CARGO_WORKSPACE_DIR")).join("test/test_projects/end_to_end"),
        );

        // Sanity check
        assert!(std::fs::metadata(&workspace_root).is_ok());

        // This is an end to end test, mocking the a client,
        // requesting all quick fixes available ("assist").
        code_action_project(
            &workspace_root,
            r#"assist_examples/src/head_mismatch.erl"#,
            Range::new(Position::new(3, 0), Position::new(5, 10)),
            expect![[r#"
                [
                  {
                    "edit": {
                      "documentChanges": [
                        {
                          "edits": [
                            {
                              "newText": "bar",
                              "range": {
                                "end": {
                                  "character": 3,
                                  "line": 5
                                },
                                "start": {
                                  "character": 0,
                                  "line": 5
                                }
                              }
                            }
                          ],
                          "textDocument": {
                            "uri": "file:///[..]/test/test_projects/end_to_end/assist_examples/src/head_mismatch.erl",
                            "version": 0
                          }
                        }
                      ]
                    },
                    "kind": "quickfix",
                    "title": "Fix head mismatch"
                  },
                  {
                    "edit": {
                      "documentChanges": [
                        {
                          "edits": [
                            {
                              "newText": "-doc \"\"\"\n[How to write documentation](https://www.erlang.org/doc/system/documentation.html)\n\"\"\".\n-doc #{params => #{\"Arg1\" => \"Argument description\"}}.\n",
                              "range": {
                                "end": {
                                  "character": 0,
                                  "line": 3
                                },
                                "start": {
                                  "character": 0,
                                  "line": 3
                                }
                              }
                            }
                          ],
                          "textDocument": {
                            "uri": "file:///[..]/test/test_projects/end_to_end/assist_examples/src/head_mismatch.erl",
                            "version": 0
                          }
                        }
                      ]
                    },
                    "kind": "",
                    "title": "Add -doc attribute"
                  },
                  {
                    "edit": {
                      "documentChanges": [
                        {
                          "edits": [
                            {
                              "newText": "-spec bar(type1()) -> return_type().\n",
                              "range": {
                                "end": {
                                  "character": 0,
                                  "line": 3
                                },
                                "start": {
                                  "character": 0,
                                  "line": 3
                                }
                              }
                            }
                          ],
                          "textDocument": {
                            "uri": "file:///[..]/test/test_projects/end_to_end/assist_examples/src/head_mismatch.erl",
                            "version": 0
                          }
                        }
                      ]
                    },
                    "kind": "",
                    "title": "Add spec stub"
                  },
                  {
                    "edit": {
                      "documentChanges": [
                        {
                          "edits": [
                            {
                              "newText": "\n-export([bar/1]).\n",
                              "range": {
                                "end": {
                                  "character": 0,
                                  "line": 1
                                },
                                "start": {
                                  "character": 0,
                                  "line": 1
                                }
                              }
                            }
                          ],
                          "textDocument": {
                            "uri": "file:///[..]/test/test_projects/end_to_end/assist_examples/src/head_mismatch.erl",
                            "version": 0
                          }
                        }
                      ]
                    },
                    "kind": "quickfix",
                    "title": "Export the function `bar/1`"
                  }
                ]"#]],
        );
    }
}

#[test]
fn test_e2e_eqwalizer_module() {
    if cfg!(feature = "buck") {
        let workspace_root = AbsPathBuf::assert(
            Utf8Path::new(env!("CARGO_WORKSPACE_DIR")).join("test/test_projects/standard"),
        );

        // Sanity check
        assert!(std::fs::metadata(&workspace_root).is_ok());

        diagnostic_project(
            &workspace_root,
            r"app_a/src/app_a.erl",
            expect![[r#"
                {
                  "diagnostics": [
                    {
                      "code": "W0073",
                      "codeDescription": {
                        "href": "<BASE_URL>/erlang-error-index/w/W0073"
                      },
                      "message": "The `eqwalizer:fixme` comment suppresses eqwalizer type errors on the following line. Consider fixing the underlying type issue instead of suppressing it.",
                      "range": {
                        "end": {
                          "character": 21,
                          "line": 51
                        },
                        "start": {
                          "character": 4,
                          "line": 51
                        }
                      },
                      "severity": 4,
                      "source": "elp"
                    },
                    {
                      "code": "W0073",
                      "codeDescription": {
                        "href": "<BASE_URL>/erlang-error-index/w/W0073"
                      },
                      "message": "The `eqwalizer:fixme` comment suppresses eqwalizer type errors on the following line. Consider fixing the underlying type issue instead of suppressing it.",
                      "range": {
                        "end": {
                          "character": 21,
                          "line": 56
                        },
                        "start": {
                          "character": 4,
                          "line": 56
                        }
                      },
                      "severity": 4,
                      "source": "elp"
                    }
                  ],
                  "uri": "file:///[..]/test/test_projects/standard/app_a/src/app_a.erl",
                  "version": 0
                }"#]],
        );
    }
}

// This used to fail because of trigerring eqwalizer for non-modules
// Now this fails with a timeout, since we never send down notifications
// if there's no diagnostics for a file.
// #[test]
// fn test_e2e_eqwalizer_header() {
//     let workspace_root =
//         AbsPathBuf::assert(Path::new(env!("CARGO_WORKSPACE_DIR")).join("test/test_projects/standard"));

//     // Sanity check
//     assert!(std::fs::metadata(&workspace_root).is_ok());

//     diagnostic_project(
//         &workspace_root,
//         r"app_a/include/app_a.hrl",
//         expect![[r#"
//           }"#]],
//     );
// }

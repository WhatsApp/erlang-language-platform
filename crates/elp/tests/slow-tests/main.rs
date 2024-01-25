/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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
mod buck_tests;
mod support;

use std::path::Path;

use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use expect_test::expect;
use lsp_types::Position;
use lsp_types::Range;

use crate::support::code_action_project;
use crate::support::diagnostic_project;

const PROFILE: &str = "";

#[test]
fn test_run_mock_lsp() {
    if cfg!(feature = "buck") {
        let workspace_root = AbsPathBuf::assert(
            Path::new(env!("CARGO_WORKSPACE_DIR")).join("test_projects/end_to_end"),
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
                              "character": 0,
                              "line": 5
                            },
                            "start": {
                              "character": 0,
                              "line": 5
                            }
                          }
                        },
                        {
                          "newText": "",
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
                        "uri": "file:///[..]/test_projects/end_to_end/assist_examples/src/head_mismatch.erl",
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
                          "newText": "%% @doc {@link https://www.erlang.org/doc/apps/edoc/chapter.html EDoc Manual}\n%% @param Arg1 Argument description\n%% @returns Return description\n",
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
                        "uri": "file:///[..]/test_projects/end_to_end/assist_examples/src/head_mismatch.erl",
                        "version": 0
                      }
                    }
                  ]
                },
                "kind": "",
                "title": "Add edoc comment"
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
                        "uri": "file:///[..]/test_projects/end_to_end/assist_examples/src/head_mismatch.erl",
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
                        "uri": "file:///[..]/test_projects/end_to_end/assist_examples/src/head_mismatch.erl",
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
            Path::new(env!("CARGO_WORKSPACE_DIR")).join("test_projects/standard"),
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
                  "code": "incompatible_types",
                  "codeDescription": {
                    "href": "https://fb.me/eqwalizer_errors#incompatible_types"
                  },
                  "message": "`'error'`.\nExpression has type:   'error'\nContext expected type: 'ok'\n        See https://fb.me/eqwalizer_errors#incompatible_types",
                  "range": {
                    "end": {
                      "character": 7,
                      "line": 8
                    },
                    "start": {
                      "character": 4,
                      "line": 8
                    }
                  },
                  "severity": 1,
                  "source": "eqWAlizer"
                },
                {
                  "code": "incompatible_types",
                  "codeDescription": {
                    "href": "https://fb.me/eqwalizer_errors#incompatible_types"
                  },
                  "message": "`'error'`.\nExpression has type:   'error'\nContext expected type: 'ok'\n        See https://fb.me/eqwalizer_errors#incompatible_types",
                  "range": {
                    "end": {
                      "character": 9,
                      "line": 12
                    },
                    "start": {
                      "character": 4,
                      "line": 12
                    }
                  },
                  "severity": 1,
                  "source": "eqWAlizer"
                },
                {
                  "code": "incompatible_types",
                  "codeDescription": {
                    "href": "https://fb.me/eqwalizer_errors#incompatible_types"
                  },
                  "message": "`'an_atom'`.\nExpression has type:   'an_atom'\nContext expected type: number()\n        See https://fb.me/eqwalizer_errors#incompatible_types",
                  "range": {
                    "end": {
                      "character": 19,
                      "line": 16
                    },
                    "start": {
                      "character": 12,
                      "line": 16
                    }
                  },
                  "severity": 1,
                  "source": "eqWAlizer"
                },
                {
                  "code": "redundant_fixme",
                  "codeDescription": {
                    "href": "https://fb.me/eqwalizer_errors#redundant_fixme"
                  },
                  "message": "redundant fixme\n        See https://fb.me/eqwalizer_errors#redundant_fixme",
                  "range": {
                    "end": {
                      "character": 21,
                      "line": 54
                    },
                    "start": {
                      "character": 4,
                      "line": 54
                    }
                  },
                  "severity": 1,
                  "source": "eqWAlizer"
                },
                {
                  "code": "incompatible_types",
                  "codeDescription": {
                    "href": "https://fb.me/eqwalizer_errors#incompatible_types"
                  },
                  "message": "`X`.\nExpression has type:   #S{k_extra => term(), k_ok => term(), k_req1 => term(), k_req2 => term(), k_wrong1 => pid(), k_wrong2 => pid()}\nContext expected type: #S{k_ok => term(), k_req1 := atom(), k_req2 := atom(), k_req3 := atom(), k_wrong1 => atom(), k_wrong2 => atom()}\n\nThese associations do not match:\n\n  #S{\n+    k_extra  => ...\n-    k_req1   := ...\n+    k_req1   => ...\n-    k_req2   := ...\n+    k_req2   => ...\n-    k_req3   := ...\n     ...\n  }\n        See https://fb.me/eqwalizer_errors#incompatible_types",
                  "range": {
                    "end": {
                      "character": 5,
                      "line": 76
                    },
                    "start": {
                      "character": 4,
                      "line": 76
                    }
                  },
                  "severity": 1,
                  "source": "eqWAlizer"
                },
                {
                  "code": "incompatible_types",
                  "codeDescription": {
                    "href": "https://fb.me/eqwalizer_errors#incompatible_types"
                  },
                  "message": "`X`.\nExpression has type:   id(#S{a := 'va', b := #S{c := #S{d => atom()}}})\nContext expected type: #S{a := 'va', b := #S{c := id(#S{d := atom(), e := atom()})}}\n\n  id(#S{a := 'va', b := #S{c := #S{d => atom()}}}) is not compatible with #S{a := 'va', b := #S{c := id(#S{d := atom(), e := atom()})}}\n  because\n  at shape key 'b':\n  #S{a := 'va', b := #S{c := #S{d => atom()}}} is not compatible with #S{a := 'va', b := #S{c := id(#S{d := atom(), e := atom()})}}\n  because\n  at shape key 'c':\n  #S{c := #S{d => atom()}} is not compatible with #S{c := id(#S{d := atom(), e := atom()})}\n  because\n  #S{d => atom()} is not compatible with id(#S{d := atom(), e := atom()})\n        See https://fb.me/eqwalizer_errors#incompatible_types",
                  "range": {
                    "end": {
                      "character": 5,
                      "line": 100
                    },
                    "start": {
                      "character": 4,
                      "line": 100
                    }
                  },
                  "severity": 1,
                  "source": "eqWAlizer"
                },
                {
                  "code": "incompatible_types",
                  "codeDescription": {
                    "href": "https://fb.me/eqwalizer_errors#incompatible_types"
                  },
                  "message": "`X`.\nExpression has type:   id(#S{a := 'va', b := #S{c := #S{d := pid(), e := pid()}}})\nContext expected type: #S{a := 'va', b := #S{c := id(#S{d := atom(), e := atom()})}}\n\n  id(#S{a := 'va', b := #S{c := #S{d := pid(), e := pid()}}}) is not compatible with #S{a := 'va', b := #S{c := id(#S{d := atom(), e := atom()})}}\n  because\n  at shape key 'b':\n  #S{a := 'va', b := #S{c := #S{d := pid(), e := pid()}}} is not compatible with #S{a := 'va', b := #S{c := id(#S{d := atom(), e := atom()})}}\n  because\n  at shape key 'c':\n  #S{c := #S{d := pid(), e := pid()}} is not compatible with #S{c := id(#S{d := atom(), e := atom()})}\n  because\n  #S{d := pid(), e := pid()} is not compatible with id(#S{d := atom(), e := atom()})\n        See https://fb.me/eqwalizer_errors#incompatible_types",
                  "range": {
                    "end": {
                      "character": 5,
                      "line": 123
                    },
                    "start": {
                      "character": 4,
                      "line": 123
                    }
                  },
                  "severity": 1,
                  "source": "eqWAlizer"
                }
              ],
              "uri": "file:///[..]/test_projects/standard/app_a/src/app_a.erl",
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
//         AbsPathBuf::assert(Path::new(env!("CARGO_WORKSPACE_DIR")).join("test_projects/standard"));

//     // Sanity check
//     assert!(std::fs::metadata(&workspace_root).is_ok());

//     diagnostic_project(
//         &workspace_root,
//         r"app_a/include/app_a.hrl",
//         expect![[r#"
//           }"#]],
//     );
// }

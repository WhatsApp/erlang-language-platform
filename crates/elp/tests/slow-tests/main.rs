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

use elp_ide::elp_ide_db::elp_base_db::AbsPathBuf;
use expect_test::expect;
use lsp_types::Position;
use lsp_types::Range;
use paths::Utf8Path;

use crate::support::code_action_project;
use crate::support::diagnostic_project;

#[test]
fn test_run_mock_lsp() {
    if cfg!(feature = "buck") {
        let workspace_root = AbsPathBuf::assert(
            Utf8Path::new(env!("CARGO_WORKSPACE_DIR")).join("test_projects/end_to_end"),
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
                            "uri": "file:///[..]/test_projects/end_to_end/assist_examples/src/head_mismatch.erl",
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
            Utf8Path::new(env!("CARGO_WORKSPACE_DIR")).join("test_projects/standard"),
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
                      "message": "`X`.\nExpression has type:   #{k_extra => term(), k_ok => term(), k_req1 => term(), k_req2 => term(), k_wrong1 => pid(), k_wrong2 => pid()}\nContext expected type: #{k_ok => term(), k_req1 := atom(), k_req2 := atom(), k_req3 := atom(), k_wrong1 => atom(), k_wrong2 => atom()}\n\nBecause in the expression's type:\n  Here the type is:     #{k_req2 => ..., k_req1 => ..., ...}\n  Context expects type: #{k_req3 := ..., k_req2 := ..., k_req1 := ..., ...}\n  The type of the expression is missing the following required keys: k_req3, k_req2, k_req1.\n\n------------------------------ Detailed message ------------------------------\n\nkeys `k_req1`, `k_req2`, `k_req3` are declared as required in the latter but not in the former\n        See https://fb.me/eqwalizer_errors#incompatible_types",
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
                      "message": "`X`.\nExpression has type:   id(#{a := 'va', b := #{c := #{d => atom()}}})\nContext expected type: #{a := 'va', b := #{c := id(#{d := atom(), e := atom()})}}\n\nBecause in the expression's type:\n  #{ b =>\n    #{ c =>\n      Here the type is:     #{d => ..., ...}\n      Context expects type: #{d := ..., e := ..., ...}\n      The type of the expression is missing the following required keys: d, e.\n    , ... }\n  , ... }\n\n------------------------------ Detailed message ------------------------------\n\n  id(#{a := 'va', b := #{c := #{d => atom()}}}) is not compatible with #{a := 'va', b := #{c := id(#{d := atom(), e := atom()})}}\n  because\n  #{a := 'va', b := #{c := #{d => atom()}}} is not compatible with #{a := 'va', b := #{c := id(#{d := atom(), e := atom()})}}\n  because\n  at key `b`:\n  #{a := 'va', b := #{c := #{d => atom()}}} is not compatible with #{a := 'va', b := #{c := id(#{d := atom(), e := atom()})}}\n  because\n  #{c := #{d => atom()}} is not compatible with #{c := id(#{d := atom(), e := atom()})}\n        See https://fb.me/eqwalizer_errors#incompatible_types",
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
                      "message": "`X`.\nExpression has type:   id(#{a := 'va', b := #{c := #{d := pid(), e := pid()}}})\nContext expected type: #{a := 'va', b := #{c := id(#{d := atom(), e := atom()})}}\n\nBecause in the expression's type:\n  #{ b =>\n    #{ c =>\n      #{ d =>\n        Here the type is:     pid()\n        Context expects type: atom()\n      , ... }\n    , ... }\n  , ... }\n\n------------------------------ Detailed message ------------------------------\n\n  id(#{a := 'va', b := #{c := #{d := pid(), e := pid()}}}) is not compatible with #{a := 'va', b := #{c := id(#{d := atom(), e := atom()})}}\n  because\n  #{a := 'va', b := #{c := #{d := pid(), e := pid()}}} is not compatible with #{a := 'va', b := #{c := id(#{d := atom(), e := atom()})}}\n  because\n  at key `b`:\n  #{a := 'va', b := #{c := #{d := pid(), e := pid()}}} is not compatible with #{a := 'va', b := #{c := id(#{d := atom(), e := atom()})}}\n  because\n  #{c := #{d := pid(), e := pid()}} is not compatible with #{c := id(#{d := atom(), e := atom()})}\n        See https://fb.me/eqwalizer_errors#incompatible_types",
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

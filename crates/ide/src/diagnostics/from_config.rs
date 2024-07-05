/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Read a section of the config file and generate diagnostics from it.

use elp_ide_db::elp_base_db::FileId;
use hir::Semantic;
use serde::Deserialize;
use serde::Serialize;

use super::replace_call;
use super::replace_call::Replacement;
use super::Diagnostic;
use crate::codemod_helpers::FunctionMatch;

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
pub struct LintsFromConfig {
    pub lints: Vec<Lint>,
}

impl LintsFromConfig {
    pub fn get_diagnostics(&self, acc: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
        self.lints
            .iter()
            .for_each(|l| l.get_diagnostics(acc, sema, file_id));
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(tag = "type")]
pub enum Lint {
    ReplaceCall(ReplaceCall),
}

impl Lint {
    pub fn get_diagnostics(&self, acc: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
        match self {
            Lint::ReplaceCall(l) => l.get_diagnostics(acc, sema, file_id),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct ReplaceCall {
    pub matcher: FunctionMatch,
    pub action: ReplaceCallAction,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(tag = "action")]
pub enum ReplaceCallAction {
    Replace(Replacement),
    RemoveFromList,
}

impl ReplaceCall {
    pub fn get_diagnostics(&self, acc: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
        match &self.action {
            ReplaceCallAction::Replace(replacement) => replace_call::replace_call_site(
                &self.matcher,
                replacement.clone(),
                &replace_call::adhoc_diagnostic,
                acc,
                sema,
                file_id,
            ),
            ReplaceCallAction::RemoveFromList => replace_call::remove_fun_ref_from_list(
                &self.matcher,
                &replace_call::adhoc_diagnostic,
                acc,
                sema,
                file_id,
            ),
        }
    }
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::Lint;
    use super::LintsFromConfig;
    use super::ReplaceCall;
    use super::ReplaceCallAction;
    use crate::codemod_helpers::FunctionMatch;
    use crate::codemod_helpers::MFA;
    use crate::diagnostics::replace_call::Replacement;

    #[test]
    fn serde_serialize_function_match_mfa() {
        expect![[r#"
            type = "MFA"
            mfa = "mod:fun/3"
        "#]]
        .assert_eq(
            &toml::to_string::<FunctionMatch>(&FunctionMatch::MFA {
                mfa: MFA::new("mod", "fun", 3),
            })
            .unwrap(),
        );
    }

    #[test]
    fn serde_deserialize_function_match_mfa() {
        let function_match: FunctionMatch = toml::from_str(
            r#"
              type = "MFA"
              mfa = "mod:fun/3"
             "#,
        )
        .unwrap();

        expect![[r#"
            MFA {
                mfa: MFA {
                    module: "mod",
                    name: "fun",
                    arity: 3,
                },
            }
        "#]]
        .assert_debug_eq(&function_match);
    }

    #[test]
    fn serde_deserialize_function_match_mfa_arity_bogus() {
        expect![[r#"
            Err(
                Error {
                    inner: ErrorInner {
                        kind: Custom,
                        line: None,
                        col: 0,
                        at: None,
                        message: "invalid MFA 'mod:fun/x'",
                        key: [],
                    },
                },
            )
        "#]]
        .assert_debug_eq(&toml::from_str::<FunctionMatch>(
            r#"
              type = "MFA"
              mfa = "mod:fun/x"
             "#,
        ));
    }

    #[test]
    fn serde_deserialize_function_match_mfa_bad_format() {
        expect![[r#"
            Err(
                Error {
                    inner: ErrorInner {
                        kind: Custom,
                        line: None,
                        col: 0,
                        at: None,
                        message: "invalid MFA 'junk'",
                        key: [],
                    },
                },
            )
        "#]]
        .assert_debug_eq(&toml::from_str::<FunctionMatch>(
            r#"
              type = "MFA"
              mfa = "junk"
             "#,
        ));
    }

    #[test]
    fn serde_serialize_function_match_mf() {
        expect![[r#"
            type = "MF"
            module = "mod"
            name = "fun"
        "#]]
        .assert_eq(&toml::to_string::<FunctionMatch>(&FunctionMatch::mf("mod", "fun")).unwrap());
    }

    #[test]
    fn serde_serialize_replace_call() {
        expect![[r#"
            [matcher]
            type = "MF"
            module = "mod_a"
            name = "func"

            [action]
            action = "Replace"
            type = "UseOk"
        "#]]
        .assert_eq(
            &toml::to_string::<ReplaceCall>(&ReplaceCall {
                matcher: FunctionMatch::mf("mod_a", "func"),
                action: ReplaceCallAction::Replace(Replacement::UseOk),
            })
            .unwrap(),
        );
    }

    #[test]
    fn serde_deserialize_replace_call() {
        let replace_call: ReplaceCall = toml::from_str(
            r#"
                [matcher]
                type = "MF"
                module = "mod_a"
                name = "func"

                [action]
                action = "Replace"
                type = "UseOk"
             "#,
        )
        .unwrap();

        expect![[r#"
            ReplaceCall {
                matcher: MF {
                    module: "mod_a",
                    name: "func",
                },
                action: Replace(
                    UseOk,
                ),
            }
        "#]]
        .assert_debug_eq(&replace_call);
    }

    #[test]
    fn serde_serialize_replace_call_use_call_arg() {
        expect![[r#"
            [matcher]
            type = "M"
            module = "mod_a"

            [action]
            action = "Replace"
            type = "UseCallArg"
            n = 5
        "#]]
        .assert_eq(
            &toml::to_string::<ReplaceCall>(&ReplaceCall {
                matcher: FunctionMatch::m("mod_a"),
                action: ReplaceCallAction::Replace(Replacement::UseCallArg { n: 5 }),
            })
            .unwrap(),
        );
    }

    #[test]
    fn serde_serialize_replace_call_invocation() {
        expect![[r#"
            [matcher]
            type = "M"
            module = "mod_a"

            [action]
            action = "Replace"
            type = "Invocation"
            replacement = "modu:fn"
        "#]]
        .assert_eq(
            &toml::to_string::<ReplaceCall>(&ReplaceCall {
                matcher: FunctionMatch::m("mod_a"),
                action: ReplaceCallAction::Replace(Replacement::Invocation {
                    replacement: "modu:fn".to_owned(),
                }),
            })
            .unwrap(),
        );
    }

    #[test]
    fn serde_serialize_replace_call_permutation() {
        expect![[r#"
            [matcher]
            type = "M"
            module = "mod_a"

            [action]
            action = "Replace"
            type = "ArgsPermutation"
            perm = [1, 2, 3]
        "#]]
        .assert_eq(
            &toml::to_string::<ReplaceCall>(&ReplaceCall {
                matcher: FunctionMatch::m("mod_a"),
                action: ReplaceCallAction::Replace(Replacement::ArgsPermutation {
                    perm: vec![1, 2, 3],
                }),
            })
            .unwrap(),
        );
    }

    #[test]
    fn serde_serialize_lints_from_config() {
        let result = toml::to_string::<LintsFromConfig>(&LintsFromConfig {
            lints: vec![Lint::ReplaceCall(ReplaceCall {
                matcher: FunctionMatch::mf("mod_a", "func"),
                action: ReplaceCallAction::Replace(Replacement::UseOk),
            })],
        })
        .unwrap();
        expect![[r#"
            [[lints]]
            type = "ReplaceCall"

            [lints.matcher]
            type = "MF"
            module = "mod_a"
            name = "func"

            [lints.action]
            action = "Replace"
            type = "UseOk"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_deserialize_lints_from_config() {
        let lints: LintsFromConfig = toml::from_str(
            r#"
            [[lints]]
            type = "ReplaceCall"

            [lints.matcher]
            type = "MF"
            module = "mod_a"
            name = "func"

            [lints.action]
            action = "Replace"
            type = "UseOk"
             "#,
        )
        .unwrap();

        expect![[r#"
            LintsFromConfig {
                lints: [
                    ReplaceCall(
                        ReplaceCall {
                            matcher: MF {
                                module: "mod_a",
                                name: "func",
                            },
                            action: Replace(
                                UseOk,
                            ),
                        },
                    ),
                ],
            }
        "#]]
        .assert_debug_eq(&lints);
    }
}

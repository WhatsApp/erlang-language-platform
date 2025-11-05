/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Read a section of the config file and generate diagnostics from it.

use elp_ide_db::DiagnosticCode;
use elp_ide_db::RootDatabase;
use elp_ide_db::elp_base_db::FileId;
use elp_ide_ssr::SsrRule;
use elp_ide_ssr::SsrSearchScope;
use elp_ide_ssr::match_pattern;
use hir::Semantic;
use hir::Strategy;
use hir::fold::MacroStrategy;
use hir::fold::ParenStrategy;
use serde::Deserialize;
use serde::Serialize;

use super::Diagnostic;
use super::Severity;
use super::TypeReplacement;
use super::replace_call;
use super::replace_call::Replacement;
use super::replace_in_spec;
use crate::MFA;
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
    ReplaceInSpec(ReplaceInSpec),
    LintMatchSsr(MatchSsr),
}

impl Lint {
    pub fn get_diagnostics(&self, acc: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
        match self {
            Lint::ReplaceCall(l) => l.get_diagnostics(acc, sema, file_id),
            Lint::ReplaceInSpec(l) => l.get_diagnostics(acc, sema, file_id),
            Lint::LintMatchSsr(l) => l.get_diagnostics(acc, sema, file_id),
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

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct ReplaceInSpec {
    pub functions: Vec<MFA>,
    pub action: ReplaceInSpecAction,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(tag = "action")]
pub enum ReplaceInSpecAction {
    Replace(TypeReplacement),
}

impl ReplaceInSpec {
    pub fn get_diagnostics(&self, acc: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
        match &self.action {
            ReplaceInSpecAction::Replace(replace) => match replace {
                TypeReplacement::TypeAliasWithString { from, to } => {
                    replace_in_spec::replace_in_spec(&self.functions, from, to, acc, sema, file_id)
                }
            },
        }
    }
}

// ---------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct MatchSsr {
    pub ssr_pattern: String,
    pub message: Option<String>,
    pub strategy: Option<Strategy>,
    pub severity: Option<Severity>,
}

impl Serialize for MatchSsr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;

        let mut state = serializer.serialize_struct("MatchSsr", 5)?;
        state.serialize_field("ssr_pattern", &self.ssr_pattern)?;
        if let Some(ref message) = self.message {
            state.serialize_field("message", message)?;
        }
        if let Some(ref severity) = self.severity {
            state.serialize_field("severity", severity)?;
        }
        if let Some(strategy) = self.strategy {
            // Default strategy is Expand and InvisibleParens
            let is_default = strategy.macros == MacroStrategy::Expand
                && strategy.parens == ParenStrategy::InvisibleParens;

            // Only serialize strategy if it's not the default
            if !is_default {
                // Serialize macro strategy
                let macro_strategy = match strategy.macros {
                    MacroStrategy::DoNotExpand => "no-expand",
                    MacroStrategy::Expand => "expand",
                    MacroStrategy::ExpandButIncludeMacroCall => "visible-expand",
                };
                state.serialize_field("macro_strategy", macro_strategy)?;

                // Serialize paren strategy
                let paren_strategy = match strategy.parens {
                    ParenStrategy::VisibleParens => "visible",
                    ParenStrategy::InvisibleParens => "invisible",
                };
                state.serialize_field("paren_strategy", paren_strategy)?;
            }
        }
        state.end()
    }
}

impl<'de> Deserialize<'de> for MatchSsr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;

        #[derive(Deserialize)]
        struct MatchSsrHelper {
            ssr_pattern: String,
            #[serde(default)]
            message: Option<String>,
            #[serde(default)]
            severity: Option<Severity>,
            #[serde(default)]
            macro_strategy: Option<String>,
            #[serde(default)]
            paren_strategy: Option<String>,
        }

        let helper = MatchSsrHelper::deserialize(deserializer)?;

        // Validate the SSR pattern by trying to parse it
        // Use a minimal database for validation
        let db = RootDatabase::default();
        SsrRule::parse_str(&db, &helper.ssr_pattern).map_err(|e| {
            D::Error::custom(format!(
                "invalid SSR pattern '{}': {}",
                helper.ssr_pattern, e
            ))
        })?;

        // Parse strategy from strings if provided
        let strategy = if helper.macro_strategy.is_some() || helper.paren_strategy.is_some() {
            let macros = match helper.macro_strategy.as_deref() {
                Some("no-expand") => MacroStrategy::DoNotExpand,
                Some("expand") | None => MacroStrategy::Expand,
                Some("visible-expand") => MacroStrategy::ExpandButIncludeMacroCall,
                Some(s) => {
                    return Err(D::Error::custom(format!(
                        "invalid macro strategy '{}'. Valid options are: expand, no-expand, visible-expand",
                        s
                    )));
                }
            };

            let parens = match helper.paren_strategy.as_deref() {
                Some("visible") => ParenStrategy::VisibleParens,
                Some("invisible") | None => ParenStrategy::InvisibleParens,
                Some(s) => {
                    return Err(D::Error::custom(format!(
                        "invalid paren strategy '{}'. Valid options are: visible, invisible",
                        s
                    )));
                }
            };

            Some(Strategy { macros, parens })
        } else {
            None
        };

        Ok(MatchSsr {
            ssr_pattern: helper.ssr_pattern,
            message: helper.message,
            strategy,
            severity: helper.severity,
        })
    }
}

impl MatchSsr {
    pub fn get_diagnostics(&self, acc: &mut Vec<Diagnostic>, sema: &Semantic, file_id: FileId) {
        let strategy = self.strategy.unwrap_or(Strategy {
            macros: MacroStrategy::Expand,
            parens: ParenStrategy::InvisibleParens,
        });

        let scope = SsrSearchScope::WholeFile(file_id);
        let matches = match_pattern(sema, strategy, &self.ssr_pattern, scope);

        for matched in matches.matches {
            let message = self
                .message
                .clone()
                .unwrap_or_else(|| format!("SSR pattern matched: {}", self.ssr_pattern));

            let severity = self.severity.unwrap_or(Severity::WeakWarning);
            let diag = Diagnostic::new(
                DiagnosticCode::AdHoc("ssr-match".to_string()),
                message,
                matched.range.range,
            )
            .with_severity(severity);
            acc.push(diag);
        }
    }
}

// ---------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use hir::Strategy;
    use hir::fold::MacroStrategy;
    use hir::fold::ParenStrategy;

    use super::Lint;
    use super::LintsFromConfig;
    use super::MatchSsr;
    use super::ReplaceCall;
    use super::ReplaceCallAction;
    use super::ReplaceInSpec;
    use super::ReplaceInSpecAction;
    use crate::codemod_helpers::FunctionMatch;
    use crate::codemod_helpers::MFA;
    use crate::diagnostics::TypeReplacement;
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
    fn serde_serialize_replace_in_spec() {
        expect![[r#"
            functions = ["modu:fn/3"]

            [action]
            action = "Replace"
            type = "TypeAliasWithString"
            from = "modu:one/0"
            to = "modu:other()"
        "#]]
        .assert_eq(
            &toml::to_string::<ReplaceInSpec>(&ReplaceInSpec {
                functions: vec!["modu:fn/3".try_into().unwrap()],
                action: ReplaceInSpecAction::Replace(TypeReplacement::TypeAliasWithString {
                    from: "modu:one/0".try_into().unwrap(),
                    to: "modu:other()".to_string(),
                }),
            })
            .unwrap(),
        );
    }
    #[test]
    fn serde_deserialize_replace_in_spec() {
        let replace_in_spec: ReplaceInSpec = toml::from_str(
            r#"
              functions = ["modu:fn/3"]

              [action]
              action = "Replace"
              type = "TypeAliasWithString"
              from = "modu:one/0"
              to = "modu:other()"
             "#,
        )
        .unwrap();

        expect![[r#"
            ReplaceInSpec {
                functions: [
                    MFA {
                        module: "modu",
                        name: "fn",
                        arity: 3,
                    },
                ],
                action: Replace(
                    TypeAliasWithString {
                        from: MFA {
                            module: "modu",
                            name: "one",
                            arity: 0,
                        },
                        to: "modu:other()",
                    },
                ),
            }
        "#]]
        .assert_debug_eq(&replace_in_spec);
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

    #[test]
    fn serde_serialize_lints_from_config_2() {
        let result = toml::to_string::<LintsFromConfig>(&LintsFromConfig {
            lints: vec![Lint::ReplaceInSpec(ReplaceInSpec {
                functions: vec!["modu:fn/3".try_into().unwrap()],
                action: ReplaceInSpecAction::Replace(TypeReplacement::TypeAliasWithString {
                    from: "modu:one/0".try_into().unwrap(),
                    to: "modu:other()".to_string(),
                }),
            })],
        })
        .unwrap();
        expect![[r#"
            [[lints]]
            type = "ReplaceInSpec"
            functions = ["modu:fn/3"]

            [lints.action]
            action = "Replace"
            type = "TypeAliasWithString"
            from = "modu:one/0"
            to = "modu:other()"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_serialize_match_ssr() {
        let result = toml::to_string::<MatchSsr>(&MatchSsr {
            ssr_pattern: "ssr: _@A = 10.".to_string(),
            message: Some("Found pattern".to_string()),
            strategy: None,
            severity: None,
        })
        .unwrap();
        expect![[r#"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_deserialize_match_ssr() {
        let match_ssr: MatchSsr = toml::from_str(
            r#"
              ssr_pattern = "ssr: _@A = 10."
              message = "Found pattern"
             "#,
        )
        .unwrap();

        expect![[r#"
            MatchSsr {
                ssr_pattern: "ssr: _@A = 10.",
                message: Some(
                    "Found pattern",
                ),
                strategy: None,
                severity: None,
            }
        "#]]
        .assert_debug_eq(&match_ssr);
    }

    #[test]
    fn serde_serialize_lint_match_ssr() {
        let result = toml::to_string::<LintsFromConfig>(&LintsFromConfig {
            lints: vec![Lint::LintMatchSsr(MatchSsr {
                ssr_pattern: "ssr: _@A = 10.".to_string(),
                message: Some("Found pattern".to_string()),
                strategy: None,
                severity: None,
            })],
        })
        .unwrap();
        expect![[r#"
            [[lints]]
            type = "LintMatchSsr"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_serialize_lint_match_ssr_with_strategy() {
        let result = toml::to_string::<LintsFromConfig>(&LintsFromConfig {
            lints: vec![Lint::LintMatchSsr(MatchSsr {
                ssr_pattern: "ssr: _@A = 10.".to_string(),
                message: Some("Found pattern".to_string()),
                strategy: Some(Strategy {
                    macros: MacroStrategy::Expand,
                    parens: ParenStrategy::InvisibleParens,
                }),
                severity: None,
            })],
        })
        .unwrap();
        expect![[r#"
            [[lints]]
            type = "LintMatchSsr"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_serialize_lint_match_ssr_with_non_default_strategy() {
        let result = toml::to_string::<LintsFromConfig>(&LintsFromConfig {
            lints: vec![Lint::LintMatchSsr(MatchSsr {
                ssr_pattern: "ssr: _@A = 10.".to_string(),
                message: Some("Found pattern".to_string()),
                strategy: Some(Strategy {
                    macros: MacroStrategy::DoNotExpand,
                    parens: ParenStrategy::VisibleParens,
                }),
                severity: None,
            })],
        })
        .unwrap();
        expect![[r#"
            [[lints]]
            type = "LintMatchSsr"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
            macro_strategy = "no-expand"
            paren_strategy = "visible"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_deserialize_lint_match_ssr_with_strategy() {
        let match_ssr: LintsFromConfig = toml::from_str(
            r#"
              [[lints]]
              type = "LintMatchSsr"
              ssr_pattern = "ssr: _@A = 10."
              message = "Found pattern"
              macro_strategy = "visible-expand"
              paren_strategy = "visible"
             "#,
        )
        .unwrap();

        expect![[r#"
            LintsFromConfig {
                lints: [
                    LintMatchSsr(
                        MatchSsr {
                            ssr_pattern: "ssr: _@A = 10.",
                            message: Some(
                                "Found pattern",
                            ),
                            strategy: Some(
                                Strategy {
                                    macros: ExpandButIncludeMacroCall,
                                    parens: VisibleParens,
                                },
                            ),
                            severity: None,
                        },
                    ),
                ],
            }
        "#]]
        .assert_debug_eq(&match_ssr);
    }

    #[test]
    fn serde_serialize_match_ssr_with_severity_error() {
        use crate::diagnostics::Severity;
        let result = toml::to_string::<MatchSsr>(&MatchSsr {
            ssr_pattern: "ssr: _@A = 10.".to_string(),
            message: Some("Found pattern".to_string()),
            strategy: None,
            severity: Some(Severity::Error),
        })
        .unwrap();
        expect![[r#"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
            severity = "error"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_serialize_match_ssr_with_severity_warning() {
        use crate::diagnostics::Severity;
        let result = toml::to_string::<MatchSsr>(&MatchSsr {
            ssr_pattern: "ssr: _@A = 10.".to_string(),
            message: Some("Found pattern".to_string()),
            strategy: None,
            severity: Some(Severity::Warning),
        })
        .unwrap();
        expect![[r#"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
            severity = "warning"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_serialize_match_ssr_with_severity_weak() {
        use crate::diagnostics::Severity;
        let result = toml::to_string::<MatchSsr>(&MatchSsr {
            ssr_pattern: "ssr: _@A = 10.".to_string(),
            message: Some("Found pattern".to_string()),
            strategy: None,
            severity: Some(Severity::WeakWarning),
        })
        .unwrap();
        expect![[r#"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
            severity = "weak"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_serialize_match_ssr_with_severity_info() {
        use crate::diagnostics::Severity;
        let result = toml::to_string::<MatchSsr>(&MatchSsr {
            ssr_pattern: "ssr: _@A = 10.".to_string(),
            message: Some("Found pattern".to_string()),
            strategy: None,
            severity: Some(Severity::Information),
        })
        .unwrap();
        expect![[r#"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
            severity = "info"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_deserialize_match_ssr_with_severity() {
        use crate::diagnostics::Severity;
        let match_ssr: MatchSsr = toml::from_str(
            r#"
              ssr_pattern = "ssr: _@A = 10."
              message = "Found pattern"
              severity = "error"
             "#,
        )
        .unwrap();

        assert_eq!(match_ssr.severity, Some(Severity::Error));
    }

    #[test]
    fn serde_deserialize_lint_match_ssr_with_severity() {
        use crate::diagnostics::Severity;
        let lints: LintsFromConfig = toml::from_str(
            r#"
              [[lints]]
              type = "LintMatchSsr"
              ssr_pattern = "ssr: _@A = 10."
              message = "Found pattern"
              severity = "warning"
             "#,
        )
        .unwrap();

        match &lints.lints[0] {
            Lint::LintMatchSsr(match_ssr) => {
                assert_eq!(match_ssr.severity, Some(Severity::Warning));
            }
            _ => panic!("Expected LintMatchSsr"),
        }
    }

    #[test]
    fn serde_serialize_lint_match_ssr_with_severity_and_strategy() {
        use crate::diagnostics::Severity;
        let result = toml::to_string::<LintsFromConfig>(&LintsFromConfig {
            lints: vec![Lint::LintMatchSsr(MatchSsr {
                ssr_pattern: "ssr: _@A = 10.".to_string(),
                message: Some("Found pattern".to_string()),
                strategy: Some(Strategy {
                    macros: MacroStrategy::DoNotExpand,
                    parens: ParenStrategy::VisibleParens,
                }),
                severity: Some(Severity::Error),
            })],
        })
        .unwrap();
        expect![[r#"
            [[lints]]
            type = "LintMatchSsr"
            ssr_pattern = "ssr: _@A = 10."
            message = "Found pattern"
            severity = "error"
            macro_strategy = "no-expand"
            paren_strategy = "visible"
        "#]]
        .assert_eq(&result);
    }

    #[test]
    fn serde_deserialize_lint_match_ssr_with_severity_and_strategy() {
        let lints: LintsFromConfig = toml::from_str(
            r#"
              [[lints]]
              type = "LintMatchSsr"
              ssr_pattern = "ssr: _@A = 10."
              message = "Found pattern"
              severity = "error"
              macro_strategy = "no-expand"
              paren_strategy = "visible"
             "#,
        )
        .unwrap();

        expect![[r#"
            LintsFromConfig {
                lints: [
                    LintMatchSsr(
                        MatchSsr {
                            ssr_pattern: "ssr: _@A = 10.",
                            message: Some(
                                "Found pattern",
                            ),
                            strategy: Some(
                                Strategy {
                                    macros: DoNotExpand,
                                    parens: VisibleParens,
                                },
                            ),
                            severity: Some(
                                Error,
                            ),
                        },
                    ),
                ],
            }
        "#]]
        .assert_debug_eq(&lints);
    }
}

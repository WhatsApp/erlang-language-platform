/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::str::FromStr;

use fxhash::FxHashMap;
use lazy_static::lazy_static;
use regex::Regex;
use serde::de;
use serde::Deserialize;
use serde::Deserializer;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

// @fb-only: use crate::meta_only::MetaOnlyDiagnosticCode;

// @fb-only: pub const BASE_URL: &str = crate::meta_only::BASE_URL;
pub const BASE_URL: &str = "https://whatsapp.github.io/erlang-language-platform/docs"; // @oss-only

#[derive(Clone, Debug, PartialEq, Eq, Hash, EnumIter)]
pub enum DiagnosticCode {
    DefaultCodeForEnumIter,
    HeadMismatch,
    MissingModule,
    ModuleMismatch,
    UnusedInclude,
    BoundVarInPattern,
    UnusedMacro,
    UnusedRecordField,
    MutableVarBug,
    SyntaxError,
    Missing(String),
    StatementHasNoEffect,
    TrivialMatch,
    UnusedFunctionArg,
    RedundantAssignment,
    UnreachableTest,
    ApplicationGetEnv,
    MissingCompileWarnMissingSpec,
    MisspelledAttribute,
    CrossNodeEval,
    DependentHeader,
    DeprecatedFunction,
    UndefinedFunction,
    Unexpected(String),
    ExpressionCanBeSimplified,
    CannotEvaluateCTCallbacks,
    MeckMissingNoLinkInInitPerSuite,
    AtomsExhaustion,
    SlowFunction,

    // Wrapper for erlang service diagnostic codes
    ErlangService(String),
    // Wrapper for EqWAlizer diagnostic codes
    Eqwalizer(String),
    // Used for ad-hoc diagnostics via lints/codemods
    AdHoc(String),
    // @fb-only: MetaOnly(MetaOnlyDiagnosticCode),
}

impl<'de> Deserialize<'de> for DiagnosticCode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        FromStr::from_str(&s).map_err(de::Error::custom)
    }
}

impl serde::Serialize for DiagnosticCode {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_code().as_str())
    }
}

impl Default for DiagnosticCode {
    fn default() -> Self {
        DiagnosticCode::DefaultCodeForEnumIter
    }
}

impl DiagnosticCode {
    pub fn as_code(&self) -> String {
        match self {
            DiagnosticCode::DefaultCodeForEnumIter => "DEFAULT-UNUSED-CONSTRUCTOR".to_string(),
            DiagnosticCode::MissingModule => "L1201".to_string(),
            DiagnosticCode::HeadMismatch => "P1700".to_string(), // "head-mismatch"
            DiagnosticCode::SyntaxError => "P1711".to_string(),
            DiagnosticCode::BoundVarInPattern => "W0000".to_string(),
            DiagnosticCode::ModuleMismatch => "W0001".to_string(), // "module-mismatch"
            DiagnosticCode::UnusedMacro => "W0002".to_string(),    // "unused-macro"
            DiagnosticCode::UnusedRecordField => "W0003".to_string(), // unused-record-field
            DiagnosticCode::Missing(_) => "W0004".to_string(), // epp had missing_comma and missing_parenthesis
            DiagnosticCode::MutableVarBug => "W0005".to_string(), // mutable-variable
            DiagnosticCode::StatementHasNoEffect => "W0006".to_string(), // statement-has-no-effect
            DiagnosticCode::TrivialMatch => "W0007".to_string(), // trivial-match
            DiagnosticCode::UnreachableTest => "W0008".to_string(),
            DiagnosticCode::RedundantAssignment => "W0009".to_string(), // redundant-assignment
            DiagnosticCode::UnusedFunctionArg => "W0010".to_string(),   // unused-function-arg
            DiagnosticCode::ApplicationGetEnv => "W0011".to_string(),   // application_get_env
            DiagnosticCode::MissingCompileWarnMissingSpec => "W0012".to_string(),
            DiagnosticCode::MisspelledAttribute => "W0013".to_string(), // misspelled-attribute
            DiagnosticCode::CrossNodeEval => "W0014".to_string(),       // cross-node-eval
            DiagnosticCode::DependentHeader => "W0015".to_string(),     // dependent-header
            DiagnosticCode::DeprecatedFunction => "W0016".to_string(),  // deprecated-function
            DiagnosticCode::UndefinedFunction => "W0017".to_string(),   // undefined-function
            DiagnosticCode::Unexpected(_) => "W0018".to_string(), // unexpected_semi, unexpected_dot
            DiagnosticCode::ExpressionCanBeSimplified => "W0019".to_string(), // expression-can-be-simplified
            DiagnosticCode::UnusedInclude => "W0020".to_string(), // Unused include (previously known as L1500 due to a bug)
            DiagnosticCode::CannotEvaluateCTCallbacks => "W0021".to_string(),
            DiagnosticCode::MeckMissingNoLinkInInitPerSuite => "W0022".to_string(),
            DiagnosticCode::AtomsExhaustion => "W0023".to_string(),
            DiagnosticCode::SlowFunction => "W0024".to_string(),
            DiagnosticCode::ErlangService(c) => c.to_string(),
            DiagnosticCode::Eqwalizer(c) => format!("eqwalizer: {c}"),
            DiagnosticCode::AdHoc(c) => format!("ad-hoc: {c}"),
            // @fb-only: DiagnosticCode::MetaOnly(c) => c.as_code(),
        }
    }

    pub fn as_label(&self) -> String {
        match self {
            DiagnosticCode::DefaultCodeForEnumIter => "DEFAULT-UNUSED-CONSTRUCTOR".to_string(),
            DiagnosticCode::MissingModule => "missing_module".to_string(),
            DiagnosticCode::UnusedInclude => "unused_include".to_string(),
            DiagnosticCode::HeadMismatch => "head_mismatch".to_string(),
            DiagnosticCode::SyntaxError => "syntax_error".to_string(),
            DiagnosticCode::BoundVarInPattern => "bound_var_in_pattern".to_string(),
            DiagnosticCode::ModuleMismatch => "module_mismatch".to_string(),
            DiagnosticCode::UnusedMacro => "unused_macro".to_string(),
            DiagnosticCode::UnusedRecordField => "unused_record_field".to_string(),
            DiagnosticCode::Missing(_) => "missing_comma_or_parenthesis".to_string(),
            DiagnosticCode::MutableVarBug => "mutable_variable_bug".to_string(),
            DiagnosticCode::StatementHasNoEffect => "statement_has_no_effect".to_string(),
            DiagnosticCode::TrivialMatch => "trivial_match".to_string(),
            DiagnosticCode::UnusedFunctionArg => "unused_function_arg".to_string(),
            DiagnosticCode::RedundantAssignment => "redundant_assignment".to_string(),
            DiagnosticCode::UnreachableTest => "unreachable_test".to_string(),
            DiagnosticCode::CannotEvaluateCTCallbacks => "cannot_evaluate_ct_callbacks".to_string(),
            DiagnosticCode::MeckMissingNoLinkInInitPerSuite => {
                "meck_missing_no_link_in_init_per_suite".to_string()
            }
            DiagnosticCode::AtomsExhaustion => "atoms_exhaustion".to_string(),
            DiagnosticCode::SlowFunction => "slow_function".to_string(),
            DiagnosticCode::MissingCompileWarnMissingSpec => {
                // Match the name in the original
                "compile-warn-missing-spec".to_string()
            }
            DiagnosticCode::ApplicationGetEnv => "application_get_env".to_string(),
            DiagnosticCode::MisspelledAttribute => "misspelled_attribute".to_string(),
            DiagnosticCode::CrossNodeEval => "cross_node_eval".to_string(),
            DiagnosticCode::DependentHeader => "dependent_header".to_string(),
            DiagnosticCode::DeprecatedFunction => "deprecated_function".to_string(),
            DiagnosticCode::UndefinedFunction => "undefined_function".to_string(),
            DiagnosticCode::Unexpected(_) => "unexpected_semi_or_dot".to_string(),
            DiagnosticCode::ExpressionCanBeSimplified => "expression_can_be_simplified".to_string(),
            DiagnosticCode::ErlangService(c) => c.to_string(),
            DiagnosticCode::Eqwalizer(c) => c.to_string(),
            DiagnosticCode::AdHoc(c) => format!("ad-hoc: {c}"),
            // @fb-only: DiagnosticCode::MetaOnly(c) => c.as_label(),
        }
    }

    pub fn as_labeled_code(&self) -> String {
        format!("{} ({})", self.as_code(), self.as_label())
    }

    pub fn maybe_from_string(s: &str) -> Option<DiagnosticCode> {
        DIAGNOSTIC_CODE_LOOKUPS
            .get(s).cloned()
            // @fb-only: .or_else(|| MetaOnlyDiagnosticCode::from_str(s).ok().map(DiagnosticCode::MetaOnly))
            .or_else( ||
                // Look for ErlangService and AdHoc
                if let Some(code) = Self::is_adhoc(s) {
                    Some(DiagnosticCode::AdHoc(code))
                } else {
                    Self::is_erlang_service(s).map(DiagnosticCode::ErlangService)
                },
            )
    }

    pub fn namespace(code: &String) -> Option<String> {
        let first = code.to_string().chars().next()?;
        Some(first.to_lowercase().to_string())
    }

    pub fn as_namespace(&self) -> Option<String> {
        match self {
            DiagnosticCode::DefaultCodeForEnumIter => None,
            DiagnosticCode::AdHoc(_) => None,
            // @fb-only: DiagnosticCode::MetaOnly(_) => None,
            DiagnosticCode::ErlangService(code) => Self::namespace(code),
            _ => Self::namespace(&self.as_code()),
        }
    }

    pub fn as_uri(&self) -> Option<String> {
        let namespace = self.as_namespace()?;
        let code = self.as_code();
        Some(format!(
            "{}/erlang-error-index/{namespace}/{code}",
            BASE_URL
        ))
    }

    /// Check if the diagnostic label is for an AdHoc one.
    fn is_adhoc(s: &str) -> Option<String> {
        // Looking for something like "ad-hoc: ad-hoc-title-1"
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^ad-hoc: ([^\s]+)$").unwrap();
        }
        RE.captures_iter(s).next().map(|c| c[1].to_string())
    }

    /// Check if the diagnostic label is for an ErlangService one.
    fn is_erlang_service(s: &str) -> Option<String> {
        // Looing for something like "L0008"
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^([A-Z]+[0-9]{4})$").unwrap();
        }
        RE.captures_iter(s).next().map(|c| c[1].to_string())
    }

    pub fn is_syntax_error(&self) -> bool {
        match self {
            DiagnosticCode::SyntaxError => true,
            DiagnosticCode::Missing(_) => true,
            DiagnosticCode::ErlangService(s) => s == "P1711",
            _ => false,
        }
    }

    pub fn allows_fixme_comment(&self) -> bool {
        // Note: exhaustive match, to make sure new ones get categorized too.
        match self {
            // True list
            DiagnosticCode::MisspelledAttribute => true,
            DiagnosticCode::CrossNodeEval => true,
            // False list
            DiagnosticCode::DefaultCodeForEnumIter => false,
            DiagnosticCode::HeadMismatch => false,
            DiagnosticCode::MissingModule => false,
            DiagnosticCode::ModuleMismatch => false,
            DiagnosticCode::UnusedInclude => false,
            DiagnosticCode::BoundVarInPattern => false,
            DiagnosticCode::UnusedMacro => false,
            DiagnosticCode::UnusedRecordField => false,
            DiagnosticCode::MutableVarBug => false,
            DiagnosticCode::SyntaxError => false,
            DiagnosticCode::Missing(_) => false,
            DiagnosticCode::StatementHasNoEffect => false,
            DiagnosticCode::TrivialMatch => false,
            DiagnosticCode::UnusedFunctionArg => false,
            DiagnosticCode::RedundantAssignment => false,
            DiagnosticCode::UnreachableTest => false,
            DiagnosticCode::ApplicationGetEnv => false,
            DiagnosticCode::MissingCompileWarnMissingSpec => false,
            DiagnosticCode::DependentHeader => false,
            DiagnosticCode::DeprecatedFunction => false,
            DiagnosticCode::UndefinedFunction => false,
            DiagnosticCode::Unexpected(_) => false,
            DiagnosticCode::ExpressionCanBeSimplified => false,
            DiagnosticCode::CannotEvaluateCTCallbacks => false,
            DiagnosticCode::MeckMissingNoLinkInInitPerSuite => false,
            DiagnosticCode::AtomsExhaustion => false,
            DiagnosticCode::SlowFunction => false,
            DiagnosticCode::ErlangService(_) => false,
            DiagnosticCode::Eqwalizer(_) => false,
            DiagnosticCode::AdHoc(_) => false,
            DiagnosticCode::MetaOnly(code) => code.allows_fixme_comment(),
        }
    }
}

lazy_static! {
    static ref DIAGNOSTIC_CODE_LOOKUPS: FxHashMap<String, DiagnosticCode> = {
        let mut res = FxHashMap::default();
        for code in DiagnosticCode::iter() {
            res.insert(code.as_code(), code.clone());
            res.insert(code.as_label(), code.clone());
        }
        res
    };
}

impl FromStr for DiagnosticCode {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(code) = DiagnosticCode::maybe_from_string(s) {
            Ok(code)
        } else {
            Err(format!("Unknown DiagnosticCode: '{s}'"))
        }
    }
}

impl From<&str> for DiagnosticCode {
    fn from(str: &str) -> Self {
        match DiagnosticCode::from_str(str) {
            Ok(c) => c,
            Err(err) => panic!("{err}"),
        }
    }
}

impl fmt::Display for DiagnosticCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_code())
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use serde::Deserialize;

    use super::DiagnosticCode;

    #[test]
    fn from_string_1() {
        let strings = vec!["W0008", "unreachable_test"];
        let codes = strings
            .iter()
            .map(|s| DiagnosticCode::maybe_from_string(s))
            .collect::<Vec<_>>();
        expect![[r#"
            [
                Some(
                    UnreachableTest,
                ),
                Some(
                    UnreachableTest,
                ),
            ]
        "#]]
        .assert_debug_eq(&codes);
    }

    #[test]
    fn from_string_2() {
        let strings = vec![
            DiagnosticCode::AdHoc("ad-hoc-title-1".to_string()).as_label(),
            DiagnosticCode::AdHoc("ad-hoc-title-2".to_string()).as_code(),
        ];
        let codes = strings
            .iter()
            .map(|s| DiagnosticCode::maybe_from_string(s))
            .collect::<Vec<_>>();
        expect![[r#"
            [
                Some(
                    AdHoc(
                        "ad-hoc-title-1",
                    ),
                ),
                Some(
                    AdHoc(
                        "ad-hoc-title-2",
                    ),
                ),
            ]
        "#]]
        .assert_debug_eq(&codes);
    }

    #[test]
    fn from_string_3() {
        let strings = vec!["C1000", "L1213"];
        let codes = strings
            .iter()
            .map(|s| DiagnosticCode::maybe_from_string(s))
            .collect::<Vec<_>>();
        expect![[r#"
            [
                Some(
                    ErlangService(
                        "C1000",
                    ),
                ),
                Some(
                    ErlangService(
                        "L1213",
                    ),
                ),
            ]
        "#]]
        .assert_debug_eq(&codes);
    }

    #[test]
    fn serde_serialize_diagnostic_code() {
        assert_eq!(
            toml::to_string::<DiagnosticCode>(&DiagnosticCode::CrossNodeEval),
            Ok("\"W0014\"".to_string())
        );
    }

    #[derive(Deserialize, Debug)]
    struct Config {
        #[allow(dead_code)]
        enabled: DiagnosticCode,
    }

    #[test]
    fn serde_deserialize_diagnostic_code_1() {
        let config: Config = toml::from_str(r#"enabled = 'W0014'"#).unwrap();

        expect![[r#"
            Config {
                enabled: CrossNodeEval,
            }
        "#]]
        .assert_debug_eq(&config);
    }

    #[test]
    fn serde_deserialize_diagnostic_code_2() {
        let config: Config = toml::from_str(r#"enabled = 'cross_node_eval'"#).unwrap();

        expect![[r#"
            Config {
                enabled: CrossNodeEval,
            }
        "#]]
        .assert_debug_eq(&config);
    }
}

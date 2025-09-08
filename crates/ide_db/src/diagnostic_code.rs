/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::str::FromStr;

use fxhash::FxHashMap;
use lazy_static::lazy_static;
use regex::Regex;
use serde::Deserialize;
use serde::Deserializer;
use serde::de;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

// @fb-only

// @fb-only
pub const BASE_URL: &str = "https://whatsapp.github.io/erlang-language-platform/docs"; // @oss-only

#[derive(Clone, Debug, PartialEq, Eq, Hash, EnumIter)]
#[derive(Default)]
pub enum DiagnosticCode {
    #[default]
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
    BooleanPrecedence,
    UnexportedFunction,
    RecordTupleMatch,
    UnnecessaryFlatteningToFindFlatLength,
    UnnecessaryReversalToFindLastElementOfList,
    UnnecessaryMapToListInComprehension,
    MapsPutFunctionRatherThanSyntax,
    MapsUpdateFunctionRatherThanSyntax,
    MapsFindFunctionRatherThanSyntax,
    ListsZipWithSeqRatherThanEnumerate,
    UnnecessaryFoldToBuildMapFromList,
    UnnecessaryMapFromListAroundComprehension,
    UnspecificInclude,
    OldEdocSyntax,
    MacroPrecedenceEscape,
    UndocumentedFunction,
    DebuggingFunction,
    EqualityCheckWithUnnecessaryOperator,
    NonStandardIntegerFormatting,
    SimplifyNegation,
    DuplicateModule,
    UndocumentedModule,
    NoGarbageCollect,
    NoDialyzerAttribute,
    SetsVersion2,
    NoSize,
    BinaryStringToSigil,
    NoCatch,
    NoErrorLogger,
    NoNoWarnSuppressions,
    CouldBeAStringLiteral,

    // Wrapper for erlang service diagnostic codes
    ErlangService(String),
    // Wrapper for EqWAlizer diagnostic codes
    Eqwalizer(String),
    // Used for ad-hoc diagnostics via lints/codemods
    AdHoc(String),
    // @fb-only
}

// These namespaces map the error codes returned by the Erlang Service.
// See erlang_service/src/erlang_service_error_codes.erl
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Namespace {
    Compiler,
    Dodger,
    Linter,
    Scanner,
    PreProcessor,
    QueryListComprehension,
    Parser,
    EDoc,
    WhatsApp,
    // @fb-only
}

impl fmt::Display for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let namespace = match self {
            Namespace::Compiler => "c",
            Namespace::Dodger => "d",
            Namespace::Linter => "l",
            Namespace::Scanner => "s",
            Namespace::PreProcessor => "e",
            Namespace::QueryListComprehension => "q",
            Namespace::Parser => "p",
            Namespace::EDoc => "o",
            Namespace::WhatsApp => "w",
            // @fb-only
        };
        write!(f, "{namespace}")
    }
}

impl FromStr for Namespace {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let first = s
            .to_string()
            .chars()
            .next()
            .ok_or(format!("Cannot extract namespace from: '{s}'"))?;
        match first.to_lowercase().to_string().as_str() {
            "c" => Ok(Namespace::Compiler),
            "d" => Ok(Namespace::Dodger),
            "l" => Ok(Namespace::Linter),
            "s" => Ok(Namespace::Scanner),
            "e" => Ok(Namespace::PreProcessor),
            "q" => Ok(Namespace::QueryListComprehension),
            "p" => Ok(Namespace::Parser),
            "o" => Ok(Namespace::EDoc),
            "w" => Ok(Namespace::WhatsApp),
            _ => Err(format!("No namespace found for: '{s}'")),
        }
    }
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
            DiagnosticCode::BooleanPrecedence => "W0025".to_string(),
            DiagnosticCode::UnexportedFunction => "W0026".to_string(),
            DiagnosticCode::RecordTupleMatch => "W0027".to_string(),
            DiagnosticCode::UnnecessaryFlatteningToFindFlatLength => "W0028".to_string(),
            DiagnosticCode::UnnecessaryReversalToFindLastElementOfList => "W0029".to_string(),
            DiagnosticCode::MapsPutFunctionRatherThanSyntax => "W0030".to_string(),
            DiagnosticCode::MapsUpdateFunctionRatherThanSyntax => "W0031".to_string(),
            DiagnosticCode::MapsFindFunctionRatherThanSyntax => "W0032".to_string(),
            DiagnosticCode::ListsZipWithSeqRatherThanEnumerate => "W0033".to_string(),
            DiagnosticCode::UnnecessaryMapToListInComprehension => "W0034".to_string(),
            DiagnosticCode::UnnecessaryFoldToBuildMapFromList => "W0035".to_string(),
            DiagnosticCode::UnnecessaryMapFromListAroundComprehension => "W0036".to_string(),
            DiagnosticCode::UnspecificInclude => "W0037".to_string(),
            DiagnosticCode::OldEdocSyntax => "W0038".to_string(),
            DiagnosticCode::MacroPrecedenceEscape => "W0039".to_string(),
            DiagnosticCode::UndocumentedFunction => "W0040".to_string(),
            DiagnosticCode::DebuggingFunction => "W0041".to_string(),
            DiagnosticCode::EqualityCheckWithUnnecessaryOperator => "W0042".to_string(),
            DiagnosticCode::NonStandardIntegerFormatting => "W0043".to_string(),
            DiagnosticCode::SimplifyNegation => "W0044".to_string(),
            DiagnosticCode::DuplicateModule => "W0045".to_string(),
            DiagnosticCode::UndocumentedModule => "W0046".to_string(),
            DiagnosticCode::NoGarbageCollect => "W0047".to_string(),
            DiagnosticCode::NoDialyzerAttribute => "W0048".to_string(),
            DiagnosticCode::SetsVersion2 => "W0049".to_string(),
            DiagnosticCode::NoSize => "W0050".to_string(),
            DiagnosticCode::BinaryStringToSigil => "W0051".to_string(),
            DiagnosticCode::NoCatch => "W0052".to_string(),
            DiagnosticCode::NoErrorLogger => "W0053".to_string(),
            DiagnosticCode::NoNoWarnSuppressions => "W0054".to_string(),
            DiagnosticCode::CouldBeAStringLiteral => "W0055".to_string(),
            DiagnosticCode::ErlangService(c) => c.to_string(),
            DiagnosticCode::Eqwalizer(c) => format!("eqwalizer: {c}"),
            DiagnosticCode::AdHoc(c) => format!("ad-hoc: {c}"),
            // @fb-only
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
            DiagnosticCode::BooleanPrecedence => "boolean_precedence".to_string(),
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
            DiagnosticCode::UnexportedFunction => "unexported_function".to_string(),
            DiagnosticCode::Unexpected(_) => "unexpected_semi_or_dot".to_string(),
            DiagnosticCode::ExpressionCanBeSimplified => "expression_can_be_simplified".to_string(),
            DiagnosticCode::UnnecessaryFlatteningToFindFlatLength => {
                "unnecessary_flattening_to_find_flat_length".to_string()
            }
            DiagnosticCode::UnnecessaryReversalToFindLastElementOfList => {
                "unnecessary_reversal_to_find_last_element_of_list".to_string()
            }
            DiagnosticCode::MapsPutFunctionRatherThanSyntax => {
                "maps_put_function_rather_than_syntax".to_string()
            }
            DiagnosticCode::MapsUpdateFunctionRatherThanSyntax => {
                "maps_update_function_rather_than_syntax".to_string()
            }
            DiagnosticCode::MapsFindFunctionRatherThanSyntax => {
                "maps_find_rather_than_syntax".to_string()
            }
            DiagnosticCode::ListsZipWithSeqRatherThanEnumerate => {
                "lists_zip_with_seq_rather_than_enumerate".to_string()
            }
            DiagnosticCode::UnnecessaryMapToListInComprehension => {
                "unnecessary_map_to_list_in_comprehension".to_string()
            }
            DiagnosticCode::UnnecessaryFoldToBuildMapFromList => {
                "unnecessary_fold_to_build_map_from_list".to_string()
            }
            DiagnosticCode::UnnecessaryMapFromListAroundComprehension => {
                "unnecessary_map_from_list_around_comprehension".to_string()
            }
            DiagnosticCode::UnspecificInclude => "unspecific_include".to_string(),
            DiagnosticCode::EqualityCheckWithUnnecessaryOperator => {
                "equality_check_with_unnecessary_operator".to_string()
            }
            DiagnosticCode::NonStandardIntegerFormatting => {
                "nonstandard_integer_formatting".to_string()
            }
            DiagnosticCode::SimplifyNegation => "simplify_negation".to_string(),
            DiagnosticCode::RecordTupleMatch => "record_tuple_match".to_string(),
            DiagnosticCode::OldEdocSyntax => "old_edoc_syntax".to_string(),
            DiagnosticCode::MacroPrecedenceEscape => "macro_precedence_escape".to_string(),
            DiagnosticCode::UndocumentedFunction => "undocumented_function".to_string(),
            DiagnosticCode::UndocumentedModule => "undocumented_module".to_string(),
            DiagnosticCode::DebuggingFunction => "debugging_function".to_string(),
            DiagnosticCode::DuplicateModule => "duplicate_module".to_string(),
            DiagnosticCode::NoGarbageCollect => "no_garbage_collect".to_string(),
            DiagnosticCode::NoDialyzerAttribute => "no_dialyzer_attribute".to_string(),
            DiagnosticCode::SetsVersion2 => "sets_version_2".to_string(),
            DiagnosticCode::NoSize => "no_size".to_string(),
            DiagnosticCode::BinaryStringToSigil => "binary_string_to_sigil".to_string(),
            DiagnosticCode::NoCatch => "no_catch".to_string(),
            DiagnosticCode::NoErrorLogger => "no_error_logger".to_string(),
            DiagnosticCode::NoNoWarnSuppressions => "no_nowarn_suppressions".to_string(),
            DiagnosticCode::CouldBeAStringLiteral => "could_be_a_binary_string_literal".to_string(),

            DiagnosticCode::ErlangService(c) => c.to_string(),
            DiagnosticCode::Eqwalizer(c) => c.to_string(),
            DiagnosticCode::AdHoc(c) => format!("ad-hoc: {c}"),
            // @fb-only
        }
    }

    pub fn as_labeled_code(&self) -> String {
        format!("{} ({})", self.as_code(), self.as_label())
    }

    pub fn maybe_from_string(s: &str) -> Option<DiagnosticCode> {
        DIAGNOSTIC_CODE_LOOKUPS
            .get(s).cloned()
            // @fb-only
            .or_else( ||
                // Look for ErlangService and AdHoc
                if let Some(code) = Self::is_adhoc(s) {
                    Some(DiagnosticCode::AdHoc(code))
                } else if let Some(code) = Self::is_eqwalizer(s) {
                    Some(DiagnosticCode::Eqwalizer(code))
                } else {
                    Self::is_erlang_service(s).map(DiagnosticCode::ErlangService)
                },
            )
    }

    pub fn as_namespace(&self) -> Option<Namespace> {
        match self {
            DiagnosticCode::DefaultCodeForEnumIter => None,
            DiagnosticCode::AdHoc(_) => None,
            // @fb-only
            DiagnosticCode::ErlangService(code) => Namespace::from_str(code).ok(),
            _ => Namespace::from_str(&self.as_code()).ok(),
        }
    }

    pub fn as_uri(&self) -> Option<String> {
        let namespace = self.as_namespace()?;
        let code = self.as_code();
        Some(format!("{BASE_URL}/erlang-error-index/{namespace}/{code}"))
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

    /// Check if the diagnostic label is for an Eqwalizer one.
    fn is_eqwalizer(s: &str) -> Option<String> {
        // Looking for something like "eqwalizer: unknown_id"
        lazy_static! {
            static ref RE: Regex = Regex::new(r"^eqwalizer: ([^\s]+)$").unwrap();
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
            // True
            DiagnosticCode::MisspelledAttribute => true,
            DiagnosticCode::CrossNodeEval => true,
            DiagnosticCode::MissingCompileWarnMissingSpec => true,
            DiagnosticCode::RecordTupleMatch => true,
            DiagnosticCode::DebuggingFunction => true,
            DiagnosticCode::NonStandardIntegerFormatting => true,
            DiagnosticCode::CouldBeAStringLiteral => true,

            // False
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
            DiagnosticCode::DependentHeader => false,
            DiagnosticCode::DeprecatedFunction => false,
            DiagnosticCode::UndefinedFunction => false,
            DiagnosticCode::Unexpected(_) => false,
            DiagnosticCode::ExpressionCanBeSimplified => false,
            DiagnosticCode::UnnecessaryFlatteningToFindFlatLength => false,
            DiagnosticCode::UnnecessaryReversalToFindLastElementOfList => false,
            DiagnosticCode::UnnecessaryMapToListInComprehension => false,
            DiagnosticCode::MapsPutFunctionRatherThanSyntax => false,
            DiagnosticCode::MapsUpdateFunctionRatherThanSyntax => false,
            DiagnosticCode::MapsFindFunctionRatherThanSyntax => false,
            DiagnosticCode::ListsZipWithSeqRatherThanEnumerate => false,
            DiagnosticCode::UnnecessaryFoldToBuildMapFromList => false,
            DiagnosticCode::UnnecessaryMapFromListAroundComprehension => false,
            DiagnosticCode::EqualityCheckWithUnnecessaryOperator => false,
            DiagnosticCode::SimplifyNegation => false,
            DiagnosticCode::CannotEvaluateCTCallbacks => false,
            DiagnosticCode::MeckMissingNoLinkInInitPerSuite => false,
            DiagnosticCode::AtomsExhaustion => false,
            DiagnosticCode::SlowFunction => false,
            DiagnosticCode::BooleanPrecedence => false,
            DiagnosticCode::UnexportedFunction => false,
            DiagnosticCode::UnspecificInclude => false,
            DiagnosticCode::OldEdocSyntax => false,
            DiagnosticCode::MacroPrecedenceEscape => false,
            DiagnosticCode::UndocumentedFunction => false,
            DiagnosticCode::DuplicateModule => false,
            DiagnosticCode::UndocumentedModule => false,
            DiagnosticCode::NoGarbageCollect => false,
            DiagnosticCode::NoDialyzerAttribute => false,
            DiagnosticCode::SetsVersion2 => false,
            DiagnosticCode::NoSize => false,
            DiagnosticCode::NoCatch => false,
            DiagnosticCode::NoErrorLogger => false,
            DiagnosticCode::NoNoWarnSuppressions => false,

            DiagnosticCode::BinaryStringToSigil => false,
            DiagnosticCode::ErlangService(_) => false,
            DiagnosticCode::Eqwalizer(_) => false,
            DiagnosticCode::AdHoc(_) => false,
            // @fb-only
        }
    }

    pub fn codes_iter() -> impl Iterator<Item = DiagnosticCode> {
        DiagnosticCode::iter()
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
        let strings = ["W0008", "unreachable_test"];
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
        let strings = [
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
        let strings = ["C1000", "L1213"];
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
    fn from_string_eqwalizer() {
        let strings = ["eqwalizer: unknown_id"];
        let codes = strings
            .iter()
            .map(|s| DiagnosticCode::maybe_from_string(s))
            .collect::<Vec<_>>();
        expect![[r#"
            [
                Some(
                    Eqwalizer(
                        "unknown_id",
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

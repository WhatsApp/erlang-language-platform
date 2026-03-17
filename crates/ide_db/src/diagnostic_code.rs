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
use strum::AsRefStr;
use strum::EnumIter;
use strum::EnumProperty;
use strum::IntoEnumIterator;

// @fb-only: use crate::meta_only::MetaOnlyDiagnosticCode;

// @fb-only: pub const BASE_URL: &str = crate::meta_only::BASE_URL;
pub const BASE_URL: &str = "https://whatsapp.github.io/erlang-language-platform/docs"; // @oss-only

const CODE_PROP: &str = "code";

#[derive(Clone, Debug, PartialEq, Eq, Hash, EnumIter, EnumProperty, AsRefStr)]
#[derive(Default)]
#[strum(serialize_all = "snake_case")]
pub enum DiagnosticCode {
    #[default]
    #[strum(
        props(code = "DEFAULT-UNUSED-CONSTRUCTOR"),
        serialize = "DEFAULT-UNUSED-CONSTRUCTOR"
    )]
    DefaultCodeForEnumIter,
    #[strum(props(code = "L1201"))]
    MissingModule,
    #[strum(props(code = "P1700"))]
    HeadMismatch,
    #[strum(props(code = "P1711"))]
    SyntaxError,
    #[strum(props(code = "W0000"))]
    BoundVarInPattern,
    #[strum(props(code = "W0001"))]
    ModuleMismatch,
    #[strum(props(code = "W0002"))]
    UnusedMacro,
    #[strum(props(code = "W0003"))]
    UnusedRecordField,
    #[strum(props(code = "W0004"))]
    Missing(String),
    #[strum(props(code = "W0005"), serialize = "mutable_variable_bug")]
    MutableVarBug,
    #[strum(props(code = "W0006"))]
    StatementHasNoEffect,
    #[strum(props(code = "W0007"))]
    TrivialMatch,
    #[strum(props(code = "W0008"))]
    UnreachableTest,
    #[strum(props(code = "W0009"))]
    RedundantAssignment,
    #[strum(props(code = "W0010"))]
    UnusedFunctionArg,
    #[strum(props(code = "W0011"))]
    ApplicationGetEnv,
    #[strum(props(code = "W0012"), serialize = "compile-warn-missing-spec")]
    MissingCompileWarnMissingSpec,
    #[strum(props(code = "W0013"))]
    MisspelledAttribute,
    #[strum(props(code = "W0014"))]
    CrossNodeEval,
    #[strum(props(code = "W0015"))]
    DependentHeader,
    #[strum(props(code = "W0016"))]
    DeprecatedFunction,
    #[strum(props(code = "W0017"))]
    UndefinedFunction,
    #[strum(props(code = "W0018"))]
    Unexpected(String),
    #[strum(props(code = "W0019"))]
    ExpressionCanBeSimplified,
    #[strum(props(code = "W0020"))]
    UnusedInclude,
    #[strum(props(code = "W0021"))]
    CannotEvaluateCTCallbacks,
    #[strum(props(code = "W0022"))]
    MeckMissingNoLinkInInitPerSuite,
    #[strum(props(code = "W0023"))]
    AtomsExhaustion,
    #[strum(props(code = "W0024"))]
    SlowFunction,
    #[strum(props(code = "W0025"))]
    BooleanPrecedence,
    #[strum(props(code = "W0026"))]
    UnexportedFunction,
    #[strum(props(code = "W0027"))]
    RecordTupleMatch,
    #[strum(props(code = "W0028"))]
    UnnecessaryFlatteningToFindFlatLength,
    #[strum(props(code = "W0029"))]
    UnnecessaryReversalToFindLastElementOfList,
    #[strum(props(code = "W0030"))]
    MapsPutFunctionRatherThanSyntax,
    #[strum(props(code = "W0031"))]
    MapsUpdateFunctionRatherThanSyntax,
    #[strum(props(code = "W0032"), serialize = "maps_find_rather_than_syntax")]
    MapsFindFunctionRatherThanSyntax,
    #[strum(props(code = "W0033"))]
    ListsZipWithSeqRatherThanEnumerate,
    #[strum(props(code = "W0034"))]
    UnnecessaryMapToListInComprehension,
    #[strum(props(code = "W0035"))]
    UnnecessaryFoldToBuildMapFromList,
    #[strum(props(code = "W0036"))]
    UnnecessaryMapFromListAroundComprehension,
    #[strum(props(code = "W0037"))]
    UnspecificInclude,
    #[strum(props(code = "W0038"))]
    OldEdocSyntax,
    #[strum(props(code = "W0039"))]
    MacroPrecedenceEscape,
    #[strum(props(code = "W0040"))]
    UndocumentedFunction,
    #[strum(props(code = "W0041"))]
    DebuggingFunction,
    #[strum(props(code = "W0042"))]
    EqualityCheckWithUnnecessaryOperator,
    #[strum(props(code = "W0043"), serialize = "nonstandard_integer_formatting")]
    NonStandardIntegerFormatting,
    #[strum(props(code = "W0044"))]
    SimplifyNegation,
    #[strum(props(code = "W0045"))]
    DuplicateModule,
    #[strum(props(code = "W0046"))]
    UndocumentedModule,
    #[strum(props(code = "W0047"))]
    NoGarbageCollect,
    #[strum(props(code = "W0048"))]
    NoDialyzerAttribute,
    #[strum(props(code = "W0049"), serialize = "sets_version_2")]
    SetsVersion2,
    #[strum(props(code = "W0050"))]
    NoSize,
    #[strum(props(code = "W0051"))]
    BinaryStringToSigil,
    #[strum(props(code = "W0052"))]
    NoCatch,
    #[strum(props(code = "W0053"))]
    NoErrorLogger,
    #[strum(props(code = "W0054"), serialize = "no_nowarn_suppressions")]
    NoNoWarnSuppressions,
    #[strum(props(code = "W0055"), serialize = "could_be_a_binary_string_literal")]
    CouldBeAStringLiteral,
    #[strum(props(code = "W0056"))]
    ListsReverseAppend,
    #[strum(props(code = "W0057"))]
    HirUnresolvedMacro,
    #[strum(props(code = "W0058"))]
    HirUnresolvedInclude,
    #[strum(props(code = "W0059"))]
    UnavailableType,
    #[strum(props(code = "W0060"))]
    BoundVarInLhs,
    #[strum(props(code = "W0061"))]
    HirInvalidPPCondition,
    #[strum(props(code = "W0062"))]
    HirIssueInIncludedFile,
    #[strum(props(code = "W0063"))]
    MixedStrictRelaxedGenerators,
    #[strum(props(code = "W0064"))]
    EtsLookupToLookupElement,
    #[strum(props(code = "W0065"))]
    InefficientListEmptyCheck,
    #[strum(props(code = "W0066"))]
    ListsMapToComprehension,
    #[strum(props(code = "W0067"))]
    MissingMsTransformInclude,
    #[strum(props(code = "W0068"))]
    MeckRestricted,
    #[strum(props(code = "W0069"))]
    NoAndOr,
    #[strum(props(code = "W0070"))]
    InlineNestedListComprehension,
    #[strum(props(code = "W0071"))]
    RedundantFunWrapper,
    #[strum(props(code = "W0072"))]
    EncodeHexWithCase,
    #[strum(props(code = "W0073"))]
    EqwalizerFixme,
    #[strum(props(code = "W0074"))]
    EqwalizerIgnore,
    #[strum(props(code = "W0075"))]
    UncheckedCast,

    // Wrapper for erlang service diagnostic codes
    ErlangService(String),
    // Wrapper for EqWAlizer diagnostic codes
    Eqwalizer(String),
    // Used for ad-hoc diagnostics via lints/codemods
    AdHoc(String),
    // @fb-only: MetaOnly(MetaOnlyDiagnosticCode),
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
    // @fb-only: MetaOnly,
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
            // @fb-only: Namespace::MetaOnly => "meta_only",
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

impl Namespace {
    pub fn supports_doc_path(&self) -> bool {
        match self {
            Namespace::WhatsApp => true,
            // @fb-only: Namespace::MetaOnly => true,
            _ => false,
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
            DiagnosticCode::Missing(_) => "W0004".to_string(),
            DiagnosticCode::Unexpected(_) => "W0018".to_string(),
            DiagnosticCode::ErlangService(c) => c.to_string(),
            DiagnosticCode::Eqwalizer(c) => format!("eqwalizer: {c}"),
            DiagnosticCode::AdHoc(c) => format!("ad-hoc: {c}"),
            // @fb-only: DiagnosticCode::MetaOnly(c) => c.as_code(),
            _ => self
                .get_str(CODE_PROP)
                .expect("all DiagnosticCode variants must have a 'code' strum property")
                .to_string(),
        }
    }

    pub fn as_label(&self) -> String {
        match self {
            DiagnosticCode::Missing(_) => "missing_comma_or_parenthesis".to_string(),
            DiagnosticCode::Unexpected(_) => "unexpected_semi_or_dot".to_string(),
            DiagnosticCode::ErlangService(c) => c.to_string(),
            DiagnosticCode::Eqwalizer(c) => c.to_string(),
            DiagnosticCode::AdHoc(c) => format!("ad-hoc: {c}"),
            // @fb-only: DiagnosticCode::MetaOnly(c) => c.as_label(),
            _ => self.as_ref().to_string(),
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
            // @fb-only: DiagnosticCode::MetaOnly(_) => Some(Namespace::MetaOnly),
            DiagnosticCode::ErlangService(code) => Namespace::from_str(code).ok(),
            _ => Namespace::from_str(&self.as_code()).ok(),
        }
    }

    pub fn supports_doc_path(&self) -> bool {
        match self {
            DiagnosticCode::DefaultCodeForEnumIter => false,
            // @fb-only: DiagnosticCode::MetaOnly(MetaOnlyDiagnosticCode::DefaultCodeForEnumIter) => false,
            _ => true,
        }
    }

    pub fn as_doc_path_extension(&self) -> &str {
        match self {
            DiagnosticCode::CrossNodeEval => "mdx",
            DiagnosticCode::NoCatch => "mdx",
            _ => "md",
        }
    }

    // Return the path to the documentation for this diagnostic code, relative to the ELP repo root.
    pub fn as_doc_path(&self) -> Option<String> {
        if !self.supports_doc_path() {
            return None;
        };
        let namespace = self.as_namespace()?;
        if namespace.supports_doc_path() {
            let code = self.as_code();
            let ext = self.as_doc_path_extension();
            return Some(format!(
                "website/docs/erlang-error-index/{namespace}/{code}.{ext}"
            ));
        }
        None
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
            static ref RE: Regex =
                Regex::new(r"^ad-hoc: ([^\s]+)$").expect("regex should be valid");
        }
        RE.captures_iter(s).next().map(|c| c[1].to_string())
    }

    /// Check if the diagnostic label is for an ErlangService one.
    fn is_erlang_service(s: &str) -> Option<String> {
        // Looing for something like "L0008"
        lazy_static! {
            static ref RE: Regex =
                Regex::new(r"^([A-Z]+[0-9]{4})$").expect("regex should be valid");
        }
        RE.captures_iter(s).next().map(|c| c[1].to_string())
    }

    /// Check if the diagnostic label is for an Eqwalizer one.
    fn is_eqwalizer(s: &str) -> Option<String> {
        // Looking for something like "eqwalizer: unknown_id"
        lazy_static! {
            static ref RE: Regex =
                Regex::new(r"^eqwalizer: ([^\s]+)$").expect("regex should be valid");
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
            DiagnosticCode::RedundantFunWrapper => true,
            DiagnosticCode::UnusedRecordField => true, // Sometimes redundant fields are kept for over-the-wire compatibility, so allow explicit exceptions for that

            // False
            DiagnosticCode::DefaultCodeForEnumIter => false,
            DiagnosticCode::HeadMismatch => false,
            DiagnosticCode::MissingModule => false,
            DiagnosticCode::ModuleMismatch => false,
            DiagnosticCode::UnusedInclude => false,
            DiagnosticCode::BoundVarInPattern => false,
            DiagnosticCode::BoundVarInLhs => false,
            DiagnosticCode::MixedStrictRelaxedGenerators => false,
            DiagnosticCode::InefficientListEmptyCheck => false,
            DiagnosticCode::ListsMapToComprehension => false,
            DiagnosticCode::MissingMsTransformInclude => false,
            DiagnosticCode::MeckRestricted => false,
            DiagnosticCode::NoAndOr => false,
            DiagnosticCode::InlineNestedListComprehension => false,
            DiagnosticCode::EncodeHexWithCase => false,
            DiagnosticCode::UnusedMacro => false,
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
            DiagnosticCode::UnavailableType => false,
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
            DiagnosticCode::ListsReverseAppend => false,
            DiagnosticCode::EtsLookupToLookupElement => false,
            DiagnosticCode::HirUnresolvedMacro => false,
            DiagnosticCode::HirUnresolvedInclude => false,
            DiagnosticCode::HirInvalidPPCondition => false,
            DiagnosticCode::HirIssueInIncludedFile => false,

            DiagnosticCode::BinaryStringToSigil => false,
            DiagnosticCode::EqwalizerFixme => false,
            DiagnosticCode::EqwalizerIgnore => false,
            DiagnosticCode::UncheckedCast => false,
            DiagnosticCode::ErlangService(_) => false,
            DiagnosticCode::Eqwalizer(_) => false,
            DiagnosticCode::AdHoc(_) => false,
            // @fb-only: DiagnosticCode::MetaOnly(code) => code.allows_fixme_comment(),
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
    #[cfg(not(buck_build))]
    use paths::Utf8PathBuf;
    use serde::Deserialize;
    use strum::IntoEnumIterator;

    use super::DiagnosticCode;

    #[cfg(buck_build)]
    fn get_doc_path(doc_path: &str) -> std::path::PathBuf {
        let resource_path =
            buck_resources::get("whatsapp/elp/crates/ide_db/error_index_docs").unwrap();
        let canonical = std::fs::canonicalize(&resource_path)
            .expect("Failed to canonicalize resource path. Check test_resources in BUCK file.");
        canonical.join(doc_path)
    }

    #[cfg(not(buck_build))]
    fn get_doc_path(doc_path: &str) -> std::path::PathBuf {
        // Compile-time for Cargo - use CARGO_MANIFEST_DIR to find ELP root
        let manifest_dir = Utf8PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let elp_root = manifest_dir.parent().unwrap().parent().unwrap();
        elp_root.join(doc_path).into_std_path_buf()
    }

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

    use serde::Serialize;

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct Config {
        enabled: DiagnosticCode,
    }

    #[test]
    fn serde_roundtrip_diagnostic_code() {
        // Serialize
        let config = Config {
            enabled: DiagnosticCode::CrossNodeEval,
        };
        let serialized = toml::to_string(&config).unwrap();
        assert_eq!(serialized, "enabled = \"W0014\"\n");

        // Deserialize
        let deserialized: Config = toml::from_str(&serialized).unwrap();
        assert_eq!(deserialized, config);
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

    #[test]
    fn all_diagnostic_codes_have_code_property() {
        use strum::EnumProperty;

        let mut missing = Vec::new();
        for variant in DiagnosticCode::iter() {
            match &variant {
                DiagnosticCode::Missing(_)
                | DiagnosticCode::Unexpected(_)
                | DiagnosticCode::ErlangService(_)
                | DiagnosticCode::Eqwalizer(_)
                | DiagnosticCode::AdHoc(_)
                // @fb-only: | DiagnosticCode::MetaOnly(_) => continue,
                _ => {}
            }
            if variant.get_str(super::CODE_PROP).is_none() {
                missing.push(format!("{:?}", variant));
            }
        }
        assert!(
            missing.is_empty(),
            "The following DiagnosticCode variants are missing the '{}' strum property: {:?}",
            super::CODE_PROP,
            missing
        );
    }

    #[test]
    fn all_diagnostic_codes_have_documentation() {
        let mut missing_docs = Vec::new();

        for code in DiagnosticCode::iter() {
            if let Some(doc_path) = code.as_doc_path() {
                let absolute_path = get_doc_path(&doc_path);

                if !absolute_path.exists() {
                    missing_docs.push(format!("{}: {}", code.as_code(), absolute_path.display()));
                }
            }
        }

        if !missing_docs.is_empty() {
            panic!(
                "The following diagnostic codes are missing their documentation files:\n{}",
                missing_docs.join("\n")
            );
        }
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// @fb-only: /// Types as defined in https://www.internalfb.com/intern/wiki/Linting/adding-linters/#flow-type
// @fb-only: /// and https://www.internalfb.com/code/fbsource/[1238f73dac0efd4009443fee6a345a680dc9401b]/whatsapp/server/erl/tools/lint/arcanist.py?lines=17
use std::fmt;
use std::path::Path;

use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct Diagnostic {
    // Filepath
    path: String,
    line: Option<u32>,
    char: Option<u32>,
    // End of the diagnostic range, when known.
    #[serde(skip_serializing_if = "Option::is_none")]
    end_line: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    end_char: Option<u32>,
    // Linter name (normally this would need to match code in fbsource-lint-engine.toml)
    code: String,
    // Message severity
    severity: Severity,
    // Rule name
    name: String,
    original: Option<String>,
    replacement: Option<String>,
    description: Option<String>,
    doc_path: Option<String>,
    // SSR-specific fields; only set by `elp ssr --format=json`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pattern_label: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    placeholders: Option<Vec<PlaceholderBinding>>,
}

/// SSR placeholder binding. Positions are 1-indexed.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct PlaceholderBinding {
    pub name: String,
    pub text: String,
    pub start_line: u32,
    pub start_char: u32,
    pub end_line: u32,
    pub end_char: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    Error,    // May crash (eg. syntax errors); always shown; need confirmation
    Warning,  // Minor problems (eg. readability); shown on change; need confirmation
    Autofix,  // Warning that contains an automatic fix in description
    Advice,   // Improvements (eg. leftover comments); shown on change; no confrimation
    Disabled, // Suppressed error message
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
            Severity::Autofix => write!(f, "autofix"),
            Severity::Advice => write!(f, "advice"),
            Severity::Disabled => write!(f, "disabled"),
        }
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let line = self.line.unwrap_or(0);
        let loc = match (self.char, self.end_line, self.end_char) {
            (Some(c), Some(el), Some(ec)) => {
                format!("{}:{}:{}-{}:{}", self.path, line, c, el, ec)
            }
            (Some(c), _, _) => format!("{}:{}:{}", self.path, line, c),
            (None, _, _) => format!("{}:{}", self.path, line),
        };
        write!(f, "{}: {} [{}]", self.severity, loc, self.name)?;
        if let Some(desc) = &self.description {
            for line in desc.lines() {
                let trimmed = line.trim();
                if trimmed.is_empty()
                    || trimmed.starts_with("```")
                    || trimmed.starts_with("> [docs")
                {
                    continue;
                }
                write!(f, "\n  {line}")?;
            }
        }
        writeln!(f)
    }
}

impl Diagnostic {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        path: &Path,
        line: u32,
        character: Option<u32>,
        severity: Severity,
        name: String,
        description: String,
        original: Option<String>,
        doc_path: Option<String>,
    ) -> Self {
        Diagnostic {
            path: path.display().to_string(), // lossy on Windows for unicode paths
            line: Some(line),
            r#char: character,
            end_line: None,
            end_char: None,
            code: "ELP".to_owned(),
            severity,
            name,
            original,
            replacement: None,
            description: Some(description),
            doc_path,
            pattern_label: None,
            placeholders: None,
        }
    }

    /// Attach the diagnostic's end-position. Both values are 1-indexed,
    /// matching `line` / `char`.
    pub fn with_end_position(mut self, end_line: u32, end_character: u32) -> Self {
        self.end_line = Some(end_line);
        self.end_char = Some(end_character);
        self
    }

    pub fn with_ssr(
        mut self,
        pattern_label: Option<String>,
        placeholders: Vec<PlaceholderBinding>,
    ) -> Self {
        self.pattern_label = pattern_label;
        self.placeholders = Some(placeholders);
        self
    }

    pub fn with_fix(
        mut self,
        line: u32,
        character: Option<u32>,
        original: String,
        replacement: String,
    ) -> Self {
        self.line = Some(line);
        self.r#char = character;
        // The fix has its own start position but no end. Clear any prior
        // end-position so consumers don't read the resulting record as a
        // coherent range pairing the fix start with the diagnostic end.
        self.end_line = None;
        self.end_char = None;
        self.original = Some(original);
        self.replacement = Some(replacement);
        self
    }

    /// Diagnostic name (e.g. `"P1700 (head_mismatch)"`). Used as part of
    /// the `(name, severity)` group key by `elp lint-compare`.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Diagnostic severity. Used as part of the `(name, severity)`
    /// group key by `elp lint-compare` so a lint that flips
    /// severity (e.g. `warning` -> `error`) shows up as one row
    /// removed and one row added.
    pub fn severity(&self) -> &Severity {
        &self.severity
    }

    /// Documentation URL carried by lint diagnostics, when present.
    pub fn doc_path(&self) -> Option<&str> {
        self.doc_path.as_deref().filter(|path| !path.is_empty())
    }

    /// Render as a GitHub Actions workflow command, which the Actions runner
    /// turns into an annotation on the workflow run and the pull request:
    /// `::error file=<path>,line=<l>,col=<c>,endLine=<l>,endColumn=<c>,title=<name>::<message>`.
    /// See <https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-commands>.
    pub fn to_github_workflow_command(&self) -> String {
        let level = match self.severity {
            Severity::Error => "error",
            Severity::Warning | Severity::Autofix => "warning",
            Severity::Advice | Severity::Disabled => "notice",
        };
        let properties = self
            .github_properties()
            .into_iter()
            .map(|(key, value)| format!("{key}={}", escape_github_property(&value)))
            .collect::<Vec<_>>()
            .join(",");
        format!(
            "::{level} {properties}::{}",
            escape_github_data(&self.github_message())
        )
    }

    fn github_properties(&self) -> Vec<(&'static str, String)> {
        let mut properties = vec![("file", self.path.clone())];
        if let Some(line) = self.line {
            properties.push(("line", line.to_string()));
            // The runner drops `col` and `endColumn` from a range spanning
            // several lines, so such a range is reported by lines only.
            match (self.char, self.end_line, self.end_char) {
                (_, Some(end_line), _) if end_line != line => {
                    properties.push(("endLine", end_line.to_string()));
                }
                (Some(col), Some(end_line), Some(end_col)) => {
                    properties.push(("col", col.to_string()));
                    properties.push(("endLine", end_line.to_string()));
                    properties.push(("endColumn", end_col.to_string()));
                }
                (Some(col), _, _) => properties.push(("col", col.to_string())),
                (None, _, _) => {}
            }
        }
        properties.push(("title", self.name.clone()));
        properties
    }

    /// The description as plain text: annotations do not render Markdown, so
    /// code fences become paragraph breaks and a Markdown docs link becomes a
    /// bare URL.
    fn github_message(&self) -> String {
        let Some(description) = &self.description else {
            return self.name.clone();
        };
        let lines: Vec<String> = description
            .lines()
            .map(|line| {
                if line.trim_start().starts_with("```") {
                    String::new()
                } else if let Some(url) = markdown_docs_link(line) {
                    format!("For more information see: {url}")
                } else {
                    line.to_string()
                }
            })
            .collect();
        lines
            .split(|line| line.trim().is_empty())
            .filter(|paragraph| !paragraph.is_empty())
            .map(|paragraph| paragraph.join("\n"))
            .collect::<Vec<_>>()
            .join("\n\n")
    }
}

/// The URL of a `> [docs on `code`](url)` line, as written by
/// `convert::eqwalizer_to_arc_diagnostic`.
fn markdown_docs_link(line: &str) -> Option<&str> {
    let (_, url) = line.trim().strip_prefix("> [docs")?.rsplit_once("](")?;
    url.strip_suffix(')')
}

fn escape_github_data(value: &str) -> String {
    value
        .replace('%', "%25")
        .replace('\r', "%0D")
        .replace('\n', "%0A")
}

fn escape_github_property(value: &str) -> String {
    escape_github_data(value)
        .replace(':', "%3A")
        .replace(',', "%2C")
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use elp_ide::elp_ide_db::elp_base_db::assert_eq_expected;

    use super::Diagnostic;
    use super::Severity;

    fn diagnostic(severity: Severity, name: &str, description: &str) -> Diagnostic {
        Diagnostic::new(
            Path::new("src/foo.erl"),
            3,
            Some(5),
            severity,
            name.to_string(),
            description.to_string(),
            None,
            None,
        )
    }

    #[test]
    fn github_lint_diagnostic_on_one_line() {
        let diagnostic = diagnostic(
            Severity::Warning,
            "W0016 (deprecated_function)",
            "Function 'a/0' is deprecated.\n\nFor more information see: https://example.com/W0016",
        )
        .with_end_position(3, 12);
        assert_eq_expected!(
            "::warning file=src/foo.erl,line=3,col=5,endLine=3,endColumn=12,title=W0016 (deprecated_function)::Function 'a/0' is deprecated.%0A%0AFor more information see: https://example.com/W0016",
            diagnostic.to_github_workflow_command()
        );
    }

    #[test]
    fn github_multi_line_range_is_reported_by_lines() {
        let diagnostic =
            diagnostic(Severity::Error, "W0001 (example)", "Problem.").with_end_position(4, 2);
        assert_eq_expected!(
            "::error file=src/foo.erl,line=3,endLine=4,title=W0001 (example)::Problem.",
            diagnostic.to_github_workflow_command()
        );
    }

    #[test]
    fn github_diagnostic_without_column() {
        let diagnostic = Diagnostic::new(
            Path::new("src/foo.erl"),
            2,
            None,
            Severity::Error,
            "ELP".to_string(),
            "can't find include file \"missing.hrl\"".to_string(),
            None,
            None,
        );
        assert_eq_expected!(
            "::error file=src/foo.erl,line=2,title=ELP::can't find include file \"missing.hrl\"",
            diagnostic.to_github_workflow_command()
        );
    }

    #[test]
    fn github_lower_severities_are_notices() {
        for severity in [Severity::Advice, Severity::Disabled] {
            let command =
                diagnostic(severity, "W0002 (example)", "Hint.").to_github_workflow_command();
            assert!(
                command.starts_with("::notice "),
                "advice and disabled diagnostics should be notices: {command}"
            );
        }
        let command = diagnostic(Severity::Autofix, "W0003 (example)", "Fixable.")
            .to_github_workflow_command();
        assert!(
            command.starts_with("::warning "),
            "autofix diagnostics should be warnings: {command}"
        );
    }

    #[test]
    fn github_eqwalizer_markdown_becomes_plain_text() {
        let diagnostic = diagnostic(
            Severity::Error,
            "eqWAlizer: incompatible_types",
            "```lang=error,counterexample\n`'error'`.\n\nExpression has type:   'error'\nContext expected type: 'ok'\n```\n```\nBecause in the expression's type:\n  Here the type is: 'error'\n```\n> [docs on `incompatible_types`](https://example.com/eqwalizer#incompatible_types)",
        );
        assert_eq_expected!(
            "::error file=src/foo.erl,line=3,col=5,title=eqWAlizer%3A incompatible_types::`'error'`.%0A%0AExpression has type:   'error'%0AContext expected type: 'ok'%0A%0ABecause in the expression's type:%0A  Here the type is: 'error'%0A%0AFor more information see: https://example.com/eqwalizer#incompatible_types",
            diagnostic.to_github_workflow_command()
        );
    }

    #[test]
    fn github_escapes_special_characters() {
        let diagnostic = Diagnostic::new(
            Path::new("src/a,b:c.erl"),
            1,
            Some(1),
            Severity::Warning,
            "W0004 (100%, really)".to_string(),
            "50% done\r\nnext".to_string(),
            None,
            None,
        );
        assert_eq_expected!(
            "::warning file=src/a%2Cb%3Ac.erl,line=1,col=1,title=W0004 (100%25%2C really)::50%25 done%0Anext",
            diagnostic.to_github_workflow_command()
        );
    }
}

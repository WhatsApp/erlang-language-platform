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

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq)]
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
    // SSR placeholder bindings; only set by `elp ssr --format=json`.
    #[serde(skip_serializing_if = "Option::is_none")]
    placeholders: Option<Vec<PlaceholderBinding>>,
}

/// SSR placeholder binding. Positions are 1-indexed.
#[derive(Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct PlaceholderBinding {
    pub name: String,
    pub text: String,
    pub start_line: u32,
    pub start_char: u32,
    pub end_line: u32,
    pub end_char: u32,
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq)]
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

    pub fn with_ssr(mut self, placeholders: Vec<PlaceholderBinding>) -> Self {
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
}

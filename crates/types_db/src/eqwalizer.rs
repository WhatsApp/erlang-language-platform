/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Types specific to Eqwalizer.

use elp_syntax::TextRange;
use serde::Deserialize;

#[derive(Debug, Deserialize, PartialEq, Eq, Clone)]
#[serde(rename_all = "camelCase")]
pub struct EqwalizerDiagnostic {
    #[serde(deserialize_with = "deserialize_text_range")]
    pub range: TextRange,
    pub message: String,
    pub uri: String,
    pub code: String,
    #[serde(rename(deserialize = "expressionOrNull"))]
    pub expression: Option<String>,
    #[serde(rename(deserialize = "explanationOrNull"))]
    pub explanation: Option<String>,
}

impl EqwalizerDiagnostic {
    pub fn expr_string(&self) -> String {
        match &self.expression {
            Some(s) => format!("`{}`.\n", s),
            None => "".to_string(),
        }
    }
}

fn deserialize_text_range<'de, D>(deserializer: D) -> Result<TextRange, D::Error>
where
    D: serde::Deserializer<'de>,
{
    #[derive(Deserialize)]
    struct RawTextRange {
        start: u32,
        end: u32,
    }

    let range = RawTextRange::deserialize(deserializer)?;
    Ok(TextRange::new(range.start.into(), range.end.into()))
}

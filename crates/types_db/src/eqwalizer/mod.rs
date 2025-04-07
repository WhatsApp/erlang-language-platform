/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use serde::Deserialize;
use serde::Serialize;
use serde_with::DeserializeFromStr;
use serde_with::SerializeDisplay;

use crate::StringId;

pub mod binary_specifier;
pub mod expr;
pub mod ext_types;
pub mod form;
pub mod guard;
pub mod invalid_diagnostics;
pub mod pat;
pub mod tc_diagnostics;
pub mod transformer;
pub mod types;
pub mod visitor;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AST {
    pub from_beam: bool,
    pub forms: Vec<form::ExternalForm>,
}

#[derive(Debug, Deserialize, PartialEq, Eq, Clone)]
#[serde(rename_all = "camelCase")]
pub struct EqwalizerDiagnostic {
    #[serde(deserialize_with = "deserialize_text_range")]
    pub range: elp_syntax::TextRange,
    pub message: String,
    pub uri: String,
    pub code: String,
    pub expression: Option<String>,
    pub explanation: Option<String>,
    #[serde(default)]
    pub diagnostic: Option<StructuredDiagnostic>,
}

#[derive(Debug, Deserialize, PartialEq, Eq, Clone)]
pub enum StructuredDiagnostic {
    TypeError {
        error: tc_diagnostics::TypeError,
    },
    InvalidForm {
        invalid: invalid_diagnostics::Invalid,
    },
}

impl EqwalizerDiagnostic {
    pub fn expr_string(&self) -> String {
        match &self.expression {
            Some(s) => format!("`{}`.\n", s),
            None => "".to_string(),
        }
    }
}

fn deserialize_text_range<'de, D>(deserializer: D) -> Result<elp_syntax::TextRange, D::Error>
where
    D: serde::Deserializer<'de>,
{
    #[derive(Deserialize)]
    struct RawTextRange {
        start_byte: u32,
        end_byte: u32,
    }

    let range = RawTextRange::deserialize(deserializer)?;
    Ok(elp_syntax::TextRange::new(
        range.start_byte.into(),
        range.end_byte.into(),
    ))
}

// ---------------------------------------------------------------------

#[derive(
    Serialize,
    Deserialize,
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord
)]
pub struct RemoteId {
    pub module: StringId,
    pub name: StringId,
    pub arity: u32,
}

impl fmt::Display for RemoteId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}/{}", self.module, self.name, self.arity)
    }
}

#[derive(
    SerializeDisplay,
    DeserializeFromStr,
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord
)]
pub struct Id {
    pub name: StringId,
    pub arity: u32,
}

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.name, self.arity)
    }
}

impl std::str::FromStr for Id {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split('/');
        let name = parts.next().ok_or("Missing ID name")?;
        let arity = parts
            .next()
            .ok_or("Missing ID arity")?
            .parse::<u32>()
            .map_err(|e| e.to_string())?;
        Ok(Id {
            name: name.into(),
            arity,
        })
    }
}

impl From<RemoteId> for Id {
    fn from(remote_id: RemoteId) -> Self {
        Id {
            name: remote_id.name,
            arity: remote_id.arity,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Pos {
    TextRange(TextRange),
    LineAndColumn(LineAndColumn),
}
impl From<LineAndColumn> for Pos {
    fn from(x: LineAndColumn) -> Self {
        Pos::LineAndColumn(x)
    }
}
impl From<TextRange> for Pos {
    fn from(x: TextRange) -> Self {
        Pos::TextRange(x)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TextRange {
    pub start_byte: u32,
    pub end_byte: u32,
}
impl TextRange {
    pub fn fake() -> Self {
        TextRange {
            start_byte: 0,
            end_byte: 100,
        }
    }
}
impl From<TextRange> for elp_syntax::TextRange {
    fn from(val: TextRange) -> Self {
        elp_syntax::TextRange::new(val.start_byte.into(), val.end_byte.into())
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct LineAndColumn {
    pub line: u32,
    pub column: u32,
}
impl LineAndColumn {
    pub fn fake() -> Self {
        LineAndColumn {
            line: 1,
            column: 100,
        }
    }
}

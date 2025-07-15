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

use crate::TextRange;

/// Represents the result of unsuccessful tokenization, parsing
/// or tree validation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SyntaxError {
    Error(TextRange),
    Missing(String, TextRange),
}

impl SyntaxError {
    pub fn error(range: TextRange) -> Self {
        Self::Error(range)
    }

    pub fn missing(message: impl Into<String>, range: TextRange) -> Self {
        Self::Missing(message.into(), range)
    }

    pub fn range(&self) -> TextRange {
        match self {
            SyntaxError::Error(r) => *r,
            SyntaxError::Missing(_, r) => *r,
        }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SyntaxError::Error(_) => {
                write!(f, "Syntax Error")
            }
            SyntaxError::Missing(m, _) => {
                write!(f, "Missing '{m}'")
            }
        }
    }
}

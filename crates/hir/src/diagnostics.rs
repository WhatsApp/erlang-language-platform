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

use elp_syntax::TextRange;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Diagnostic {
    pub location: TextRange,
    pub message: DiagnosticMessage,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DiagnosticMessage {
    VarNameOutsideMacro,
}

impl fmt::Display for DiagnosticMessage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagnosticMessage::VarNameOutsideMacro => {
                write!(
                    f,
                    "using variable instead of an atom name is allowed only inside -define"
                )
            }
        }
    }
}

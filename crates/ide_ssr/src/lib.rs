/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Structural Search and Replace
//!
//! Allows searching the AST for code that matches one or more
//! patterns and then replacing that code based on a template.

// Note: not all parts of the feature are currently implemented, but
// this is what we are aiming for.

// Feature: Structural Search and Replace
//
// Search and replace with named wildcards that will match any
// expression, type, path, pattern or item.  The syntax for a
// structural search replace command is `ssr: <search_pattern> ==>>
// <replace_pattern>.`.  A `_@<name>` placeholder in the search pattern
// will match any AST node and `_@<name>` will reference it in the
// replacement.  Within a macro call, a placeholder will match up
// until whatever token follows the placeholder.
//
// The scope of the search / replace will be restricted to the current
// selection if any, otherwise it will apply to the whole workspace.
//
// Available via the command `elp.ssr`.
//
// ```erlang
// // Using structural search replace command [ssr: foo(_@A, _@B) ==>> foo(_@B, _@A)]
//
// // BEFORE
// foo(Y + 5, Z)
//
// // AFTER
// foo(Z, Y + 5)
// ```
//
// |===
// | Editor  | Action Name
//
// | VS Code | **elp: Structural Search Replace**
// |===
//
// Also available as an assist, by writing a comment containing the structural
// search and replace rule. You will only see the assist if the comment can
// be parsed as a valid structural search and replace rule.
//
// ```erlang
// %% Place the cursor on the line below to see the assist 💡.
// %% ssr: foo(_@A, _@B) ==>> foo(_@B, _@A).
// ```

use std::sync::Arc;

use hir::db::DefDatabase;
use hir::db::InternDatabase;
use hir::SsrBody;
use hir::SsrSource;

#[macro_use]
mod errors;

#[cfg(test)]
mod tests;

pub use errors::SsrError;

// ---------------------------------------------------------------------

// A structured search replace rule. Create by calling `parse` on a str.

#[derive(Debug)]
pub struct SsrPattern {
    #[allow(dead_code)]
    parsed_rules: Vec<Arc<SsrBody>>,
}

impl SsrPattern {
    #[allow(dead_code)]
    pub(crate) fn tree_print(&self, db: &dyn InternDatabase) -> String {
        self.parsed_rules
            .iter()
            .map(|r| r.tree_print(db))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn parse_ssr_source(
        db: &dyn DefDatabase,
        ssr_source: SsrSource,
    ) -> Result<SsrPattern, SsrError> {
        if let Some((body, _)) = db.ssr_body_with_source(ssr_source) {
            Ok(SsrPattern {
                parsed_rules: vec![body.clone()],
            })
        } else {
            Err(SsrError("Could not lower rule".to_string()))
        }
    }

    pub fn parse_str(db: &dyn DefDatabase, pattern_str: &str) -> Result<SsrPattern, SsrError> {
        let ssr_source = db.ssr(Arc::from(pattern_str));
        Self::parse_ssr_source(db, ssr_source)
    }
}

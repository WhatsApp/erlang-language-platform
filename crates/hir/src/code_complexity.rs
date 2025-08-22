/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use elp_syntax::AstNode as _;

use crate::FunctionDef;
use crate::Semantic;

pub struct CodeComplexity {
    pub score: usize,
}

// Ideally we'd navigate the AST and calcuate a score based on the complexity of the code.
// Unfortunately, we currently only provide a folding instructure around the AST, which
// makes it hard to break early.
// For now, we'll just approximate the score to the number of lines, as a starting point.
pub fn compute(sema: &Semantic, def: &FunctionDef, score_cap: Option<usize>) -> CodeComplexity {
    let mut score = 0;
    for decl in def.source(sema.db.upcast()) {
        for _line in decl.syntax().text().to_string().lines() {
            score += 1;
            if let Some(score_cap) = score_cap
                && score >= score_cap
            {
                break;
            }
        }
    }
    CodeComplexity { score }
}

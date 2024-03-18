/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub use tree_sitter::*;
use tree_sitter_erlang::language;

pub struct Parser(tree_sitter::Parser);

impl Parser {
    pub fn new() -> Self {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&language())
            .expect("incompatible tree-sitter");
        Parser(parser)
    }

    pub fn parse(&mut self, text: &str) -> Option<Tree> {
        self.0.parse(text, None)
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Types for use in HIR but which can also be produced by other code
//! intelligence sources, such as Eqwalizer, OTP compiler,
//! common_test.
//!
//! This crate carries the types only, so they can be used in
//! hir::Semantic, without concern for the messy details of actually
//! retrieving them.
//!
//! This also allows us to set up test fixtures to populate them
//! without running heavyweight processes.

pub mod eqwalizer;

use std::sync::Arc;

use elp_base_db::FileId;
use elp_base_db::FileRange;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct EqwalizerIncludes {
    pub include_generated: IncludeGenerated,
    pub include_tests: IncludeTests,
}

impl EqwalizerIncludes {
    pub fn new() -> EqwalizerIncludes {
        EqwalizerIncludes {
            include_generated: IncludeGenerated::No,
            include_tests: IncludeTests::No,
        }
    }

    pub fn generated(mut self) -> EqwalizerIncludes {
        self.include_generated = IncludeGenerated::Yes;
        self
    }

    pub fn set_generated(mut self, include_generated: IncludeGenerated) -> EqwalizerIncludes {
        self.include_generated = include_generated;
        self
    }

    pub fn tests(mut self) -> EqwalizerIncludes {
        self.include_generated = IncludeGenerated::Yes;
        self
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum IncludeGenerated {
    Yes,
    No,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum IncludeTests {
    Yes,
    No,
}

impl From<bool> for IncludeGenerated {
    fn from(value: bool) -> Self {
        if value {
            IncludeGenerated::Yes
        } else {
            IncludeGenerated::No
        }
    }
}

pub trait TypedSemantic {
    fn eqwalizer_diagnostics(
        &self,
        file_id: FileId,
        include_generated: EqwalizerIncludes,
    ) -> Option<Vec<eqwalizer::EqwalizerDiagnostic>>;

    fn eqwalizer_type_at_position(
        &self,
        range: FileRange,
    ) -> Option<Arc<(eqwalizer::types::Type, FileRange)>>;
}

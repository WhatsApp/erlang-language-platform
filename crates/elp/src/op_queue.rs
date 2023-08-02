/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Bookkeeping to make sure only one long-running operation is being executed
//! at a time.

// From https://github.com/rust-lang/rust-analyzer/blob/1efd220f2f844596dd22bfd73a8a0c596354be38/crates/rust-analyzer/src/op_queue.rs

pub struct OpQueue<Args, Output> {
    op_requested: Option<Args>,
    op_in_progress: bool,
    last_op_result: Output,
}

impl<Args, Output: Default> Default for OpQueue<Args, Output> {
    fn default() -> Self {
        Self {
            op_requested: None,
            op_in_progress: false,
            last_op_result: Default::default(),
        }
    }
}

#[allow(unused)]
impl<Args, Output> OpQueue<Args, Output> {
    pub fn request_op(&mut self, data: Args) {
        self.op_requested = Some(data);
    }
    pub fn should_start_op(&mut self) -> Option<Args> {
        if self.op_in_progress {
            return None;
        }
        self.op_in_progress = self.op_requested.is_some();
        self.op_requested.take()
    }
    pub fn op_completed(&mut self, result: Output) {
        assert!(self.op_in_progress);
        self.op_in_progress = false;
        self.last_op_result = result;
    }

    pub fn last_op_result(&self) -> &Output {
        &self.last_op_result
    }
    pub fn op_in_progress(&self) -> bool {
        self.op_in_progress
    }
    pub fn op_requested(&self) -> bool {
        self.op_requested.is_some()
    }
}

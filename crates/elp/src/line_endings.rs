/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! We maintain invariant that all internal strings use `\n` as line separator.
//! This module does line ending conversion and detection (so that we can
//! convert back to `\r\n` on the way out).

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LineEndings {
    Unix,
    Dos,
}

impl LineEndings {
    /// Replaces `\r\n` with `\n` in `src`.
    pub fn normalize(src: String) -> (String, Self) {
        if !src.as_bytes().contains(&b'\r') {
            (src, Self::Unix)
        } else {
            (src.replace("\r\n", "\n"), Self::Dos)
        }
    }

    pub fn revert(&self, src: String) -> String {
        match self {
            Self::Unix => src,
            Self::Dos => src.replace('\n', "\r\n"),
        }
    }
}

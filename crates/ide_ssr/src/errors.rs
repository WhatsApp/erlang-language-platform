/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Code relating to errors produced by SSR.

/// Constructs an SsrError taking arguments like the format macro.
macro_rules! _error {
    ($fmt:expr_2021) => {$crate::SsrError::new(format!($fmt))};
    ($fmt:expr_2021, $($arg:tt)+) => {$crate::SsrError::new(format!($fmt, $($arg)+))}
}
#[allow(unused_imports)]
pub(crate) use _error as error;

/// Returns from the current function with an error, supplied by arguments as for format!
macro_rules! _bail {
    ($($tokens:tt)*) => {return Err(crate::errors::error!($($tokens)*))}
}
#[allow(unused_imports)]
pub(crate) use _bail as bail;

#[derive(Debug, PartialEq)]
pub struct SsrError(pub(crate) String);

impl std::fmt::Display for SsrError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Parse error: {}", self.0)
    }
}

impl SsrError {
    pub(crate) fn new(message: impl Into<String>) -> SsrError {
        SsrError(message.into())
    }
}

/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! See [`Label`]
use std::fmt;

/// A type to specify UI label, like an entry in the list of assists. Enforces
/// proper casing:
///
///    Frobnicate bar
///
/// Note the upper-case first letter and the absence of `.` at the end.
#[derive(Clone)]
pub struct Label(String);

impl PartialEq<str> for Label {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl PartialEq<&'_ str> for Label {
    fn eq(&self, other: &&str) -> bool {
        self == *other
    }
}

impl From<Label> for String {
    fn from(label: Label) -> String {
        label.0
    }
}

impl Label {
    pub fn new(label: impl Into<String>) -> Label {
        let label = label.into();
        assert!(label.starts_with(char::is_uppercase) && !label.ends_with('.'));
        Label(label)
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl fmt::Debug for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

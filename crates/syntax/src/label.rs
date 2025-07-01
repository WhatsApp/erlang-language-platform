/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! See [`Label`]
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

/// A type to specify UI label, like an entry in the list of assists. Enforces
/// proper casing:
///
///    Frobnicate bar
///
/// Note the upper-case first letter and the absence of `.` at the end.
#[derive(Clone, Eq)]
pub struct Label(String);

// Explicit implementation otherwise clippy errors because of
// explicit PartialEq implementations.
impl Hash for Label {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl PartialEq for Label {
    fn eq(&self, other: &Label) -> bool {
        self.0 == other.0
    }
}
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

impl PartialEq<String> for Label {
    fn eq(&self, other: &String) -> bool {
        self.0 == *other
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

    // Used for ast::HasLabel trait
    pub fn new_raw(label: impl Into<String>) -> Label {
        let label = label.into();
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

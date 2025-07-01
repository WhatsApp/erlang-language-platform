/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use serde::Deserialize;
use serde::Serialize;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Specifier {
    SignedIntegerSpecifier,
    UnsignedIntegerSpecifier,
    FloatSpecifier,
    BinarySpecifier,
    BytesSpecifier,
    BitstringSpecifier,
    BitsSpecifier,
    Utf8Specifier,
    Utf16Specifier,
    Utf32Specifier,
}

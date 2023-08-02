/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use serde::Serialize;

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
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

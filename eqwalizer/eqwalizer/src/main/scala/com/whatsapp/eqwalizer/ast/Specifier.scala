/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.whatsapp.eqwalizer.ast

import com.whatsapp.eqwalizer.ast.Types._

enum Specifier {
  case SignedInteger, UnsignedInteger, Float, Binary, Bytes, Bitstring, Bits, Utf8, Utf16, Utf32
}

object Specifier {
  def expType(s: Specifier, stringLiteral: Boolean): Type =
    s match {
      case UnsignedInteger | Utf8 | Utf16 | Utf32 =>
        if (stringLiteral) stringType
        else IntegerType
      case SignedInteger =>
        IntegerType
      case Float =>
        FloatType
      case Binary | Bytes | Bitstring | Bits =>
        BinaryType
    }
}

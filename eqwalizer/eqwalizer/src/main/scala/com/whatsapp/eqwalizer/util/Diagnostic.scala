/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.whatsapp.eqwalizer.util

import com.whatsapp.eqwalizer.ast.Exprs.Expr
import com.whatsapp.eqwalizer.ast.Pos

object Diagnostic {
  trait Diagnostic {
    val pos: Pos
    val msg: String
    def explanation: Option[String] = None
    def errorName: String // stable identifier for the class of error, to be used in metrics
    def docURL: String = s"https://fb.me/eqwalizer_errors#$errorName"
    def erroneousExpr: Option[Expr]
  }
}

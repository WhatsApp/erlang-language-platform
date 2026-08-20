/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.util.Diagnostic.Diagnostic

import scala.collection.mutable

class DiagnosticsInfo {
  private val moduleDiagnosticsInfo: mutable.ListBuffer[Diagnostic] = mutable.ListBuffer.empty

  def add(diag: Diagnostic): Unit = {
    moduleDiagnosticsInfo.addOne(diag)
  }

  def popErrors(): List[Diagnostic] = {
    val errors = moduleDiagnosticsInfo.distinct.toList
    moduleDiagnosticsInfo.clear()
    errors
  }
}

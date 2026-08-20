/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.whatsapp.eqwalizer

import com.whatsapp.eqwalizer.util.ELPDiagnostics

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      help()
      return
    }

    val cmd = args(0)

    cmd match {
      case "ipc"            => ipc(args)
      case "ipc-check-funs" => ELPDiagnostics.getDiagnosticsIpcCheckFuns()
      case _                => help()
    }
  }

  private def ipc(ipcArgs: Array[String]): Unit = {
    val modules = ipcArgs.tail
    ELPDiagnostics.getDiagnosticsIpc(modules)
  }

  private def help(): Unit =
    Console.print(helpText)

  private val helpText: String = {
    """com.whatsapp.eqwalizer.Main
      |eqWAlizer is meant to be used from ELP
      |""".stripMargin
  }
}

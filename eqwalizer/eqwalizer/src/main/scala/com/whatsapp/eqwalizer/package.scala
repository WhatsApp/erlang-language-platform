/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.whatsapp

import com.typesafe.config.ConfigFactory

package object eqwalizer {
  enum Mode {
    case Shell, ElpCli, ElpIde
  }

  object Mode {
    def fromString(str: String): Option[Mode] = {
      str match {
        case "shell"   => Some(Shell)
        case "elp_cli" => Some(ElpCli)
        case "elp_ide" => Some(ElpIde)
        case _         => None
      }
    }
  }

  case class Config(
      overloadedSpecDynamicResult: Boolean,
      mode: Mode,
      reportDynamicLambdas: Boolean,
  )

  lazy val config: Config = {
    val config = ConfigFactory.load().getConfig("eqwalizer")
    val modeStr = config.getString("mode")
    val mode = Mode.fromString(modeStr).getOrElse(throw new IllegalArgumentException(s"Unknown mode ${modeStr}"))
    Config(
      overloadedSpecDynamicResult = config.getBoolean("overloaded_spec_dynamic_result"),
      mode,
      reportDynamicLambdas = config.getBoolean("report_dynamic_lambdas"),
    )
  }
}

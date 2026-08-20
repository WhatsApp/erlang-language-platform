/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

package com.whatsapp.eqwalizer.ast.stub

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Id
import com.whatsapp.eqwalizer.ast.InvalidDiagnostics.Invalid

private case class ModuleStub(
    module: String,
    exports: Set[Id],
    imports: Map[Id, String],
    nativeRecordImports: Map[String, String] = Map.empty,
    invalids: List[Invalid],
)

private object ModuleStub {
  implicit val codec: JsonValueCodec[ModuleStub] = JsonCodecMaker.make(
    CodecMakerConfig
      .withMapMaxInsertNumber(65536)
      .withSetMaxInsertNumber(65536)
      .withAllowRecursiveTypes(true)
      .withDiscriminatorFieldName(None)
      .withFieldNameMapper(JsonCodecMaker.enforce_snake_case)
  )
}

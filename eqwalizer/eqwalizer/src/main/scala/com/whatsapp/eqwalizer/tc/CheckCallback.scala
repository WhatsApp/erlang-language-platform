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

import com.whatsapp.eqwalizer.ast.Forms._
import com.whatsapp.eqwalizer.ast.Types._
import com.whatsapp.eqwalizer.ast.stub.Db
import com.whatsapp.eqwalizer.tc.TcDiagnostics._

class CheckCallback(pipelineContext: PipelineContext) {
  private lazy val subtype = pipelineContext.subtype
  private implicit val pipelineCtx: PipelineContext = pipelineContext

  def checkImpl(module: String, b: Behaviour, cb: Callback, isOptional: Boolean): Option[TypeError] =
    if (Db.isExported(module, cb.id)) {
      Db.getSpec(module, cb.id) match {
        case Some(FunSpec(_, impl)) =>
          // don't validate invalid callbacks (callback validation is defeasible)
          if (cb.tys.isEmpty) return None
          val expectedResTy = subtype.join(cb.tys.map(_.resTy))
          if (!subtype.subType(impl.resTy, expectedResTy)) {
            return Some(IncorrectCallbackReturn(b.pos, b.name, cb.id.toString, expectedResTy, impl.resTy))
          }

          val badParamOpt = impl.argTys.zipWithIndex.find { case (implArgTy, index) =>
            !cb.tys.exists { case FunType(_, cbArgTys, _) =>
              val cbArgTy = cbArgTys(index)
              val approxMeet = subtype.meet(implArgTy, cbArgTy)
              val hasOverlap =
                Subtype.isNoneType(implArgTy) || Subtype.isNoneType(cbArgTy) || !Subtype.isNoneType(approxMeet)
              hasOverlap
            }
          }
          badParamOpt match {
            case Some((implArgTy, paramIndex)) =>
              val exp = subtype.join(cb.tys.map(_.argTys(paramIndex)))
              Some(IncorrectCallbackParams(b.pos, b.name, cb.id.toString, paramIndex, expected = exp, got = implArgTy))
            case None =>
              None
          }
        case None =>
          // allow unspecced behaviour implementations
          None
      }
    } else {
      if (isOptional) None
      else Some(MissingCallback(b.pos, b.name, cb.id.toString))
    }
}

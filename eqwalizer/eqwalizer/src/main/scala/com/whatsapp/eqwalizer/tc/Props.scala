/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Types.{Key, Type}

sealed trait Prop
case object Unknown extends Prop
case object True extends Prop
case object False extends Prop
case class Pos(obj: Obj, t: Type) extends Prop
case class Neg(obj: Obj, t: Type) extends Prop
case class And(props: List[Prop]) extends Prop
case class Or(props: List[Prop]) extends Prop

sealed trait Obj
case class VarObj(v: String) extends Obj
case class FieldObj(field: Field, obj: Obj) extends Obj

sealed trait Field
case class TupleField(index: Int, arity: Option[Int]) extends Field
case class RecordField(field: String, recName: String) extends Field
case class MapField(field: Key) extends Field
case object ListHead extends Field
case object ListTail extends Field

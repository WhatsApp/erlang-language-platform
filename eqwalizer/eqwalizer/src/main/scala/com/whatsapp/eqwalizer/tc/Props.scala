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

import com.whatsapp.eqwalizer.ast.Types.{Key, Type}

sealed trait Prop
sealed trait SProp extends Prop
case object Unknown extends SProp
case object True extends SProp
case object False extends SProp
case class Pos(obj: Obj, t: Type) extends SProp
case class Neg(obj: Obj, t: Type) extends SProp
case class And(props: List[SProp | Or]) extends Prop
case class Or(props: List[SProp | And]) extends Prop

sealed trait Obj
case class VarObj(v: String) extends Obj
case class FieldObj(field: Field, obj: Obj) extends Obj

sealed trait Field
case class TupleField(index: Int, arity: Option[Int]) extends Field
case class RecordField(field: String, recName: String) extends Field
case class MapField(field: Key) extends Field
case object AnyMapField extends Field
case object ListHead extends Field
case object ListTail extends Field

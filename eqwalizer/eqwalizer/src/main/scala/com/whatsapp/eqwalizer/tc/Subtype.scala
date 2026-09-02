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

import com.whatsapp.eqwalizer.ast.TypeVars
import com.whatsapp.eqwalizer.ast.Types.*
import com.whatsapp.eqwalizer.tc.Subtype.CType

import scala.util.boundary

object Subtype {
  def isNoneType(t: Type): Boolean =
    t match {
      case NoneType =>
        true
      case UnionType(ts) =>
        ts.forall(isNoneType)
      case BoundedDynamicType(bound) =>
        isNoneType(bound)
      case _ =>
        false
    }

  private enum ValueKind {
    case Atom, Binary, Fun, List, Map, Integer, Float, Pid, Port, Reference, Tuple, NativeRecord
  }

  type CType = AtomLitType | AtomType.type | BinaryType.type | AnyFunType.type | FunType | AnyArityFunType |
    NilType.type | ListType | ConsType | MapType | IntegerType.type | FloatType.type | PidType.type | PortType.type |
    ReferenceType.type | AnyTupleType.type | TupleType | RecordType | RefinedRecordType | NativeRecordType |
    AnyNativeRecordType.type

  private def kind(t: CType): ValueKind = t match {
    case AtomLitType(_) | AtomType =>
      ValueKind.Atom
    case BinaryType =>
      ValueKind.Binary
    case AnyFunType | FunType(_, _, _) | AnyArityFunType(_) =>
      ValueKind.Fun
    case NilType | ListType(_) | ConsType(_, _) =>
      ValueKind.List
    case MapType(_, _, _) =>
      ValueKind.Map
    case IntegerType =>
      ValueKind.Integer
    case FloatType =>
      ValueKind.Float
    case PidType =>
      ValueKind.Pid
    case PortType =>
      ValueKind.Port
    case ReferenceType =>
      ValueKind.Reference
    case AnyTupleType | TupleType(_) | RecordType(_) | RefinedRecordType(_, _) =>
      ValueKind.Tuple
    case NativeRecordType(_) | AnyNativeRecordType =>
      ValueKind.NativeRecord
  }
}

class Subtype(pipelineContext: PipelineContext) {
  private val util = pipelineContext.util
  private lazy val instantiate = pipelineContext.instantiate
  private lazy val constraints = pipelineContext.constraints

  private sealed trait Polarity
  private case object + extends Polarity
  private case object - extends Polarity

  private def negate(p: Polarity): Polarity =
    p match {
      case + => -
      case - => +
    }

  // classical consistent subtyping
  def subType(t1: Type, t2: Type): Boolean =
    subType(t1, t2, Set.empty)

  private def subType(t1: Type, t2: Type, seen: Set[(Type, Type)]): Boolean = {
    (t1, t2) match {
      case (_, _) if seen(t1 -> t2) =>
        true
      case (_, _) if t1 == t2 =>
        true
      case (_, AnyType) =>
        true
      case (NoneType, _) =>
        true

      case (DynamicType, _) =>
        true
      case (_, DynamicType) =>
        true

      case (BoundedDynamicType(_), _) =>
        true
      case (_, BoundedDynamicType(bound)) =>
        subType(t1, bound, seen)

      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        containsType(t1, t2) || subType(body, t2, seen + (t1 -> t2))
      case (_, RemoteType(rid, args)) =>
        val body = util.getTypeDeclBody(rid, args)
        subType(t1, body, seen + (t1 -> t2))

      case (UnionType(tys1), _) =>
        tys1.forall(subType(_, t2, seen))

      case (ty1: TupleType, ty2: UnionType) if ty1.argTys.nonEmpty =>
        ty1.argTys.zipWithIndex.exists { case (elem, i) => subtypeTuple(elem, ty2, i, ty1, seen) }

      case (_, UnionType(tys2)) =>
        tys2.exists(subType(t1, _, seen))

      case (AtomLitType(_), AtomType) =>
        true
      case (TupleType(_), AnyTupleType) =>
        true
      case (RecordType(_), AnyTupleType) =>
        true
      case (RefinedRecordType(_, _), AnyTupleType) =>
        true
      case (r: RecordType, t: TupleType) =>
        util.getRecord(r.module, r.name) match {
          case Some(recDecl) =>
            subType(recordAsTuple(recDecl), t, seen)
          case None =>
            false
        }
      case (t: TupleType, r: RecordType) =>
        util.getRecord(r.module, r.name) match {
          case Some(recDecl) =>
            subType(t, recordAsTuple(recDecl), seen)
          case None =>
            false
        }
      case (r: RefinedRecordType, t: TupleType) =>
        util.getRecord(r.recType.module, r.recType.name) match {
          case Some(recDecl) =>
            subType(refinedRecordAsTuple(recDecl, r), t, seen)
          case None =>
            false
        }
      case (t: TupleType, r: RefinedRecordType) =>
        util.getRecord(r.recType.module, r.recType.name) match {
          case Some(recDecl) =>
            subType(t, refinedRecordAsTuple(recDecl, r), seen)
          case None =>
            false
        }
      case (refRec: RefinedRecordType, rec: RecordType) =>
        refRec.recType.name == rec.name
      case (rec: RecordType, refRec: RefinedRecordType) if rec == refRec.recType =>
        util.getRecord(rec.module, rec.name) match {
          case Some(recDecl) =>
            refRec.fields.forall(f => subType(recDecl.fMap(f._1).tp, f._2, seen))
          case None =>
            // rec was elaborated via is_record/3, optimistically assuming subtyping here
            true
        }
      case (refRec1: RefinedRecordType, refRec2: RefinedRecordType) if refRec1.recType == refRec2.recType =>
        util.getRecord(refRec1.recType.module, refRec1.recType.name) match {
          case None => false
          case Some(recDecl) =>
            refRec2.fields.forall { case (fName, fTy) =>
              if (refRec1.fields.contains(fName))
                subType(refRec1.fields(fName), fTy, seen)
              else
                subType(recDecl.fMap(fName).tp, fTy, seen)
            }
        }
      case (NativeRecordType(_), AnyNativeRecordType) =>
        true
      case (NativeRecordType(id1), NativeRecordType(id2)) if id1 == id2 =>
        true
      case (FunType(_, _, _), AnyFunType) =>
        true
      case (AnyFunType, FunType(_, _, _)) =>
        true
      case (AnyArityFunType(_), AnyFunType) =>
        true
      case (AnyFunType, AnyArityFunType(_)) =>
        true
      case (FunType(_, _, resTy1), AnyArityFunType(resTy2)) =>
        subType(resTy1, resTy2, seen)
      case (AnyArityFunType(resTy1), FunType(_, _, resTy2)) =>
        subType(resTy1, resTy2, seen)
      case (AnyArityFunType(resTy1), AnyArityFunType(resTy2)) =>
        subType(resTy1, resTy2, seen)
      case (TupleType(tys1), TupleType(tys2)) if tys1.size == tys2.size =>
        tys1.lazyZip(tys2).forall(subType(_, _, seen))
      case (NilType, ListType(_)) =>
        true
      case (ListType(e), NilType) =>
        subType(e, NoneType, seen)
      case (ListType(et1), ListType(et2)) =>
        subType(et1, et2, seen)
      case (ListType(e), _) =>
        val body = UnionType(Set(NilType, ConsType(e, ListType(e))))
        subType(body, t2, seen + (t1 -> t2))
      case (ConsType(h1, tl1), ConsType(h2, tl2)) =>
        subType(h1, h2, seen) && subType(tl1, tl2, seen)
      case (ConsType(h, tl), ListType(e)) =>
        subType(h, e, seen) && subType(tl, ListType(e), seen)
      case (ft1: FunType, ft2: FunType) if ft1.argTys.size == ft2.argTys.size =>
        TypeVars.conformForalls(ft1, ft2) match {
          case None =>
            (ft1.forall > 0) && (ft2.forall == 0) && {
              val (vars, ft) = instantiate.instantiate(ft1)
              constraints.satisfiable(
                toSolve = vars.toSet,
                varsToElim = Set.empty,
                bounds = ft2.argTys.zip(ft.argTys) :+ (ft.resTy, ft2.resTy),
              )
            }
          case Some((FunType(_, args1, res1), FunType(_, args2, res2))) =>
            subType(res1, res2, seen) && args2.lazyZip(args1).forall(subType(_, _, seen))
        }
      case (MapType(props1, kT1, vT1), MapType(props2, kT2, vT2)) =>
        boundary {
          val tolerantSubtype = isDynamicType(kT1) && isDynamicType(vT1)
          val reqKeys1 = props1.collect { case (k, MapProp(true, _)) => k }.toSet
          val reqKeys2 = props2.collect { case (k, MapProp(true, _)) => k }.toSet
          // Verify that all required keys of M2 are also required keys in M1
          if (!tolerantSubtype && !reqKeys2.subsetOf(reqKeys1)) return false
          // Check subtype of props in M1 to either the corresponding prop in M2, or its default association
          for ((key1, prop1) <- props1) {
            props2.get(key1) match {
              case Some(prop2) if !subType(prop1.tp, prop2.tp, seen) =>
                boundary.break(false)
              case None if !subType(Key.asType(key1), kT2, seen) || !subType(prop1.tp, vT2, seen) =>
                boundary.break(false)
              case _ =>
            }
          }
          // Check that new keys in M2 are compatible with the default association in M1
          val onlyProps2 = props2.removedAll(props1.keySet).toList
          val onlyCompatProps2 = onlyProps2.filter { case (key2, _) => subType(Key.asType(key2), kT1, seen) }
          for ((_, prop2) <- onlyCompatProps2) {
            if (!subType(kT1, NoneType, seen) && !subType(vT1, prop2.tp, seen))
              boundary.break(false)
          }
          // Finally that the default association in M1 is covered by M2
          // Either it is fully covered by the compatible props of M2 checked above, in which
          // case it is a subtype, e.g. #{a | b => atom()} <: #{a => atom(), b => atom()}
          val domainProps2 = join(onlyCompatProps2.map(kp => Key.asType(kp._1)))
          if (domainProps2 != NoneType && subType(kT1, domainProps2, seen))
            return true
          // Or it must be covered by the compatible props + the default association
          val domain2 = join(kT2, domainProps2)
          subType(kT1, domain2, seen) && subType(vT1, vT2, seen)
        }
      case _ =>
        false
    }
  }

  def gradualSubType(t1: Type, t2: Type): Boolean =
    subTypePol(t1, t2, Set.empty)(+) && subTypePol(t1, t2, Set.empty)(-)

  private def subTypePol(t1: Type, t2: Type, seen: Set[(Type, Type, Polarity)])(implicit p: Polarity): Boolean =
    (t1, t2) match {
      case (_, _) if seen((t1, t2, p)) =>
        true
      case (_, _) if t1 == t2 =>
        true

      case (_, AnyType) =>
        true
      case (NoneType, _) =>
        true
      case (DynamicType, _) if p == - =>
        true
      case (DynamicType, _) if p == + =>
        subTypePol(AnyType, t2, seen)
      case (_, DynamicType) if p == - =>
        subTypePol(t1, NoneType, seen)
      case (_, DynamicType) if p == + =>
        true
      case (BoundedDynamicType(_), _) if p == - =>
        true
      case (BoundedDynamicType(bound), _) if p == + =>
        subTypePol(bound, t2, seen)
      case (_, BoundedDynamicType(_)) if p == - =>
        subTypePol(t1, NoneType, seen)
      case (_, BoundedDynamicType(bound)) if p == + =>
        subTypePol(t1, bound, seen)
      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        containsType(t1, t2, p) || subTypePol(body, t2, seen + ((t1, t2, p)))
      case (_, RemoteType(rid, args)) =>
        val body = util.getTypeDeclBody(rid, args)
        subTypePol(t1, body, seen + ((t1, t2, p)))

      case (UnionType(tys1), _) =>
        tys1.forall(subTypePol(_, t2, seen))

      case (ty1: TupleType, ty2: UnionType) if ty1.argTys.nonEmpty =>
        ty1.argTys.zipWithIndex.exists { case (elem, i) => subtypeTuple(elem, ty2, i, ty1, seen) }

      case (_, UnionType(tys2)) =>
        tys2.exists(subTypePol(t1, _, seen))

      case (AtomLitType(_), AtomType) =>
        true
      case (TupleType(_), AnyTupleType) =>
        true
      case (RecordType(_), AnyTupleType) =>
        true
      case (RefinedRecordType(_, _), AnyTupleType) =>
        true
      case (r: RecordType, t: TupleType) =>
        util.getRecord(r.module, r.name) match {
          case Some(recDecl) =>
            subTypePol(recordAsTuple(recDecl), t, seen)
          case None =>
            false
        }
      case (t: TupleType, r: RecordType) =>
        util.getRecord(r.module, r.name) match {
          case Some(recDecl) =>
            subTypePol(t, recordAsTuple(recDecl), seen)
          case None =>
            false
        }
      case (r: RefinedRecordType, t: TupleType) =>
        util.getRecord(r.recType.module, r.recType.name) match {
          case Some(recDecl) =>
            subTypePol(refinedRecordAsTuple(recDecl, r), t, seen)
          case None =>
            false
        }
      case (t: TupleType, r: RefinedRecordType) =>
        util.getRecord(r.recType.module, r.recType.name) match {
          case Some(recDecl) =>
            subTypePol(t, refinedRecordAsTuple(recDecl, r), seen)
          case None =>
            false
        }
      case (refRec: RefinedRecordType, rec: RecordType) =>
        refRec.recType.name == rec.name
      case (rec: RecordType, refRec: RefinedRecordType) if rec == refRec.recType =>
        util.getRecord(rec.module, rec.name) match {
          case Some(recDecl) =>
            refRec.fields.forall(f => subTypePol(recDecl.fMap(f._1).tp, f._2, seen))
          case None =>
            // rec was elaborated via is_record/3, optimistically assuming subtyping here
            true
        }
      case (refRec1: RefinedRecordType, refRec2: RefinedRecordType) if refRec1.recType == refRec2.recType =>
        util.getRecord(refRec1.recType.module, refRec1.recType.name) match {
          case None => false
          case Some(recDecl) =>
            refRec2.fields.forall { case (fName, fTy) =>
              if (refRec1.fields.contains(fName))
                subTypePol(refRec1.fields(fName), fTy, seen)
              else
                subTypePol(recDecl.fMap(fName).tp, fTy, seen)
            }
        }
      case (NativeRecordType(_), AnyNativeRecordType) =>
        true
      case (NativeRecordType(id1), NativeRecordType(id2)) if id1 == id2 =>
        true
      case (FunType(_, _, _), AnyFunType) =>
        true
      case (AnyFunType, FunType(_, _, _)) if p == - =>
        true
      case (AnyArityFunType(_), AnyFunType) =>
        true
      case (AnyFunType, AnyArityFunType(_)) if p == - =>
        true
      case (FunType(_, _, resTy1), AnyArityFunType(resTy2)) =>
        subTypePol(resTy1, resTy2, seen)
      case (AnyArityFunType(resTy1), FunType(_, _, resTy2)) =>
        subTypePol(resTy1, resTy2, seen)
      case (AnyArityFunType(resTy1), AnyArityFunType(resTy2)) =>
        subTypePol(resTy1, resTy2, seen)
      case (TupleType(tys1), TupleType(tys2)) if tys1.size == tys2.size =>
        tys1.lazyZip(tys2).forall(subTypePol(_, _, seen))
      case (NilType, ListType(_)) =>
        true
      case (ListType(e), NilType) =>
        subTypePol(e, NoneType, seen)
      case (ListType(et1), ListType(et2)) =>
        subTypePol(et1, et2, seen)
      case (ListType(e), _) =>
        val body = UnionType(Set(NilType, ConsType(e, ListType(e))))
        subTypePol(body, t2, seen + ((t1, t2, p)))
      case (ConsType(h1, t1), ConsType(h2, t2)) =>
        subTypePol(h1, h2, seen) && subTypePol(t1, t2, seen)
      case (ConsType(h, t), ListType(e)) =>
        subTypePol(h, e, seen) && subTypePol(t, ListType(e), seen)
      case (ft1: FunType, ft2: FunType) if ft1.argTys.size == ft2.argTys.size =>
        TypeVars.conformForalls(ft1, ft2) match {
          case None => false
          case Some((FunType(_, args1, res1), FunType(_, args2, res2))) =>
            subTypePol(res1, res2, seen) && args2
              .lazyZip(args1)
              .forall(subTypePol(_, _, seen)(negate(p)))
        }
      case (MapType(props1, kT1, vT1), MapType(props2, kT2, vT2)) =>
        boundary {
          val tolerantSubtype = isDynamicType(kT1) && isDynamicType(vT1) && p == -
          val reqKeys1 = props1.collect { case (k, MapProp(true, _)) => k }.toSet
          val reqKeys2 = props2.collect { case (k, MapProp(true, _)) => k }.toSet
          // Verify that all required keys of M2 are also required keys in M1
          if (!tolerantSubtype && !reqKeys2.subsetOf(reqKeys1)) return false
          // Check subtype of props in M1 to either the corresponding prop in M2, or its default association
          for ((key1, prop1) <- props1) {
            props2.get(key1) match {
              case Some(prop2) if !subTypePol(prop1.tp, prop2.tp, seen) =>
                boundary.break(false)
              case None if !subTypePol(Key.asType(key1), kT2, seen) || !subTypePol(prop1.tp, vT2, seen) =>
                boundary.break(false)
              case _ =>
            }
          }
          // Check that new keys in M2 are compatible with the default association in M1
          val onlyProps2 = props2.removedAll(props1.keySet).toList
          val onlyCompatProps2 = onlyProps2.filter { case (key2, _) => subTypePol(Key.asType(key2), kT1, seen) }
          for ((_, prop2) <- onlyCompatProps2) {
            if (!subTypePol(kT1, NoneType, seen) && !subTypePol(vT1, prop2.tp, seen))
              boundary.break(false)
          }
          // Finally that the default association in M1 is covered by M2
          // Either it is fully covered by the compatible props of M2 checked above, in which
          // case it is a subtype, e.g. #{a | b => atom()} <: #{a => atom(), b => atom()}
          val domainProps2 = join(onlyCompatProps2.map(kp => Key.asType(kp._1)))
          if (domainProps2 != NoneType && subTypePol(kT1, domainProps2, seen))
            return true
          // Or it must be covered by the compatible props + the default association
          val domain2 = join(kT2, domainProps2)
          subTypePol(kT1, domain2, seen) && subTypePol(vT1, vT2, seen)
        }
      case _ =>
        false
    }

  def eqv(t1: Type, t2: Type): Boolean =
    subType(t1, t2) && subType(t2, t1)

  def gradualEqv(t1: Type, t2: Type): Boolean =
    gradualSubType(t1, t2) && gradualSubType(t2, t1)

  def isDynamicType(t: Type): Boolean =
    subType(t, NoneType) && subType(AnyType, t)

  private def containsType(t1: Type, t2: Type): Boolean = {
    t2 match {
      case AnyType       => true
      case _ if t1 == t2 => true
      case UnionType(tys) =>
        tys.exists(containsType(t1, _))
      case BoundedDynamicType(bound) =>
        containsType(t1, bound)
      case _ => false
    }
  }

  private def containsType(t1: Type, t2: Type, p: Polarity): Boolean = {
    t2 match {
      case AnyType       => true
      case _ if t1 == t2 => true
      case UnionType(tys) =>
        tys.exists(containsType(t1, _, p))
      case BoundedDynamicType(bound) if p == + =>
        containsType(t1, bound, p)
      case _ => false
    }
  }

  /** Checks whether originalTuple.updated(proj, t1) < t2, by expanding t1 if it is an alias or a union.
   */
  private def subtypeTuple(
      t1: Type,
      t2: Type,
      proj: Int,
      originalTuple: TupleType,
      seen: Set[(Type, Type)],
  ): Boolean =
    (t1, t2) match {
      // Standard cases from subType
      case (NoneType, _) =>
        true
      case (_, AnyType) =>
        true
      case (_, AnyTupleType) =>
        true
      case (_, DynamicType) =>
        true
      case (_, BoundedDynamicType(bound)) =>
        subtypeTuple(t1, bound, proj, originalTuple, seen)
      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        subtypeTuple(body, t2, proj, originalTuple, seen)

      // ** Main logic **
      case (UnionType(tys1), _) =>
        // Distributes a tuple of unions into a union of tuples
        tys1.forall(subtypeTuple(_, t2, proj, originalTuple, seen))
      case (_, TupleType(tys2)) if originalTuple.argTys.size == tys2.size =>
        // Injects the union back into the original tuple
        subType(TupleType(originalTuple.argTys.updated(proj, t1)), t2, seen)
      // Standard cases from subType
      case (_, RemoteType(rid, args)) =>
        val body = util.getTypeDeclBody(rid, args)
        subtypeTuple(t1, body, proj, originalTuple, seen + (originalTuple -> t2))
      case (_, UnionType(tys)) =>
        tys.exists(t => subtypeTuple(t1, t, proj, originalTuple, seen))
      case (_, r: RecordType) =>
        util.getRecord(r.module, r.name) match {
          case Some(recDecl) =>
            subtypeTuple(t1, recordAsTuple(recDecl), proj, originalTuple, seen)
          case None =>
            false
        }
      case (_, r: RefinedRecordType) =>
        val recTy = r.recType
        util.getRecord(recTy.module, recTy.name) match {
          case Some(recDecl) =>
            subtypeTuple(t1, refinedRecordAsTuple(recDecl, r), proj, originalTuple, seen)
          case None =>
            false
        }
      case _ =>
        false
    }

  /** Checks whether originalTuple.updated(proj, t1) < t2, by expanding t1 if it is an alias or a union.
    */
  private def subtypeTuple(
      t1: Type,
      t2: Type,
      proj: Int,
      originalTuple: TupleType,
      seen: Set[(Type, Type, Polarity)],
  )(implicit p: Polarity): Boolean =
    (t1, t2) match {
      // Standard cases from subType
      case (NoneType, _) =>
        true
      case (_, AnyType) =>
        true
      case (_, AnyTupleType) =>
        true
      case (_, DynamicType) if p == + =>
        true
      case (_, DynamicType) if p == - =>
        false
      case (DynamicType, _) if p == + =>
        subtypeTuple(AnyType, t2, proj, originalTuple, seen)
      case (_, BoundedDynamicType(bound)) if p == + =>
        subtypeTuple(t1, bound, proj, originalTuple, seen)
      case (_, BoundedDynamicType(_)) if p == - =>
        false
      case (BoundedDynamicType(bound), _) if p == + =>
        subtypeTuple(bound, t2, proj, originalTuple, seen)
      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        subtypeTuple(body, t2, proj, originalTuple, seen)

      // ** Main logic **
      case (UnionType(tys1), _) =>
        // Distributes a tuple of unions into a union of tuples
        tys1.forall(subtypeTuple(_, t2, proj, originalTuple, seen))
      case (_, TupleType(tys2)) if originalTuple.argTys.size == tys2.size =>
        // Injects the union back into the original tuple
        subTypePol(TupleType(originalTuple.argTys.updated(proj, t1)), t2, seen)
      // Standard cases from subType
      case (_, RemoteType(rid, args)) =>
        val body = util.getTypeDeclBody(rid, args)
        subtypeTuple(t1, body, proj, originalTuple, seen + ((originalTuple, t2, p)))
      case (_, UnionType(tys)) =>
        tys.exists(t => subtypeTuple(t1, t, proj, originalTuple, seen))
      case (_, r: RecordType) =>
        util.getRecord(r.module, r.name) match {
          case Some(recDecl) =>
            subtypeTuple(t1, recordAsTuple(recDecl), proj, originalTuple, seen)
          case None =>
            false
        }
      case (_, r: RefinedRecordType) =>
        val recTy = r.recType
        util.getRecord(recTy.module, recTy.name) match {
          case Some(recDecl) =>
            subtypeTuple(t1, refinedRecordAsTuple(recDecl, r), proj, originalTuple, seen)
          case None =>
            false
        }
      case _ =>
        false
    }

  def joinEnvs(envs: List[Env]): Env = {
    val vars = envs.map(_.keySet).reduce(_.intersect(_))
    var envAcc: Env = envs.head.filter { case (k, _) => vars(k) }
    for { env <- envs.tail; v <- vars } envAcc = envAcc.updated(v, join(envAcc(v), env(v)))
    envAcc
  }

  def join(ts: Iterable[Type]): Type =
    join(NoneType, ts)

  def join(tinit: Type, ts: Iterable[Type]): Type =
    ts.fold(tinit)(join)

  def join(t1: Type, t2: Type): Type = {
    if (gradualSubType(t1, t2)) t2
    else if (gradualSubType(t2, t1)) t1
    else {
      (t1, t2) match {
        case (UnionType(args1), UnionType(args2)) => UnionType(args1 ++ args2)
        case (UnionType(args1), _)                => UnionType(args1 + t2)
        case (_, UnionType(args2))                => UnionType(args2 + t1)
        case (_, _)                               => UnionType(Set(t1, t2))
      }
    }
  }

  private def mayOverlapSimple(t1: CType, t2: CType): Boolean =
    Subtype.kind(t1) == Subtype.kind(t2)

  def mayOverlap(t1: Type, t2: Type): Boolean =
    mayOverlap(t1, t2, Set.empty)

  private def mayOverlap(t1: Type, t2: Type, seen: Set[(Type, Type)]): Boolean =
    (t1, t2) match {
      case (NoneType, _) =>
        false
      case (_, NoneType) =>
        false
      case (_, _) if t1 == t2 || seen.contains(t1, t2) || seen.contains(t2, t1) =>
        true
      case (AnyType, _) =>
        true
      case (_, AnyType) =>
        true

      case (DynamicType, _) =>
        true
      case (_, DynamicType) =>
        true

      case (BoundedDynamicType(bound), _) =>
        mayOverlap(bound, t2, seen)
      case (_, BoundedDynamicType(bound)) =>
        mayOverlap(t1, bound, seen)

      case (FreeVarType(_), _) =>
        true
      case (_, FreeVarType(_)) =>
        true

      case (BoundVarType(_), _) =>
        true
      case (_, BoundVarType(_)) =>
        true

      // Unions
      case (UnionType(ts), _) =>
        ts.exists(mayOverlap(_, t2, seen))
      case (_, UnionType(ts)) =>
        ts.exists(mayOverlap(t1, _, seen))

      case (NativeRecordType(id1), NativeRecordType(id2)) =>
        id1 == id2
      case (AtomLitType(l1), AtomLitType(l2)) =>
        l1 == l2

      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        mayOverlap(body, t2, seen + (t1 -> t2))
      case (_, RemoteType(rid, args)) =>
        val body = util.getTypeDeclBody(rid, args)
        mayOverlap(t1, body, seen + (t1 -> t2))

      // funs
      case (FunType(_, ins1, _), FunType(_, ins2, _)) =>
        ins1.size == ins2.size
      case (FunType(_, _, _), AnyFunType) =>
        true
      case (AnyFunType, FunType(_, _, _)) =>
        true
      case (AnyArityFunType(_), AnyFunType) =>
        true
      case (AnyFunType, AnyArityFunType(_)) =>
        true
      case (AnyArityFunType(_), FunType(_, _, _)) =>
        true
      case (FunType(_, _, _), AnyArityFunType(_)) =>
        true
      case (FunType(_, _, _), _) =>
        false
      case (_, FunType(_, _, _)) =>
        false
      case (AnyFunType, _) =>
        false
      case (_, AnyFunType) =>
        false

      // tuples and records
      case (TupleType(ts1), TupleType(ts2)) =>
        ts1.size == ts2.size && ts1.lazyZip(ts2).forall(mayOverlap(_, _, seen))
      case (TupleType(_), AnyTupleType) =>
        true
      case (AnyTupleType, TupleType(_)) =>
        true
      case (RecordType(_), AnyTupleType) =>
        true
      case (RefinedRecordType(_, _), AnyTupleType) =>
        true
      case (AnyTupleType, RefinedRecordType(_, _)) =>
        true
      case (AnyTupleType, RecordType(_)) =>
        true
      case (RecordType(n1), RecordType(n2)) =>
        n1 == n2
      case (RefinedRecordType(t1, fields1), RefinedRecordType(t2, fields2)) =>
        // Only the fields refined in both records can make them disjoint.
        t1.name == t2.name &&
        fields1.keySet.intersect(fields2.keySet).forall(fN => mayOverlap(fields1(fN), fields2(fN), seen))
      case (RefinedRecordType(t, _), RecordType(n)) =>
        n == t.name
      case (RecordType(n), RefinedRecordType(t, _)) =>
        n == t.name
      case (r: RecordType, TupleType(elems)) =>
        util.getRecordArity(r.module, r.name) match {
          case Some(arity) if arity + 1 == elems.size =>
            mayOverlap(AtomLitType(r.name), elems.head, seen)
          case _ =>
            false
        }
      case (TupleType(elems), r: RecordType) =>
        util.getRecordArity(r.module, r.name) match {
          case Some(arity) if arity + 1 == elems.size =>
            mayOverlap(elems.head, AtomLitType(r.name), seen)
          case _ =>
            false
        }
      case (RefinedRecordType(t, _), TupleType(elems)) =>
        util.getRecordArity(t.module, t.name) match {
          case Some(arity) if arity + 1 == elems.size =>
            mayOverlap(AtomLitType(t.name), elems.head, seen)
          case _ =>
            false
        }
      case (TupleType(elems), RefinedRecordType(t, _)) =>
        util.getRecordArity(t.module, t.name) match {
          case Some(arity) if arity + 1 == elems.size =>
            mayOverlap(elems.head, AtomLitType(t.name), seen)
          case _ =>
            false
        }
      case (TupleType(_), _) =>
        false
      case (_, TupleType(_)) =>
        false
      case (AnyTupleType, _) =>
        false
      case (_, AnyTupleType) =>
        false

      case (NilType, NilType) =>
        true
      case (NilType, ConsType(_, _)) =>
        false
      case (NilType, ListType(_)) =>
        true
      case (ConsType(_, _), NilType) =>
        false
      case (ConsType(h1, tl1), ConsType(h2, tl2)) =>
        mayOverlap(h1, h2, seen) && mayOverlap(tl1, tl2, seen)
      case (ConsType(h1, tl1), ListType(e2)) =>
        mayOverlap(h1, e2, seen) && mayOverlap(tl1, ListType(e2), seen)
      case (ListType(_), NilType) =>
        true
      case (ListType(e1), ConsType(h2, tl2)) =>
        mayOverlap(e1, h2, seen) && mayOverlap(ListType(e1), tl2, seen)
      case (ListType(_), ListType(_)) =>
        true
      case (ListType(_) | NilType | ConsType(_, _), _) =>
        false
      case (_, ListType(_) | NilType | ConsType(_, _)) =>
        false

      case (mt1 @ MapType(props1, kT1, vT1), mt2 @ MapType(props2, kT2, vT2)) =>
        // Checking that maps overlap in required associations.
        val reqKeys =
          props1.collect { case (k, MapProp(true, _)) => k }.toSet ++
            props2.collect { case (k, MapProp(true, _)) => k }
        reqKeys.forall(key => mayOverlap(mapValueType(key, mt1), mapValueType(key, mt2), seen))

      case (ct1: CType, ct2: CType) =>
        mayOverlapSimple(ct1, ct2)
    }

  private def mapValueType(key: Key, mt: MapType): Type =
    mt.props.get(key) match {
      case Some(prop) => prop.tp
      case None       => if (subType(Key.asType(key), mt.kType)) mt.vType else NoneType
    }

  // It tries to narrow t1 wrt t2.
  // Ideally, it finds t1 ∩ t2, but it's not guaranteed.
  // Read the implementation for more details.
  def meet(t1: Type, t2: Type): Type =
    meetAux(t1, t2, Set.empty)

  private def meetAux(t1: Type, t2: Type, seen: Set[(Type, Type)]): Type =
    if (!mayOverlap(t1, t2)) NoneType
    else if (gradualSubType(t1, t2)) t1
    else if (gradualSubType(t2, t1)) t2
    else
      (t1, t2) match {
        case (RemoteType(rid, args), _) =>
          if (seen(t1 -> t2) || seen(t2 -> t1)) t1
          else {
            val body = util.getTypeDeclBody(rid, args)
            val met = meetAux(body, t2, seen + (t1 -> t2))
            if (Subtype.isNoneType(met)) NoneType
            else if (met == body) t1
            else met
          }
        case (_, RemoteType(rid, args)) =>
          if (seen(t1 -> t2) || seen(t2 -> t1)) t1
          else {
            val body = util.getTypeDeclBody(rid, args)
            val met = meetAux(t1, body, seen + (t1 -> t2))
            if (Subtype.isNoneType(met)) NoneType
            else if (met == body) t2
            else met
          }
        case (BoundedDynamicType(b1), DynamicType) =>
          BoundedDynamicType(b1)
        case (DynamicType, BoundedDynamicType(b2)) =>
          BoundedDynamicType(b2)
        case (DynamicType, t) => BoundedDynamicType(t)
        case (t, DynamicType) => BoundedDynamicType(t)
        case (BoundedDynamicType(b1), BoundedDynamicType(b2)) =>
          BoundedDynamicType(meetAux(b1, b2, seen))
        case (BoundedDynamicType(b1), _) =>
          BoundedDynamicType(meetAux(b1, t2, seen))
        case (_, BoundedDynamicType(b2)) =>
          BoundedDynamicType(meetAux(t1, b2, seen))
        // Composed "refinable" types - refining component by component
        case (UnionType(ty1s), _) =>
          join(ty1s.map(meetAux(_, t2, seen)))
        case (_, UnionType(ty2s)) =>
          join(ty2s.map(meetAux(t1, _, seen)))
        case (TupleType(elems1), TupleType(elems2)) if elems1.size == elems2.size =>
          val elems = elems1.zip(elems2).map { (a, b) => meetAux(a, b, seen) }
          TupleType_*(elems)
        case (NilType, ConsType(_, _)) =>
          NoneType
        case (ConsType(_, _), NilType) =>
          NoneType
        case (ConsType(h1, t1), ConsType(h2, t2)) =>
          val hMeet = meetAux(h1, h2, seen)
          val tMeet = meetAux(t1, t2, seen)
          ConsType_*(hMeet, tMeet)
        case (ConsType(h, t), ListType(eT)) =>
          val hMeet = meetAux(h, eT, seen)
          val tMeet = meetAux(t, ListType(eT), seen)
          ConsType_*(hMeet, tMeet)
        case (ListType(eT), ConsType(h, t)) =>
          val hMeet = meetAux(eT, h, seen)
          val tMeet = meetAux(ListType(eT), t, seen)
          ConsType_*(hMeet, tMeet)
        case (ListType(et1), ListType(et2)) =>
          val et = meetAux(et1, et2, seen)
          if (Subtype.isNoneType(et)) NilType
          else ListType(et)
        case (ft1: FunType, ft2: FunType) if ft1.argTys.size == ft2.argTys.size =>
          TypeVars.conformForalls(ft1, ft2) match {
            case None => NoneType
            case Some((FunType(forall, args1, res1), FunType(_, args2, res2))) =>
              FunType(
                forall,
                args1.lazyZip(args2).map(join),
                meetAux(res1, res2, seen),
              )
          }
        case (AnyArityFunType(resTy1), AnyArityFunType(resTy2)) =>
          AnyArityFunType(meetAux(resTy1, resTy2, seen))
        case (MapType(props1, kT1, vT1), MapType(props2, kT2, vT2)) =>
          boundary {
            var props: Map[Key, MapProp] = Map()
            val keys = props1.keySet ++ props2.keySet
            for (key <- keys) {
              val prop1 = props1.get(key)
              val prop2 = props2.get(key)
              val keyT = Key.asType(key)
              if ((prop1.isEmpty && !subType(keyT, kT1)) || (prop2.isEmpty && !subType(keyT, kT2))) {
                boundary.break(NoneType)
              }
              val propT1 = prop1.map(_.tp).getOrElse(vT1)
              val propT2 = prop2.map(_.tp).getOrElse(vT2)
              val req = prop1.exists(_.req) || prop2.exists(_.req)
              val meetType = meetAux(propT1, propT2, seen)
              props += (key -> MapProp(req, meetType))
            }
            MapType_*(props, meetAux(kT1, kT2, seen), meetAux(vT1, vT2, seen))
          }
        case (rt: RefinedRecordType, t: RecordType) if t == rt.recType => rt
        case (t: RecordType, rt: RefinedRecordType) if t == rt.recType => rt
        case (rt1: RefinedRecordType, rt2: RefinedRecordType) if rt1.recType == rt2.recType =>
          val fields = rt1.fields.keySet ++ rt2.fields.keySet
          val fieldsMeet = fields.map { fieldName =>
            val t1 = rt1.fields.getOrElse(fieldName, AnyType)
            val t2 = rt2.fields.getOrElse(fieldName, AnyType)
            fieldName -> meet(t1, t2)
          }.toMap
          if (fieldsMeet.values.exists(Subtype.isNoneType)) NoneType
          else RefinedRecordType(rt1.recType, fieldsMeet)
        case (r: RecordType, tt: TupleType) if overlapRecordTag(r, tt) =>
          meetRecordTuple(RefinedRecordType(r, Map()), tt, seen)
        case (tt: TupleType, r: RecordType) if overlapRecordTag(r, tt) =>
          meetRecordTuple(RefinedRecordType(r, Map()), tt, seen)
        case (rt: RefinedRecordType, tt: TupleType) if overlapRecordTag(rt.recType, tt) =>
          meetRecordTuple(rt, tt, seen)
        case (tt: TupleType, rt: RefinedRecordType) if overlapRecordTag(rt.recType, tt) =>
          meetRecordTuple(rt, tt, seen)

        case (NativeRecordType(id1), NativeRecordType(id2)) if id1 == id2 =>
          t1
        case (NativeRecordType(_), AnyNativeRecordType) => t1
        case (AnyNativeRecordType, NativeRecordType(_)) => t2

        // "Non-refinable" types. - Using the main type
        case (FreeVarType(_), _)                    => t1
        case (_, FreeVarType(_))                    => t1
        case (AnyFunType, FunType(_, _, _))         => t1
        case (FunType(_, _, _), AnyFunType)         => t1
        case (AnyArityFunType(_), FunType(_, _, _)) => t1
        case (FunType(_, _, _), AnyArityFunType(_)) => t1
        case (AnyArityFunType(_), AnyFunType)       => t1
        case (AnyFunType, AnyArityFunType(_))       => t1
        // At this point we know for sure that t1 /\ t2 = 0
        case (_, _) =>
          NoneType
      }

  private def overlapRecordTag(rt: RecordType, tt: TupleType): Boolean =
    tt.argTys.headOption.exists(subType(AtomLitType(rt.name), _))

  private def meetRecordTuple(rt: RefinedRecordType, tt: TupleType, seen: Set[(Type, Type)]): Type =
    util.getRecord(rt.recType.module, rt.recType.name) match {
      case Some(recDecl) =>
        if (recDecl.fields.size + 1 == tt.argTys.size) {
          val fieldsMeet = recDecl.fields.lazyZip(tt.argTys.tail).map { (field, elemT) =>
            field.name -> meetAux(rt.fields.getOrElse(field.name, field.tp), elemT, seen)
          }
          if (fieldsMeet.exists((_, t) => Subtype.isNoneType(t))) NoneType
          else {
            // keeping only the fields which are narrower than the declared ones
            val fields = fieldsMeet.filter((name, t) => !gradualSubType(recDecl.fMap(name).tp, t)).toMap
            if (fields.isEmpty) rt.recType else RefinedRecordType(rt.recType, fields)
          }
        } else NoneType
      case _ =>
        // Falling back to use the tuple type if something is wrong with resolving record declaration.
        tt
    }

}

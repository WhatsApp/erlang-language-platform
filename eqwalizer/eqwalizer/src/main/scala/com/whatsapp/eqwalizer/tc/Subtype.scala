/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.TypeVars
import com.whatsapp.eqwalizer.ast.Types.*

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

  private def kind(t: Type): Option[ValueKind] = t match {
    case AtomLitType(_) | AtomType =>
      Some(ValueKind.Atom)
    case BinaryType =>
      Some(ValueKind.Binary)
    case AnyFunType | FunType(_, _, _) | AnyArityFunType(_) =>
      Some(ValueKind.Fun)
    case NilType | ListType(_) | ConsType(_, _) =>
      Some(ValueKind.List)
    case MapType(_, _, _) =>
      Some(ValueKind.Map)
    case IntegerType =>
      Some(ValueKind.Integer)
    case FloatType =>
      Some(ValueKind.Float)
    case PidType =>
      Some(ValueKind.Pid)
    case PortType =>
      Some(ValueKind.Port)
    case ReferenceType =>
      Some(ValueKind.Reference)
    case AnyTupleType | TupleType(_) | RecordType(_) | RefinedRecordType(_, _) =>
      Some(ValueKind.Tuple)
    case NativeRecordType(_) | AnyNativeRecordType =>
      Some(ValueKind.NativeRecord)
    case _ =>
      None
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

  private def simpleOverlap(t1: Type, t2: Type): Option[Boolean] =
    (Subtype.kind(t1), Subtype.kind(t2)) match {
      case (Some(k1), Some(k2)) =>
        Some(k1 == k2)
      case (_, _) =>
        if (subType(t1, t2) || subType(t2, t1))
          Some(true)
        else
          None
    }

  def overlap(t1: Type, t2: Type): Option[Boolean] =
    overlap(t1, t2, Set.empty)

  private def overlap(t1: Type, t2: Type, seen: Set[(Type, Type)]): Option[Boolean] =
    (t1, t2) match {
      case (_, _) if t1 == t2 || seen.contains(t1, t2) || seen.contains(t2, t1) =>
        Some(true)
      case (AnyType, _) =>
        Some(true)
      case (_, AnyType) =>
        Some(true)
      case (NoneType, _) =>
        Some(false)

      case (DynamicType, _) =>
        Some(true)
      case (_, DynamicType) =>
        Some(true)

      case (BoundedDynamicType(bound), _) =>
        overlap(bound, t2, seen)
      case (_, BoundedDynamicType(bound)) =>
        overlap(t1, bound, seen)

      case (FreeVarType(_), _) =>
        Some(true)

      // Unions
      case (UnionType(ts), _) =>
        boundary {
          var allFalse = true
          for (t1 <- ts) {
            overlap(t1, t2, seen) match {
              case Some(true) =>
                boundary.break(Some(true))
              case None =>
                allFalse = false
              case Some(false) =>
                ()
            }
          }
          if (allFalse) Some(false) else None
        }
      case (_, UnionType(ts)) =>
        boundary {
          var allFalse = true
          for (t2 <- ts) {
            overlap(t1, t2, seen) match {
              case Some(true) =>
                boundary.break(Some(true))
              case None =>
                allFalse = false
              case Some(false) =>
                ()
            }
          }
          if (allFalse) Some(false) else None
        }

      case (NativeRecordType(id1), NativeRecordType(id2)) =>
        Some(id1 == id2)
      case (AtomLitType(l1), AtomLitType(l2)) =>
        Some(l1 == l2)

      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        overlap(body, t2, seen + (t1 -> t2))
      case (_, RemoteType(rid, args)) =>
        val body = util.getTypeDeclBody(rid, args)
        overlap(t1, body, seen + (t1 -> t2))

      // funs
      case (FunType(_, ins1, _), FunType(_, ins2, _)) =>
        if (ins1.size != ins2.size)
          Some(false)
        else
          None
      case (FunType(_, _, _), AnyFunType) =>
        Some(true)
      case (AnyFunType, FunType(_, _, _)) =>
        Some(true)
      case (AnyArityFunType(_), AnyFunType) =>
        Some(true)
      case (AnyFunType, AnyArityFunType(_)) =>
        Some(true)
      case (AnyArityFunType(_), FunType(_, _, _)) =>
        None
      case (FunType(_, _, _), AnyArityFunType(_)) =>
        None
      case (FunType(_, _, _), _) =>
        Some(false)
      case (_, FunType(_, _, _)) =>
        Some(false)
      case (AnyFunType, _) =>
        Some(false)
      case (_, AnyFunType) =>
        Some(false)

      // tuples and records
      case (TupleType(ts1), TupleType(ts2)) =>
        if (ts1.size != ts2.size) Some(false)
        else
          boundary {
            var allTrue = true
            for ((t1, t2) <- ts1.lazyZip(ts2)) {
              overlap(t1, t2, seen) match {
                case Some(false) =>
                  boundary.break(Some(false))
                case Some(true) =>
                  ()
                case None =>
                  allTrue = false
              }
            }
            if (allTrue) Some(true) else None
          }
      case (TupleType(_), AnyTupleType) =>
        Some(true)
      case (AnyTupleType, TupleType(_)) =>
        Some(true)
      case (RecordType(_), AnyTupleType) =>
        Some(true)
      case (RefinedRecordType(_, _), AnyTupleType) =>
        Some(true)
      case (AnyTupleType, RefinedRecordType(_, _)) =>
        Some(true)
      case (AnyTupleType, RecordType(_)) =>
        Some(true)
      case (RecordType(n1), RecordType(n2)) =>
        Some(n1 == n2)
      case (RefinedRecordType(t1, fields1), RefinedRecordType(t2, fields2)) =>
        if (t1.name != t2.name) Some(false)
        else
          boundary {
            val fNames = fields1.keySet ++ fields2.keySet
            var allTrue = true
            for (fN <- fNames)
              (fields1.get(fN), fields2.get(fN)) match {
                case (Some(f1), Some(f2)) =>
                  overlap(f1, f2, seen) match {
                    case Some(false) =>
                      boundary.break(Some(false))
                    case Some(true) =>
                      ()
                    case None =>
                      allTrue = false
                  }
                case _ =>
                  ()
              }
            if (allTrue) Some(true) else None
          }
      case (RefinedRecordType(t, _), RecordType(n)) =>
        Some(n == t.name)
      case (RecordType(n), RefinedRecordType(t, _)) =>
        Some(n == t.name)
      case (r: RecordType, TupleType(elems)) =>
        util.getRecordArity(r.module, r.name) match {
          case Some(arity) if arity + 1 == elems.size =>
            overlap(AtomLitType(r.name), elems.head, seen)
          case _ =>
            Some(false)
        }
      case (TupleType(elems), r: RecordType) =>
        util.getRecordArity(r.module, r.name) match {
          case Some(arity) if arity + 1 == elems.size =>
            overlap(elems.head, AtomLitType(r.name), seen)
          case _ =>
            Some(false)
        }
      case (RefinedRecordType(t, _), TupleType(elems)) =>
        util.getRecordArity(t.module, t.name) match {
          case Some(arity) if arity + 1 == elems.size =>
            overlap(AtomLitType(t.name), elems.head, seen)
          case _ =>
            Some(false)
        }
      case (TupleType(elems), RefinedRecordType(t, _)) =>
        util.getRecordArity(t.module, t.name) match {
          case Some(arity) if arity + 1 == elems.size =>
            overlap(elems.head, AtomLitType(t.name), seen)
          case _ =>
            Some(false)
        }
      case (TupleType(_), _) =>
        Some(false)
      case (_, TupleType(_)) =>
        Some(false)
      case (AnyTupleType, _) =>
        Some(false)
      case (_, AnyTupleType) =>
        Some(false)

      case (NilType, NilType) =>
        Some(true)
      case (NilType, ConsType(_, _)) =>
        Some(false)
      case (NilType, ListType(_)) =>
        Some(true)
      case (ConsType(_, _), NilType) =>
        Some(false)
      case (ConsType(h1, tl1), ConsType(h2, tl2)) =>
        overlap(h1, h2, seen) match {
          case Some(false) =>
            Some(false)
          case Some(true) =>
            overlap(tl1, tl2, seen)
          case None =>
            overlap(tl1, tl2, seen) match {
              case Some(false) => Some(false)
              case _           => None
            }
        }
      case (ConsType(h1, tl1), ListType(e2)) =>
        overlap(h1, e2, seen) match {
          case Some(false) =>
            Some(false)
          case Some(true) =>
            overlap(tl1, ListType(e2), seen)
          case None =>
            overlap(tl1, ListType(e2), seen) match {
              case Some(false) => Some(false)
              case _           => None
            }
        }
      case (ListType(_), NilType) =>
        Some(true)
      case (ListType(e1), ConsType(h2, tl2)) =>
        overlap(e1, h2, seen) match {
          case Some(false) =>
            Some(false)
          case Some(true) =>
            overlap(ListType(e1), tl2, seen)
          case None =>
            overlap(ListType(e1), tl2, seen) match {
              case Some(false) => Some(false)
              case _           => None
            }
        }
      case (ListType(_), ListType(_)) =>
        Some(true)
      case (ListType(_) | NilType | ConsType(_, _), _) =>
        Some(false)
      case (_, ListType(_) | NilType | ConsType(_, _)) =>
        Some(false)

      case _ =>
        simpleOverlap(t1, t2)
    }

}

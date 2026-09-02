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

import com.whatsapp.eqwalizer.ast.Forms.RecDecl
import com.whatsapp.eqwalizer.ast.RemoteId
import com.whatsapp.eqwalizer.ast.Types.*

class Narrow(pipelineContext: PipelineContext) {
  private val subtype = pipelineContext.subtype
  private val util = pipelineContext.util

  def asListType(t: Type): Option[ListType] =
    extractListElem(t) match {
      case Nil => None
      case ts  => Some(ListType(subtype.join(ts)))
    }

  private def dynamicMap(t: MapType): MapType = {
    val MapType(props, kType, vType) = t
    MapType(
      props.map { case (key, MapProp(req, tp)) =>
        (key, MapProp(req, boundedDynamic(tp)))
      },
      boundedDynamic(kType),
      boundedDynamic(vType),
    )
  }

  private def boundedDynamic(t: Type): Type = {
    t match {
      case DynamicType           => DynamicType
      case BoundedDynamicType(b) => BoundedDynamicType(b)
      case NoneType              => NoneType
      case AnyType               => DynamicType
      case _                     => BoundedDynamicType(t)
    }
  }

  def asMapTypes(t: Type): Set[MapType] =
    t match {
      case DynamicType =>
        Set(MapType(Map(), DynamicType, DynamicType))
      case BoundedDynamicType(bound) =>
        asMapTypes(bound).map(dynamicMap)
      case AnyType | FreeVarType(_) =>
        Set(MapType(Map(), AnyType, AnyType))
      case mapType: MapType =>
        Set(mapType)
      case UnionType(ts) =>
        ts.flatMap(asMapTypes)
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        asMapTypes(body)
      case _ => Set()
    }

  def asNativeRecordTypes(t: Type): (Set[NativeRecordType], Boolean) = {
    def loop(t: Type): (Set[NativeRecordType], Boolean) = t match {
      case DynamicType | AnyType | AnyNativeRecordType =>
        (Set.empty, true)
      case BoundedDynamicType(bound) =>
        val (s, _) = loop(bound)
        (s, true)
      case FreeVarType(_) =>
        (Set.empty, true)
      case nrt: NativeRecordType =>
        (Set(nrt), false)
      case UnionType(ts) =>
        ts.foldLeft((Set.empty[NativeRecordType], false)) { case ((acc, anyAcc), ty) =>
          val (s, any) = loop(ty)
          (acc ++ s, anyAcc || any)
        }
      case RemoteType(rid, args) =>
        loop(util.getTypeDeclBody(rid, args))
      case _ =>
        (Set.empty, false)
    }
    loop(t)
  }

  def asMapOrIterTypes(t: Type): Set[MapType] =
    t match {
      case DynamicType =>
        Set(MapType(Map(), DynamicType, DynamicType))
      case BoundedDynamicType(bound) =>
        asMapOrIterTypes(bound).map(dynamicMap)
      case AnyType | FreeVarType(_) =>
        Set(MapType(Map(), AnyType, AnyType))
      case mapType: MapType =>
        Set(mapType)
      case UnionType(ts) =>
        ts.flatMap(asMapOrIterTypes)
      case RemoteType(RemoteId("maps", "iterator", 0), _) =>
        Set(MapType(Map(), AnyType, AnyType))
      case RemoteType(RemoteId("maps", "iterator", 2), List(keyT, valT)) =>
        Set(MapType(Map(), keyT, valT))
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        asMapOrIterTypes(body)
      case _ => Set()
    }

  def getKVType(t: MapType): Set[TupleType] = {
    t.props.map { case (key, prop) =>
      TupleType(List(Key.asType(key), prop.tp))
    }.toSet + TupleType(List(t.kType, t.vType))
  }

  def getKeyType(t: MapType)(implicit reqOnly: Boolean = false): Type =
    t match {
      case MapType(props, _, _) if reqOnly           => subtype.join(props.filter(_._2.req).keySet.map(Key.asType))
      case MapType(props, kType, _) if props.isEmpty => kType
      case MapType(props, kType, _)                  => subtype.join(kType, UnionType(props.keySet.map(Key.asType)))
    }

  def getValType(t: MapType): Type =
    subtype.join(t.vType, t.props.values.map(_.tp))

  def getValType(key: Key, t: MapType): Type =
    t.props.get(key).map(_.tp).getOrElse {
      // key represents a literal type
      // so we can use subtyping for testing non-empty intersection
      if (subtype.subType(Key.asType(key), t.kType))
        t.vType
      else
        NoneType
    }

  def withRequiredProp(k: Key, t: MapType): Option[MapType] =
    t.props.get(k) match {
      case Some(MapProp(_, tp)) => Some(t.copy(props = t.props.updated(k, MapProp(req = true, tp))))
      case None if subtype.subType(Key.asType(k), t.kType) =>
        Some(t.copy(props = t.props.updated(k, MapProp(req = true, t.vType))))
      case _ => None
    }

  def selectKeys(reqKeyT: Type, optKeyT: Type, mapT: MapType): Type = {
    val selectProps = mapT.props.collect {
      case (key, MapProp(true, tp)) if subtype.subType(Key.asType(key), reqKeyT) => (key, MapProp(req = true, tp))
      case (key, MapProp(_, tp)) if subtype.subType(Key.asType(key), optKeyT)    => (key, MapProp(req = false, tp))
    }
    MapType(selectProps, subtype.meet(mapT.kType, optKeyT), mapT.vType)
  }

  private def extractListElem(t: Type): List[Type] =
    t match {
      case DynamicType | BoundedDynamicType(_) =>
        List(DynamicType)
      case AnyType =>
        List(AnyType)
      case UnionType(tys) =>
        tys.toList.flatMap(extractListElem)
      case NilType =>
        List(NoneType)
      case ListType(elemType) =>
        List(elemType)
      case ConsType(headT, tailT) =>
        val tailElems = extractListElem(tailT)
        headT :: tailElems
      case NoneType =>
        List(NoneType)
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        extractListElem(body)
      case FreeVarType(_) =>
        List(AnyType)
      case _ =>
        List()
    }

  def asFunTypes(ty: Type, arity: Int): Set[FunType] = ty match {
    case DynamicType =>
      Set(FunType(0, List.fill(arity)(DynamicType), DynamicType))
    case BoundedDynamicType(bound) =>
      asFunTypes(bound, arity).map(ft =>
        FunType(ft.forall, ft.argTys.map(BoundedDynamicType(_)), BoundedDynamicType(ft.resTy))
      )
    case AnyFunType =>
      Set(FunType(0, List.fill(arity)(DynamicType), DynamicType))
    case ft: FunType =>
      if (ft.argTys.size == arity) Set(ft)
      else Set()
    case AnyArityFunType(resTy) =>
      Set(FunType(0, List.fill(arity)(DynamicType), resTy))
    case UnionType(tys) =>
      tys.flatMap(asFunTypes(_, arity))
    case RemoteType(rid, args) =>
      val body = util.getTypeDeclBody(rid, args)
      asFunTypes(body, arity)
    case _ =>
      Set()
  }

  def onlyFunTypes(ty: Type, arity: Int): Set[FunType] = ty match {
    case DynamicType =>
      Set()
    case BoundedDynamicType(bound) =>
      onlyFunTypes(bound, arity).map(ft =>
        FunType(ft.forall, ft.argTys.map(BoundedDynamicType(_)), BoundedDynamicType(ft.resTy))
      )
    case AnyFunType =>
      Set(FunType(0, List.fill(arity)(DynamicType), DynamicType))
    case ft: FunType =>
      if (ft.argTys.size == arity) Set(ft)
      else Set()
    case AnyArityFunType(resTy) =>
      Set(FunType(0, List.fill(arity)(DynamicType), resTy))
    case UnionType(tys) =>
      tys.flatMap(onlyFunTypes(_, arity))
    case RemoteType(rid, args) =>
      val body = util.getTypeDeclBody(rid, args)
      onlyFunTypes(body, arity)
    case _ =>
      Set()
  }

  def asTupleType(t: Type, arity: Int): List[TupleType] =
    asTupleTypeAux(t, arity)

  private def asTupleTypeAux(t: Type, arity: Int): List[TupleType] =
    t match {
      case DynamicType =>
        List(TupleType(List.fill(arity)(DynamicType)))
      case BoundedDynamicType(bound) =>
        asTupleTypeAux(bound, arity).map(tt => TupleType(tt.argTys.map(BoundedDynamicType(_))))
      case AnyType | FreeVarType(_) =>
        List(TupleType(List.fill(arity)(AnyType)))
      case r: RecordType if arity > 0 =>
        val rec = util.getRecord(r.module, r.name)
        val recFieldTypes = rec match {
          case Some(recDecl) =>
            recDecl.fields.map(_.tp)
          case None =>
            List.fill(arity - 1)(DynamicType)
        }
        val recArity = recFieldTypes.size + 1
        if (arity == recArity) {
          List(TupleType(AtomLitType(r.name) :: recFieldTypes))
        } else
          List()
      case r: RefinedRecordType if arity > 0 =>
        val rec = util.getRecord(r.recType.module, r.recType.name)
        val recFieldTypes = rec match {
          case Some(recDecl) =>
            recDecl.fields.map(f => r.fields.getOrElse(f.name, f.tp))
          case None =>
            List.fill(arity - 1)(DynamicType)
        }
        val recArity = recFieldTypes.size + 1
        if (arity == recArity) {
          List(TupleType(AtomLitType(r.recType.name) :: recFieldTypes))
        } else
          List()
      case AnyTupleType =>
        List(TupleType(List.fill(arity)(AnyType)))
      case tt: TupleType if tt.argTys.size == arity => List(tt)
      case UnionType(tys)                           => tys.flatMap(asTupleTypeAux(_, arity)).toList
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        asTupleTypeAux(body, arity)
      case _ => List()
    }

  def filterTupleType(t: Type, elemIndex: Int, elemTy: Type): Type =
    if (elemIndex >= 1)
      filterTupleTypeAux(t, elemIndex - 1, elemTy)
    else
      NoneType

  private def filterTupleTypeAux(t: Type, elemIndex: Int, elemTy: Type): Type =
    t match {
      case DynamicType =>
        t
      case BoundedDynamicType(bound) =>
        BoundedDynamicType(filterTupleTypeAux(bound, elemIndex, elemTy))
      case AnyType | FreeVarType(_) =>
        t
      case AnyTupleType =>
        t
      case tt: TupleType if isTupleElem(tt, elemIndex, elemTy) =>
        t
      case r: RecordType =>
        recordToTuple(r) match {
          case Some(tt) if isTupleElem(tt, elemIndex, elemTy) =>
            t
          case _ =>
            NoneType
        }
      case r: RefinedRecordType =>
        refinedRecordToTuple(r) match {
          case Some(tt) if isTupleElem(tt, elemIndex, elemTy) =>
            t
          case _ =>
            NoneType
        }
      case UnionType(tys) =>
        UnionType(tys.map(filterTupleTypeAux(_, elemIndex, elemTy)))
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        filterTupleTypeAux(body, elemIndex, elemTy)
      case _ => NoneType
    }

  private def isTupleElem(tupleType: TupleType, elemIndex: Int, elemTy: Type): Boolean = {
    val TupleType(ts) = tupleType
    (elemIndex < ts.length) && subtype.subType(ts(elemIndex), elemTy)
  }

  /**
  * Given a type (required to be a subtype of `AnyTupleType`) and an index, returns the type of the tuple element at
  * the index wrapped in a `Right`. If the index can be possibly out of bounds (in at least one of the options in a
  * union) the function returns `Left(tupLen)`, where `tupLen` is the minimum index value for which this operation would
  * type check.
  */
  def getTupleElement(t: Type, idx: Int): Either[Int, Type] = t match {
    case NoneType =>
      Right(NoneType)
    case DynamicType =>
      Right(DynamicType)
    case AnyTupleType =>
      Right(AnyType)
    case BoundedDynamicType(t) if subtype.subType(t, AnyTupleType) =>
      Right(BoundedDynamicType(getTupleElement(t, idx).getOrElse(NoneType)))
    case BoundedDynamicType(t) =>
      Right(BoundedDynamicType(NoneType))
    case TupleType(elemTys) if idx >= 1 && idx <= elemTys.length =>
      Right(elemTys(idx - 1))
    case TupleType(elemTys) =>
      Left(elemTys.length)
    case r: RecordType =>
      recordToTuple(r) match {
        case Some(tupTy) => getTupleElement(tupTy, idx)
        case None        => Right(DynamicType)
      }
    case r: RefinedRecordType =>
      refinedRecordToTuple(r) match {
        case Some(tupTy) => getTupleElement(tupTy, idx)
        case None        => Right(DynamicType)
      }
    case UnionType(tys) =>
      val res = tys.map(getTupleElement(_, idx)).foldLeft[Either[Int, Set[Type]]](Right(Set.empty)) {
        case (Right(accTy), Right(elemTy)) => Right(accTy + elemTy)
        case (Left(n1), Left(n2))          => Left(n1.min(n2))
        case (Left(n1), _)                 => Left(n1)
        case (_, Left(n2))                 => Left(n2)
      }
      res.map { optionTys => UnionType(util.flattenUnions(UnionType(optionTys)).toSet) }
    case RemoteType(rid, args) =>
      val body = util.getTypeDeclBody(rid, args)
      getTupleElement(body, idx)
    case _ =>
      throw new IllegalStateException()
  }

  def setTupleElement(t: Type, idx: Int, elemT: Type): Either[Int, Type] = t match {
    case NoneType =>
      Right(NoneType)
    case DynamicType =>
      Right(DynamicType)
    case AnyTupleType =>
      Right(AnyTupleType)
    case BoundedDynamicType(t) if subtype.subType(t, AnyTupleType) =>
      Right(BoundedDynamicType(setTupleElement(t, idx, elemT).getOrElse(NoneType)))
    case BoundedDynamicType(t) =>
      Right(BoundedDynamicType(NoneType))
    case TupleType(elemTys) if idx >= 1 && idx <= elemTys.length =>
      Right(TupleType(elemTys.updated(idx - 1, elemT)))
    case TupleType(elemTys) =>
      Left(elemTys.length)
    case r: RecordType =>
      recordToTuple(r) match {
        case Some(tupTy) => setTupleElement(tupTy, idx, elemT)
        case None        => Right(DynamicType)
      }
    case r: RefinedRecordType =>
      refinedRecordToTuple(r) match {
        case Some(tupTy) => setTupleElement(tupTy, idx, elemT)
        case None        => Right(DynamicType)
      }
    case UnionType(tys) =>
      val res = tys.map(setTupleElement(_, idx, elemT)).foldLeft[Either[Int, Set[Type]]](Right(Set.empty)) {
        case (Right(accTy), Right(elemTy)) => Right(accTy + elemTy)
        case (Left(n1), Left(n2))          => Left(n1.min(n2))
        case (Left(n1), _)                 => Left(n1)
        case (_, Left(n2))                 => Left(n2)
      }
      res.map { optionTys => UnionType(util.flattenUnions(UnionType(optionTys)).toSet) }
    case RemoteType(rid, args) =>
      val body = util.getTypeDeclBody(rid, args)
      setTupleElement(body, idx, elemT)
    case _ =>
      throw new IllegalStateException()
  }

  /**
  * Given a type (required to be a subtype of `AnyTupleType`), returns the union of all its element types.
  */
  def getAllTupleElements(t: Type): Type = t match {
    case NoneType =>
      NoneType
    case DynamicType =>
      DynamicType
    case AnyTupleType =>
      AnyType
    case BoundedDynamicType(t) if subtype.subType(t, AnyTupleType) =>
      BoundedDynamicType(getAllTupleElements(t))
    case BoundedDynamicType(t) =>
      BoundedDynamicType(NoneType)
    case TupleType(elemTys) =>
      UnionType(elemTys.toSet)
    case r: RecordType =>
      recordToTuple(r) match {
        case Some(tupTy) => getAllTupleElements(tupTy)
        case None        => DynamicType
      }
    case r: RefinedRecordType =>
      refinedRecordToTuple(r) match {
        case Some(tupTy) => getAllTupleElements(tupTy)
        case None        => DynamicType
      }
    case UnionType(tys) =>
      UnionType(util.flattenUnions(UnionType(tys.map(getAllTupleElements))).toSet)
    case RemoteType(rid, args) =>
      val body = util.getTypeDeclBody(rid, args)
      getAllTupleElements(body)
    case _ =>
      throw new IllegalStateException()
  }

  private def recordToTuple(r: RecordType): Option[TupleType] =
    refinedRecordToTuple(RefinedRecordType(r, Map()))

  private def refinedRecordToTuple(r: RefinedRecordType): Option[TupleType] =
    util.getRecord(r.recType.module, r.recType.name).map { recDecl =>
      val elemTys = AtomLitType(r.recType.name) :: recDecl.fields.map(f => r.fields.getOrElse(f.name, f.tp))
      TupleType(elemTys)
    }

  def adjustMapType(mapType: MapType, keyT: Type, valT: Type): MapType =
    asKeys(keyT) match {
      case Some(keys) if keys.size == 1 =>
        MapType(mapType.props.updated(keys.head, MapProp(req = true, valT)), mapType.kType, mapType.vType)
      case Some(keys) =>
        keys.foldLeft(mapType) { case (mapType, key) =>
          val props = mapType.props.updatedWith(key) {
            case Some(prop) => Some(MapProp(prop.req, subtype.join(valT, prop.tp)))
            case None       => Some(MapProp(req = false, valT))
          }
          MapType(props, mapType.kType, mapType.vType)
        }
      case None if subtype.isDynamicType(mapType.kType) && subtype.isDynamicType(mapType.vType) =>
        mapType
      case None =>
        val props = mapType.props.map { case (key, prop) =>
          if (subtype.mayOverlap(Key.asType(key), keyT))
            (key, MapProp(prop.req, subtype.join(prop.tp, valT)))
          else
            (key, prop)
        }
        MapType(props, subtype.join(mapType.kType, keyT), subtype.join(mapType.vType, valT))
    }

  def setAllFieldsOptional(mapType: MapType, newValTy: Option[Type] = None): Type =
    MapType_*(
      mapType.props.map { case (key, MapProp(_, tp)) => (key, MapProp(req = false, newValTy.getOrElse(tp))) },
      mapType.kType,
      newValTy.getOrElse(mapType.vType),
    )

  def getRecordField(recDecl: RecDecl, recTy: Type, fieldName: String): Type = {
    val field = recDecl.fMap(fieldName)
    recTy match {
      case RefinedRecordType(recType, fields) if recDecl.name == recType.name =>
        fields.getOrElse(fieldName, field.tp)
      case RecordType(name) if recDecl.name == name =>
        field.tp
      case TupleType(argTys) if argTys.size - 1 == recDecl.fields.size && argTys.head == AtomLitType(recDecl.name) =>
        recDecl.fields.zipWithIndex
          .collectFirst { case (f, i) if f.name == fieldName => i + 1 }
          .map(argTys(_))
          .getOrElse(field.tp)
      case AnyTupleType =>
        AnyType
      case DynamicType | FreeVarType(_) =>
        field.tp
      case BoundedDynamicType(_) =>
        BoundedDynamicType(field.tp)
      case RemoteType(id, argTys) =>
        getRecordField(recDecl, util.getTypeDeclBody(id, argTys), fieldName)
      case UnionType(argTys) =>
        val fieldTys = argTys.map(getRecordField(recDecl, _, fieldName))
        UnionType(fieldTys)
      case NoneType =>
        NoneType
      case _ =>
        throw new IllegalStateException()
    }
  }

  // Recursion is sound since we don't unfold under constructors
  def asKeys(t: Type): Option[Set[Key]] =
    t match {
      case BoundedDynamicType(bound) =>
        asKeys(bound)
      case UnionType(ts) =>
        ts.foldLeft[Option[Set[Key]]](Some(Set())) { (acc, ty) =>
          acc.flatMap(keys => asKeys(ty).map(keys2 => keys ++ keys2))
        }
      case RemoteType(rid, args) =>
        val body = util.getTypeDeclBody(rid, args)
        asKeys(body)
      case NoneType => Some(Set())
      case _        => Key.fromType(t).map(Set(_))
    }

  private def mergeMaps(s1: MapType, s2: MapType, inOrder: Boolean): MapType = {
    MapType(
      (s1.props.keySet ++ s2.props.keySet).map { key =>
        val prop1 = s1.props.getOrElse(key, MapProp(req = false, NoneType))
        val prop2 = s2.props.getOrElse(key, MapProp(req = false, NoneType))
        val req = (inOrder && (prop1.req || prop2.req)) || (prop1.req && prop2.req)
        val tp = if (inOrder && prop2.req) prop2.tp else subtype.join(prop1.tp, prop2.tp)
        key -> MapProp(req, tp)
      }.toMap,
      subtype.join(s1.kType, s2.kType),
      subtype.join(s1.vType, s2.vType),
    )
  }

  def joinAndMergeMaps(tys: Iterable[Type], inOrder: Boolean = false): Type = {
    val (maps, notMaps) = tys.partition {
      case m: MapType => true
      case _          => false
    }
    val joinedNotMaps = subtype.join(notMaps)
    val mapsCoerced = maps.collect { case s: MapType => s }
    if (mapsCoerced.isEmpty) {
      joinedNotMaps
    } else {
      subtype.join(
        mapsCoerced.tail.foldLeft(mapsCoerced.head)((acc, map) => mergeMaps(acc, map, inOrder)),
        joinedNotMaps,
      )
    }
  }
}

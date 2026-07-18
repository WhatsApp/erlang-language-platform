/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Guards.*
import com.whatsapp.eqwalizer.ast.Exprs.*
import com.whatsapp.eqwalizer.ast.{Id, RemoteId, Types}
import com.whatsapp.eqwalizer.ast.Pats.*
import com.whatsapp.eqwalizer.ast.Types.*
import com.whatsapp.eqwalizer.ast.stub.Db
import com.whatsapp.eqwalizer.tc

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.boundary

object Occurrence {
  // Atomic Proposition
  type AProp = Pos | Neg
  private val nType = UnionType(Set(IntegerType, FloatType))

  private def flattenAnd(p: Prop): List[SProp | Or] = p match {
    case And(l)          => l.flatMap(flattenAnd)
    case True            => List()
    case p: (SProp | Or) => List(p)
  }

  /** Prefer these functions to And.apply and Or.apply */
  private def and(props0: List[Prop]): Prop = {
    val props = props0.flatMap(flattenAnd).distinct
    if (props.isEmpty) True
    else if (props.contains(False)) False
    else if (props.size == 1) props.head
    else And(props)
  }

  private def flattenOr(p: Prop): List[SProp | And] = p match {
    case Or(l)            => l.flatMap(flattenOr)
    case False            => List()
    case p: (SProp | And) => List(p)
  }

  private def or(props0: List[Prop]): Prop = {
    val props = props0.flatMap(flattenOr).distinct
    if (props.isEmpty) False
    else if (props.contains(True)) True
    else if (props.size == 1) props.head
    else Or(props)
  }

  private type AMap = Map[String, Obj]

  private implicit class MaybeOps(maybe: Option[Boolean]) {
    @inline
    def isTrue: Boolean = maybe.contains(true)
    @inline
    def isFalse: Boolean = maybe.contains(false)
  }

  private sealed trait Polarity
  private case object + extends Polarity
  private case object - extends Polarity
  private type Path = List[Field]

  private val unary_predicates: Map[String, Type] =
    Map(
      "is_atom" -> AtomType,
      "is_binary" -> BinaryType,
      "is_bitstring" -> BinaryType,
      "is_boolean" -> UnionType(Set(falseType, trueType)),
      "is_float" -> FloatType,
      "is_function" -> AnyFunType,
      "is_integer" -> IntegerType,
      "is_list" -> ListType(AnyType),
      "is_number" -> nType,
      "is_pid" -> PidType,
      "is_port" -> PortType,
      "is_reference" -> ReferenceType,
      "is_map" -> MapType(Map(), AnyType, AnyType),
      "is_tuple" -> AnyTupleType,
    )

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

// The main logic of occurrence typing.
final class Occurrence(pipelineContext: PipelineContext) {
  import Occurrence._
  private lazy val module = pipelineContext.module
  private lazy val subtype = pipelineContext.subtype
  private lazy val narrow = pipelineContext.narrow
  private lazy val util = pipelineContext.util
  private lazy val vars = pipelineContext.vars
  private type Name = String
  private var gen = 0
  private def genVar(): String = {
    gen += 1
    s"$$$gen"
  }

  private def isEnabled(clauses: List[Clause]): Boolean = {
    val emptyPatterns = clauses.forall(_.pats.isEmpty)
    val shortGuards = clauses.forall(clause => clause.guards.map(guardSize).sum < 32)
    val smallClauses = pipelineContext.unlimitedRefinement || (clauses.size < 7 && shortGuards)
    (module != "erl_syntax") && (emptyPatterns || smallClauses)
  }

  private def linearVars(clause: Clause): Boolean = {
    val varsL = vars.clausePatVarsL(clause)
    varsL.toSet.size == varsL.size
  }

  private def guardSize(guard: Guard): Int =
    guard.tests.map(testSize).sum

  private def testSize(test: Test): Int =
    test match {
      case TestUnOp("not", test) =>
        testSize(test)
      case TestBinOp("and" | "andalso", test1, test2) =>
        testSize(test1) + testSize(test2)
      case TestBinOp("or" | "orelse", test1, test2) =>
        testSize(test1) + testSize(test2)
      case _ =>
        1
    }

  // Basic heuristic to check coverage of a single clause in isolation
  def clauseCovered(clause: Clause, argTys: List[Type]): Boolean = {
    val hasComplexPattern = clause.pats.exists {
      case PatWild() => false
      case PatVar(_) => false
      case _         => true
    }
    if (!hasComplexPattern)
      return true
    val env = clausesEnvs(List(clause), argTys, Map()).head
    !env.exists { case (_, ty) => subtype.isNoneType(ty) }
  }

  // These are specialized methods to upgrade environments/context
  // by occurrence typing
  def ifEnvs(i: If, env: Env): List[Env] = {
    var propsAcc = List.empty[Prop]
    val clauseEnvs = ListBuffer.empty[Env]
    val accumulateNegProps = isEnabled(i.clauses)
    for (clause <- i.clauses) {
      val aMap = Map.empty[Name, Obj]
      val (testPos, testNeg) = guardsProps(clause.guards, aMap)
      val localClauseProps = testPos.toList
      val clauseProps =
        if (accumulateNegProps) combine(localClauseProps, propsAcc)
        else localClauseProps
      val clauseEnv = batchSelect(env, clauseProps, aMap)
      clauseEnvs.addOne(clauseEnv)
      if (accumulateNegProps) {
        propsAcc = propsAcc :+ or(testNeg.toList)
      }
    }
    clauseEnvs.toList
  }

  def caseEnvs(c: Case, selType: Type, env: Env): List[Env] = {
    val (env1, x) = c.expr match {
      case Var(n) =>
        (env, n)
      // important for thrift - see D31025723
      case Match(PatVar(n), _) =>
        (env, n)
      case _ =>
        val v = genVar()
        (env + (v -> selType), v)
    }
    val eMap = c.expr match {
      case Tuple(elems) =>
        elems.zipWithIndex.collect { case (Var(n), i) => n -> mkObj(x, List(TupleField(i, Some(elems.size)))) }.toMap
      case _ =>
        Map.empty[Name, Obj]
    }

    val accumulateNegProps = isEnabled(c.clauses)
    var propsAcc = List.empty[Prop]
    val clauseEnvs = ListBuffer.empty[Env]
    for (clause <- c.clauses) {
      val pat = clause.pats.head
      val (patPos, patNeg) =
        pat match {
          case PatVar(`x`) => (None, None)
          case _           => patProps(x, Nil, pat, env).unzip
        }
      val aMap = aliases(x, Nil, pat, env).toMap
      val (testPos, testNeg) = guardsProps(clause.guards, aMap)
      val localClauseProps = patPos.toList ++ testPos
      val clauseProps =
        if (accumulateNegProps) combine(localClauseProps, propsAcc)
        else localClauseProps
      val clauseEnv = batchSelect(env1, clauseProps, aMap ++ eMap)
      clauseEnvs.addOne(clauseEnv)
      if (accumulateNegProps && linearVars(clause)) {
        val clauseNeg = patNeg.toList ++ testNeg
        propsAcc = propsAcc :+ or(clauseNeg)
      }
    }
    clauseEnvs.toList
  }

  def clausesEnvs(clauses: List[Clause], argTys: List[Type], env: Env): List[Env] = {
    val accumulateNegProps = isEnabled(clauses)
    var propsAcc = List.empty[Prop]
    val clauseEnvs = ListBuffer.empty[Env]

    val vars = argTys.map(_ => genVar())
    val env1 = env ++ vars.zip(argTys).toMap

    for (clause <- clauses) {
      val pats = clause.pats
      val patsPos = ListBuffer.empty[Prop]
      val patsNeg = ListBuffer.empty[Prop]
      var aMap: AMap = Map.empty
      for ((x, pat) <- vars.zip(pats)) {
        val (patPos, patNeg) = patProps(x, Nil, pat, env).unzip
        patPos.foreach(patsPos.addOne)
        patNeg.foreach(patsNeg.addOne)
        aMap = aMap ++ aliases(x, Nil, pat, env).toMap
      }
      val (testPos, testNeg) = guardsProps(clause.guards, aMap)
      val localClauseProps = (patsPos ++ testPos).toList
      val clauseProps =
        if (accumulateNegProps) combine(localClauseProps, propsAcc)
        else localClauseProps
      val clauseEnv = batchSelect(env1, clauseProps, aMap)
      clauseEnvs.addOne(clauseEnv)
      if (accumulateNegProps && linearVars(clause)) {
        val clauseNeg = (patsNeg ++ testNeg).toList
        propsAcc = propsAcc :+ or(clauseNeg)
      }
    }
    clauseEnvs.toList
  }

  def testEnv(test: Test, env: Env, result: Boolean): Env = {
    val (testPos, testNeg) = testProps(test, Map.empty)
    val relevantProp = if (result) testPos else testNeg
    batchSelect(env, List(relevantProp), Map.empty)
  }

  private def collectAtomic(p: Prop): List[AProp] =
    p match {
      case aProp: AProp => List(aProp)
      case And(ps)      => ps.flatMap(collectAtomic)
      case _            => List()
    }

  private def implies(p: AProp, q: AProp): Boolean = (p, q) match {
    case (Pos(o1, t1), Pos(o2, t2)) if o1 == o2 => subtype.gradualSubType(t1, t2)
    case (Neg(o1, t1), Neg(o2, t2)) if o1 == o2 => subtype.gradualSubType(t2, t1)
    case (Pos(o1, t1), Neg(o2, t2)) if o1 == o2 => overlap(t1, t2).isFalse
    case _                                      => false
  }

  private def contradicts(p: AProp, q: AProp): Boolean = (p, q) match {
    case (Pos(o1, t1), Neg(o2, t2)) if o1 == o2 => subtype.gradualSubType(t1, t2)
    case (Neg(o1, t1), Pos(o2, t2)) if o1 == o2 => subtype.gradualSubType(t2, t1)
    case (Pos(o1, t1), Pos(o2, t2)) if o1 == o2 => overlap(t1, t2).isFalse
    case _                                      => false
  }

  // Combines the propositions from `props` and `acc` into a single list,
  // the propositions in `acc` are simplified wrt atomic propositions in `props`.
  private def combine(props: List[Prop], acc: List[Prop]): List[Prop] = {
    val atomicProps: List[AProp] = props.flatMap(collectAtomic)
    def isImplied(p: AProp): Boolean =
      atomicProps.exists(implies(_, p))
    def isContra(p: AProp): Boolean =
      atomicProps.exists(contradicts(_, p))
    def reduceImplied(p: Prop): Prop =
      p match {
        case Or(ps)                   => or(ps.map(reduceImplied))
        case And(ps)                  => and(ps.map(reduceImplied))
        case a: AProp if isImplied(a) => True
        case p                        => p
      }
    def reduceContras(p: Prop): Prop =
      p match {
        case Or(ps)                  => or(ps.map(reduceContras))
        case And(ps)                 => and(ps.map(reduceContras))
        case a: AProp if isContra(a) => False
        case p                       => p
      }
    val acc1 = acc.map(reduceImplied)
    val acc2 = acc1.map(reduceContras)
    if (acc2.contains(False)) List(False)
    else props ++ acc2
  }

  private def aliases(x: String, path: Path, pat: Pat, env: Env): List[(Name, Obj)] =
    pat match {
      case PatVar(v) if !env.contains(v) =>
        val obj = mkObj(x, path)
        List(v -> obj)
      case PatTuple(elems) =>
        val arity = elems.size
        elems.zipWithIndex.flatMap { case (elem, i) =>
          val pathI = path ++ List(TupleField(i, Some(arity)))
          aliases(x, pathI, elem, env)
        }
      case PatRecord(recName, fields, gen) =>
        val fieldsAliases =
          fields.flatMap(field => aliases(x, path ++ List(RecordField(field.name, recName)), field.pat, env))
        gen match {
          case None => fieldsAliases
          case Some(genPat) =>
            val recDecl = util.getRecord(module, recName)
            recDecl match {
              case None => fieldsAliases
              case Some(recDecl) =>
                val genAliases =
                  recDecl.fields
                    .filter(fDecl => !fields.exists(f => f.name == fDecl._1))
                    .flatMap(fDecl => aliases(x, path ++ List(RecordField(fDecl._1, recName)), genPat, env))
                genAliases ++ fieldsAliases
            }
        }
      case PatMatch(pat1, pat2) =>
        aliases(x, path, pat1, env) ++ aliases(x, path, pat2, env)
      case PatMap(pats) =>
        pats.flatMap { case (patKey, patR) =>
          Key.fromTest(patKey).map { key =>
            val pathI = path ++ List(MapField(key))
            aliases(x, pathI, patR, env)
          }
        }.flatten
      case PatCons(hpat, tpat) =>
        aliases(x, path ++ List(ListHead), hpat, env) ++ aliases(x, path ++ List(ListTail), tpat, env)
      case _ =>
        Nil
    }

  private def guardsProps(guards: List[Guard], aMap: Map[Name, Obj]): (Option[Prop], Option[Prop]) =
    // the same as connecting via OR
    if (guards.isEmpty) (None, None)
    else {
      val (pos, neg) = guards.map(guardProp(_, aMap)).unzip
      (Some(or(pos)), Some(and(neg)))
    }

  private def guardProp(guard: Guard, aMap: Map[Name, Obj]): (Prop, Prop) = {
    // the same as connecting via AND
    val (pos, neg) = guard.tests.map(testProps(_, aMap)).unzip
    (and(pos), or(neg))
  }

  private def testObj(test: Test, aMap: Map[Name, Obj]): Option[Obj] = {
    test match {
      case TestVar(v) =>
        Some(aMap.getOrElse(v, VarObj(v)))
      case TestRecordSelect(rec, recName, fieldName) =>
        testObj(rec, aMap).map(FieldObj(RecordField(fieldName, recName), _))
      case TestCall(Id("hd", 1), List(arg)) =>
        testObj(arg, aMap).map(FieldObj(ListHead, _))
      case TestCall(Id("element", 2), List(TestInteger(Some(index)), arg)) =>
        testObj(arg, aMap).map(FieldObj(TupleField(index, None), _))
      case _ =>
        None
    }
  }

  private def cmpTypes(test: Test): (Option[Type], Option[Type]) = {
    def unzipOpt(tys: List[Option[Type]]): Option[List[Type]] = {
      tys
        .foldLeft(Option(List.empty[Type])) {
          case (None, _) | (_, None) => None
          case (Some(l), Some(ty))   => Some(ty :: l)
        }
        .map(_.reverse)
    }
    test match {
      case TestAtom(s)     => (Some(AtomLitType(s)), Some(AtomLitType(s)))
      case TestBinaryLit() => (Some(BinaryType), None)
      case TestInteger(_)  => (Some(IntegerType), None)
      case TestFloat()     => (Some(FloatType), None)
      case TestString()    => (Some(ListType(charType)), None)
      case TestTuple(tests) =>
        val (pos, neg) = tests.map(cmpTypes).unzip
        (unzipOpt(pos).map(TupleType(_)), unzipOpt(neg).map(TupleType(_)))
      case _ => (None, None)
    }
  }

  private def cmpProps(obj: Obj, test: Test): (Prop, Prop) = {
    val (posTy, negTy) = cmpTypes(test)
    val pos = posTy.map(Pos(obj, _)).getOrElse(Unknown)
    val neg = negTy.map(Neg(obj, _)).getOrElse(Unknown)
    (pos, neg)
  }

  // Equality narrowing is symmetric in operand order: refine whichever side is a
  // tracked object against the other side's value (e.g. both `X =:= a` and `a =:= X`).
  private def eqProps(test1: Test, test2: Test, aMap: Map[Name, Obj]): (Prop, Prop) =
    testObj(test1, aMap)
      .map(cmpProps(_, test2))
      .orElse(testObj(test2, aMap).map(cmpProps(_, test1)))
      .getOrElse((Unknown, Unknown))

  private def testProps(test: Test, aMap: Map[Name, Obj]): (Prop, Prop) = {
    test match {
      case TestVar(_) =>
        // A bare variable guard succeeds iff the variable is `true`.
        testObj(test, aMap)
          .map(obj => (Pos(obj, trueType), Neg(obj, trueType)))
          .getOrElse(Unknown, Unknown)
      case TestCall(Id(pred, 1), List(arg)) if unary_predicates.isDefinedAt(pred) =>
        val tp = unary_predicates(pred)
        testObj(arg, aMap)
          .map(obj => (Pos(obj, tp), Neg(obj, tp)))
          .getOrElse(Unknown, Unknown)
      case TestCall(Id("is_function", 2), List(arg, TestInteger(Some(arity)))) =>
        val tPos = FunType(0, List.fill(arity.intValue)(AnyType), AnyType)
        val tNeg = FunType(0, List.fill(arity.intValue)(NoneType), AnyType)
        testObj(arg, aMap)
          .map(obj => (Pos(obj, tPos), Neg(obj, tNeg)))
          .getOrElse(Unknown, Unknown)
      case TestCall(Id("is_record", 3), List(arg, TestAtom(modName), TestAtom(recName))) =>
        val tp = nativeRecordTypeFor(modName, recName)
        testObj(arg, aMap)
          .map(obj => (Pos(obj, tp), Neg(obj, tp)))
          .getOrElse(Unknown, Unknown)
      case TestCall(Id("is_record", 2), List(arg, TestAtom(recName))) =>
        val tp = resolveIsRecord2Name(recName)
        testObj(arg, aMap)
          .map(obj => (Pos(obj, tp), Neg(obj, tp)))
          .getOrElse(Unknown, Unknown)
      case TestCall(Id("is_record", 3), arg :: TestAtom(recName) :: _) =>
        val tp = RecordType(recName)(module)
        testObj(arg, aMap)
          .map(obj => (Pos(obj, tp), Neg(obj, tp)))
          .getOrElse(Unknown, Unknown)
      case TestCall(Id("is_map_key", 2), List(keyArg, mapArg)) =>
        // `is_map_key(K, M)` implies M is a map
        // we don't try to be super-precise here yet when handling the key
        val pos = testObj(mapArg, aMap).map(Pos(_, MapType(Map(), AnyType, AnyType)))
        (pos.getOrElse(Unknown), Unknown)
      case TestUnOp("not", test) =>
        testProps(test, aMap).swap
      case TestBinOp("and" | "andalso", test1, test2) =>
        val (pos1, neg1) = testProps(test1, aMap)
        val (pos2, neg2) = testProps(test2, aMap)
        (and(List(pos1, pos2)), or(List(neg1, neg2)))
      case TestBinOp("or" | "orelse", test1, test2) =>
        val (pos1, neg1) = testProps(test1, aMap)
        val (pos2, neg2) = testProps(test2, aMap)
        (or(List(pos1, pos2)), and(List(neg1, neg2)))
      case TestBinOp("==" | "=:=", test1, test2) =>
        eqProps(test1, test2, aMap)
      case TestBinOp("=/=" | "/=", test1, test2) =>
        eqProps(test1, test2, aMap).swap
      case _ =>
        (Unknown, Unknown)
    }
  }

  private def patProps(x: String, path: Path, pat: Pat, env: Env): Option[(Prop, Prop)] = {
    pat match {
      case PatWild() =>
        None
      case PatVar(v) =>
        env.get(v) map { _ => (Unknown, Unknown) }
      case PatAtom(s) =>
        val obj = mkObj(x, path)
        val pos = Pos(obj, AtomLitType(s))
        val neg = Neg(obj, AtomLitType(s))
        Some(pos, neg)
      case PatFloat() =>
        val obj = mkObj(x, path)
        val pos = Pos(obj, FloatType)
        Some(pos, Unknown)
      case PatInt() =>
        val obj = mkObj(x, path)
        val pos = Pos(obj, IntegerType)
        Some(pos, Unknown)
      case PatTuple(elems) =>
        val arity = elems.size
        val obj = mkObj(x, path)
        val posThis = Pos(obj, TupleType(List.fill(arity)(AnyType)))
        val negThis = Neg(obj, TupleType(List.fill(arity)(AnyType)))
        val (posThat, negThat) = elems.zipWithIndex.flatMap { case (elem, i) =>
          patProps(x, path :+ TupleField(i, Some(arity)), elem, env)
        }.unzip
        val pos = and(posThis :: posThat)
        val neg = or(List(negThis, and(List(posThis, or(negThat)))))
        Some(pos, neg)
      case PatRecord(recName, fields, gen) =>
        val obj = mkObj(x, path)
        val posThis = Pos(obj, RecordType(recName)(module))
        val negThis = Neg(obj, RecordType(recName)(module))
        val (posNamed, negNamed) =
          fields.flatMap(field => patProps(x, path :+ RecordField(field.name, recName), field.pat, env)).unzip
        val (posThat, negThat) = gen match {
          case None => (posNamed, negNamed)
          case Some(genPat) =>
            val recDecl = util.getRecord(module, recName)
            recDecl match {
              case None => (posNamed, negNamed)
              case Some(recDecl) =>
                val (posGen, negGen) =
                  recDecl.fields
                    .filter(fDecl => !fields.exists(f => f.name == fDecl._1))
                    .flatMap(fDecl => patProps(x, path :+ RecordField(fDecl._1, recName), genPat, env))
                    .unzip
                (posNamed ++ posGen, negNamed ++ negGen)
            }
        }
        val pos = and(posThis :: posThat)
        val neg = or(List(negThis, and(List(posThis, or(negThat)))))
        Some(pos, neg)
      case PatMatch(PatVar(alias), pat1) =>
        env.get(alias) match {
          case Some(_) =>
            Some(Unknown, Unknown)
          case None =>
            patProps(x, path, pat1, env)
        }
      case PatMatch(pat1, PatVar(alias)) =>
        env.get(alias) match {
          case Some(_) =>
            Some(Unknown, Unknown)
          case None =>
            patProps(x, path, pat1, env)
        }
      case PatMap(pats) =>
        boundary {
          val obj = mkObj(x, path)
          val posThis = Pos(obj, MapType(Map(), AnyType, AnyType))
          val negThis = Neg(obj, MapType(Map(), AnyType, AnyType))
          val fields = pats.map { case (patK, patV) =>
            Key.fromTest(patK) match {
              case Some(key) => (key, patV)
              case None      => boundary.break(Some(posThis, Unknown))
            }
          }
          val (posThat, negThat) = fields.map { case (field, pat) =>
            val objField = mkObj(x, path :+ MapField(field))
            patProps(x, path :+ MapField(field), pat, env).getOrElse((Pos(objField, AnyType), Neg(objField, AnyType)))
          }.unzip
          val pos = and(posThis :: posThat)
          val neg = or(List(negThis, and(List(posThis, or(negThat)))))
          Some(pos, neg)
        }
      case PatNil() =>
        val obj = mkObj(x, path)
        val pos = Pos(obj, NilType)
        val neg = Neg(obj, NilType)
        Some(pos, neg)
      case PatCons(hpat, tpat) =>
        val obj = mkObj(x, path)
        val posThis = and(List(Pos(obj, ListType(AnyType)), Neg(obj, NilType)))
        val negThis = or(List(Neg(obj, ListType(AnyType)), Pos(obj, NilType)))
        val (posThat, negThat) =
          List(patProps(x, path :+ ListHead, hpat, env), patProps(x, path :+ ListTail, tpat, env)).flatten.unzip
        val pos = and(posThis :: posThat)
        val neg = or(List(negThis, and(List(posThis, or(negThat)))))
        Some(pos, neg)
      case PatBinary(_) =>
        val obj = mkObj(x, path)
        val pos = Pos(obj, BinaryType)
        val neg = Unknown
        Some(pos, neg)
      case _ =>
        Some(Unknown, Unknown)
    }
  }

  private def simpleOverlap(t1: Type, t2: Type): Option[Boolean] =
    (kind(t1), kind(t2)) match {
      case (Some(k1), Some(k2)) =>
        Some(k1 == k2)
      case (_, _) =>
        if (subtype.subType(t1, t2) || subtype.subType(t2, t1))
          Some(true)
        else
          None
    }

  private def overlap(t1: Type, t2: Type): Option[Boolean] =
    (t1, t2) match {
      case (_, _) if t1 == t2 =>
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
        overlap(bound, t2)
      case (_, BoundedDynamicType(bound)) =>
        overlap(t1, bound)

      case (FreeVarType(_), _) =>
        Some(true)

      // Unions
      case (UnionType(ts), _) =>
        boundary {
          var allFalse = true
          for (t1 <- ts) {
            overlap(t1, t2) match {
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
            overlap(t1, t2) match {
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
        overlap(body, t2)
      // t2 is generated from "predicates" - they are always without aliases
      case (_, RemoteType(_, _)) =>
        throw new IllegalStateException(t2.toString)

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
              overlap(t1, t2) match {
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
      case (AnyTupleType, RecordType(_)) =>
        Some(true)
      case (RecordType(n1), RecordType(n2)) =>
        Some(n1 == n2)
      case (RefinedRecordType(t, _), RecordType(n)) =>
        Some(n == t.name)
      case (r: RecordType, TupleType(elems)) =>
        util.getRecordArity(r.module, r.name) match {
          case Some(arity) if arity + 1 == elems.size =>
            overlap(AtomLitType(r.name), elems.head)
          case _ =>
            Some(false)
        }
      case (TupleType(elems), r: RecordType) =>
        util.getRecordArity(r.module, r.name) match {
          case Some(arity) if arity + 1 == elems.size =>
            overlap(elems.head, AtomLitType(r.name))
          case _ =>
            Some(false)
        }
      case (RefinedRecordType(t, _), TupleType(elems)) =>
        util.getRecordArity(t.module, t.name) match {
          case Some(arity) if arity + 1 == elems.size =>
            overlap(AtomLitType(t.name), elems.head)
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

      case (_, RefinedRecordType(_, _)) =>
        // t2 comes from props
        throw new IllegalStateException(t2.toString)

      case (NilType, NilType) =>
        Some(true)
      case (NilType, ConsType(_, _)) =>
        Some(false)
      case (NilType, ListType(_)) =>
        Some(true)
      case (ConsType(_, _), NilType) =>
        Some(false)
      case (ConsType(h1, tl1), ConsType(h2, tl2)) =>
        overlap(h1, h2) match {
          case Some(false) =>
            Some(false)
          case Some(true) =>
            overlap(tl1, tl2)
          case None =>
            overlap(tl1, tl2) match {
              case Some(false) => Some(false)
              case _           => None
            }
        }
      case (ConsType(h1, tl1), ListType(e2)) =>
        overlap(h1, e2) match {
          case Some(false) =>
            Some(false)
          case Some(true) =>
            overlap(tl1, ListType(e2))
          case None =>
            overlap(tl1, ListType(e2)) match {
              case Some(false) => Some(false)
              case _           => None
            }
        }
      case (ListType(_), NilType) =>
        Some(true)
      case (ListType(e1), ConsType(h2, tl2)) =>
        overlap(e1, h2) match {
          case Some(false) =>
            Some(false)
          case Some(true) =>
            overlap(ListType(e1), tl2)
          case None =>
            overlap(ListType(e1), tl2) match {
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

  private def restrict(t1: Type, t2: Type): Type = {
    (t1, t2) match {
      case (t, s) if overlap(t, s).isFalse =>
        NoneType
      case (t, s) if subtype.gradualSubType(t, s) =>
        t
      case (t, s) if subtype.gradualSubType(s, t) =>
        s
      case (UnionType(ts), s) =>
        UnionType(ts.map(restrict(_, s)))
      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        val restricted = restrict(body, t2)
        if (restricted == body) t1 else restricted
      case (BoundedDynamicType(t), s) =>
        BoundedDynamicType(restrict(t, s))
      case (DynamicType, s) =>
        BoundedDynamicType(s)
      case (ConsType(_, _), NilType) =>
        NoneType
      case (NilType, ConsType(_, _)) =>
        NoneType
      case (ListType(e), ConsType(h, tl)) =>
        ConsType_*(restrict(e, h), restrict(ListType(e), tl))
      case (ConsType(h, tl), ListType(e)) =>
        ConsType_*(restrict(h, e), restrict(tl, ListType(e)))
      case (ConsType(h1, tl1), ConsType(h2, tl2)) =>
        ConsType_*(restrict(h1, h2), restrict(tl1, tl2))
      case (_, _) =>
        t1
    }
  }

  def remove(t1: Type, t2: Type): Type =
    (t1, t2) match {
      case (t, s) if subtype.gradualSubType(t, s) =>
        NoneType
      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        val removed = remove(body, t2)
        if (removed == body) t1 else removed
      case (ListType(e), _) =>
        val body = UnionType(Set(NilType, ConsType(e, ListType(e))))
        val removed = remove(body, t2)
        if (removed == body) t1 else removed
      case (UnionType(ts), s) =>
        UnionType(ts.map(remove(_, s)))
      case (BoundedDynamicType(t), s) =>
        BoundedDynamicType(remove(t, s))
      case (t, _) =>
        t
    }

  private def ConsType_*(headT: Type, tailT: Type): Type =
    if (subtype.isNoneType(headT) || subtype.isNoneType(tailT)) NoneType
    else ConsType(headT, tailT)

  private def TupleType_*(elems: List[Type]): Type =
    if (elems.exists(subtype.isNoneType))
      NoneType
    else
      TupleType(elems)

  private def MapType_*(props: Map[Types.Key, Types.MapProp], kTy: Type, vTy: Type): Type = {
    val hasPropEmpty = props.values.exists { case MapProp(req, tp) => req && subtype.isNoneType(tp) }
    val propsNonEmpty = props.filter { case (_, MapProp(req, tp)) => req || !subtype.isNoneType(tp) }
    if (hasPropEmpty)
      NoneType
    else
      MapType(propsNonEmpty, kTy, vTy)
  }

  private def refineRecord(t: Type, field: String, refined: Type): Type = {
    if (subtype.isNoneType(refined)) {
      NoneType
    } else {
      t match {
        case rt: RefinedRecordType =>
          RefinedRecordType(rt.recType, rt.fields.updated(field, refined))
        case rt: RecordType =>
          RefinedRecordType(rt, Map(field -> refined))
        case _ => t
      }
    }
  }

  private def update(t: Type, path: Path, pol: Polarity, s: Type): Type =
    (t, path) match {
      case (_, Nil) =>
        pol match {
          case + => restrict(t, s)
          case - => remove(t, s)
        }
      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        update(body, path, pol, s)
      case (UnionType(ts), _) =>
        UnionType(ts.map(update(_, path, pol, s)))
      case (BoundedDynamicType(t), _) =>
        BoundedDynamicType(update(t, path, pol, s))
      case (TupleType(ts), TupleField(pos, Some(arity)) :: path) if ts.size == arity =>
        val t = ts(pos)
        val t1 = update(t, path, pol, s)
        TupleType_*(ts.updated(pos, t1))
      case (rt: RecordType, RecordField(fieldName, recName) :: path) if rt.name == recName =>
        util.getRecord(rt.module, rt.name) match {
          case Some(recDecl) =>
            val t = recDecl.fMap(fieldName).tp
            val t1 = update(t, path, pol, s)
            refineRecord(rt, fieldName, t1)
          case _ => rt
        }
      case (rt: RecordType, TupleField(_, Some(arity)) :: _) =>
        util.getRecord(rt.module, rt.name) match {
          case Some(recDecl) if recDecl.fields.size + 1 == arity =>
            val rTy = narrow.asTupleType(rt, arity).head
            update(rTy, path, pol, s)
          case _ => rt
        }
      case (rt: RefinedRecordType, RecordField(fieldName, recName) :: path) if rt.recType.name == recName =>
        if (rt.fields.contains(fieldName)) {
          val t = rt.fields(fieldName)
          val t1 = update(t, path, pol, s)
          refineRecord(rt, fieldName, t1)
        } else {
          util.getRecord(rt.recType.module, rt.recType.name) match {
            case Some(recDecl) =>
              val t = recDecl.fMap(fieldName).tp
              val t1 = update(t, path, pol, s)
              refineRecord(rt, fieldName, t1)
            case None => rt
          }
        }
      case (rt: RefinedRecordType, TupleField(_, Some(arity)) :: _) =>
        util.getRecord(rt.recType.module, rt.recType.name) match {
          case Some(recDecl) if recDecl.fields.size + 1 == arity =>
            val rTy = narrow.asTupleType(rt, arity).head
            update(rTy, path, pol, s)
          case _ => rt
        }
      case (MapType(props, kTy, vTy), MapField(field) :: path) =>
        if (props.contains(field) || (subtype.subType(Key.asType(field), kTy) && pol == +)) {
          val refinedProps = props.updatedWith(field) {
            case Some(MapProp(req, tp)) => Some(MapProp((pol == +) || req, update(tp, path, pol, s)))
            case None                   => Some(MapProp(req = true, update(vTy, path, pol, s)))
          }
          MapType_*(refinedProps, kTy, vTy)
        } else {
          pol match {
            case + => NoneType
            case - => t
          }
        }
      case (ListType(lt), ListHead :: path) =>
        if (subtype.isNoneType(update(lt, path, pol, s)))
          NoneType
        else
          ListType(lt)
      case (ListType(lt), ListTail :: path) =>
        if (subtype.isNoneType(update(ListType(lt), path, pol, s)))
          NoneType
        else
          ListType(lt)
      case (ConsType(h, tl), ListHead :: path) =>
        val h1 = update(h, path, pol, s)
        ConsType_*(h1, tl)
      case (ConsType(h, tl), ListTail :: path) =>
        val tl1 = update(tl, path, pol, s)
        ConsType_*(h, tl1)
      case (TupleType(ts), TupleField(index, None) :: path) if index >= 1 && index <= ts.size =>
        TupleType_*(ts.updated(index - 1, update(ts(index - 1), path, pol, s)))
      case (_, TupleField(_, None) :: path) if pol == + =>
        update(t, path, pol, AnyTupleType)
      case (_, _) =>
        t
    }

  private def batchSelect(typeEnv: Env, props: List[Prop], aMap: AMap): Env = {
    val refinedEnvs = applyProps(props, List(typeEnv))
    var result: Env = Map.empty
    val names = typeEnv.keySet ++ aMap.keySet
    for (name <- names) {
      val ts = aMap.get(name) match {
        case Some(obj) =>
          val id = objId(obj)
          val path = objPath(obj)
          refinedEnvs.map(_(id)).map(typePathRef(_, path))
        case None =>
          refinedEnvs.map(_(name))
      }
      val t = ts match {
        case List(t1) => t1
        case _        => subtype.join(ts)
      }
      result += name -> t
    }
    result
  }

  private def envSubtype(env1: Env, env2: Env): Boolean =
    env1.forall { case (k, t1) => env2.get(k).exists(subtype.gradualSubType(t1, _)) }

  /** Removes redundant environments from a list
    * by keeping only the less precise ones for subtyping */
  private def keepBestEnvs(envs: List[Env]): List[Env] = {
    var acc: List[Env] = List()
    envs.foreach { env =>
      if (!acc.exists(envSubtype(env, _))) {
        acc = env :: acc.filter(!envSubtype(_, env))
      }
    }
    acc
  }

  private def applyProps(props: List[Prop], envs: List[Env]): List[Env] =
    props match {
      case Nil =>
        envs
      case False :: _ =>
        List()
      case True :: props =>
        applyProps(props, envs)
      case Unknown :: props =>
        applyProps(props, envs)
      case Pos(x, t) :: props =>
        applyProps(props, keepBestEnvs(envs.flatMap(updateTypeEnv(_, +, x, t))))
      case Neg(x, t) :: props =>
        applyProps(props, keepBestEnvs(envs.flatMap(updateTypeEnv(_, -, x, t))))
      case And(ps) :: props =>
        applyProps(ps ++ props, envs)
      case Or(ps) :: props =>
        val envs2 = applyProps(props, envs)
        keepBestEnvs(ps.flatMap((p: Prop) => applyProps(List(p), envs2)))
    }

  private def updateTypeEnv(typeEnv: Env, pol: Polarity, obj: Obj, t: Type): Option[Env] = {
    val x = objId(obj)
    typeEnv.get(x) match {
      case None =>
        Some(typeEnv)
      case Some(old) =>
        val s = update(old, objPath(obj), pol, t)
        if (subtype.isNoneType(s)) None else Some(typeEnv.updated(x, s))
    }
  }

  @tailrec
  private def objId(obj: Obj): String =
    obj match {
      case VarObj(v)      => v
      case FieldObj(_, o) => objId(o)
    }

  private def objPath(obj: Obj): Path =
    obj match {
      case VarObj(_) =>
        List.empty
      case FieldObj(field, obj) =>
        field :: objPath(obj)
    }

  private def typePathRef(t: Type, path: Path): Type =
    (t, path) match {
      case (NoneType, _) =>
        NoneType
      case (s, Nil) =>
        s
      case (DynamicType, TupleField(_, _) :: _) =>
        DynamicType
      case (DynamicType, RecordField(fieldName, recName) :: path1) =>
        util
          .getRecord(module, recName)
          .map(_.fMap(fieldName).tp)
          .map(typePathRef(_, path1))
          .getOrElse(DynamicType)
      case (BoundedDynamicType(bound), _) =>
        BoundedDynamicType(typePathRef(bound, path))
      case (UnionType(ts), _) =>
        UnionType(ts.map(typePathRef(_, path)))
      case (TupleType(ts), TupleField(index, Some(arity)) :: path1) if ts.size == arity =>
        typePathRef(ts(index), path1)
      case (rTy: RecordType, RecordField(fieldName, recName) :: path1) if rTy.name == recName =>
        util
          .getRecord(rTy.module, rTy.name)
          .map(_.fMap(fieldName).tp)
          .map(typePathRef(_, path1))
          .getOrElse(AnyType)
      case (rTy: RecordType, TupleField(index, Some(arity)) :: path1) =>
        util.getRecord(rTy.module, rTy.name) match {
          case Some(recDecl) if recDecl.fields.size + 1 == arity =>
            val tuple = narrow.asTupleType(rTy, arity).head
            typePathRef(tuple.argTys(index), path1)
          case _ => AnyType
        }
      case (rTy: RefinedRecordType, RecordField(fieldName, recName) :: path1) if rTy.recType.name == recName =>
        if (rTy.fields.contains(fieldName)) {
          typePathRef(rTy.fields(fieldName), path1)
        } else {
          util
            .getRecord(rTy.recType.module, rTy.recType.name)
            .map(_.fMap(fieldName).tp)
            .map(typePathRef(_, path1))
            .getOrElse(AnyType)
        }
      case (rTy: RefinedRecordType, TupleField(index, Some(arity)) :: path1) =>
        util.getRecord(rTy.recType.module, rTy.recType.name) match {
          case Some(recDecl) if recDecl.fields.size + 1 == arity =>
            val tuple = narrow.asTupleType(rTy, arity).head
            typePathRef(tuple.argTys(index), path1)
          case _ => AnyType
        }
      case (MapType(props, _, vTy), MapField(field) :: path1) =>
        val ty = props
          .get(field)
          .map(_.tp)
          .getOrElse(vTy)
        typePathRef(ty, path1)
      case (RemoteType(rid, args), path) =>
        val body = util.getTypeDeclBody(rid, args)
        typePathRef(body, path)
      case (ListType(lt), ListHead :: path) =>
        typePathRef(lt, path)
      case (ListType(lt), ListTail :: path) =>
        typePathRef(ListType(lt), path)
      case (ConsType(h, _), ListHead :: path) =>
        typePathRef(h, path)
      case (ConsType(_, tl), ListTail :: path) =>
        typePathRef(tl, path)
      case _ =>
        AnyType
    }

  private def mkObj(v: String, path: Path): Obj =
    path match {
      case Nil =>
        VarObj(v)
      case field :: path =>
        FieldObj(field, mkObj(v, path))
    }

  private def nativeRecordTypeFor(modName: String, recName: String): Type =
    NativeRecordType(RemoteId(modName, recName, 0))

  private def resolveIsRecord2Name(recName: String): Type = {
    val localNative = Db.getNativeRecord(module, recName).map(_ => module)
    val importedNative = Db.getNativeRecordImports(module).flatMap(_.get(recName))
    localNative.orElse(importedNative) match {
      case Some(definingModule) => nativeRecordTypeFor(definingModule, recName)
      case None                 => RecordType(recName)(module)
    }
  }
}

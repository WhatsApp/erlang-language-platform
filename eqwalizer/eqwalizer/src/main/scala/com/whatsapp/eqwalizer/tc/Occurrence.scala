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

import com.whatsapp.eqwalizer.ast.Guards.*
import com.whatsapp.eqwalizer.ast.Exprs.*
import com.whatsapp.eqwalizer.ast.{Id, RemoteId, Types}
import com.whatsapp.eqwalizer.ast.Pats.*
import com.whatsapp.eqwalizer.ast.Types.*
import com.whatsapp.eqwalizer.ast.stub.Db
import com.whatsapp.eqwalizer.tc

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Occurrence {
  // Atomic Proposition
  type AProp = Pos | Neg

  // Semantics of a guard test. At runtime there are four outcomes: it
  // evaluates to `true`, to `false`, to another (non-boolean) value, or it
  // throws. TP partitions them into three cells - `false` and "other value"
  // are merged into ff, since a guard treats both as "did not pass":
  //   tt   - holds when the test evaluates to `true`
  //   ff   - holds when the test evaluates, without throwing, to anything
  //          other than `true` (`false` or a non-boolean value)
  //   ev   - holds whenever the test evaluates without throwing, whatever
  //          the result: `X + Y > 0` having evaluated implies X, Y :: number()
  //   th   - holds when evaluation throws: the test's throw-domain
  //          (`hd(L)` threw implies L is [] or not a list). False means the
  //          test is total; Unknown means the throw-cause is inexpressible.
  //   resT - the result type of the test in EVERY env. Currently, we exploit
  //          boolean resT for handling connections.
  // Invariant: tt and ff each conjoin ev.
  private case class TP(tt: Prop, ff: Prop, ev: Prop, th: Prop, resT: Option[Type]) {
    // Knowledge available when a clause was skipped over this test: it
    // evaluated to non-`true` or it threw. An inexpressible throw-domain
    // (th = Unknown) absorbs the disjunction - no negative information.
    def notTaken: Prop = or(List(ff, th))
  }

  private def flattenAnd(p: Prop): List[SProp | Or] = p match {
    case And(l)          => l.flatMap(flattenAnd)
    case True | Unknown  => List()
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
    // an uninformative disjunct makes the whole disjunction uninformative:
    // resolution keeps the unrefined env alive and joins back to it
    else if (props.contains(Unknown)) Unknown
    else if (props.size == 1) props.head
    else Or(props)
  }

  private type AMap = Map[String, Obj]

  private sealed trait Polarity
  private case object + extends Polarity
  private case object - extends Polarity
  private type Path = List[Field]

  private val unary_predicates: Map[String, Type] =
    Map(
      "is_atom" -> AtomType,
      "is_binary" -> BinaryType,
      "is_bitstring" -> BinaryType,
      "is_boolean" -> booleanType,
      "is_float" -> FloatType,
      "is_function" -> AnyFunType,
      "is_integer" -> IntegerType,
      "is_list" -> ListType(AnyType),
      "is_number" -> numberType,
      "is_pid" -> PidType,
      "is_port" -> PortType,
      "is_reference" -> ReferenceType,
      "is_map" -> MapType(Map(), AnyType, AnyType),
      "is_tuple" -> AnyTupleType,
      "is_record" -> AnyNativeRecordType,
    )
}

// The main logic of occurrence typing.
final class Occurrence(pipelineContext: PipelineContext) {
  import Occurrence._
  private lazy val module = pipelineContext.module
  private lazy val subtype = pipelineContext.subtype
  private lazy val narrow = pipelineContext.narrow
  private lazy val util = pipelineContext.util
  private lazy val vars = pipelineContext.vars
  private lazy val typeInfo = pipelineContext.typeInfo
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
    !env.exists { case (_, ty) => Subtype.isNoneType(ty) }
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
      val info = patProps(x, Nil, pat, env)
      val (patPos, patNeg) =
        pat match {
          case PatVar(`x`) => (None, None)
          case _           => info.props.unzip
        }
      val aMap = info.aliases.toMap
      val (testPos, testNeg) = guardsProps(clause.guards, aMap)
      val localClauseProps = patPos.toList ++ testPos
      val clauseProps =
        if (accumulateNegProps) combine(localClauseProps, propsAcc)
        else localClauseProps
      val clauseEnv = batchSelect(bindSeeds(env1, info.seeds), clauseProps, aMap ++ eMap)
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
      val seeds = ListBuffer.empty[(Name, Obj)]
      for ((x, pat) <- vars.zip(pats)) {
        val info = patProps(x, Nil, pat, env)
        val (patPos, patNeg) = info.props.unzip
        patPos.foreach(patsPos.addOne)
        patNeg.foreach(patsNeg.addOne)
        aMap = aMap ++ info.aliases
        seeds.addAll(info.seeds)
      }
      val (testPos, testNeg) = guardsProps(clause.guards, aMap)
      val localClauseProps = (patsPos ++ testPos).toList
      val clauseProps =
        if (accumulateNegProps) combine(localClauseProps, propsAcc)
        else localClauseProps
      val clauseEnv = batchSelect(bindSeeds(env1, seeds.toList), clauseProps, aMap)
      clauseEnvs.addOne(clauseEnv)
      if (accumulateNegProps && linearVars(clause)) {
        val clauseNeg = (patsNeg ++ testNeg).toList
        propsAcc = propsAcc :+ or(clauseNeg)
      }
    }
    clauseEnvs.toList
  }

  // Refine an env knowing that `test` was evaluated - in expression position,
  // so a throw would have propagated - and produced `result`. `result = false`
  // therefore implies the test evaluated to boolean `false` (stronger than a
  // guard clause being skipped, which could also mean the test threw).
  def testEnv(test: Test, env: Env, result: Boolean): Env = {
    val tp = testProps(test, Map.empty)
    val relevantProp = if (result) tp.tt else bff(test, tp, Map.empty)
    batchSelect(env, List(relevantProp), Map.empty)
  }

  // Refine an env knowing that `test` was evaluated - in a position where a
  // throw would have propagated - to a value of type `ty` (e.g. a non-literal
  // map-pattern key evaluates to a key of the map's key type).
  def valueEnv(test: Test, ty: Type, env: Env): Env =
    batchSelect(env, List(evIn(test, ty, Map.empty)), Map.empty)

  // Refine an env by a clause's guards, refining variables by name (empty aMap).
  // Called both before elabPats (pattern vars are pre-bound by enterScope, so
  // guard refinements feed pattern elaboration) and after (so leaves keep their
  // post-pattern types); this is the occurrence-based replacement for ElabGuard
  // and covers leaves that dispatch-time env refinement cannot reach (e.g.
  // values bound under a non-literal map key).
  def refineGuards(guards: List[Guard], env: Env): Env =
    if (guards.isEmpty) env
    else batchSelect(env, guardsProps(guards, Map.empty)._1.toList, Map.empty)

  def annotateGuards(guards: List[Guard], env: Env): Unit =
    guards.foreach(annotateGuard(_, env))

  private def annotateGuard(guard: Guard, env: Env): Unit =
    guard.tests.foreach(annotateTest(_, env))

  private def annotateTest(test: Test, env: Env): Unit =
    test match {
      case TestVar(v) =>
        typeInfo.add(test.pos, env(v))
      case TestTuple(elems) =>
        elems.foreach(annotateTest(_, env))
      case TestCons(h, t) =>
        annotateTest(h, env)
        annotateTest(t, env)
      case TestCall(id, args) =>
        args.foreach(annotateTest(_, env))
      case TestRecordCreate(recName, fields) =>
        fields.foreach(f => annotateTest(f.value, env))
      case TestRecordSelect(rec, recName, fieldName) =>
        annotateTest(rec, env)
      case TestNativeRecordSelect(rec, name, fieldName) =>
        annotateTest(rec, env)
      case TestMapCreate(kvs) =>
        kvs.foreach { kv =>
          annotateTest(kv._1, env)
          annotateTest(kv._2, env)
        }
      case TestMapUpdate(map, kvs) =>
        annotateTest(map, env)
        kvs.foreach { kv =>
          annotateTest(kv._1, env)
          annotateTest(kv._2, env)
        }
      case TestUnOp(op, arg) =>
        annotateTest(arg, env)
      case TestBinOp(op, arg1, arg2) =>
        annotateTest(arg1, env)
        annotateTest(arg2, env)
      case TestNil() | TestString() | TestRecordIndex(_, _) | TestAtom(_) | TestInteger(_) | TestFloat() |
          TestBinaryLit() =>
        ()
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
    case (Pos(o1, t1), Neg(o2, t2)) if o1 == o2 => !subtype.mayOverlap(t1, t2)
    case _                                      => false
  }

  private def contradicts(p: AProp, q: AProp): Boolean = (p, q) match {
    case (Pos(o1, t1), Neg(o2, t2)) if o1 == o2 => subtype.gradualSubType(t1, t2)
    case (Neg(o1, t1), Pos(o2, t2)) if o1 == o2 => subtype.gradualSubType(t2, t1)
    case (Pos(o1, t1), Pos(o2, t2)) if o1 == o2 => !subtype.mayOverlap(t1, t2)
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

  private def guardsProps(guards: List[Guard], aMap: Map[Name, Obj]): (Option[Prop], Option[Prop]) =
    // the same as connecting via OR
    if (guards.isEmpty) (None, None)
    else {
      val (pos, neg) = guards.map(guardProp(_, aMap)).unzip
      (Some(or(pos)), Some(and(neg)))
    }

  private def guardProp(guard: Guard, aMap: Map[Name, Obj]): (Prop, Prop) = {
    // tests connect via AND; the guard is not taken when some test is not taken
    val tps = guard.tests.map(testProps(_, aMap))
    (and(tps.map(_.tt)), or(tps.map(_.notTaken)))
  }

  private def testObj(test: Test, aMap: Map[Name, Obj]): Option[Obj] = {
    test match {
      case TestVar(v) =>
        Some(aMap.getOrElse(v, VarObj(v)))
      case TestRecordSelect(rec, recName, fieldName) =>
        testObj(rec, aMap).map(FieldObj(RecordField(fieldName, recName), _))
      case TestCall(Id("hd", 1), List(arg)) =>
        testObj(arg, aMap).map(FieldObj(ListHead, _))
      case TestCall(Id("tl", 1), List(arg)) =>
        testObj(arg, aMap).map(FieldObj(ListTail, _))
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
      case TestAtom(s) =>
        (Some(AtomLitType(s)), Some(AtomLitType(s)))
      case TestBinaryLit() =>
        (Some(BinaryType), None)
      case TestInteger(_) =>
        (Some(IntegerType), None)
      case TestFloat() =>
        (Some(FloatType), None)
      case TestString() =>
        (Some(ListType(charType)), None)
      case TestUnOp("-" | "+", TestInteger(_)) =>
        (Some(IntegerType), None)
      case TestUnOp("-" | "+", TestFloat()) =>
        (Some(FloatType), None)
      case TestTuple(tests) =>
        val (pos, neg) = tests.map(cmpTypes).unzip
        (unzipOpt(pos).map(TupleType(_)), unzipOpt(neg).map(TupleType(_)))
      case _ =>
        (None, None)
    }
  }

  private def cmpProps(obj: Obj, test: Test, aMap: AMap): (Prop, Prop) = {
    test match {
      case rc: TestRecordCreate =>
        (Pos(obj, RecordType(rc.recName)(module)), Unknown)
      case _ =>
        val (posTy, negTy) = cmpTypes(test)
        val pos = posTy.map(Pos(obj, _)).getOrElse(Unknown)
        val neg = negTy.map(Neg(obj, _)).getOrElse(Unknown)
        (pos, neg)
    }
  }

  // Equality narrowing is symmetric in operand order: refine whichever side is a
  // tracked object against the other side's value (e.g. both `X =:= a` and `a =:= X`).
  private def eqProps(test1: Test, test2: Test, aMap: Map[Name, Obj]): (Prop, Prop) =
    testObj(test1, aMap)
      .map(cmpProps(_, test2, aMap))
      .orElse(testObj(test2, aMap).map(cmpProps(_, test1, aMap)))
      .getOrElse((Unknown, Unknown))

  // A test whose result can be anything and whose throw-cause is
  // inexpressible; `ev` carries whatever is known from its sub-evaluations.
  private def opaque(ev: Prop): TP =
    TP(ev, ev, ev, th = Unknown, resT = None)

  // A test that evaluates its components but can never be `true`
  // (tuples, conses, map/record literals, numbers, ...); it throws only if
  // some component throws.
  private def neverTrue(args: List[Test], aMap: AMap): TP = {
    val tps = args.map(testProps(_, aMap))
    val ev = and(tps.map(_.ev))
    TP(False, ev, ev, th = or(tps.map(_.th)), resT = None)
  }

  private def objProp(test: Test, aMap: AMap, ty: Type): Prop =
    testObj(test, aMap).map(Pos(_, ty)).getOrElse(True)

  private def objProps(test: Test, aMap: AMap, ty: Type): (Prop, Prop) =
    testObj(test, aMap) match {
      case Some(obj) => (Pos(obj, ty), Neg(obj, ty))
      case None      => (True, True)
    }

  // Facts implied by `test` having been evaluated, without throwing, in a
  // position that requires type `ty` (the enclosing operation throws otherwise).
  private def evIn(test: Test, ty: Type, aMap: AMap): Prop =
    and(List(testProps(test, aMap).ev, objProp(test, aMap, ty)))

  // "the result of `test`, if any, is a boolean"
  // as a proposition when the test is a trackable object, no info otherwise
  private def boolFact(test: Test, tp: TP, aMap: AMap): Prop =
    if (tp.resT.exists(subtype.gradualSubType(_, booleanType))) True
    else objProp(test, aMap, booleanType)

  // "evaluated to boolean `false`": non-true plus boolean-ness of the result
  private def bff(test: Test, tp: TP, aMap: AMap): Prop =
    and(List(tp.ff, boolFact(test, tp, aMap)))

  // Throw-domain duals of objProp/boolFact: facts holding about `test`'s
  // value when the enclosing operation threw because of it. Inexpressible
  // for untrackable tests - Unknown, which absorbs the disjunction.
  private def thNot(test: Test, aMap: AMap, ty: Type): Prop =
    testObj(test, aMap).map(Neg(_, ty)).getOrElse(Unknown)

  private def thIs(test: Test, aMap: AMap, ty: Type): Prop =
    testObj(test, aMap).map(Pos(_, ty)).getOrElse(Unknown)

  // "the result of `test` is not a boolean" as a throw-cause: impossible for
  // structurally-boolean tests, a Neg for trackable ones, unknowable otherwise
  private def thNotBool(test: Test, tp: TP, aMap: AMap): Prop =
    if (tp.resT.exists(subtype.gradualSubType(_, booleanType))) False
    else thNot(test, aMap, booleanType)

  // A type-test on `arg`: true iff `arg` has type `ty`, throws only if
  // evaluating `arg` throws.
  private def typeTest(arg: Test, ty: Type, aMap: AMap): TP = {
    val tpArg = testProps(arg, aMap)
    val (pos, neg) = objProps(arg, aMap, ty)
    TP(and(List(pos, tpArg.ev)), and(List(neg, tpArg.ev)), tpArg.ev, th = tpArg.th, resT = Some(booleanType))
  }

  private def testProps(test: Test, aMap: Map[Name, Obj]): TP = {
    test match {
      // literals evaluate to themselves and never throw
      case TestAtom("true") =>
        TP(True, False, True, th = False, resT = Some(booleanType))
      case TestAtom("false") =>
        TP(False, True, True, th = False, resT = Some(booleanType))
      case TestAtom(_) | TestInteger(_) | TestFloat() | TestString() | TestNil() | TestBinaryLit() |
          TestRecordIndex(_, _) =>
        TP(False, True, True, th = False, resT = None)
      case TestVar(v) =>
        // a bare variable test passes iff the variable is `true`
        val obj = aMap.getOrElse(v, VarObj(v))
        TP(Pos(obj, trueType), Neg(obj, trueType), True, th = False, resT = None)
      // structured literals: never `true`, their components are evaluated
      case TestTuple(elems) =>
        neverTrue(elems, aMap)
      case TestCons(h, t) =>
        neverTrue(List(h, t), aMap)
      case TestMapCreate(kvs) =>
        neverTrue(kvs.flatMap { case (k, v) => List(k, v) }, aMap)
      case TestRecordCreate(_, fields) =>
        neverTrue(fields.map(_.value), aMap)
      case TestRecordSelect(rec, recName, _) =>
        // selection throws iff `rec` throws or is not a #recName{}
        val tpRec = testProps(rec, aMap)
        val recTy = RecordType(recName)(module)
        val ev = and(List(tpRec.ev, objProp(rec, aMap, recTy)))
        val th = or(List(tpRec.th, thNot(rec, aMap, recTy)))
        val (pos, neg) = objProps(test, aMap, trueType)
        TP(and(List(pos, ev)), and(List(neg, ev)), ev, th, resT = None)
      case TestNativeRecordSelect(rec, _, _) =>
        opaque(testProps(rec, aMap).ev)
      case TestMapUpdate(map, kvs) =>
        // update throws unless `map` is a map, but also on a missing key for
        // `:=` associations (not distinguished in the AST) - th stays Unknown
        val tpMap = testProps(map, aMap)
        val kvTps = kvs.flatMap { case (k, v) => List(k, v) }.map(testProps(_, aMap))
        val ev = and(objProp(map, aMap, MapType(Map(), AnyType, AnyType)) :: tpMap.ev :: kvTps.map(_.ev))
        TP(False, ev, ev, th = Unknown, resT = None)
      case TestCall(Id(pred, 1), List(arg)) if unary_predicates.isDefinedAt(pred) =>
        typeTest(arg, unary_predicates(pred), aMap)
      case TestCall(Id("is_function", 2), List(arg, TestInteger(Some(arity)))) =>
        val tPos = FunType(0, List.fill(arity.intValue)(AnyType), AnyType)
        val tNeg = FunType(0, List.fill(arity.intValue)(NoneType), AnyType)
        val tpArg = testProps(arg, aMap)
        val pos = testObj(arg, aMap).map(Pos(_, tPos)).getOrElse(True)
        val neg = testObj(arg, aMap).map(Neg(_, tNeg)).getOrElse(True)
        TP(and(List(pos, tpArg.ev)), and(List(neg, tpArg.ev)), tpArg.ev, th = tpArg.th, resT = Some(booleanType))
      case TestCall(Id("is_function", 2), List(arg, _)) =>
        // non-literal arity: narrow to any fun; evaluation may throw on a bad
        // arity argument, and there is no precise negative
        val tpArg = testProps(arg, aMap)
        val pos = objProp(arg, aMap, AnyFunType)
        TP(and(List(pos, tpArg.ev)), tpArg.ev, tpArg.ev, th = Unknown, resT = Some(booleanType))
      case TestCall(Id("is_record", 3), List(arg, TestAtom(modName), TestAtom(recName))) =>
        typeTest(arg, nativeRecordTypeFor(modName, recName), aMap)
      case TestCall(Id("is_record", 2), List(arg, TestAtom(recName))) =>
        typeTest(arg, resolveIsRecord2Name(recName), aMap)
      case TestCall(Id("is_record", 3), arg :: TestAtom(recName) :: TestInteger(Some(_)) :: Nil) =>
        typeTest(arg, RecordType(recName)(module), aMap)
      case TestCall(Id("is_map_key", 2), List(keyArg, mapArg)) =>
        val tpKey = testProps(keyArg, aMap)
        val tpMap = testProps(mapArg, aMap)
        val mapKT = mapArg match {
          case TestMapCreate(kvs) if kvs.nonEmpty =>
            val keys = kvs.map { case (k, _) => Key.fromTest(k) }
            Option.when(keys.forall(_.isDefined))(UnionType(keys.flatten.map(Key.asType).toSet))
          case _ =>
            None
        }
        val mapTy = MapType(Map(), AnyType, AnyType)
        // `is_map_key(K, M)` throws unless M is a map
        val ev = and(List(tpKey.ev, tpMap.ev, objProp(mapArg, aMap, mapTy)))
        val th = mapArg match {
          case _: TestMapCreate => or(List(tpKey.th, tpMap.th))
          case _                => or(List(tpKey.th, tpMap.th, thNot(mapArg, aMap, mapTy)))
        }
        // is_map_key(K, #{k1 => .., k2 => ....}) -> K :: k1 | k2 | ...
        val keyFact = mapKT.flatMap(keyTy => testObj(keyArg, aMap).map(Pos(_, keyTy))).getOrElse(True)
        TP(and(List(keyFact, ev)), ev, ev, th, resT = Some(booleanType))
      case TestCall(Id("element", 2), List(TestInteger(i), tup)) =>
        // element/2 with a known index throws unless `tup` is a tuple, but
        // also on an out-of-range index - th stays Unknown
        val ev = evIn(tup, AnyTupleType, aMap)
        val (pos, neg) = objProps(test, aMap, trueType)
        TP(and(List(pos, ev)), and(List(neg, ev)), ev, th = Unknown, resT = None)
      case TestCall(Id("element", 2), List(idx, tup)) =>
        // element/2 throws unless `idx` is an integer and `tup` a tuple, but
        // also on an out-of-range index - th stays Unknown
        val ev = and(List(evIn(idx, IntegerType, aMap), evIn(tup, AnyTupleType, aMap)))
        val (pos, neg) = objProps(test, aMap, trueType)
        TP(and(List(pos, ev)), and(List(neg, ev)), ev, th = Unknown, resT = None)
      case TestCall(Id("hd" | "tl", 1), List(arg)) =>
        // throws iff `arg` throws, is not a list, or is []
        val tpArg = testProps(arg, aMap)
        val listTy = ListType(AnyType)
        val ev = and(List(tpArg.ev, objProp(arg, aMap, listTy)))
        val th = or(List(tpArg.th, thNot(arg, aMap, listTy), thIs(arg, aMap, NilType)))
        val (pos, neg) = objProps(test, aMap, trueType)
        TP(and(List(pos, ev)), and(List(neg, ev)), ev, th, resT = None)
      case TestCall(_, args) =>
        // unknown guard BIF: outcome unknown, but its arguments were evaluated
        opaque(and(args.map(testProps(_, aMap).ev)))
      case TestUnOp("not", arg) =>
        val tpArg = testProps(arg, aMap)
        val bf = boolFact(arg, tpArg, aMap)
        val ev = and(List(tpArg.ev, bf))
        val th = or(List(tpArg.th, thNotBool(arg, tpArg, aMap)))
        TP(and(List(tpArg.ff, bf)), and(List(tpArg.tt, bf)), ev, th, resT = Some(booleanType))
      case TestUnOp("bnot", arg) =>
        val tpArg = testProps(arg, aMap)
        val ev = and(List(tpArg.ev, objProp(arg, aMap, IntegerType)))
        val th = or(List(tpArg.th, thNot(arg, aMap, IntegerType)))
        TP(False, ev, ev, th, resT = None)
      case TestUnOp("+" | "-", arg) =>
        val tpArg = testProps(arg, aMap)
        val ev = and(List(tpArg.ev, objProp(arg, aMap, numberType)))
        val th = or(List(tpArg.th, thNot(arg, aMap, numberType)))
        TP(False, ev, ev, th, resT = None)
      case TestUnOp(_, arg) =>
        opaque(testProps(arg, aMap).ev)
      case TestBinOp("and", arg1, arg2) =>
        val tp1 = testProps(arg1, aMap)
        val tp2 = testProps(arg2, aMap)
        val ev = and(List(tp1.ev, boolFact(arg1, tp1, aMap), tp2.ev, boolFact(arg2, tp2, aMap)))
        val tt = and(List(tp1.tt, tp2.tt, ev))
        val ff = and(List(ev, or(List(tp1.ff, tp2.ff))))
        val th = or(List(tp1.th, thNotBool(arg1, tp1, aMap), tp2.th, thNotBool(arg2, tp2, aMap)))
        TP(tt, ff, ev, th, resT = Some(booleanType))
      case TestBinOp("or", arg1, arg2) =>
        val tp1 = testProps(arg1, aMap)
        val tp2 = testProps(arg2, aMap)
        val ev = and(List(tp1.ev, boolFact(arg1, tp1, aMap), tp2.ev, boolFact(arg2, tp2, aMap)))
        val tt = and(List(ev, or(List(tp1.tt, tp2.tt))))
        val ff = and(List(ev, tp1.ff, tp2.ff))
        val th = or(List(tp1.th, thNotBool(arg1, tp1, aMap), tp2.th, thNotBool(arg2, tp2, aMap)))
        TP(tt, ff, ev, th, resT = Some(booleanType))
      case TestBinOp("xor", arg1, arg2) =>
        val tp1 = testProps(arg1, aMap)
        val tp2 = testProps(arg2, aMap)
        val ev = and(List(tp1.ev, boolFact(arg1, tp1, aMap), tp2.ev, boolFact(arg2, tp2, aMap)))
        val tt = and(List(ev, or(List(and(List(tp1.tt, tp2.ff)), and(List(tp1.ff, tp2.tt))))))
        val ff = and(List(ev, or(List(and(List(tp1.tt, tp2.tt)), and(List(tp1.ff, tp2.ff))))))
        val th = or(List(tp1.th, thNotBool(arg1, tp1, aMap), tp2.th, thNotBool(arg2, tp2, aMap)))
        TP(tt, ff, ev, th, resT = Some(booleanType))
      case TestBinOp("andalso", arg1, arg2) =>
        val tp1 = testProps(arg1, aMap)
        val tp2 = testProps(arg2, aMap)
        // arg1 is always evaluated and must be a boolean; arg2 only when arg1 is true
        val ev = and(List(tp1.ev, boolFact(arg1, tp1, aMap)))
        val tt = and(List(tp1.tt, tp2.tt, ev))
        val ff = and(List(ev, or(List(bff(arg1, tp1, aMap), and(List(tp1.tt, tp2.ff))))))
        val th = or(List(tp1.th, thNotBool(arg1, tp1, aMap), and(List(tp1.tt, tp2.th))))
        TP(tt, ff, ev, th, resT = tp2.resT.map(subtype.join(_, falseType)))
      case TestBinOp("orelse", arg1, arg2) =>
        val tp1 = testProps(arg1, aMap)
        val tp2 = testProps(arg2, aMap)
        // arg1 is always evaluated and must be a boolean; arg2 only when arg1 is false
        val ev = and(List(tp1.ev, boolFact(arg1, tp1, aMap)))
        val tt = and(List(ev, or(List(tp1.tt, and(List(bff(arg1, tp1, aMap), tp2.tt))))))
        val ff = and(List(ev, bff(arg1, tp1, aMap), tp2.ff))
        val th = or(List(tp1.th, thNotBool(arg1, tp1, aMap), and(List(bff(arg1, tp1, aMap), tp2.th))))
        TP(tt, ff, ev, th, resT = tp2.resT.map(subtype.join(_, trueType)))
      case TestBinOp("+" | "-" | "*", arg1, arg2) =>
        val tp1 = testProps(arg1, aMap)
        val tp2 = testProps(arg2, aMap)
        val ev = and(List(tp1.ev, objProp(arg1, aMap, numberType), tp2.ev, objProp(arg2, aMap, numberType)))
        val th = or(List(tp1.th, thNot(arg1, aMap, numberType), tp2.th, thNot(arg2, aMap, numberType)))
        TP(False, ev, ev, th, resT = None)
      case TestBinOp("/", arg1, arg2) =>
        // also throws on division by zero - th stays Unknown
        val ev = and(List(evIn(arg1, numberType, aMap), evIn(arg2, numberType, aMap)))
        TP(False, ev, ev, th = Unknown, resT = None)
      case TestBinOp("band" | "bor" | "bxor", arg1, arg2) =>
        val tp1 = testProps(arg1, aMap)
        val tp2 = testProps(arg2, aMap)
        val ev = and(List(tp1.ev, objProp(arg1, aMap, IntegerType), tp2.ev, objProp(arg2, aMap, IntegerType)))
        val th = or(List(tp1.th, thNot(arg1, aMap, IntegerType), tp2.th, thNot(arg2, aMap, IntegerType)))
        TP(False, ev, ev, th, resT = None)
      case TestBinOp("div" | "rem" | "bsl" | "bsr", arg1, arg2) =>
        // also throw on a zero divisor / an absurd shift - th stays Unknown
        val ev = and(List(evIn(arg1, IntegerType, aMap), evIn(arg2, IntegerType, aMap)))
        TP(False, ev, ev, th = Unknown, resT = None)
      case TestBinOp("==" | "=:=", arg1, arg2) =>
        val tp1 = testProps(arg1, aMap)
        val tp2 = testProps(arg2, aMap)
        val (pos, neg) = eqProps(arg1, arg2, aMap)
        val ev = and(List(tp1.ev, tp2.ev))
        TP(and(List(pos, ev)), and(List(neg, ev)), ev, th = or(List(tp1.th, tp2.th)), resT = Some(booleanType))
      case TestBinOp("=/=" | "/=", arg1, arg2) =>
        val tp1 = testProps(arg1, aMap)
        val tp2 = testProps(arg2, aMap)
        val (pos, neg) = eqProps(arg1, arg2, aMap)
        val ev = and(List(tp1.ev, tp2.ev))
        TP(and(List(neg, ev)), and(List(pos, ev)), ev, th = or(List(tp1.th, tp2.th)), resT = Some(booleanType))
      case TestBinOp("<" | "=<" | ">" | ">=", arg1, arg2) =>
        // term ordering never throws, but the operands are still evaluated
        val tp1 = testProps(arg1, aMap)
        val tp2 = testProps(arg2, aMap)
        val ev = and(List(tp1.ev, tp2.ev))
        TP(ev, ev, ev, th = or(List(tp1.th, tp2.th)), resT = Some(booleanType))
      case TestBinOp(_, arg1, arg2) =>
        opaque(and(List(testProps(arg1, aMap).ev, testProps(arg2, aMap).ev)))
    }
  }

  // What a pattern says about the value it matches:
  //   - props   - how the scrutinee is refined when the pattern matches
  //               (positive) and when it does not (negative), if anything
  //               is known
  //   - aliases - the variables the pattern binds, each paired with the
  //               position it matches
  //   - seeds   - fresh variables standing for values that no position
  //               addresses (see the `PatMap` case), each paired with the
  //               object it takes its type from. Ordered outside-in, so a
  //               seed nested inside another one comes after it.
  private case class PatInfo(props: Option[(Prop, Prop)], aliases: List[(Name, Obj)], seeds: List[(Name, Obj)])

  private object PatInfo {
    // A pattern that neither refines nor binds anything
    val trivial: PatInfo = PatInfo(None, Nil, Nil)
    // A pattern whose effect is beyond our precision
    val inexpressible: PatInfo = PatInfo(Some(Unknown, Unknown), Nil, Nil)
    // A pattern that binds nothing and refines only the value it matches
    def leaf(pos: Prop, neg: Prop): PatInfo = PatInfo(Some(pos, neg), Nil, Nil)
  }

  private def patNode(posThis: Prop, negThis: Prop, subs: List[PatInfo]): PatInfo = {
    val (posThat, negThat) = subs.flatMap(_.props).unzip
    val pos = and(posThis :: posThat)
    val neg = or(List(negThis, and(List(posThis, or(negThat)))))
    PatInfo(Some(pos, neg), subs.flatMap(_.aliases), subs.flatMap(_.seeds))
  }

  private def patProps(x: String, path: Path, pat: Pat, env: Env): PatInfo = {
    pat match {
      case PatWild() =>
        PatInfo.trivial
      case PatVar(v) =>
        if (env.contains(v)) PatInfo.inexpressible
        else PatInfo(None, List(v -> mkObj(x, path)), Nil)
      case PatAtom(s) =>
        val obj = mkObj(x, path)
        PatInfo.leaf(Pos(obj, AtomLitType(s)), Neg(obj, AtomLitType(s)))
      case PatFloat() =>
        PatInfo.leaf(Pos(mkObj(x, path), FloatType), Unknown)
      case PatInt() =>
        PatInfo.leaf(Pos(mkObj(x, path), IntegerType), Unknown)
      case PatTuple(elems) =>
        val obj = mkObj(x, path)
        val arity = elems.size
        val tupleTy = TupleType(List.fill(arity)(AnyType))
        val elemsInfo = elems.zipWithIndex.map { case (elem, i) =>
          patProps(x, path :+ TupleField(i, Some(arity)), elem, env)
        }
        patNode(Pos(obj, tupleTy), Neg(obj, tupleTy), elemsInfo)
      case PatRecord(recName, fields, gen) =>
        val obj = mkObj(x, path)
        val recTy = RecordType(recName)(module)
        val namedInfo =
          fields.map(field => patProps(x, path :+ RecordField(field.name, recName), field.pat, env))
        lazy val rec = util.getRecord(module, recName)
        // `_ = GenPat` applies to every field not mentioned explicitly
        val genInfo = gen.toList.flatMap { genPat =>
          rec.toList.flatMap {
            _.fields
              .filter(fDecl => !fields.exists(f => f.name == fDecl.name))
              .map(fDecl => patProps(x, path :+ RecordField(fDecl.name, recName), genPat, env))
          }
        }
        patNode(Pos(obj, recTy), Neg(obj, recTy), genInfo ++ namedInfo)
      case PatMatch(pat1, pat2) =>
        val info1 = patProps(x, path, pat1, env)
        val info2 = patProps(x, path, pat2, env)
        // at most one side can be informative (the other is then a variable or
        // a wildcard); two informative sides are beyond our precision
        val props = (info1.props, info2.props) match {
          case (None, props) => props
          case (props, None) => props
          case _             => Some(Unknown, Unknown)
        }
        PatInfo(props, info1.aliases ++ info2.aliases, info1.seeds ++ info2.seeds)
      case PatMap(pats) =>
        val obj = mkObj(x, path)
        val mapTy = MapType(Map(), AnyType, AnyType)
        val assocsInfo = pats.map { case (patK, patV) =>
          Key.fromTest(patK) match {
            case Some(key) =>
              val keyPath = path :+ MapField(key)
              val info = patProps(x, keyPath, patV, env)
              // the key is required even when its value pattern constrains nothing
              val objKey = mkObj(x, keyPath)
              info.copy(props = info.props.orElse(Some(Pos(objKey, AnyType), Neg(objKey, AnyType))))
            case None =>
              val v = genVar()
              val info = patProps(v, Nil, patV, env)
              val seed = v -> mkObj(x, path :+ AnyMapField)
              val pos = info.props.map(_._1).getOrElse(True)
              PatInfo(Some(pos, Unknown), info.aliases, seed :: info.seeds)
          }
        }
        patNode(Pos(obj, mapTy), Neg(obj, mapTy), assocsInfo)
      case PatNil() =>
        val obj = mkObj(x, path)
        PatInfo.leaf(Pos(obj, NilType), Neg(obj, NilType))
      case PatCons(hpat, tpat) =>
        val obj = mkObj(x, path)
        val posThis = and(List(Pos(obj, ListType(AnyType)), Neg(obj, NilType)))
        val negThis = or(List(Neg(obj, ListType(AnyType)), Pos(obj, NilType)))
        val hInfo = patProps(x, path :+ ListHead, hpat, env)
        val tInfo = patProps(x, path :+ ListTail, tpat, env)
        patNode(posThis, negThis, List(hInfo, tInfo))
      case PatBinary(_) =>
        PatInfo.leaf(Pos(mkObj(x, path), BinaryType), Unknown)
      case _ =>
        PatInfo.inexpressible
    }
  }

  // Binds each fresh variable of `seeds` to the type of the value it stands
  // for. Seeds come outside-in, so one nested inside another resolves against
  // the variable that has just been bound.
  private def bindSeeds(env: Env, seeds: List[(Name, Obj)]): Env =
    seeds.foldLeft(env) { case (env, (v, obj)) =>
      env + (v -> typePathRef(env(objId(obj)), objPath(obj)))
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
        subtype.join(ts.map(remove(_, s)))
      case (BoundedDynamicType(t), s) =>
        BoundedDynamicType(remove(t, s))
      case (t, _) =>
        t
    }

  private def refineRecord(t: Type, field: String, refined: Type): Type = {
    if (Subtype.isNoneType(refined)) {
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
          case + => subtype.meet(t, s)
          case - => remove(t, s)
        }
      case (RemoteType(rid, args), _) =>
        val body = util.getTypeDeclBody(rid, args)
        update(body, path, pol, s)
      case (UnionType(ts), _) =>
        subtype.join(ts.map(update(_, path, pol, s)))
      case (BoundedDynamicType(t), _) =>
        BoundedDynamicType(update(t, path, pol, s))
      case (TupleType(ts), TupleField(pos, Some(arity)) :: path) if ts.size == arity =>
        val t = ts(pos)
        val t1 = update(t, path, pol, s)
        TupleType_*(ts.updated(pos, t1))
      case (rt: RecordType, RecordField(fieldName, recName) :: path) if rt.name == recName =>
        util.getRecord(rt.module, rt.name).flatMap(_.fMap.get(fieldName)) match {
          case Some(field) =>
            val t1 = update(field.tp, path, pol, s)
            refineRecord(rt, fieldName, t1)
          case None => rt
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
          util.getRecord(rt.recType.module, rt.recType.name).flatMap(_.fMap.get(fieldName)) match {
            case Some(field) =>
              val t1 = update(field.tp, path, pol, s)
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
        if (Subtype.isNoneType(update(lt, path, pol, s)))
          NoneType
        else
          ListType(lt)
      case (ListType(lt), ListTail :: path) =>
        if (Subtype.isNoneType(update(ListType(lt), path, pol, s)))
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
        if (Subtype.isNoneType(s)) None else Some(typeEnv.updated(x, s))
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
          .flatMap(_.fMap.get(fieldName))
          .map(_.tp)
          .map(typePathRef(_, path1))
          .getOrElse(DynamicType)
      case (BoundedDynamicType(bound), _) =>
        BoundedDynamicType(typePathRef(bound, path))
      case (UnionType(ts), _) =>
        subtype.join(ts.map(typePathRef(_, path)))
      case (TupleType(ts), TupleField(index, Some(arity)) :: path1) if ts.size == arity =>
        typePathRef(ts(index), path1)
      case (rTy: RecordType, RecordField(fieldName, recName) :: path1) if rTy.name == recName =>
        util
          .getRecord(rTy.module, rTy.name)
          .flatMap(_.fMap.get(fieldName))
          .map(_.tp)
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
            .flatMap(_.fMap.get(fieldName))
            .map(_.tp)
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
      case (mTy: MapType, AnyMapField :: path1) =>
        typePathRef(narrow.getValType(mTy), path1)
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

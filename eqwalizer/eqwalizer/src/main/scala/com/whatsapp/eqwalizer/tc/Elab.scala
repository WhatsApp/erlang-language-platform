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

import com.whatsapp.eqwalizer.ast.Exprs.*
import com.whatsapp.eqwalizer.ast.Forms.{FunDecl, FunSpec, NativeRecField, OverloadedFunSpec}
import com.whatsapp.eqwalizer.ast.Guards.Guard
import com.whatsapp.eqwalizer.ast.Pats.{PatMatch, PatVar}
import com.whatsapp.eqwalizer.ast.Types.*
import com.whatsapp.eqwalizer.ast.stub.Db
import com.whatsapp.eqwalizer.ast.{Specifier, Filters, Pats, RemoteId, Vars}
import com.whatsapp.eqwalizer.tc.TcDiagnostics.*

final class Elab(pipelineContext: PipelineContext) {
  private lazy val module = pipelineContext.module
  private lazy val elabPat = pipelineContext.elabPat
  private lazy val elabApply = pipelineContext.elabApply
  private lazy val elabApplyCustom = pipelineContext.elabApplyCustom
  private lazy val elabApplyOverloaded = pipelineContext.elabApplyOverloaded
  private lazy val subtype = pipelineContext.subtype
  private lazy val util = pipelineContext.util
  private lazy val narrow = pipelineContext.narrow
  private lazy val occurrence = pipelineContext.occurrence
  private lazy val customReturn = pipelineContext.customReturn
  private lazy val typeInfo = pipelineContext.typeInfo
  private lazy val diagnosticsInfo = pipelineContext.diagnosticsInfo
  private lazy val instantiate = pipelineContext.instantiate
  private implicit val pipelineCtx: PipelineContext = pipelineContext

  def checkFun(f: FunDecl, spec: FunSpec): Unit = {
    val (_, FunType(_, argTys, resTy)) = instantiate.instantiate(spec.ty)
    val clauseEnvs = occurrence.clausesEnvs(f.clauses, argTys, Env.empty)
    val singleClause = f.clauses.length == 1
    f.clauses
      .lazyZip(1 to f.clauses.length)
      .lazyZip(clauseEnvs)
      .foreach((clause, index, occEnv) =>
        elabClause(
          clause,
          argTys,
          occEnv,
          Set.empty,
          resTy,
          checkCoverage = true,
          fullCoverage = singleClause || (index != f.clauses.length),
        )
      )
  }

  def checkOverloadedFun(f: FunDecl, overloadedSpec: OverloadedFunSpec): Unit =
    overloadedSpec.tys.foreach { ft0 =>
      val (_, FunType(_, argTys, resTy)) = instantiate.instantiate(ft0)
      val clauseEnvs = occurrence.clausesEnvs(f.clauses, argTys, Env.empty)
      f.clauses
        .lazyZip(clauseEnvs)
        .foreach((clause, occEnv) => elabClause(clause, argTys, occEnv, Set.empty, resTy, checkReachability = true))
    }

  def elabBody(body: Body, env: Env, expected: Type = AnyType): (Type, Env) = {
    var envAcc = env
    for (expr <- body.exprs.init)
      envAcc = elabExpr(expr, envAcc)._2
    elabExpr(body.exprs.last, envAcc, expected)
  }

  def elabClause(
      clause: Clause,
      argTys: List[Type],
      env0: Env,
      exportedVars: Set[String],
      expected: Type = AnyType,
      checkReachability: Boolean = false,
      checkCoverage: Boolean = false,
      fullCoverage: Boolean = false,
  ): (Type, Env) = {
    val patVars = Vars.clausePatVars(clause)
    val env1 = util.enterScope(env0, patVars)
    // Refine guards before patterns (so refinements feed pattern elaboration)
    val env2 = occurrence.refineGuards(clause.guards, env1)
    val (patTys, env3) = elabPat.elabPats(clause.pats, argTys, env2)
    occurrence.annotateGuards(clause.guards, env3)
    val hasEmptyType = env3.exists { case (_, ty) => Subtype.isNoneType(ty) }
    if (hasEmptyType && checkCoverage && (fullCoverage || !occurrence.clauseCovered(clause, argTys)))
      diagnosticsInfo.add(ClauseNotCovered(clause.pos))
    if (checkReachability && (hasEmptyType || patTys.exists(Subtype.isNoneType)))
      return (NoneType, util.exitScope(env0, env3, exportedVars))
    val (eType, env4) = elabBody(clause.body, env3, expected)
    val env5 = util.exitScope(env0, env4, exportedVars)
    if (subtype.gradualSubType(eType, NoneType))
      (NoneType, env5.map { case (name, _) => name -> NoneType })
    else
      (eType, env5)
  }

  def elabExprs(exprs: List[Expr], env: Env): (List[Type], Env) = {
    var envAcc = env
    val tys = exprs.map { expr =>
      val (ty, env1) = elabExpr(expr, envAcc)
      envAcc = env1
      ty
    }
    (tys, envAcc)
  }

  private def elabMaybe(maybe: Maybe, env: Env): (Type, Env) = {
    var envAcc = env
    var tyAcc: Type = NoneType
    var lastTy: Type = NoneType
    val exprs = maybe.body.exprs
    for (expr <- exprs) {
      expr match {
        case MaybeMatch(Pats.PatAtom("true"), mExp) if Filters.asTest(mExp).isDefined =>
          val test = Filters.asTest(mExp).get
          val (mType, _) = elabExpr(mExp, envAcc)
          val elseTy = occurrence.remove(mType, trueType)
          envAcc = occurrence.testEnv(test, envAcc, result = true)
          tyAcc = subtype.join(tyAcc, elseTy)
          lastTy = mType
        case MaybeMatch(mPat, mExp) =>
          val (mType, env1) = elabExpr(mExp, envAcc)
          val (patTy, env2) = elabPat.elabPat(mPat, mType, env1)
          val elseTy = occurrence.remove(mType, patTy)
          tyAcc = subtype.join(tyAcc, elseTy)
          lastTy = patTy
          envAcc = env2
        case _ =>
          val (expTy, env1) = elabExpr(expr, envAcc)
          lastTy = expTy
          envAcc = env1
      }
    }
    (subtype.join(tyAcc, lastTy), env)
  }

  /** Elaborates `expr`, checking it against `expected`. Branching forms propagate `expected` to their
    * branches, so mismatches are reported at the innermost expression. On mismatch the type is `DynamicType`.
    */
  def elabExpr(expr: Expr, env: Env, expected: Type = AnyType): (Type, Env) =
    expr match {
      case lambda: Lambda if !subtype.subType(AnyType, expected) =>
        val (env1, typed) = checkLambda(lambda, expected, env)
        (if (typed) expected else DynamicType, env1)
      case DynCall(l: Lambda, args) =>
        val arity = l.clauses.head.pats.size
        val (argTys, env1) = elabExprs(args, env)
        if (arity != args.size) {
          diagnosticsInfo.add(LambdaArityMismatch(l.pos, l, lambdaArity = arity, argsArity = args.size))
          return (DynamicType, env1)
        }
        l.name match {
          case Some(name) =>
            val resTy = if (subtype.subType(AnyType, expected)) DynamicType else expected
            val funType = FunType(0, List.fill(argTys.size)(DynamicType), resTy)
            if (arity > 0 && pipelineCtx.reportDynamicLambdas && typeInfo.isCollect) {
              diagnosticsInfo.add(DynamicLambda(l.pos))
            }
            elabExpr(l, env.updated(name, funType), funType)
            (resTy, env1)
          case _ =>
            val envs = occurrence.clausesEnvs(l.clauses, argTys, env1)
            val (resTys, resEnvs) =
              l.clauses
                .lazyZip(envs)
                .map((clause, occEnv) => elabClause(clause, argTys, occEnv, Set.empty, expected))
                .unzip
            val resEnv = if (args.isEmpty) subtype.joinEnvs(resEnvs) else env1
            (subtype.join(resTys), resEnv)
        }
      case Block(block) =>
        elabBody(block, env, expected)
      case c: Case if Predicates.isCaseIf(c) =>
        // Elaborate test expression to store its type info
        elabExpr(c.expr, env)
        elabExpr(Predicates.asIf(c), env, expected)
      case Case(call @ RemoteCall(id, args), clauses)
          if Predicates.booleanClauses(clauses) && elabApplyCustom.isCustomPredicate(id) =>
        val (_, posEnv, negEnv) = elabApplyCustom.elabCustomPredicate(id, args, env, call.pos)
        val (posClause, negClause) = Predicates.posNegClauses(clauses)
        val effVars = Vars.clausesVars(clauses)
        val (posT, posEnv1) =
          elabClause(posClause, List(booleanType), posEnv, effVars, expected)
        val (negT, negEnv1) =
          elabClause(negClause, List(booleanType), negEnv, effVars, expected)
        (subtype.join(posT, negT), subtype.joinEnvs(List(posEnv1, negEnv1)))
      case c @ Case(sel, clauses) =>
        val (selTy, env1) = elabExpr(sel, env)
        val effVars = Vars.clausesVars(clauses)
        val clauseEnvs = occurrence.caseEnvs(c, selTy, env1)
        val (ts, envs) = clauses
          .lazyZip(clauseEnvs)
          .map((clause, occEnv) => elabClause(clause, List(selTy), occEnv, effVars, expected))
          .unzip
        (subtype.join(ts), subtype.joinEnvs(envs))
      case i @ If(clauses) =>
        val effVars = Vars.clausesVars(clauses)
        val clauseEnvs = occurrence.ifEnvs(i, env)
        val (ts, envs) = clauses
          .lazyZip(clauseEnvs)
          .map((clause, occEnv) => elabClause(clause, List.empty, occEnv, effVars, expected))
          .unzip
        (subtype.join(ts), subtype.joinEnvs(envs))
      case TryCatchExpr(tryBody, catchClauses, afterBody) =>
        val (tryT, _) = elabBody(tryBody, env, expected)
        val stackType = clsExnStackTypeDynamic
        val catchEnvs = occurrence.clausesEnvs(catchClauses, List(stackType), env)
        val (catchTs, _) = catchClauses
          .lazyZip(catchEnvs)
          .map((clause, occEnv) => elabClause(clause, List(stackType), occEnv, Set.empty, expected))
          .unzip
        val env1 = afterBody match {
          case Some(block) => elabBody(block, env)._2
          case None        => env
        }
        (subtype.join(tryT :: catchTs), env1)
      case TryOfCatchExpr(tryBody, tryClauses, catchClauses, afterBody) =>
        val (tryT, tryEnv) = elabBody(tryBody, env)
        val stackType = clsExnStackTypeDynamic
        val tryEnvs = occurrence.clausesEnvs(tryClauses, List(tryT), tryEnv)
        val (tryTs, _) =
          tryClauses
            .lazyZip(tryEnvs)
            .map((clause, occEnv) => elabClause(clause, List(tryT), occEnv, Set.empty, expected))
            .unzip
        val catchEnvs = occurrence.clausesEnvs(catchClauses, List(stackType), env)
        val (catchTs, _) = catchClauses
          .lazyZip(catchEnvs)
          .map((clause, occEnv) => elabClause(clause, List(stackType), occEnv, Set.empty, expected))
          .unzip
        val env1 = afterBody match {
          case Some(block) => elabBody(block, env)._2
          case None        => env
        }
        (subtype.join(tryTs ::: catchTs), env1)
      case Receive(clauses) =>
        val effVars = Vars.clausesVars(clauses)
        val argType = DynamicType
        val clauseEnvs = occurrence.clausesEnvs(clauses, List(argType), env)
        val (ts, envs) = clauses
          .lazyZip(clauseEnvs)
          .map((clause, occEnv) => elabClause(clause, List(argType), occEnv, effVars, expected))
          .unzip
        (subtype.join(ts), subtype.joinEnvs(envs))
      case ReceiveWithTimeout(List(), timeout, timeoutBlock) =>
        val (_, env1) = elabExpr(timeout, env, builtinTypes("timeout"))
        elabBody(timeoutBlock, env1, expected)
      case ReceiveWithTimeout(clauses, timeout, timeoutBlock) =>
        val effVars = Vars.clausesAndBlockVars(clauses, timeoutBlock)
        val argType = DynamicType
        val clauseEnvs = occurrence.clausesEnvs(clauses, List(argType), env)
        val (ts, envs) = clauses
          .lazyZip(clauseEnvs)
          .map((clause, occEnv) => elabClause(clause, List(argType), occEnv, effVars, expected))
          .unzip
        val (_, env1) = elabExpr(timeout, env, builtinTypes("timeout"))
        val (timeoutT, timeoutEnv) = elabBody(timeoutBlock, env1, expected)
        (subtype.join(timeoutT :: ts), subtype.joinEnvs(timeoutEnv :: envs))
      case MaybeElse(body, elseClauses) =>
        val (bodyType, _) = elabBody(body, env, expected)
        val argType = DynamicType
        val (ts, _) = elseClauses.map(elabClause(_, List(argType), env, Set.empty, expected)).unzip
        (subtype.join(bodyType :: ts), env)
      case _ =>
        val (ty, env1) = synthExpr(expr, env)
        if (subtype.subType(ty, expected)) (ty, env1)
        else {
          diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = expected, got = ty))
          (DynamicType, env1)
        }
    }

  private def synthExpr(expr: Expr, env: Env): (Type, Env) =
    expr match {
      case Var(v) =>
        val ty = env.getOrElse(v, { diagnosticsInfo.add(UnboundVar(expr.pos, v)); DynamicType })
        typeInfo.add(expr.pos, ty)
        (ty, env)
      case AtomLit(a) =>
        (AtomLitType(a), env)
      case FloatLit() =>
        (FloatType, env)
      case IntLit(_) =>
        (IntegerType, env)
      case Tuple(elems) =>
        var envAcc = env
        val elemTypes = elems.map { elem =>
          val (eType, env1) = elabExpr(elem, envAcc)
          envAcc = env1
          eType
        }
        (TupleType(elemTypes), envAcc)
      case StringLit(empty) =>
        val litType = if (empty) NilType else stringType
        (litType, env)
      case NilLit() =>
        (NilType, env)
      case Cons(head, NilLit()) =>
        val (headT, env1) = elabExpr(head, env)
        val resType = subtype.join(util.flattenUnions(headT).map(ListType(_)).toSet)
        (resType, env1)
      case Cons(head, tail) =>
        val (headT, env1) = elabExpr(head, env)
        val (tailT, env2) = elabExpr(tail, env1, ListType(AnyType))
        val resType = narrow.asListType(tailT) match {
          case Some(ListType(t)) => ListType(subtype.join(headT, t))
          case None              => ListType(headT)
        }
        (resType, env2)
      case LocalCall(id, args) =>
        val funId = util.globalFunId(module, id)
        if (elabApplyCustom.isCustom(funId)) {
          elabApplyCustom.elabCustom(funId, args, env, expr.pos)
        } else if (elabApplyOverloaded.isOverloadedFun(funId)) {
          elabApplyOverloaded.elabOverloaded(expr, funId, args, env)
        } else {
          val ft = util.getFunType(module, id)
          val (argTys, env1) = typeInfo.withoutLambdaTypeCollection {
            elabExprs(args, env)
          }
          var resTy = elabApply.elabApply(ft, args, argTys, env1, expr.pos)
          if (customReturn.isCustomReturn(funId))
            resTy = customReturn.customizeResultType(funId, args, argTys, resTy)
          (resTy, env1)
        }
      case DynCall(DynRemoteFun(mod, name), args) =>
        val (_, env1) = elabExpr(mod, env, AtomType)
        val (_, env2) = elabExpr(name, env1, AtomType)
        val (_argTys, env3) = elabExprs(args, env)
        (DynamicType, env3)
      case DynCall(f, args) =>
        val (ty, env1) = elabExpr(f, env)
        val expArity = args.size
        val funTy =
          if (!util.isFunType(ty, expArity)) {
            diagnosticsInfo.add(ExpectedFunType(f.pos, f, expArity, ty))
            DynamicType
          } else {
            ty
          }
        val funTys = narrow.asFunTypes(funTy, args.size)
        if (funTys.isEmpty) {
          val (_, env2) = elabExprs(args, env1)
          (NoneType, env2)
        } else {
          val (argTys, env2) = elabExprs(args, env1)
          val resTys = funTys.map(elabApply.elabApply(_, args, argTys, env2, expr.pos))
          (subtype.join(resTys), env2)
        }
      case DynRemoteFunArity(mod, name, arityExpr) =>
        val (_, env1) = elabExpr(mod, env, AtomType)
        val (_, env2) = elabExpr(name, env1, AtomType)
        val (_, env3) = elabExpr(arityExpr, env2, IntegerType)
        val funType =
          arityExpr match {
            case IntLit(Some(arity)) =>
              FunType(0, List.fill(arity)(DynamicType), DynamicType)
            case _ =>
              AnyFunType
          }
        (funType, env3)
      case RemoteCall(RemoteId("eqwalizer", "reveal_type", 1), List(expr)) =>
        val (t, env1) = elabExpr(expr, env)
        diagnosticsInfo.add(RevealTypeHint(t)(expr.pos)(pipelineContext))
        (t, env1)
      case RemoteCall(fqn, args) =>
        if (elabApplyCustom.isCustom(fqn)) {
          elabApplyCustom.elabCustom(fqn, args, env, expr.pos)
        } else if (elabApplyOverloaded.isOverloadedFun(fqn)) {
          elabApplyOverloaded.elabOverloaded(expr, fqn, args, env)
        } else {
          val ft = util.getFunType(fqn)
          val (argTys, env1) = typeInfo.withoutLambdaTypeCollection {
            elabExprs(args, env)
          }
          var resTy = elabApply.elabApply(ft, args, argTys, env1, expr.pos)
          if (customReturn.isCustomReturn(fqn))
            resTy = customReturn.customizeResultType(fqn, args, argTys, resTy)
          (resTy, env1)
        }
      case LocalFun(id) =>
        val fqn = util.globalFunId(module, id)
        val ft = util.getFunType(fqn)
        (ft, env)
      case RemoteFun(fqn) =>
        val ft = util.getFunType(fqn)
        (ft, env)
      case lambda @ Lambda(clauses) =>
        val arity = clauses.head.pats.length
        val funType = FunType(0, List.fill(arity)(DynamicType), DynamicType)
        val env1 = lambda.name match {
          case Some(name) =>
            env.updated(name, funType)
          case _ =>
            env
        }
        if (arity == 0) {
          val clauseTys = lambda.clauses.map(elabClause(_, Nil, env1, Set.empty)).map(_._1)
          val resTy = subtype.join(clauseTys)
          (FunType(0, Nil, resTy), env)
        } else {
          typeInfo.processLambda {
            if (pipelineCtx.reportDynamicLambdas && typeInfo.isCollect) {
              diagnosticsInfo.add(DynamicLambda(lambda.pos))
            }
            elabExpr(lambda, env1, funType)
          }
          (funType, env)
        }
      case Match(Pats.PatAtom("true"), mExp) if Filters.asTest(mExp).isDefined =>
        val test = Filters.asTest(mExp).get
        val env1 = occurrence.testEnv(test, env, result = true)
        (AtomLitType("true"), env1)
      case Match(mPat1, m @ Match(mPat2, mExp)) =>
        elabExpr(Match(PatMatch(mPat1, mPat2)(m.pos), mExp)(expr.pos), env)
      case Match(mPat, mExp) =>
        val (ty, env1) = elabExpr(mExp, env)
        val (patTy, patEnv) = elabPat.elabPat(mPat, ty, env1)
        (patTy, patEnv)
      case UnOp(op, arg) =>
        op match {
          case "not" =>
            val (_, env1) = elabExpr(arg, env, booleanType)
            (booleanType, env1)
          case "bnot" =>
            val (_, env1) = elabExpr(arg, env, IntegerType)
            (IntegerType, env1)
          case "-" | "+" =>
            val (argTy, env1) = elabExpr(arg, env)
            if (subtype.subType(argTy, numberType)) (argTy, env1)
            else {
              diagnosticsInfo.add(ExpectedSubtype(arg.pos, arg, expected = numberType, got = argTy))
              (DynamicType, env1)
            }
          case _ =>
            throw UnhandledOp(expr.pos, op)
        }
      case BinOp("orelse", testArg, RemoteCall(RemoteId("erlang", "throw" | "error" | "exit", _), _))
          if Filters.asTest(testArg).isDefined =>
        val test = Filters.asTest(testArg).get
        val env1 = occurrence.testEnv(test, env, result = true)
        (AtomLitType("true"), env1)
      case BinOp(
            "orelse",
            call @ RemoteCall(id, args),
            RemoteCall(RemoteId("erlang", "throw" | "error" | "exit", _), _),
          ) if elabApplyCustom.isCustomPredicate(id) =>
        val (_, posEnv, _) = elabApplyCustom.elabCustomPredicate(id, args, env, call.pos)
        (AtomLitType("true"), posEnv)
      case BinOp("andalso", testArg, RemoteCall(RemoteId("erlang", "throw" | "error" | "exit", _), _))
          if Filters.asTest(testArg).isDefined =>
        val test = Filters.asTest(testArg).get
        val env1 = occurrence.testEnv(test, env, result = false)
        (AtomLitType("false"), env1)
      case BinOp(op, arg1, arg2) =>
        op match {
          case "div" | "rem" | "band" | "bor" | "bxor" | "bsl" | "bsr" =>
            val (_, env1) = elabExpr(arg1, env, IntegerType)
            val (_, env2) = elabExpr(arg2, env1, IntegerType)
            (IntegerType, env2)
          case "/" =>
            val (_, env1) = elabExpr(arg1, env, numberType)
            val (_, env2) = elabExpr(arg2, env1, numberType)
            (FloatType, env2)
          case "*" | "+" | "-" =>
            val (arg1Ty, env1) = elabExpr(arg1, env)
            val (arg2Ty, env2) = elabExpr(arg2, env1)
            val v1 = subtype.subType(arg1Ty, numberType)
            val v2 = subtype.subType(arg2Ty, numberType)
            if (v1 && v2) {
              if (subtype.gradualEqv(arg1Ty, FloatType) || subtype.gradualEqv(arg2Ty, FloatType))
                (FloatType, env2)
              else
                (subtype.join(arg1Ty, arg2Ty), env2)
            } else {
              if (!v1)
                diagnosticsInfo.add(ExpectedSubtype(arg1.pos, arg1, expected = numberType, got = arg1Ty))
              if (!v2)
                diagnosticsInfo.add(ExpectedSubtype(arg2.pos, arg2, expected = numberType, got = arg2Ty))
              (DynamicType, env2)
            }
          case "or" | "and" | "xor" =>
            val (_, env1) = elabExpr(arg1, env, booleanType)
            val (_, env2) = elabExpr(arg2, env1, booleanType)
            (booleanType, env2)
          case "orelse" =>
            val (_, env1) = elabExpr(arg1, env, booleanType)
            Filters.asTest(arg1) match {
              case Some(test) =>
                val ifClause1 =
                  Clause(List.empty, List(Guard(List(test))), Body(List(AtomLit("true")(arg1.pos))))(arg1.pos)
                val ifClause2 = Clause(List.empty, List.empty, Body(List(arg2)))(arg2.pos)
                val ifExpr = If(List(ifClause1, ifClause2))(expr.pos)
                elabExpr(ifExpr, env)
              case None =>
                val (t2, env2) = elabExpr(arg2, env1)
                (subtype.join(trueType, t2), env2)
            }
          case "andalso" =>
            val (t1, env1) = elabExpr(arg1, env, booleanType)
            val env1Refined = Filters.asTest(arg1) match {
              case None =>
                env1
              case Some(test) =>
                val env11 = occurrence.testEnv(test, env1, result = true)
                env11
            }
            val t1False = subtype.subType(t1, falseType) && !subtype.subType(t1, trueType)
            if (t1False) (falseType, env1)
            else {
              val (t2, _) = elabExpr(arg2, env1Refined)
              val t1True = subtype.subType(t1, trueType) && !subtype.subType(t1, falseType)
              if (t1True)
                (t2, env1)
              else
                (subtype.join(falseType, t2), env1)
            }
          case ">" | "<" | "/=" | ">=" | "=<" | "=/=" | "=:=" | "==" =>
            val (t1, env1) = elabExpr(arg1, env)
            val (t2, env2) = elabExpr(arg2, env1)
            (booleanType, env2)
          case "!" =>
            val sendCall = RemoteCall(RemoteId("erlang", "send", 2), List(arg1, arg2))(expr.pos)
            elabExpr(sendCall, env)
          case "++" | "--" =>
            val (arg1Ty, env1) = elabExpr(arg1, env, ListType(AnyType))
            val (arg2Ty, env2) = elabExpr(arg2, env1, ListType(AnyType))
            val resTy =
              if (op == "--") arg1Ty
              else {
                val Some(ListType(elem1Ty)) = narrow.asListType(arg1Ty): @unchecked
                val Some(ListType(elem2Ty)) = narrow.asListType(arg2Ty): @unchecked
                ListType(subtype.join(elem1Ty, elem2Ty))
              }
            (resTy, env2)
          case _ =>
            throw UnhandledOp(expr.pos, op)
        }
      case Binary(elems) =>
        var envAcc = env
        for { elem <- elems } {
          val (_, env1) = elabBinaryElem(elem, envAcc)
          envAcc = env1
        }
        (BinaryType, envAcc)
      case Catch(cExpr) =>
        val (strictType, _) = elabExpr(cExpr, env)
        val resultType = UnionType(Set(strictType, DynamicType))
        (resultType, env)
      case LComprehension(template, qualifiers) =>
        val qEnv = elabQualifiers(qualifiers, env)
        val (tType, _) = elabExpr(template, qEnv)
        (ListType(tType), env)
      case BComprehension(template, qualifiers) =>
        val qEnv = elabQualifiers(qualifiers, env)
        elabExpr(template, qEnv, BinaryType)
        (BinaryType, env)
      case MComprehension(kTemplate, vTemplate, List(MGenerate(gk: PatVar, gv: PatVar, gExpr))) =>
        val (gT, gEnv) = elabExpr(gExpr, env, mapOrIterTy)
        val mapT = narrow.asMapOrIterTypes(gT)
        val kvTys = mapT.flatMap(narrow.getKVType)
        var mapsAcc: Set[MapType] = Set()
        for (TupleType(List(kTy, vTy)) <- kvTys) {
          val (_, kPatEnv) = elabPat.elabPat(gk, kTy, gEnv)
          val (_, vPatEnv) = elabPat.elabPat(gv, vTy, kPatEnv)
          val (kType, _) = elabExpr(kTemplate, vPatEnv)
          val (vType, _) = elabExpr(vTemplate, vPatEnv)
          val mapTy = MapType(Map(), kType, vType)
          mapsAcc = mapsAcc + mapTy
        }
        (narrow.joinAndMergeMaps(mapsAcc), env)
      case MComprehension(kTemplate, vTemplate, List(MGenerateStrict(gk: PatVar, gv: PatVar, gExpr))) =>
        val (gT, gEnv) = elabExpr(gExpr, env, mapOrIterTy)
        val mapT = narrow.asMapOrIterTypes(gT)
        val kvTys = mapT.flatMap(narrow.getKVType)
        var mapsAcc: Set[MapType] = Set()
        for (TupleType(List(kTy, vTy)) <- kvTys) {
          val (_, kPatEnv) = elabPat.elabPat(gk, kTy, gEnv)
          val (_, vPatEnv) = elabPat.elabPat(gv, vTy, kPatEnv)
          val (kType, _) = elabExpr(kTemplate, vPatEnv)
          val (vType, _) = elabExpr(vTemplate, vPatEnv)
          val mapTy = MapType(Map(), kType, vType)
          mapsAcc = mapsAcc + mapTy
        }
        (narrow.joinAndMergeMaps(mapsAcc), env)
      case MComprehension(kTemplate, vTemplate, qualifiers) =>
        val qEnv = elabQualifiers(qualifiers, env)
        val (kType, _) = elabExpr(kTemplate, qEnv)
        val (vType, _) = elabExpr(vTemplate, qEnv)
        (MapType(Map(), kType, vType), env)
      case rCreate: RecordCreate =>
        elabRecordCreate(rCreate, env)
      case rUpdate: RecordUpdate =>
        elabRecordUpdate(rUpdate, env)
      case RecordSelect(recExpr, recName, fieldName) =>
        val recDecl = util.getRecord(module, recName)
        val (elabTy, elabEnv) = elabExpr(recExpr, env, RecordType(recName)(module))
        (narrow.getRecordField(recDecl, elabTy, fieldName), elabEnv)
      case RecordIndex(_, _) =>
        (IntegerType, env)
      case rUpdate: NativeRecordUpdate =>
        elabNativeRecordUpdate(rUpdate, env)
      case rCreate: NativeRecordCreate =>
        elabNativeRecordCreate(rCreate, env)
      case nrSelect: NativeRecordSelect =>
        elabNativeRecordSelect(nrSelect, env)
      case MapCreate(kvs) =>
        var envAcc = env
        val (props, kts) = kvs.partitionMap { case (kExpr, vExpr) =>
          Key.fromExpr(kExpr) match {
            case Some(key) =>
              val (valT, env1) = elabExpr(vExpr, envAcc)
              envAcc = env1
              Left(key -> MapProp(req = true, valT))
            case None =>
              val (keyT, env1) = elabExpr(kExpr, envAcc)
              val (valT, env2) = elabExpr(vExpr, env1)
              envAcc = env2
              Right(keyT, valT)
          }
        }
        val (keyTs, valTs) = kts.unzip
        val domain = subtype.join(keyTs)
        val codomain = subtype.join(valTs)
        (MapType(props.toMap, domain, codomain), envAcc)
      case MapUpdate(map, kvs) =>
        val (mapT, env1) = elabExpr(map, env, MapType(Map(), AnyType, AnyType))
        var envAcc = env1
        var resT = narrow.asMapTypes(mapT)
        for ((key, value) <- kvs) {
          val (keyT, env2) = elabExpr(key, envAcc)
          val (valT, env3) = elabExpr(value, env2)
          envAcc = env3
          resT = resT.map(narrow.adjustMapType(_, keyT, valT))
        }
        (subtype.join(resT), envAcc)
      case MaybeMatch(mPat, mExp) =>
        val (mType, env1) = elabExpr(mExp, env)
        elabPat.elabPat(mPat, mType, env1)
      case m: Maybe =>
        elabMaybe(m, env)
      case TypeCast(expr, ty, checked) =>
        val validTy = {
          Db.validateType(ty) match {
            case Left(ty) => ty
            case Right(invalid) =>
              diagnosticsInfo.add(invalid)
              DynamicType
          }
        }
        val (exprTy, env1) = elabExpr(expr, env)
        if (checked && !subtype.subType(exprTy, validTy))
          diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = validTy, got = exprTy))
        (validTy, env1)
      case _ =>
        throw new IllegalStateException(s"unexpected $expr")
    }

  def checkLambda(lambda: Lambda, resTy: Type, env: Env): (Env, Boolean) =
    resTy match {
      case t: FunType =>
        checkLambdaFunType(lambda, t, env)
      case _ =>
        narrow.asFunTypes(resTy, lambda.clauses.head.pats.size).toList match {
          case List(funTy) =>
            checkLambdaFunType(lambda, funTy, env)
          case _ =>
            val (ty, _) = elabExpr(lambda, env)
            if (!subtype.subType(ty, resTy)) {
              diagnosticsInfo.add(ExpectedSubtype(lambda.pos, lambda, expected = resTy, got = ty))
              (env, false)
            } else (env, true)
        }
    }

  private def checkLambdaFunType(lambda: Lambda, funTy: FunType, env: Env): (Env, Boolean) = {
    val FunType(_, fParamTys, fResTy) = funTy
    val arity = lambda.clauses.head.pats.size
    if (arity != fParamTys.size) {
      diagnosticsInfo.add(LambdaArityMismatch(lambda.pos, lambda, lambdaArity = arity, argsArity = fParamTys.size))
      return (env, false)
    }
    val env1 = lambda.name match {
      case Some(name) =>
        env.updated(name, funTy)
      case _ =>
        env
    }
    val envs = occurrence.clausesEnvs(lambda.clauses, fParamTys, env1)

    var typed: Boolean = true
    for ((clause, occEnv) <- lambda.clauses.lazyZip(envs)) {
      val (infResType, _) = elabClause(clause, fParamTys, occEnv, Set.empty)
      if (!subtype.subType(infResType, fResTy)) {
        val expr = clause.body.exprs.last
        diagnosticsInfo.add(ExpectedSubtype(expr.pos, expr, expected = fResTy, got = infResType))
        typed = false
      }
    }
    (env, typed)
  }

  private def elabBinaryElem(elem: BinaryElem, env: Env): (Type, Env) = {
    val env1 = elem.size match {
      case Some(s) => elabExpr(s, env, IntegerType)._2
      case None    => env
    }
    val isStringLiteral = elem.expr.isInstanceOf[StringLit]
    val expType = Specifier.expType(elem.specifier, isStringLiteral)
    val (_, env2) = elabExpr(elem.expr, env1, expType)
    (expType, env2)
  }

  private def elabRecordCreate(rCreate: RecordCreate, env: Env): (Type, Env) = {
    val RecordCreate(recName, fields) = rCreate
    val recType = RecordType(recName)(module)
    val namedFields = fields.collect { case n: RecordFieldNamed => n }
    val genFieldOpt = fields.collectFirst { case g: RecordFieldGen => g }
    val recDecl = util.getRecord(module, recName)
    var refinedFields: Map[String, Type] = Map.empty

    var envAcc = env

    genFieldOpt match {
      case Some(genField) =>
        val genNames = (recDecl.fMap.keySet -- namedFields.map(_.name)).toList.sorted
        for (genName <- genNames) {
          val fieldDecl = recDecl.fMap(genName)
          val (fTy, fEnv) = elabExpr(genField.value, envAcc, fieldDecl.tp)
          if (fieldDecl.refinable)
            refinedFields += (fieldDecl.name -> fTy)
          envAcc = fEnv
        }
      case None =>
        val undefinedFields = (recDecl.fMap.keySet -- namedFields.map(_.name)).toList.sorted
        for (uField <- undefinedFields) {
          val fieldDecl = recDecl.fMap(uField)
          val refinable = fieldDecl.refinable
          fieldDecl.defaultValue match {
            case None =>
              if (!subtype.subType(undefined, fieldDecl.tp))
                diagnosticsInfo.add(UndefinedField(rCreate.pos, recName, uField))
              if (refinable)
                refinedFields += (uField -> undefined)
            case Some(defVal) =>
              val (valTy, envVal) = elabExpr(defVal, env, fieldDecl.tp)
              if (refinable)
                refinedFields += (uField -> valTy)
              envAcc = envVal
          }
        }
    }

    for (namedField <- namedFields) {
      val fieldDecl = recDecl.fMap(namedField.name)
      val (fTy, fEnv) = elabExpr(namedField.value, envAcc, fieldDecl.tp)
      if (fieldDecl.refinable)
        refinedFields += (fieldDecl.name -> fTy)
      envAcc = fEnv
    }

    if (refinedFields.isEmpty) (recType, envAcc)
    else (RefinedRecordType(recType, refinedFields), envAcc)
  }

  private def elabRecordUpdate(rUpdate: RecordUpdate, env: Env): (Type, Env) = {
    val RecordUpdate(recExpr, recName, fields) = rUpdate
    val recType = RecordType(recName)(module)
    val recDecl = util.getRecord(module, recName)
    var refinedFields: Map[String, Type] = Map.empty
    val (refTy, refEnv) = elabExpr(recExpr, env, recType)
    if (recDecl.refinable) {
      val allRefinedFields = recDecl.fields.collect { case f if f.refinable => f.name }.toSet
      val keepFields = allRefinedFields -- fields.map(_.name)
      keepFields.foreach { fieldName =>
        val fieldTy = narrow.getRecordField(recDecl, refTy, fieldName)
        refinedFields += (fieldName -> fieldTy)
      }
    }
    var envAcc = refEnv
    for (field <- fields) {
      val fieldDecl = recDecl.fMap(field.name)
      val (fTy, fEnv) = elabExpr(field.value, envAcc, fieldDecl.tp)
      if (fieldDecl.refinable)
        refinedFields += (fieldDecl.name -> fTy)
      envAcc = fEnv
    }
    if (refinedFields.isEmpty) (recType, envAcc)
    else (RefinedRecordType(recType, refinedFields), envAcc)
  }

  private def elabNativeRecordCreate(rCreate: NativeRecordCreate, env: Env): (Type, Env) = {
    val NativeRecordCreate(id, fields) = rCreate
    util.getNativeRecord(id.module, id.name) match {
      case None =>
        diagnosticsInfo.add(UnboundNativeRecord(rCreate.pos, id.module, id.name))
        var envAcc = env
        for (f <- fields) {
          val (_, e1) = elabExpr(f.value, envAcc)
          envAcc = e1
        }
        (DynamicType, envAcc)
      case Some(decl) =>
        util.checkNativeRecordVisibility(id, decl, rCreate.pos)
        val providedNames = fields.map(_.name).toSet
        var envAcc = env
        for (f <- fields) {
          decl.fMap.get(f.name) match {
            case None =>
              diagnosticsInfo.add(UndefinedNativeRecordField(f.value.pos, id.module, id.name, f.name))
              val (_, e1) = elabExpr(f.value, envAcc)
              envAcc = e1
            case Some(fieldDecl) =>
              envAcc = elabExpr(f.value, envAcc, fieldDecl.tp)._2
          }
        }
        for (field <- decl.fields if !providedNames.contains(field.name) && field.defaultValue.isEmpty) {
          diagnosticsInfo.add(MissingRequiredNativeRecordField(rCreate.pos, id.module, id.name, field.name))
        }
        (NativeRecordType(id), envAcc)
    }
  }

  private def elabNativeRecordSelect(nrSelect: NativeRecordSelect, env: Env): (Type, Env) = {
    val NativeRecordSelect(recExpr, name, fieldName) = nrSelect
    name match {
      case NativeRecordName.Anon =>
        val (recTy, env1) = elabExpr(recExpr, env)
        val (concretes, anyDyn) = narrow.asNativeRecordTypes(recTy)
        if (!subtype.gradualSubType(recTy, AnyNativeRecordType)) {
          diagnosticsInfo.add(
            ExpectedSubtype(nrSelect.pos, nrSelect.expr, expected = AnyNativeRecordType, got = recTy)
          )
        }
        if (concretes.isEmpty) {
          (DynamicType, env1)
        } else {
          val resolved = concretes.toList.map(nrt => nrt -> util.getNativeRecord(nrt.id.module, nrt.id.name))
          resolved.foreach { case (nrt, declOpt) =>
            declOpt.foreach(decl => util.checkNativeRecordVisibility(nrt.id, decl, nrSelect.pos))
          }
          val fieldTys: List[Type] = resolved.flatMap {
            case (_, None)       => List(DynamicType)
            case (_, Some(decl)) => decl.fMap.get(fieldName).map(_.tp).toList
          }
          val declaredSomewhere = resolved.exists { case (_, declOpt) => declOpt.exists(_.fMap.contains(fieldName)) }
          if (!declaredSomewhere && !anyDyn)
            diagnosticsInfo.add(UndefinedAnonNativeRecordField(nrSelect.pos, fieldName))
          val resultTys = fieldTys ++ (if (anyDyn) List(DynamicType) else Nil)
          (if (resultTys.isEmpty) DynamicType else subtype.join(resultTys.toSet), env1)
        }
      case NativeRecordName.Qualified(id) =>
        util.getNativeRecord(id.module, id.name) match {
          case None =>
            diagnosticsInfo.add(UnboundNativeRecord(nrSelect.pos, id.module, id.name))
            val (_, env1) = elabExpr(recExpr, env)
            (DynamicType, env1)
          case Some(decl) =>
            util.checkNativeRecordVisibility(id, decl, nrSelect.pos)
            val (recTy, env1) = elabExpr(recExpr, env)
            decl.fMap.get(fieldName) match {
              case None =>
                diagnosticsInfo.add(
                  UndefinedNativeRecordField(nrSelect.pos, id.module, id.name, fieldName)
                )
                (DynamicType, env1)
              case Some(fieldDecl) =>
                nativeRecordFieldType(fieldDecl, recTy, nrSelect, env1)
            }
        }
    }
  }

  private def nativeRecordFieldType(
      fieldDecl: NativeRecField,
      recTy: Type,
      nrSelect: NativeRecordSelect,
      env: Env,
  ): (Type, Env) = {
    val expected = nrSelect.name match {
      case NativeRecordName.Qualified(id) => NativeRecordType(id)
      case NativeRecordName.Anon          => return (DynamicType, env)
    }
    if (!subtype.subType(recTy, expected))
      diagnosticsInfo.add(
        ExpectedSubtype(nrSelect.pos, nrSelect.expr, expected = expected, got = recTy)
      )
    (fieldDecl.tp, env)
  }

  private def elabNativeRecordUpdate(rUpdate: NativeRecordUpdate, env: Env): (Type, Env) = {
    val NativeRecordUpdate(recExpr, name, fields) = rUpdate
    name match {
      case NativeRecordName.Anon =>
        val (recTy, env1) = elabExpr(recExpr, env)
        val (concretes, anyDyn) = narrow.asNativeRecordTypes(recTy)
        if (!subtype.gradualSubType(recTy, AnyNativeRecordType)) {
          diagnosticsInfo.add(
            ExpectedSubtype(rUpdate.pos, rUpdate.expr, expected = AnyNativeRecordType, got = recTy)
          )
        }
        if (concretes.isEmpty) {
          var envAcc = env1
          for (f <- fields) {
            val (_, e1) = elabExpr(f.value, envAcc)
            envAcc = e1
          }
          (DynamicType, envAcc)
        } else {
          var envAcc = env1
          val fieldInferred: List[(String, Type, Expr)] = fields.map { f =>
            val (ty, e1) = elabExpr(f.value, envAcc)
            envAcc = e1
            (f.name, ty, f.value)
          }
          val declaredFieldNames: Set[String] =
            concretes.flatMap(nrt => util.getNativeRecord(nrt.id.module, nrt.id.name).toList.flatMap(_.fMap.keys))
          if (!anyDyn)
            for ((fName, _, fExpr) <- fieldInferred if !declaredFieldNames.contains(fName))
              diagnosticsInfo.add(UndefinedAnonNativeRecordField(fExpr.pos, fName))
          var resultTys: List[Type] = if (anyDyn) List(DynamicType) else Nil
          for (nrt <- concretes) {
            util.getNativeRecord(nrt.id.module, nrt.id.name) match {
              case None =>
                resultTys ::= DynamicType
              case Some(decl) =>
                util.checkNativeRecordVisibility(nrt.id, decl, rUpdate.pos)
                var allFieldsPresent = true
                for ((fName, inferredTy, fExpr) <- fieldInferred) {
                  decl.fMap.get(fName) match {
                    case None =>
                      allFieldsPresent = false
                    case Some(fieldDecl) =>
                      val expectedFieldTy = fieldDecl.tp
                      if (!subtype.subType(inferredTy, expectedFieldTy)) {
                        diagnosticsInfo.add(
                          ExpectedSubtype(fExpr.pos, fExpr, expected = expectedFieldTy, got = inferredTy)
                        )
                      }
                  }
                }
                if (allFieldsPresent) resultTys ::= nrt
            }
          }
          val res = if (resultTys.isEmpty) DynamicType else subtype.join(resultTys.toSet)
          (res, envAcc)
        }
      case NativeRecordName.Qualified(id) =>
        util.getNativeRecord(id.module, id.name) match {
          case None =>
            diagnosticsInfo.add(UnboundNativeRecord(rUpdate.pos, id.module, id.name))
            var envAcc = env
            val (_, e0) = elabExpr(recExpr, envAcc)
            envAcc = e0
            for (f <- fields) {
              val (_, e1) = elabExpr(f.value, envAcc)
              envAcc = e1
            }
            (DynamicType, envAcc)
          case Some(decl) =>
            util.checkNativeRecordVisibility(id, decl, rUpdate.pos)
            val (recTy, env1) = elabExpr(rUpdate.expr, env)
            val expectedRecTy = NativeRecordType(id)
            if (!subtype.subType(recTy, expectedRecTy))
              diagnosticsInfo.add(ExpectedSubtype(rUpdate.pos, rUpdate.expr, expected = expectedRecTy, got = recTy))
            var envAcc = env1
            for (f <- rUpdate.fields) {
              decl.fMap.get(f.name) match {
                case None =>
                  diagnosticsInfo.add(UndefinedNativeRecordField(f.value.pos, id.module, id.name, f.name))
                  val (_, e1) = elabExpr(f.value, envAcc)
                  envAcc = e1
                case Some(fieldDecl) =>
                  envAcc = elabExpr(f.value, envAcc, fieldDecl.tp)._2
              }
            }
            (NativeRecordType(id), envAcc)
        }
    }
  }
  private def elabQualifiers(qualifiers: List[Qualifier], env: Env): Env = {
    var envAcc = env
    qualifiers.foreach {
      case LGenerate(gPat, gExpr) =>
        val (gT, gEnv) = elabExpr(gExpr, envAcc, ListType(AnyType))
        val Some(ListType(gElemT)) = narrow.asListType(gT): @unchecked
        val (_, pEnv) = elabPat.elabPat(gPat, gElemT, gEnv)
        envAcc = pEnv
      case LGenerateStrict(gPat, gExpr) =>
        val (gT, gEnv) = elabExpr(gExpr, envAcc, ListType(AnyType))
        val Some(ListType(gElemT)) = narrow.asListType(gT): @unchecked
        val (_, pEnv) = elabPat.elabPat(gPat, gElemT, gEnv)
        envAcc = pEnv
      case BGenerate(gPat, gExpr) =>
        envAcc = elabExpr(gExpr, envAcc, BinaryType)._2
        val (_, pEnv) = elabPat.elabPat(gPat, BinaryType, envAcc)
        envAcc = pEnv
      case BGenerateStrict(gPat, gExpr) =>
        envAcc = elabExpr(gExpr, envAcc, BinaryType)._2
        val (_, pEnv) = elabPat.elabPat(gPat, BinaryType, envAcc)
        envAcc = pEnv
      case MGenerate(gkPat, gvPat, gExpr) =>
        val (gT, gEnv) = elabExpr(gExpr, envAcc, mapOrIterTy)
        val mapT = narrow.asMapOrIterTypes(gT)
        val kT = subtype.join(mapT.map(narrow.getKeyType))
        val vT = subtype.join(mapT.map(narrow.getValType))
        val (_, kPatEnv) = elabPat.elabPat(gkPat, kT, gEnv)
        val (_, vPatEnv) = elabPat.elabPat(gvPat, vT, kPatEnv)
        envAcc = vPatEnv
      case MGenerateStrict(gkPat, gvPat, gExpr) =>
        val (gT, gEnv) = elabExpr(gExpr, envAcc, mapOrIterTy)
        val mapT = narrow.asMapOrIterTypes(gT)
        val kT = subtype.join(mapT.map(narrow.getKeyType))
        val vT = subtype.join(mapT.map(narrow.getValType))
        val (_, kPatEnv) = elabPat.elabPat(gkPat, kT, gEnv)
        val (_, vPatEnv) = elabPat.elabPat(gvPat, vT, kPatEnv)
        envAcc = vPatEnv
      case Zip(generators) =>
        envAcc = elabQualifiers(generators, envAcc)
      // [X || ... (X = foo()) =/= undefined]
      case Filter(binOp @ BinOp(op, m @ Match(pv @ PatVar(v), _), exp2)) =>
        envAcc = elabExpr(m, envAcc)._2
        val fExpr = BinOp(op, Var(v)(pv.pos), exp2)(binOp.pos)
        Filters.asTest(fExpr).foreach { test =>
          envAcc = occurrence.testEnv(test, envAcc, result = true)
        }
        envAcc = elabExpr(fExpr, envAcc)._2
      case Filter(fExpr) =>
        Filters.asTest(fExpr).foreach { test =>
          envAcc = occurrence.testEnv(test, envAcc, result = true)
        }
        envAcc = elabExpr(fExpr, envAcc)._2
    }
    envAcc
  }

  private lazy val mapOrIterTy = UnionType(
    Set(MapType(Map(), AnyType, AnyType), RemoteType(RemoteId("maps", "iterator", 0), List()))
  )
}

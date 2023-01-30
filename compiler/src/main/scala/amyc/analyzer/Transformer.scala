package amyc.analyzer

import amyc.*
import amyc.core.*
import amyc.core.Signatures.*
import amyc.core.StdNames.binOp
import amyc.ast.{Identifier, NominalTreeModule as N, SymbolicTreeModule as S}

object Transformer {

  /**
    *
    * @param p
    * @param core .Context
    * @return
    */
  def transformProgram(p: N.Program)(using Context): S.Program =
    val symMods = for mod <- p.modules yield
      transformModule(mod).setPos(mod)
    S.Program(symMods).setPos(p)

  /**
    * 
    * @param mod
    * @param Context
    * @return
    */
  def transformModule(mod: N.ModuleDef)(using Context) =
    val N.ModuleDef(name, defs, optExpr) = mod
    val symName = symbols.getModule(name).getOrElse {
      reporter.fatal(s"Cannot find symbol for module $name")
    }
    val symDefs = for d <- defs yield transformDef(d, name)
    val symExpr = optExpr.map(transformExpr(_)(name, Scope.fresh, ctx))
    S.ModuleDef(symName, symDefs, symExpr)

  /**
    * 
    * @param tt
    * @param inModule
    * @param core.Context
    * @return
    */
  def transformType(tt: N.TypeTree, inModule: String)(using Context): S.Type = {
    tt.tpe match {
      case N.NoType =>
        reporter.fatal(s"Type tree $tt has a type of NoType")
      case N.IntType => S.IntType
      case N.BooleanType => S.BooleanType
      case N.StringType => S.StringType
      case N.UnitType => S.UnitType
      case N.ClassType(qn@N.QualifiedName(module, name)) =>
        symbols.getType(module getOrElse inModule, name) match {
          case Some(symbol) =>
            S.ClassType(symbol)
          case None =>
            reporter.fatal(s"Could not find type $qn", tt)
        }

      case N.FunctionType(params, rte) =>
        S.FunctionType(params.map(tt => S.TypeTree(transformType(tt, inModule))), S.TypeTree(transformType(rte, inModule)))
    }
  }

  /**
    * 
    * @param fd
    * @param module
    * @param core.Context
    * @return
    */
  def transformFunDef(fd: N.FunDef, module: String)(using Context): S.FunDef = {
    val N.FunDef(name, params, retType, body) = fd
    val Some((sym, sig)) = symbols.getFunction(module, name)

    params.groupBy(_.name).foreach { case (name, ps) =>
      if (ps.size > 1) {
        reporter.fatal(s"Two parameters named $name in function ${fd.name}", fd)
      }
    }

    val paramNames = params.map(_.name)

    val newParams = params zip sig.argTypes map { case (pd@N.ParamDef(name, tt), tpe) =>
      val s = Identifier.fresh(name)
      S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
    }

    val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

    S.FunDef(
      sym,
      newParams,
      S.TypeTree(sig.retType).setPos(retType),
      transformExpr(body)(module, Scope.fresh.withParams(paramsMap), ctx)
    ).setPos(fd)
  }

  /**
    * 
    * @param df
    * @param module
    * @param core.Context
    * @return
    */
  def transformDef(df: N.ClassOrFunDef, module: String)(using Context): S.ClassOrFunDef = {
    df match {
      case N.AbstractClassDef(name) =>
        S.AbstractClassDef(symbols.getType(module, name).get)
      case N.CaseClassDef(name, _, _) =>
        val Some((sym, sig)) = symbols.getConstructor(module, name)
        S.CaseClassDef(
          sym,
          sig.argTypes map S.TypeTree.apply,
          sig.parent
        )
      case fd: N.FunDef =>
        transformFunDef(fd, module)
    }
  }.setPos(df)

  /**
    * 
    * @param expr
    * @param module
    * @param names
    * @param context
    * @return
    */
  def transformExpr(expr: N.Expr)
                   (implicit module: String, scope : Scope, context: Context): S.Expr = {
    val res = expr match {
      case N.Variable(name) =>
        scope.resolve(name) match
          case Some(id) => S.Variable(id)
          case _ => reporter.fatal(s"Variable $name not found", expr)
      case N.FunRef(N.QualifiedName(module, name)) =>
        // TODO HR : get won't throw an exception; operation guaranteed to work
        val sym = symbols.getFunction(module.get, name)
          .getOrElse(reporter.fatal(s"Fix error message here"))
          ._1
        S.FunRef(sym)
      case N.IntLiteral(value) =>
        S.IntLiteral(value)
      case N.BooleanLiteral(value) =>
        S.BooleanLiteral(value)
      case N.StringLiteral(value) =>
        S.StringLiteral(value)
      case N.UnitLiteral() =>
        S.UnitLiteral()
      case N.InfixCall(lhs, op, rhs) =>
        S.InfixCall(transformExpr(lhs), binOp(op), transformExpr(rhs))
      case N.Not(e) =>
        S.Not(transformExpr(e))
      case N.Neg(e) =>
        S.Neg(transformExpr(e))
      case N.Call(qname, args) =>
        val owner = qname.module.getOrElse(module)
        val name = qname.name
        val entry =
          scope.resolve(qname.name) orElse {
            symbols.getConstructor(owner, name)
          } orElse {
            symbols.getFunction(owner, name)
          }
        entry match {
          case None =>
            reporter.fatal(s"Function or constructor $qname not found", expr)
          case Some((sym: Identifier, sig: Signature[_])) =>
            if (sig.argTypes.size != args.size) {
              reporter.fatal(s"Wrong number of arguments for function/constructor $qname", expr)
            }
            S.Call(sym, args.map(transformExpr(_)))
          case Some(sym: Identifier) =>
            S.Call(sym, args.map(transformExpr(_)))
          case _ =>
            reporter.fatal(s"NameAnalyzer resolved to $entry")
        }
      case N.Sequence(e1, e2) =>
        S.Sequence(transformExpr(e1), transformExpr(e2))
      case N.Let(vd, value, body) =>
        if (scope.isLocal(vd.name)) {
          reporter.fatal(s"Variable redefinition: ${vd.name}", vd)
        }
        if (scope.isParam(vd.name)) {
          reporter.warning(s"Local variable ${vd.name} shadows function parameter", vd)
        }
        val sym = Identifier.fresh(vd.name)
        val tpe = transformType(vd.tt, module)
        S.Let(
          S.ParamDef(sym, S.TypeTree(tpe)).setPos(vd),
          transformExpr(value),
          transformExpr(body)(module, scope.withLocal(vd.name, sym), ctx)
        )
      case N.Ite(cond, thenn, elze) =>
        S.Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze))
      case N.Match(scrut, cases) =>
        def transformCase(cse: N.MatchCase) = {
          val N.MatchCase(pat, rhs) = cse
          val (newPat, caseScope) = transformPattern(pat)
          S.MatchCase(newPat, transformExpr(rhs)(module, caseScope, ctx).setPos(rhs)).setPos(cse)
        }

        def transformPattern(pat: N.Pattern): (S.Pattern, Scope) = {
          val (newPat, newScope): (S.Pattern, Scope) = pat match {
            case N.WildcardPattern() =>
              (S.WildcardPattern(), scope)
            case N.IdPattern(name) =>
              if (scope.isLocal(name)) {
                reporter.fatal(s"Pattern identifier $name already defined", pat)
              }
              if (scope.isParam(name)) {
                reporter.warning("Suspicious shadowing by an Id Pattern", pat)
              }
              symbols.getConstructor(module, name) match {
                case Some((_, ConstrSig(Nil, _, _))) =>
                  reporter.warning(s"There is a nullary constructor in this module called '$name'. Did you mean '$name()'?", pat)
                case _ =>
              }
              val sym = Identifier.fresh(name)
              (S.IdPattern(sym), scope.withLocal(name, sym))
            case N.LiteralPattern(lit) =>
              (S.LiteralPattern(transformExpr(lit).asInstanceOf[S.Literal[_]]), scope)
            case N.CaseClassPattern(constr, args) =>
              val (sym, sig) = symbols
                .getConstructor(constr.module.getOrElse(module), constr.name)
                .getOrElse {
                  reporter.fatal(s"Constructor $constr not found", pat)
                }
              if (sig.argTypes.size != args.size) {
                reporter.fatal(s"Wrong number of args for constructor $constr", pat)
              }
              val (newPatts, moreLocals0) = (args map transformPattern).unzip
              val moreLocals = if moreLocals0.nonEmpty then
                // TODO HR : This check here should be refactored (inefficient)
                moreLocals0.toSet.flatMap(_.locals.map(identity)).groupBy(_._1).foreach { case (name, pairs) =>
                  if (pairs.size > 1) {
                    reporter.fatal(s"Multiple definitions of $name in pattern", pat)
                  }
                }
                moreLocals0.reduce(Scope.combine)
              else
                scope
              (S.CaseClassPattern(sym, newPatts), moreLocals)
          }
          (newPat.setPos(pat), newScope)
        }

        S.Match(transformExpr(scrut), cases map transformCase)
      case N.Error(msg) =>
        S.Error(transformExpr(msg))
    }
    res.setPos(expr)
  }

}

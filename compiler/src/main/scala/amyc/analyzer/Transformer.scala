package amyc
package analyzer

import core.*
import core.Symbols.*
import core.Types.*
import core.StdDefinitions.*
import ast.{NominalTreeModule as N, SymbolicTreeModule as S}

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
    val symExpr = optExpr.map(transformExpr(_)(name, ctx.scope(symName), ctx))
    S.ModuleDef(symName, symDefs, symExpr)

  /**
    * 
    * @param tt
    * @param inModule
    * @param core.Context
    * @return
    */
  def transformType(scope: Scope)(tt: N.TypeTree, inModule: String)(using Context): S.TypeTree = {
    tt match
      case N.FunctionTypeTree(params, rte) =>
        S.FunctionTypeTree(params.map(transformType(scope)(_, inModule)), transformType(scope)(rte, inModule))
      case N.ClassTypeTree(N.QualifiedName(None, name)) =>
        name match
          case "Unit" => S.ClassTypeTree(stdDef.UnitType)
          case "Boolean" => S.ClassTypeTree(stdDef.BooleanType)
          case "Int" => S.ClassTypeTree(stdDef.IntType)
          case "String" => S.ClassTypeTree(stdDef.StringType)
          case _ =>
            scope.resolve(name).map(sym => sym.asInstanceOf[ParameterSymbol].tpe) orElse {
              symbols.getType(inModule, name).map(S.ClassTypeTree.apply)
            } getOrElse :
                reporter.fatal(s"Could not find type $name", tt)
      case N.ClassTypeTree(qn@N.QualifiedName(pre, name)) =>
        symbols.getType(pre getOrElse inModule, name) map S.ClassTypeTree.apply getOrElse{
          reporter.fatal(s"Could not find type $qn", tt)
        }
      case N.TTypeTree(tpe) =>
        S.TTypeTree(tpe)
  }

  /**
    * 
    * @param fd
    * @param module
    * @param core.Context
    * @return
    */
  def transformFunDef(fd: N.FunDef, module: String)(using Context): S.FunDef = {
    val N.FunDef(name, tparams, vparams, retType, body) = fd
    val sym = symbols.function(module, name)

    // Check that the value parameter names are unique
    for case (name, ps) <- vparams.groupBy(_.name)
        if ps.size > 1
    do
      reporter.fatal(s"Function ${fd.name} has ${ps.size} parameters named $name", fd)

    // Check that the type parameter names are unique
    for case (name, ps) <- tparams.groupBy(_.name)
        if ps.size > 1
    do
      reporter.fatal(s"Function ${fd.name} has ${ps.size} type parameters named $name", fd)

    val symtparams = tparams zip sym.tparams map :
      case (tpd: N.TypeParamDef, sym) =>
        S.TypeParamDef(sym).setPos(tpd)

    val symvparams = vparams zip sym.vparams map :
      case (pd@N.ValParamDef(_, tt), sym) =>
        S.ValParamDef(sym, sym.tpe.setPos(tt)).setPos(pd)

    val scope = Scope.fresh
      .withTParams(sym.tparams.map(s => (s.name, s)).toMap)
      .withVParams(sym.vparams.map(s => (s.name, s)).toMap)

    S.FunDef(sym, symtparams, symvparams, sym.rte.setPos(retType), transformExpr(body)(module, scope, ctx)).setPos(fd)
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
      case N.CaseClassDef(name, params, _) =>
        val sym = symbols.constructor(module, name)
        val newParams = params zip sym.vparams map {
          case (pd@N.ValParamDef(_, tt), sym) =>
            S.ValParamDef(sym, sym.tpe.setPos(tt)).setPos(pd)
        }
        S.CaseClassDef(sym, newParams, sym.parent)
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
        // desugar infix calls to function calls
        transformExpr(N.Call(N.QualifiedName(Some("unnamed"), op), Nil, lhs :: rhs :: Nil))
      case N.Not(e) =>
        S.Not(transformExpr(e))
      case N.Neg(e) =>
        S.Neg(transformExpr(e))
      case N.Call(qname, targs, vargs) =>
        val owner = qname.module.getOrElse(module)
        val name = qname.name
        val entry = scope.resolve(qname.name) orElse {
            symbols.getConstructor(owner, name)
          } orElse {
            symbols.getFunction(owner, name)
          }
        entry match {
          case None =>
            reporter.fatal(s"Function or constructor $qname not found", expr)
          case Some(sym: ApplicationSymbol) =>
            if (sym.vparams.size != vargs.size) {
              reporter.fatal(s"Wrong number of arguments for function/constructor $qname", expr)
            }
            S.Call(sym, targs.map(transformType(scope)(_, module)), vargs.map(transformExpr))
          case Some(sym: Symbol) =>
            S.Call(sym, targs.map(transformType(scope)(_, module)), vargs.map(transformExpr(_)))
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
        val sym = LocalSymbol(Identifier.fresh(vd.name))
        val tpe = transformType(scope)(vd.tt, module)
        S.Let(
          S.ValParamDef(sym, tpe).setPos(vd),
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
                case Some(sym : ConstructorSymbol) if sym.vparams.isEmpty =>
                  reporter.warning(s"There is a nullary constructor in this module called '$name'. Did you mean '$name()'?", pat)
                case _ =>
              }
              val sym = LocalSymbol(Identifier.fresh(name))
              (S.IdPattern(sym), scope.withLocal(name, sym))
            case N.LiteralPattern(lit) =>
              (S.LiteralPattern(transformExpr(lit).asInstanceOf[S.Literal[_]]), scope)
            case N.CaseClassPattern(constr, args) =>
              val sym = symbols
                .getConstructor(constr.module.getOrElse(module), constr.name)
                .getOrElse {
                  reporter.fatal(s"Constructor $constr not found", pat)
                }
              if (sym.vparams.size != args.size) {
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
      case N.EmptyExpr() =>
        S.EmptyExpr()
    }
    res.setPos(expr)
  }

}

package amyc
package analyzer

import amyc.{core, *}
import amyc.utils.*
import amyc.ast.{Identifier, NominalTreeModule as N, SymbolicTreeModule as S}

// Name analyzer for Amy
// Takes a nominal program (names are plain string, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates symbol table.
object NameAnalyzer extends Pipeline[N.Program, S.Program] {

  override val name = "NameAnalyzer"
  override def run(p: N.Program)(using core.Context): S.Program = {

    // Step 0: Initialize symbol table
    ctx.withSymTable(new SymbolTable)

    // Step 1: Add modules
    registerModules(p)

    // Step 2: Check name uniqueness in modules
    for mod <- p.modules do
      checkModuleConsistency(mod)

    // Step 3: Discover types
    for mod <- p.modules do
      registerTypes(mod)

    // Step 4: Discover type constructors
    for {
      m <- p.modules
      cc@N.CaseClassDef(name, fields, parent) <- m.defs
    } {
      val argTypes = fields map (tt => transformType(tt, m.name))
      val retType = symbols.getType(m.name, parent).getOrElse{
        reporter.fatal(s"Parent class $parent not found", cc)
      }
      symbols.addConstructor(m.name, name, argTypes, retType)
    }

    // Step 5: Discover functions signatures.
    for {
      m <- p.modules
      N.FunDef(name, params, retType1, _) <- m.defs
    } {
      val argTypes = params map (p => transformType(p.tt, m.name))
      val retType2 = transformType(retType1, m.name)
      symbols.addFunction(m.name, name, argTypes, retType2)
    }

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions
    transformProgram(p)

  }

  def registerModules(prog: N.Program)(using core.Context) =
    val modNames = prog.modules.groupBy(_.name)
    for (name, modules) <- modNames do
      if (modules.size > 1) {
        reporter.fatal(s"Two modules named $name in program", modules.head.position)
      }
    for mod <- modNames.keys.toList do
      symbols.addModule(mod)

  def registerTypes(mod: N.ModuleDef)(using core.Context) =
     for N.AbstractClassDef(name) <- mod.defs do
       symbols.addType(mod.name, name)

  def checkModuleConsistency(mod: N.ModuleDef)(using core.Context) =
     val names = mod.defs.groupBy(_.name)
     for (name, defs) <- names do
      if (defs.size > 1) {
         reporter.fatal(s"Two definitions named $name in module ${mod.name}", defs.head)
      }

  // ==============================================================================================
  // =================================== TRANSFORM METHODS ========================================
  // ==============================================================================================

  def transformFunDef(fd: N.FunDef, module: String)(using core.Context): S.FunDef = {
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
      transformExpr(body)(module, (paramsMap, Map()), ctx)
    ).setPos(fd)
  }

  def transformDef(df: N.ClassOrFunDef, module: String)(using core.Context): S.ClassOrFunDef = {
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

  def transformExpr(expr: N.Expr)
                   (implicit module: String, names: (Map[String, Identifier], Map[String, Identifier]), context: core.Context): S.Expr = {
    val (params, locals) = names
    val res = expr match {
      case N.Variable(name) =>
        // Local variables shadow parameters!
        val sym = locals.get(name) orElse {
          params.get(name)
        } orElse {
          symbols.getFunction(module, name)
        }
        sym match
          case Some(id: Identifier) => S.Variable(id)
          case Some((id:Identifier, _)) => S.Variable(id)
          case _ => reporter.fatal(s"Variable $name not found", expr)
      case N.IntLiteral(value) =>
        S.IntLiteral(value)
      case N.BooleanLiteral(value) =>
        S.BooleanLiteral(value)
      case N.StringLiteral(value) =>
        S.StringLiteral(value)
      case N.UnitLiteral() =>
        S.UnitLiteral()
      case N.Plus(lhs, rhs) =>
        S.Plus(transformExpr(lhs), transformExpr(rhs))
      case N.Minus(lhs, rhs) =>
        S.Minus(transformExpr(lhs), transformExpr(rhs))
      case N.Times(lhs, rhs) =>
        S.Times(transformExpr(lhs), transformExpr(rhs))
      case N.Div(lhs, rhs) =>
        S.Div(transformExpr(lhs), transformExpr(rhs))
      case N.Mod(lhs, rhs) =>
        S.Mod(transformExpr(lhs), transformExpr(rhs))
      case N.LessThan(lhs, rhs) =>
        S.LessThan(transformExpr(lhs), transformExpr(rhs))
      case N.LessEquals(lhs, rhs) =>
        S.LessEquals(transformExpr(lhs), transformExpr(rhs))
      case N.And(lhs, rhs) =>
        S.And(transformExpr(lhs), transformExpr(rhs))
      case N.Or(lhs, rhs) =>
        S.Or(transformExpr(lhs), transformExpr(rhs))
      case N.Equals(lhs, rhs) =>
        S.Equals(transformExpr(lhs), transformExpr(rhs))
      case N.Concat(lhs, rhs) =>
        S.Concat(transformExpr(lhs), transformExpr(rhs))
      case N.Not(e) =>
        S.Not(transformExpr(e))
      case N.Neg(e) =>
        S.Neg(transformExpr(e))
      case N.Call(qname, args) =>
        val owner = qname.module.getOrElse(module)
        val name = qname.name
        val entry =
          locals.get(qname.name).orElse{
            params.get(qname.name)
          }.orElse{
            symbols.getConstructor(owner, name)
          }.orElse{
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
        if (locals.contains(vd.name)) {
          reporter.fatal(s"Variable redefinition: ${vd.name}", vd)
        }
        if (params.contains(vd.name)) {
          reporter.warning(s"Local variable ${vd.name} shadows function parameter", vd)
        }
        val sym = Identifier.fresh(vd.name)
        val tpe = transformType(vd.tt, module)
        S.Let(
          S.ParamDef(sym, S.TypeTree(tpe)).setPos(vd),
          transformExpr(value),
          transformExpr(body)(module, (params, locals + (vd.name -> sym)), ctx)
        )
      case N.Ite(cond, thenn, elze) =>
        S.Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze))
      case N.Match(scrut, cases) =>
        def transformCase(cse: N.MatchCase) = {
          val N.MatchCase(pat, rhs) = cse
          val (newPat, moreLocals) = transformPattern(pat)
          S.MatchCase(newPat, transformExpr(rhs)(module, (params, locals ++ moreLocals), ctx).setPos(rhs)).setPos(cse)
        }

        def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = {
          val (newPat, newNames): (S.Pattern, List[(String, Identifier)]) = pat match {
            case N.WildcardPattern() =>
              (S.WildcardPattern(), List())
            case N.IdPattern(name) =>
              if (locals.contains(name)) {
                reporter.fatal(s"Pattern identifier $name already defined", pat)
              }
              if (params.contains(name)) {
                reporter.warning("Suspicious shadowing by an Id Pattern", pat)
              }
              symbols.getConstructor(module, name) match {
                case Some((_, ConstrSig(Nil, _, _))) =>
                  reporter.warning(s"There is a nullary constructor in this module called '$name'. Did you mean '$name()'?", pat)
                case _ =>
              }
              val sym = Identifier.fresh(name)
              (S.IdPattern(sym), List(name -> sym))
            case N.LiteralPattern(lit) =>
              (S.LiteralPattern(transformExpr(lit).asInstanceOf[S.Literal[_]]), List())
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
              val moreLocals = moreLocals0.flatten
              moreLocals.groupBy(_._1).foreach { case (name, pairs) =>
                if (pairs.size > 1) {
                  reporter.fatal(s"Multiple definitions of $name in pattern", pat)
                }
              }
              (S.CaseClassPattern(sym, newPatts), moreLocals)
          }
          (newPat.setPos(pat), newNames)
        }

        S.Match(transformExpr(scrut), cases map transformCase)

      case N.Error(msg) =>
        S.Error(transformExpr(msg))
    }
    res.setPos(expr)
  }

  def transformType(tt: N.TypeTree, inModule: String)(using core.Context): S.Type = {
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

  def transformProgram(p: N.Program)(using core.Context): S.Program =
    val symMods = for mod <- p.modules yield
      transformModule(mod).setPos(mod)
    S.Program(symMods).setPos(p)

  def transformModule(mod: N.ModuleDef)(using core.Context) =
    val N.ModuleDef(name, defs, optExpr) = mod
    val symName = symbols.getModule(name).getOrElse{
      reporter.fatal(s"Cannot find symbol for module $name")
    }
    val symDefs = for d <- defs yield transformDef(d, name)
    val symExpr = optExpr.map(transformExpr(_)(name, (Map(), Map()), ctx))
    S.ModuleDef(symName, symDefs, symExpr)


}

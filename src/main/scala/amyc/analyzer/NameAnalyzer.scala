package amyc
package analyzer

import amyc.utils._
import amyc.ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}

// Name analyzer for Amy
// Takes a nominal program (names are plain string, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates symbol table.
object NameAnalyzer extends Pipeline[N.Program, (S.Program, SymbolTable)] {
  def run(p: N.Program)(using Context): (S.Program, SymbolTable) = {

    // Step 0: Initialize symbol table
    val table = new SymbolTable

    // Step 1: Add modules
    registerModules(p, table)

    // Step 2: Check name uniqueness in modules
    for mod <- p.modules do
      checkModuleConsistency(mod)

    // Step 3: Discover types
    for mod <- p.modules do
      registerTypes(mod, table)

    // Step 4: Discover type constructors
    for {
      m <- p.modules
      cc@N.CaseClassDef(name, fields, parent) <- m.defs
    } {
      val argTypes = fields map (tt => transformType(tt, m.name, table))
      val retType = table.getType(m.name, parent).getOrElse{
        reporter.fatal(s"Parent class $parent not found", cc)
      }
      table.addConstructor(m.name, name, argTypes, retType)
    }

    // Step 5: Discover functions signatures.
    for {
      m <- p.modules
      N.FunDef(name, params, retType1, _) <- m.defs
    } {
      val argTypes = params map (p => transformType(p.tt, m.name, table))
      val retType2 = transformType(retType1, m.name, table)
      table.addFunction(m.name, name, argTypes, retType2)
    }

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions
    val symProgram = transformProgram(p, table)

    (symProgram, table)

  }

  def registerModules(prog: N.Program, table: SymbolTable)(using Context) =
    val modNames = prog.modules.groupBy(_.name)
    for (name, modules) <- modNames do
      if (modules.size > 1) {
        reporter.fatal(s"Two modules named $name in program", modules.head.position)
      }
    for mod <- modNames.keys.toList do
     table.addModule(mod)

  def registerTypes(mod: N.ModuleDef, table: SymbolTable)(using Context) =
     for N.AbstractClassDef(name) <- mod.defs do
       table.addType(mod.name, name)

  def checkModuleConsistency(mod: N.ModuleDef)(using Context) =
     val names = mod.defs.groupBy(_.name)
     for (name, defs) <- names do
      if (defs.size > 1) {
         reporter.fatal(s"Two definitions named $name in module ${mod.name}", defs.head)
      }

  // ==============================================================================================
  // =================================== TRANSFORM METHODS ========================================
  // ==============================================================================================

  def transformFunDef(fd: N.FunDef, module: String, table: SymbolTable)(using Context): S.FunDef = {
    val N.FunDef(name, params, retType, body) = fd
    val Some((sym, sig)) = table.getFunction(module, name)

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
      transformExpr(body, table)(module, (paramsMap, Map()), ctx)
    ).setPos(fd)
  }

  def transformDef(df: N.ClassOrFunDef, module: String, table: SymbolTable)(using Context): S.ClassOrFunDef = {
    df match {
      case N.AbstractClassDef(name) =>
        S.AbstractClassDef(table.getType(module, name).get)
      case N.CaseClassDef(name, _, _) =>
        val Some((sym, sig)) = table.getConstructor(module, name)
        S.CaseClassDef(
          sym,
          sig.argTypes map S.TypeTree.apply,
          sig.parent
        )
      case fd: N.FunDef =>
        transformFunDef(fd, module, table)
    }
  }.setPos(df)

  def transformExpr(expr: N.Expr, table: SymbolTable)
                   (implicit module: String, names: (Map[String, Identifier], Map[String, Identifier]), context: Context): S.Expr = {
    val (params, locals) = names
    val res = expr match {
      case N.Variable(name) =>
        S.Variable(
          locals.getOrElse(name, // Local variables shadow parameters!
            params.getOrElse(name,
              reporter.fatal(s"Variable $name not found", expr))))
      case N.IntLiteral(value) =>
        S.IntLiteral(value)
      case N.BooleanLiteral(value) =>
        S.BooleanLiteral(value)
      case N.StringLiteral(value) =>
        S.StringLiteral(value)
      case N.UnitLiteral() =>
        S.UnitLiteral()
      case N.Plus(lhs, rhs) =>
        S.Plus(transformExpr(lhs, table), transformExpr(rhs, table))
      case N.Minus(lhs, rhs) =>
        S.Minus(transformExpr(lhs, table), transformExpr(rhs, table))
      case N.Times(lhs, rhs) =>
        S.Times(transformExpr(lhs, table), transformExpr(rhs, table))
      case N.Div(lhs, rhs) =>
        S.Div(transformExpr(lhs, table), transformExpr(rhs, table))
      case N.Mod(lhs, rhs) =>
        S.Mod(transformExpr(lhs, table), transformExpr(rhs, table))
      case N.LessThan(lhs, rhs) =>
        S.LessThan(transformExpr(lhs, table), transformExpr(rhs, table))
      case N.LessEquals(lhs, rhs) =>
        S.LessEquals(transformExpr(lhs, table), transformExpr(rhs, table))
      case N.And(lhs, rhs) =>
        S.And(transformExpr(lhs, table), transformExpr(rhs, table))
      case N.Or(lhs, rhs) =>
        S.Or(transformExpr(lhs, table), transformExpr(rhs, table))
      case N.Equals(lhs, rhs) =>
        S.Equals(transformExpr(lhs, table), transformExpr(rhs, table))
      case N.Concat(lhs, rhs) =>
        S.Concat(transformExpr(lhs, table), transformExpr(rhs, table))
      case N.Not(e) =>
        S.Not(transformExpr(e, table))
      case N.Neg(e) =>
        S.Neg(transformExpr(e, table))
      case N.Call(qname, args) =>
        val owner = qname.module.getOrElse(module)
        val name = qname.name
        val entry = table.getConstructor(owner, name).orElse(table.getFunction(owner, name))
        entry match {
          case None =>
            reporter.fatal(s"Function or constructor $qname not found", expr)
          case Some((sym, sig)) =>
            if (sig.argTypes.size != args.size) {
              reporter.fatal(s"Wrong number of arguments for function/constructor $qname", expr)
            }
            S.Call(sym, args.map(transformExpr(_,table)))
        }
      case N.Sequence(e1, e2) =>
        S.Sequence(transformExpr(e1, table), transformExpr(e2, table))
      case N.Let(vd, value, body) =>
        if (locals.contains(vd.name)) {
          reporter.fatal(s"Variable redefinition: ${vd.name}", vd)
        }
        if (params.contains(vd.name)) {
          reporter.warning(s"Local variable ${vd.name} shadows function parameter", vd)
        }
        val sym = Identifier.fresh(vd.name)
        val tpe = transformType(vd.tt, module, table)
        S.Let(
          S.ParamDef(sym, S.TypeTree(tpe)).setPos(vd),
          transformExpr(value, table),
          transformExpr(body, table)(module, (params, locals + (vd.name -> sym)), ctx)
        )
      case N.Ite(cond, thenn, elze) =>
        S.Ite(transformExpr(cond, table), transformExpr(thenn, table), transformExpr(elze, table))
      case N.Match(scrut, cases) =>
        def transformCase(cse: N.MatchCase) = {
          val N.MatchCase(pat, rhs) = cse
          val (newPat, moreLocals) = transformPattern(pat)
          S.MatchCase(newPat, transformExpr(rhs, table)(module, (params, locals ++ moreLocals), ctx).setPos(rhs)).setPos(cse)
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
              table.getConstructor(module, name) match {
                case Some((_, ConstrSig(Nil, _, _))) =>
                  reporter.warning(s"There is a nullary constructor in this module called '$name'. Did you mean '$name()'?", pat)
                case _ =>
              }
              val sym = Identifier.fresh(name)
              (S.IdPattern(sym), List(name -> sym))
            case N.LiteralPattern(lit) =>
              (S.LiteralPattern(transformExpr(lit, table).asInstanceOf[S.Literal[_]]), List())
            case N.CaseClassPattern(constr, args) =>
              val (sym, sig) = table
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

        S.Match(transformExpr(scrut, table), cases map transformCase)

      case N.Error(msg) =>
        S.Error(transformExpr(msg, table))
    }
    res.setPos(expr)
  }

  def transformType(tt: N.TypeTree, inModule: String, table: SymbolTable)(using Context): S.Type = {
    tt.tpe match {
      case N.NoType =>
        reporter.fatal(s"Type tree $tt has a type of NoType")
      case N.IntType => S.IntType
      case N.BooleanType => S.BooleanType
      case N.StringType => S.StringType
      case N.UnitType => S.UnitType
      case N.ClassType(qn@N.QualifiedName(module, name)) =>
        table.getType(module getOrElse inModule, name) match {
          case Some(symbol) =>
            S.ClassType(symbol)
          case None =>
            reporter.fatal(s"Could not find type $qn", tt)
        }

      case N.FunctionType(params, rte) =>
        S.FunctionType(params.map(tt => S.TypeTree(transformType(tt, inModule, table))), S.TypeTree(transformType(rte, inModule, table)))
    }
  }

  def transformProgram(p: N.Program, table: SymbolTable)(using Context): S.Program =
    val symMods = for mod <- p.modules yield
      transformModule(mod, table).setPos(mod)
    S.Program(symMods).setPos(p)

  def transformModule(mod: N.ModuleDef, table: SymbolTable)(using Context) =
    val N.ModuleDef(name, defs, optExpr) = mod
    val symName = table.getModule(name).getOrElse{
      reporter.fatal(s"Cannot find symbol for module $name")
    }
    val symDefs = for d <- defs yield transformDef(d, name, table)
    val symExpr = optExpr.map(transformExpr(_, table)(name, (Map(), Map()), ctx))
    S.ModuleDef(symName, symDefs, symExpr)


}

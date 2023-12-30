package amyc.typer

import amyc.core.StdDefinitions.*
import amyc.core.StdTypes.*
import amyc.core.Types.*
import amyc.core.{Context, Identifier}
import amyc.ast.SymbolicTreeModule
import amyc.utils.*
import amyc.ast.SymbolicTreeModule.*
import amyc.core.Symbols.{ConstructorSymbol, FunctionSymbol}
import amyc.{ctx, reporter}
import amyc.utils.Pipeline

object TypeInferer extends Pipeline[Program, Program]{

  override val name = "TypeInferer"

  def constraintsFunDef(fd : FunDef)(using Context) : List[(Type, Type)] =
    val FunDef(_, params, rte_tpe, body) = fd
    rte_tpe.withType(ctx.tpe(rte_tpe))
    val env = params.map {
      case df@ValParamDef(name, tt) =>
        tt.withType(ctx.tpe(tt))
        name.id -> df.withType(ctx.tpe(tt)).tpe
    }.toMap
    // we will need to infer the type of the result. If we assume that the type is correct
    // This will always type check.
    solveConstraints(genConstraints(body, TypeVariable.fresh())(env, ctx))

  def constraintsExpr(expr : Expr)(using Context) : List[(Type, Type)] =
    solveConstraints(genConstraints(expr, TypeVariable.fresh())(Map(), ctx))

  override def run(program: Program)(using Context): Program = {
    // We will first type check each function defined in a module
    for mod <- program.modules
        case CaseClassDef(_, args, _) <- mod.defs
        param <- args
    do
      param match
        case vparam@ValParamDef(_, tt) =>
          tt.withType(ctx.tpe(tt))
          vparam.withType(ctx.tpe(tt))
        case _ =>

    val inferred1 = for
      mod <- program.modules
      case fd@FunDef(_, _, _, _) <- mod.defs
    yield constraintsFunDef(fd)

    // Type-check expression if present. We allow the result to be of an arbitrary type by
    // passing a fresh (and therefore unconstrained) type variable as the expected type.
    val inferred2 = for
      mod <- program.modules
      expr <- mod.optExpr
    yield constraintsExpr(expr)

    // Add all type variables binding to the context to be solved in the next phase
    ctx.tv.addAll(inferred1.flatten.toMap)
    ctx.tv.addAll(inferred2.flatten.toMap)

    program
  }


  private case class Constraint(found: Type, expected: Type, pos: Position)

  // Given a list of constraints `constraints`, replace every occurence of type variable
  //  with id `from` by type `to`.
  private def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] =
    // Do a single substitution.
    def subst(tpe: Type, from: Int, to: Type): Type =
      tpe match
        case TypeVariable(`from`) => to
        case other => other

    constraints map {
      case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
    }


  // Generates typing constraints for an expression `e` with a given expected type.
  // The environment `env` contains all currently available bindings (you will have to
  //  extend these, e.g., to account for local variables).
  // Returns a list of constraints among types. These will later be solved via unification.
  private def genConstraints(e: Expr, expected: Type)
                            (implicit env: Map[Identifier, Type], ctx: Context): List[Constraint] = {

    // This helper returns a list of a single constraint recording the type
    //  that we found (or generated) for the current expression `e`
    def topLevelConstraint(found: Type): List[Constraint] =
      List(Constraint(found, expected, e.position))
    val defs = stdDef
    e match {
      // ===================== Type Check Variables =============================
      case Variable(name) =>
        val symbol = env.get(name.id)
        symbol match
          case Some(tpe: Type) =>
            e.withType(tpe)
            topLevelConstraint(tpe)
          case _ =>
            e.withType(ErrorType)
            reporter.error(s"Cannot find symbol $name")
            Nil
      case FunRef(id : FunctionSymbol) =>
        e.withType(ctx.tpe(FunctionTypeTree(id.param.map(_.tpe), id.rte)))
        Nil
      // ===================== Type Check Literals ==============================
      case IntLiteral(_) =>
        e.withType(stdType.IntType)
        topLevelConstraint(stdType.IntType)
      case BooleanLiteral(_) =>
        e.withType(stdType.BooleanType)
        topLevelConstraint(stdType.BooleanType)
      case StringLiteral(_) =>
        e.withType(stdType.StringType)
        topLevelConstraint(stdType.StringType)
      case UnitLiteral() =>
        e.withType(stdType.UnitType)
        topLevelConstraint(stdType.UnitType)
      // ========================== Type Check Binary Operators =========================
      case InfixCall(_, op, _) =>
        e.withType(ErrorType)
        reporter.error(s"Cannot infer type of infix call $e (it should have been desugared)")
        Nil
      // ============================== Type Check Unary Operators ==============================
      case Not(expr) =>
        e.withType(stdType.BooleanType)
        topLevelConstraint(stdType.BooleanType) ::: genConstraints(expr, stdType.BooleanType)
      case Neg(expr) =>
        e.withType(stdType.IntType)
        topLevelConstraint(stdType.IntType) ::: genConstraints(expr, stdType.IntType)
      // ============================== Type Check Applications =================================
      case Call(defs.binop_==, args) =>
        val tv = TypeVariable.fresh()
        e.withType(stdType.BooleanType)
        args(0).withType(tv)
        args(1).withType(tv)
        topLevelConstraint(stdType.BooleanType) ::: genConstraints(args(0), tv) ::: genConstraints(args(1), tv)
      case Call(qname: ConstructorSymbol, args) =>
          val argsConstraint = (args zip qname.param) flatMap {
            (expr, pd) => expr.withType(ctx.tpe(pd.tpe)); genConstraints(expr, ctx.tpe(pd.tpe))
          }
          e.withType(ctx.tpe(qname.rte))
          topLevelConstraint(e.tpe) ::: argsConstraint
      case Call(qname: FunctionSymbol, args) =>
        val fn = env.get(qname.id) orElse {
          Some(qname)
        }.asInstanceOf[Option[FunctionType | FunctionSymbol]]
         fn match
          case Some(f : FunctionSymbol) =>
            val argsConstraint = (args zip f.param) flatMap {
              (expr, param) => expr.withType(ctx.tpe(param.tpe)); genConstraints(expr, expr.tpe)
            }
            e.withType(ctx.tpe(f.rte))
            topLevelConstraint(e.tpe) ::: argsConstraint
          case Some(FunctionType(args_tpe, rte_tpe)) =>
            val argsConstraint = (args zip args_tpe) flatMap {
              (expr, tpe) => expr.withType(tpe); genConstraints(expr, tpe)
            }
            e.withType(rte_tpe)
            topLevelConstraint(rte_tpe) ::: argsConstraint
      // ================================ Type Check Sequences ==================================
      case Sequence(e1, e2) =>
        e.withType(expected)
        genConstraints(e1, TypeVariable.fresh()) ::: genConstraints(e2, expected)
      //  ========================== Type Check Variable Definitions ============================
      case Let(df, value, body) =>
        val tv = TypeVariable.fresh()
        df.tt.withType(ctx.tpe(df.tt))
        e.withType(expected)
        df.withType(df.tt.tpe)
        genConstraints(value, tv) ::: genConstraints(body, expected)(using env + (df.name.id -> df.tt.tpe), ctx)
      // =========================== Type Check Conditions ======================================
      case Ite(cond, thenn, elze) =>
        val generic = TypeVariable.fresh()
        e.withType(generic)
        topLevelConstraint(generic) ::: genConstraints(cond, stdType.BooleanType) ::: genConstraints(thenn, generic) ::: genConstraints(elze, generic)
      // =============================== Type Check Pattern Matching ============================
      case Match(scrut, cases) =>
        // Returns additional constraints from within the pattern with all bindings
        // from identifiers to types for names bound in the pattern.
        // (This is analogous to `transformPattern` in NameAnalyzer.)
        def handlePattern(pat: Pattern, scrutExpected: Type): (List[Constraint], Map[Identifier, Type]) = {
          pat match
            case WildcardPattern() =>
              pat.withType(WildCardType)
              (Nil, Map.empty)
            case IdPattern(name) =>
              pat.withType(scrutExpected)
              (Nil, Map(name.id -> scrutExpected))
            case LiteralPattern(lit) =>
              val tv = TypeVariable.fresh()
              pat.withType(tv)
              (genConstraints(lit, tv), Map.empty)
            case CaseClassPattern(constr: ConstructorSymbol, args) =>
              pat.withType(ClassType(constr.id))
              val pat_tpe = args zip constr.param
              for (p, pd) <- pat_tpe do p.withType(ctx.tpe(pd.tpe))
              val a = pat_tpe.foldLeft((List[Constraint](), Map.empty[Identifier, Type])) {
                case (acc, (pat, pd)) =>
                  val handle = handlePattern(pat, ctx.tpe(pd.tpe))
                  (acc._1 ::: handle._1, acc._2 ++ handle._2)
              }
              (Constraint(ctx.tpe(constr.rte), scrutExpected, pat.position) :: a._1, a._2)
        }

        def handleCase(cse: MatchCase, scrutExpected: Type, rt: Type): List[Constraint] = {
          cse.withType(rt)
          val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
          patConstraints ::: genConstraints(cse.expr, rt)(env ++ moreEnv, ctx)
        }

        val st = TypeVariable.fresh()
        val mul = MultiTypeVariable()
        e.withType(mul)
        genConstraints(scrut, st) ++ cases.flatMap(cse => {
          val tv = TypeVariable.fresh()
          mul.add(tv)
          handleCase(cse, st, tv)
        }) ++ topLevelConstraint(mul)

      // ============================= Type Check Errors =====================================
      case Error(msg) =>
        val tv = TypeVariable.fresh()
        e.withType(tv)
        genConstraints(msg, stdType.StringType) ::: topLevelConstraint(tv)
      case EmptyExpr() =>
        e.withType(expected)
        Nil
      // ============================= DEFAULT ====================================
      case expr =>
        ctx.reporter.fatal(s"Cannot type check tree $expr of type ${expr.getClass.getTypeName}")
    }
  }


  // Solve the given set of typing constraints and report errors
  //  using `ctx.reporter.error` if they are not satisfiable.
  // We consider a set of constraints to be satisfiable exactly if they unify.


    private def solveConstraints(constraints: List[Constraint])(using Context): List[(Type, Type)] = {
      constraints match {
        case Nil => Nil
        case Constraint(found, expected, pos) :: more =>
          (found, expected) match
            case (TypeVariable(id1), TypeVariable(id2)) if id1 == id2 =>
              solveConstraints(more)
            case (TypeVariable(id1), t2@TypeVariable(_)) =>
              (found -> t2) :: solveConstraints(subst_*(constraints, id1, t2))
            case (TypeVariable(_), _) =>
              solveConstraints(Constraint(expected, found, pos) :: more)
            case (type1, TypeVariable(i)) =>
              val newList = subst_*(constraints, i, type1)
              (expected -> type1) :: solveConstraints(newList)
            case (lhs_tpe, rhs_tpe) if lhs_tpe == rhs_tpe =>
              solveConstraints(more)
            case (lhs_tpe, rhs_tpe) if lhs_tpe != rhs_tpe =>
              solveConstraints(more)
            case _ =>
              ctx.reporter.fatal(s"TypeChecker, found= $found & expected= $expected", pos)
      }
  }

}

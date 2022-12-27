package amyc.typer

import amyc.analyzer.{ConstrSig, FunSig, SymbolTable}
import amyc.ast.Identifier
import amyc.utils.*
import amyc.ast.SymbolicTreeModule.*
import amyc.{ctx, symbols}
import amyc.utils.Pipeline

object TypeInferer extends Pipeline[Program, Program]{

  override def run(program: Program)(using Context) = {
    // We will first type check each function defined in a module
    val inferred1 = for
      mod <- program.modules
      FunDef(_, params, _, body) <- mod.defs
    yield
      val env = params.map {
        case df@ParamDef(name, tt) =>
          df.withType(tt.tpe)
          name -> tt.tpe
      }.toMap
      // we will need to infer the type of the result. If we assume that the type is correct
      // This will always type check.
      solveConstraints(genConstraints(body, TypeVariable.fresh())(env, symbols, ctx))

    // Type-check expression if present. We allow the result to be of an arbitrary type by
    // passing a fresh (and therefore unconstrained) type variable as the expected type.
    val inferred2 = for
      mod <- program.modules
      expr <- mod.optExpr
    yield
      solveConstraints(genConstraints(expr, TypeVariable.fresh())(Map(), symbols, ctx))

    ctx.tv.addAll(inferred1.flatten.toMap)
    ctx.tv.addAll(inferred2.flatten.toMap)

    program
  }


  private case class Constraint(found: Type, expected: Type, pos: Position)

  // Given a list of constraints `constraints`, replace every occurence of type variable
  //  with id `from` by type `to`.
  private def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
    // Do a single substitution.
    def subst(tpe: Type, from: Int, to: Type): Type = {
      tpe match {
        case TypeVariable(`from`) => to
        case other => other
      }
    }

    constraints map { case Constraint(found, expected, pos) =>
      Constraint(subst(found, from, to), subst(expected, from, to), pos)
    }
  }

  // Generates typing constraints for an expression `e` with a given expected type.
  // The environment `env` contains all currently available bindings (you will have to
  //  extend these, e.g., to account for local variables).
  // Returns a list of constraints among types. These will later be solved via unification.
  private def genConstraints(e: Expr, expected: Type)
                            (implicit env: Map[Identifier, Type], table: SymbolTable, ctx: Context): List[Constraint] = {

    // This helper returns a list of a single constraint recording the type
    //  that we found (or generated) for the current expression `e`
    def topLevelConstraint(found: Type): List[Constraint] =
      List(Constraint(found, expected, e.position))

    e match {
      // ===================== Type Check Variables =============================
      case Variable(name) =>
        env.get(name) match
          case Some(tpe) =>
            e.withType(tpe)
            topLevelConstraint(tpe)
          case None =>
            e.withType(ErrorType)
            ctx.reporter.error(s"Cannot find symbol $name")
            Nil
      // ===================== Type Check Literals ==============================
      case IntLiteral(_) =>
        e.withType(IntType)
        topLevelConstraint(IntType)
      case BooleanLiteral(_) =>
        e.withType(BooleanType)
        topLevelConstraint(BooleanType)
      case StringLiteral(_) =>
        e.withType(StringType)
        topLevelConstraint(StringType)
      case UnitLiteral() =>
        e.withType(UnitType)
        topLevelConstraint(UnitType)
      // ========================== Type Check Binary Operators =========================
      case Plus(lhs, rhs) =>
        e.withType(IntType)
        topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
      case Minus(lhs, rhs) =>
        e.withType(IntType)
        topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
      case Times(lhs, rhs) =>
        e.withType(IntType)
        topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
      case Div(lhs, rhs) =>
        e.withType(IntType)
        topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
      case Mod(lhs, rhs) =>
        e.withType(IntType)
        topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
      case LessThan(lhs, rhs) =>
        e.withType(BooleanType)
        topLevelConstraint(BooleanType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
      case LessEquals(lhs, rhs) =>
        e.withType(BooleanType)
        topLevelConstraint(BooleanType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
      case And(lhs, rhs) =>
        e.withType(BooleanType)
        topLevelConstraint(BooleanType) ::: genConstraints(lhs, BooleanType) ::: genConstraints(rhs, BooleanType)
      case Or(lhs, rhs) =>
        e.withType(BooleanType)
        topLevelConstraint(BooleanType) ::: genConstraints(lhs, BooleanType) ::: genConstraints(rhs, BooleanType)
      case Equals(lhs, rhs) =>
        val generic = TypeVariable.fresh()
        e.withType(BooleanType)
        topLevelConstraint(BooleanType) ::: genConstraints(lhs, generic) ::: genConstraints(rhs, generic)
      case Concat(lhs, rhs) =>
        e.withType(StringType)
        topLevelConstraint(StringType) ::: genConstraints(lhs, StringType) ::: genConstraints(rhs, StringType)
      // ============================== Type Check Unary Operators ==============================
      case Not(expr) =>
        e.withType(BooleanType)
        topLevelConstraint(BooleanType) ::: genConstraints(expr, BooleanType)
      case Neg(expr) =>
        e.withType(IntType)
        topLevelConstraint(IntType) ::: genConstraints(expr, IntType)
      // ============================== Type Check Applications =================================
      case Call(qname, args) =>
        // WARNING BY HR : An Application can either be a call to a constructor of a function
        val constructor = table.getConstructor(qname)
        constructor match
          case Some(constr@ConstrSig(args_tpe, _, _)) =>
            val argsConstraint = (args zip args_tpe) flatMap { (expr, tpe) => expr.withType(tpe); genConstraints(expr, tpe) }
            e.withType(constr.retType)
            topLevelConstraint(constr.retType) ::: argsConstraint
          case None =>
            table.getFunction(qname) match
              case Some(FunSig(args_tpe, rte_tpe, _)) =>
                val argsConstraint = (args zip args_tpe) flatMap { (expr, tpe) => expr.withType(tpe); genConstraints(expr, tpe) }
                e.withType(rte_tpe)
                topLevelConstraint(rte_tpe) ::: argsConstraint
              case None =>
                ctx.reporter.error(s"unknown symbol $qname")
                Nil
      // ================================ Type Check Sequences ==================================
      case Sequence(e1, e2) =>
        e.withType(expected)
        genConstraints(e1, TypeVariable.fresh()) ::: genConstraints(e2, expected)
      //  ========================== Type Check Variable Definitions ============================
      case Let(df, value, body) =>
        val tv = TypeVariable.fresh()
        e.withType(expected)
        df.withType(df.tt.tpe)
        genConstraints(value, tv) ::: genConstraints(body, expected)(using env + (df.name -> df.tt.tpe), table, ctx)
      // =========================== Type Check Conditions ======================================
      case Ite(cond, thenn, elze) =>
        val generic = TypeVariable.fresh()
        e.withType(generic)
        topLevelConstraint(generic) ::: genConstraints(cond, BooleanType) ::: genConstraints(thenn, generic) ::: genConstraints(elze, generic)
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
              (Nil, Map(name -> scrutExpected))
            case LiteralPattern(lit) =>
              val tv = TypeVariable.fresh()
              pat.withType(tv)
              (genConstraints(lit, tv), Map.empty)
            case CaseClassPattern(constr, args) =>
              pat.withType(ClassType(constr))
              val constructor = table.getConstructor(constr) match
                case Some(c) => c
                case None => ctx.reporter.fatal(s"Constructor type was not found $constr")
              val pat_tpe = args zip constructor.argTypes
              for (p, t) <- pat_tpe do p.withType(t)
              val a = pat_tpe.foldLeft((List[Constraint](), Map.empty[Identifier, Type])) {
                case (acc, (pat, tpe)) =>
                  val handle = handlePattern(pat, tpe)
                  (acc._1 ::: handle._1, acc._2 ++ handle._2)
              }
              (Constraint(constructor.retType, scrutExpected, pat.position) :: a._1, a._2)
        }

        def handleCase(cse: MatchCase, scrutExpected: Type, rt: Type): List[Constraint] = {
          cse.withType(rt)
          val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
          patConstraints ::: genConstraints(cse.expr, rt)(env ++ moreEnv, table, ctx)
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
        genConstraints(msg, StringType) ::: topLevelConstraint(tv)
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

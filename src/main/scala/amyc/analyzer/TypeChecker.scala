package amyc
package analyzer

import amyc.utils.*
import amyc.ast.SymbolicTreeModule.*
import amyc.ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(v: (Program, SymbolTable))(using Context): (Program, SymbolTable) = {
    val (program, table) = v
    // We will first type check each function defined in a module
    for
      mod <- program.modules
      f@FunDef(name, params, retType, body) <- mod.defs
    do
      val env = params.map { case ParamDef(name, tt) => name -> tt.tpe }.toMap
      val cst = solveConstraints(genConstraints(body, retType.tpe)(env, table, ctx))
      //ctx.reporter.warning(s"$cst for ${mod.name}::$name")

    // Type-check expression if present. We allow the result to be of an arbitrary type by
    // passing a fresh (and therefore unconstrained) type variable as the expected type.
    for
      mod <- program.modules
      expr <- mod.optExpr
    do
      val tv = TypeVariable.fresh()
      val cst = solveConstraints(genConstraints(expr, tv)(Map(), table, ctx))
      //ctx.reporter.warning(s"$cst for ${mod.name}")

    v
  }

  private case class Constraint(found: Type, expected: Type, pos: Position)


  // Represents a type variable.
  // It extends Type, but it is meant only for internal type checker use,
  //  since no Amy value can have such type.
  private case class TypeVariable private(id: Int) extends Type

  private object TypeVariable {
    private val c = new UniqueCounter[Unit]
    def fresh(): TypeVariable = TypeVariable(c.next(()))
  }

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
        e.withType(expected)
        e.withType(df.tt.tpe)
        genConstraints(value, df.tt.tpe) ::: genConstraints(body, expected)(using env + (df.name -> df.tt.tpe), table, ctx)
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
              (Nil, Map.empty)
            case IdPattern(name) =>
              (Nil, Map(name -> scrutExpected))
            case LiteralPattern(lit) =>
              (genConstraints(lit, scrutExpected), Map.empty)
            case CaseClassPattern(constr, args) =>
              val constructor = table.getConstructor(constr) match
                case Some(c) => c
                case None => ctx.reporter.fatal(s"Constructor type was not found $constr")
              val pat_tpe = args zip constructor.argTypes
              val a = pat_tpe.foldLeft((List[Constraint](), Map.empty[Identifier, Type])) {
                case (acc, (pat, tpe)) =>
                  val handle = handlePattern(pat, tpe)
                  (acc._1 ::: handle._1, acc._2 ++ handle._2)
              }
              (Constraint(constructor.retType, scrutExpected, pat.position) :: a._1, a._2)
        }

        def handleCase(cse: MatchCase, scrutExpected: Type, rt: Type): List[Constraint] = {
          val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
          patConstraints ::: genConstraints(cse.expr, rt)(env ++ moreEnv, table, ctx)
        }

        val st = TypeVariable.fresh()
        val rt = TypeVariable.fresh()
        e.withType(rt)
        genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st, rt)) ++ topLevelConstraint(rt)

      // ============================= Type Check Errors =====================================
      case Error(msg) =>
        e.withType(expected)
        genConstraints(msg, StringType)
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
            ctx.reporter.error(s"{Type error} found $lhs_tpe instead of $rhs_tpe", pos)
            solveConstraints(more)
          case _ | null => // TODO HR: null needs to be removed when Explicit nulls will be released ;-)
            ctx.reporter.fatal(s"TypeChecker, found= $found & expected= $expected", pos)
    }
  }

}

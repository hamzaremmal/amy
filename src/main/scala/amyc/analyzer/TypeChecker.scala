package amyc
package analyzer

import amyc.utils._
import amyc.ast.SymbolicTreeModule._
import amyc.ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private(id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]

      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {

      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))

      e match {
        case IntLiteral(_) =>
          topLevelConstraint(IntType)
        case BooleanLiteral(_) =>
          topLevelConstraint(BooleanType)
        case StringLiteral(_) =>
          topLevelConstraint(StringType)
        case UnitLiteral() =>
          topLevelConstraint(UnitType)
        case Plus(lhs, rhs) =>
          topLevelConstraint(IntType) :::
            genConstraints(lhs, IntType) :::
            genConstraints(rhs, IntType)
        case Minus(lhs, rhs) =>
          topLevelConstraint(IntType)
          genConstraints(lhs, IntType) :::
            genConstraints(rhs, IntType)
        case Times(lhs, rhs) =>
          topLevelConstraint(IntType) :::
            genConstraints(lhs, IntType) :::
            genConstraints(rhs, IntType)
        case Div(lhs, rhs) =>
          topLevelConstraint(IntType) :::
            genConstraints(lhs, IntType) :::
            genConstraints(rhs, IntType)
        case Mod(lhs, rhs) =>
          topLevelConstraint(IntType) :::
            genConstraints(lhs, IntType) :::
            genConstraints(rhs, IntType)
        case Neg(expr) =>
          topLevelConstraint(IntType)
          genConstraints(expr, IntType)
        case Not(expr) =>
          topLevelConstraint(BooleanType)
          genConstraints(expr, BooleanType)
        case LessThan(lhs, rhs) =>
          topLevelConstraint(BooleanType) :::
            genConstraints(lhs, IntType) :::
            genConstraints(rhs, IntType)
        case LessEquals(lhs, rhs) =>
          topLevelConstraint(BooleanType) :::
            genConstraints(lhs, IntType) :::
            genConstraints(rhs, IntType)
        case And(lhs, rhs) =>
          topLevelConstraint(BooleanType) :::
            genConstraints(lhs, BooleanType) :::
            genConstraints(rhs, BooleanType)
        case Or(lhs, rhs) =>
          topLevelConstraint(BooleanType) :::
            genConstraints(lhs, BooleanType) :::
            genConstraints(rhs, BooleanType)
        case Equals(lhs, rhs) =>
          val generic = TypeVariable.fresh()
          topLevelConstraint(BooleanType) :::
            genConstraints(lhs, generic) :::
            genConstraints(rhs, generic)

        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
          (List[Constraint], Map[Identifier, Type]) = {
            ??? // TODO
          }

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            ??? // TODO
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))

        case e =>
          println(s"this is missing ${e}")
          ??? // TODO: Implement the remaining cases
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
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

    // Solve the given set of typing constraints and report errors
    //  using `ctx.reporter.error` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      println(constraints)
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          (found, expected) match
            case (t1: TypeVariable, t2: TypeVariable) =>
              if t1.id == t2.id then
                solveConstraints(more)
              else
                val newList = subst_*(constraints, t1.id, t2)
                solveConstraints(newList)
            case (TypeVariable(_), _) =>
              solveConstraints(Constraint(expected, found, pos) :: more)
            case (type1, TypeVariable(i)) =>
              val newList = subst_*(constraints, i, type1)
              solveConstraints(newList)
            case (type1, type2) =>
              if type1 == type2 then
                solveConstraints(more)
            case _ | null =>
              error(s"Type error,found $found instead of $expected", pos)
              solveConstraints(more)
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map { case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}

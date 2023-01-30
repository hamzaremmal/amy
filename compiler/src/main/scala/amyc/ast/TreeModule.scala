package amyc.ast

import amyc.core.Context
import amyc.core.Types.Type
import amyc.utils.printers.{NominalPrinter, Printer, SymbolicPrinter}
import amyc.utils.{Positioned, UniqueCounter}

/* A polymorphic module containing definitions of Amy trees.
 *
 * This trait represents either nominal trees (where names have not been resolved)
 * or symbolic trees (where names/qualified names) have been resolved to unique identifiers.
 * This is done by having two type fields within the module,
 * which will be instantiated differently by the two different modules.
 *
 */

trait TreeModule { self =>
  /* Represents the type for the name for this tree module.
   * (It will be either a plain string, or a unique symbol)
   */
  type Name

  // Represents a name within an module
  type QualifiedName

  // Common ancestor for all trees
  trait Tree extends Positioned {

    private var tpe_ : Type = NoType

    def tpe: Type = tpe_

    final def withType(tpe: Type) =
      tpe_ = tpe
      this
  }

  // Expressions
  trait Expr extends Tree

  // Variables
  case class Variable(name: Name) extends Expr
  // Function reference (Module::name)
  case class FunRef(name: QualifiedName) extends Expr

  // Literals
  trait Literal[+T](value: T) extends Expr
  case class IntLiteral(value: Int) extends Literal(value)
  case class BooleanLiteral(value: Boolean) extends Literal(value)
  case class StringLiteral(value: String) extends Literal(value)
  case class UnitLiteral() extends Literal(())

  // Binary operators
  case class InfixCall(lhs: Expr, op: Name, rhs: Expr) extends Expr

  // Unary operators
  case class Not(e: Expr) extends Expr
  case class Neg(e: Expr) extends Expr

  // Function/constructor call
  case class Call(qname: QualifiedName, args: List[Expr]) extends Expr
  // The ; operator
  case class Sequence(e1: Expr, e2: Expr) extends Expr
  // Local variable definition
  case class Let(df: ParamDef, value: Expr, body: Expr) extends Expr
  // If-then-else
  case class Ite(cond: Expr, thenn: Expr, elze: Expr) extends Expr
  // Pattern matching
  case class Match(scrut: Expr, cases: List[MatchCase]) extends Expr {
    require(cases.nonEmpty)
  }
  // Represents a computational error; prints its message, then exits
  case class Error(msg: Expr) extends Expr

  // Cases and patterns for Match expressions
  case class MatchCase(pat: Pattern, expr: Expr) extends Tree

  abstract class Pattern extends Tree
  case class WildcardPattern() extends Pattern // _
  case class IdPattern(name: Name) extends Pattern // x
  case class LiteralPattern[+T](lit: Literal[T]) extends Pattern // 42, true
  case class CaseClassPattern(constr: QualifiedName, args: List[Pattern]) extends Pattern // C(arg1, arg2)

  // All is wrapped in a program
  case class Program(modules: List[ModuleDef]) extends Tree

  // Definitions
  trait Definition extends Tree { val name: Name }
  case class ModuleDef(name: Name, defs: List[ClassOrFunDef], optExpr: Option[Expr]) extends Definition
  trait ClassOrFunDef extends Definition
  case class FunDef(name: Name, params: List[ParamDef], retType: TypeTree, body: Expr) extends ClassOrFunDef {
    def paramNames = params.map(_.name)
  }
  case class AbstractClassDef(name: Name) extends ClassOrFunDef
  case class CaseClassDef(name: Name, fields: List[TypeTree], parent: Name) extends ClassOrFunDef
  case class ParamDef(name: Name, tt: TypeTree) extends Definition

  // A wrapper for types that is also a Tree (i.e. has a position)
  // This here should not have a type
  trait TypeTree extends Tree

  /* */
  case class ClassType(qname: QualifiedName) extends TypeTree

  /* */
  case class FunctionType(args: List[TypeTree], rte: TypeTree) extends TypeTree



}

/* A module containing trees where the names have not been resolved.
 * Instantiates Name to String and QualifiedName to a pair of Strings
 * representing (module, name) (where module is optional)
 */
object NominalTreeModule extends TreeModule {
  type Name = String
  case class QualifiedName(module: Option[String], name: String)
}

/* A module containing trees where the names have been resolved to unique identifiers.
 * Both Name and ModuleName are instantiated to Identifier.
 */
object SymbolicTreeModule extends TreeModule {
  type Name = Identifier
  type QualifiedName = Identifier


}


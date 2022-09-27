package amyc.ast

import amyc.utils.Positioned

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

  // A printer that knows how to print trees in this module.
  // The modules will instantiate it as appropriate
  val printer: Printer { val treeModule: self.type }

  // Common ancestor for all trees
  trait Tree extends Positioned {
    override def toString: String = printer(this)
  }

  // Expressions
  trait Expr extends Tree

  // Variables
  case class Variable(name: Name) extends Expr

  // Literals
  trait Literal[+T] extends Expr { val value: T }
  case class IntLiteral(value: Int) extends Literal[Int]
  case class BooleanLiteral(value: Boolean) extends Literal[Boolean]
  case class StringLiteral(value: String) extends Literal[String]
  case class UnitLiteral() extends Literal[Unit] { val value: Unit = () }

  // Binary operators
  case class Plus(lhs: Expr, rhs: Expr) extends Expr
  case class Minus(lhs: Expr, rhs: Expr) extends Expr
  case class Times(lhs: Expr, rhs: Expr) extends Expr
  case class Div(lhs: Expr, rhs: Expr) extends Expr
  case class Mod(lhs: Expr, rhs: Expr) extends Expr
  case class LessThan(lhs: Expr, rhs: Expr) extends Expr
  case class LessEquals(lhs: Expr, rhs: Expr) extends Expr
  case class And(lhs: Expr, rhs: Expr) extends Expr
  case class Or(lhs: Expr, rhs: Expr) extends Expr
  case class Equals(lhs: Expr, rhs: Expr) extends Expr
  case class Concat(lhs: Expr, rhs: Expr) extends Expr

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

  // Types
  trait Type
  case object IntType extends Type {
    override def toString: String = "Int"
  }
  case object BooleanType extends Type {
    override def toString: String = "Boolean"
  }
  case object StringType extends Type {
    override def toString: String = "String"
  }
  case object UnitType extends Type {
    override def toString: String = "Unit"
  }
  case class ClassType(qname: QualifiedName) extends Type {
    override def toString: String = printer.printQName(qname)(false).print
  }

  // A wrapper for types that is also a Tree (i.e. has a position)
  case class TypeTree(tpe: Type) extends Tree

  // All is wrapped in a program
  case class Program(modules: List[ModuleDef]) extends Tree
}

/* A module containing trees where the names have not been resolved.
 * Instantiates Name to String and QualifiedName to a pair of Strings
 * representing (module, name) (where module is optional)
 */
object NominalTreeModule extends TreeModule {
  type Name = String
  case class QualifiedName(module: Option[String], name: String) {
    override def toString: String = printer.printQName(this)(false).print
  }
  val printer = NominalPrinter
}

/* A module containing trees where the names have been resolved to unique identifiers.
 * Both Name and ModuleName are instantiated to Identifier.
 */
object SymbolicTreeModule extends TreeModule {
  type Name = Identifier
  type QualifiedName = Identifier
  val printer = SymbolicPrinter
}


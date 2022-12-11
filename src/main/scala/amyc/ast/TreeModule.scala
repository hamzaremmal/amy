package amyc.ast

import amyc.analyzer.NameAnalyzer.reporter
import amyc.utils.{Context, Positioned, UniqueCounter}

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

    private var tpe_ : Type = NoType

    def tpe: Type = tpe_

    final def withType(tpe: Type) =
      tpe_ = tpe
      this
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
  case class TypeTree(override val tpe: Type) extends Tree

  // Types
  trait Type {

    // Check subtyping
    infix def <:< (lhs: Type) : Boolean =
      isBottomType || lhs.isBottomType || this =:= lhs

      // Check the equality of 2 types
    infix def =:= (lhs: Type) : Boolean =
      (this, lhs) match
        case (ClassType(i), ClassType(j)) if i == j => true
        case _ => this == lhs

    def isBottomType : Boolean =
      this =:= BottomType

  }

  // To mark the fact that a tree has no type
  // This usually means that the type should be inferred
  case object NoType extends Type

  // This type is used to fill the type information of a tree
  // when an error happens at the typer level
  case object ErrorType extends Type

  case object WildCardType extends Type

  // Bottom type (Should not be defined like this)
  // This type will be removed in the future
  case object BottomType extends Type

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

  case class OrType(lhs : Type, rhs: Type) extends Type

  // Represents a type variable.
  // It extends Type, but it is meant only for internal type checker use,
  //  since no Amy value can have such type.
  case class TypeVariable private(id: Int) extends Type

  object TypeVariable {
    private val c = new UniqueCounter[Unit]

    def fresh(): TypeVariable = TypeVariable(c.next(()))
  }

  case class MultiTypeVariable() extends Type {
    private var t: List[Type] = Nil


    override def toString: String =
      t.toString()

    def add(tpe: Type) =
      t ::= tpe

    def resolve : Type = {
      def consistentacc(xs: List[Type], acc: Type): Type =
        xs match
          case y :: ys if y <:< acc => consistentacc(ys, y)
          case y :: ys => consistentacc(ys, OrType(acc, y))
          case Nil => acc
      consistentacc(t, BottomType)
    }

    def bind(bindings : Map[Type, Type])(using Context) : MultiTypeVariable =
      def bindt(tpe: Type): Type =
        tpe match
          case TypeVariable(_) =>
            val b = bindings.getOrElse(tpe,
              reporter.fatal(s"$tpe has leaked from the bindings while inferring the type ($bindings)"))
            bindt(b)
          case t@MultiTypeVariable() =>
            t.bind(bindings).resolve
          case _ =>  tpe

      t = for tp <- t yield bindt(tp)
      this

  }

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


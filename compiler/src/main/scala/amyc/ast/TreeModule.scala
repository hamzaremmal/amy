package amyc.ast

import amyc.core.{Context, Identifier}
import amyc.core.Types.{NoType, Type}
import amyc.utils.printers.{NominalPrinter, Printer, SymbolicPrinter}
import amyc.utils.{Positioned, UniqueCounter}

/**
  * A polymorphic module containing definitions of Amy trees.
  *
  * This trait represents either nominal trees (where names have not been resolved)
  * or symbolic trees (where names/qualified names) have been resolved to unique identifiers.
  * This is done by having two type fields within the module,
  * which will be instantiated differently by the two different modules.
  */
trait TreeModule :

  /** Represents the type for the name for this tree module. */
  type Name

  /** Represents a name within an module */
  type QualifiedName

  /** Base type for all the AST nodes */
  sealed trait Tree extends Positioned :
    private var tpe_ : Type = NoType
    def tpe: Type = tpe_
    final def withType(tpe: Type) =
      tpe_ = tpe
      this

  /** Root of the AST, we only have one Program with multiple modules */
  case class Program(modules: List[ModuleDef]) extends Tree

  // ==============================================================================================
  // ===================================== DEFINITIONS ============================================
  // ==============================================================================================

  /** TODO */
  sealed trait Definition extends Tree :
    val name: Name

  case class ModuleDef(name: Name, defs: List[ClassOrFunDef], optExpr: Option[Expr]) extends Definition
  trait ClassOrFunDef extends Definition
  case class FunDef(name: Name, params: List[ParamDef], retType: TypeTree, body: Expr) extends ClassOrFunDef :
    def paramNames = params.map(_.name)
  case class AbstractClassDef(name: Name) extends ClassOrFunDef
  case class CaseClassDef(name: Name, fields: List[TypeTree], parent: Name) extends ClassOrFunDef
  case class ParamDef(name: Name, tt: TypeTree) extends Definition

  // ==============================================================================================
  // ======================================= EXPRESSIONS ==========================================
  // ==============================================================================================

  /** Base type for all the expressions in Amy */
  sealed trait Expr extends Tree

  /** Represents an access to a variable */
  case class Variable(name: Name) extends Expr

  /** Function references (Module::function_name) */
  case class FunRef(name: QualifiedName) extends Expr

  /** Represent a Literal (Int, Boolean, String or Unit) */
  sealed trait Literal[+T](value: T) extends Expr
  case class IntLiteral(value: Int) extends Literal(value)
  case class BooleanLiteral(value: Boolean) extends Literal(value)
  case class StringLiteral(value: String) extends Literal(value)
  case class UnitLiteral() extends Literal(())

  /**
    * Represents an Infix call to a function
    * For now, we only use it for operators.
    *
    * It should be desugared to a Call(op, List(lhs, rhs))
    */
  case class InfixCall(lhs: Expr, op: Name, rhs: Expr) extends Expr

  /** Function/constructor call */
  case class Call(qname: QualifiedName, args: List[Expr]) extends Expr

  /** Represents a !(expr) */
  case class Not(e: Expr) extends Expr
  /** Represents a -(expr) */
  case class Neg(e: Expr) extends Expr

  /** The ; operator */
  case class Sequence(e1: Expr, e2: Expr) extends Expr
  /** Local variable definition */
  case class Let(df: ParamDef, value: Expr, body: Expr) extends Expr

  /** If-then-else */
  case class Ite(cond: Expr, thenn: Expr, elze: Expr) extends Expr

  /** Pattern matching */
  case class Match(scrut: Expr, cases: List[MatchCase]) extends Expr:
    require(cases.nonEmpty)

  /** Cases and patterns for Match expressions */
  case class MatchCase(pat: Pattern, expr: Expr) extends Tree

  /** Represents a computational error; prints its message, then exits */
  case class Error(msg: Expr) extends Expr

  // ==============================================================================================
  // ===================================== PATTERNS ===============================================
  // ==============================================================================================

  /** Base Type to represent a pattern */
  sealed abstract class Pattern extends Tree

  case class WildcardPattern() extends Pattern // _
  case class IdPattern(name: Name) extends Pattern // x
  case class LiteralPattern[+T](lit: Literal[T]) extends Pattern // 42, true
  case class CaseClassPattern(constr: QualifiedName, args: List[Pattern]) extends Pattern // C(arg1, arg2)

  // ==============================================================================================
  // ======================================== TYPES ===============================================
  // ==============================================================================================

  /** Base Type to represent type node */
  trait TypeTree extends Tree

  /** Represent a ClassType such as O.Option or String */
  case class ClassTypeTree(qname: QualifiedName) extends TypeTree

  /** Represents a FunctionType such as (String) => String */
  case class FunctionTypeTree(args: List[TypeTree], rte: TypeTree) extends TypeTree

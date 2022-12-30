package amyc.interpreter

import amyc.ast.Identifier
import amyc.ast.SymbolicTreeModule.Expr
import amyc.*
import amyc.core.Context
import amyc.interpreter.BooleanValue.*
import amyc.interpreter.Value.{val2Boolean, val2Int, val2String}

import scala.annotation.targetName
import scala.language.implicitConversions

// A class that represents a value computed by interpreting an expression
abstract class Value {
  final def asInt(using Context): Int = val2Int(this).i

  final def asBoolean(using Context): Boolean = val2Boolean(this).b

  final def asString(using Context): String = val2String(this).s

  final infix def ==(value: Value): BooleanValue =
    // TODO HR : Handle if the effective type is not the same in lhs and rhs
    // TODO HR : by doing (_, ...) and (..., _) in some of the match cases
    (this, value) match
      case (StringValue(_), StringValue(_)) => BooleanValue(this eq value)
      case (CaseClassValue(_, _), CaseClassValue(_, _)) => BooleanValue(this eq value)
      case (IntValue(a), IntValue(b)) => BooleanValue(a == b)
      case (a: BooleanValue, b: BooleanValue) => BooleanValue(a == b)
      case (a, b) => BooleanValue(a eq b)

  override def toString: String = this match {
    case IntValue(i) => i.toString
    case b:BooleanValue => b.toString
    case StringValue(s) => s
    case UnitValue => "()"
    case CaseClassValue(constructor, args) =>
      constructor.name + "(" + args.map(_.toString).mkString(", ") + ")"
  }
}

object Value {

  implicit def val2Int(v: Value)(using Context): IntValue =
    v match
      case a: IntValue => a
      case _ => reporter.fatal(s"")  // TODO HR : Fix error message

  implicit def val2Boolean(v: Value)(using Context): BooleanValue =
    v match
      case a: BooleanValue => a
      case _ => reporter.fatal(s"") // TODO HR : Fix error message

  implicit def val2String(v: Value)(using Context): StringValue =
    v match
      case a: StringValue => a
      case _ => reporter.fatal(s"") // TODO HR : Fix error message

}

case class IntValue(i: Int) extends Value{

  def unary_-(using Context): IntValue = IntValue(-i)
  @targetName("plus")  infix def +(v: IntValue)(using Context): IntValue = IntValue(i + v.i)
  @targetName("minus") infix def -(v: IntValue)(using Context): IntValue = IntValue(i - v.i)
  @targetName("times") infix def *(v: IntValue)(using Context): IntValue = IntValue(i * v.i)
  @targetName("div") infix def /(v: IntValue)(using Context): IntValue =
    if v.i != 0 then
      IntValue(i / v.i)
    else
      reporter.fatal("Cannot divide by 0") // TODO HR : Fix message here

  @targetName("mod") infix def %(v: IntValue)(using Context): IntValue =
    if v.i != 0 then
      IntValue(i % v.i)
    else
      reporter.fatal("Cannot divide by 0") // TODO HR : Fix message here

  @targetName("less") infix def <(v: IntValue)(using Context): BooleanValue =
    BooleanValue(i < v.i)

  @targetName("lessEq") infix def <=(v: IntValue)(using Context): BooleanValue =
    BooleanValue(i <= v.i)

}

// ================================================================================================
// ===================================== BOOLEAN VALUES ===========================================
// ================================================================================================
case class BooleanValue(b: Boolean) extends Value {
  final def scala2Amy(bool: Boolean): BooleanValue =
    BooleanValue(bool)

  def amy2Scala(bool: BooleanValue): Boolean =
    bool.b

  def unary_!(using Context): BooleanValue = BooleanValue(!b)

  infix def &&(bool: => BooleanValue)(using Context): BooleanValue =
    BooleanValue(b && bool.b)

  infix def ||(bool: => BooleanValue)(using Context): BooleanValue =
    BooleanValue(b || bool.b)
}

// ====================================== STRING VALUES ===========================================
case class StringValue(s: String) extends Value {
  infix def ++(ss: StringValue) = StringValue(s + ss.s)
}

case object UnitValue extends Value

case class CaseClassValue(constructor: Identifier, args: List[Value]) extends Value

case class FunctionValue(args: List[Identifier], body: Expr) extends Value
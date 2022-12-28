package amyc.interpreter

import amyc.ast.Identifier

// A class that represents a value computed by interpreting an expression
abstract class Value {
  def asInt: Int = this.asInstanceOf[IntValue].i

  def asBoolean: Boolean = this.asInstanceOf[BooleanValue].b

  def asString: String = this.asInstanceOf[StringValue].s

  override def toString: String = this match {
    case IntValue(i) => i.toString
    case BooleanValue(b) => b.toString
    case StringValue(s) => s
    case UnitValue => "()"
    case CaseClassValue(constructor, args) =>
      constructor.name + "(" + args.map(_.toString).mkString(", ") + ")"
  }
}

case class IntValue(i: Int) extends Value

case class BooleanValue(b: Boolean) extends Value

case class StringValue(s: String) extends Value

case object UnitValue extends Value

case class CaseClassValue(constructor: Identifier, args: List[Value]) extends Value
package amyc.interpreter

import amyc.core.Context
import amyc.utils.Preconditions.*
import amyc.ctx

import scala.language.implicitConversions

object BuiltIns {

  type BuiltInFunction = List[Value] => Value

  // These built-in functions do not have an Amy implementation in the program,
  // instead their implementation is encoded in this map
  lazy val builtIns: Context ?=> Map[(String, String), BuiltInFunction] = Map(
    ("Std", "printInt")      -> Std_printInt,
    ("Std", "printString")   -> Std_printString,
    ("Std", "readString")    -> Std_readString,
    ("Std", "readInt")       -> Std_readInt,
    ("Std", "intToString")   -> Std_intToString,
    ("Std", "digitToString") -> Std_digitToString
  )

  // ==============================================================================================
  // ================================= IMPLICIT CONVERSIONS =======================================
  // ==============================================================================================

  implicit def function0(f: () => Value)(using Context): List[Value] => Value =
    l => require(l.isEmpty) {
      f()
    }

  implicit def function1(f: Value => Value)(using Context): List[Value] => Value =
    l => require(l.length == 1){
      f(l.head)
    }

  // ==============================================================================================
  // ===================================== BUILTIN FUNCTION =======================================
  // ==============================================================================================

  def Std_printInt(arg: Value)(using Context) : Value =
    println(arg.asInt)
    UnitValue


  def Std_printString(arg: Value)(using Context): Value =
    println(arg.asString)
    UnitValue

  def Std_readString()(using Context): Value =
    StringValue(scala.io.StdIn.readLine())

  def Std_readInt()(using Context): Value =
    val input = scala.io.StdIn.readLine()
    try {
      IntValue(input.toInt)
    } catch {
      case ne: NumberFormatException =>
        ctx.reporter.fatal(s"""Could not parse "$input" to Int""")
    }

  def Std_intToString(arg: Value)(using Context): Value =
    StringValue(arg.asInt.toString)

  def Std_digitToString(arg: Value)(using Context): Value =
    StringValue(arg.asInt.toString)

}

package amyc.interpreter

import amyc.core.Context
import amyc.utils.Preconditions.*
import amyc.ctx

import scala.language.implicitConversions

object BuiltIns {

  private type V = Value
  type BuiltInFunction = List[V] => V

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

  implicit def function0(f: () => V)(using Context): List[V] => V =
    l => require(l.isEmpty) {
      f()
    }

  implicit def function1(f: V => V)(using Context): List[V] => V =
    l => require(l.length == 1){
      f(l.head)
    }

  // ==============================================================================================
  // ===================================== BUILTIN FUNCTION =======================================
  // ==============================================================================================

  def Std_printInt(arg: V)(using Context) : V =
    println(arg.asInt)
    UnitValue


  def Std_printString(arg: V)(using Context): V =
    println(arg.asString)
    UnitValue

  def Std_readString()(using Context): V =
    StringValue(scala.io.StdIn.readLine())

  def Std_readInt()(using Context): V =
    val input = scala.io.StdIn.readLine()
    try {
      IntValue(input.toInt)
    } catch {
      case ne: NumberFormatException =>
        ctx.reporter.fatal(s"""Could not parse "$input" to Int""")
    }

  def Std_intToString(arg: V)(using Context): V =
    StringValue(arg.asInt.toString)

  def Std_digitToString(arg: V)(using Context): V =
    StringValue(arg.asInt.toString)

}

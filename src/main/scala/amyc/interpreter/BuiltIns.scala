package amyc.interpreter

import amyc.ctx
import amyc.utils.Context

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
  // ===================================== BUILTIN FUNCTION =======================================
  // ==============================================================================================

  def Std_printInt(args: List[Value])(using Context) : Value =
    println(args.head.asInt)
    UnitValue

  def Std_printString(args: List[Value])(using Context): Value =
    println(args.head.asString)
    UnitValue

  def Std_readString(args: List[Value])(using Context): Value =
    StringValue(scala.io.StdIn.readLine())

  def Std_readInt(args: List[Value])(using Context): Value =
    val input = scala.io.StdIn.readLine()
    try {
      IntValue(input.toInt)
    } catch {
      case ne: NumberFormatException =>
        ctx.reporter.fatal(s"""Could not parse "$input" to Int""")
    }

  def Std_intToString(args: List[Value])(using Context): Value =
    StringValue(args.head.asInt.toString)

  def Std_digitToString(args: List[Value])(using Context): Value =
    StringValue(args.head.asInt.toString)

}

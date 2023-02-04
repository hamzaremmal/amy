package amyc.interpreter.builtin.amy

import amyc.core.Context
import amyc.interpreter.*
import amyc.interpreter.builtin.BuiltinModule
import amyc.interpreter.builtin.BuiltinModule.V
import amyc.reporter

/**
  *
  */
object Std extends BuiltinModule:

  /**
    *
    * @param arg
    * @param Context
    * @return
    */
  def printInt(arg: V)(using Context): V =
    println(arg.asInt)
    UnitValue

  /**
    *
    * @param arg
    * @param Context
    * @return
    */
  def printString(arg: V)(using Context): V =
    println(arg.asString)
    UnitValue

  /**
    *
    * @param Context
    * @return
    */
  def readString()(using Context): V =
    StringValue(scala.io.StdIn.readLine())

  /**
    *
    * @param Context
    * @return
    */
  def readInt()(using Context): V =
    val input = scala.io.StdIn.readLine()
    try {
      IntValue(input.toInt)
    } catch {
      case ne: NumberFormatException =>
        reporter.fatal(s"""Could not parse "$input" to Int""")
    }

  /**
    *
    * @param arg
    * @param Context
    * @return
    */
  def intToString(arg: V)(using Context): V =
    StringValue(arg.asInt.toString)

  /**
    *
    * @param arg
    * @param Context
    * @return
    */
  def digitToString(arg: V)(using Context): V =
    StringValue(arg.asInt.toString)

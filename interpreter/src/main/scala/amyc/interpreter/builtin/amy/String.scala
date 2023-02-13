package amyc.interpreter.builtin.amy

import amyc.core.Context
import amyc.interpreter.{IntValue, StringValue}
import amyc.interpreter.builtin.BuiltinModule
import amyc.interpreter.builtin.BuiltinModule.V
import amyc.reporter

object String extends BuiltinModule:
  
  def length(str: V)(using Context): V =
    str match
      case StringValue(str, _) => IntValue(str.length)
      case _ => reporter.fatal("")

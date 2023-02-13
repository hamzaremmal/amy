package amyc.interpreter.builtin

import amyc.*
import amyc.core.Context
import amyc.core.Symbols.*
import amyc.core.StdDefinitions.*
import amyc.utils.Preconditions.*
import amyc.interpreter.{IntValue, StringValue, UnitValue, Value}
import amyc.interpreter.builtin.amy.*

import scala.language.implicitConversions

object BuiltIns :

  import BuiltinModule.*

  // These built-in functions do not have an Amy implementation in the program,
  // instead their implementation is encoded in this map
  lazy val builtIns: Context ?=> Map[Symbol, BuiltInFunction] = Map(
    stdDef.Std_printInt      -> Std.printInt,
    stdDef.Std_printString   -> Std.printString,
    stdDef.Std_readString    -> Std.readString,
    stdDef.Std_readInt       -> Std.readInt,
    stdDef.Std_intToString   -> Std.intToString,
    stdDef.Std_digitToString -> Std.digitToString,
    stdDef.String_length     -> String.length,
    // TODO HR : Add String::concat here
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

  // TODO HR : Implement other bridge function here

package amyc.interpreter.builtin

import amyc.interpreter.Value

trait BuiltinModule

object BuiltinModule :
  type V = Value
  type BuiltInFunction = List[V] => V


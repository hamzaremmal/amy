package amyc

import amyc.utils.Frontend
import interpreter.Interpreter

class InterpreterTests extends ExecutionTests :
  override val pipeline = Frontend andThen Interpreter

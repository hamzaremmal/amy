package amyc

import amyc.utils.Frontend
import amyc.interpreter.Interpreter
import amyc.tools.Pipeline

import java.io.File

class InterpreterTests extends ExecutionTests :
  
  override val pipeline =
    Frontend andThen
    Interpreter
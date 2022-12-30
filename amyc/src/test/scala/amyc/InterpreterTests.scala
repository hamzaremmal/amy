package amyc

import amyc.utils.{Frontend, Pipeline}
import interpreter.Interpreter

import java.io.File

class InterpreterTests extends ExecutionTests :
  
  override val pipeline =
    Frontend andThen
    Interpreter
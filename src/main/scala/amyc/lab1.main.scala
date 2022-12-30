package amyc

import amyc.utils.*
import amyc.core.Context.inFreshContext
import interpreter.Interpreter

import java.io.File

object Lab1:

  private lazy val pipeline = FetchFiles andThen Frontend andThen Interpreter

  def main(args: Array[String]): Unit =
    inFreshContext {
      try
        pipeline.run(args.toList)
      catch
        case AmycFatalError(_) =>
          sys.exit(1)
    }

package amyc

import amyc.core.Context.inFreshContext
import amyc.interpreter.Interpreter
import amyc.utils.{AmycFatalError, FetchFiles, Frontend, Pipeline}
import amyc.utils.Pipeline.execute

object runner {

  lazy val pipeline: Pipeline[List[String], Unit] =
    FetchFiles andThen
    Frontend andThen
    Interpreter

  def main(args: Array[String]): Unit =
    inFreshContext {
      try
        execute(pipeline) {
          args.toList
        }
      catch
        case AmycFatalError(_) =>
          sys.exit(1)
    }

}

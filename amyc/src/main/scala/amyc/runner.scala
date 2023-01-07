package amyc

import amyc.core.Context.inFreshContext
import amyc.interpreter.Interpreter
import amyc.utils.{AmycFatalError, FetchFiles, Frontend, Pipeline}
import amyc.utils.Pipeline.execute
import amyc.utils.error.checkAmycErrors

object runner {

  lazy val pipeline: Pipeline[List[String], Unit] =
    FetchFiles andThen
    Frontend andThen
    Interpreter

  def main(args: Array[String]): Unit =
    inFreshContext {
      checkAmycErrors {
        execute(pipeline) {
          args.toList
        }
      }
    }

}

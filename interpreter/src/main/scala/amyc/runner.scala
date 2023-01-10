package amyc

import amyc.core.Context.inFreshContext
import amyc.utils.Pipeline.execute
import amyc.utils.error.checkAmycErrors
import amyc.utils.{FetchFiles, Frontend, Pipeline}
import amyc.interpreter.Interpreter

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

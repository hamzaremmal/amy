package amyc

import amyc.core.Context.inFreshContext
import amyc.tools.Pipeline.execute
import amyc.utils.error.checkAmycErrors
import amyc.utils.{FetchFiles, Frontend}
import amyc.interpreter.Interpreter
import amyc.tools.Pipeline

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

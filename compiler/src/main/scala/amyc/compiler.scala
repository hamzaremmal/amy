package amyc

import amyc.backend.wasm.gen.{CodePrinter, WASMCodeGenerator}
import amyc.utils.{AmycFatalError, FetchFiles, Frontend, Pipeline}
import amyc.core.Context.inFreshContext
import amyc.utils.error.checkAmycErrors
import amyc.utils.Pipeline.execute

object compiler {

  lazy val pipeline: Pipeline[List[String], Unit] =
    FetchFiles andThen
    Frontend andThen
    WASMCodeGenerator andThen
    CodePrinter

  def main(args: Array[String]): Unit =
    inFreshContext {
      checkAmycErrors {
        execute(pipeline) {
          args.toList
        }
      }
    }

}

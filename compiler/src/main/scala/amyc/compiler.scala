package amyc

import backend.wasm.gen.{CodePrinter, WASMCodeGenerator}
import utils.{AmycFatalError, FetchFiles, Frontend, Pipeline}
import core.Context.inFreshContext
import utils.error.checkAmycErrors
import utils.Pipeline.execute

object compiler :

  lazy val pipeline: Pipeline[List[String], Unit] =
    FetchFiles andThen
    Frontend andThen
    WASMCodeGenerator andThen
    CodePrinter

  def main(args: Array[String]): Unit =
    inFreshContext :
      checkAmycErrors :
        execute(pipeline) :
          args.toList

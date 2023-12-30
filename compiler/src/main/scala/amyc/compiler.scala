package amyc

import amyc.tools.Pipeline
import backend.wasm.gen.{CodePrinter, WASMCodeGenerator}
import utils.{FetchFiles, Frontend}
import core.Context.inFreshContext
import utils.error.checkAmycErrors
import amyc.tools.Pipeline.execute

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

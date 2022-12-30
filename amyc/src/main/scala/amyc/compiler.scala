package amyc

import amyc.codegen.{CodeGen, CodePrinter}
import amyc.utils.{FetchFiles, Frontend, Pipeline}
import amyc.core.Context.inFreshContext
import amyc.utils.Pipeline.execute

object compiler {

  lazy val pipeline: Pipeline[List[String], Unit] =
    FetchFiles andThen
    Frontend andThen
    CodeGen andThen
    CodePrinter

  def main(args: Array[String]): Unit =
    inFreshContext {
      execute(pipeline){
        args.toList
      }
    }

}

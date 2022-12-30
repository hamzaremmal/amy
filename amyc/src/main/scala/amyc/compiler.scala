package amyc

import amyc.codegen.{CodeGen, CodePrinter}
import amyc.utils.{AmycFatalError, FetchFiles, Frontend, Pipeline}
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
      try
        execute(pipeline){
          args.toList
        }
      catch
        case AmycFatalError(_) =>
          sys.exit(1)
    }

}

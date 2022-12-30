package amyc

import amyc.core.Context.inFreshContext
import amyc.utils.*
import amyc.ast.*
import amyc.utils.printers.NominalTreePrinter
import parsing.*

import java.io.File

object Lab3:

  private lazy val pipeline =
    FetchFiles andThen
    Lexer andThen
    Parser andThen
    new NominalTreePrinter

  def main(args: Array[String]): Unit =
    inFreshContext {
      try
        pipeline.run(args.toList)
      catch
        case AmycFatalError(_) =>
          sys.exit(1)
    }
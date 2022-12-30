package amyc

import amyc.core.Context.inFreshContext
import amyc.utils.*
import amyc.utils.printers.DisplayTokens
import parsing.*

import java.io.File

object Lab2:

  private lazy val pipeline =
    FetchFiles andThen
    Lexer andThen
    DisplayTokens

  def main(args: Array[String]): Unit =
    inFreshContext {
      try
        pipeline.run(args.toList)
      catch
        case AmycFatalError(_) =>
          sys.exit(1)
    }
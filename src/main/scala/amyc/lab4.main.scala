package amyc

import amyc.core.Context.inFreshContext
import amyc.utils.*
import amyc.ast.*
import amyc.typer.Typer
import parsing.*
import analyzer.*

import java.io.File

object Lab4:

  private lazy val pipeline =
    FetchFiles andThen
    Lexer andThen
    Parser andThen
    NameAnalyzer andThen
    Typer

  def main(args: Array[String]): Unit =
    inFreshContext {
      try
        pipeline.run(args.toList)
      catch
        case AmycFatalError(_) =>
          sys.exit(1)
    }
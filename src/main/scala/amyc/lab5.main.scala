package amyc

import amyc.core.Context.inFreshConext
import amyc.utils.{printers, *}
import amyc.ast.*
import parsing.*
import analyzer.*
import typer.Typer as tp
import codegen.*

import java.io.File
import scala.compiletime.testing.ErrorKind.Typer

object Lab5:

  private lazy val pipeline =
    FetchFiles andThen
    Lexer andThen
    Parser andThen
    NameAnalyzer andThen
    tp andThen
    CodeGen andThen
    CodePrinter

  def main(args: Array[String]): Unit =
    inFreshConext {
      try
        pipeline.run(args.toList)
      catch
        case AmycFatalError(_) =>
          sys.exit(1)
    }
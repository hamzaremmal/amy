package amyc

import amyc.utils.*
import amyc.ast.*
import parsing.*
import analyzer.*
import typer.Typer as tp
import codegen.*

import java.io.File
import scala.compiletime.testing.ErrorKind.Typer

object Lab5 {
  private def parseArgs(args: Array[String]): core.Context = {
    core.Context(new Reporter, args.toList)
  }

  def main(args: Array[String]): Unit = {
    given ctx : core.Context = parseArgs(args)
    val pipeline =
      Lexer andThen
      Parser andThen
      NameAnalyzer andThen
      tp andThen
      CodeGen andThen
      CodePrinter

    val files = ctx.files.map(new File(_))

    try {
      if (files.isEmpty) {
        ctx.reporter.fatal("No input files")
      }
      files.find(!_.exists()).foreach { f =>
        ctx.reporter.fatal(s"File not found: ${f.getName}")
      }
      pipeline.run(files)
      ctx.reporter.terminateIfErrors()
    } catch {
      case AmycFatalError(_) =>
        sys.exit(1)
    }
  }

  
  import SymbolicTreeModule.{Program => SP}
  import NominalTreeModule.{Program => NP}

  def treePrinterS(title: String): Pipeline[(SP, SymbolTable), Unit] = {
    new Pipeline[(SP, SymbolTable), Unit] {

      val name = "treePrinterS"

      def run(v: (SP, SymbolTable))(using core.Context) = {
        println(title)
        println(SymbolicPrinter(v._1)(true))
      }
    }
  }

  def treePrinterN(title: String): Pipeline[NP, Unit] = {
    new Pipeline[NP, Unit] {

      override val name: String = "treePrinterN"

      def run(v: NP)(using core.Context) = {
        println(title)
        println(NominalPrinter(v))
      }
    }
  }
}

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
  private def parseArgs(args: Array[String]): Context = {
    Context(new Reporter, args.toList)
  }

  def main(args: Array[String]): Unit = {
    val ctx = parseArgs(args)
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
      pipeline.run(files)(using ctx)
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
      def run(v: (SP, SymbolTable))(using Context) = {
        println(title)
        println(SymbolicPrinter(v._1)(true))
      }
    }
  }

  def treePrinterN(title: String): Pipeline[NP, Unit] = {
    new Pipeline[NP, Unit] {
      def run(v: NP)(using Context) = {
        println(title)
        println(NominalPrinter(v))
      }
    }
  }
}

package amyc

import amyc.utils.*
import amyc.ast.*
import amyc.typer.Typer
import parsing.*
import analyzer.*

import java.io.File

object Lab4 {
  private def parseArgs(args: Array[String]): Context = {
    Context(new Reporter, args.toList)
  }

  def main(args: Array[String]): Unit = {
    given ctx : Context = parseArgs(args)
    val pipeline =
      Lexer andThen
      Parser andThen
      //treePrinterN("Tree after Parser") andThen
      NameAnalyzer andThen
      //treePrinterS("Tree after NameAnalyzer") andThen
      Typer //andThen
      //treePrinterS("Tree after type Checking")

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

  def treePrinterS(title: String): Pipeline[(SP, SymbolTable), (SP, SymbolTable)] = {
    new Pipeline[(SP, SymbolTable), (SP, SymbolTable)] {

      val name = "treePrinterS"

      def run(v: (SP, SymbolTable))(using Context) = {
        println(title)
        println(SymbolicPrinter(v._1)(true))
        v
      }
    }
  }

  def treePrinterN(title: String): Pipeline[NP, NP] = {
    new Pipeline[NP, NP] {

      val name = "treePrinterN"

      def run(v: NP)(using Context) = {
        println(title)
        println(NominalPrinter(v))
        v
      }
    }
  }
}

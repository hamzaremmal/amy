package amyc

import amyc.utils.{printers, *}
import amyc.ast.*
import amyc.typer.Typer
import parsing.*
import analyzer.*

import java.io.File

object Lab4 {
  private def parseArgs(args: Array[String]): core.Context = {
    core.Context(new Reporter, args.toList)
  }

  def main(args: Array[String]): Unit = {
    given ctx : core.Context = parseArgs(args)
    val pipeline =
      Lexer andThen
      Parser andThen
      NameAnalyzer andThen
      Typer

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

}

package amyc

import amyc.utils._
import amyc.ast._
import parsing._

import java.io.File

object Lab3 {
  private def parseArgs(args: Array[String]): Context = {
    Context(new Reporter, args.toList)
  }

  def main(args: Array[String]): Unit = {
    given ctx : Context = parseArgs(args)
    val pipeline = Lexer andThen Parser andThen treePrinterN("Trees after parsing")

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

  
  import NominalTreeModule.{Program => NP}

  def treePrinterN(title: String): Pipeline[NP, Unit] = {
    new Pipeline[NP, Unit] {

      override val name = "treePrinterN"

      override def run(v: NP)(using Context) = {
        println(title)
        println(NominalPrinter(v))
      }
    }
  }
}


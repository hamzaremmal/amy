package amyc

import amyc.utils._
import interpreter.Interpreter

import java.io.File

object Lab1 {
  private def parseArgs(args: Array[String]): Context = {
    Context(new Reporter, args.toList)
  }

  def main(args: Array[String]): Unit = {
    val ctx = parseArgs(args)
    val pipeline = Frontend.pipeline andThen Interpreter

    val files = ctx.files.map(new File(_))

    try {
      if (files.isEmpty) {
        ctx.reporter.fatal("No input files")
      }
      files.find(!_.exists()).foreach { f =>
        ctx.reporter.fatal(s"File not found: ${f.getName}")
      }
      pipeline.run(ctx)(files)
      ctx.reporter.terminateIfErrors()
    } catch {
      case AmycFatalError(_) =>
        sys.exit(1)
    }
  }
}

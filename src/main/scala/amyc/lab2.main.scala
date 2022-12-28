package amyc

import amyc.utils.*
import parsing.*

import java.io.File

object Lab2 {
  private def parseArgs(args: Array[String]): core.Context = {
    core.Context(new Reporter, args.toList)
  }

  def main(args: Array[String]): Unit = {
    given ctx : core.Context = parseArgs(args)
    val pipeline = Lexer andThen DisplayTokens

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
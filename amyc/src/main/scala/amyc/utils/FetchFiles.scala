package amyc.utils

import amyc.core.Context
import amyc.reporter

import java.io.File

object FetchFiles extends Pipeline[List[String], List[File]]{

  override val name = "FetchFiles"

  override def run(paths: List[String])(using Context): List[File] =
    val files = paths map(new File(_))

    if (files.isEmpty) {
      reporter.fatal("No input files")
    }
    files.find(!_.exists()).foreach { f =>
      reporter.fatal(s"File not found: ${f.getName}")
    }
    reporter.info(s"Compiling ${files.length} Amy source file${if files.length == 1 then "" else "s"}")
    files

}

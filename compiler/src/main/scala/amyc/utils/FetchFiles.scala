package amyc.utils

import amyc.core.Context
import amyc.reporter

import java.io.File

object FetchFiles extends Pipeline[List[String], List[File]]:

  private val mandatory_files =
    "./library/Boolean.amy" ::
    "./library/Int.amy"     ::
    "./library/List.amy"    ::
    "./library/Option.amy"  ::
    "./library/Std.amy"     ::
    "./library/String.amy"  ::
    "./library/Unit.amy"    ::
    "./library/unnamed.amy" ::
    Nil

  override val name = "FetchFiles"

  override def run(paths: List[String])(using Context): List[File] =
    if paths.isEmpty then
      reporter.fatal("No input files")

    val files = (paths ++ mandatory_files).map(path => new File(path)).toSet

    files.find(!_.exists()).foreach { f =>
      reporter.fatal(s"File not found: ${f.getName}")
    }

    reporter.info(s"Compiling ${files.size} Amy source file${if files.size == 1 then "" else "s"}")
    files.toList

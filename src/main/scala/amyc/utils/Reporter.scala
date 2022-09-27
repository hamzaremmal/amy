package amyc.utils

import java.io.File
import scala.io.Source

// Reports errors and warnings during compilation
class Reporter {

  /** Issues some information from the compiler */
  def info(msg: Any, pos: Position = NoPosition): Unit = {
    report("[ Info  ]", msg, pos)
  }

  /** Issues a warning from the compiler */
  def warning(msg: Any, pos: Position = NoPosition): Unit = {
    report("[Warning]", msg, pos)
  }

  private var hasErrors = false

  /** Issues a recoverable error message */
  def error(msg: Any, pos: Position = NoPosition): Unit = {
    hasErrors = true
    report("[ Error ]", msg, pos)
  }

  /** Used for an unrecoverable error: Issues a message, then exits the compiler */
  def fatal(msg: Any, pos: Position = NoPosition): Nothing = {
    report("[ Fatal ]", msg, pos)
    // Despite printing the message, we store it in the error for testing
    val errMsg = s"$pos: $msg"
    throw AmycFatalError(errMsg)
  }

  // Versions for Positioned
  def info(msg: Any, pos: Positioned): Unit = info(msg, pos.position)
  def warning(msg: Any, pos: Positioned): Unit = warning(msg, pos.position)
  def error(msg: Any, pos: Positioned): Unit = error(msg, pos.position)
  def fatal(msg: Any, pos: Positioned): Nothing = fatal(msg, pos.position)


  /** Terminates the compiler if any errors have been detected. */
  def terminateIfErrors() = {
    if (hasErrors) {
      fatal("There were errors.")
    }
  }

  private def err(msg: String): Unit = {
    Console.err.println(msg)
  }

  private def report(prefix: String, msg: Any, pos: Position): Unit = {
    if (pos.isDefined) {
      err(s"$prefix $pos: $msg")

      val lines = getLines(pos.file)

      if (pos.line > 0 && pos.line-1 < lines.size) {
        err(s"$prefix ${lines(pos.line-1)}")
        err(prefix + " " + " "*(pos.col - 1)+"^")
      } else {
        err(s"$prefix <line unavailable in source file>")
      }
    } else {
      err(s"$prefix $msg")
    }
  }

  private var filesToLines = Map[File, IndexedSeq[String]]()

  private def getLines(f: File): IndexedSeq[String] = {
    filesToLines.get(f) match {
      case Some(lines) =>
        lines

      case None =>
        val source = Source.fromFile(f).withPositioning(true)
        val lines = source.getLines().toIndexedSeq
        source.close()

        filesToLines += f -> lines

        lines
    }
  }
}

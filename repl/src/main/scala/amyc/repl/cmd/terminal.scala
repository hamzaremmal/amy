package amyc.repl.cmd

import amyc.utils.Properties.*
import amyc.core.Context
import org.jline.reader.LineReader.*
import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.reader.impl.history.DefaultHistory
import org.jline.terminal.{Terminal, TerminalBuilder}

object terminal extends AutoCloseable {

  val terminal: Terminal = TerminalBuilder.builder().build()

  lazy val reader: LineReader = LineReaderBuilder
    .builder()
    .terminal(terminal)
    .history(new DefaultHistory)
    .variable(HISTORY_FILE, s"$user_home/.amy_history") // Save history to file
    .build()

  def readLine: String =
    reader.readLine("amy> ")

  override def close() : Unit =
      terminal.close()

}

package amyc.repl

import amyc.repl.cmd.terminal

object repl {

  def main(args: Array[String]): Unit =
    println(s"Welcome to Amy's REPL")
    var input = terminal.readLine
    while(input != "exit"){
      println(s"repl have read: '$input'")
      input = terminal.readLine
    }
    terminal.close()


}

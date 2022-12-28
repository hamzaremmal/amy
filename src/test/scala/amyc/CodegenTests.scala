package amyc

import parsing.*
import analyzer.NameAnalyzer
import typer.Typer
import codegen.*
import wasm.Module
import amyc.utils.*

import scala.sys.process.*
import scala.collection.JavaConverters.*
import java.io.ByteArrayInputStream

class CodegenTests extends ExecutionTests {

  object CodePrinterExecutor extends Pipeline[Module, Unit] {

    override val name = "CodePrinterExecutor"

    def run(m: Module)(using core.Context) = {
      CodePrinter.run(m)(using ctx)
      val fileName = s"${m.name}.js"

      // Consume all standard input!
      val input = Console.in.lines.iterator().asScala.toList.mkString("\n")
      val inputS = new ByteArrayInputStream(input.getBytes("UTF-8"))

      val exitCode = s"node wasmout/$fileName" #< inputS ! ProcessLogger(Console.out.println, Console.err.println)
      if (exitCode != 0)
        throw AmycFatalError("Nonzero code returned from nodejs. Maybe you forgot to install deasync? (run `npm install deasync` in the root folder of the project, the one that contains the build.sbt)")
    }
  }

  val pipeline =
    Lexer andThen
    Parser andThen
    NameAnalyzer andThen
    Typer andThen
    CodeGen andThen
    CodePrinterExecutor
}


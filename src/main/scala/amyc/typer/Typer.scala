package amyc.typer

import amyc.analyzer.SymbolTable
import amyc.utils.{Context, Pipeline}
import amyc.ast.SymbolicTreeModule.*

object Typer extends Pipeline[Program, Program]{

  private lazy val typerPipeline =
    TypeInferer andThen
    TypeAssigner andThen
    TypeChecker

  override def run(v: Program)(using Context) =
    typerPipeline.run(v)

  override val name = "Typer"

}

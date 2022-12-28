package amyc.typer

import amyc.analyzer.SymbolTable
import amyc.utils.Pipeline
import amyc.ast.SymbolicTreeModule.*
import amyc.core.Context

object Typer extends Pipeline[Program, Program]{

  private lazy val typerPipeline =
    TypeInferer andThen
    TypeAssigner andThen
    TypeChecker

  override def run(v: Program)(using Context) =
    typerPipeline.run(v)

  override val name = "Typer"

}

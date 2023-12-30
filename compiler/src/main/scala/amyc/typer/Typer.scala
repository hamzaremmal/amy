package amyc
package typer

import ast.SymbolicTreeModule.Program
import core.Context
import utils.Pipeline

object Typer extends Pipeline[Program, Program]{

  private lazy val typerPipeline =
    TypeInferer andThen
    TypeAssigner andThen
    TypeChecker

  override def run(v: Program)(using Context): Program =
    typerPipeline.run(v)

  override val name = "Typer"

}

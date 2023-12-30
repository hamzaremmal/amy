package amyc
package typer

import tools.Pipeline
import ast.SymbolicTreeModule.Program
import core.Context

object Typer extends Pipeline[Program, Program]{

  private lazy val typerPipeline =
    ConstraintSolver andThen
    TypeAssigner andThen
    TypeChecker

  override def run(v: Program)(using Context): Program =
    typerPipeline.run(v)

  override val name = "Typer"

}

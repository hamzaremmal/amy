package amyc.typer

import amyc.analyzer.SymbolTable
import amyc.ast.SymbolicTreeModule
import amyc.utils.{Context, Pipeline}
import amyc.ast.SymbolicTreeModule.*

object Typer extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)]{

  private lazy val typerPipeline =
    TypeInferer andThen
    TypeAssigner andThen
    TypeChecker

  override def run(v: (SymbolicTreeModule.Program, SymbolTable))(using Context) =
    typerPipeline.run(v)

}

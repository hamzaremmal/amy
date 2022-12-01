package amyc.typer

import amyc.analyzer.SymbolTable
import amyc.ast.SymbolicTreeModule.*
import amyc.utils.{Context, Pipeline}

object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)]{

  override def run(v: (Program, SymbolTable))(using Context) = ???

}
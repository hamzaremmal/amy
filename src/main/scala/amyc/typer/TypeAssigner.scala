package amyc.typer

import amyc.analyzer.SymbolTable
import amyc.ast.SymbolicTreeModule.*
import amyc.utils.{Context, Pipeline}

object TypeAssigner extends Pipeline[(Program, SymbolTable, List[(Type, Type)]), (Program, SymbolTable)]{

  override def run(v: (Program, SymbolTable, List[(Type, Type)]))(using Context) =
    (v._1, v._2)

}

package amyc.utils

import amyc.analyzer.{NameAnalyzer, SymbolTable}
import amyc.ast.SymbolicTreeModule
import amyc.parsing.{Lexer, Parser}
import amyc.typer.Typer
import amyc.ast.SymbolicTreeModule.Program
import amyc.core.Context
import amyc.transform.LambdaLifter

import java.io.File

object Frontend extends Pipeline[List[File], Program]{

  private lazy val pipeline =
    Lexer andThen
    Parser andThen
    NameAnalyzer andThen
    LambdaLifter andThen
    Typer

  override def run(v: List[File])(using Context) =
    pipeline.run(v)

  override val name = "Frontend"


}

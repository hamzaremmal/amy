package amyc.utils

import amyc.analyzer.NameAnalyzer
import amyc.parsing.{Lexer, Parser}
import amyc.typer.Typer

object Frontend {

  lazy val pipeline =
    Lexer andThen
    Parser andThen
    NameAnalyzer andThen
    Typer

}

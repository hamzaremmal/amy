package amyc.transform

import amyc.ast.SymbolicTreeModule
import amyc.utils.Pipeline
import amyc.ast.SymbolicTreeModule.*
import amyc.core.Context
import amyc.*

object LambdaLifter extends Pipeline[Program, Program]{

  override val name: String = "LambdaLifter"

  override def run(v: Program)(using Context): Program =
    reporter.info(s"$name phase was ignored")
    v

}

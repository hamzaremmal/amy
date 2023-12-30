package amyc
package typer

import ast.{TreeTraverser, SymbolicTreeModule as S}
import amyc.tools.MiniPipeline

object ConstraintGenerator extends MiniPipeline[S.Program] with TreeTraverser {

  override val name = "constraint-generator"

}

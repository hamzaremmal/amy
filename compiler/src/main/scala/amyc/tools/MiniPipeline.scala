package amyc
package tools

import ast.NominalTreeModule as N
import amyc.core.Context
import ast.TreeTraverser

abstract class MiniPipeline[F <: N.Tree] extends Pipeline[F, F] {
  traverser: TreeTraverser =>

  override def run(tree: F)(using Context): F =
    traverser.traverse(tree)
    tree


}

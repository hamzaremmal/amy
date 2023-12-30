package amyc
package tools

import ast.SymbolicTreeModule as S
import amyc.core.Context
import amyc.transform.{TypedTreeTransformer, UntypedTreeTransformer}
import ast.TreeTraverser

abstract class MiniPipeline[F <: S.Tree] extends Pipeline[F, F] {
  self: TreeTraverser | TypedTreeTransformer =>

  override final def run(tree: F)(using Context): F =
    self match
      case traverser: TreeTraverser =>
        traverser.traverse(tree)
        tree
      case transformer: TypedTreeTransformer =>
        transformer.transform(tree)


}

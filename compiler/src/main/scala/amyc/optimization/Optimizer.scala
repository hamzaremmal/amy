package amyc
package optimization

import amyc.core.Context
import amyc.transform.TypedTreeTransformer
import ast.SymbolicTreeModule as S
import tools.MiniPipeline

object Optimizer extends MiniPipeline[S.Program] with TypedTreeTransformer :

  override val name = "optimizer"

  override def transform(tree: S.Not)(using Context): S.Expr =
    tree match
      case S.Not(S.Not(tree)) => transform(tree)
      case _ => transformChildren(tree)

  override def transform(tree: S.Neg)(using Context): S.Expr =
    tree match
      case S.Neg(S.Neg(tree)) => transform(tree)
      case _ => transformChildren(tree)

  override def transform(tree: S.Ite)(using Context): S.Expr =
    tree match
      case S.Ite(S.BooleanLiteral(true), expr, _) => expr
      case S.Ite(S.BooleanLiteral(false), _, expr) => expr
      case _ => transformChildren(tree)

end Optimizer
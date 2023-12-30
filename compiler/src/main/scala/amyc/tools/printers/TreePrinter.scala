package amyc
package tools
package printers

import amyc.core.Context
import ast.SymbolicTreeModule as S
import ast.TreeTraverser

object TreePrinter extends MiniPipeline[S.Program] with TreeTraverser {

  override def name: String = "tree-printer"

  override def traverse(tree: S.Program)(using Context): Unit =
    println("Hello world from program")
    traverseChildren(tree)

  override def traverse(tree: S.FunDef)(using Context): Unit =
    println(s"Defining function ${tree.name}")

}

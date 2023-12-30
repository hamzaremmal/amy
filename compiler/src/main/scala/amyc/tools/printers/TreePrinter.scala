package amyc
package tools
package printers

import amyc.core.Context
import ast.NominalTreeModule as N
import ast.TreeTraverser

object TreePrinter extends MiniPipeline[N.Program] with TreeTraverser {

  override def name: String = "tree-printer"

  override def traverse(tree: N.Program)(using Context): Unit =
    println("Hello world from program")
    traverseChildren(tree)

  override def traverse(tree: N.FunDef)(using Context): Unit =
    println(s"Defining function ${tree.name}")

}

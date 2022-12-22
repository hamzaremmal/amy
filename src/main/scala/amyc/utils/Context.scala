package amyc.utils

import scala.collection.mutable.HashMap
import amyc.ast.SymbolicTreeModule.*

import scala.collection.mutable

// Contains a reporter and configuration for the compiler
case class Context(
  reporter: Reporter,
  files: List[String],
  printTokens: Boolean = false,
  printTrees: Boolean = false,
  printNames: Boolean = false,
  interpret: Boolean = false,
  help: Boolean = false
){

  val tv : HashMap[Type, Type] = mutable.HashMap.empty[Type, Type]


}

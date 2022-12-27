package amyc.utils

import amyc.analyzer.SymbolTable

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

  private var _symtable: Option[SymbolTable] = None

  def symbols: SymbolTable =
    _symtable.getOrElse{
        reporter.fatal(s"Cannot access the symbol table before the NameAnalyzer")
    }

  def withSymTable(table: SymbolTable) =
    _symtable match
      case None => _symtable = Some(table)
      case Some(_) => reporter.fatal(s"Cannot change the symbol table in a compiler Run")


}

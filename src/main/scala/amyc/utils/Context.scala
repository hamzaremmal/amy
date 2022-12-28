package amyc.utils

import amyc.analyzer.{NameAnalyzer, SymbolTable}

import scala.collection.mutable.HashMap
import amyc.ast.SymbolicTreeModule.*

import scala.collection.mutable

// Contains a reporter and configuration for the compiler
case class Context(reporter: Reporter, files: List[String]){

  val tv : HashMap[Type, Type] = mutable.HashMap.empty[Type, Type]

  private var _symtable: Option[SymbolTable] = None
  private var _pipeline : String = compiletime.uninitialized


  // ==============================================================================================
  // =================================== SYMBOL MANAGEMENT ========================================
  // ==============================================================================================
  def symbols: SymbolTable =
    _symtable.getOrElse{
        reporter.fatal(s"Cannot access the symbol table before the NameAnalyzer")
    }

  def withSymTable(table: SymbolTable) =
    _symtable match
      case None if phase == NameAnalyzer.name => _symtable = Some(table)
      case _ => reporter.fatal(s"Cannot change the symbol table in a compiler Run")

  // ==============================================================================================
  // ===================================== PHASE MANAGEMENT =======================================
  // ==============================================================================================

  def atPhase(pipeline: Pipeline[_, _]) =
    _pipeline = pipeline.name

  def phase : String =
    _pipeline

}

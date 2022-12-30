package amyc.core

import amyc.analyzer.{NameAnalyzer, SymbolTable}
import amyc.ast.SymbolicTreeModule.*
import amyc.utils.{Pipeline, Reporter}

import scala.collection.mutable
import scala.collection.mutable.HashMap

// Contains a reporter and configuration for the compiler
case class Context private (reporter: Reporter){

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

object Context {

  def inFreshConext[A](body: Context ?=> A) =
    given ctx : Context = new Context(new Reporter)
    body
}

package amyc
package core

import amyc.analyzer.{EmptyScope, NameAnalyzer, Scope, SymbolTable}
import amyc.ast.SymbolicTreeModule.*
import amyc.core.Symbols.*
import amyc.core.Types.*
import amyc.utils.{Pipeline, Reporter}

import scala.collection.mutable
import scala.collection.mutable.HashMap

// Contains a reporter and configuration for the compiler
case class Context private (reporter: Reporter){

  val tv : mutable.HashMap[Type, Type] = mutable.HashMap.empty[Type, Type]

  // Store Scopes of each module
  private val _scopes : mutable.HashMap[Symbol, Scope] = mutable.HashMap.empty

  private var _symtable : Option[SymbolTable] = None
  private var _pipeline : String = compiletime.uninitialized

  // TODO HR : Should be removed from Context
  def tpe(tree : TypeTree) : Type =
    tree match
      case ClassTypeTree(id) => ClassType(id.id)
      case TTypeTree(tpe) => tpe
      case FunctionTypeTree(args, rte) =>
        Identifier.fresh(s"(${args.map(tpe).mkString(";")}:${tpe(rte)})")
        FunctionType(args.map(tpe), tpe(rte))


  // ==============================================================================================
  // =================================== SYMBOL MANAGEMENT ========================================
  // ==============================================================================================
  def symbols: SymbolTable =
    _symtable.getOrElse{
        reporter.fatal(s"Cannot access the symbol table before the NameAnalyzer")
    }

  def withSymTable(table: SymbolTable): Unit =
    _symtable match
      case None if phase == NameAnalyzer.name => _symtable = Some(table)
      case _ => reporter.fatal(s"Cannot change the symbol table in a compiler Run")

  // ==============================================================================================
  // ===================================== PHASE MANAGEMENT =======================================
  // ==============================================================================================

  def atPhase(pipeline: Pipeline[_, _]): Unit =
    _pipeline = pipeline.name

  def phase : String =
    _pipeline

  // ==============================================================================================
  // ================================= SCOPE MANAGEMENT ===========================================
  // ==============================================================================================

  /**
    * Index the Scope of each module
    * @param id
    * @param scope
    * @return
    */
  def withScope(id: Symbol, scope : Scope = EmptyScope) =
    _scopes.put(id, scope)

  /* TODO : This method is unsafe, fix it */
  def scope(id: Symbol) : Scope =
    _scopes(id)

}

object Context {

  def inFreshContext[A](body: Context ?=> A): A =
    inContext(new Context(new Reporter))(body)

  def inContext[A](ctx: Context)(body: Context ?=> A): A =
    body(using ctx)
}

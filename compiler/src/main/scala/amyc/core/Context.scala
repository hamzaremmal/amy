package amyc.core

import amyc.ast.SymbolicTreeModule.*
import amyc.analyzer.{NameAnalyzer, SymbolTable}
import amyc.ast.Identifier
import amyc.core.Types.*
import amyc.utils.{Pipeline, Reporter}

import scala.collection.mutable
import scala.collection.mutable.HashMap

// Contains a reporter and configuration for the compiler
case class Context private (reporter: Reporter){

  val tv : mutable.HashMap[Type, Type] = mutable.HashMap.empty[Type, Type]
  val _types : mutable.HashMap[Identifier, Type] =
    mutable.HashMap(
      StdNames.IIntType -> IntType,
      StdNames.IStringType -> StringType,
      StdNames.IBooleanType -> BooleanType,
      StdNames.IUnitType -> UnitType
  )

  private var _symtable: Option[SymbolTable] = None
  private var _pipeline : String = compiletime.uninitialized

  def tpe(tree : TypeTree) : Type =
    tree match
      case ClassTypeTree(id) => _types(id)
      case FunctionTypeTree(args, rte) =>
        val id = Identifier.fresh(s"(${args.map(tpe).mkString(";")}:${tpe(rte)})")
        _types.getOrElseUpdate(id, FunctionType(args.map(tpe), tpe(rte)))


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

}

object Context {

  def inFreshContext[A](body: Context ?=> A): A =
    given ctx : Context = new Context(new Reporter)
    body
}

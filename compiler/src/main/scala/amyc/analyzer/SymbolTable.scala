package amyc.analyzer

import amyc.ast.SymbolicTreeModule.TypeTree
import amyc.core.Identifier
import amyc.core.Symbols.*
import amyc.utils.UniqueCounter
import amyc.core.Signatures.*

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.collection.mutable.HashMap

// A class that represents a dictionary of symbols for an Amy program
class SymbolTable {
  /* Atomic counters to index constructors and functions */
  private val constrIndexes = new UniqueCounter[Symbol]
  private val funIndexes = new AtomicInteger

  // ==============================================================================================
  // ========================================= TABLES =============================================
  // ==============================================================================================

  // map a definition (owner, def) to its symbol
  private val defsByName = mutable.HashMap[(String, String), Symbol]()
  // maps a nominal representation of the tree to its symbol
  private val modules = mutable.HashMap[String, ModuleSymbol]()

  // ==============================================================================================
  // ======================================== API =================================================
  // ==============================================================================================

  /* register a new module */
  def addModule(name: String): ModuleSymbol =
    val sym = ModuleSymbol(Identifier.fresh(name))
    modules += name -> sym
    sym

  /* register a new type */
  def addType(owner: String, name: String): Symbol =
    val sym_owner = modules.getOrElse(owner, sys.error(s"Module $name not found!"))
    val sym = TypeSymbol(Identifier.fresh(name), sym_owner)
    defsByName += (owner, name) -> sym
    sym

  /* register a new constructor */
  def addConstructor(owner: String, name: String, argTypes: List[TypeTree], parent: Symbol): Symbol =
    val sym_owner = getModule(owner).getOrElse(sys.error(s"Module $owner not found!"))
    val sym = ConstructorSymbol(Identifier.fresh(name), sym_owner)
    defsByName += (owner, name) -> sym
    sym.signature(ConstrSig(argTypes, parent, constrIndexes.next(parent)))
    sym

  /* register a new function */
  def addFunction(owner: String, name: String, argTypes: List[TypeTree], retType: TypeTree): Symbol =
    val sym_owner = getModule(owner).getOrElse(sys.error(s"Module $owner not found!"))
    val sym = FunctionSymbol(Identifier.fresh(name), sym_owner)
    val idx = funIndexes.incrementAndGet()
    defsByName += (owner, name) -> sym
    sym.signature(FunSig(argTypes, retType, idx))
    sym

  /* register a new infix function (trick for now to implement binary operators) */
  def addInfixFunction(owner: String, name: String, argTypes: List[TypeTree], retType: TypeTree): Symbol =
    val sym_owner = getModule(owner).getOrElse(sys.error(s"Module $owner not found!"))
    val sym = FunctionSymbol(Identifier.fresh(name), sym_owner, true)
    val idx = funIndexes.incrementAndGet()
    defsByName += (owner, name) -> sym
    sym.signature(FunSig(argTypes, retType, idx))
    sym


  /* fetch symbol of a module */
  def getModule(name: String): Option[ModuleSymbol] =
    modules.get(name)

  /* fetch the symbol of a type */
  def getType(owner: String, name: String): Option[TypeSymbol] =
    defsByName get (owner, name) flatMap { _ match
        case sym: TypeSymbol => Some(sym)
        case _ => None
    }

  /* fetch the symbol of a constructor */
  def getConstructor(owner: String, name: String): Option[ConstructorSymbol] =
    defsByName get(owner, name) flatMap {
      _ match
        case sym: ConstructorSymbol => Some(sym)
        case _ => None
    }

  /* fetch the symbol of a function */
  def getFunction(owner: String, name: String): Option[FunctionSymbol] =
    defsByName get (owner, name) flatMap { _ match
      case sym : FunctionSymbol => Some(sym)
      case _ => None
    }

}

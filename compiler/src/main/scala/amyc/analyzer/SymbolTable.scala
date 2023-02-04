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
  // ???
  private val types = mutable.HashMap[Symbol, ModuleSymbol]()

  // ==============================================================================================
  // ======================================== API =================================================
  // ==============================================================================================

  /**
    * Register a new Module
    * @param name
    * @return Symbol of the module
    */
  def addModule(name: String): ModuleSymbol =
    val sym = ModuleSymbol(Identifier.fresh(name))
    modules += name -> sym
    sym

  /**
    * Fetch the Symbol of the requested module
    * @param name
    * @return
    */
  def getModule(name: String): Option[ModuleSymbol] =
    modules.get(name)

  /**
    *
    * @param owner
    * @param name
    * @return
    */
  def addType(owner: String, name: String): Symbol =
    val sym_owner = modules.getOrElse(owner, sys.error(s"Module $name not found!"))
    val sym = TypeSymbol(Identifier.fresh(name), sym_owner)
    defsByName += (owner, name) -> sym
    types += (sym -> sym_owner)
    sym

  /**
    *
    * @param owner
    * @param name
    * @return
    */
  def getType(owner: String, name: String): Option[Symbol] =
    defsByName.get(owner,name) filter types.contains

  /**
    *
    * @param symbol
    * @return
    */
  def getType(symbol: Symbol): Option[Symbol] =
    types.get(symbol)

  /**
    *
    * @param owner
    * @param name
    * @param argTypes
    * @param parent
    * @return
    */
  def addConstructor(owner: String, name: String, argTypes: List[TypeTree], parent: Symbol): Symbol =
    val sym_owner = getModule(owner).getOrElse(sys.error(s"Module $owner not found!"))
    val sym = ConstructorSymbol(Identifier.fresh(name), sym_owner)
    defsByName += (owner, name) -> sym
    sym.signature(
      ConstrSig(
        argTypes,
        parent,
        constrIndexes.next(parent)
      )
    )
    sym

  /**
    *
    * @param owner
    * @param name
    * @return
    */
  def getConstructor(owner: String, name: String): Option[Symbol] =
    defsByName.get(owner, name)

  /**
    *
    * @param owner
    * @param name
    * @param argTypes
    * @param retType
    * @return
    */
  def addFunction(owner: String, name: String, argTypes: List[TypeTree], retType: TypeTree): Symbol =
    val sym_owner = getModule(owner).getOrElse(sys.error(s"Module $owner not found!"))
    val sym = FunctionSymbol(Identifier.fresh(name), sym_owner)
    val idx = funIndexes.incrementAndGet()
    defsByName += (owner, name) -> sym
    sym.signature(FunSig(argTypes, retType, idx))
    sym

  def addInfixFunction(owner: String, name: String, argTypes: List[TypeTree], retType: TypeTree): Symbol =
    val sym_owner = getModule(owner).getOrElse(sys.error(s"Module $owner not found!"))
    val sym = FunctionSymbol(Identifier.fresh(name), sym_owner, true)
    val idx = funIndexes.incrementAndGet()
    defsByName += (owner, name) -> sym
    sym.signature(FunSig(argTypes, retType, idx))
    sym

  /**
    *
    * @param owner
    * @param name
    * @return
    */
  def getFunction(owner: String, name: String): Option[Symbol] =
    // TODO HR : Still need to check if it's a function
    defsByName.get((owner, name))

}

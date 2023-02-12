package amyc.analyzer

import amyc.*
import amyc.core.*
import amyc.core.Signatures.*
import amyc.core.Symbols.*
import amyc.analyzer.Transformer.*
import amyc.ast.NominalTreeModule.ParamDef
import amyc.ast.SymbolicTreeModule.TypeTree
import amyc.utils.UniqueCounter

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

// A class that represents a dictionary of symbols for an Amy program
class SymbolTable :
  /* Atomic counters to index constructors and functions */
  /* TODO HR : Remove both counters as those are specific to wasm backend */
  private val constrIndexes = new UniqueCounter[Symbol]
  private val funIndexes = new AtomicInteger

  // map a definition (owner, def) to its symbol
  private val defsByName : mutable.HashMap[(String, String), Symbol] =
    mutable.HashMap.empty
  // maps a nominal representation of the tree to its symbol
  private val modules : mutable.HashMap[String, ModuleSymbol] =
    mutable.HashMap.empty

  // ====================================== REGISTER METHODS ======================================

  /* register a new module */
  def addModule(name: String)(using Context): ModuleSymbol =
    if modules.contains(name) then
      reporter.fatal(s"module $name is already defined")
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
  /*
    - OWNER
    - NAME
    - MODS
    - PARAM INFO
    - RET
  */
  def addFunction(owner: ModuleSymbol, name: String, mods: List[String], params: List[ParamDef], rte: TypeTree)
                 (using Context): FunctionSymbol=
    val idx = funIndexes.incrementAndGet()
    val id = Identifier.fresh(name)
    val sym = FunctionSymbol(id, owner, mods, idx)
    defsByName += (owner.name, name) -> sym
    val paramsym = params.map(p => ParameterSymbol(Identifier.fresh(p.name), sym, transformType(p.tt, owner.name)))
    sym.info(paramsym, rte)
    sym

  // ====================================== SAFE METHODS ==========================================

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

  // ===================================== FAIL METHODS ===========================================

  def module(name: String)(using Context): ModuleSymbol =
    getModule(name) getOrElse {
      reporter.fatal(s"Definition of module $name is missing")
    }

  def `type`(module: String, name: String)(using Context): TypeSymbol =
    getType(module, name) getOrElse {
      reporter.fatal(s"Definition of type $name in module $module is missing")
    }

  def `type`(module: Symbol, name: String)(using Context): TypeSymbol =
    `type`(module.name, name)

  def function(module: Symbol, name: String)(using Context): FunctionSymbol =
    function(module.name, name)

  def function(module: String, name: String)(using Context): FunctionSymbol =
    getFunction(module, name) getOrElse {
      reporter.fatal(s"Definition of function $name in module $module is missing")
    }

  def constructor(module: String, name: String)(using Context): ConstructorSymbol =
    getConstructor(module, name) getOrElse {
      reporter.fatal(s"Definition of constructor $name in module $module is missing")
    }

  def constructor(module: ModuleSymbol, name: String)(using Context): ConstructorSymbol =
    constructor(module.name, name)

package amyc
package analyzer

import Transformer.*
import core.*
import core.Context.*
import core.Symbols.*
import core.Types.*
import ast.{NominalTreeModule as N, SymbolicTreeModule as S}

import scala.collection.mutable

// A class that represents a dictionary of symbols for an Amy program
class SymbolTable :
  
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
  def addType(owner: String, name: String)(using Context): Symbol =
    val sym_owner = module(owner)
    val sym = TypeSymbol(Identifier.fresh(name), sym_owner)
    defsByName += (owner, name) -> sym
    sym

  /* register a new constructor */
  def addConstructor(owner: String, name: String, params: List[N.ValParamDef], parent: Symbol)
                    (using Context): Symbol =
    val sym_owner = module(owner)
    val sym = ConstructorSymbol(Identifier.fresh(name), sym_owner, parent)
    defsByName += (owner, name) -> sym
    sym.info {
      for p <- params yield
        ParameterSymbol(Identifier.fresh(p.name), sym, transformType(Scope.fresh)(p.tt, owner))
    }
    sym

  /* register a new function */
  /*
    - OWNER
    - NAME
    - MODS
    - PARAM INFO
    - RET
  */
  def addFunction(owner: ModuleSymbol, name: String, mods: List[String], tparams: List[N.TypeParamDef], vparams: List[N.ValParamDef], rte: N.TypeTree)
                 (using Context): FunctionSymbol=
    val id = Identifier.fresh(name)
    val sym = FunctionSymbol(id, owner, mods)
    defsByName += (owner.name, name) -> sym
    val symtparams = tparams.map(p => ParameterSymbol(Identifier.fresh(p.name), sym, S.TTypeTree(Types.NoType)))
    ctx.withScope(sym, Scope.fresh.withTParams(symtparams.map(sym => (sym.name, sym)).toMap))
    val symvparams = vparams.map(p => ParameterSymbol(Identifier.fresh(p.name), sym, transformType(ctx.scope(sym))(p.tt, owner.name)))
    sym.info(symtparams, symvparams, transformType(ctx.scope(sym))(rte, owner.name))
    sym

  // ====================================== SAFE METHODS ==========================================

  /* fetch symbol of a module */
  def getModule(name: String): Option[ModuleSymbol] =
    modules.get(name)

  /* fetch the symbol of a type */
  def getType(owner: String, name: String): Option[TypeSymbol] =
    defsByName get (owner, name) flatMap :
      case sym: TypeSymbol => Some(sym)
      case _ => None

  /* fetch the symbol of a constructor */
  def getConstructor(owner: String, name: String): Option[ConstructorSymbol] =
    defsByName get(owner, name) flatMap :
      case sym: ConstructorSymbol => Some(sym)
      case _ => None

  /* fetch the symbol of a function */
  def getFunction(owner: String, name: String): Option[FunctionSymbol] =
    defsByName get (owner, name) flatMap :
      case sym: FunctionSymbol => Some(sym)
      case _ => None

  // ===================================== FAIL METHODS ===========================================

  def module(name: String)(using Context): ModuleSymbol =
    getModule(name) getOrElse :
      reporter.fatal(s"Definition of module $name is missing")

  def `type`(module: String, name: String)(using Context): TypeSymbol =
    getType(module, name) getOrElse :
      reporter.fatal(s"Definition of type $name in module $module is missing")

  def `type`(module: Symbol, name: String)(using Context): TypeSymbol =
    `type`(module.name, name)

  def function(module: Symbol, name: String)(using Context): FunctionSymbol =
    function(module.name, name)

  def function(module: String, name: String)(using Context): FunctionSymbol =
    getFunction(module, name) getOrElse :
      reporter.fatal(s"Definition of function $name in module $module is missing")

  def constructor(module: String, name: String)(using Context): ConstructorSymbol =
    getConstructor(module, name) getOrElse :
      reporter.fatal(s"Definition of constructor $name in module $module is missing")

  def constructor(module: ModuleSymbol, name: String)(using Context): ConstructorSymbol =
    constructor(module.name, name)

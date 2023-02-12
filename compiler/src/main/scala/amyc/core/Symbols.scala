package amyc.core

import amyc.ast.SymbolicTreeModule
import amyc.ast.SymbolicTreeModule.*
import amyc.core.Identifier
import amyc.core.Signatures.ConstrSig
import amyc.core.Types.Type

object Symbols:

  trait Symbol(val id: Identifier, owner: Symbol):
    final def name: String = id.name
    final def fullName: String = id.fullName

  /* Used for modules */
  case class ModuleSymbol(override val id: Identifier)
      extends Symbol(id, NoSymbol)

  /* Used for types */
  case class TypeSymbol(override val id: Identifier, owner: ModuleSymbol)
      extends Symbol(id, owner)

  /* Used for functions */

  case class FunctionSymbol(
      override val id: Identifier,
      owner: ModuleSymbol,
      private val mods: List[String]
  ) extends Symbol(id, owner):
    private var _param: List[ParameterSymbol] = compiletime.uninitialized
    private var _rte: TypeTree = compiletime.uninitialized

    def info(param: List[ParameterSymbol], rte: TypeTree): this.type =
      _param = param
      _rte = rte
      this

    def info: List[ParameterSymbol] = _param
    final def rte: TypeTree = _rte

    override val toString: String = id.name

    infix def is(mod: String): Boolean =
      mods contains mod

  /* Used for constructors */
  case class ConstructorSymbol(override val id: Identifier, owner: ModuleSymbol)
      extends Symbol(id, owner):
    private var _sig: ConstrSig = compiletime.uninitialized

    def signature(fs: ConstrSig): ConstructorSymbol =
      _sig = fs
      this

    def signature: ConstrSig = _sig

    final def param: List[TypeTree] = signature.argTypes

    final def rte: TypeTree = signature.retType

  /**
    * Used for function parameters
    * @param id
    * @param owner
    * @param tpe
    */
  case class ParameterSymbol(
      override val id: Identifier,
      owner: FunctionSymbol,
      tpe: TypeTree
  ) extends Symbol(id, owner)

  /* Used for local variables, parameters and patterns */
  /* TODO HR : owner symbol should be the enclosed symbol (either a function, a module or a module) */
  case class LocalSymbol(override val id: Identifier)
      extends Symbol(id, NoSymbol)

  // TODO HR : Remove null from id
  case object NoSymbol extends Symbol(null, NoSymbol)

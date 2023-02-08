package amyc.core

import amyc.ast.SymbolicTreeModule
import amyc.ast.SymbolicTreeModule.*
import amyc.core.Identifier
import amyc.core.Signatures.{ConstrSig, FunSig}

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
      is_infix: Boolean = false
  ) extends Symbol(id, owner):
    private var _sig: FunSig = compiletime.uninitialized
    def signature(fs: FunSig): FunctionSymbol =
      _sig = fs
      this

    def signature: FunSig = _sig
    final def param: List[TypeTree] = signature.argTypes
    final def rte: TypeTree = signature.retType
    final def idx: Int = signature.idx

    override val toString: String = id.name

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

    final def idx: Int = signature.idx

  /* Used for local variables, parameters and patterns */
  /* TODO HR : owner symbol should be the enclosed symbol (either a function, a module or a module) */
  case class LocalSymbol(override val id: Identifier)
      extends Symbol(id, NoSymbol)

  // TODO HR : Remove null from id
  case object NoSymbol extends Symbol(null, NoSymbol)

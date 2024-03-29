package amyc.core

import amyc.ast.SymbolicTreeModule
import amyc.ast.SymbolicTreeModule.*
import amyc.core.Identifier
import amyc.core.Types.Type

object Symbols:

  trait Symbol(val id: Identifier, val owner: Symbol):
    final def name: String = id.name
    final def fullName: String = id.fullName

  /* Used for modules */
  case class ModuleSymbol(override val id: Identifier)
      extends Symbol(id, NoSymbol)

  /* Used for types */
  case class TypeSymbol(override val id: Identifier, override val owner: ModuleSymbol)
      extends Symbol(id, owner)

  // ==============================================================================================
  // =================================== APPLICATION SYMBOLS ======================================
  // ==============================================================================================

  abstract class ApplicationSymbol(id: Identifier, owner: ModuleSymbol) extends Symbol(id, owner):
    def param: List[ParameterSymbol]
    def rte : TypeTree

  /* Used for functions */
  final case class FunctionSymbol(
      override val id: Identifier,
      override val owner: ModuleSymbol,
      private val mods: List[String]
  ) extends ApplicationSymbol(id, owner):

    private var _param: List[ParameterSymbol] = compiletime.uninitialized
    private var _rte: TypeTree = compiletime.uninitialized

    def info(param: List[ParameterSymbol], rte: TypeTree): this.type =
      _param = param
      _rte = rte
      this

    override def param: List[ParameterSymbol] = _param
    override def rte: TypeTree = _rte

    override val toString: String = id.name

    infix def is(mod: String): Boolean =
      mods contains mod

  /* Used for constructors */
  final case class ConstructorSymbol(override val id: Identifier, override val owner: ModuleSymbol, parent: Symbol)
      extends ApplicationSymbol(id, owner):

    private var _param: List[ParameterSymbol] = compiletime.uninitialized

    def info(param: List[ParameterSymbol]): this.type =
      _param = param
      this

    override def param: List[ParameterSymbol] = _param

    override def rte: TypeTree = ClassTypeTree(parent)

  // ==============================================================================================
  // ===================================== LOCAL SYMBOLS ==========================================
  // ==============================================================================================


  /**
    * Used for function parameters
    * @param id
    * @param owner
    * @param tpe
    */
  case class ParameterSymbol(
      override val id: Identifier,
      override val owner: ApplicationSymbol,
      tpe: TypeTree
  ) extends Symbol(id, owner)

  /* Used for local variables, parameters and patterns */
  /* TODO HR : owner symbol should be the enclosed symbol (either a function, a module or a module) */
  case class LocalSymbol(override val id: Identifier)
      extends Symbol(id, NoSymbol)

  // TODO HR : Remove null from id
  case object NoSymbol extends Symbol(null, NoSymbol)

package amyc.core

import amyc.core.Identifier

object Symbols:

  trait Symbol(val id: Identifier, owner: Symbol) :
    final def name: String = id.name
    final def fullName: String = id.fullName

  /* Used for modules */
  case class ModuleSymbol(override val id: Identifier) extends Symbol(id, NoSymbol)

  /* Used for types */
  case class TypeSymbol(override val id: Identifier, owner : ModuleSymbol) extends Symbol(id, owner)

  /* Used for functions */

  case class FunctionSymbol(override val id: Identifier) extends Symbol(id, NoSymbol):
    override val toString: String = id.name

  /* Used for constructors */
  case class ConstructorSymbol(override val id: Identifier) extends Symbol(id, NoSymbol)

  /* Used for local variables, parameters and patterns */
  case class LocalSymbol(override val id: Identifier) extends Symbol(id, NoSymbol)

  // TODO HR : Remove null from id
  case object NoSymbol extends Symbol(null, NoSymbol)


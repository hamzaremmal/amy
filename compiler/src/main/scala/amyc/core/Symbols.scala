package amyc.core

import amyc.core.Identifier

object Symbols:

  trait Symbol(val id: Identifier) :
    final def name: String = id.name
    final def fullName: String = id.fullName

  /* Used for modules */
  case class ModuleSymbol(override val id: Identifier) extends Symbol(id)

  /* Used for types */
  case class TypeSymbol(override val id: Identifier) extends Symbol(id)

  /* Used for functions */

  case class FunctionSymbol(override val id: Identifier) extends Symbol(id):
    override val toString: String = id.name

  /* Used for constructors */
  case class ConstructorSymbol(override val id: Identifier) extends Symbol(id)

  /* Used for local variables, parameters and patterns */
  case class LocalSymbol(override val id: Identifier) extends Symbol(id)


package amyc.core

import amyc.core.Identifier

object Symbols:

  trait Symbol(val id: Identifier)

  case class ModuleSymbol(override val id: Identifier) extends Symbol(id)

  case class TypeSymbol(override val id: Identifier) extends Symbol(id)

  case class FunctionSymbol(override val id: Identifier) extends Symbol(id)

  case class ConstructorSymbol(override val id: Identifier) extends Symbol(id)


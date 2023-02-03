package amyc.ast

import amyc.core.Identifier

/**
  * A module containing trees where the names have been resolved to unique identifiers.
  * Both Name and ModuleName are instantiated to Identifier.
  */
object SymbolicTreeModule extends TreeModule :
  type Name = Identifier
  type QualifiedName = Identifier
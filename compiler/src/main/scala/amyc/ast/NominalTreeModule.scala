package amyc.ast

/**
  * A module containing trees where the names have not been resolved.
  * Instantiates Name to String and QualifiedName to a pair of Strings
  * representing (module, name) (where module is optional)
  */
object NominalTreeModule extends TreeModule :
  type Name = String
  case class QualifiedName(module: Option[String], name: String)

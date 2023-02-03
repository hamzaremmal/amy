package amyc.core

import amyc.{reporter, symbols}

class StdDefinitions(using Context) :

  private type I = Identifier
  
  lazy val StdModule    : I = ofModule("Std")
  lazy val UnnamedModule : I = ofModule("unnamed")

  lazy val StringType  : I = ofType(UnnamedModule, "String")
  lazy val UnitType    : I = ofType(UnnamedModule, "Unit")
  lazy val IntType     : I = ofType(UnnamedModule, "Int")
  lazy val BooleanType : I = ofType(UnnamedModule, "Boolean")
  
  // ==============================================================================================
  // ===================================== HELPER METHODS =========================================
  // ==============================================================================================

  def ofModule(name: String): I =
    symbols.getModule(name).getOrElse {
      reporter.fatal(s"Definition of module $name is missing")
    }

  def ofType(module: String, name: String): I =
    symbols.getType(module, name).getOrElse {
      reporter.fatal(s"Definition of type $name in module $module is missing")
    }

  def ofType(module: Identifier, name: String): I = ofType(module.name, name)

object StdDefinitions:

  /** Return a brand new set of definitions to use */
  def stdDef(using Context) = new StdDefinitions
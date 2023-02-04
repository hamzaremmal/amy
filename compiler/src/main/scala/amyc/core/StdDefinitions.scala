package amyc.core

import amyc.{reporter, symbols}

import amyc.core.Symbols.*

class StdDefinitions(using Context) :

  private type I = Symbol

  // ----------------- Std.amy ------------------
  lazy val StdModule         : I = ofModule("Std")

  lazy val Std_printInt      : I = ofFunction(StdModule, "printInt")
  lazy val Std_printString   : I = ofFunction(StdModule, "printString")
  lazy val Std_readString    : I = ofFunction(StdModule, "readString")
  lazy val Std_readInt       : I = ofFunction(StdModule, "readInt")
  lazy val Std_intToString   : I = ofFunction(StdModule, "intToString")
  lazy val Std_digitToString : I = ofFunction(StdModule, "digitToString")

  // ----------------- unnamed ------------------
  lazy val UnnamedModule     : I = ofModule("<unnamed>")

  lazy val StringType        : I = ofType(UnnamedModule, "String")
  lazy val UnitType          : I = ofType(UnnamedModule, "Unit")
  lazy val IntType           : I = ofType(UnnamedModule, "Int")
  lazy val BooleanType       : I = ofType(UnnamedModule, "Boolean")

  lazy val binop_+           : I = ofFunction(UnnamedModule, "+")
  lazy val binop_-           : I = ofFunction(UnnamedModule, "-")
  lazy val binop_*           : I = ofFunction(UnnamedModule, "*")
  lazy val binop_/           : I = ofFunction(UnnamedModule, "/")
  lazy val binop_%           : I = ofFunction(UnnamedModule, "%")
  lazy val binop_<           : I = ofFunction(UnnamedModule, "<")
  lazy val binop_<=          : I = ofFunction(UnnamedModule, "<=")
  lazy val binop_&&          : I = ofFunction(UnnamedModule, "&&")
  lazy val binop_||          : I = ofFunction(UnnamedModule, "||")
  lazy val binop_==          : I = ofFunction(UnnamedModule, "==")
  lazy val binop_++          : I = ofFunction(UnnamedModule, "++")

  // --------------------- ..... --------------------------------

  // TODO HR : Add a new module here

  // ==============================================================================================
  // ===================================== HELPER METHODS =========================================
  // ==============================================================================================

  private def ofModule(name: String): I =
    symbols.getModule(name).getOrElse {
      reporter.fatal(s"Definition of module $name is missing")
    }

  private def ofType(module: String, name: String): I =
    symbols.getType(module, name).getOrElse {
      reporter.fatal(s"Definition of type $name in module $module is missing")
    }

  private def ofType(module: Symbol, name: String): I = ofType(module.name, name)

  private def ofFunction(module: Symbol, name: String): I =
    ofFunction(module.name, name)

  private def ofFunction(module: String, name: String): I =
    symbols.getFunction(module, name).getOrElse {
      reporter.fatal(s"Definition of type $name in module $module is missing")
    }._1

object StdDefinitions:

  /** Return a brand new set of definitions to use */
  def stdDef(using Context) = new StdDefinitions
package amyc.core

import amyc.symbols

import amyc.core.Symbols.*

object StdDefinitions:
  /** Return a brand new set of definitions to use */
  def stdDef(using Context) = new StdDefinitions

class StdDefinitions(using Context) :

  private final val sym = symbols
  import sym.*

  private type S  = Symbol
  private type MS = ModuleSymbol
  private type TS = TypeSymbol
  private type FS = FunctionSymbol
  private type CS = ConstructorSymbol

  // ----------------- Std.amy ------------------
  lazy val StdModule         : MS = module("Std")

  lazy val Std_printInt      : FS = function(StdModule, "printInt")
  lazy val Std_printString   : FS = function(StdModule, "printString")
  lazy val Std_readString    : FS = function(StdModule, "readString")
  lazy val Std_readInt       : FS = function(StdModule, "readInt")
  lazy val Std_intToString   : FS = function(StdModule, "intToString")
  lazy val Std_digitToString : FS = function(StdModule, "digitToString")

  // ---------------- String.amy ----------------
  lazy val StringModule      : MS = module("String")
  lazy val StringType        : TS = `type`(StringModule, "String")
  
  lazy val String_length     : FS = function(StringModule, "length")
  
  // ----------------- Boolean.amy --------------
  lazy val BooleanModule     : MS = module("Boolean")
  lazy val BooleanType       : TS = `type`(BooleanModule, "Boolean")
  // ----------------- Int.amy ------------------
  lazy val IntModule         : MS = module("Int")
  lazy val IntType           : TS = `type`(IntModule, "Int")
  // ----------------- Unit.amy -----------------
  lazy val UnitModule        : MS = module("Unit")
  lazy val UnitType          : TS = `type`(UnitModule, "Unit")
  // ----------------- unnamed ------------------
  lazy val UnnamedModule     : MS = module("unnamed")
  lazy val binop_+           : FS = function(UnnamedModule, "+")
  lazy val binop_-           : FS = function(UnnamedModule, "-")
  lazy val binop_*           : FS = function(UnnamedModule, "*")
  lazy val binop_/           : FS = function(UnnamedModule, "/")
  lazy val binop_%           : FS = function(UnnamedModule, "%")
  lazy val binop_<           : FS = function(UnnamedModule, "<")
  lazy val binop_<=          : FS = function(UnnamedModule, "<=")
  lazy val binop_&&          : FS = function(UnnamedModule, "&&")
  lazy val binop_||          : FS = function(UnnamedModule, "||")
  lazy val binop_==          : FS = function(UnnamedModule, "==")
  lazy val binop_++          : FS = function(UnnamedModule, "++")

  // --------------------- ..... ----------------

  // TODO HR : Add a new module here

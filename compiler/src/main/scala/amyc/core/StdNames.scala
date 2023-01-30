package amyc.core

import amyc.core.Context
import amyc.*

import scala.annotation.targetName

object StdNames {

  private type I = Identifier

  private implicit def str2id(s: String): I =
    Identifier.fresh(s)

  // ==============================================================================================
  // ==================================== BINARY OPERATORS ========================================
  // ==============================================================================================

  // TODO HR: This object should contain symbols, not identifiers
  // TODO HR: This will make the typer easier to write since symbols
  // TODO HR: already contains the type information

  @targetName("plus")   lazy val +     : I = "+"
  @targetName("minus")  lazy val -     : I = "-"
  @targetName("times")  lazy val *     : I = "*"
  @targetName("div")    lazy val /     : I = "/"
  @targetName("mod")    lazy val %     : I = "%"
  @targetName("less")   lazy val <     : I = "<"
  @targetName("lessEq") lazy val <=    : I = "<="
  @targetName("and")    lazy val &&    : I = "&&"
  @targetName("or")     lazy val ||    : I = "||"
  @targetName("eq")     lazy val eq_== : I = "=="
  @targetName("concat") lazy val ++    : I = "++"


  def binOp(op: String)(using Context): Identifier =
    op match
      case "+"  => +
      case "-"  => -
      case "*"  => *
      case "/"  => /
      case "%"  => %
      case "<"  => <
      case "<=" => <=
      case "&&" => &&
      case "||" => ||
      case "==" => eq_==
      case "++" => ++
      case _ =>
        reporter.fatal(s"TODO: ADD THE ERROR MESSAGE HERE")

  // ==============================================================================================
  // TYPES
  // ==============================================================================================

  lazy val IStringType  : I = "String"
  lazy val IUnitType    : I = "Unit"
  lazy val IIntType     : I = "Int"
  lazy val IBooleanType : I = "Boolean"


}

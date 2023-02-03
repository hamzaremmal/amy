package amyc.core

import amyc.core.Types.*
import amyc.core.StdDefinitions.*

class StdTypes(using Context):
  
  private type T = Type
  
  lazy val IntType     : T = ClassType(stdDef.IntType)
  lazy val StringType  : T = ClassType(stdDef.StringType)
  lazy val UnitType    : T = ClassType(stdDef.UnitType)
  lazy val BooleanType : T = ClassType(stdDef.BooleanType)


object StdTypes :
  def stdType(using Context) = new StdTypes


package amyc
package core

import amyc.core.Types.*
import amyc.core.StdDefinitions.*

class StdTypes(using Context):
  
  private type T = Type
  
  lazy val IntType     : T = ClassType(stdDef.IntType.id)
  lazy val StringType  : T = ClassType(stdDef.StringType.id)
  lazy val UnitType    : T = ClassType(stdDef.UnitType.id)
  lazy val BooleanType : T = ClassType(stdDef.BooleanType.id)


object StdTypes :
  def stdType(using Context) = new StdTypes


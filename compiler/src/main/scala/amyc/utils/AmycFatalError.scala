package amyc.utils

import amyc.*
import amyc.core.Context

case class AmycFatalError(msg: String) extends Exception(msg)

object error {
  
  def checkAmycErrors[A](body: => A)(using Context): A =
    try{
      body
    } catch
      case AmycFatalError(msg) =>
        sys.exit(1)
        
  
}

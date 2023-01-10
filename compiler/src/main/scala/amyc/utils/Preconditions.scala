package amyc.utils

import amyc.*
import amyc.core.Context

object Preconditions {

  inline def require(cc: Boolean)(using Context): Unit =
    if !cc then reporter.fatal(s"require error")

  inline def require[A](cc: Boolean)(body: => A)(using Context): A =
    if(cc)
      body
    else
      reporter.fatal("require error")


}

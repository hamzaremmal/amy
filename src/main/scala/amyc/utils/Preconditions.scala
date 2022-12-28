package amyc.utils

import amyc.*
import amyc.core.Context

object Preconditions {

  inline def require[A](cc: Boolean)(body: => A)(using Context) =
    if(cc)
      body
    else
      reporter.fatal("require error")


}

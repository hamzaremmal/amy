package amyc
package backend
package wasm
package gen

import amyc.backend.wasm.Modules.*
import amyc.core.Context

object WATFile :

  def apply(module: Module)(using Context): String = ModulePrinter(module)

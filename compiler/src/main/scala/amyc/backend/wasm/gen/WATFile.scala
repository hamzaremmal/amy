package amyc.backend.wasm.gen

import amyc.backend.wasm.Modules.*
import amyc.core.Context

object WATFile :

  def apply(module: Module)(using Context): String = ModulePrinter(module)

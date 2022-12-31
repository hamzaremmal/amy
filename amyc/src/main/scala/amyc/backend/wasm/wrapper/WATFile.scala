package amyc.backend.wasm.wrapper

import amyc.core.Context
import amyc.backend.wasm.Module
import amyc.backend.wasm.ModulePrinter

object WATFile {

  def apply(module: Module)(using Context): String = ModulePrinter(module)

}

package amyc.backend.wasm.builtin

import amyc.backend.wasm.Function
import amyc.backend.wasm.Instructions.Code
import amyc.backend.wasm.utils.LocalsHandler
import amyc.backend.wasm.utils.Utils.fullName
import amyc.core.Context
import amyc.core.Symbols.FunctionSymbol
import amyc.{reporter, symbols}

abstract class BuiltInModule {

  val owner: String

  final type BuiltIn = Context ?=> Function

  def builtInForSym(name: String)(code: LocalsHandler ?=> Code): BuiltIn =
    val sym = symbols
      .getFunction(owner, name)
      .getOrElse {
        reporter.fatal(
          s"BuiltIn function ${owner}_$name is not defined - symbol is missing"
        )
      }
      .asInstanceOf[FunctionSymbol]
    Function(fullName(sym.owner.id, sym), sym.param.length, false, sym.idx) {
      code
    }

}

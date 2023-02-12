package amyc.backend.wasm.builtin

import amyc.{reporter, symbols}
import amyc.core.Context
import amyc.core.Symbols.*
import amyc.backend.wasm.Function
import amyc.backend.wasm.Instructions.{Code, i32}
import amyc.backend.wasm.types.result
import amyc.backend.wasm.utils.LocalsHandler
import amyc.backend.wasm.utils.Utils.fullName

abstract class BuiltInModule {

  lazy val owner: Context ?=> Symbol

  final type BuiltIn = Context ?=> Function

  def builtInForSymbol(name: String)(code: LocalsHandler ?=> Code): BuiltIn =
    val sym = symbols.function(owner, name)
    Function.forSymbol(sym, Some(result(i32)))(code)

}

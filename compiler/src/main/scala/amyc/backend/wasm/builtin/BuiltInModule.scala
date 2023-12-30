package amyc
package backend
package wasm
package builtin

import amyc.{reporter, symbols}
import amyc.core.Context
import amyc.core.Symbols.*
import amyc.backend.wasm.Instructions.{Code, i32}
import amyc.backend.wasm.Modules.*
import amyc.backend.wasm.Types.result
import amyc.backend.wasm.handlers.{LocalsHandler, ModuleHandler}
import amyc.backend.wasm.utils.*

abstract class BuiltInModule {

  lazy val owner: Context ?=> ModuleSymbol

  final type BuiltIn = (Context, ModuleHandler) ?=> Function

  def builtInForSymbol(name: String)(code: LocalsHandler ?=> Code): BuiltIn =
    val sym = symbols.function(owner, name)
    Function.forSymbol(sym, Some(result(i32)))(code)

}

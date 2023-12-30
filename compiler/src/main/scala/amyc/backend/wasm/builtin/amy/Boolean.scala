package amyc
package backend
package wasm
package builtin
package amy

import amyc.backend.wasm.Instructions.{Code, i32}
import amyc.backend.wasm.builtin.BuiltInModule
import amyc.core.StdDefinitions.*
import amyc.core.Symbols.*
import amyc.core.Context

object Boolean extends BuiltInModule :

  override lazy val owner: Context ?=> ModuleSymbol = stdDef.BooleanModule

  inline def mkBoolean(inline b: Boolean): Code =
    i32.const(if b then 1 else 0)


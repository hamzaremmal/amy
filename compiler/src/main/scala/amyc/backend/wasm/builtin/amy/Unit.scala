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

object Unit extends BuiltInModule :

  override lazy val owner: Context ?=> ModuleSymbol = stdDef.UnitModule

  inline def mkUnit : Code = i32.const(0)

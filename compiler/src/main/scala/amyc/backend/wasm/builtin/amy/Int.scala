package amyc
package backend
package wasm
package builtin
package amy

import amyc.backend.wasm.builtin.BuiltInModule
import amyc.core.StdDefinitions.*
import amyc.core.Symbols.*
import amyc.core.Context

object Int extends BuiltInModule :

  override lazy val owner: Context ?=> ModuleSymbol = stdDef.IntModule

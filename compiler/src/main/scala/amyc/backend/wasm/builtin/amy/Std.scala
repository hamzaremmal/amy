package amyc
package backend
package wasm
package builtin
package amy

import amyc.core.Context
import amyc.core.Symbols.*
import amyc.core.StdDefinitions.*
import amyc.backend.wasm.*
import amyc.backend.wasm.builtin.BuiltIn.*
import amyc.backend.wasm.builtin.BuiltInModule
import amyc.backend.wasm.Instructions.*
import amyc.backend.wasm.utils.*

object Std extends BuiltInModule :

  override lazy val owner: Context ?=> ModuleSymbol = stdDef.StdModule

  lazy val readString: BuiltIn =
    builtInForSymbol("readString") {
      // We need to use the weird interface of javascript read string:
      // we pass the old memory boundary and get the new one.
      // In the end we have to return the old, where the fresh string lies.
      mh.dynamic_boundary <:>
      mh.dynamic_boundary {
        mh.dynamic_boundary <:>
          // This function will allocate the space
          // This function return the next free space in memory
          call("js_readString0")
        }
    }

  // We know we have to create a string of total size 4 (digit code + padding), so we do it all together
  // We do not need to shift the digit due to little endian structure!
  lazy val digitToString: BuiltIn =
    builtInForSymbol("digitToString") {
      // Leave the base address in the stack
      mh.dynamic_boundary <:>
      // Allocate space for one letter
      mh.dynamic_alloc(1) <:>
      // Compute the ASCII code for the character
      local.get(0) <:> i32.const('0'.toInt) <:> i32.add <:>
      // Store the ASCII code in memory
      i32.store
    }
package amyc.backend.wasm.builtin.amy

import amyc.backend.wasm
import amyc.backend.wasm.builtin.BuiltIn.*
import amyc.backend.wasm.builtin.BuiltInModule
import amyc.backend.wasm.instructions.Instructions.{Call, Store}
import amyc.backend.wasm.instructions.variable.*
import amyc.backend.wasm.types.Integer.i32
import amyc.backend.wasm.utils.Utils.memoryBoundary

object Std extends BuiltInModule {

  override val owner = "Std"

  lazy val readString: BuiltIn =
    builtInForSym("readString") {
      // We need to use the weird interface of javascript read string:
      // we pass the old memory boundary and get the new one.
      // In the end we have to return the old, where the fresh string lies.
      global.get(memoryBoundary) <:>
        global.get(memoryBoundary) <:>
        Call("js_readString0") <:>
        global.set(memoryBoundary)
    }

  lazy val digitToString: BuiltIn =
    builtInForSym("digitToString") {
      // We know we have to create a string of total size 4 (digit code + padding), so we do it all together
      // We do not need to shift the digit due to little endian structure!
      global.get(memoryBoundary) <:> local.get(0) <:> i32.const('0'.toInt) <:> i32.add <:> Store <:>
        // Load memory boundary to stack, then move it by 4
        global.get(memoryBoundary) <:>
        global.get(memoryBoundary) <:> i32.const(4) <:> i32.add <:> global.set(memoryBoundary)
    }

}

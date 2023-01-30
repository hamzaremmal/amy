package amyc.backend.wasm.builtin.amy

import amyc.core.Types.StringType
import amyc.backend.wasm.builtin.BuiltInModule
import amyc.backend.wasm.Instructions.*
import amyc.backend.wasm.utils.Utils.{getFreshLabel, incr, memoryBoundary}
import amyc.backend.wasm.utils.lh
import amyc.core.Context
import amyc.symbols

object String extends BuiltInModule {

  override val owner = "String"

  override lazy val onLoad: Context ?=> Unit = {
    // TODO HR : This patch should be remove when introducing native functions and this method should
    // TODO HR : Should be registered as a native method instead of adding it here
    symbols.addModule("String") // This garanties not to fail since String is considered as a Keyword
    symbols.addFunction("String", "concat", StringType :: StringType :: Nil, StringType)
  }

  lazy val concat : BuiltIn =
    builtInForSym("concat") {
      val ptrS = lh.getFreshLocal
      val ptrD = lh.getFreshLocal

      val label = getFreshLabel()

      def mkLoop: Code = {
        val label = getFreshLabel()
        Loop(label) <:>
          // Load current character
          local.get(ptrS) <:> i32.load8_u <:>
          // If != 0
          `if`() <:>
          // Copy to destination
          local.get(ptrD) <:>
          local.get(ptrS) <:> i32.load8_u <:>
          i32.store8 <:>
          // Increment pointers
          incr(ptrD) <:> incr(ptrS) <:>
          // Jump to loop
          br(label) <:>
          `else`() <:> end <:> end
      }

      // Instantiate ptrD to previous memory, ptrS to first string
      global.get(memoryBoundary) <:>
        local.set(ptrD) <:>
        local.get(0) <:>
        local.set(ptrS) <:>
        // Copy first string
        mkLoop <:>
        // Set ptrS to second string
        local.get(1) <:>
        local.set(ptrS) <:>
        // Copy second string
        mkLoop <:>
        //
        // Pad with zeros until multiple of 4
        //
        Loop(label) <:>
        // Write 0
        local.get(ptrD) <:> i32.const(0) <:> i32.store8 <:>
        // Check if multiple of 4
        local.get(ptrD) <:> i32.const(4) <:> i32.rem_s <:>
        // If not
        `if`() <:>
        // Increment pointer and go back
        incr(ptrD) <:>
        br(label) <:> `else`() <:> end <:> end <:>
        // Put string pointer to stack, set new memory boundary and return
        global.get(memoryBoundary) <:> local.get(ptrD) <:> i32.const(1) <:> i32.add <:> global.set(memoryBoundary)
    }

}

package amyc.backend.wasm.builtin.amy

import amyc.symbols
import amyc.ast.SymbolicTreeModule.ClassTypeTree
import amyc.core.Symbols.*
import amyc.core.StdDefinitions.*
import amyc.core.Context
import amyc.backend.wasm.builtin.BuiltInModule
import amyc.backend.wasm.Instructions.*
import amyc.backend.wasm.utils.{lh, getFreshLabel, incr, memoryBoundary}

object String extends BuiltInModule {

  override lazy val owner: Context ?=> Symbol = stdDef.StringModule

  lazy val concat : BuiltIn =
    builtInForSymbol("concat") {
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

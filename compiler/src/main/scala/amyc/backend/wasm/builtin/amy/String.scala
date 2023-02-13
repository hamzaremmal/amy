package amyc.backend.wasm.builtin.amy

import amyc.symbols
import amyc.ast.SymbolicTreeModule.ClassTypeTree
import amyc.core.Symbols.*
import amyc.core.StdDefinitions.*
import amyc.core.Context
import amyc.backend.wasm.builtin.BuiltInModule
import amyc.backend.wasm.Instructions.*
import amyc.backend.wasm.indices.localidx
import amyc.backend.wasm.types.result
import amyc.backend.wasm.utils.{getFreshLabel, ift, incr, lh}

object String extends BuiltInModule :

  override lazy val owner: Context ?=> Symbol = stdDef.StringModule

  /* TODO HR : To be removed after implementing String::length */
  @deprecated
  private val memoryBoundary : Int = 0

  /**
    * Steps :
    * 1- Allocate enough space to store both strings (length(lhs) + length(rhs) + 1)
    * 2- Copy the first string without the '\0' character
    * 3- Copy the second string without the '\0' character
    * 4- Append '\0' character
    * 5- Return the base address of the String
    */
  lazy val concat : BuiltIn =
    builtInForSymbol("concat") {
      // Pointer to the source String
      val ptrS = lh.getFreshLocal
      // Pointer to the destination String
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

    /*
    fn length(str: String): Int =
      val size = 0
      val offset = 0
      label:
        if str[offset] != 0 then
          size++
          offset++
          br label
        else
          return size
      }
    */
  lazy val length: BuiltIn =
    builtInForSymbol("length"){
      val str : localidx = 0 // string is stored in the first parameter
      val size: localidx = lh.getFreshLocal // allocate space to store the size to return
      val offset : localidx = lh.getFreshLocal // allocate space to store the offset
      val label = getFreshLabel()
      Loop(label, Some(result(i32))) <:>
      ift(
        {
        local.get(str) <:>
        local.get(offset) <:>
        i32.add <:>
        i32.load <:>
        i32.const(0) <:>
        i32.ne
        },
        {
          incr(size) <:>
          incr(offset) <:>
          br(label) <:>
          i32.const(0) // HR: trick to satisfy typechecking of wat2wasm
        },
        {
          local.get(size)
        }
      ) <:> end
    }
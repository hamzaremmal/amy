package amyc.backend.wasm

import amyc.*
import amyc.core.Context
import amyc.backend.wasm.Function
import amyc.backend.wasm.Instructions.*

object BuiltIn {

  import amyc.backend.codegen.Utils.*

  private type F = Context ?=> Function

  lazy val wasmFunctions: Context ?=> List[Function] =
    concatImpl ::
    digitToStringImpl ::
    readStringImpl ::
    Nil

  // Built-in implementation of concatenation
  lazy val concatImpl: F = {
    // TODO HR : Add a symbol for this function
    //val sym = symbols.getFunction("S", "concat").get._2
    Function("String_concat", 2, false, 0) { lh =>
      val ptrS = lh.getFreshLocal()
      val ptrD = lh.getFreshLocal()
      val label = getFreshLabel()

      def mkLoop: Code = {
        val label = getFreshLabel()
        Loop(label) <:>
          // Load current character
          GetLocal(ptrS) <:> Load8_u <:>
          // If != 0
          If_void <:>
          // Copy to destination
          GetLocal(ptrD) <:>
          GetLocal(ptrS) <:> Load8_u <:>
          Store8 <:>
          // Increment pointers
          incr(ptrD) <:> incr(ptrS) <:>
          // Jump to loop
          Br(label) <:>
          Else <:>
          End <:>
          End
      }

      // Instantiate ptrD to previous memory, ptrS to first string
      GetGlobal(memoryBoundary) <:>
        SetLocal(ptrD) <:>
        GetLocal(0) <:>
        SetLocal(ptrS) <:>
        // Copy first string
        mkLoop <:>
        // Set ptrS to second string
        GetLocal(1) <:>
        SetLocal(ptrS) <:>
        // Copy second string
        mkLoop <:>
        //
        // Pad with zeros until multiple of 4
        //
        Loop(label) <:>
        // Write 0
        GetLocal(ptrD) <:> Const(0) <:> Store8 <:>
        // Check if multiple of 4
        GetLocal(ptrD) <:> Const(4) <:> Rem <:>
        // If not
        If_void <:>
        // Increment pointer and go back
        incr(ptrD) <:>
        Br(label) <:>
        Else <:>
        End <:>
        End <:>
        // Put string pointer to stack, set new memory boundary and return
        GetGlobal(memoryBoundary) <:> GetLocal(ptrD) <:> Const(1) <:> Add <:> SetGlobal(memoryBoundary)
    }
  }

  lazy val digitToStringImpl: F = {
    val sym = symbols.getFunction("Std", "digitToString").get._2
    Function("Std_digitToString", 1, false, sym.idx) { lh =>
      // We know we have to create a string of total size 4 (digit code + padding), so we do it all together
      // We do not need to shift the digit due to little endian structure!
      GetGlobal(memoryBoundary) <:> GetLocal(0) <:> Const('0'.toInt) <:> Add <:> Store <:>
        // Load memory boundary to stack, then move it by 4
        GetGlobal(memoryBoundary) <:>
        GetGlobal(memoryBoundary) <:> Const(4) <:> Add <:> SetGlobal(memoryBoundary)
    }
  }

  lazy val readStringImpl: F = {
    // TODO HR : Fix symbol for this method
    val sym = symbols.getFunction("Std", "readString").get._2
    Function("Std_readString", 0, false, 0) { _ =>
      // We need to use the weird interface of javascript read string:
      // we pass the old memory boundary and get the new one.
      // In the end we have to return the old, where the fresh string lies.
      GetGlobal(memoryBoundary) <:>
      GetGlobal(memoryBoundary) <:>
      Call("js_readString0") <:>
      SetGlobal(memoryBoundary)
    }
  }

}
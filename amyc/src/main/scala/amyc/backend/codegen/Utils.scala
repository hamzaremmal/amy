package amyc.backend.codegen

import amyc.analyzer.ConstrSig
import amyc.ast.Identifier
import amyc.backend.wasm
import amyc.backend.wasm.Instructions.*

// Utilities for CodeGen
object Utils {

  // The index of the global variable that represents the free memory boundary
  val memoryBoundary: Int = 0
  // # of global variables
  val globalsNo = 1

  // The default imports we will pass to a wasm Module
  val defaultImports: List[String] = List(
    "\"system\" \"printInt\" (func $Std_printInt (param i32) (result i32))",
    "\"system\" \"printString\" (func $Std_printString (param i32) (result i32))",
    "\"system\" \"readString0\" (func $js_readString0 (param i32) (result i32))",
    "\"system\" \"readInt\" (func $Std_readInt (result i32))",
    "\"system\" \"mem\" (memory 100)"
  )

  // We don't generate code for these functions in CodeGen (they are hard-coded here or in js wrapper)
  val builtInFunctions: Set[String] = Set(
    "Std_printInt",
    "Std_printString",
    "Std_digitToString",
    "Std_readInt",
    "Std_readString"
  )

  /** Utilities */
  // A globally unique name for definitions
  def fullName(owner: Identifier, df: Identifier): String = owner.name + "_" + df.name

  // A fresh label name
  def getFreshLabel(name: String = "label") = {
    Identifier.fresh(name).fullName
  }

  // ==============================================================================================
  // =============================== CODE GENERATOR FUNCTIONS =====================================
  // ==============================================================================================

  // Given a pointer to an ADT on the top of the stack,
  // will point at its field in index (and consume the ADT).
  // 'index' MUST be 0-based.
  inline def adtField(inline base: Code, inline index: Int): Code =
    withComment(s"adtField index: $index from base : $base"){
      base <:> Const(4* (index + 1)) <:> Add
    }

  // Increment a local variable
  inline def incr(local: Int) =
    GetLocal(local) <:> Const(1) <:> Add <:> SetLocal(local)

  inline def withComment(inline comment : String)(inline code: Code) : Code =
    Comment(comment) <:> code

  // Creates a known string constant s in memory
  def mkString(s: String): Code = {
    val size = s.length
    val padding = 4 - size % 4

    val completeS = s + 0.toChar.toString * padding

    val setChars = for ((c, ind) <- completeS.zipWithIndex.toList) yield {
      GetGlobal(memoryBoundary) <:> Const(ind) <:> Add <:>
        Const(c.toInt) <:> Store8
    }

    val setMemory =
      GetGlobal(memoryBoundary) <:> GetGlobal(memoryBoundary) <:> Const(size + padding) <:> Add <:>
        SetGlobal(memoryBoundary)

    withComment(s"mkString: $s"){
      setChars <:> setMemory
    }
  }

  inline def mkBoolean(inline b : Boolean): Code =
    Const(if b then 1 else 0)

  inline def mkUnit : Code = Const(0)

  inline def mkBinOp(inline lhs : Code, inline rhs : Code)(op : Instruction) : Code =
    lhs <:> rhs <:> op

  inline def equ(inline lhs: Code, inline rhs: Code) : Code =
    mkBinOp(lhs, rhs)(Eq)

  inline def and(inline lhs: Code, inline rhs: Code) : Code =
    ift(lhs, rhs, mkBoolean(false))

  inline def or(lhs: Code, rhs: Code) : Code =
    ift(lhs, mkBoolean(true), rhs)

  inline def loadGlobal(inline idx: Int) : Code =
    GetGlobal(idx) <:> Load

  inline def setGlobal(inline code: Code, inline idx: Int): Code =
    code <:> SetGlobal(idx)

  inline def loadLocal(inline idx: Int) : Code =
    GetLocal(idx) <:> Load

  inline def setLocal(inline code: Code, inline idx: Int): Code =
    code <:> SetLocal(idx)

  inline def constructor(inline const: ConstrSig) : Code =
    Const(const.index)

  inline def error(inline msg: Code) : Code =
    msg <:> Call("Std_printString") <:> Unreachable

  inline def ift(inline cond: Code, inline thenn: Code, elze: Code) =
    cond <:>
    If_i32 <:>
    thenn <:>
    Else <:>
    elze <:>
    End

  // ==============================================================================================
  // ================================ NATIVE FUNCTIONS IMPL =======================================
  // ==============================================================================================

  // Built-in implementation of concatenation
  val concatImpl: wasm.Function = {
    wasm.Function("String_concat", 2, false) { lh =>
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

  val digitToStringImpl: wasm.Function = {
    wasm.Function("Std_digitToString", 1, false) { lh =>
      // We know we have to create a string of total size 4 (digit code + padding), so we do it all together
      // We do not need to shift the digit due to little endian structure!
      GetGlobal(memoryBoundary) <:> GetLocal(0) <:> Const('0'.toInt) <:> Add <:> Store <:>
      // Load memory boundary to stack, then move it by 4
      GetGlobal(memoryBoundary) <:>
      GetGlobal(memoryBoundary) <:> Const(4) <:> Add <:> SetGlobal(memoryBoundary)
    }
  }

  val readStringImpl: wasm.Function = {
    wasm.Function("Std_readString", 0, false) { lh =>
      // We need to use the weird interface of javascript read string:
      // we pass the old memory boundary and get the new one.
      // In the end we have to return the old, where the fresh string lies.
      GetGlobal(memoryBoundary) <:> GetGlobal(memoryBoundary) <:> Call("js_readString0") <:>
      SetGlobal(memoryBoundary)
    }
  }

  val wasmFunctions = List(concatImpl, digitToStringImpl, readStringImpl)

}

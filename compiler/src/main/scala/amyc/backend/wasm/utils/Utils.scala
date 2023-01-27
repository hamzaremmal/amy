package amyc.backend.wasm.utils

import amyc.ast.Identifier
import amyc.backend.wasm.Function
import amyc.backend.wasm.instructions.*
import amyc.backend.wasm.types.*
import amyc.backend.wasm.instructions.Instructions.*
import amyc.backend.wasm.instructions.numeric.i32
import amyc.backend.wasm.instructions.variable.*
import amyc.core.Context
import amyc.core.Signatures.*
import amyc.reporter

// Utilities for CodeGen
object Utils {

  // The index of the global variable that represents the free memory boundary
  val memoryBoundary: Int = 0
  // # of global variables
  val globalsNo = 1

  // Max number of parameters supporter for wwasm
  val maxParamsInFun = 5

  // ==============================================================================================
  // ================================== WASM FUNCTION TYPES =======================================
  // ==============================================================================================

  lazy val defaultFunTypes : Context ?=> List[String] =
    s"(type ${mkFunTypeName(0)} (func (result i32)))"::
    s"(type ${mkFunTypeName(1)} (func (param i32) (result i32)))" ::
    s"(type ${mkFunTypeName(2)} (func (param i32 i32) (result i32)))" ::
    s"(type ${mkFunTypeName(3)} (func (param i32 i32 i32) (result i32)))" ::
    s"(type ${mkFunTypeName(4)} (func (param i32 i32 i32 i32) (result i32)))" ::
    s"(type ${mkFunTypeName(5)} (func (param i32 i32 i32 i32 i32) (result i32)))" ::
    Nil

  def mkFunTypeName(params: Int)(using Context) : String =
    if(params <= maxParamsInFun)
      s"$$fun_$params"
    else
      reporter.fatal(s"WASM don't define function types for $params parameters")

  // ==============================================================================================
  // ================================= DEFAULT IMPORTS ============================================
  // ==============================================================================================

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
      base <:> i32.const(4* (index + 1)) <:> i32.add
    }

  // Increment a local variable
  inline def incr(l: Int) =
    local.get(l) <:> i32.const(1) <:> i32.add <:> local.set(l)

  inline def withComment(inline comment : String)(inline code: Code) : Code =
    Comment(comment) <:> code

  // Creates a known string constant s in memory
  def mkString(s: String): Code = {
    val size = s.length
    val padding = 4 - size % 4

    val completeS = s + 0.toChar.toString * padding

    val setChars = for ((c, ind) <- completeS.zipWithIndex.toList) yield {
      global.get(memoryBoundary) <:> i32.const(ind) <:> i32.add <:>
        i32.const(c.toInt) <:> i32.store8
    }

    val setMemory =
      global.get(memoryBoundary) <:> global.get(memoryBoundary) <:> i32.const(size + padding) <:> i32.add <:>
        global.set(memoryBoundary)

    withComment(s"mkString: $s"){
      setChars <:> setMemory
    }
  }

  inline def mkBoolean(inline b : Boolean): Code =
    i32.const(if b then 1 else 0)

  inline def mkUnit : Code = i32.const(0)

  inline def mkBinOp(inline lhs : Code, inline rhs : Code)(op : Instruction) : Code =
    lhs <:> rhs <:> op

  inline def equ(inline lhs: Code, inline rhs: Code) : Code =
    mkBinOp(lhs, rhs)(i32.eq)

  inline def and(inline lhs: Code, inline rhs: Code) : Code =
    ift(lhs, rhs, mkBoolean(false))

  inline def or(lhs: Code, rhs: Code) : Code =
    ift(lhs, mkBoolean(true), rhs)

  inline def loadGlobal(inline idx: Int) : Code =
    global.get(idx) <:> i32.load

  inline def setGlobal(inline code: Code, inline idx: Int): Code =
    code <:> global.set(idx)

  inline def loadLocal(inline idx: Int) : Code =
    local.get(idx) <:> i32.load

  inline def setLocal(inline code: Code, inline idx: Int): Code =
    code <:> local.set(idx)

  inline def constructor(inline const: ConstrSig) : Code =
    i32.const(const.idx)

  inline def error(inline msg: Code) : Code =
    msg <:> call("Std_printString") <:> unreachable

  inline def ift(inline cond: Code, inline thenn: Code, elze: Code) =
    cond <:>
    `if`(None, Some(result(i32))) <:>
    thenn <:>
    `else`() <:>
    elze <:> end

  def resolveOrder(fn: List[Function], n: => Function)(using Context): List[Function] =
    def resolve(idx: Int) =
      fn.find(_.idx == idx).getOrElse(n)
    val size = fn.map(_.idx).max
    for i <- (0 to size).toList yield resolve(i)

}

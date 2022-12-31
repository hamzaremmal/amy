package amyc.backend.codegen

import amyc.ast.Identifier
import amyc.backend.wasm
import amyc.backend.wasm.Instructions.*
import amyc.core.Signatures.*
import amyc.core.Context
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
    Const(const.idx)

  inline def error(inline msg: Code) : Code =
    msg <:> Call("Std_printString") <:> Unreachable

  inline def ift(inline cond: Code, inline thenn: Code, elze: Code) =
    cond <:>
    If_i32 <:>
    thenn <:>
    Else <:>
    elze <:>
    End

}

package amyc.backend.wasm.utils

import amyc.backend.wasm.Function
import amyc.backend.wasm.Instructions.*
import amyc.backend.wasm.indices.{globalidx, localidx}
import amyc.backend.wasm.types.{local as l, *}
import amyc.core.{Context, Identifier}
import amyc.core.Symbols.*
import amyc.core.Signatures.*
import amyc.reporter

// Utilities for CodeGen

  inline def lh(using LocalsHandler): LocalsHandler = summon

  inline def mh(using ModuleHandler): ModuleHandler = summon

  // Max number of parameters supporter for wwasm
  val maxParamsInFun = 5

  // ==============================================================================================
  // ================================== WASM FUNCTION TYPES =======================================
  // ==============================================================================================

  lazy val defaultFunTypes : Context ?=> List[String] =
    s"(type $$${mkFunTypeName(0)} (func (result i32)))"::
    s"(type $$${mkFunTypeName(1)} (func (param i32) (result i32)))" ::
    s"(type $$${mkFunTypeName(2)} (func (param i32 i32) (result i32)))" ::
    s"(type $$${mkFunTypeName(3)} (func (param i32 i32 i32) (result i32)))" ::
    s"(type $$${mkFunTypeName(4)} (func (param i32 i32 i32 i32) (result i32)))" ::
    s"(type $$${mkFunTypeName(5)} (func (param i32 i32 i32 i32 i32) (result i32)))" ::
    Nil

  def mkFunTypeName(params: Int)(using Context) : String =
    if(params <= maxParamsInFun)
      s"fun_$params"
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
    "Std_readString",
    "String_concat"
  )

  /** Utilities */
  // A globally unique name for definitions
  def fullName(owner: Symbol, df: Symbol): String = owner.name + "_" + df.name

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
    base <:>
    i32.const(4* (index + 1)) <:>
    i32.add

  // Increment a local variable
  inline def incr(l: localidx) =
    local.get(l) <:> i32.const(1) <:> i32.add <:> local.set(l)

  inline def withComment(inline comment : String)(inline code: Code) : Code =
    Comment(comment) <:> code

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

  inline def setGlobal(inline code: Code, inline idx: globalidx): Code =
    code <:> global.set(idx)

  inline def loadLocal(inline idx: localidx) : Code =
    local.get(idx) <:> i32.load

  inline def setLocal(inline code: Code, inline idx: localidx): Code =
    code <:> local.set(idx)

  def constructor(const: ConstructorSymbol)(using ModuleHandler) : Code =
    i32.const(mh.constructor(const))

  inline def error(inline msg: Code) : Code =
    msg <:> call("Std_printString") <:> unreachable

  inline def ift(inline cond: Code, inline thenn: Code, elze: Code) =
    cond <:>
    `if`(None, Some(result(i32))) <:>
    thenn <:>
    `else`() <:>
    elze <:> end
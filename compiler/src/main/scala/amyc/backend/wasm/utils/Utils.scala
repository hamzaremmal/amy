package amyc.backend.wasm.utils

import amyc.backend.wasm.Function
import amyc.backend.wasm.Instructions.*
import amyc.backend.wasm.builtin.amy.Boolean.mkBoolean
import amyc.backend.wasm.Indices.{globalidx, localidx}
import amyc.backend.wasm.Types.{local as l, *}
import amyc.core.{Context, Identifier}
import amyc.core.Symbols.*
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
  inline def incr(inline l: localidx) =
    local.get(l) <:> i32.const(1) <:> i32.add <:> local.set(l)

  inline def withComment(inline comment : String)(inline code: Code) : Code =
    Comment(comment) <:> code

  inline def and(inline lhs: Code, inline rhs: Code) : Code =
    ift(lhs, rhs, mkBoolean(false))

  inline def or(lhs: Code, rhs: Code) : Code =
    ift(lhs, mkBoolean(true), rhs)

  inline def error(inline msg: Code) : Code =
    msg <:> call("Std_printString") <:> unreachable

  inline def ift(inline cond: Code, inline thenn: Code, inline elze: Code): Code  =
    cond <:>
    `if`(None, Some(result(i32))) <:>
    thenn <:>
    `else`() <:>
    elze <:> end
package amyc.backend.wasm.builtin

import amyc.*
import amyc.ast.SymbolicTreeModule.StringLiteral
import amyc.backend.wasm.*
import amyc.backend.wasm.WASMCodeGenerator.cgExpr
import amyc.backend.wasm.builtin.amy.*
import Instructions.*
import amyc.backend.wasm.utils.*
import amyc.core.{Context, Identifier}

object BuiltIn :

  // We don't generate code for these functions in CodeGen (they are hard-coded here or in js wrapper)
  val builtInFunctions: Set[String] = Set(
    "Std_printInt",
    "Std_printString",
    "Std_digitToString",
    "Std_readInt",
    "Std_readString",
    "String_concat",
    "String_length",
    "unnamed_+",
    "unnamed_-",
    "unnamed_*",
    "unnamed_/",
    "unnamed_%",
    "unnamed_<",
    "unnamed_<=",
    //"unnamed_&&",
    //"unnamed_||",
    "unnamed_==",
    "unnamed_++"
  )

  lazy val wasmFunctions: (Context, ModuleHandler) ?=> List[Function] =
    String.length     ::
    String.concat     ::
    Std.digitToString ::
    Std.readString    ::
    unnamed.+         ::
    unnamed.-         ::
    unnamed.*         ::
    unnamed./         ::
    unnamed.%         ::
    unnamed.<         ::
    unnamed.<=        ::
    //unnamed.&&        ::
    //unnamed.||        ::
    unnamed.==        ::
    unnamed.++        ::
    Nil

  // The default imports we will pass to a wasm Module
  val defaultImports: List[String] = List(
    "\"system\" \"printInt\" (func $Std_printInt (param i32) (result i32))",
    "\"system\" \"printString\" (func $Std_printString (param i32) (result i32))",
    "\"system\" \"readString0\" (func $js_readString0 (param i32) (result i32))",
    "\"system\" \"readInt\" (func $Std_readInt (result i32))",
    "\"system\" \"mem\" (memory 100)"
  )
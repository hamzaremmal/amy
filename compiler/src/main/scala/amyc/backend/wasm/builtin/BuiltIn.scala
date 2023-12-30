package amyc
package backend
package wasm
package builtin

import amyc.*
import amyc.ast.SymbolicTreeModule.StringLiteral
import amyc.backend.wasm.*
import amyc.backend.wasm.Modules.*
import amyc.backend.wasm.gen.WASMCodeGenerator.cgExpr
import amyc.backend.wasm.builtin.amy.*
import Instructions.*
import amyc.backend.wasm.handlers.ModuleHandler
import amyc.backend.wasm.utils.*
import amyc.core.{Context, Identifier}

object BuiltIn :

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
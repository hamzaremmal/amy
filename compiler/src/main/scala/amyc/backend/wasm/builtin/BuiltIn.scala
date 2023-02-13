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

  lazy val wasmFunctions: (Context, ModuleHandler) ?=> List[Function] =
    String.length ::
    String.concat ::
    Std.digitToString ::
    Std.readString ::
    Nil
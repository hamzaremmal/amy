package amyc.backend.wasm.builtin

import amyc.*
import amyc.ast.Identifier
import amyc.ast.SymbolicTreeModule.{StringLiteral, StringType}
import amyc.backend.wasm.*
import amyc.backend.wasm.utils.Utils.*
import amyc.backend.wasm.WASMCodeGenerator.cgExpr
import amyc.backend.wasm.builtin.amy.Std
import amyc.backend.wasm.instructions.Instructions.*
import amyc.backend.wasm.instructions.numeric.i32
import amyc.backend.wasm.instructions.variable.*
import amyc.backend.wasm.utils.*
import amyc.core.Context
import amyc.core.Signatures.FunSig

object BuiltIn :

  lazy val wasmFunctions: Context ?=> List[Function] =
    unnamed.null_fn ::
    amy.String.concat ::
    Std.digitToString ::
    Std.readString ::
    Nil
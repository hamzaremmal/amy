package amyc.backend.wasm.builtin

import amyc.*
import amyc.ast.SymbolicTreeModule.StringLiteral
import amyc.backend.wasm.*
import amyc.backend.wasm.utils.Utils.*
import amyc.backend.wasm.WASMCodeGenerator.cgExpr
import amyc.backend.wasm.builtin.amy.{Std, unnamed}
import Instructions.*
import amyc.backend.wasm.utils.*
import amyc.core.{Context, Identifier}
import amyc.core.Signatures.FunSig

object BuiltIn :

  lazy val wasmFunctions: Context ?=> List[Function] =
    unnamed.null_fn ::
    amy.String.concat ::
    Std.digitToString ::
    Std.readString ::
    Nil
package amyc.backend.wasm.builtin.amy

import amyc.ast.SymbolicTreeModule.{ClassTypeTree, StringLiteral}
import amyc.backend.wasm.WASMCodeGenerator.cgExpr
import amyc.backend.wasm.builtin.BuiltInModule
import amyc.backend.wasm.utils.Utils.error
import amyc.core.Context
import amyc.core.StdDefinitions.*
import amyc.{ctx, symbols}

/**
  * Unnamed Amy module
  */
object unnamed extends BuiltInModule {

  override val owner = "unnamed"

  // Pointer to a null function
  lazy val null_fn: BuiltIn =
    builtInForSym("null") {
      error(cgExpr(StringLiteral("Null function")))
    }

}

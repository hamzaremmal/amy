package amyc.backend.wasm.builtin

import amyc.core.StdDefinitions.*
import amyc.backend.wasm.WASMCodeGenerator.cgExpr
import amyc.backend.wasm.utils.Utils.error
import amyc.{ctx, symbols}
import amyc.core.Context
import amyc.ast.SymbolicTreeModule.{ClassTypeTree, StringLiteral}

/**
  * Unnamed Amy module
  */
object unnamed extends BuiltInModule {

  override val owner = "<unnamed>"

  // Pointer to a null function
  lazy val null_fn: BuiltIn =
    builtInForSym("null") {
      error(cgExpr(StringLiteral("Null function")))
    }

}

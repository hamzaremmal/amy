package amyc.backend.wasm.builtin

import amyc.core.StdDefinitions.*
import amyc.backend.wasm.WASMCodeGenerator.cgExpr
import amyc.backend.wasm.utils.Utils.error
import amyc.{ctx, symbols}
import amyc.core.Context
import amyc.core.Symbols.*
import amyc.ast.SymbolicTreeModule.{ClassTypeTree, StringLiteral}

/**
  * Unnamed Amy module
  */
object unnamed extends BuiltInModule {

  override lazy val owner : Context ?=> Symbol = stdDef.UnnamedModule

  // Pointer to a null function
  lazy val null_fn: BuiltIn =
    builtInForSymbol("null") {
      error(cgExpr(StringLiteral("Null function")))
    }

}

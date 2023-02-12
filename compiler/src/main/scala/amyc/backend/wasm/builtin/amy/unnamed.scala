package amyc.backend.wasm.builtin.amy

import amyc.ast.SymbolicTreeModule.{ClassTypeTree, StringLiteral}
import amyc.backend.wasm.utils.ModuleHandler
import amyc.backend.wasm.WASMCodeGenerator.cgExpr
import amyc.backend.wasm.builtin.BuiltInModule
import amyc.backend.wasm.utils.error
import amyc.core.Context
import amyc.core.Symbols.*
import amyc.ast.SymbolicTreeModule.{ClassTypeTree, StringLiteral}
import amyc.core.StdDefinitions.*
import amyc.{ctx, symbols}

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

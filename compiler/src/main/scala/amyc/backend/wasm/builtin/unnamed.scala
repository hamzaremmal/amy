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
  
  override lazy val onLoad: Context ?=> Unit =
    // TODO HR : This patch should be remove when introducing native functions and this method should
    // TODO HR : Should be registered as a native method instead of adding it here
    symbols.addModule(owner) // This garanties not to fail since String is considered as a Keyword
    symbols.addFunction(owner,
      "null",
      Nil,
      ClassTypeTree(stdDef.StringType)
    )

  // Pointer to a null function
  lazy val null_fn: BuiltIn =
    builtInForSym("null"){
      error(cgExpr(StringLiteral("Null function")))
    }

}

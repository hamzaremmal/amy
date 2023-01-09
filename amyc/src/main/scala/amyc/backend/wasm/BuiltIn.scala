package amyc.backend.wasm

import amyc.*
import amyc.core.Context
import amyc.ast.SymbolicTreeModule.StringType
import amyc.ast.SymbolicTreeModule.StringLiteral
import amyc.backend.wasm.Function
import amyc.backend.wasm.Instructions.*
import amyc.backend.wasm.utils.LocalsHandler
import amyc.backend.wasm.WASMCodeGenerator.cgExpr
import Utils.*
import amyc.ast.Identifier
import amyc.core.Signatures.FunSig

object BuiltIn {

  private type F = Context ?=> Function

  lazy val wasmFunctions: Context ?=> List[Function] =
    null_fn ::
    concatImpl ::
    digitToStringImpl ::
    readStringImpl ::
    Nil

  // ==============================================================================================
  // ================================ CREATE CODE FOR BUILTIN =====================================
  // ==============================================================================================

  def builtInForSym(owner: String, name: String)(code: LocalsHandler ?=> Code)(using Context) =
    val (id, sym) = symbols.getFunction(owner, name).getOrElse{
      reporter.fatal(s"BuiltIn function ${owner}_$name is not defined - symbol is missing")
    }
    Function(fullName(sym.owner, id), sym.argTypes.length, false, sym.idx){
      code
    }

  // ==============================================================================================
  // ======================================= BUILTINs =============================================
  // ==============================================================================================


  // Pointer to a null function
  lazy val null_fn : F =
    Function("null", 0, false, 0) {
      error(cgExpr(StringLiteral("Null function"))(using Map.empty))
    }

  // Built-in implementation of concatenation
  lazy val concatImpl: F =
  // Register function because it's missing
  // TODO HR : This patch should be remove when introducing native functions and this method should
  // TODO HR : Should be registered as a native method instead of adding it here
    symbols.addModule("String") // This garanties not to fail since String is considered as a Keyword
    symbols.addFunction("String", "concat", StringType :: StringType :: Nil, StringType)
    builtInForSym("String", "concat") {
      val ptrS = lh.getFreshLocal
      val ptrD = lh.getFreshLocal
      val label = getFreshLabel()

      def mkLoop: Code = {
        val label = getFreshLabel()
        Loop(label) <:>
          // Load current character
          GetLocal(ptrS) <:> Load8_u <:>
          // If != 0
          If_void <:>
          // Copy to destination
          GetLocal(ptrD) <:>
          GetLocal(ptrS) <:> Load8_u <:>
          Store8 <:>
          // Increment pointers
          incr(ptrD) <:> incr(ptrS) <:>
          // Jump to loop
          Br(label) <:>
          Else <:>
          End <:>
          End
      }

      // Instantiate ptrD to previous memory, ptrS to first string
      GetGlobal(memoryBoundary) <:>
        SetLocal(ptrD) <:>
        GetLocal(0) <:>
        SetLocal(ptrS) <:>
        // Copy first string
        mkLoop <:>
        // Set ptrS to second string
        GetLocal(1) <:>
        SetLocal(ptrS) <:>
        // Copy second string
        mkLoop <:>
        //
        // Pad with zeros until multiple of 4
        //
        Loop(label) <:>
        // Write 0
        GetLocal(ptrD) <:> Const(0) <:> Store8 <:>
        // Check if multiple of 4
        GetLocal(ptrD) <:> Const(4) <:> Rem <:>
        // If not
        If_void <:>
        // Increment pointer and go back
        incr(ptrD) <:>
        Br(label) <:>
        Else <:>
        End <:>
        End <:>
        // Put string pointer to stack, set new memory boundary and return
        GetGlobal(memoryBoundary) <:> GetLocal(ptrD) <:> Const(1) <:> Add <:> SetGlobal(memoryBoundary)
    }

  lazy val digitToStringImpl: F =
    builtInForSym("Std", "digitToString") {
      // We know we have to create a string of total size 4 (digit code + padding), so we do it all together
      // We do not need to shift the digit due to little endian structure!
      GetGlobal(memoryBoundary) <:> GetLocal(0) <:> Const('0'.toInt) <:> Add <:> Store <:>
        // Load memory boundary to stack, then move it by 4
        GetGlobal(memoryBoundary) <:>
        GetGlobal(memoryBoundary) <:> Const(4) <:> Add <:> SetGlobal(memoryBoundary)
    }

  lazy val readStringImpl: F =
    builtInForSym("Std", "readString"){
      // We need to use the weird interface of javascript read string:
      // we pass the old memory boundary and get the new one.
      // In the end we have to return the old, where the fresh string lies.
      GetGlobal(memoryBoundary) <:>
      GetGlobal(memoryBoundary) <:>
      Call("js_readString0") <:>
      SetGlobal(memoryBoundary)
    }

}
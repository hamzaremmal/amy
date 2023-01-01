package amyc.backend.wasm

import amyc.backend.wasm.Instructions.Code
import amyc.backend.wasm.utils.LocalsHandler
import amyc.core.Context

// A WebAssembly module
case class Module(name: String, imports: List[String], globals: Int, functions: List[Function])

// If isMain = false, represents a function which returns an i32 and will not be exported to js
// If isMain = true , represents a function which does not return a value, and will be exported to js
case class Function private (name: String, args: Int, isMain: Boolean, locals: Int, code: Code, idx: Int)

object Function {
  
  def apply(name: String, args: Int, isMain: Boolean, idx: Int)(codeGen: LocalsHandler => Code): Function =
    val lh = new LocalsHandler(args)
    // Make code first, as it may increment the locals in lh
    val code = codeGen(lh)
    new Function(name, args, isMain, lh.locals, code, idx)
    
}
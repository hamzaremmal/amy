package amyc
package wasm
import Instructions.Code

// If isMain = false, represents a function which returns an i32 and will not be exported to js
// If isMain = true , represents a function which does not return a value, and will be exported to js
class Function private (val name: String, val args: Int, val isMain: Boolean, val locals: Int, val code: Code) {
  override def toString: String = ModulePrinter(this)
}

class LocalsHandler(args: Int) {
  private var locals_ = 0
  def getFreshLocal(): Int = {
    locals_ += 1
    args + locals_ - 1
  }
  private[wasm] def locals = locals_
}

object Function {
  def apply(name: String, args: Int, isMain: Boolean)(codeGen: LocalsHandler => Code) = {
    val lh = new LocalsHandler(args)
    // Make code first, as it may increment the locals in lh
    val code = codeGen(lh)
    new Function(name, args, isMain, lh.locals, code)
  }
}

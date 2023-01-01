package amyc.backend.wasm.utils

class LocalsHandler(args: Int) {
  private var locals_ = 0
  def getFreshLocal: Int = {
    locals_ += 1
    args + locals_ - 1
  }
  private[wasm] def locals = locals_
}

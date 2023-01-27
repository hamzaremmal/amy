package amyc.backend.wasm.instructions.variable

import amyc.backend.wasm.indices.globalidx
import amyc.backend.wasm.instructions.Instructions.Instruction

object global {

  case class get(idx : globalidx) extends Instruction

  case class set(idx : globalidx) extends Instruction

}

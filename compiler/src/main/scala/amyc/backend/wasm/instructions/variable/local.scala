package amyc.backend.wasm.instructions.variable

import amyc.backend.wasm.indices.localidx
import amyc.backend.wasm.instructions.Instructions.Instruction

object local {

  case class get(idx : localidx) extends Instruction
  case class set(idx : localidx) extends Instruction
  case class tee(idx : localidx) extends Instruction

}

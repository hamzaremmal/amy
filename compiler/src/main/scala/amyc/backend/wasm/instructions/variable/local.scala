package amyc.backend.wasm.instructions.variable

import amyc.backend.wasm.instructions.Instructions.Instruction

object local {
  
  case class get(idx : Int) extends Instruction
  case class set(idx : Int) extends Instruction
  case class tee(idx : Int) extends Instruction

}

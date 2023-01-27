package amyc.backend.wasm.instructions.variable

import amyc.backend.wasm.instructions.Instructions.Instruction

object global {

  case class get(idx : Int) extends Instruction

  case class set(idx : Int) extends Instruction

}

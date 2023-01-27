package amyc.backend.wasm.instructions.numeric.integer

import amyc.backend.wasm.instructions.Instructions.Instruction

trait ibinop {

  val add   : Instruction
  val sub   : Instruction
  val mul   : Instruction
  val div_u : Instruction
  val div_s : Instruction
  val rem_u : Instruction
  val rem_s : Instruction
  val and   : Instruction
  val or    : Instruction
  val xor   : Instruction
  val shl   : Instruction
  val shr_u : Instruction
  val shr_s : Instruction
  val rotl  : Instruction
  val rotr  : Instruction

}

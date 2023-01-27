package amyc.backend.wasm.instructions.integer

import amyc.backend.wasm.instructions.Instructions.Instruction

trait iunop {

  val clz    : Instruction
  val ctz    : Instruction
  val popcnt : Instruction

}

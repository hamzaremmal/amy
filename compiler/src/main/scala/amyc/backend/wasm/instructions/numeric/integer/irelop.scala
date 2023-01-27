package amyc.backend.wasm.instructions.numeric.integer

import amyc.backend.wasm.instructions.Instructions.Instruction

trait irelop :

  val eq   : Instruction
  val ne   : Instruction
  val lt_s : Instruction
  val lt_u : Instruction
  val gt_s : Instruction
  val gt_u : Instruction
  val le_s : Instruction
  val le_u : Instruction
  val ge_s : Instruction
  val ge_u : Instruction

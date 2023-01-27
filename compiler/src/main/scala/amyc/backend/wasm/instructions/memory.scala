package amyc.backend.wasm.instructions

import amyc.backend.wasm.indices.dataidx
import amyc.backend.wasm.instructions.Instructions.Instruction

object memory :
  case object size extends Instruction
  case object grow extends Instruction
  case object fill extends Instruction
  case object copy extends Instruction
  case class init(x: dataidx) extends Instruction

object data:
  case class drop(x: dataidx) extends Instruction

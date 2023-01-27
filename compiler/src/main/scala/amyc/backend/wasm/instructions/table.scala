package amyc.backend.wasm.instructions

import amyc.backend.wasm.indices.*
import amyc.backend.wasm.instructions.Instructions.Instruction

/**
  * https://webassembly.github.io/spec/core/text/instructions.html#table-instructions
  */
object table :
  case class get(idx : tableidx = 0) extends Instruction
  case class set(idx : tableidx = 0) extends Instruction
  case class size(idx : tableidx = 0) extends Instruction
  case class grow(idx : tableidx = 0) extends Instruction
  case class fill(idx : tableidx = 0) extends Instruction
  case class copy(x : tableidx = 0, y : tableidx = 0) extends Instruction
  case class init(x : tableidx = 0, y : elemidx) extends Instruction

object elem :
  case class drop(x: elemidx) extends Instruction

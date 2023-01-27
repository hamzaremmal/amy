package amyc.backend.wasm.instructions

import amyc.backend.wasm.indices.*
import amyc.backend.wasm.instructions.Instructions.Instruction

/**
  * https://webassembly.github.io/spec/core/text/instructions.html#control-instructions
  */

case object unreachable extends Instruction
case object nop extends Instruction
case class label(l: labelidx) extends Instruction
case class br_if(l: labelidx) extends Instruction
case class br_table(l: List[labelidx], ln: labelidx) extends Instruction
case object `return` extends Instruction
case class call(x: funcidx) extends Instruction

// TODO HR : Add second parameter here after defining typeuse
case class call_indirect(x: tableidx = 0) extends Instruction

// TODO HR : Should not be defined here
case object end extends Instruction



package amyc.backend.wasm.instructions

import amyc.backend.wasm.indices.*
import amyc.backend.wasm.instructions.Instructions.{Instruction, id}
import amyc.backend.wasm.types.{result, typeuse}

/**
  * https://webassembly.github.io/spec/core/text/instructions.html#control-instructions
  */

// ================================================================================================
// ================================ PLAIN INSTRUCTIONS ============================================
// ================================================================================================

case object unreachable extends Instruction
case object nop extends Instruction
case class br(l: labelidx) extends Instruction
case class br_if(l: labelidx) extends Instruction
case class br_table(l: List[labelidx], ln: labelidx) extends Instruction
case object `return` extends Instruction
case class call(x: funcidx) extends Instruction

case class call_indirect(tpe: typeuse, x: tableidx = 0) extends Instruction

case object end extends Instruction

// ================================================================================================
// =================================== BLOCK INSTRUCTIONS =========================================
// ================================================================================================

case class `if`(label: Option[id] = None, blocktype: Option[result] = None) extends Instruction
case class `else` (l: Option[id] = None) extends Instruction


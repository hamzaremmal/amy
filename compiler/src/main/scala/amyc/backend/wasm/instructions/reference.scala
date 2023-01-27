package amyc.backend.wasm.instructions

import amyc.backend.wasm.indices.funcidx
import amyc.backend.wasm.instructions.Instructions.Instruction

/**
  * https://webassembly.github.io/spec/core/text/instructions.html#reference-instructions
  */
object ref:

  // TODO HR : Fix parameter type
  case class `null`(t: Any) extends Instruction
  case object is_null extends Instruction
  case class func(idx: funcidx) extends Instruction

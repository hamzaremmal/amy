package amyc.backend.wasm.instructions

import amyc.backend.wasm.indices.{globalidx, localidx}
import amyc.backend.wasm.instructions.Instructions.Instruction

/**
  * https://webassembly.github.io/spec/core/text/instructions.html#variable-instructions
  */
object variable:

  object global:
    case class get(idx: globalidx) extends Instruction
    case class set(idx: globalidx) extends Instruction

  object local :

    case class get(idx: localidx) extends Instruction
    case class set(idx: localidx) extends Instruction
    case class tee(idx: localidx) extends Instruction

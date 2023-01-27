package amyc.backend.wasm.instructions

import amyc.backend.wasm.instructions.Instructions.Instruction

case object nop extends Instruction
case object unreachable extends Instruction
// Return
case object ret extends Instruction



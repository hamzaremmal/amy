package amyc.backend.wasm.types.Integer

import amyc.backend.wasm.Type
import amyc.backend.wasm.instructions.*
import amyc.backend.wasm.instructions.Instructions.Instruction
import amyc.backend.wasm.instructions.integer.*

trait WasmInt[V <: Int | Long] extends Type, const, iunop, ibinop, itestop, irelop :

  val extend8_s : Instruction
  val extend16_s : Instruction
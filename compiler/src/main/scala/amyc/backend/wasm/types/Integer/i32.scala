package amyc.backend.wasm.types.Integer

import amyc.backend.wasm.instructions.Instructions.Instruction
import amyc.backend.wasm.instructions.numeric.integer.ibinop

object i32 extends WasmInt[Int] {

  case class const(value : Int) extends Instruction

  case object add extends Instruction
  case object sub extends Instruction
  case object mul extends Instruction
  case object div_u extends Instruction
  case object div_s extends Instruction
  case object rem_u extends Instruction
  case object rem_s extends Instruction
  case object and extends Instruction
  case object or extends Instruction
  case object xor extends Instruction
  case object shl extends Instruction
  case object shr_u extends Instruction
  case object shr_s extends Instruction
  case object rotl extends Instruction
  case object rotr extends Instruction

  case object eq extends Instruction
  case object ge_s extends Instruction
  case object ge_u  extends Instruction
  case object gt_s extends Instruction
  case object gt_u extends Instruction
  case object le_s extends Instruction
  case object le_u extends Instruction
  case object lt_s extends Instruction
  case object lt_u extends Instruction
  case object ne extends Instruction

  case object eqz extends Instruction

  case object clz extends Instruction
  case object ctz extends Instruction
  case object popcnt extends Instruction

  case object extend8_s extends Instruction
  case object extend16_s extends Instruction

  case object wrap_i64 extends Instruction

  // TODO HR : Add below instructions
  /**
    * inn_trunc_fmm_sx
    * inn_trunc_sat_fmm_sx
    * inn_reinterpret_fnn
    */
}

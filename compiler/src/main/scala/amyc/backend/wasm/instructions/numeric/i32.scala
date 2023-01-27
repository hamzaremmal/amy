package amyc.backend.wasm.instructions.numeric

import amyc.backend.wasm.instructions.Instructions.Instruction

/**
  * For Numeric Instructions : https://webassembly.github.io/spec/core/text/instructions.html#numeric-instructions
  * For Memory Instructions : https://webassembly.github.io/spec/core/text/instructions.html#memory-instructions
  */
object i32 :

  // ==============================================================================================
  // =================================== NUMERIC INSTRUCTIONS =====================================
  // ==============================================================================================

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
  case object eqz extends Instruction
  case object ne extends Instruction
  case object ge_s extends Instruction
  case object ge_u  extends Instruction
  case object gt_s extends Instruction
  case object gt_u extends Instruction
  case object le_s extends Instruction
  case object le_u extends Instruction
  case object lt_s extends Instruction
  case object lt_u extends Instruction
  case object clz extends Instruction
  case object ctz extends Instruction
  case object popcnt extends Instruction
  case object extend8_s extends Instruction
  case object extend16_s extends Instruction
  case object wrap_i64 extends Instruction
  case object trunc_f32_s extends Instruction
  case object trunc_f32_u extends Instruction
  case object trunc_f64_s extends Instruction
  case object trunc_f64_u extends Instruction
  case object trunc_sat_f32_s extends Instruction
  case object trunc_sat_f32_u extends Instruction
  case object trunc_sat_f64_s extends Instruction
  case object trunc_sat_f64_u extends Instruction
  case object reinterpret_f32 extends Instruction

  // ==============================================================================================
  // =================================== MEMORY INSTRUCTIONS ======================================
  // ==============================================================================================

  case object load extends Instruction
  case object load8_s extends Instruction
  case object load8_u extends Instruction
  case object load16_s extends Instruction
  case object load16_u extends Instruction
  case object store extends Instruction
  case object store8 extends Instruction
  case object store16 extends Instruction
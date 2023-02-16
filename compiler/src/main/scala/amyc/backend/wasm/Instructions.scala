package amyc.backend.wasm

import amyc.backend.wasm.Indices.*
import amyc.backend.wasm.Values.*
import amyc.backend.wasm.Types.*

import scala.annotation.targetName

// A subset of instructions defined by the WASM standard
object Instructions :

  sealed abstract class Instruction:
    @targetName("concat")
    def <:>(i: Instruction): Code = (this, i) match
      case (Code(rhs), Code(lhs)) => Code(rhs ::: lhs)
      case (Code(rhs), _) => Code(rhs :+ i)
      case (_, Code(lhs)) => Code(this +: lhs)
      case _ => Code(this :: i :: Nil)

  // Represents a sequence of instructions
  case class Code(instructions: List[Instruction]) extends Instruction

  // Comment
  case class Comment(msg: String) extends Instruction

  // ==============================================================================================
  // =================== Useful implicit conversions to construct Code objects ====================
  // ==============================================================================================

  implicit def is2c(is: List[Instruction]): Code = is.foldRight(Code(Nil))(_ <:> _)
  implicit def cs2c(cs: List[Code]): Code = cs.foldRight(Code(Nil))(_ <:> _)
  implicit def i2c(i: Instruction): Code = i match
    case c:Code => c
    case _ => Code(List(i))

  // ==============================================================================================
  // ============================= ??? ============================================================
  // ==============================================================================================

  // Control instructions
  @deprecated
  case class Loop(label: String, tpe: Option[result] = None)  extends Instruction // A block of instructions with a label at the beginning
  @deprecated
  case class Block(label: String) extends Instruction // A block of instructions with a label at the end

  // ==============================================================================================
  //
  // ==============================================================================================

  /**
    * https://webassembly.github.io/spec/core/text/instructions.html#variable-instructions
    */
  object global:
    case class get(idx: globalidx) extends Instruction
    case class set(idx: globalidx) extends Instruction

  object local:

    case class get(idx: localidx) extends Instruction
    case class set(idx: localidx) extends Instruction
    case class tee(idx: localidx) extends Instruction

  /**
    * https://webassembly.github.io/spec/core/text/instructions.html#table-instructions
    */
  object table:
    case class get(idx: tableidx = 0) extends Instruction
    case class set(idx: tableidx = 0) extends Instruction
    case class size(idx: tableidx = 0) extends Instruction
    case class grow(idx: tableidx = 0) extends Instruction
    case class fill(idx: tableidx = 0) extends Instruction
    case class copy(x: tableidx = 0, y: tableidx = 0) extends Instruction
    case class init(x: tableidx = 0, y: elemidx) extends Instruction

  object elem:
    case class drop(x: elemidx) extends Instruction


  /**
    * https://webassembly.github.io/spec/core/text/instructions.html#reference-instructions
    */
  object ref:
    // TODO HR : Fix parameter type
    case class `null`(t: Any) extends Instruction
    case object is_null extends Instruction
    case class func(idx: funcidx) extends Instruction

  /**
    * https://webassembly.github.io/spec/core/text/instructions.html#parametric-instructions
    */

  case object drop extends Instruction

  // TODO HR : Add select instructions

  /**
    * https://webassembly.github.io/spec/core/text/instructions.html#memory-instructions
    */

  object memory:
    case object size extends Instruction
    case object grow extends Instruction
    case object fill extends Instruction
    case object copy extends Instruction
    case class init(x: dataidx) extends Instruction

  object data:
    case class drop(x: dataidx) extends Instruction

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
  case class `else`(l: Option[id] = None) extends Instruction

  /**
    * For Numeric Instructions : https://webassembly.github.io/spec/core/text/instructions.html#numeric-instructions
    * For Memory Instructions : https://webassembly.github.io/spec/core/text/instructions.html#memory-instructions
    */
  case object i32 extends numtype:
    // === NUMERIC INSTRUCTIONS ===
    case class const(value: Int) extends Instruction
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
    case object ge_u extends Instruction
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
    // === MEMORY INSTRUCTIONS ===
    case object load extends Instruction
    case object load8_s extends Instruction
    case object load8_u extends Instruction
    case object load16_s extends Instruction
    case object load16_u extends Instruction
    case object store extends Instruction
    case object store8 extends Instruction
    case object store16 extends Instruction

  case object i64 extends numtype:
    // TODO HR : Add missing instructions
    ???

  case object f32 extends numtype:
    // TODO HR : Add missing instructions
    ???

  case object f64 extends numtype:
    // TODO HR : Add missing instructions
    ???

  case object v128 extends vectype:
    // TODO HR : Add missing instructions
    ???

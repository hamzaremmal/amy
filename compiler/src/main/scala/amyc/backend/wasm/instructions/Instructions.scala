package amyc.backend.wasm.instructions

import scala.annotation.targetName
import scala.language.implicitConversions

// A subset of instructions defined by the WASM standard
object Instructions {
  abstract class Instruction:
    @targetName("concat")
    def <:>(i: Instruction): Code = (this, i) match
      case (Code(rhs), Code(lhs)) => Code(rhs ::: lhs)
      case (Code(rhs), _) => Code(rhs :+ i)
      case (_, Code(lhs)) => Code(this +: lhs)
      case _ => Code(this :: i :: Nil)

  // Represents a sequence of instructions
  case class Code(instructions: List[Instruction]) extends Instruction

  // Useful implicit conversions to construct Code objects
  implicit def i2c(i: Instruction): Code = i match
    case c:Code => c
    case _ => Code(List(i))

  implicit def is2c(is: List[Instruction]): Code = is.foldRight(Code(Nil))(_ <:> _)

  implicit def cs2c(cs: List[Code]): Code = cs.foldRight(Code(Nil))(_ <:> _)

  // id
  opaque type id = String
  // TODO HR : Should check for allowed characters (https://webassembly.github.io/spec/core/text/values.html#text-id)
  implicit def id(str: String) : id = s"$$$str"

  // ==============================================================================================
  // ============================= ??? ============================================================
  // ==============================================================================================

  // Control instructions
  case class Loop(label: String)  extends Instruction // A block of instructions with a label at the beginning
  case class Block(label: String) extends Instruction // A block of instructions with a label at the end

  case class CallIndirect(tpe: String) extends Instruction

    // Comment
  case class Comment(msg: String) extends Instruction

}
package amyc.backend.wasm.instructions

import scala.language.implicitConversions

// A subset of instructions defined by the WASM standard
object Instructions {
  abstract class Instruction

  // Represents a sequence of instructions
  case class Code(instructions: List[Instruction]) {
    def <:>(i: Instruction) = Code(instructions :+ i)

    def <:>(other: Code) = Code(this.instructions ++ other.instructions)
  }

  // Useful implicit conversions to construct Code objects
  implicit def i2c(i: Instruction): Code = Code(List(i))

  implicit def is2c(is: List[Instruction]): Code = Code(is)

  implicit def cs2c(cs: List[Code]): Code = Code(cs flatMap (_.instructions))

  // id
  opaque type id = String
  // TODO HR : Should check for allowed characters (https://webassembly.github.io/spec/core/text/values.html#text-id)
  def id(str: String) : id = s"$$${str}"

  // ==============================================================================================
  // ============================= ??? ============================================================
  // ==============================================================================================

  case object Drop extends Instruction // Drops the top value of the stack

  // Control instructions
  case object If_void extends Instruction // Marks the beginning of an if-block (with implicit 'then').
  case object If_i32  extends Instruction // Marks the beginning of an if-block (with implicit 'then'). Must leave an i32 on the stack
  case object Else    extends Instruction // Marks the end of the implicit 'then' of an if-block
  case object End     extends Instruction // Marks the end of an if-then-else or block
  case class Loop(label: String)  extends Instruction // A block of instructions with a label at the beginning
  case class Block(label: String) extends Instruction // A block of instructions with a label at the end
  case class Br(label: String)    extends Instruction // Jump to "label", which MUST be the label of an enclosing structure
  case class Call(name: String)   extends Instruction

  case class CallIndirect(tpe: String) extends Instruction

    // Comment
  case class Comment(msg: String) extends Instruction

}
package amyc.utils

import java.io.File

import silex._

object Position {
  /** Number of bits used to encode the line number */
  private final val LINE_BITS   = 20
  /** Number of bits used to encode the column number */
  private final val COLUMN_BITS = 31 - LINE_BITS // no negatives => 31
  /** Mask to decode the line number */
  private final val LINE_MASK   = (1 << LINE_BITS) - 1
  /** Mask to decode the column number */
  private final val COLUMN_MASK = (1 << COLUMN_BITS) - 1

  private def lineOf(pos: Int): Int = (pos >> COLUMN_BITS) & LINE_MASK
  private def columnOf(pos: Int): Int = pos & COLUMN_MASK

  def fromFile(f: File, i: Int) = {
    SourcePosition(f, lineOf(i), columnOf(i))
  }
}

abstract class Position {
  val file: File
  val line: Int
  val col: Int

  def isDefined: Boolean
  def withoutFile: String
}

case class SourcePosition(file: File, line: Int, col: Int) extends Position {
  override def toString: String = s"${file.getPath}:$line:$col"
  def withoutFile = s"$line:$col"
  val isDefined = true
}

case object NoPosition extends Position {
  val file = null
  val line = 0
  val col = 0

  override def toString: String = "?:?"
  def withoutFile = toString
  val isDefined = false
}

// A trait for entities which have a position in a file
trait Positioned {

  protected var pos_ : Position = NoPosition

  def hasPosition = pos_ != NoPosition

  def position = pos_

  def setPos(pos: Position): this.type = {
    pos_ = pos
    this
  }

  def setPos(other: Positioned): this.type = {
    setPos(other.position)
  }

}

case class SourcePositioner(file: File) extends Positioner[Char, SourcePosition] {
  override val start: SourcePosition = SourcePosition(file, 1, 1)

  override def increment(position: SourcePosition, character: Char): SourcePosition =
    if (character == '\n') {
      position.copy(line = position.line + 1, col = 1)
    }
    else {
      position.copy(col = position.col + 1)
    }
}


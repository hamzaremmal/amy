package amyc
package parsing

import amyc.utils.Positioned

import scala.util.matching.Regex

sealed trait Token extends Positioned with Product:
  override def toString: String =
    s"$productPrefix${productIterator.mkString("(", ",", ")")}(${position.withoutFile})"

object Tokens:
  final case class KeywordToken(value: String)
      extends Token // e.g. keyword "if"
  final case class IdentifierToken(name: String)
      extends Token // e.g. variable name "x"
  final case class IntLitToken(value: Int)
      extends Token // e.g. integer literal "123"
  final case class StringLitToken(value: String) extends Token
  final case class BoolLitToken(value: Boolean) extends Token
  final case class DelimiterToken(value: String)
      extends Token // .,:;(){}[]= and =>
  final case class OperatorToken(name: String) extends Token // e.g. "+"
  final case class CommentToken(text: String)
      extends Token // e.g. "// this is a comment"
  final case class SpaceToken() extends Token // e.g. "\n  "
  final case class ErrorToken(content: String) extends Token
  final case class EOFToken() extends Token // special token at the end of file

sealed abstract class TokenKind(representation: String):
  override def toString: String = representation

object TokenKinds {
  final case class KeywordKind(value: String) extends TokenKind(value)
  final case class IdentifierKind(value: String | Regex) extends TokenKind(value.toString):
    // TODO HR : Find a better way to generate hashcode
    override def hashCode: Int = 0

    override def equals(obj: Any): Boolean =
      (this, obj) match
        case (IdentifierKind(s1: String), IdentifierKind(s2: String)) => s1 == s2
        case (IdentifierKind(s: String), IdentifierKind(reg: Regex)) => reg matches s
        case (IdentifierKind(reg: Regex), IdentifierKind(s: String)) => reg matches s
        case _ => false

  case object LiteralKind extends TokenKind("<Literal>")
  final case class DelimiterKind(value: String) extends TokenKind(value)
  final case class OperatorKind(value: String) extends TokenKind(value)
  case object EOFKind extends TokenKind("<EOF>")
  case object NoKind extends TokenKind("<???>")
}

object TokenKind:
  import Tokens._
  import TokenKinds._

  def of(token: Token): TokenKind =
    token match
      case KeywordToken(value)    => KeywordKind(value)
      case IdentifierToken(value) => IdentifierKind(value)
      case BoolLitToken(_)        => LiteralKind
      case IntLitToken(_)         => LiteralKind
      case StringLitToken(_)      => LiteralKind
      case DelimiterToken(value)  => DelimiterKind(value)
      case OperatorToken(value)   => OperatorKind(value)
      case EOFToken()             => EOFKind
      case _                      => NoKind

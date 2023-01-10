package amyc
package parsing

import amyc.utils.Positioned

sealed trait Token extends Positioned with Product {
  override def toString = {
    productPrefix + productIterator.mkString("(", ",", ")") + "(" + position.withoutFile + ")"
  }
}

object Tokens {
  final case class KeywordToken(value: String) extends Token    // e.g. keyword "if"
  final case class IdentifierToken(name: String) extends Token  // e.g. variable name "x" 
  final case class PrimTypeToken(value: String) extends Token   // e.g. primitive type "Int"
  final case class IntLitToken(value: Int) extends Token        // e.g. integer literal "123"
  final case class StringLitToken(value: String) extends Token
  final case class BoolLitToken(value: Boolean) extends Token
  final case class DelimiterToken(value: String) extends Token  // .,:;(){}[]= and =>
  final case class OperatorToken(name: String) extends Token    // e.g. "+"
  final case class CommentToken(text: String) extends Token     // e.g. "// this is a comment"
  final case class SpaceToken() extends Token                   // e.g. "\n  "
  final case class ErrorToken(content: String) extends Token
  final case class EOFToken() extends Token                     // special token at the end of file
}

sealed abstract class TokenKind(representation: String) {
  override def toString: String = representation
}

object TokenKinds {
  final case class KeywordKind(value: String) extends TokenKind(value)
  case object IdentifierKind extends TokenKind("<Identifier>")
  case object PrimTypeKind extends TokenKind("<Primitive Type>")
  case object LiteralKind extends TokenKind("<Literal>")
  final case class DelimiterKind(value: String) extends TokenKind(value)
  final case class OperatorKind(value: String) extends TokenKind(value)
  case object EOFKind extends TokenKind("<EOF>")
  case object NoKind extends TokenKind("<???>")
}

object TokenKind {
  import Tokens._
  import TokenKinds._

  def of(token: Token): TokenKind = token match {
    case KeywordToken(value) => KeywordKind(value)
    case IdentifierToken(_) => IdentifierKind
    case PrimTypeToken(_) => PrimTypeKind
    case BoolLitToken(_) => LiteralKind
    case IntLitToken(_) => LiteralKind
    case StringLitToken(_) => LiteralKind
    case DelimiterToken(value) => DelimiterKind(value)
    case OperatorToken(value) => OperatorKind(value)
    case EOFToken() => EOFKind
    case _ => NoKind
  }
}
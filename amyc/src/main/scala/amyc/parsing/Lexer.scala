package amyc
package parsing

import amyc.core
import amyc.utils.*

import java.io.File
import silex.*
import amyc.utils.Position
import amyc.utils.Position.withPosition

// The lexer for Amy.
object Lexer extends Pipeline[List[File], Iterator[Token]] with Lexers {

  /** Tiny Silex reference:
    * ==============================
    * Silex's lexer essentially allows you to define a list of regular expressions
    * in their order of priority. To tokenize a given input stream of characters, each
    * individual regular expression is applied in turn. If a given expression matches, it
    * is used to produce a token of maximal length. Whenever a regular expression does not
    * match, the expression of next-highest priority is tried.
    * The result is a stream of tokens.
    *
    * Regular expressions `r` can be built using the following operators:
    *   - `word("abc")`  matches the sequence "abc" exactly
    *   - `r1 | r2`      matches either expression `r1` or expression `r2`
    *   - `r1 ~ r2`      matches `r1` followed by `r2`
    *   - `oneOf("xy")`  matches either "x" or "y"
    *     (i.e., it is a shorthand of `word` and `|` for single characters)
    *   - `elem(c)`      matches character `c`
    *   - `elem(f)`      matches any character for which the boolean predicate `f` holds
    *   - `opt(r)`       matches `r` or nothing at all
    *   - `many(r)`      matches any number of repetitions of `r` (including none at all)
    *   - `many1(r)`     matches any non-zero number of repetitions of `r`
    *
    * To define the token that should be output for a given expression, one can use
    * the `|>` combinator with an expression on the left-hand side and a function
    * producing the token on the right. The function is given the sequence of matched
    * characters and the source-position range as arguments.
    *
    * For instance,
    *
    * `elem(_.isDigit) ~ word("kg") |> {
    * (cs, range) => WeightLiteralToken(cs.mkString).setPos(range._1)) }`
    *
    * will match a single digit followed by the characters "kg" and turn them into a
    * "WeightLiteralToken" whose value will be the full string matched (e.g. "1kg").
    */

  // Type of characters consumed.
  type Character = Char

  // Type of positions.
  type Position = SourcePosition

  // Type of tokens produced.
  type Token = parsing.Token

  private type P = Producer
  private type L = Lexer

  import Tokens._

  lazy val keywords : P =
    word("abstract") | word("case") | word("class") |
    word("fn") | word("else") | word("extends") |
    word("if") | word("match") | word("object") |
    word("val") | word("error") | word("_") | word("end")
    word("lambda")
    |> { (cs, range) =>
      withPosition(range._1) {
        KeywordToken(cs.mkString)
      }
    }

  lazy val primTypeToken : P =
    word("Int") | word("Boolean") | word("String") | word("Unit")
    |> { (cs, range) =>
      withPosition(range._1) {
        PrimTypeToken(cs.mkString)
      }
    }

  // Boolean literals,
  lazy val boolLitToken : P =
    word("true") | word("false")
    |> { (cs, range) =>
      withPosition(range._1) {
        BoolLitToken(java.lang.Boolean.parseBoolean(cs.mkString))
      }
    }

  // Operators, !
  lazy val operators : P =
    oneOf("+-*/%!<") | word("&&") | word("||") | word("==") | word("++") | word("<=")
    |> { (cs, range) =>
      withPosition(range._1) {
        OperatorToken(cs.mkString)
      }
    }

  // Identifiers,
  lazy val identifiers : P =
    elem(_.isUnicodeIdentifierStart) ~ many(elem(_.isUnicodeIdentifierPart))
    |> { (cs, range) =>
      withPosition(range._1) {
        IdentifierToken(cs.mkString)
      }
    }

  // Integer literal
  lazy val intLitToken : P =
    many1(elem(_.isDigit))
      |> { (cs, range) =>
      withPosition(range._1) {
        val litteral = BigInt(cs.mkString)
        if litteral < Int.MaxValue then
          IntLitToken(litteral.toInt)
        else
          ErrorToken(cs.mkString)
      }
    }

  // String literal,
  lazy val stringLitToken : P =
    word("\"") ~ many(elem(x => !x.isControl && x != '"')) ~ word("\"")
      |> { (cs, range) =>
      withPosition(range._1) {
        val str = cs.mkString;
        StringLitToken(str.substring(1, str.length() - 1))
      }
    }

  lazy val delimiters : P =
    oneOf(";,{}():.=") | word("=>")
      |> { (cs, range) =>
      withPosition(range._1) {
        DelimiterToken(cs.mkString)
      }
    }

  // Whitespace,
  lazy val whitespace : P =
    elem(_.isWhitespace)
      |> { (_, range) =>
      withPosition(range._1) {
        SpaceToken()
      }
    }

  // Single line comment,
  lazy val singleLineComment : P =
    word("//") ~ many(elem(_ != '\n'))
    |> { (cs, range) =>
      withPosition(range._1) {
        CommentToken(cs.mkString(""))
      }
    }

  // Multiline comments,
  // NOTE: Amy does not support nested multi-line comments (e.g. `/* foo /* bar */ */`).
  //       Make sure that unclosed multi-line comments result in an ErrorToken.
  lazy val multiLineComment : P =
    word("/*") ~ many((many1(word("*")) ~ elem(x => x != '/' && x != '*')) | elem(_ != '*')) ~ many(word("*")) ~ word("*/")
      |> { (cs, range) =>
      withPosition(range._1) {
        var str = cs.mkString
        str = str.substring(2, str.length() - 2)
        CommentToken(str)
      }
    }

  lazy val multiLineCommentError : P =
    word("/*") ~ many((many1(word("*")) ~ elem(x => x != '/' && x != '*')) | elem(_ != '*'))
    |> { (cs, range) =>
      withPosition(range._1) {
        val str = cs.mkString
        ErrorToken(str)
      }
    }

  // ==============================================================================================
  // =========================================== LEXER ============================================
  // ==============================================================================================

  lazy val lexer : L = Lexer(
    keywords,
    primTypeToken,
    boolLitToken,
    operators,
    identifiers,
    intLitToken,
    stringLitToken,
    delimiters,
    whitespace,
    singleLineComment,
    multiLineComment,
    multiLineCommentError
  ) onError {
    // We also emit ErrorTokens for Silex-handled errors.
    (cs, range) =>
      withPosition(range._1){
        ErrorToken(cs.mkString)
      }
  } onEnd {
    // Once all the input has been consumed, we emit one EOFToken.
    withPosition(_){
      EOFToken()
    }
  }

  override val name = "Lexer"

  override def run(files: List[File])(using core.Context): Iterator[Token] = {
    var it = Seq[Token]().iterator

    for (file <- files) {
      val source = Source.fromFile(file.toString, SourcePositioner(file))
      it ++= lexer.spawn(source).filter {
          _ match
            case CommentToken(_) => false
            case SpaceToken() => false
            case token@ErrorToken(error) =>
              reporter.error(s"Unknown token : $error ", token.position)
              false
            case _ => true
      }
    }
    it
  }
}

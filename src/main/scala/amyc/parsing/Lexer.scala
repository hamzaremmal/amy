package amyc
package parsing

import amyc.core
import amyc.utils.*

import java.io.File
import silex.*
import amyc.utils.Position

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

  import Tokens._

  val lexer = Lexer(
    // Keywords,
    word("abstract") | word("case") | word("class") |
      word("fn") | word("else") | word("extends") |
      word("if") | word("match") | word("object") |
      word("val") | word("error") | word("_") | word("end")
      |> { (cs, range) => KeywordToken(cs.mkString).setPos(range._1) },

    // Primitive type names,
      // HR : DONE
      word("Int") | word("Boolean") | word("String") | word("Unit")
      |> {(cs, range) => PrimTypeToken(cs.mkString).setPos(range._1)},
        

    // Boolean literals,
      // HR : DONE
      word("true") | word("false")
      |> {(cs, range) => BoolLitToken(java.lang.Boolean.parseBoolean(cs.mkString)).setPos(range._1)},
         

    // Operators,
      // HR : DONE
      // HR : ; + - * / % < <= && || == ++ !
    oneOf("+-*/%!<") | word("&&") | word("||") | word("==") | word("++") | word("<=")
    |> {(cs, range) => OperatorToken(cs.mkString).setPos(range._1)},
        
    // Identifiers,
    // HR : DONE
    elem(_.isUnicodeIdentifierStart) ~ many(elem(_.isUnicodeIdentifierPart))
    |> {(cs, range) => IdentifierToken(cs.mkString).setPos(range._1)},
        
    // Integer literal,
    // NOTE: Make sure to handle invalid (e.g. overflowing) integer values safely by
    //       emitting an ErrorToken instead.
    // HR : DONE
    many1(elem(_.isDigit))
    |> {(cs, range) => 
      val litteral = BigInt(cs.mkString)
      if litteral < Int.MaxValue then
        IntLitToken(litteral.toInt).setPos(range._1)
      else
        ErrorToken(cs.mkString).setPos(range._1)
      },
        
    // String literal,
        // HR : DONE
      word("\"") ~ many(elem(x => !x.isControl && x != '"')) ~ word("\"")
      |> {(cs, range) =>
        val str =  cs.mkString;
        StringLitToken(str.substring(1, str.length() - 1)).setPos(range._1)},
        
    // Delimiters,
    // HR : { } ( ) , : . = =>
    // HR : DONE
      oneOf(";,{}():.=") | word("=>")
      |> {(cs, range) => DelimiterToken(cs.mkString).setPos(range._1)},
    

    // Whitespace,
        // HR : DONE
    elem(_.isWhitespace)
    |> {(_, range) => SpaceToken().setPos(range._1)},
    
    // Single line comment,
    word("//") ~ many(elem(_ != '\n'))
      |> { cs => CommentToken(cs.mkString("")) },

    // Multiline comments,
    // NOTE: Amy does not support nested multi-line comments (e.g. `/* foo /* bar */ */`).
    //       Make sure that unclosed multi-line comments result in an ErrorToken.

    word("/*") ~ many((many1(word("*")) ~ elem(x => x != '/' && x != '*')) | elem(_ != '*')) ~ many(word("*")) ~ word("*/")
    |> {(cs, range) => 
      var str = cs.mkString
      str = str.substring(2, str.length() - 2)
      CommentToken(str).setPos(range._1)
      },

    word("/*") ~ many((many1(word("*")) ~ elem(x => x != '/' && x != '*')) | elem(_ != '*'))
    |> {
      (cs, range) =>
        val str = cs.mkString
        ErrorToken(str).setPos(range._1)
    }

    ) onError {
    // We also emit ErrorTokens for Silex-handled errors.
    (cs, range) => ErrorToken(cs.mkString).setPos(range._1)
  } onEnd {
    // Once all the input has been consumed, we emit one EOFToken.
    pos => EOFToken().setPos(pos)
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
            case _ => true}.map {
        case token@ErrorToken(error) => ctx.reporter.fatal("Unknown token at " + token.position + ": " + error)
        case token => token
      }
    }
    it
  }
}

/** Extracts all tokens from input and displays them */
object DisplayTokens extends Pipeline[Iterator[Token], Unit] {

  override val name = "DisplayTokens"

  override def run(tokens: Iterator[Token])(using core.Context): Unit = {
    tokens.foreach(println(_))
  }
}

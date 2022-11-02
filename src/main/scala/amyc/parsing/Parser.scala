package amyc
package parsing

import scala.language.implicitConversions
import amyc.ast.NominalTreeModule.*
import amyc.utils.*
import Tokens.*
import TokenKinds.*
import amyc.parsing.Parser.literal
import scallion.{~, *}

// The parser for Amy
object Parser extends Pipeline[Iterator[Token], Program]
                 with Parsers {

  type Token = amyc.parsing.Token
  type Kind = amyc.parsing.TokenKind

  import Implicits._

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  // ==============================================================================================
  // ================================== HELPER FUNCTIONS ==========================================
  // ==============================================================================================

  def inBrace[A](syntax : => Syntax[A]) : Syntax[A] =
    delimiter("{").skip ~ syntax ~ delimiter("}").skip

  def inParenthesis[A](syntax : => Syntax[A]) : Syntax[A] =
    delimiter("(").skip ~ syntax ~ delimiter(")").skip

  def qualifiedName : Syntax[(Option[String], String)] =
    identifier ~ opt(delimiter(".").skip ~ identifier) map {
      case id ~ Some(id2) => (Some(id), id2)
      case id ~ None => (None, id)
    }

  def op(string: String): Syntax[Token] = elem(OperatorKind(string))
  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

  // ==============================================================================================
  // ========================================== EOF ===============================================
  // ==============================================================================================

  lazy val eof: Syntax[Token] = elem(EOFKind)

  // ==============================================================================================
  // ================================== TOP LEVEL - PROGRAM =======================================
  // ==============================================================================================

  // An entire program (the starting rule for any Amy file).
  lazy val program: Syntax[Program] =
    many1(many1(module) ~<~ eof) map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  // ==============================================================================================
  // ====================================== AMY MODULE ============================================
  // ==============================================================================================

  // A module (i.e., a collection of definitions and an initializer expression)
  lazy val module: Syntax[ModuleDef] =
    (kw("object") ~ identifier ~
      many(definition) ~
      opt(expr) ~
      kw("end") ~ identifier) map {
    case obj ~ id ~ defs ~ body ~ _ ~ id1 =>
      if id == id1 then
        ModuleDef(id, defs.toList, body).setPos(obj)
      else 
        throw AmycFatalError("Begin and end module names do not match: " + id + " and " + id1)
  }

  // ==============================================================================================
  // ==================================== DEFINITIONS =============================================
  // ==============================================================================================

  // TODO HR : this is correct
  lazy val definition: Syntax[ClassOrFunDef] =
    funDef.up[ClassOrFunDef] | abstractClassDef.up[ClassOrFunDef] | caseClassDef.up[ClassOrFunDef]

  // TODO HR : This is correct
  lazy val abstractClassDef: Syntax[AbstractClassDef] =
    (kw("abstract") ~ kw("class") ~ identifier) map {
      case _ ~ _ ~ className => AbstractClassDef(className)
    }

  // TODO HR : this is correct
  lazy val caseClassDef: Syntax[CaseClassDef] =
    (kw("case") ~ kw("class") ~ identifier ~ inParenthesis(parameters) ~ kw("extends") ~ identifier) map {
      case _ ~ _ ~ className ~ params ~ _ ~ superClassName =>
        CaseClassDef(className, params.map(_.tt), superClassName)
    }

  // TODO HR : This is correct
  lazy val funDef: Syntax[FunDef] =
    (kw("fn") ~ identifier ~ inParenthesis(parameters) ~ delimiter(":") ~ typeTree ~ delimiter("=") ~ inBrace(expr)) map {
      case _ ~ funcName ~ params ~ _ ~ tpe ~ _ ~ expr =>
        FunDef(funcName, params, tpe, expr)
    }

  // ==============================================================================================
  // ===================================== IDENTIFIERS ============================================
  // ==============================================================================================

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }

  // ==============================================================================================
  // ===================================== PARAMETERS =============================================
  // ==============================================================================================
    
  // A list of parameter definitions.
  lazy val parameters: Syntax[List[ParamDef]] =
    repsep(parameterDef, delimiter(",")).map(_.toList)

  // A parameter definition, i.e., an identifier along with the expected type.
  lazy val parameterDef: Syntax[ParamDef] =
    (identifier ~ delimiter(":") ~ typeTree) map {
      case name ~ _ ~ tpe => ParamDef(name, tpe)
    }

  lazy val args : Syntax[Seq[Expr]] =
    repsep(expr, delimiter(","))

  // ==============================================================================================
  // ======================================== TYPES ===============================================
  // ==============================================================================================
   
  // A type expression.
  lazy val typeTree: Syntax[TypeTree] = primitiveType | identifierType

  // A built-in type (such as `Int`).
  val primitiveType: Syntax[TypeTree] = (accept(PrimTypeKind) {
    case tk@PrimTypeToken(name) => TypeTree(name match {
      case "Unit" => UnitType
      case "Boolean" => BooleanType
      case "Int" => IntType
      case "String" => StringType
      case _ => throw new java.lang.Error("Unexpected primitive type name: " + name)
    }).setPos(tk)
  } ~ opt("(" ~ literal ~ ")")).map { 
    case (prim@TypeTree(IntType)) ~ Some(_ ~ IntLiteral(32) ~ _) => prim
    case TypeTree(IntType) ~ Some(_ ~ IntLiteral(width) ~ _) => 
      throw AmycFatalError("Int type can only be used with a width of 32 bits, found : " + width)
    case TypeTree(IntType) ~ Some(_ ~ lit ~ _) =>
      throw AmycFatalError("Int type should have an integer width (only 32 bits is supported)")
    case TypeTree(IntType) ~ None => 
      throw AmycFatalError("Int type should have a specific width (only 32 bits is supported)")
    case prim ~ Some(_) => 
      throw AmycFatalError("Only Int type can have a specific width")
    case prim ~ None => prim
  }

  // A user-defined type (such as `List`).
  lazy val identifierType: Syntax[TypeTree] =
    qualifiedName map {
      case (Some(id), id2) => TypeTree(ClassType(QualifiedName(Some(id), id2)))
      case (None, id) => TypeTree(ClassType(QualifiedName(None, id)))
    }

  // ==============================================================================================
  // ====================================== OPERATORS =============================================
  // ==============================================================================================

  lazy val plus     : Syntax[String] = op("+") map (_ => "+")
  lazy val minus    : Syntax[String] = op("-") map (_ => "-")
  lazy val times    : Syntax[String] = op("*") map (_ => "*")
  lazy val div      : Syntax[String] = op("/") map (_ => "/")
  lazy val not      : Syntax[String] = op("!") map (_ => "!")
  lazy val mod      : Syntax[String] = op("%") map {_ => "%"}
  lazy val lessThan : Syntax[String] = op("<") map {_ => "<"}
  lazy val lessEq   : Syntax[String] = op("<=") map {_ => "<="}
  lazy val and      : Syntax[String] = op("&&") map {_ => "&&"}
  lazy val or       : Syntax[String] = op("||") map {_ => "||"}
  lazy val equals   : Syntax[String] = op("==") map {_ => "=="}
  lazy val concat   : Syntax[String] = op("++") map {_ => "++"}

  // ==============================================================================================
  // =================================== PATTERN MATCHING =========================================
  // ==============================================================================================

  lazy val matchCase : Syntax[MatchCase] =
    (kw("case") ~ pattern ~ delimiter("=>") ~ expr) map {
      case _ ~ pattern ~ _ ~ expr => MatchCase(pattern, expr)
    }

  // ==============================================================================================
  // ======================================== PATTERNS ============================================
  // ==============================================================================================

  lazy val patterns : Syntax[Seq[Pattern]] = recursive {
    repsep(pattern, delimiter(","))
  }
    
  // A pattern as part of a mach case.
  lazy val pattern: Syntax[Pattern] =
    idOrCaseClassPattern | literalPattern.up[Pattern] | wildPattern.up[Pattern]
    
  lazy val literalPattern: Syntax[Pattern] =
    (literal map (LiteralPattern(_))).up[Pattern] | unitPattern

  lazy val unitPattern : Syntax[Pattern] =
    delimiter("(") ~ delimiter(")") map {_ => LiteralPattern(new UnitLiteral)}

  lazy val wildPattern: Syntax[WildcardPattern] =
    kw("_") map (_ => WildcardPattern())

  lazy val idOrCaseClassPattern : Syntax[Pattern] =
    (identifier ~ opt(delimiter(".") ~ identifier) ~ opt(inParenthesis(patterns))) map {
      case id ~ Some(_ ~ id2) ~ Some(patterns) => CaseClassPattern(QualifiedName(Some(id), id2), patterns.toList)
      case id ~ None ~ Some(patterns)  => CaseClassPattern(QualifiedName(None, id), patterns.toList)
      // Only an identifier
      case id ~ None ~ None => IdPattern(id)
      case _ => ??? // TODO HR : throw an exception
    }

  // ============================================================================================
  // ====================================== EXPRESSIONS =========================================
  // ============================================================================================

  // Entry-point to an expression
  lazy val expr: Syntax[Expr] = recursive {
    sequenceExpression | valDefinitionExpression
  }

  // ------------------------- First level expressions ----------------------------

  // Val definitions
  lazy val valDefinitionExpression: Syntax[Expr] =
    (kw("val") ~ parameterDef ~ delimiter("=") ~ simpleExpression ~ delimiter(";") ~ expr) map {
      case _ ~ param ~ _ ~ assign ~ _ ~ seq => Let(param, assign, seq)
    }

  // Sequence definition
  lazy val sequenceExpression : Syntax[Expr] =
    (simpleExpression ~ opt(delimiter(";") ~ expr)) map {
      case exp1 ~ None => exp1
      case exp1 ~ Some(_ ~ exp2) => Sequence(exp1, exp2)
    }

  // ----------------------- Second level expressions ------------------------------

  lazy val ifExpression : Syntax[Expr] =
    (kw("if") ~ inParenthesis(expr) ~ inBrace(expr) ~ kw("else") ~ inBrace(expr)) map {
      case _ ~ cond ~ trueBranch ~ _ ~ falseBranch =>
        Ite(cond, trueBranch, falseBranch)
    }

  lazy val simpleExpression : Syntax[Expr] = recursive {
    (ifOrBinary ~ many(kw("match").skip ~ inBrace(many1(matchCase)))) map {
      case expr ~ Seq() => expr
      case scrut ~ s => s.foldLeft(scrut)((a, b) => Match(a, b.toList))
    }
  }

  lazy val ifOrBinary =
    ifExpression | binaryExpression

  lazy val binaryExpression : Syntax[Expr] =
    operators(termExpression)(
      // Defines the different operators, by decreasing priority.
      times | div | mod is LeftAssociative,
      plus | minus | concat is LeftAssociative,
      lessThan is LeftAssociative,
      lessEq is LeftAssociative,
      equals is LeftAssociative,
      and is LeftAssociative,
      or is LeftAssociative) {
        case (lhs, "+", rhs) => Plus(lhs, rhs)
        case (lhs, "-", rhs) => Minus(lhs, rhs)
        case (lhs, "*", rhs) => Times(lhs, rhs)
        case (lhs, "/", rhs) => Div(lhs, rhs)
        case (lhs, "%", rhs) => Mod(lhs, rhs)
        case (lhs, "<", rhs) => LessThan(lhs, rhs)
        case (lhs, "<=", rhs) => LessEquals(lhs, rhs)
        case (lhs, "&&", rhs) => And(lhs, rhs)
        case (lhs, "||", rhs) => Or(lhs, rhs)
        case (lhs, "==", rhs) => Equals(lhs, rhs)
        case (lhs, "++", rhs) => Concat(lhs, rhs)
        case _ => ??? // TODO Add message error for unknown operator
      }

  // ------------------------- Third Level expressions ------------------------------

  lazy val termExpression : Syntax[Expr] = recursive {
    (opt(minus | not) ~ factor) map {
      case Some("-") ~ expr => Neg(expr)
      case Some("!") ~ expr => Not(expr)
      case Some(_) ~ _ => ??? // Add error here
      case None ~ expr => expr
    }
  }

  // --------------------- Fourth Level expressions --------------------------

  lazy val factor : Syntax[Expr] =
    errorExpression | literal.up[Expr] | variableOrCall | valExpressionOrUnit

  lazy val errorExpression : Syntax[Expr] =
    (kw("error") ~ termExpression) map {
      case _ ~ expr => Error(expr)
    }

  lazy val variableOrCall: Syntax[Expr] =
    (identifier ~ opt(delimiter(".") ~ identifier) ~ opt(inParenthesis(args))) map {
      case id ~ Some(_ ~ id2) ~ Some(args) => Call(QualifiedName(Some(id), id2), args.toList)
      case id ~ None ~ Some(args) => Call(QualifiedName(None, id), args.toList)
      case id ~ Some(_ ~ id2) ~ None => throw AmycFatalError("Int type can only be used with a width of 32 bits, found : ") // TODO HR : Change message
      case id2 ~ None ~ None => Variable(id2)
    }

  lazy val valExpressionOrUnit : Syntax[Expr] =
    inParenthesis(opt(expr)) map {
      case Some(expr) => expr
      case None => new UnitLiteral
    }

  // ==============================================================================================
  // ==================================== LITTERALS ===============================================
  // ==============================================================================================

  // A literal expression.
  lazy val literal: Syntax[Literal[_]] =
    accept(LiteralKind) {
      case BoolLitToken(value) => BooleanLiteral(value)
      case IntLitToken(value) => IntLiteral(value)
      case StringLitToken(value) => StringLiteral(value)
    }.up[Literal[_]]

  // ==============================================================================================

  // Ensures the grammar is in LL(1)
  lazy val checkLL1: Boolean = {
    if (program.isLL1) {
      true
    } else {
      // Set `showTrails` to true to make Scallion generate some counterexamples for you.
      // Depending on your grammar, this may be very slow.
      val showTrails = true
      debug(program, showTrails)
      false
    }
  }

  override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    if (!checkLL1) {
      ctx.reporter.fatal("Program grammar is not LL1!")
    }

    val parser = Parser(program)

    parser(tokens) match {
      case Parsed(result, rest) => result
      case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
      case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
    }
  }
}
package amyc
package parsing

import scala.language.implicitConversions
import amyc.ast.NominalTreeModule.*
import amyc.utils.*
import Tokens.*
import TokenKinds.*
import amyc.ast.NominalTreeModule
import amyc.{core, parsing}
import amyc.parsing.Parser.literal
import scallion.{~, *}

import scala.annotation.targetName

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
    "{" ~>~ syntax ~<~ "}"

  def inParenthesis[A](syntax : => Syntax[A]) : Syntax[A] =
    "(" ~>~ syntax ~<~ ")"

  def op(string: String): Syntax[Token] = elem(OperatorKind(string))
  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

  // ==============================================================================================
  // ========================================== EOF ===============================================
  // ==============================================================================================

  lazy val eof: Syntax[Token] = elem(EOFKind)

  // ==============================================================================================
  // ====================================== KEYWORDS ==============================================
  // ==============================================================================================

  // To distinguish keywords parser and other parsers, we will define them using `...`
  lazy val `class`    : Syntax[Token] = kw("class")
  lazy val `abstract` : Syntax[Token] = kw("abstract")
  lazy val `case`     : Syntax[Token] = kw("case")
  lazy val `extends`  : Syntax[Token] = kw("extends")
  lazy val `object`   : Syntax[Token] = kw("object")
  lazy val `fn`       : Syntax[Token] = kw("fn")
  lazy val `end`      : Syntax[Token] = kw("end")
  lazy val `kw_`      : Syntax[Token] = kw("_")
  lazy val `val`      : Syntax[Token] = kw("val")
  lazy val `if`       : Syntax[Token] = kw("if")
  lazy val `else`     : Syntax[Token] = kw("else")
  lazy val `error`    : Syntax[Token] = kw("error")
  lazy val `match`    : Syntax[Token] = kw("match")

  // ==============================================================================================
  // ====================================== OPERATORS =============================================
  // ==============================================================================================

  // Rename operators to their names plus --> +
  @targetName("plus")       lazy val plus  : Syntax[String] = op("+") map (_ => "+")
  @targetName("minus")      lazy val minus  : Syntax[String] = op("-") map (_ => "-")
  @targetName("times")      lazy val times  : Syntax[String] = op("*") map (_ => "*")
  @targetName("div")        lazy val div  : Syntax[String] = op("/") map (_ => "/")
  @targetName("not")        lazy val not  : Syntax[String] = op("!") map (_ => "!")
  @targetName("mod")        lazy val mod  : Syntax[String] = op("%") map { _ => "%" }
  @targetName("lessThan")   lazy val lessThan  : Syntax[String] = op("<") map { _ => "<" }
  @targetName("lessThanEq") lazy val lessThanEq : Syntax[String] = op("<=") map { _ => "<=" }
  @targetName("and")        lazy val and : Syntax[String] = op("&&") map { _ => "&&" }
  @targetName("or")         lazy val or : Syntax[String] = op("||") map { _ => "||" }
  @targetName("equals")     lazy val equals : Syntax[String] = op("==") map { _ => "==" }
  @targetName("concat")     lazy val concat : Syntax[String] = op("++") map { _ => "++" }

  // ==============================================================================================
  // ================================== TOP LEVEL - PROGRAM =======================================
  // ==============================================================================================

  // An entire program (the starting rule for any Amy file).
  /**
    *
    */
  lazy val program: Syntax[Program] =
    many1(many1(module) ~<~ eof) map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  // ==============================================================================================
  // ====================================== AMY MODULE ============================================
  // ==============================================================================================

  // A module (i.e., a collection of definitions and an initializer expression)
  /**
    *
    */
  lazy val module: Syntax[ModuleDef] =
    (`object` ~ identifier ~
      many(definition) ~
      opt(expr) ~
      `end` ~ identifier) map {
    case obj ~ id ~ defs ~ body ~ _ ~ id1 =>
      if id == id1 then
        ModuleDef(id, defs.toList, body).setPos(obj)
      else
        throw AmycFatalError(s"Begin and end module names do not match: $id and $id1")
  }

  // ==============================================================================================
  // ==================================== DEFINITIONS =============================================
  // ==============================================================================================

  /**
    *
    */
  lazy val definition: Syntax[ClassOrFunDef] =
    funDef.up[ClassOrFunDef] | abstractClassDef.up[ClassOrFunDef] | caseClassDef.up[ClassOrFunDef]

  /**
    *
    */
  lazy val abstractClassDef: Syntax[AbstractClassDef] =
    (`abstract` ~ `class` ~ identifier) map {
      case _ ~ _ ~ className => AbstractClassDef(className)
    }

  /**
    *
    */
  lazy val caseClassDef: Syntax[CaseClassDef] =
    (`case` ~ `class` ~ identifier ~ inParenthesis(parameters) ~ `extends` ~ identifier) map {
      case _ ~ _ ~ className ~ params ~ _ ~ superClassName =>
        CaseClassDef(className, params.map(_.tt), superClassName)
    }

  /**
    *
    */
  lazy val funDef: Syntax[FunDef] =
    (`fn` ~>~ identifier ~ inParenthesis(parameters) ~<~ ":" ~ typeTree ~<~ "=" ~ inBrace(expr)) map {
      case funcName ~ params ~ tpe ~ expr =>
        FunDef(funcName, params, tpe, expr)
    }

  // ==============================================================================================
  // ===================================== IDENTIFIERS ============================================
  // ==============================================================================================

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }
  // A QualifiedName (identifier.identifier)
  def qualifiedName: Syntax[(Option[String], String)] =
    (identifier ~ opt("." ~>~ identifier)) map {
      case id ~ Some(id2) => (Some(id), id2)
      case id ~ None => (None, id)
    }

  // ==============================================================================================
  // ======================================== TYPES ===============================================
  // ==============================================================================================

  // A type expression.
  lazy val typeTree: Syntax[TypeTree] = recursive {
      primitiveType | identifierType | functionType
    }

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
    case _ ~ Some(_) =>
      throw AmycFatalError("Only Int type can have a specific width")
    case prim ~ None => prim
  }

  // A user-defined type (such as `List`).
  lazy val identifierType: Syntax[TypeTree] =
    qualifiedName map {
      case (Some(id), id2) => TypeTree(ClassType(QualifiedName(Some(id), id2)))
      case (None, id) => TypeTree(ClassType(QualifiedName(None, id)))
    }

  lazy val functionType: Syntax[TypeTree] =
    ("(" ~>~ repsep(typeTree, ",") ~<~ ")" ~<~ "=>" ~ typeTree) map {
      case params ~ rte => TypeTree(FunctionType(params.toList, rte))
    }

  // ==============================================================================================
  // ===================================== PARAMETERS =============================================
  // ==============================================================================================

  // A list of parameter definitions.
  lazy val parameters: Syntax[List[ParamDef]] =
    repsep(parameterDef, ",").map(_.toList)

  // A parameter definition, i.e., an identifier along with the expected type.
  lazy val parameterDef: Syntax[ParamDef] =
    (identifier ~<~ ":" ~ typeTree) map {
      case name ~ tpe => ParamDef(name, tpe)
    }

  lazy val args : Syntax[Seq[Expr]] =
    repsep(expr, ",")

  // ==============================================================================================
  // =================================== PATTERN MATCHING =========================================
  // ==============================================================================================

  lazy val matchCase : Syntax[MatchCase] =
    (`case` ~>~ pattern ~<~ "=>" ~ expr) map {
      case pattern ~ expr => MatchCase(pattern, expr)
    }

  // ==============================================================================================
  // ======================================== PATTERNS ============================================
  // ==============================================================================================

  lazy val patterns : Syntax[Seq[Pattern]] = recursive {
    repsep(pattern, ",")
  }

  // A pattern as part of a mach case.
  lazy val pattern: Syntax[Pattern] =
    idOrCaseClassPattern.up[Pattern] | literalPattern.up[Pattern] | wildPattern.up[Pattern]

  // TODO HR : Restrict return type
  lazy val literalPattern: Syntax[Pattern] =
    (literal map (LiteralPattern(_))) | unitPattern

  // TODO HR : Restrict return type
  lazy val unitPattern : Syntax[Pattern] =
    ("(" ~ ")") map (_ => LiteralPattern(new UnitLiteral))

  lazy val wildPattern: Syntax[WildcardPattern] =
    `kw_` map (_ => WildcardPattern())

  lazy val idOrCaseClassPattern : Syntax[IdPattern | CaseClassPattern] =
    (qualifiedName ~ opt(inParenthesis(patterns))) map {
      // Only an identifier
      case (None, id) ~ None =>
        IdPattern(id)
      // Case class wih simple name or qualified name
      case (None, id) ~ Some(patterns) =>
        CaseClassPattern(QualifiedName(None, id), patterns.toList)
      case (Some(id), id2) ~ Some(patterns) =>
        CaseClassPattern(QualifiedName(Some(id), id2), patterns.toList)
      case (Some(id), id2) ~ None =>
        throw AmycFatalError(s"Cannot have qualified name $id.$id2 without parameters")
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
    (`val` ~>~ parameterDef ~<~ "=" ~ simpleExpression ~<~ ";" ~ expr) map {
      case param ~ assign ~ rhs => Let(param, assign, rhs)
    }

  // Sequence definition
  lazy val sequenceExpression : Syntax[Expr] =
    (simpleExpression ~ opt(";" ~>~ expr)) map {
      case expr ~ None => expr
      case lhs ~ Some(rhs) => Sequence(lhs, rhs)
    }

  // ----------------------- Second level expressions ------------------------------

  lazy val ifExpression : Syntax[Expr] =
    (`if` ~>~ inParenthesis(expr) ~ inBrace(expr) ~<~ `else` ~ inBrace(expr)) map {
      case cond ~ trueBranch ~ falseBranch =>
        Ite(cond, trueBranch, falseBranch)
    }

  lazy val simpleExpression : Syntax[Expr] = recursive {
    (ifOrBinary ~ many(`match` ~>~ inBrace(many1(matchCase)))) map {
      case expr ~ Seq() => expr
      case scrut ~ s =>
        s.foldLeft(scrut)((a, b) => Match(a, b.toList))
    }
  }

  lazy val ifOrBinary: Syntax[Expr] = ifExpression | binaryExpression

  lazy val binaryExpression : Syntax[Expr] =
    operators(termExpression)(
      // Defines the different operators, by decreasing priority.
      times | div | mod  is LeftAssociative,
      plus | minus | concat is LeftAssociative,
      lessThan is LeftAssociative,
      lessThanEq is LeftAssociative,
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
        case ( _ , op , _ ) =>
          throw AmycFatalError(s"Binary operator '$op' is not defined !")
      }

  // ------------------------- Third Level expressions ------------------------------

  lazy val termExpression : Syntax[Expr] = recursive {
    (opt(minus | not) ~ factor) map {
      case None ~ expr => expr
      case Some("-") ~ expr => Neg(expr)
      case Some("!") ~ expr => Not(expr)
      case Some(op) ~ _ =>
        throw AmycFatalError(s"Unary operator '$op' is not defined !")
    }
  }

  // --------------------- Fourth Level expressions --------------------------

  lazy val factor : Syntax[Expr] =
    errorExpression | literal.up[Expr] | variableOrCall | valExpressionOrUnit

  lazy val errorExpression : Syntax[Expr] =
    (`error` ~>~ termExpression) map { x => Error(x)}

  lazy val variableOrCall: Syntax[Expr] =
    (qualifiedName ~ opt(inParenthesis(args))) map {
      case (Some(id), id2) ~ Some(args) =>
        Call(QualifiedName(Some(id), id2), args.toList)
      case (None, id) ~ Some(args) =>
        Call(QualifiedName(None, id), args.toList)
      case (None, id) ~ None =>
        Variable(id)
      case (Some(id), id2) ~ None =>
        throw AmycFatalError(s"Call to $id.$id2 is missing the parameters")
    }

  lazy val valExpressionOrUnit : Syntax[Expr] =
    inParenthesis(opt(expr)) map {
      case Some(expr) => expr
      case None => new UnitLiteral
    }

  // ==============================================================================================
  // ===================================== LITERALS ===============================================
  // ==============================================================================================

  // A literal expression.
  lazy val literal: Syntax[Literal[Boolean | Int | String]] =
    accept(LiteralKind) {
      case BoolLitToken(value) => BooleanLiteral(value)
      case IntLitToken(value) => IntLiteral(value)
      case StringLitToken(value) => StringLiteral(value)
      case tk =>
        throw AmycFatalError(
          s"""Token of type 'LiteralKind' not defined in the Parser.
             |Token is : $tk
             |Need to add it a new match case in syntax 'literal'.""".stripMargin
        )
    }

  // ==============================================================================================

  // Ensures the grammar is in LL(1)
  lazy val checkLL1: Boolean = {
    if (program.isLL1) {
      true
    } else {
      // Set `showTrails` to true to make Scallion generate some counterexamples for you.
      // Depending on your grammar, this may be very slow.
      val showTrails = false
      debug(program, showTrails)
      false
    }
  }

  override val name = "Parser"

  override def run(tokens: Iterator[Token])(using core.Context): Program = {
    if (!checkLL1) {
      ctx.reporter.fatal("Program grammar is not LL1!")
    }

    val parser = Parser(program)

    parser(tokens) match {
      case Parsed(result, _) => result
      case UnexpectedEnd(_) => ctx.reporter.fatal("Unexpected end of input.")
      case UnexpectedToken(token, rest) => ctx.reporter.fatal(s"Unexpected token: $token, possible kinds: ${rest.first.map(_.toString).mkString(", ")}")
    }
  }
}
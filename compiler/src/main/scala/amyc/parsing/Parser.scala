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
import amyc.parsing.keywords.Keyword
import scallion.{~, *}

import scala.annotation.targetName

// The parser for Amy
object Parser extends Pipeline[Iterator[Token], Program] with Parsers {

  type Token = amyc.parsing.Token
  type Kind = amyc.parsing.TokenKind

  import Implicits._
  import keywords.*

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  // ==============================================================================================
  // ================================== HELPER FUNCTIONS ==========================================
  // ==============================================================================================

  inline def inBrace[A](inline syntax : Syntax[A]) : Syntax[A] =
    "{" ~>~ syntax ~<~ "}"

  inline def inParenthesis[A](inline syntax : Syntax[A]) : Syntax[A] =
    "(" ~>~ syntax ~<~ ")"

  inline def op(string: String): Syntax[Token] = elem(OperatorKind(string))
  inline implicit def kw(inline k: Keyword): Syntax[Token] = elem(KeywordKind(k.toString))
  inline implicit def delimiter(inline string: String): Syntax[Token] = elem(DelimiterKind(string))

  // ==============================================================================================
  // ========================================== EOF ===============================================
  // ==============================================================================================

  lazy val eof: Syntax[Token] = elem(EOFKind)

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
    many1(many1(module_def) ~<~ eof) map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  // ==============================================================================================
  // ====================================== AMY MODULE ============================================
  // ==============================================================================================

  // A module (i.e., a collection of definitions and an initializer expression)
  /**
    *
    */
  lazy val module_def: Syntax[ModuleDef] =
    (`module` ~ identifier ~
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
    (`case` ~ `class` ~ identifier ~ inParenthesis(parameters) ~ ":" ~ identifier) map {
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
  def qualifiedName: Syntax[QualifiedName | Name | FunRef] =
    (identifier ~ opt(("." | "::") ~ identifier)) map {
      case id ~ Some(DelimiterToken(".") ~ id2) => QualifiedName(Some(id), id2)
      case id ~ Some(DelimiterToken("::") ~ id2) => FunRef(QualifiedName(Some(id), id2))
      case id ~ None => id
    }

  // ==============================================================================================
  // ======================================== TYPES ===============================================
  // ==============================================================================================

  // A type expression.
  lazy val typeTree: Syntax[TypeTree] = recursive {
      primitiveType | identifierType | functionType
    }

  // A built-in type (such as `Int`).
  val primitiveType: Syntax[TypeTree] = accept(PrimTypeKind) {
    case tk@PrimTypeToken(name) => TypeTree(name match {
      case "Unit" => UnitType
      case "Boolean" => BooleanType
      case "Int" => IntType
      case "String" => StringType
      case _ => throw new java.lang.Error("Unexpected primitive type name: " + name)
    }).setPos(tk)
  }

  // A user-defined type (such as `List`).
  lazy val identifierType: Syntax[TypeTree] =
    qualifiedName map {
      case qn: QualifiedName => TypeTree(ClassType(qn))
      case id: Name => TypeTree(ClassType(QualifiedName(None, id)))
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
    wildcard map (_ => WildcardPattern())

  lazy val idOrCaseClassPattern : Syntax[IdPattern | CaseClassPattern] =
    (qualifiedName ~ opt(inParenthesis(patterns))) map {
      // Only an identifier
      case (id: Name) ~ None => IdPattern(id)
      // Case class wih simple name or qualified name
      case (id: Name) ~ Some(patterns) =>
        CaseClassPattern(QualifiedName(None, id), patterns.toList)
      case (qn:QualifiedName) ~ Some(patterns) =>
        CaseClassPattern(qn, patterns.toList)
      case QualifiedName(Some(id), id2) ~ None =>
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
        case (lhs, op, rhs) => InfixCall(lhs, op, rhs)
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
      case (qn: QualifiedName) ~ Some(args) =>
        Call(qn, args.toList)
      case (id: Name) ~ Some(args) =>
        Call(QualifiedName(None, id), args.toList)
      case (id: Name) ~ None => Variable(id)
      case (fr: FunRef) ~ None => fr
      case (_: FunRef) ~ Some(_) =>
        throw AmycFatalError(s"Cannot reference to a function with parameters")
      case QualifiedName(Some(id), id2) ~ None =>
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
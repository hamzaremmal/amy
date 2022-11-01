package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Tokens._
import TokenKinds._

import scallion._

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
    (delimiter("{") ~ syntax ~ delimiter("}")) map {
      case _ ~ expr ~ _ => expr
    }

  def inParenthesis[A](syntax : => Syntax[A]) : Syntax[A] =
    (delimiter("(") ~ syntax ~ delimiter(")")) map {
      case _ ~ expr ~ _ => expr
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
  // ===================================== IDENTIFIERS ============================================
  // ==============================================================================================

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }

  // An identifier along with its position.
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id@IdentifierToken(name) => (name, id.position)
  }

  // ==============================================================================================
  // ===================================== PARAMETERS =============================================
  // ==============================================================================================
    
  // A list of parameter definitions.
  lazy val parameters: Syntax[List[ParamDef]] =
    repsep(parameterDef, ",").map(_.toList)

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
    (opt(identifier ~ delimiter(".")) ~ identifier) map {
      case Some(id ~ _) ~ id2 => TypeTree(ClassType(QualifiedName(Some(id), id2)))
      case None ~ id => TypeTree(ClassType(QualifiedName(None, id)))
    }

  // ==============================================================================================
  // ====================================== OPERATORS =============================================
  // ==============================================================================================

  lazy val binaryOperator : Syntax[Token] = accept(OperatorKind("<Operator>")){
    case tk@OperatorToken("+") => tk
    case tk@OperatorToken("-") => tk
    case tk@OperatorToken("*") => tk
    case tk@OperatorToken("/") => tk
    case tk@OperatorToken("%") => tk
    case tk@OperatorToken("<") => tk
    case tk@OperatorToken("<=") => tk
    case tk@OperatorToken("&&") => tk
    case tk@OperatorToken("||") => tk
    case tk@OperatorToken("==") => tk
    case tk@OperatorToken("++") => tk
    case _ => ??? // TODO HR throw an exception illegal operator
  }

  lazy val unaryOperator : Syntax[Token] =
    accept(OperatorKind("<Operator>")) {
      case tk@OperatorToken("-") => tk
      case tk@OperatorToken("!") => tk
      case _ => ??? // TODO HR throw an exception illegal operator
    }

  // ==============================================================================================
  // ==================================== LITTERALS ===============================================
  // ==============================================================================================
    
  // A literal expression.
  lazy val literal: Syntax[Literal[_]] =
    accept(LiteralKind){
      case BoolLitToken(value) => BooleanLiteral(value)
      case IntLitToken(value) => IntLiteral(value)
      case StringLiteral(value) => StringLiteral(value)
    }.up[Literal[_]] //| ((delimiter("(") ~ delimiter(")")) map (_ => new UnitLiteral)).up[Literal[_]]

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

  lazy val patterns : Syntax[Seq[Pattern]] =
    repsep(pattern, delimiter(","))
    
  // A pattern as part of a mach case.
  lazy val pattern: Syntax[Pattern] = recursive {
    idOrCaseClassPattern | literalPattern.up[Pattern] | wildPattern.up[Pattern]
  }
    
  lazy val literalPattern: Syntax[Pattern] =
    (literal map (LiteralPattern(_))).up[Pattern]
    
  lazy val wildPattern: Syntax[WildcardPattern] =
    delimiter("_") map (_ => new WildcardPattern())

  lazy val idOrCaseClassPattern : Syntax[Pattern] =
    (opt(identifier ~ delimiter(".")) ~ identifier ~ opt(inParenthesis(patterns))) map {
      case Some(id ~ _) ~ id2 ~ Some(patterns) => CaseClassPattern(QualifiedName(Some(id), id2), patterns.toList)
      case None ~ id2 ~ Some(patterns)  => CaseClassPattern(QualifiedName(None, id2), patterns.toList)
      // Only an identifier
      case None ~ id ~ None => IdPattern(id)
      case _ => ??? // TODO HR : throw an exception
    }

  // ==============================================================================================
  // ==================================== DEFINITIONS =============================================
  // ==============================================================================================

  // A definition within a module.
  lazy val definition: Syntax[ClassOrFunDef] =
    abstractClassDef.up[ClassOrFunDef] | caseClassDef.up[ClassOrFunDef] | funDef.up[ClassOrFunDef]

  lazy val abstractClassDef : Syntax[AbstractClassDef] =
    (kw("abstract") ~ kw("class") ~ identifier) map {
      case _ ~ _ ~ className => AbstractClassDef(className)
    }

  lazy val caseClassDef : Syntax[CaseClassDef] =
    (kw("case") ~ kw("class") ~ identifier ~ inParenthesis(parameters) ~ kw("extends") ~ identifier) map {
      case _ ~ _ ~ className ~ params ~ _ ~ superClassName =>
        CaseClassDef(className, params.map(_.tt), superClassName)
    }

  lazy val funDef : Syntax[FunDef] =
    (kw("fn") ~
    identifier ~
    inParenthesis(parameters) ~
    delimiter(":")  ~
    typeTree ~
    delimiter("=") ~
    inBrace(expr)) map {
      case _ ~ funcName ~ params ~ _ ~ tpe ~ _ ~ expr=>
        FunDef(funcName, params, tpe, expr)
    }
    
  // ============================================================================================
  // ====================================== EXPRESSIONS =========================================
  // ============================================================================================

  // An expression.
  // HINT: You can use `operators` to take care of associativity and precedence
  lazy val expr: Syntax[Expr] = recursive {
      simpleExpr |
      ifExpression.up[Expr] |
      sequenceExpression.up[Expr] |
      variableDeclaration.up[Expr] |
      matchExpression.up[Expr]
  }

  // HINT: It is useful to have a restricted set of expressions that don't include any more operators on the outer level.
  lazy val simpleExpr: Syntax[Expr] = recursive {
    literal.up[Expr] |
      variableOrCall |
      binaryExpression |
      unaryExpression |
      errorExpression.up[Expr] |
      parenthesisExpression
  }
    
  lazy val variableOrCall: Syntax[Expr] =
    (opt(identifier ~ delimiter(".")) ~ identifier ~ opt(inParenthesis(args))) map {
      case Some(id ~ _) ~ id2 ~ Some(args) => Call(QualifiedName(Some(id), id2), args.toList)
      case None ~ id2 ~ Some(args) => Call(QualifiedName(None, id2), args.toList)
      case Some(id ~ _) ~ id2 ~ None => ??? // Variable(QualifiedName(Some(id), id2)) TODO HR : Analyze this behavior if illegal throw Error
      case None ~ id2 ~ None => Variable(id2)
    }

  lazy val sequenceExpression : Syntax[Sequence] = recursive {
    (expr ~ delimiter(";") ~ expr) map {
      case lhs ~ _ ~ rhs => Sequence(lhs, rhs)
    }
  }

  lazy val variableDeclaration : Syntax[Let] = recursive {
    (kw("val") ~ parameterDef ~ delimiter("=") ~
      expr ~ delimiter(";") ~
      expr) map {
      case _ ~ param ~ _ ~ value ~ _ ~ expr => Let(param, value, expr)
    }
  }

  lazy val ifExpression : Syntax[Ite] = recursive {
    (kw("if") ~ parenthesisExpression ~ braceExpression ~ kw("else") ~ braceExpression) map {
      case _ ~ cond ~ thenn ~ _ ~ elze => Ite(cond, thenn, elze)
    }
  }

  lazy val matchExpression : Syntax[Match] = recursive {
    (expr ~ kw("match") ~ inBrace(many1(matchCase))) map {
      case scrut ~ _ ~ cases => Match(scrut, cases.toList)
    }
  }

  lazy val parenthesisExpression : Syntax[Expr] =  inParenthesis(expr)

  lazy val braceExpression : Syntax[Expr] = inBrace(expr)

  lazy val unaryExpression : Syntax[Expr] = recursive {
    (unaryOperator ~ expr) map {
      case OperatorToken("-") ~ rhs => Neg(rhs)
      case OperatorToken("!") ~ rhs => Not(rhs)
      case _ => ??? // TODO Add message error for unknown operator
    }
  }

  lazy val binaryExpression : Syntax[Expr] = recursive {
    (expr ~ binaryOperator ~ expr) map {
      case lhs ~ OperatorToken("+") ~ rhs => Plus(lhs, rhs)
      case lhs ~ OperatorToken("-") ~ rhs => Minus(lhs, rhs)
      case lhs ~ OperatorToken("*") ~ rhs => Times(lhs, rhs)
      case lhs ~ OperatorToken("/") ~ rhs => Div(lhs, rhs)
      case lhs ~ OperatorToken("%") ~ rhs => Mod(lhs, rhs)
      case lhs ~ OperatorToken("<") ~ rhs => LessThan(lhs, rhs)
      case lhs ~ OperatorToken("<=") ~ rhs => LessEquals(lhs, rhs)
      case lhs ~ OperatorToken("&&") ~ rhs => And(lhs, rhs)
      case lhs ~ OperatorToken("||") ~ rhs => Or(lhs, rhs)
      case lhs ~ OperatorToken("==") ~ rhs => Equals(lhs, rhs)
      case lhs ~ OperatorToken("++") ~ rhs => Concat(lhs, rhs)
      case _ => ??? // TODO Add message error for unknown operator
    }
  }

  lazy val errorExpression : Syntax[Error] = recursive {
    (kw("error") ~ inParenthesis(expr)) map {
      case _ ~ expr => Error(expr)
    }
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
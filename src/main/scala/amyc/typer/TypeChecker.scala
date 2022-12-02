package amyc.typer

import amyc.analyzer.SymbolTable
import amyc.ast.Identifier
import amyc.ast.SymbolicTreeModule.*
import amyc.utils.{Context, Pipeline}

object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)]{

  override def run(v: (Program, SymbolTable))(using Context) =
    //check(v._1) TODO HR : Remove the comment here when the TypeChecker is ready
    v

  def check(tree: Tree)(using Context): Tree =
    tree match
      case v : Variable => checkVariable(v)
      case i : IntLiteral => checkIntLiteral(i)
      case b : BooleanLiteral => checkBooleanLiteral(b)
      case s : StringLiteral => checkStringLiteral(s)
      case u : UnitLiteral => checkUnitLiteral(u)
      case op : Plus => checkPlusOp(op)
      case op : Minus => checkMinusOp(op)
      case op : Times => checkTimesOp(op)
      case op : Div => checkDivOp(op)
      case op : Mod => checkModOp(op)
      case op : LessThan => checkLessThan(op)
      case op : LessEquals => checkLessEquals(op)
      case op : And => checkAnd(op)
      case op : Or => checkOr(op)
      case op : Equals => checkEquals(op)
      case op : Concat => checkConcat(op)
      case op : Not => checkNot(op)
      case op : Neg => checkNeg(op)
      case op : Call => checkCall(op)
      case seq : Sequence => checkSequence(seq)
      case let : Let => checkLet(let)
      case ite : Ite => checkIte(ite)
      case m : Match => checkMatch(m)
      case e : Error => checkError(e)
      case m : MatchCase => checkMatchCase(m)
      case t : WildcardPattern => checkWildCardPattern(t)
      case t : IdPattern => checkIdPattern(t)
      case t : LiteralPattern[_] => checkLiteralPattern(t)
      case t : CaseClassPattern => checkCaseClassPattern(t)
      case p : Program => checkProgram(p)
      case m : ModuleDef => checkModule(m)
      case fn : FunDef => checkFunctionDefinition(fn)
      case cls : AbstractClassDef => checkAbstractClassDef(cls)
      case cls : CaseClassDef => checkCaseClassDef(cls)
      case df : ParamDef => checkParamDef(df)
      case tt : TypeTree => checkType(tt)
      case _ => reporter.fatal(s"TypeChecker cannot handle tree of type $tree")

  def verifiy(actual: Type, expected: Type)(using Context)=
    if actual != expected then
      reporter.error(
        s"""Found    : $actual
           |Expected : $expected
           |""".stripMargin)


  /**
    * Type Check a variable
    * @param v
    * @param env
    * @param Context
    */
  def checkVariable(v: Variable)(using Context) =
    ???

  def checkIntLiteral(i: IntLiteral)(using Context) =
    ???

  def checkBooleanLiteral(b: BooleanLiteral)(using Context) =
    verifiy(b.tpe, BooleanType)
    b

  def checkStringLiteral(s: StringLiteral)(using Context) =
    verifiy(s.tpe, StringType)
    s

  def checkUnitLiteral(u: UnitLiteral)(using Context) =
    verifiy(u.tpe, UnitType)
    u

  def checkPlusOp(expr: Plus)(using Context) =
    verifiy(check(expr.lhs).tpe, IntType) // LHS =:= Int
    verifiy(check(expr.rhs).tpe, IntType) // RHS =:= Int
    verifiy(expr.tpe, IntType) // (LHS + RHS) =:= Int
    expr
  def checkMinusOp(expr: Minus)(using Context) =
    ???

  def checkTimesOp(expr: Times)(using Context) =
    ???

  def checkDivOp(expr: Div)(using Context)=
    ???

  def checkModOp(expr: Mod)(using Context) =
    ???

  def checkLessThan(expr: LessThan)(using Context) =
    ???

  def checkLessEquals(expr: LessEquals)(using Context) =
    ???

  def checkAnd(expr: And)(using Context) =
    ???

  def checkOr(expr: Or)(using Context) =
    ???

  def checkEquals(expr: Equals)(using Context) =
    ???

  def checkConcat(expr: Concat)(using Context)=
    ???

  def checkNot(expr: Not)(using Context) =
    ???

  def checkNeg(expr: Neg)(using Context) =
    ???

  def checkCall(expr: Call)(using Context) =
    ???

  def checkSequence(seq: Sequence)(using Context) =
    ???

  def checkLet(expr: Let)(using Context) =
    ???

  def checkIte(expr: Ite)(using Context) =
    ???

  def checkMatch(expr: Match)(using Context) =
    ???

  def checkError(expr: Error)(using Context) =
    ???

  def checkMatchCase(expr: MatchCase)(using Context) =
    ???

  def checkWildCardPattern(expr: WildcardPattern)(using Context) =
    ???

  def checkIdPattern(expr: IdPattern)(using Context) =
    ???

  def checkLiteralPattern[T](expr: LiteralPattern[T])(using Context) =
    ???

  def checkCaseClassPattern(expr: CaseClassPattern)(using Context) =
    ???

  def checkModule(module: ModuleDef)(using Context) =
    ???

  def checkFunctionDefinition(fn: FunDef)(using Context) =
    ???

  def checkAbstractClassDef(cls: AbstractClassDef)(using Context) =
    ???

  def checkCaseClassDef(cls: CaseClassDef)(using Context) =
    ???

  def checkParamDef(df: ParamDef)(using Context) =
    ???

  def checkProgram(prog: Program)(using Context) =
    ???

  def checkType(tt : TypeTree)(using Context) =
    ???

}
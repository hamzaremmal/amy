package amyc.typer

import amyc.analyzer.SymbolTable
import amyc.ast.Identifier
import amyc.ast.SymbolicTreeModule.*
import amyc.utils.{Context, Pipeline}

object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)]{

  override def run(v: (Program, SymbolTable))(using Context) = ???

  def check(tree: Tree)(using Context): Tree = ???
    // TODO HR : Big pattern matching here to check each case separately

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
  def checkVariable(v: Variable)(env : Map[Identifier, Type])(using Context) =
    val expected = env.getOrElse(v.name, reporter.error(s"variable ${v.name} is not declared"))
    //verifiy(v.tpe, expected) TODO HR : Fix here
    v

  def checkIntLiteral(i: IntLiteral)(using Context) =
    verifiy(i.tpe, IntType)
    i

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

}
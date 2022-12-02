package amyc.typer

import amyc.analyzer.SymbolTable
import amyc.ast.SymbolicTreeModule.*
import amyc.utils.{Context, Pipeline}

object TypeAssigner extends Pipeline[(Program, SymbolTable, List[(Type, Type)]), (Program, SymbolTable)]{

  override def run(v: (Program, SymbolTable, List[(Type, Type)]))(using Context) =
    (v._1, v._2)


  def assign(tree: Tree)(using Context) =
    tree match
      case v: Variable => assignVariable(v)
      case i: IntLiteral => assignIntLiteral(i)
      case b: BooleanLiteral => assignBooleanLiteral(b)
      case s: StringLiteral => assignStringLiteral(s)
      case u: UnitLiteral => assignUnitLiteral(u)
      case op: Plus => assignPlusOp(op)
      case op: Minus => assignMinusOp(op)
      case op: Times => assignTimesOp(op)
      case op: Div => assignDivOp(op)
      case op: Mod => assignModOp(op)
      case op: LessThan => assignLessThan(op)
      case op: LessEquals => assignLessEquals(op)
      case op: And => assignAnd(op)
      case op: Or => assignOr(op)
      case op: Equals => assignEquals(op)
      case op: Concat => assignConcat(op)
      case op: Not => assignNot(op)
      case op: Neg => assignNeg(op)
      case op: Call => assignCall(op)
      case seq: Sequence => assignSequence(seq)
      case let: Let => assignLet(let)
      case ite: Ite => assignIte(ite)
      case m: Match => assignMatch(m)
      case e: Error => assignError(e)
      case m: MatchCase => assignMatchCase(m)
      case t: WildcardPattern => assignWildCardPattern(t)
      case t: IdPattern => assignIdPattern(t)
      case t: LiteralPattern[_] => assignLiteralPattern(t)
      case t: CaseClassPattern => assignCaseClassPattern(t)
      case p: Program => assignProgram(p)
      case m: ModuleDef => assignModule(m)
      case fn: FunDef => assignFunctionDefinition(fn)
      case cls: AbstractClassDef => assignAbstractClassDef(cls)
      case cls: CaseClassDef => assignCaseClassDef(cls)
      case df: ParamDef => assignParamDef(df)
      case tt: TypeTree => assignType(tt)
      case _ => reporter.fatal(s"TypeAssigner cannot handle tree of type $tree")
  def assignVariable(v: Variable)(using Context) =
    ???

  def assignIntLiteral(i: IntLiteral)(using Context) =
    ???

  def assignBooleanLiteral(b: BooleanLiteral)(using Context) =
    ???

  def assignStringLiteral(s: StringLiteral)(using Context) =
    ???

  def assignUnitLiteral(u: UnitLiteral)(using Context) =
    ???

  def assignPlusOp(expr: Plus)(using Context) =
    ???

  def assignMinusOp(expr: Minus)(using Context) =
    ???

  def assignTimesOp(expr: Times)(using Context) =
    ???

  def assignDivOp(expr: Div)(using Context) =
    ???

  def assignModOp(expr: Mod)(using Context) =
    ???

  def assignLessThan(expr: LessThan)(using Context) =
    ???

  def assignLessEquals(expr: LessEquals)(using Context) =
    ???

  def assignAnd(expr: And)(using Context) =
    ???

  def assignOr(expr: Or)(using Context) =
    ???

  def assignEquals(expr: Equals)(using Context) =
    ???

  def assignConcat(expr: Concat)(using Context) =
    ???

  def assignNot(expr: Not)(using Context) =
    ???

  def assignNeg(expr: Neg)(using Context) =
    ???

  def assignCall(expr: Call)(using Context) =
    ???

  def assignSequence(seq: Sequence)(using Context) =
    ???

  def assignLet(expr: Let)(using Context) =
    ???

  def assignIte(expr: Ite)(using Context) =
    ???

  def assignMatch(expr: Match)(using Context) =
    ???

  def assignError(expr: Error)(using Context) =
    ???

  def assignMatchCase(expr: MatchCase)(using Context) =
    ???

  def assignWildCardPattern(expr: WildcardPattern)(using Context) =
    ???

  def assignIdPattern(expr: IdPattern)(using Context) =
    ???

  def assignLiteralPattern[T](expr: LiteralPattern[T])(using Context) =
    ???

  def assignCaseClassPattern(expr: CaseClassPattern)(using Context) =
    ???

  def assignModule(module: ModuleDef)(using Context) =
    ???

  def assignFunctionDefinition(fn: FunDef)(using Context) =
    ???

  def assignAbstractClassDef(cls: AbstractClassDef)(using Context) =
    ???

  def assignCaseClassDef(cls: CaseClassDef)(using Context) =
    ???

  def assignParamDef(df: ParamDef)(using Context) =
    ???

  def assignProgram(prog: Program)(using Context) =
    ???

  def assignType(tt: TypeTree)(using Context) =
    ???

}

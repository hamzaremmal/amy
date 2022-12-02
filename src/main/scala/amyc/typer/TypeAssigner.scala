package amyc.typer

import amyc.analyzer.SymbolTable
import amyc.ast.SymbolicTreeModule.*
import amyc.utils.{Context, Pipeline}

object TypeAssigner extends Pipeline[(Program, SymbolTable, Map[Type, Type]), (Program, SymbolTable)]{

  override def run(v: (Program, SymbolTable, Map[Type, Type]))(using Context) =
    val (program, _, bindings) = v
    assign(program)(using bindings)
    (v._1, v._2)

  def infer(tree: Tree)(using bindings : Map[Type, Type])(using Context) =
    tree.tpe match
      case tv@TypeVariable(_) =>
        tree.withType{
          bindings.getOrElse(tv,
            reporter.fatal(s"TypeVariable ($tv) has leaked from the bindings while inferring the type ($bindings)"))
        }
      case NoType =>
        reporter.fatal(
          s"""
             |NoType was found at the TypeAssigner. This should not happen
             |Tree ${tree.getClass.getTypeName}
             |""".stripMargin)
      case _ => tree


  def assign(tree: Tree)(using Map[Type, Type])(using Context): Tree =
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


  def assignVariable(v: Variable)(using Map[Type, Type])(using Context) =
    infer(v)

  def assignIntLiteral(i: IntLiteral)(using Map[Type, Type])(using Context) =
    infer(i)

  def assignBooleanLiteral(b: BooleanLiteral)(using Map[Type, Type])(using Context) =
    infer(b)

  def assignStringLiteral(s: StringLiteral)(using Map[Type, Type])(using Context) =
    infer(s)

  def assignUnitLiteral(u: UnitLiteral)(using Map[Type, Type])(using Context) =
    infer(u)

  def assignPlusOp(expr: Plus)(using Map[Type, Type])(using Context) =
    val Plus(lhs, rhs) = expr
    assign(lhs)
    assign(rhs)
    infer(expr)

  def assignMinusOp(expr: Minus)(using Map[Type, Type])(using Context) =
    val Minus(lhs, rhs) = expr
    assign(lhs)
    assign(rhs)
    infer(expr)

  def assignTimesOp(expr: Times)(using Map[Type, Type])(using Context) =
    val Times(lhs, rhs) = expr
    assign(lhs)
    assign(rhs)
    infer(expr)

  def assignDivOp(expr: Div)(using Map[Type, Type])(using Context) =
    val Div(lhs, rhs) = expr
    assign(lhs)
    assign(rhs)
    infer(expr)

  def assignModOp(expr: Mod)(using Map[Type, Type])(using Context) =
    val Mod(lhs, rhs) = expr
    assign(lhs)
    assign(rhs)
    infer(expr)

  def assignLessThan(expr: LessThan)(using Map[Type, Type])(using Context) =
    val LessThan(lhs, rhs) = expr
    assign(lhs)
    assign(rhs)
    infer(expr)

  def assignLessEquals(expr: LessEquals)(using Map[Type, Type])(using Context) =
    val LessEquals(lhs, rhs) = expr
    assign(lhs)
    assign(rhs)
    infer(expr)

  def assignAnd(expr: And)(using Map[Type, Type])(using Context) =
    val And(lhs, rhs) = expr
    assign(lhs)
    assign(rhs)
    infer(expr)

  def assignOr(expr: Or)(using Map[Type, Type])(using Context) =
    val Or(lhs, rhs) = expr
    assign(lhs)
    assign(rhs)
    infer(expr)

  def assignEquals(expr: Equals)(using Map[Type, Type])(using Context) =
    val Equals(lhs, rhs) = expr
    assign(lhs)
    assign(rhs)
    infer(expr)

  def assignConcat(expr: Concat)(using Map[Type, Type])(using Context) =
    val Concat(lhs, rhs) = expr
    assign(lhs)
    assign(rhs)
    infer(expr)

  def assignNot(expr: Not)(using Map[Type, Type])(using Context) =
    val Not(e) = expr
    assign(e)
    infer(expr)

  def assignNeg(expr: Neg)(using Map[Type, Type])(using Context) =
    val Neg(e) = expr
    assign(e)
    infer(expr)

  def assignCall(expr: Call)(using Map[Type, Type])(using Context) =
    val Call(_, args) = expr
    for arg <- args do assign(arg)
    infer(expr)

  def assignSequence(seq: Sequence)(using Map[Type, Type])(using Context) =
    val Sequence(e1, e2) = seq
    assign(e1)
    assign(e2)
    infer(seq)

  def assignLet(expr: Let)(using Map[Type, Type])(using Context) =
    val Let(df, value, body) = expr
    assign(df)
    assign(value)
    assign(body)
    infer(expr)

  def assignIte(expr: Ite)(using Map[Type, Type])(using Context) =
    val Ite(cond, thenn, elze) = expr
    assign(cond)
    assign(thenn)
    assign(elze)
    infer(expr)

  def assignMatch(expr: Match)(using Map[Type, Type])(using Context) =
    ???

  def assignError(expr: Error)(using Map[Type, Type])(using Context) =
    val Error(msg) = expr
    assign(msg)
    infer(expr)

  def assignMatchCase(expr: MatchCase)(using Map[Type, Type])(using Context) =
    ???

  def assignWildCardPattern(expr: WildcardPattern)(using Map[Type, Type])(using Context) =
    ???

  def assignIdPattern(expr: IdPattern)(using Map[Type, Type])(using Context) =
    ???

  def assignLiteralPattern[T](expr: LiteralPattern[T])(using Map[Type, Type])(using Context) =
    ???

  def assignCaseClassPattern(expr: CaseClassPattern)(using Map[Type, Type])(using Context) =
    ???

  def assignModule(module: ModuleDef)(using Map[Type, Type])(using Context) =
    val ModuleDef(_, defs, expr) = module
    for df <- defs do assign(df)
    for e <- expr do assign(e)
    reporter.warning(s"Cannot assign a type to a Module for now")
    module

  def assignFunctionDefinition(fn: FunDef)(using Map[Type, Type])(using Context) =
    val FunDef(_, params, retType, body) = fn
    for param <- params do assign(param)
    assign(retType)
    assign(body)
    reporter.warning(s"Cannot assign a type to a Function Definition for now")
    fn

  def assignAbstractClassDef(cls: AbstractClassDef)(using Map[Type, Type])(using Context) =
    val AbstractClassDef(_) = cls
    reporter.warning("Cannot assign a type to an `abstract class` definition for now")
    cls

  def assignCaseClassDef(cls: CaseClassDef)(using Map[Type, Type])(using Context) =
    val CaseClassDef(_, fields, _) = cls
    for field <- fields do assign(field)
    reporter.warning("Cannot assign a type to a `case class` definition for now")
    cls

  def assignParamDef(df: ParamDef)(using Map[Type, Type])(using Context) =
    val ParamDef(_, tt) = df
    assign(tt)
    infer(df)

  def assignProgram (prog: Program)(using Map[Type, Type])(using Context) =
      for
        mod <- prog.modules
      do
        assign(mod)
      reporter.warning("Cannot assign a type to a Program for now")
      prog

  def assignType(tt: TypeTree)(using Map[Type, Type])(using Context) =
    infer(tt)

}

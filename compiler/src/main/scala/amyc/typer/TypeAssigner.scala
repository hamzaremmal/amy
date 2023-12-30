package amyc
package typer

import amyc.ast.SymbolicTreeModule.*
import amyc.core.Types.*
import amyc.core.Context
import amyc.core.Symbols.FunctionSymbol
import amyc.tools.Pipeline
import amyc.{ctx, reporter}

object TypeAssigner extends Pipeline[Program, Program]{

  override val name = "TypeAssigner"

  override def run(program: Program)(using Context) =
    assign(program)
    // TODO HR : This is just a patch for now, we should not call assign twice
    assign(program)
    
    program

  def isBounded(tpe: Type)(using Context) : Boolean =
    def bind(tpe: Type): Option[Type] =
      val b = ctx.tv.get(tpe)
      b match
        case Some(tv@TypeVariable(_)) => bind(tv)
        case Some(mtv@MultiTypeVariable()) => Some(mtv.bind.resolve)
        case _ => b
    bind(tpe).isDefined

  def bound(lhs: Type, rhs:Type)(using Context) =
    ctx.tv.update(lhs, rhs)
    rhs

  def infer(tree: Tree)(using Context) =
    def bind(tpe: TypeVariable) : Option[Type] =
      val b = ctx.tv.get(tpe)
      b match
        case Some(tv@TypeVariable(_)) => bind(tv)
        case Some(mtv@MultiTypeVariable()) => Some(mtv.bind.resolve)
        case _ =>
          b

    tree.tpe match
      case tv@TypeVariable(_) =>
        bind(tv).map(tree.withType).getOrElse(tree)
      case m : MultiTypeVariable =>
        val t = m.bind.resolve
        tree.withType(t)
      case NoType =>
        reporter.fatal(
          s"""
             |NoType was found at the TypeAssigner. This should not happen
             |Tree $tree
             |""".stripMargin)
      case _ => tree


  def assign(tree: Tree)(using Context): Tree =
    tree match
      case v: Variable => assignVariable(v)
      case f: FunRef => assignFunRef(f)
      case i: IntLiteral => assignIntLiteral(i)
      case b: BooleanLiteral => assignBooleanLiteral(b)
      case s: StringLiteral => assignStringLiteral(s)
      case u: UnitLiteral => assignUnitLiteral(u)
      case op: InfixCall => assignInfixCall(op)
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
      case df: ValParamDef => assignParamDef(df)
      case tt: TypeTree => assignType(tt)
      case _ => reporter.fatal(s"TypeAssigner cannot handle tree of type $tree")


  def assignVariable(v: Variable)(using Context) =
    infer(v)

  def assignFunRef(f: FunRef)(using Context) =
    infer(f)

  def assignIntLiteral(i: IntLiteral)(using Context) =
    infer(i)

  def assignBooleanLiteral(b: BooleanLiteral)(using Context) =
    infer(b)

  def assignStringLiteral(s: StringLiteral)(using Context) =
    infer(s)

  def assignUnitLiteral(u: UnitLiteral)(using Context) =
    infer(u)

  def assignInfixCall(expr: InfixCall)(using Context) =
    val InfixCall(lhs, _, rhs) = expr
    assign(lhs)
    assign(rhs)
    infer(expr)

  def assignNot(expr: Not)(using Context) =
    val Not(e) = expr
    assign(e)
    infer(expr)

  def assignNeg(expr: Neg)(using Context) =
    val Neg(e) = expr
    assign(e)
    infer(expr)

  def assignCall(expr: Call)(using Context) =
    val Call(_, _, args) = expr
    for arg <- args do assign(arg)
    infer(expr)

  def assignSequence(seq: Sequence)(using Context) =
    val Sequence(e1, e2) = seq
    assign(e1)
    assign(e2)
    infer(seq)

  def assignLet(expr: Let)(using Context) =
    val Let(df, value, body) = expr
    assign(df)
    assign(value)
    assign(body)
    infer(expr)

  def assignIte(expr: Ite)(using Context) =
    val Ite(cond, thenn, elze) = expr
    assign(cond)
    assign(thenn)
    assign(elze)
    infer(expr)
    expr

  def assignMatch(expr: Match)(using Context) =
    val Match(scrut, cases) = expr
    assign(scrut)
    for c <- cases do assign(c)
    infer(expr)

  def assignError(expr: Error)(using Context) =
    val Error(msg) = expr
    assign(msg)
    infer(expr)

  def assignMatchCase(expr: MatchCase)(using Context) =
    val MatchCase(pat, e) = expr
    assign(pat)
    assign(e)
    infer(expr)

  def assignWildCardPattern(expr: WildcardPattern)(using Context) =
    infer(expr)

  def assignIdPattern(expr: IdPattern)(using Context) =
    infer(expr)

  def assignLiteralPattern[T](expr: LiteralPattern[T])(using Context) =
    val LiteralPattern(lit) = expr
    assign(lit)
    infer(expr)

  def assignCaseClassPattern(expr: CaseClassPattern)(using Context) =
    val CaseClassPattern(_, patterns) = expr
    for pat <- patterns do assign(pat)
    infer(expr)

  def assignModule(module: ModuleDef)(using Context) =
    val ModuleDef(_, defs, expr) = module
    for df <- defs do assign(df)
    for e <- expr do assign(e)
    module

  def assignFunctionDefinition(fn: FunDef)(using Context) =
    val FunDef(_, tparams, vparams, retType, body) = fn
    for param <- vparams do assign(param)
    assign(retType)
    if ! (fn.name.asInstanceOf[FunctionSymbol] is "native") then
      assign(body)
      if !isBounded(body.tpe) then body.withType(bound(body.tpe, retType.tpe))
    fn

  def assignAbstractClassDef(cls: AbstractClassDef)(using Context) =
    val AbstractClassDef(_) = cls
    cls

  def assignCaseClassDef(cls: CaseClassDef)(using Context) =
    val CaseClassDef(_, fields, _) = cls
    for field <- fields do assign(field)
    cls

  def assignParamDef(df: ValParamDef)(using Context) =
    val ValParamDef(_, tt) = df
    assign(tt)
    infer(df)

  def assignProgram (prog: Program)(using Context) =
      for
        mod <- prog.modules
      do
        assign(mod)
      prog

  def assignType(tt: TypeTree)(using Context) =
    infer(tt)

}

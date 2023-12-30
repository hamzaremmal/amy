package amyc.typer

import amyc.ast.SymbolicTreeModule.*
import amyc.core.Context
import amyc.core.StdDefinitions.*
import amyc.core.StdTypes.*
import amyc.core.Symbols.{ConstructorSymbol, FunctionSymbol}
import amyc.core.Types.*
import amyc.utils.Pipeline
import amyc.{ctx, reporter}

object TypeChecker extends Pipeline[Program, Program]{

  override val name = "TypeChecker"

  override def run(program: Program)(using Context) =
    checkProgram(program)

  def check(tree: Tree)(using Context): Tree =
    tree match
      case v : Variable => checkVariable(v)
      case f : FunRef => checkFunRef(f)
      case i : IntLiteral => checkIntLiteral(i)
      case b : BooleanLiteral => checkBooleanLiteral(b)
      case s : StringLiteral => checkStringLiteral(s)
      case u : UnitLiteral => checkUnitLiteral(u)
      case InfixCall(_, _, _) =>
        reporter.fatal(s"Cannot type check infix operator $tree")
      case op : Not => checkNot(op)
      case op : Neg => checkNeg(op)
      case op : Call => checkCall(op)
      case seq : Sequence => checkSequence(seq)
      case let : Let => checkLet(let)
      case ite : Ite => checkIte(ite)
      case m : Match => checkMatch(m)
      case e : Error => checkError(e)
      case m : MatchCase => checkMatchCase(m, ErrorType)
      case t : WildcardPattern => checkWildCardPattern(t, ErrorType)
      case t : IdPattern => checkIdPattern(t,ErrorType)
      case t : LiteralPattern[_] => checkLiteralPattern(t, ErrorType)
      case t : CaseClassPattern => checkCaseClassPattern(t, ErrorType)
      case p : Program => checkProgram(p)
      case m : ModuleDef => checkModule(m)
      case fn : FunDef => checkFunctionDefinition(fn)
      case cls : AbstractClassDef => checkAbstractClassDef(cls)
      case cls : CaseClassDef => checkCaseClassDef(cls)
      case df : ParamDef => checkParamDef(df)
      case tt : TypeTree => checkType(tt)
      case _ => reporter.fatal(s"TypeChecker cannot handle tree of type $tree")

  def =:=(actual: Tree, expected: Type)(using Context)=
    if !(actual.tpe =:= expected) then
      reporter.error(
        s"""Found    : ${actual.tpe}
           |Expected : $expected
           |Tree     : $actual
           |""".stripMargin)

  def <:<(actual: Tree, expected: Type)(using Context) =
    if !(actual.tpe <:< expected) then
      reporter.error(
        s"""Found    : ${actual.tpe}
           |Expected : $expected
           |Tree     : $actual
           |""".stripMargin)

  /**
    * Type Check a variable
    * @param v
    * @param env
    * @param Context
    */
  def checkVariable(v: Variable)(using Context) =
    v

  def checkFunRef(f: FunRef)(using Context) =
    f

  def checkIntLiteral(i: IntLiteral)(using Context) =
    i

  def checkBooleanLiteral(b: BooleanLiteral)(using Context) =
    =:=(b, stdType.BooleanType)
    b

  def checkStringLiteral(s: StringLiteral)(using Context) =
    =:=(s, stdType.StringType)
    s

  def checkUnitLiteral(u: UnitLiteral)(using Context) =
    =:=(u, stdType.UnitType)
    u

  def checkBinOp(expr: InfixCall)(tlhs: Type, trhs: Type, rte: Type)(using Context) =
    val InfixCall(lhs, _, rhs) = expr
    =:=(check(lhs), tlhs)
    =:=(check(rhs), trhs)
    =:=(expr, rte)
    expr


  def checkEquals(expr: InfixCall)(using Context) =
    check(expr.lhs)
    val rhs = check(expr.rhs)
    checkBinOp(expr)(rhs.tpe, rhs.tpe, stdType.BooleanType)


  def checkNot(expr: Not)(using Context) =
    val Not(e) = expr
    check(e)
    =:=(e, stdType.BooleanType)
    =:=(expr, stdType.BooleanType)
    expr

  def checkNeg(expr: Neg)(using Context) =
    val Neg(e) = expr
    check(e)
    =:=(e,stdType.IntType)
    =:=(expr, stdType.IntType)
    expr

  def checkCall(expr: Call)(using Context) =
    val Call(qname, args) = expr
    if qname == stdDef.binop_== then
      val lhs = check(args(0))
      val rhs = check(args(1))
      =:=(lhs, rhs.tpe)
      =:=(rhs, rhs.tpe)
      =:=(expr, stdType.BooleanType)
      expr

    else
      args.foreach(check)
      qname match
        case f: FunctionSymbol =>
          args zip f.vparams map ((arg, param) => =:=(arg, param.tpe.tpe))
          =:=(expr, f.rte.tpe)
        case f: ConstructorSymbol =>
          args zip f.vparams map ((arg, pd) => =:=(arg, pd.tpe.tpe))
          =:=(expr, ctx.tpe(f.rte))
      expr

  def checkSequence(seq: Sequence)(using Context) =
    val Sequence(e1, e2) = seq
    check(e1)
    check(e2)
    =:=(seq, e2.tpe)
    seq

  def checkLet(expr: Let)(using Context) =
    val Let(df, value, body) = expr
    check(value)
    <:<(value, df.tpe)
    check(body)
    expr

  def checkIte(expr: Ite)(using Context) =
    val Ite(cond, thenn, elze) = expr
    check(cond)
    =:=(cond, stdType.BooleanType)
    check(thenn)
    check(elze)
    =:=(thenn, elze.tpe)
    expr

  def checkMatch(expr: Match)(using Context) =
    val Match(scrut, cases) = expr
    check(scrut)
    for
      c <- cases
    do
      checkMatchCase(c, scrut.tpe)
      =:=(expr, c.tpe)
    expr

  def checkError(expr: Error)(using Context) =
    val Error(msg) = expr
    check(msg)
    =:=(msg, stdType.StringType)
    expr

  def checkPattern(pat: Pattern, scrut: Type)(using Context): Tree =
    pat match
      case t@WildcardPattern() => checkWildCardPattern(t, scrut)
      case t@IdPattern(_) => checkIdPattern(t, scrut)
      case t@LiteralPattern(_) => checkLiteralPattern(t, scrut)
      case t@CaseClassPattern(_, _) => checkCaseClassPattern(t, scrut)
    pat

  def checkMatchCase(expr: MatchCase, scrut: Type)(using Context) =
    val MatchCase(pat, e) = expr
    checkPattern(pat, scrut)
    check(e)
    expr

  def checkWildCardPattern(expr: WildcardPattern, scrut : Type)(using Context) =
    =:=(expr, WildCardType)
    expr

  def checkIdPattern(expr: IdPattern, scrut : Type)(using Context) =
    =:=(expr, scrut)
    expr

  def checkLiteralPattern[T](expr: LiteralPattern[T], scrut : Type)(using Context) =
    reporter.warning(s"${expr}, $scrut")
    =:=(expr, scrut)
    expr

  def checkCaseClassPattern(expr: CaseClassPattern, scrut: Type)(using Context) =
    val CaseClassPattern(constr, args) = expr
      if ClassType(constr.id) =:= scrut || ClassType(constr.asInstanceOf[ConstructorSymbol].parent.id) =:= scrut then
        // TODO HR : Need to have a symbol as the type not a qualified name
        (args zip constr.asInstanceOf[ConstructorSymbol].vparams) foreach ((p, pd) => checkPattern(p, pd.tpe.tpe))
      else
        reporter.error(s"found $constr instead of $scrut")
    expr

  def checkModule(module: ModuleDef)(using Context) =
    val ModuleDef(_, defs, expr) = module
    for df <- defs do check(df)
    for e <- expr do check(e)
    module

  def checkFunctionDefinition(fn: FunDef)(using Context) =
    if fn.name.asInstanceOf[FunctionSymbol] is "native" then
      fn
    else
      val FunDef(_, _, _, retType, body) = fn
      check(body)
      <:<(body, retType.tpe)
      fn

  def checkAbstractClassDef(cls: AbstractClassDef)(using Context) =
    cls

  def checkCaseClassDef(cls: CaseClassDef)(using Context) =
    cls

  def checkParamDef(df: ParamDef)(using Context) =
    df

  def checkProgram(prog: Program)(using Context) =
    for
      mod <- prog.modules
    do
      check(mod)
    prog

  def checkType(tt : TypeTree)(using Context) =
    tt

}
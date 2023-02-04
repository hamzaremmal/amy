package amyc.typer

import amyc.analyzer.SymbolTable
import amyc.ast.SymbolicTreeModule.*
import amyc.core.{Context, Identifier, StdNames}
import amyc.core.Types.*
import amyc.core.Signatures.*
import amyc.core.StdNames.*
import amyc.core.StdTypes.*
import amyc.core.Symbols.{ConstructorSymbol, FunctionSymbol}
import amyc.{ctx, reporter, symbols}
import amyc.utils.Pipeline

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
      case op@InfixCall(_, StdNames.+ | StdNames.- | StdNames.* | StdNames./ | StdNames.%, _) =>
        checkBinOp(op)(stdType.IntType, stdType.IntType, stdType.IntType)
      case op@InfixCall(_, StdNames.< | StdNames.<=, _) =>
        checkBinOp(op)(stdType.IntType, stdType.IntType, stdType.BooleanType)
      case op@InfixCall(_, StdNames.&& | StdNames.||, _) =>
        checkBinOp(op)(stdType.BooleanType, stdType.BooleanType, stdType.BooleanType)
      case op@InfixCall(_, StdNames.eq_==, _) =>
        checkEquals(op)
      case op@InfixCall(_, StdNames.++, _) =>
        checkBinOp(op)(stdType.StringType, stdType.StringType, stdType.StringType)
      case InfixCall(_, op, _) =>
        reporter.fatal(s"Cannot type check operator $op")
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
    args.foreach(check)
    // In case of a function application
    for
      FunSig(argTypes, retType, _, _) <- symbols.getFunction(FunctionSymbol(qname))
    do
      args zip argTypes map ((arg, tpe) => =:=(arg, tpe.tpe))
      =:=(expr, retType.tpe)
    // In case of a constructor application
    for
      cs@ConstrSig(argTypes, _, _) <- symbols.getConstructor(ConstructorSymbol(qname))
    do
      args zip argTypes map ((arg, tpe) => =:=(arg, tpe.tpe))
      =:=(expr, ctx.tpe(cs.retType))

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
    symbols.getConstructor(ConstructorSymbol(constr)) match
      case Some(ConstrSig(argTypes, parent, _)) =>
        if ClassType(constr) =:= scrut || ClassType(parent.id) =:= scrut then
          // TODO HR : Need to have a symbol as the type not a qualified name
          (args zip argTypes) foreach ((p, t) => checkPattern(p, t.tpe))
        else
          reporter.error(s"found $constr instead of $scrut")
      case None => reporter.error(s"Constructor not found")
    expr

  def checkModule(module: ModuleDef)(using Context) =
    val ModuleDef(_, defs, expr) = module
    for df <- defs do check(df)
    for e <- expr do check(e)
    module

  def checkFunctionDefinition(fn: FunDef)(using Context) =
    val FunDef(_, _, retType, body) = fn
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
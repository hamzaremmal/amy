package amyc.typer

import amyc.analyzer.{ConstrSig, FunSig, SymbolTable}
import amyc.ast.Identifier
import amyc.ast.SymbolicTreeModule.*
import amyc.utils.{Context, Pipeline}

object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)]{

  override def run(v: (Program, SymbolTable))(using Context) =
    (checkProgram(v._1)(using v._2), v._2)

  def check(tree: Tree)(using SymbolTable)(using Context): Tree =
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
  def checkVariable(v: Variable)(using SymbolTable)(using Context) =
    v

  def checkIntLiteral(i: IntLiteral)(using SymbolTable)(using Context) =
    i

  def checkBooleanLiteral(b: BooleanLiteral)(using SymbolTable)(using Context) =
    =:=(b, BooleanType)
    b

  def checkStringLiteral(s: StringLiteral)(using SymbolTable)(using Context) =
    =:=(s, StringType)
    s

  def checkUnitLiteral(u: UnitLiteral)(using SymbolTable)(using Context) =
    =:=(u, UnitType)
    u

  def checkPlusOp(expr: Plus)(using SymbolTable)(using Context) =
    =:=(check(expr.lhs), IntType) // LHS =:= Int
    =:=(check(expr.rhs), IntType) // RHS =:= Int
    =:=(expr, IntType) // (LHS + RHS) =:= Int
    expr
  def checkMinusOp(expr: Minus)(using SymbolTable)(using Context) =
    =:=(check(expr.lhs), IntType) // LHS =:= Int
    =:=(check(expr.rhs), IntType) // RHS =:= Int
    =:=(expr, IntType) // (LHS + RHS) =:= Int
    expr

  def checkTimesOp(expr: Times)(using SymbolTable)(using Context) =
    =:=(check(expr.lhs), IntType) // LHS =:= Int
    =:=(check(expr.rhs), IntType) // RHS =:= Int
    =:=(expr, IntType) // (LHS + RHS) =:= Int
    expr

  def checkDivOp(expr: Div)(using SymbolTable)(using Context)=
    =:=(check(expr.lhs), IntType) // LHS =:= Int
    =:=(check(expr.rhs), IntType) // RHS =:= Int
    =:=(expr, IntType) // (LHS + RHS) =:= Int
    expr

  def checkModOp(expr: Mod)(using SymbolTable)(using Context) =
    =:=(check(expr.lhs), IntType) // LHS =:= Int
    =:=(check(expr.rhs), IntType) // RHS =:= Int
    =:=(expr, IntType) // (LHS + RHS) =:= Int
    expr

  def checkLessThan(expr: LessThan)(using SymbolTable)(using Context) =
    =:=(check(expr.lhs), IntType) // LHS =:= Int
    =:=(check(expr.rhs), IntType) // RHS =:= Int
    =:=(expr, BooleanType) // (LHS + RHS) =:= Boolean
    expr

  def checkLessEquals(expr: LessEquals)(using SymbolTable)(using Context) =
    =:=(check(expr.lhs), IntType) // LHS =:= Int
    =:=(check(expr.rhs), IntType) // RHS =:= Int
    =:=(expr, BooleanType) // (LHS + RHS) =:= Boolean
    expr

  def checkAnd(expr: And)(using SymbolTable)(using Context) =
    =:=(check(expr.lhs), BooleanType) // LHS =:= Boolean
    =:=(check(expr.rhs), BooleanType) // RHS =:= Boolean
    =:=(expr, BooleanType) // (LHS + RHS) =:= Boolean
    expr

  def checkOr(expr: Or)(using SymbolTable)(using Context) =
    =:=(check(expr.lhs), BooleanType) // LHS =:= Boolean
    =:=(check(expr.rhs), BooleanType) // RHS =:= Boolean
    =:=(expr, BooleanType) // (LHS + RHS) =:= Boolean
    expr

  def checkEquals(expr: Equals)(using SymbolTable)(using Context) =
    val lhs = check(expr.lhs)
    val rhs = check(expr.rhs)
    =:=(lhs, rhs.tpe) // LHS =:= RHS
    =:=(expr, BooleanType) // (LHS == RHS) =:= Boolean
    expr

  def checkConcat(expr: Concat)(using SymbolTable)(using Context)=
    =:=(check(expr.lhs), StringType) // LHS =:= String
    =:=(check(expr.rhs), StringType) // RHS =:= String
    =:=(expr, StringType) // (LHS + RHS) =:= String
    expr

  def checkNot(expr: Not)(using SymbolTable)(using Context) =
    val Not(e) = expr
    check(e)
    =:=(e, BooleanType)
    =:=(expr, BooleanType)
    expr

  def checkNeg(expr: Neg)(using SymbolTable)(using Context) =
    val Neg(e) = expr
    check(e)
    =:=(e,IntType)
    =:=(expr, IntType)
    expr

  def checkCall(expr: Call)(using sym : SymbolTable)(using Context) =
    val Call(qname, args) = expr
    args.foreach(check)
    // In case of a function application
    for
      FunSig(argTypes, retType, _) <- sym.getFunction(qname)
    do
      args zip argTypes map ((arg, tpe) => =:=(arg, tpe))
      =:=(expr, retType)
    // In case of a constructor application
    for
      cs@ConstrSig(argTypes, _, _) <- sym.getConstructor(qname)
    do
      args zip argTypes map ((arg, tpe) => =:=(arg, tpe))
      =:=(expr, cs.retType)

    expr

  def checkSequence(seq: Sequence)(using SymbolTable)(using Context) =
    val Sequence(e1, e2) = seq
    check(e1)
    check(e2)
    =:=(seq, e2.tpe)
    seq

  def checkLet(expr: Let)(using SymbolTable)(using Context) =
    val Let(df, value, body) = expr
    check(value)
    <:<(value, df.tpe)
    check(body)
    expr

  def checkIte(expr: Ite)(using SymbolTable)(using Context) =
    val Ite(cond, thenn, elze) = expr
    check(cond)
    =:=(cond, BooleanType)
    check(thenn)
    check(elze)
    =:=(thenn, elze.tpe)
    expr

  def checkMatch(expr: Match)(using SymbolTable)(using Context) =
    val Match(scrut, cases) = expr
    check(scrut)
    for
      c <- cases
    do
      check(c)
      =:=(expr, c.tpe)
    expr

  def checkError(expr: Error)(using SymbolTable)(using Context) =
    val Error(msg) = expr
    check(msg)
    =:=(msg, StringType)
    expr

  def checkMatchCase(expr: MatchCase)(using SymbolTable)(using Context) =
    val MatchCase(pat, e) = expr
    check(pat)
    check(e)
    expr

  def checkWildCardPattern(expr: WildcardPattern)(using SymbolTable)(using Context) =
    =:=(expr, WildCardType)
    expr

  def checkIdPattern(expr: IdPattern)(using SymbolTable)(using Context) =
    expr

  def checkLiteralPattern[T](expr: LiteralPattern[T])(using SymbolTable)(using Context) =
    expr

  def checkCaseClassPattern(expr: CaseClassPattern)(using sym : SymbolTable)(using Context) =
    val CaseClassPattern(constr, args) = expr
    sym.getConstructor(constr) match
      case Some(cs) =>
        (args zip cs.argTypes) foreach ((p, t) => =:=(check(p), t))
      case None => reporter.error(s"Constructor not found")
    expr

  def checkModule(module: ModuleDef)(using SymbolTable)(using Context) =
    val ModuleDef(_, defs, expr) = module
    for df <- defs do check(df)
    for e <- expr do check(e)
    module

  def checkFunctionDefinition(fn: FunDef)(using SymbolTable)(using Context) =
    val FunDef(_, _, retType, body) = fn
    check(body)
    <:<(body, retType.tpe)
    fn

  def checkAbstractClassDef(cls: AbstractClassDef)(using SymbolTable)(using Context) =
    cls

  def checkCaseClassDef(cls: CaseClassDef)(using SymbolTable)(using Context) =
    cls

  def checkParamDef(df: ParamDef)(using SymbolTable)(using Context) =
    df

  def checkProgram(prog: Program)(using SymbolTable)(using Context) =
    for
      mod <- prog.modules
    do
      check(mod)
    prog

  def checkType(tt : TypeTree)(using Context) =
    tt

}
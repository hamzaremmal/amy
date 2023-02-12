package amyc.backend.wasm

import amyc.*
import amyc.ast.*
import amyc.ast.SymbolicTreeModule.{Call as AmyCall, *}
import amyc.core.*
import amyc.core.Symbols.*
import amyc.core.Signatures.*
import amyc.core.StdDefinitions.*
import amyc.utils.Pipeline
import amyc.backend.wasm.*
import amyc.backend.wasm.utils.*
import amyc.backend.wasm.utils.Utils.*
import amyc.backend.wasm.builtin.BuiltIn.*
import amyc.backend.wasm.Instructions.*
import amyc.backend.wasm.builtin.amy.*
import unnamed.null_fn
import amyc.backend.wasm.types.{result, typeuse}
import amyc.core.Symbols.{ConstructorSymbol, FunctionSymbol}

// TODO HR: Generate all wasm related files here
object WASMCodeGenerator extends Pipeline[Program, Module]{

  override val name: String = "WASMCodeGenerator"

  override def run(program: Program)(using Context): Module =
    val fn = wasmFunctions ++ (program.modules flatMap cgModule)
    Module(
      program.modules.last.name.name,
      globalsNo,
      defaultImports,
      cgTable(fn),
      fn
    )

  def cgTable(fn: List[Function])(using Context): Option[Table]=
    if fn.isEmpty then
      None
    else
      val f = resolveOrder(fn.filterNot(_.isMain), null_fn)
      Some(Table(f.size, f))

  // Generate code for an Amy module
  private def cgModule(moduleDef: ModuleDef)(using Context): List[Function] = {
    val ModuleDef(name, defs, optExpr) = moduleDef
    // Generate code for all functions
    defs.collect {
      case fd: FunDef if !builtInFunctions(fullName(name.id, fd.name)) =>
        cgFunction(fd, name.id, false)
    } ++
      // Generate code for the "main" function, which contains the module expression
      optExpr.toList.map { expr =>
        val mainFd = FunDef(FunctionSymbol(Identifier.fresh("main"), name.asInstanceOf, Nil), Nil, ClassTypeTree(stdDef.IntType), expr)
        cgFunction(mainFd, name.id, true)
      }
  }

  // Generate code for a function in module 'owner'
  def cgFunction(fd: FunDef, owner: Identifier, isMain: Boolean)(using Context): Function = {
    // Note: We create the wasm function name from a combination of
    // module and function name, since we put everything in the same wasm module.
    val sig = symbols.getFunction(owner.name, fd.name.name).map(_.signature.idx).getOrElse(0)
    Function(fd, owner, isMain, sig) {
      val body = cgExpr(fd.body)
      withComment(fd.toString) {
        if isMain then
          body <:> drop // Main functions do not return a value,
        // so we need to drop the value generated by their body
        else
          body
      }
    }
  }

  // Generate code for an expression expr.
  // Additional arguments are a mapping from identifiers (parameters and variables) to
  // their index in the wasm local variables, and a LocalsHandler which will generate
  // fresh local slots as required.
  def cgExpr(expr: Expr)(using LocalsHandler, Context): Code = {
    expr match {
      case Variable(name) =>
        local.get(lh(name.id))
      case FunRef(ref : FunctionSymbol) => i32.const(ref.idx)
      case IntLiteral(i) => i32.const(i)
      case BooleanLiteral(b) => mkBoolean(b)
      case StringLiteral(s) => mkString(s)
      case UnitLiteral() => mkUnit
      case InfixCall(_, op, _) =>
        reporter.fatal(s"Cannot generate wasm code for operator, should not appear here $op")
      case Not(e) =>
        cgExpr(e) <:> i32.eqz
      case Neg(e) =>
        mkBinOp(i32.const(0), cgExpr(e))(i32.sub)
      case AmyCall(sym: ConstructorSymbol, args) =>
        genConstructorCall(sym, args)
      case AmyCall(qname, args) =>
        genFunctionCall(args, qname)
      case Sequence(e1, e2) =>
        withComment(e1.toString) {
          cgExpr(e1)
        } <:> drop <:>
          withComment(e2.toString) {
            cgExpr(e2)
          }
      case Let(pdf, value, body) =>
        val idx = lh.getFreshLocal(pdf.name.id)
        withComment(expr.toString) {
          setLocal(cgExpr(value), idx) <:>
            cgExpr(body)
        }
      case Ite(cond, thenn, elze) =>
        ift(cgExpr(cond), cgExpr(thenn), cgExpr(elze))
      case Match(scrut, cases) =>
        val l = lh.getFreshLocal
        setLocal(cgExpr(scrut), l) <:> {
          for
            c <- cases
            cond = matchAndBind(c.pat)
          yield
            local.get(l) <:>
              cond <:>
              `if`(None, Some(result(i32))) <:>
              cgExpr(c.expr) <:>
              `else`()
          // Else here become we are building a big if else bloc.
          // Last bloc will be concatenated with the Match error below and the
          // match error case there
        } <:>
          error(mkString("Match error!" + scrut.toString)) <:>
          cases.map(_ => end) // HR: Autant de End que de cases
      case Error(msg) =>
        error(cgExpr(msg))
      case _ =>
        ctx.reporter.fatal(s"Cannot generate wasm code for $expr", expr.position)
    }
  }

  // ==============================================================================================
  // ==================================== GENERATE APPLICATIONS ===================================
  // ==============================================================================================

  def genFunctionCall(args: List[Expr], qname: Symbol)(using LocalsHandler, Context) =
      if qname == stdDef.binop_+ then
        mkBinOp(cgExpr(args.head), cgExpr(args(1)))(i32.add)
      else if qname == stdDef.binop_- then
        mkBinOp(cgExpr(args(0)), cgExpr(args(1)))(i32.sub)
      else if qname == stdDef.binop_* then
        mkBinOp(cgExpr(args(0)), cgExpr(args(1)))(i32.mul)
      else if qname == stdDef.binop_/ then
        mkBinOp(cgExpr(args(0)), cgExpr(args(1)))(i32.div_s)
      else if qname == stdDef.binop_% then
        mkBinOp(cgExpr(args(0)), cgExpr(args(1)))(i32.rem_s)
      else if qname == stdDef.binop_< then
        mkBinOp(cgExpr(args(0)), cgExpr(args(1)))(i32.lt_s)
      else if qname == stdDef.binop_<= then
        mkBinOp(cgExpr(args(0)), cgExpr(args(1)))(i32.le_s)
      else if qname == stdDef.binop_&& then
        and(cgExpr(args(0)), cgExpr(args(1)))
      else if qname == stdDef.binop_|| then
        or(cgExpr(args(0)), cgExpr(args(1)))
      else if qname == stdDef.binop_== then
        equ(cgExpr(args(0)), cgExpr(args(1)))
      else if qname == stdDef.binop_++ then
        mkBinOp(cgExpr(args(0)), cgExpr(args(1)))(call(id(String.concat.name)))
      else
        args.map(cgExpr) <:> {
        lh(qname.id) match
          case -1 =>
            call(fullName(qname.asInstanceOf[FunctionSymbol].owner.id, qname))
          case idx =>
            local.get(idx) <:>
            call_indirect(typeuse(mkFunTypeName(args.size)))
        }



  def genConstructorCall(sym: ConstructorSymbol, args: List[Expr])(using LocalsHandler, Context) = {
    val ConstrSig(_, _, index) = sym.signature
    val l = lh.getFreshLocal // ref to object, should contain the name

    global.get(memoryBoundary) <:>
    local.set(l) <:>
    adtField(global.get(memoryBoundary), args.size) <:>
    global.set(memoryBoundary) <:>
    local.get(l) <:>
    i32.const(index) <:>
    i32.store <:> {
    // HR: Store each of the constructor parameter
    for
      (arg, idx) <- args.zipWithIndex
    yield
      adtField(local.get(l), idx) <:> // Compute the offset to store in
      cgExpr(arg) <:> // Compute the data to store
      i32.store
    } <:>
    local.get(l)
  }

  // ==============================================================================================
  // =============================== GENERATE PATTERN MATCHING ====================================
  // ==============================================================================================

  // Checks if a value matches a pattern.
  // Assumes value is on top of stack (and CONSUMES it)
  // Returns the code to check the value, and a map of bindings.
  def matchAndBind(pat: Pattern)(using LocalsHandler, Context): Code = pat match {
    case WildcardPattern() => genWildCardPattern
    case IdPattern(id) => genIdPattern(id)
    case LiteralPattern(lit) => genLiteralPattern(lit)
    case CaseClassPattern(constr, args) => genCaseClassPattern(constr, args)
    case _ => ???
  }

  /**
    *
    * @param locals
    * @param lh
    * @param table
    * @param Context
    * @return
    */
  def genWildCardPattern(using LocalsHandler)
                        (using Context) =
  // HR : We return true as this pattern will be executed if encountered
    drop <:> mkBoolean(true)

  /**
    *
    * @param id
    * @param locals
    * @param lh
    * @param table
    * @param Context
    * @return
    */
  def genIdPattern(id: Name)
                  (using LocalsHandler)
                  (using Context) =
    val idLocal = lh.getFreshLocal(id.id)
    // HR : We return true as this pattern will be executed if encountered
    local.set(idLocal) <:> mkBoolean(true)

  /**
    *
    * @param lit
    * @param locals
    * @param lh
    * @param table
    * @param Context
    * @return
    */
  def genLiteralPattern(lit: Literal[_])
                       (using LocalsHandler)
                       (using Context) =
    cgExpr(lit) <:> i32.eq

  /**
    *
    * @param constr
    * @param args
    * @param locals
    * @param lh
    * @param table
    * @param Context
    * @return
    */
  def genCaseClassPattern(constr: QualifiedName, args: List[Pattern])
                         (using LocalsHandler)
                         (using Context) = {
    val idx = lh.getFreshLocal
    val code = args.map(matchAndBind).zipWithIndex.map {
      p => adtField(local.get(idx), p._2) <:> i32.load <:> p._1
    }

    // HR : Setting the constructor index we are checking as a local variable
    local.set(idx) <:>
    ift({
      // HR : First check if the primary constructor is the same
      equ(loadLocal(idx), constructor(constr.asInstanceOf[ConstructorSymbol].signature))
    }, {
      // HR : Check if all the pattern applies
      // HR : if the constructor has no parameters the foldLeft returns true
      code.foldLeft(mkBoolean(true))((acc, c) => and(c._1, acc))
    }, {
      mkBoolean(false) // HR : Signal that the scrut does not follow this pattern
    })
  }


}

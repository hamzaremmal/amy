package amyc
package codegen

import analyzer.*
import amyc.ast.Identifier
import amyc.core.StdNames.*
import amyc.ast.SymbolicTreeModule.{Call as AmyCall, *}
import amyc.utils.Pipeline
import wasm.*
import Instructions.*
import Utils.*
import amyc.core.{Context, StdNames}

import scala.annotation.tailrec

// Generates WebAssembly code for an Amy program
object CodeGen extends Pipeline[Program, Module] {

  override val name = "CodeGen"

  override def run(program: Program)(using Context): Module = {

    Module(
      program.modules.last.name.name,
      defaultImports,
      globalsNo,
      wasmFunctions ++ (program.modules flatMap cgModule)
    )

  }

  // Generate code for an Amy module
  private def cgModule(moduleDef: ModuleDef)(using Context): List[Function] = {
    val ModuleDef(name, defs, optExpr) = moduleDef
    // Generate code for all functions
    defs.collect {
      case fd: FunDef if !builtInFunctions(fullName(name, fd.name)) =>
        cgFunction(fd, name, false)
    } ++
      // Generate code for the "main" function, which contains the module expression
      optExpr.toList.map { expr =>
        val mainFd = FunDef(Identifier.fresh("main"), Nil, TypeTree(IntType), expr)
        cgFunction(mainFd, name, true)
      }
  }

  // Generate code for a function in module 'owner'
  private def cgFunction(fd: FunDef, owner: Identifier, isMain: Boolean)(using Context): Function = {
    // Note: We create the wasm function name from a combination of
    // module and function name, since we put everything in the same wasm module.
    val name = fullName(owner, fd.name)
    Function(name, fd.params.size, isMain) { lh =>
      val locals = fd.paramNames.zipWithIndex.toMap
      val body = cgExpr(fd.body)(using locals, lh)
      val comment = Comment(fd.toString)
      if (isMain) {
        body <:> Drop // Main functions do not return a value,
        // so we need to drop the value generated by their body
      } else {
        comment <:> body
      }
    }
  }

  // Generate code for an expression expr.
  // Additional arguments are a mapping from identifiers (parameters and variables) to
  // their index in the wasm local variables, and a LocalsHandler which will generate
  // fresh local slots as required.
  private def cgExpr(expr: Expr)(using locals: Map[Identifier, Int], lh: LocalsHandler)(using Context): Code = {
    expr match {
      case Variable(name) =>
        // HR : Only use locals here since variables in amy only refer to local variables
        GetLocal(locals(name))
      case IntLiteral(i) => Const(i)
      case BooleanLiteral(b) => //withComment(expr.toString){
        mkBoolean(b)
      //}
      case StringLiteral(s) => mkString(s)
      case UnitLiteral() => mkUnit
      case InfixCall(lhs, StdNames.+, rhs) =>
        mkBinOp(cgExpr(lhs), cgExpr(rhs))(Add)
      case InfixCall(lhs, StdNames.-, rhs) =>
        mkBinOp(cgExpr(lhs), cgExpr(rhs))(Sub)
      case InfixCall(lhs, StdNames.*, rhs) =>
        mkBinOp(cgExpr(lhs), cgExpr(rhs))(Mul)
      case InfixCall(lhs, StdNames./, rhs) =>
        mkBinOp(cgExpr(lhs), cgExpr(rhs))(Div)
      case InfixCall(lhs, StdNames.%, rhs) =>
        mkBinOp(cgExpr(lhs), cgExpr(rhs))(Rem)
      case InfixCall(lhs, StdNames.<, rhs) =>
        mkBinOp(cgExpr(lhs), cgExpr(rhs))(Lt_s)
      case InfixCall(lhs, StdNames.<=, rhs) =>
        mkBinOp(cgExpr(lhs), cgExpr(rhs))(Le_s)
      case InfixCall(lhs, StdNames.&&, rhs) =>
        and(cgExpr(lhs), cgExpr(rhs))
      case InfixCall(lhs, StdNames.||, rhs) =>
        or(cgExpr(lhs), cgExpr(rhs))
      case InfixCall(lhs, StdNames.eq_==, rhs) =>
        equ(cgExpr(lhs), cgExpr(rhs))
      case InfixCall(lhs, StdNames.++, rhs) =>
        mkBinOp(cgExpr(lhs), cgExpr(rhs))(Call(concatImpl.name))
      case Not(e) =>
        cgExpr(e) <:> Eqz
      case Neg(e) =>
        mkBinOp(Const(0), cgExpr(e))(Sub)
      case AmyCall(qname, args) =>
        symbols.getConstructor(qname)
          .map(genConstructorCall(_, args))
          .getOrElse(genFunctionCall(args, qname))
      case Sequence(e1, e2) =>
        withComment(e1.toString) {
          cgExpr(e1)
        } <:> Drop <:>
        withComment(e2.toString) {
          cgExpr(e2)
        }
      case Let(paramDf, value, body) =>
        val idx = lh.getFreshLocal()
        withComment(expr.toString) {
          setLocal(cgExpr(value), idx) <:>
          cgExpr(body)(using locals + (paramDf.name -> idx), lh)
        }
      case Ite(cond, thenn, elze) =>
        ift(cgExpr(cond), cgExpr(thenn), cgExpr(elze))
      case Match(scrut, cases) =>
        val local = lh.getFreshLocal()
        setLocal(cgExpr(scrut), local) <:> {
          for
            c <- cases
            (cond, loc) =  matchAndBind(c.pat)
          yield
            GetLocal(local) <:>
            cond <:>
            If_i32 <:>
            cgExpr(c.expr)(using locals ++ loc, lh) <:>
            Else
          // Else here become we are building a big if else bloc.
          // Last bloc will be concatenated with the Match error below and the
          // match error case there
        } <:>
        error(mkString("Match error!" + scrut.toString)) <:>
        cases.map(_ => End) // HR: Autant de End que de cases
      case Error(msg) =>
        error(cgExpr(msg))
      case _ =>
        ctx.reporter.fatal(s"Cannot generate wasm code for $expr", expr.position)
    }
  }

  // ==============================================================================================
  // ==================================== GENERATE APPLICATIONS ===================================
  // ==============================================================================================

  def genFunctionCall(args: List[Expr], qname: Identifier)
                     (using locals: Map[Identifier, Int], lh: LocalsHandler)
                     (using Context) =
    args.map(cgExpr) <:>
    Call(fullName(symbols.getFunction(qname).get.owner, qname))

  def genConstructorCall(constrSig: ConstrSig, args: List[Expr])
                        (using locals: Map[Identifier, Int], lh: LocalsHandler)
                        (using Context) = {
    val ConstrSig(_, _, index) = constrSig
    val local = lh.getFreshLocal()

    GetGlobal(memoryBoundary) <:>
    SetLocal(local) <:>
    adtField(GetGlobal(memoryBoundary), args.size) <:>
    SetGlobal(memoryBoundary) <:>
    GetLocal(local) <:>
    Const(index) <:>
    Store <:> {
      // HR: Store each of the constructor parameter
      for
        (arg, idx) <- args.zipWithIndex
      yield
        adtField(GetLocal(local), idx) <:> // Compute the offset to store in
        cgExpr(arg) <:> // Compute the data to store
        Store
    } <:>
    GetLocal(local)
  }

  // ==============================================================================================
  // =============================== GENERATE PATTERN MATCHING ====================================
  // ==============================================================================================

  // Checks if a value matches a pattern.
  // Assumes value is on top of stack (and CONSUMES it)
  // Returns the code to check the value, and a map of bindings.
  def matchAndBind(pat: Pattern)
                  (using locals: Map[Identifier, Int], lh: LocalsHandler)
                  (using Context) : (Code, Map[Identifier, Int]) = pat match {
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
  def genWildCardPattern(using locals: Map[Identifier, Int], lh: LocalsHandler)
                        (using Context) =
  // HR : We return true as this pattern will be executed if encountered
    (Drop <:> mkBoolean(true), locals)

  /**
    *
    * @param id
    * @param locals
    * @param lh
    * @param table
    * @param Context
    * @return
    */
  def genIdPattern(id:Name)
                  (using locals: Map[Identifier, Int], lh: LocalsHandler)
                  (using Context) =
    val idLocal = lh.getFreshLocal()
    // HR : We return true as this pattern will be executed if encountered
    (SetLocal(idLocal) <:> mkBoolean(true), Map(id -> idLocal))

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
                       (using locals: Map[Identifier, Int], lh: LocalsHandler)
                       (using Context) =
    (cgExpr(lit) <:> Eq, locals)

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
                         (using locals: Map[Identifier, Int], lh: LocalsHandler)
                         (using Context) = {
    val idx = lh.getFreshLocal()
    val code = args.map(matchAndBind).zipWithIndex.map{
      p => (adtField(GetLocal(idx), p._2) <:> Load <:> p._1._1, p._1._2)
    }
    val lc = code.map(_._2).foldLeft(Map[Identifier, Int]())(_ ++ _)

    // HR : Setting the constructor index we are checking as a local variable
    (
      SetLocal(idx) <:>
      ift({
        // HR : First check if the primary constructor is the same
        equ(loadLocal(idx), constructor(symbols.getConstructor(constr).get))
      }, {
        // HR : Check if all the pattern applies
        // HR : if the constructor has no parameters the foldLeft returns true
        code.foldLeft(mkBoolean(true))((acc, c) => and(c._1, acc))
      }, {
        mkBoolean(false) // HR : Signal that the scrut does not follow this pattern
      }), lc)
  }

}

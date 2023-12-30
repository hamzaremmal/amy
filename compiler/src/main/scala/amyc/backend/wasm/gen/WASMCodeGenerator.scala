package amyc
package backend
package wasm
package gen

import amyc.tools.Pipeline
import ast.{NominalTreeModule as N, SymbolicTreeModule as S}
import ast.SymbolicTreeModule.{Call as AmyCall, *}
import core.*
import core.StdDefinitions.*
import core.StdTypes.*
import core.Symbols.*
import wasm.Instructions.*
import wasm.Modules.*
import wasm.Types.{result, typeuse}
import wasm.builtin.BuiltIn.*
import wasm.builtin.amy.*
import wasm.builtin.amy.Boolean.mkBoolean
import wasm.builtin.amy.Unit.mkUnit
import wasm.handlers.{LocalsHandler, ModuleHandler}
import wasm.utils.*

object WASMCodeGenerator extends Pipeline[Program, Module] :

  override val name: String = "WASMCodeGenerator"
  override def run(program: Program)(using Context): Module =
    given ModuleHandler = ModuleHandler(program.modules.last.name.name)
    val fn = wasmFunctions ++ (program.modules flatMap cgModule)
    Module(
      program.modules.last.name.name,
      Global(mh.static_boundary) :: Nil,
      defaultImports,
      Some(mh.table),
      mh.strpool,
      fn
    )

  // ==============================================================================================
  // ===================================== TRANSFORMER ============================================
  // ==============================================================================================

  /**
    *
    * @param moduleDef
    * @param Context
    * @param ModuleHandler
    * @return
    */
  def cgModule(moduleDef: ModuleDef)(using Context)(using ModuleHandler): List[Function] =
    val ModuleDef(name, defs, optExpr) = moduleDef
    defs.collect {
      case cd@CaseClassDef(sym, params, _) =>
        Function.forDefinition(cd){
          val index = lh.constructor(sym)
          val l = lh.getFreshLocal // ref to object, should contain the name

          // Dynamically allocate space for the object in memory
          lh.mh.dynamic_alloc(params.size) <:>
          local.set(l) <:>
          local.get(l) <:>
          i32.const(index) <:>
          i32.store <:> {
            // HR: Store each of the constructor parameter
            for offset <- (0 to params.size).toList yield
              adtField(local.get(l), offset) <:> // Compute the offset to store in
              local.get(offset) <:> // Fetch the data to store
              i32.store
          } <:>
          local.get(l)
        }
    } ++
    // Generate code for all functions
    defs.collect {
      case fd: FunDef if !(fd.name.asInstanceOf[FunctionSymbol] is "native") =>
        cgFunction(fd, Some(result(i32)))
    } ++
      // Generate code for the "main" function, which contains the module expression
      optExpr.toList.map { expr =>
        val sym = symbols.addFunction(name.asInstanceOf, "main", Nil, Nil, Nil, N.TTypeTree(stdType.IntType))
        val mainFd = FunDef(sym, Nil, Nil, ClassTypeTree(stdDef.IntType), expr)
        cgFunction(mainFd, None)
      }

  /**
    * Generate code for a function in module 'owner'
    * @param fd
    * @param owner
    * @param result
    * @param Context
    * @param ModuleHandler
    * @return
    */
  def cgFunction(fd: FunDef, result : Option[result])(using Context)(using ModuleHandler): Function = {
    // Note: We create the wasm function name from a combination of
    // module and function name, since we put everything in the same wasm module.
    Function.forDefinition(fd, result) {
      val body = cgExpr(fd.body)
      withComment(fd.toString) {
        if result.isEmpty then
          body <:> drop // Main functions do not return a value,
        // so we need to drop the value generated by their body
        else
          body
      }
    }
  }

  /**
    * Generate code for an expression expr.
    * @param expr
    * @param Context
    * @param LocalsHandler
    * @return
    */
  def cgExpr(expr: Expr)(using Context)(using LocalsHandler): Code = {
    expr match {
      case Variable(name) =>
        val idx = lh.fetch(name)
        if idx == -1 then
          reporter.error(s"cannot find $name")
        local.get(idx)
      case FunRef(ref: FunctionSymbol) => i32.const(lh.function(ref))
      case IntLiteral(i) => i32.const(i)
      case BooleanLiteral(b) => mkBoolean(b)
      case StringLiteral(s) =>
        i32.const(lh.mh.string(s))
      case UnitLiteral() => mkUnit
      case InfixCall(_, op, _) =>
        reporter.fatal(s"Cannot generate wasm code for operator, should not appear here $op")
      case Not(e) =>
        cgExpr(e) <:> i32.eqz
      case Neg(e) =>
        i32.const(0) <:>
        cgExpr(e) <:>
        i32.sub
      case AmyCall(sym: ConstructorSymbol, _, args) =>
        args.map(cgExpr) <:>
        call(fullName(sym))
      case AmyCall(qname: FunctionSymbol, _, args) =>
        val defn = stdDef(using ctx)
        qname match
          case defn.binop_&& =>
            and(cgExpr(args.head), cgExpr(args(1)))
          case defn.binop_|| =>
            or(cgExpr(args.head), cgExpr(args(1)))
          case _ =>
            args.map(cgExpr) <:>
            call(fullName(qname))
      case AmyCall(sym: (LocalSymbol | ParameterSymbol), _, args) =>
        args.map(cgExpr) <:>
        local.get(lh.fetch(sym)) <:>
        call_indirect(typeuse(mkFunTypeName(args.size)))
      case Sequence(e1, e2) =>
        cgExpr(e1) <:>
        drop <:>
        cgExpr(e2)
      case Let(pdf, value, body) =>
        val idx = lh.getFreshLocal(pdf.name)
        cgExpr(value) <:>
        local.set(idx) <:>
        cgExpr(body)
      case Ite(cond, thenn, elze) =>
        ift(cgExpr(cond), cgExpr(thenn), cgExpr(elze))
      case Match(scrut, cases) =>
        val l = lh.getFreshLocal

        cgExpr(scrut) <:>
        local.set(l) <:> {
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
          i32.const(lh.mh.string(s"Match error!$scrut")) <:>
          cases.map(_ => end) // HR: Autant de End que de cases
      case Error(msg) =>
        error(cgExpr(msg))
      case _ =>
        reporter.fatal(s"Cannot generate wasm code for $expr", expr.position)
    }
  }

  // ==============================================================================================
  // =============================== GENERATE PATTERN MATCHING ====================================
  // ==============================================================================================

  /**
    * Checks if a value matches a pattern.
    * Assumes value is on top of stack (and CONSUMES it)
    * Returns the code to check the value
    * @param pat
    * @param Context
    * @param LocalsHandler
    * @return
    */
  def matchAndBind(pat: Pattern)(using Context)(using LocalsHandler): Code =
    pat match
      case WildcardPattern() =>
        // HR : We return true as this pattern will be executed if encountered
        drop <:> mkBoolean(true)
      case IdPattern(id) =>
        // HR : We return true as this pattern will be executed if encountered
        val idLocal = lh.getFreshLocal(id)
        local.set(idLocal) <:> mkBoolean(true)
      case LiteralPattern(lit) =>
        cgExpr(lit) <:> i32.eq
      case CaseClassPattern(constr, args) =>
        val idx = lh.getFreshLocal
        val code = args.map(matchAndBind).zipWithIndex.map {
          p => adtField(local.get(idx), p._2) <:> i32.load <:> p._1
        }
        // HR : Setting the constructor index we are checking as a local variable
        local.set(idx) <:>
          ift({
            // HR : First check if the primary constructor is the same
            local.get(idx) <:>
            i32.load <:>
            i32.const(lh.constructor(constr)) <:>
            i32.eq
          }, {
            // HR : Check if all the pattern applies
            // HR : if the constructor has no parameters the foldLeft returns true
            code.foldLeft(mkBoolean(true))((acc, c) => and(c._1, acc))
          }, {
            mkBoolean(false) // HR : Signal that the scrut does not follow this pattern
          })

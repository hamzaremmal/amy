package amyc
package codegen

import analyzer.*
import amyc.ast.Identifier
import amyc.ast.SymbolicTreeModule.{And as AmyAnd, Call as AmyCall, Div as AmyDiv, Or as AmyOr, *}
import amyc.utils.{Context, Pipeline}
import wasm.*
import Instructions.*
import Utils.*

import scala.annotation.tailrec

// Generates WebAssembly code for an Amy program
object CodeGen extends Pipeline[(Program, SymbolTable), Module] {
  def run(ctx: Context)(v: (Program, SymbolTable)): Module = {
    given ctx1 : Context = ctx
    given program : Program = v._1
    given table : SymbolTable = v._2
    
    Module(
      program.modules.last.name.name,
      defaultImports,
      globalsNo,
      wasmFunctions ++ (program.modules flatMap cgModule)
    )

  }

  // Generate code for an Amy module
  private def cgModule(moduleDef: ModuleDef)(using SymbolTable)(using Context): List[Function] = {
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
  private def cgFunction(fd: FunDef, owner: Identifier, isMain: Boolean)(using SymbolTable)(using Context): Function = {
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
  private def cgExpr(expr: Expr)(using locals: Map[Identifier, Int], lh: LocalsHandler, table: SymbolTable)(using ctx: Context): Code = {
    def cgExpr1  :Code =
      expr match {
        case Variable(name) =>
          GetLocal(locals(name)) // HR : Only use locals here since variables in amy only refer to local variables
        case IntLiteral(i) =>
            Const(i)
        case BooleanLiteral(b) =>
            mkBoolean(b)
        case StringLiteral(s) =>
            mkString(s)
        case UnitLiteral() =>
            mkUnit
        case Plus(lhs, rhs) =>
            mkBinOp(cgExpr(lhs), cgExpr(rhs))(Add)
        case Minus(lhs, rhs) =>
            mkBinOp(cgExpr(lhs), cgExpr(rhs))(Sub)
        case Times(lhs, rhs) =>
          mkBinOp(cgExpr(lhs), cgExpr(rhs))(Mul)
        case AmyDiv(lhs, rhs) =>
          mkBinOp(cgExpr(lhs), cgExpr(rhs))(Div)
        case Mod(lhs, rhs) =>
          mkBinOp(cgExpr(lhs), cgExpr(rhs))(Rem)
        case LessThan(lhs, rhs) =>
          mkBinOp(cgExpr(lhs), cgExpr(rhs))(Lt_s)
        case LessEquals(lhs, rhs) =>
          mkBinOp(cgExpr(lhs), cgExpr(rhs))(Le_s)
        case AmyAnd(lhs, rhs) =>
          mkBinOp(cgExpr(lhs), cgExpr(rhs))(And)
        case AmyOr(lhs, rhs) =>
            mkBinOp(cgExpr(lhs), cgExpr(rhs))(Or)
        case Equals(lhs, rhs) =>
            mkBinOp(cgExpr(lhs), cgExpr(rhs))(Eq)
        case Concat(lhs, rhs) =>
            mkBinOp(cgExpr(lhs), cgExpr(rhs))(Call("String_concat"))
        case Not(e) =>
          mkBinOp(cgExpr(e), Const(-1))(Xor)
          Comment("Not implemented") // TODO HR : Implement here
        case Neg(e) =>
          mkBinOp(Const(0), cgExpr(e))(Sub)
        case AmyCall(qname, args) =>
          val argsCode = for v <- args yield cgExpr(v)
          withComment(expr.toString){
            if args.nonEmpty then
              argsCode.reduce(_ <:> _) <:>
                Call(fullName(table.getFunction(qname).get.owner, qname))
            else
              Call(fullName(table.getFunction(qname).get.owner, qname))
          }
        case Sequence(e1, e2) =>
          withComment(e1.toString) {
            cgExpr(e1) <:> Drop
          } <:>
          withComment(e2.toString) {
            cgExpr(e2)
          }
        case Let(paramDf, value, body) =>
          val idx = lh.getFreshLocal()
          withComment(expr.toString){
            cgExpr(value) <:>
            SetLocal(idx) <:>
            cgExpr(body)(using locals + (paramDf.name -> idx), lh)
          }
        case Ite(cond, thenn, elze) =>
          cgExpr(cond) <:>
          If_i32 <:>
          cgExpr(thenn) <:>
          Else <:>
          cgExpr(elze) <:>
          End
        case Match(scrut, cases) =>
          // Checks if a value matches a pattern.
          // Assumes value is on top of stack (and CONSUMES it)
          // Returns the code to check the value, and a map of bindings.
          def matchAndBind(pat: Pattern): (Code, Map[Identifier, Int]) = pat match {
            case IdPattern(id) =>
              val idLocal = lh.getFreshLocal()
              (Comment(pat.toString) <:>
                // Assign val to id.
                SetLocal(idLocal) <:>
                // Return true (IdPattern always matches).
                Const(1),
                // Let the code generation of the expression which corresponds to this pattern
                // know that the bound id is at local idLocal.
                Map(id -> idLocal))

            case _ => ???
          }

          Comment(expr.toString) // TODO HR : Implement here
        case Error(msg) =>
          mkUnaryOp(cgExpr(msg))(Call("Std_error"))
        case _ =>
          ctx.reporter.fatal(s"Cannot generate wasm code for ${expr}", expr.position)
      }

    withComment(expr.toString){
      cgExpr1
    }
  }

}

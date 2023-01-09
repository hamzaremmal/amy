package amyc
package interpreter

import amyc.utils.*
import amyc.core.Signatures.*
import amyc.ast.SymbolicTreeModule.*
import amyc.ast.Identifier
import amyc.analyzer.SymbolTable
import amyc.core.{Context, StdNames}
import amyc.core.StdNames.*
import amyc.interpreter.BuiltIns.*
import amyc.interpreter.*

import scala.collection.mutable

// An interpreter for Amy programs, implemented in Scala
object Interpreter extends Pipeline[Program, Unit] {

  override val name = "Interpreter"

  override def run(program: Program)(using Context): Unit = {
    for {
      m <- program.modules
      e <- m.optExpr
    } do
      interpret(e, program)(Map.empty, ctx)
  }

  def findFunctionOwner(functionName: Identifier)(using Context) =
    symbols.getFunction(functionName).getOrElse{
      reporter.fatal(s"Function $functionName not found")
    }.owner.name

  def findFunction(program: Program, owner: String, name: String) = {
    program.modules.find(_.name.name == owner).get.defs.collectFirst {
      case fd@FunDef(fn, _, _, _) if fn.name == name => fd
    }
  }

  // Interprets a function, using evaluations for local variables contained in 'locals'
  // TODO HR: We will have to remove `program` as a parameter of this function
  def interpret(expr: Expr, program: Program)(implicit locals: Map[Identifier, Value], ctx: Context): Value = {
    expr match {
      case Variable(name) =>
        locals.get(name) match
          case Some(value) => value
          case None =>
            ctx.reporter.fatal(s"variable '$name' is not in scope or defined")
      case FunRef(ref) =>
        val owner = findFunctionOwner(ref)
        builtIns.get(owner, ref.name) map {
          BuiltInFunctionValue
        } orElse  {
          locals.get(ref)
        } getOrElse {
          reporter.fatal("todo")
        }
      case IntLiteral(i) => IntValue(i)
      case BooleanLiteral(b) => BooleanValue(b)
      case StringLiteral(s) => StringValue(s)
      case UnitLiteral() => UnitValue
      case InfixCall(lhs, StdNames.+, rhs) =>
        interpret(lhs, program) + interpret(rhs, program)
      case InfixCall(lhs, StdNames.-, rhs) =>
        interpret(lhs, program) - interpret(rhs, program)
      case InfixCall(lhs, StdNames.*, rhs) =>
        interpret(lhs, program) * interpret(rhs, program)
      case InfixCall(lhs, StdNames./, rhs) =>
        interpret(lhs, program) / interpret(rhs, program)
      case InfixCall(lhs, StdNames.%, rhs) =>
        interpret(lhs, program) % interpret(rhs, program)
      case InfixCall(lhs, StdNames.<, rhs) =>
        interpret(lhs, program) < interpret(rhs, program)
      case InfixCall(lhs, StdNames.<=, rhs) =>
        interpret(lhs, program) <= interpret(rhs, program)
      case InfixCall(lhs, StdNames.&&, rhs) =>
        interpret(lhs, program) && interpret(rhs, program)
      case InfixCall(lhs, StdNames.||, rhs) =>
        interpret(lhs, program) || interpret(rhs, program)
      case InfixCall(lhs, StdNames.eq_==, rhs) =>
        interpret(lhs, program) == interpret(rhs, program)
      case InfixCall(lhs, StdNames.++ ,rhs) =>
        interpret(lhs, program) ++ interpret(rhs, program)
      case InfixCall(_, op, _) =>
        reporter.fatal(s"Cannot interpret operator $op")
      case Not(e) => ! interpret(e, program)
      case Neg(e) => - interpret(e, program)
      case Call(qname, args) =>
        val fn = symbols.getConstructor(qname) orElse {
          locals.get(qname)
        } orElse {
          val owner = findFunctionOwner(qname)
          builtIns.get((owner, qname.name))
        } orElse {
          val owner = findFunctionOwner(qname)
          findFunction(program, owner, qname.name)
        }
        fn match
          case Some(ConstrSig(_, _, _)) =>
            CaseClassValue(qname, args.map(interpret(_, program)))
          case Some(f: BuiltIns.BuiltInFunction) =>
            f(args.map(interpret(_, program)))
          case Some(BuiltInFunctionValue(f)) =>
            f(args.map(interpret(_, program)))
          case Some(FunctionValue(n_args, body)) =>
            val vargs = (n_args zip args.map(interpret(_,program))).toMap
            interpret(body, program)(locals ++ vargs, ctx)
          case Some(f: FunDef) =>
            val arg_val = (f.params.map(_.name) zip args.map(interpret(_, program))).toMap
            val lookup = locals ++ arg_val
            interpret(f.body, program)(using lookup, ctx)
          case _ =>
            reporter.fatal(s"Function  not defined $qname")
      case Sequence(e1, e2) =>
        interpret(e1, program)
        interpret(e2, program)
      case Let(df, value, body) =>
        val df_val = interpret(value, program)
        val locals_n = locals + (df.name -> df_val)
        interpret(body, program)(using locals_n, ctx)
      case Ite(cond, thenn, elze) =>
        if interpret(cond, program).asBoolean then
          interpret(thenn, program)
        else
          interpret(elze, program)
      case Match(scrut, cases) =>
        val evS = interpret(scrut, program)
        def matchesPattern(v: Value, pat: Pattern): Option[List[(Identifier, Value)]] = {
          ((v, pat): @unchecked) match {
            case (_, WildcardPattern()) =>
              Some(Nil)
            case (_, IdPattern(name)) =>
              Some((name -> v) :: Nil)
            case (IntValue(i1), LiteralPattern(IntLiteral(i2))) =>
              if i1 != i2 then None else Some(Nil)
            case (BooleanValue(b1), LiteralPattern(BooleanLiteral(b2))) =>
              if b1 != b2 then None else Some(Nil)
            case (StringValue(_), LiteralPattern(StringLiteral(_))) =>
              None
            case (UnitValue, LiteralPattern(UnitLiteral())) =>
              Some(Nil)
            case (CaseClassValue(con1, realArgs), CaseClassPattern(con2, formalArgs)) =>
              if con1 != con2 then
                None
              else
                val zipped_parameters = realArgs zip formalArgs
                var locals: List[(Identifier, Value)] = Nil
                for match_case <- zipped_parameters do
                  matchesPattern(match_case._1, match_case._2) match
                    case None =>
                      return None
                    case Some(l) =>
                      locals = locals ++ l
                Some(locals)
          }
        }

        // Main "loop" of the implementation: Go through every case,
        // check if the pattern matches, and if so return the evaluation of the case expression
        for {
          MatchCase(pat, rhs) <- cases
          moreLocals <- matchesPattern(evS, pat)
        } {
          return interpret(rhs, program)(locals ++ moreLocals, ctx)
        }
        // No case matched: The program fails with a match error
        ctx.reporter.fatal(s"Match error: ${evS.toString}@${scrut.position}")
      case Error(msg) =>
        ctx.reporter.fatal(interpret(msg, program).asString)
    }
  }

  def loadFunctions(mod: ModuleDef) =
    val fn = for FunDef(name, param, _, body) <- mod.defs yield
        (name, FunctionValue(param.map(_.name), body))
    fn.toMap

}
package amyc
package interpreter

import amyc.utils._
import amyc.ast.SymbolicTreeModule._
import amyc.ast.Identifier
import amyc.analyzer.SymbolTable

// An interpreter for Amy programs, implemented in Scala
object Interpreter extends Pipeline[Program, Unit] {

  // A class that represents a value computed by interpreting an expression
  abstract class Value {
    def asInt: Int = this.asInstanceOf[IntValue].i
    def asBoolean: Boolean = this.asInstanceOf[BooleanValue].b
    def asString: String = this.asInstanceOf[StringValue].s

    override def toString: String = this match {
      case IntValue(i) => i.toString
      case BooleanValue(b) => b.toString
      case StringValue(s) => s
      case UnitValue => "()"
      case CaseClassValue(constructor, args) =>
        constructor.name + "(" + args.map(_.toString).mkString(", ") + ")"
    }
  }
  case class IntValue(i: Int) extends Value
  case class BooleanValue(b: Boolean) extends Value
  case class StringValue(s: String) extends Value
  case object UnitValue extends Value
  case class CaseClassValue(constructor: Identifier, args: List[Value]) extends Value

  def run(program: Program)(using Context): Unit = {

    // Body of the interpreter: Go through every module in order
    // and evaluate its expression if present
    for {
      m <- program.modules
      e <- m.optExpr
    } do interpret(e, program)(Map(), ctx)
  }

  // These built-in functions do not have an Amy implementation in the program,
  // instead their implementation is encoded in this map
  lazy val builtIns: Context ?=> Map[(String, String), List[Value] => Value] = ctx ?=> Map(
    ("Std", "printInt") -> { args => println(args.head.asInt); UnitValue },
    ("Std", "printString") -> { args => println(args.head.asString); UnitValue },
    ("Std", "readString") -> { args => StringValue(scala.io.StdIn.readLine()) },
    ("Std", "readInt") -> { args =>
      val input = scala.io.StdIn.readLine()
      try {
        IntValue(input.toInt)
      } catch {
        case ne: NumberFormatException =>
          ctx.reporter.fatal(s"""Could not parse "$input" to Int""")
      }
    },
    ("Std", "intToString") -> { args => StringValue(args.head.asInt.toString) },
    ("Std", "digitToString") -> { args => StringValue(args.head.asInt.toString) }
  )

  // Utility functions to interface with the symbol table.
  def isConstructor(name: Identifier)(using Context) =
    symbols.getConstructor(name).isDefined

  def findFunctionOwner(functionName: Identifier)(using Context) =
    symbols.getFunction(functionName).get.owner.name

  def findFunction(program: Program, owner: String, name: String) = {
    program.modules.find(_.name.name == owner).get.defs.collectFirst {
      case fd@FunDef(fn, _, _, _) if fn.name == name => fd
    }.get
  }

  // Interprets a function, using evaluations for local variables contained in 'locals'
  // TODO HR: We will have to remove `program` as a parameter of this function
  def interpret(expr: Expr, program: Program)(implicit locals: Map[Identifier, Value], ctx: Context): Value = {
    expr match {
      case Variable(name) =>
        // TODO HR : is Name <: Identifier ?
        locals.get(name) match
          case Some(value) => value
          case None =>
            ctx.reporter.fatal(s"variable '$name' is not in scope or defined")
      case IntLiteral(i) => IntValue(i)
      case BooleanLiteral(b) => BooleanValue(b)
      case StringLiteral(s) => StringValue(s)
      case UnitLiteral() => UnitValue
      case Plus(lhs, rhs) =>
        // HR : No special case to handle here
        IntValue(interpret(lhs, program).asInt + interpret(rhs, program).asInt)
      case Minus(lhs, rhs) =>
        // HR : No special case to handle here
        IntValue(interpret(lhs, program).asInt - interpret(rhs, program).asInt)
      case Times(lhs, rhs) =>
        // HR : No special case to handle here
        IntValue(interpret(lhs, program).asInt * interpret(rhs, program).asInt)
      case Div(lhs, rhs) =>
        // HR : Interpreter should handle the case of the division by 0
        val v_lhs = interpret(lhs, program).asInt
        val v_rhs = interpret(rhs, program).asInt
        if v_rhs != 0 then
          IntValue(v_lhs / v_rhs)
        else
          ctx.reporter.fatal("Cannot divide by 0") // TODO HR : Fix message here
      case Mod(lhs, rhs) =>
        // HR : Interpreter should handle the case of the modulo by 0
        // TODO
        IntValue(interpret(lhs, program).asInt % interpret(rhs, program).asInt)
      case LessThan(lhs, rhs) =>
        // HR : No special case to handle here
        BooleanValue(interpret(lhs, program).asInt < interpret(rhs, program).asInt)
      case LessEquals(lhs, rhs) =>
        // HR : No special case to handle here
        BooleanValue(interpret(lhs, program).asInt <= interpret(rhs, program).asInt)
      case And(lhs, rhs) =>
        // TODO HR : Is there any bitwise operations in Amy ?
        BooleanValue(interpret(lhs, program).asBoolean && interpret(rhs, program).asBoolean)
      case Or(lhs, rhs) =>
        // TODO HR : Is there any bitwise operations in Amy ?
        BooleanValue(interpret(lhs, program).asBoolean || interpret(rhs, program).asBoolean)
      case Equals(lhs, rhs) =>
        // Hint: Take care to implement Amy equality semantics
        // TODO
        val v_lhs = interpret(lhs, program)
        val v_rhs = interpret(rhs, program)
        (v_lhs, v_rhs) match
          // TODO HR : Handle if the effective type is not the same in lhs and rhs
          // TODO HR : by doing (_, ...) and (..., _) in some of the match cases
          case (StringValue(_), StringValue(_)) => BooleanValue(v_lhs eq v_rhs)
          case (CaseClassValue(_, _), CaseClassValue(_, _)) => BooleanValue(v_lhs eq v_rhs)
          case (IntValue(a), IntValue(b)) => BooleanValue(a == b)
          case (BooleanValue(a), BooleanValue(b)) => BooleanValue(a == b)
          case (_, _) => BooleanValue(v_lhs eq v_rhs)
      case Concat(lhs, rhs) =>
        // HR : No special case to handle here
        StringValue(interpret(lhs, program).toString ++ interpret(rhs, program).toString)
      case Not(e) =>
        // HR : No special case to handle here
        BooleanValue(!interpret(e, program).asBoolean)
      case Neg(e) =>
        // HR : No special case to handle here
        IntValue(-interpret(e, program).asInt)
      case Call(qname, args) =>
        if isConstructor(qname) then
          val arg_values = args.map(interpret(_, program))
          CaseClassValue(qname, arg_values)
        else
          val owner = findFunctionOwner(qname)
          builtIns.get((owner, qname.name)) match
            case Some(f) =>
              val arg_values = args.map(interpret(_, program))
              f(arg_values)
            case None =>
              // Find function to execute
              val f = findFunction(program, owner, qname.name);
              // Build new arguments
              val arg_val = (f.params.map(_.name) zip args.map(interpret(_, program))).toMap
              val lookup = locals ++ arg_val
              interpret(f.body, program)(using lookup, ctx)
      // Hint: Check if it is a call to a constructor first,
      //       then if it is a built-in function (otherwise it is a normal function).
      //       Use the helper methods provided above to retrieve information from the symbol table.
      //       Think how locals should be modified.
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
        // Hint: We give you a skeleton to implement pattern matching
        //       and the main body of the implementation
        val evS = interpret(scrut, program)
        // Returns a list of pairs id -> value,
        // where id has been bound to value within the pattern.
        // Returns None when the pattern fails to match.
        // Note: Only works on well typed patterns (which have been ensured by the type checker).
        def matchesPattern(v: Value, pat: Pattern): Option[List[(Identifier, Value)]] = {
          ((v, pat): @unchecked) match {
            case (_, WildcardPattern()) =>
              // HR : No need to add a new local since the pattern is _
              // HR : If you return None, the pattern matching won't work and rhs will not be executed
              Some(Nil)
            case (_, IdPattern(name)) =>
              // HR : You only need to create a new local since
              // HR : If you return None, the pattern matching won't work and rhs will not be executed
              Some((name -> v) :: Nil)
            case (IntValue(i1), LiteralPattern(IntLiteral(i2))) =>
              // HR : Check if the value is correct
              // HR : If the value is correct, you return Some otherwise None
              // HR : If you return None, the pattern matching won't work and rhs will not be executed
              // HR : This means that both integers are not the same
              if i1 != i2 then None else Some(Nil)
            case (BooleanValue(b1), LiteralPattern(BooleanLiteral(b2))) =>
              // HR : Check if the value is correct
              // HR : If the value is correct, you return Some otherwise None
              // HR : If you return None, the pattern matching won't work and rhs will not be executed
              // HR : This means that both booleans are not the same
              if b1 != b2 then None else Some(Nil)
            //case (StringValue(_), LiteralPattern(StringLiteral(_))) => Commented by HR TODO : Ask TA about it
            case (StringValue(s1), LiteralPattern(StringLiteral(s2))) =>
              // HR : Check if the value is correct
              // HR : If the value is correct, you return Some otherwise None
              // HR : If you return None, the pattern matching won't work and rhs will not be executed
              // HR : This means that both Strings are not the same
              // if s1 == s2 then Some(Nil) else None TODO : Ask TA about it, Amy Specification page 12
              None
            case (UnitValue, LiteralPattern(UnitLiteral())) =>
              // HR : Check if the value is correct
              // HR : If the value is correct, you return Some otherwise None
              // HR : If you return None, the pattern matching won't work and rhs will not be executed
              // HR : This means that both value are not the Unit object
              Some(Nil)
            case (CaseClassValue(con1, realArgs), CaseClassPattern(con2, formalArgs)) =>
              // HR : First you need to check if the class are the same
              // HR : If it's the case, you proceed, otherwise return None so that rhs will not be executed
              if con1 != con2 then
                None
              else
                // HR : Now that we've established that the classes are the same
                // HR : We need to check the parameters and check with the pattern
                val zipped_parameters = realArgs zip formalArgs
                // HR : We will check each parameter with its pattern in a loop
                // HR : If a match returns None, we'll stop and return None
                // HR : This means that the value doesn't have the pattern. Therefore rhs will not be evaluated
                // HR : We will also need to store the new locals in a list
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

}
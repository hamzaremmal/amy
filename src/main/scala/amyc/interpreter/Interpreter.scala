package amyc
package interpreter

import amyc.utils._
import amyc.ast.SymbolicTreeModule._
import amyc.ast.Identifier
import amyc.analyzer.SymbolTable

// An interpreter for Amy programs, implemented in Scala
object Interpreter extends Pipeline[(Program, SymbolTable), Unit] {

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

  def run(ctx: Context)(v: (Program, SymbolTable)): Unit = {
    val (program, table) = v

    // These built-in functions do not have an Amy implementation in the program,
    // instead their implementation is encoded in this map
    val builtIns: Map[(String, String), (List[Value]) => Value] = Map(
      ("Std", "printInt")    -> { args => println(args.head.asInt); UnitValue },
      ("Std", "printString") -> { args => println(args.head.asString); UnitValue },
      ("Std", "readString")  -> { args => StringValue(scala.io.StdIn.readLine()) },
      ("Std", "readInt")     -> { args =>
        val input = scala.io.StdIn.readLine()
        try {
          IntValue(input.toInt)
        } catch {
          case ne: NumberFormatException =>
            ctx.reporter.fatal(s"""Could not parse "$input" to Int""")
        }
      },
      ("Std", "intToString")   -> { args => StringValue(args.head.asInt.toString) },
      ("Std", "digitToString") -> { args => StringValue(args.head.asInt.toString) }
    )

    // Utility functions to interface with the symbol table.
    def isConstructor(name: Identifier) = table.getConstructor(name).isDefined
    def findFunctionOwner(functionName: Identifier) = table.getFunction(functionName).get.owner.name
    def findFunction(owner: String, name: String) = {
      program.modules.find(_.name.name == owner).get.defs.collectFirst {
        case fd@FunDef(fn, _, _, _) if fn.name == name => fd
      }.get
    }

    // Interprets a function, using evaluations for local variables contained in 'locals'
    def interpret(expr: Expr)(implicit locals: Map[Identifier, Value]): Value = {
      expr match {
        case Variable(name) =>
          // TODO : is Name <: Identifier ?
          locals.get(name) match
            case Some(value) => value
            case None =>
              ctx.reporter.fatal(s"variable '$name' is not in scope or defined")
        case IntLiteral(i) => IntValue(i)
        case BooleanLiteral(b) => BooleanValue(b)
        case StringLiteral(s) => StringValue(s)
        case UnitLiteral() => UnitValue
        case Plus(lhs, rhs) => IntValue(interpret(lhs).asInt + interpret(rhs).asInt)
        case Minus(lhs, rhs) => IntValue(interpret(lhs).asInt - interpret(rhs).asInt)
        case Times(lhs, rhs) => IntValue(interpret(lhs).asInt * interpret(rhs).asInt)
        case Div(lhs, rhs) => IntValue(interpret(lhs).asInt / interpret(rhs).asInt)
        case Mod(lhs, rhs) => IntValue(interpret(lhs).asInt % interpret(rhs).asInt)
        case LessThan(lhs, rhs) => BooleanValue(interpret(lhs).asInt < interpret(rhs).asInt)
        case LessEquals(lhs, rhs) => BooleanValue(interpret(lhs).asInt <= interpret(rhs).asInt)
        case And(lhs, rhs) => IntValue(interpret(lhs).asInt & interpret(rhs).asInt)
        case Or(lhs, rhs) => BooleanValue(interpret(lhs).asBoolean | interpret(rhs).asBoolean)
        case Equals(lhs, rhs) =>
          // Hint: Take care to implement Amy equality semantics
          BooleanValue(interpret(lhs) == interpret(rhs))
        case Concat(lhs, rhs) => StringValue(interpret(lhs).toString ++ interpret(rhs).toString)
        case Not(e) => BooleanValue(! interpret(e).asBoolean)
        case Neg(e) => IntValue(-interpret(e).asInt)
        case Call(qname, args) =>
          if isConstructor(qname) then
            val arg_values = args.map(interpret(_))
            CaseClassValue(qname, arg_values)
          else
            val owner = findFunctionOwner(qname)
            builtIns.get((owner, qname.name)) match
              case Some(f) =>
                val arg_values = args.map(interpret(_))
                f(arg_values)
              case None =>
                // Find function to execute
                val f = findFunction(owner, qname.name);
                // Build new arguments
                val arg_val = (f.params.map(_.name) zip args.map(interpret(_))).toMap
                val lookup = locals ++ arg_val
                interpret(f.body)(using lookup)
        // Hint: Check if it is a call to a constructor first,
        //       then if it is a built-in function (otherwise it is a normal function).
        //       Use the helper methods provided above to retrieve information from the symbol table.
        //       Think how locals should be modified.
        case Sequence(e1, e2) =>
          interpret(e1)
          interpret(e2)
        case Let(df, value, body) =>
          val df_val = interpret(value)
          val locals_n = locals + (df.name ->  df_val)
          interpret(body)(using locals_n)
        case Ite(cond, thenn, elze) => if interpret(cond).asBoolean then interpret(thenn) else interpret(elze)
        case Match(scrut, cases) =>
          // Hint: We give you a skeleton to implement pattern matching
          //       and the main body of the implementation

          val evS = interpret(scrut)

          // Returns a list of pairs id -> value,
          // where id has been bound to value within the pattern.
          // Returns None when the pattern fails to match.
          // Note: Only works on well typed patterns (which have been ensured by the type checker).
          def matchesPattern(v: Value, pat: Pattern): Option[List[(Identifier, Value)]] = {
            println(v -> pat)
            Console.flush()
            ((v, pat): @unchecked) match {
              case (_, WildcardPattern()) =>
                println("1")
                ???
              case (_, IdPattern(name)) =>
                println("2")
                ???
              case (IntValue(i1), LiteralPattern(IntLiteral(i2))) =>

                println("3")
                ???
              case (BooleanValue(b1), LiteralPattern(BooleanLiteral(b2))) =>
                println("4")
                ???
              case (StringValue(_), LiteralPattern(StringLiteral(_))) =>
                println("5")
                ???
              case (UnitValue, LiteralPattern(UnitLiteral())) =>
                println("6")
                ???
              case (CaseClassValue(con1, realArgs), CaseClassPattern(con2, formalArgs)) =>
                println(s"con1 = $con1 , realArgs = $realArgs")
                println(s"con2 = $con2 , formalArgs = $formalArgs")
                Some((formalArgs zip realArgs))
                println("7")
                ???
            }
          }

          // Main "loop" of the implementation: Go through every case,
          // check if the pattern matches, and if so return the evaluation of the case expression
          for {
            MatchCase(pat, rhs) <- cases
            moreLocals <- matchesPattern(evS, pat)
          } {
            return interpret(rhs)(locals ++ moreLocals)
          }
          // No case matched: The program fails with a match error
          ctx.reporter.fatal(s"Match error: ${evS.toString}@${scrut.position}")


        case Error(msg) =>
          ctx.reporter.fatal(interpret(msg).asString)
      }
    }

    // Body of the interpreter: Go through every module in order
    // and evaluate its expression if present
    for {
      m <- program.modules
      e <- m.optExpr
    } {
      interpret(e)(Map())
    }
  }
}
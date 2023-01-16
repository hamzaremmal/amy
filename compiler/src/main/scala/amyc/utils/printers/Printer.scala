package amyc.utils.printers

import amyc.ast.*
import amyc.utils.*

// A printer for Amy trees
trait Printer {

  val treeModule: TreeModule
  import treeModule.*

  implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document
  implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document

  protected implicit def stringToDoc(s: String): Raw = Raw(s)

  def apply(t: Tree)(implicit printUniqueIDs: Boolean = false): String = {

    def binOp(e1: Expr, op: String, e2: Expr) = "(" <:> rec(e1) <:> " " + op + " " <:> rec(e2) <:> ")"

    def rec(t: Tree, parens: Boolean = true): Document = t match {
      /* Definitions */
      case Program(modules) =>
        Stacked(modules map (rec(_)), emptyLines = true)

      case ModuleDef(name, defs, optExpr) =>
        Stacked(
          "object " <:> name,
          "",
          Indented(Stacked(defs ++ optExpr.toList map (rec(_, false)), emptyLines = true)),
          "end " <:> name,
          ""
        )

      case AbstractClassDef(name) =>
        "abstract class " <:> printName(name)

      case CaseClassDef(name, fields, parent) =>
        def printField(f: TypeTree) = "v: " <:> rec(f)
        "case class " <:> name <:> "(" <:> Lined(fields map printField, ", ") <:> ") extends " <:> parent

      case FunDef(name, params, retType, body) =>
        Stacked(
          "fn " <:> name <:> "(" <:> Lined(params map (rec(_)), ", ") <:> "): " <:> rec(retType) <:> " = {",
          Indented(rec(body, false)),
          "}"
        )

      case ParamDef(name, tpe) =>
        name <:> ": " <:> rec(tpe)

      /* Expressions */
      case Variable(name) =>
        name
      case IntLiteral(value) =>
        value.toString
      case BooleanLiteral(value) =>
        value.toString
      case StringLiteral(value) =>
        "\"" + value + '"'
      case UnitLiteral() =>
        "()"
      case InfixCall(lhs, op, rhs) =>
        binOp(lhs, op.toString, rhs)
      case Not(e) =>
        "!(" <:> rec(e) <:> ")"
      case Neg(e) =>
        "-(" <:> rec(e) <:> ")"
      case Call(name, args) =>
        name <:> "(" <:> Lined(args map (rec(_)), ", ") <:> ")"
      case Sequence(lhs, rhs) =>
        val main = Stacked(
          rec(lhs, false) <:> ";",
          rec(rhs, false),
        )
        if (parens) {
          Stacked(
            "(",
            Indented(main),
            ")"
          )
        } else {
          main
        }
      case Let(df, value, body) =>
        val main = Stacked(
          "val " <:> rec(df) <:> " =",
          Indented(rec(value)) <:> ";",
          rec(body, false) // For demonstration purposes, the scope or df is indented
        )
        if (parens) {
          Stacked(
            "(",
            Indented(main),
            ")"
          )
        } else {
          main
        }
      case Ite(cond, thenn, elze) =>
        Stacked(
          "(if(" <:> rec(cond) <:> ") {",
          Indented(rec(thenn)),
          "} else {",
          Indented(rec(elze)),
          "})"
        )
      case Match(scrut, cases) =>
        Stacked(
          rec(scrut) <:> " match {",
          Indented(Stacked(cases map (rec(_)))),
          "}"
        )
      case Error(msg) =>
        "error(" <:> rec(msg) <:> ")"

      /* cases and patterns */
      case MatchCase(pat, expr) =>
        Stacked(
          "case " <:> rec(pat) <:> " =>",
          Indented(rec(expr))
        )
      case WildcardPattern() =>
        "_"
      case IdPattern(name) =>
        name
      case LiteralPattern(lit) =>
        rec(lit)
      case CaseClassPattern(name, args) =>
        name <:> "(" <:> Lined(args map (rec(_)), ", ") <:> ")"

      /* Types */
      case TypeTree(tp) =>
        tp match {
          case IntType => "Int"
          case BooleanType => "Boolean"
          case StringType => "String"
          case UnitType => "Unit"
          case ClassType(name) => name
          case FunctionType(args, rte) => s"(${args.map(apply).mkString(", ")}) => ${apply(rte)}"
        }

    }

    rec(t).print
  }
}

object NominalPrinter extends Printer {
  import amyc.ast.NominalTreeModule.*
  val treeModule: NominalTreeModule.type = NominalTreeModule

  implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document = Raw(name)

  implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document = {
    Raw(name match {
      case QualifiedName(Some(module), name) =>
        s"$module.$name"
      case QualifiedName(None, name) =>
        name
    })
  }
}

object SymbolicPrinter extends SymbolicPrinter

trait SymbolicPrinter extends Printer {

  import amyc.ast.SymbolicTreeModule.*

  val treeModule: SymbolicTreeModule.type = SymbolicTreeModule

  implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document = {
    if (printUniqueIds) {
      name.fullName
    } else {
      name.name
    }
  }

  @inline implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document = {
    printName(name)
  }
}


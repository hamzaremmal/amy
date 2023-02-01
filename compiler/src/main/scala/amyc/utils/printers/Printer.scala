package amyc.utils.printers

import amyc.ast.*
import amyc.utils.{Document, Indented, Lined, Stacked}
import amyc.utils.Document.*
import amyc.utils.printers.highlight.{Highlighter, NoHighlight}
import amyc.parsing.keywords.*

// A printer for Amy trees
trait Printer(highlighter: Highlighter) {
  import highlighter.*

  val treeModule: TreeModule
  import treeModule.*

  implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document
  implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document

  def apply(t: Tree)(implicit printUniqueIDs: Boolean = false): String = {
    def rec(t: Tree, parens: Boolean = true): Document = t match {
      /* Definitions */
      case Program(modules) =>
        Stacked(modules map (rec(_)), emptyLines = true)

      case ModuleDef(name, defs, optExpr) =>
        Stacked(
          doc"$module $name",
          "",
          iden"${Stacked(defs ++ optExpr.toList map (rec(_, false)), emptyLines = true)}",
          doc"$end $name",
          ""
        )

      case AbstractClassDef(name) =>
        doc"${`abstract`} ${`class`} ${printName(name)}"

      case CaseClassDef(name, fields, parent) =>
        def printField(f: TypeTree) = doc"v: ${rec(f)}"
        doc"${`case`} ${`class`} $name(${fields.map(printField).mkDoc(", ")}) : $parent"

      case FunDef(name, params, retType, body) =>
        Stacked(
          doc"$fn $name(${params.map(rec(_)).mkDoc(", ")}): ${rec(retType)} = {",
          iden"${rec(body, false)}",
          "}"
        )
      case ParamDef(name, tpe) =>
        doc"$name: ${rec(tpe)}"
      case Variable(name) =>
        name
      case IntLiteral(value) =>
        value.toString
      case BooleanLiteral(value) =>
        value.toString
      case StringLiteral(value) =>
        s"\"$value\""
      case UnitLiteral() =>
        "()"
      case InfixCall(lhs, op, rhs) =>
        doc"(${rec(lhs)} ${op.toString} ${rec(rhs)})"
      case Not(e) =>
        doc"!(${rec(e)})"
      case Neg(e) =>
        doc"-(${rec(e)})"
      case Call(name, args) =>
        doc"$name(${args.map(rec(_)).mkDoc(", ")})"
      case Sequence(lhs, rhs) =>
        val main = Stacked(
          doc"${rec(lhs, false)};",
          rec(rhs, false),
        )
        if (parens) {
          Stacked(
            "(",
            iden"${main}",
            ")"
          )
        } else {
          main
        }
      case Let(df, value, body) =>
        val main = Stacked(
          doc"${`val`} ${rec(df)} =",
          iden"${rec(value)};",
          rec(body, false) // For demonstration purposes, the scope or df is indented
        )
        if (parens) {
          Stacked(
            "(",
            iden"${main}",
            ")"
          )
        } else {
          main
        }
      case Ite(cond, thenn, elze) =>
        Stacked(
          doc"(${`if`}(${rec(cond)}) {",
          iden"${rec(thenn)}",
          doc"} ${`else`} {",
          iden"${rec(elze)}",
          "})"
        )
      case Match(scrut, cases) =>
        Stacked(
          doc"${rec(scrut)} ${`match`}{",
          iden"${Stacked(cases map (rec(_)))}",
          "}"
        )
      case Error(msg) =>
        doc"$error(${rec(msg)})"
      /* cases and patterns */
      case MatchCase(pat, expr) =>
        Stacked(
          doc"${`case`} ${rec(pat)} =>",
          iden"${rec(expr)}"
        )
      case WildcardPattern() =>
        "_"
      case IdPattern(name) =>
        name
      case LiteralPattern(lit) =>
        rec(lit)
      case CaseClassPattern(name, args) =>
        doc"$name(${args.map(rec(_)).mkDoc(", ")})"
      /* Types */
      case FunctionTypeTree(args, rte) =>
        s"(${args.map(apply).mkString(", ")}) => ${apply(rte)}"
      case ClassTypeTree(name) => name
    }

    rec(t).print
  }
}

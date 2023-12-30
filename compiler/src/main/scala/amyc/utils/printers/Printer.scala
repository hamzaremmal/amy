package amyc.utils.printers

import amyc.ast.*
import amyc.utils.{Stacked, *}
import amyc.utils.printers.highlight.Highlighter

// A printer for Amy trees
trait Printer(highlighter: Highlighter) {
  import highlighter.*

  val treeModule: TreeModule
  import treeModule.*

  implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document
  implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document

  def printCall(c : Call)(implicit printUniqueIDs: Boolean) : Document

  protected implicit def stringToDoc(s: String): Raw = Raw(s)

  def toDoc(t: Tree, parens: Boolean = true)(implicit printUniqueIDs: Boolean = false): Document =
    def binOp(e1: Expr, op: String, e2: Expr) = "(" <:> toDoc(e1) <:> " " + op + " " <:> toDoc(e2) <:> ")"
    t match {
    /* Definitions */
    case Program(modules) =>
      Stacked(modules map (toDoc(_)), emptyLines = true)

    case ModuleDef(name, defs, optExpr) =>
      Stacked(
        Lined(List(highlightKeyword("module"), name), " "),
        "",
        Indented(Stacked(defs ++ optExpr.toList map (toDoc(_, false)), emptyLines = true)),
        Lined(List(highlightKeyword("end"), name), " "),
        ""
      )

    case AbstractClassDef(name) =>
      Lined(List(highlightKeyword("abstract"), highlightKeyword("class"), printName(name)), " ")

    case CaseClassDef(name, fields, parent) =>
      def printField(f: TypeTree) = "v: " <:> toDoc(f)

      "case class " <:> name <:> "(" <:> Lined(fields.map(_.tt) map printField, ", ") <:> ") : " <:> parent

    case FunDef(name, tparams, vparams, retType, body) =>
      Stacked(
        "fn " <:> name <:>
          "[" <:> Lined(tparams.map(toDoc(_)), ", ") <:> "]" <:>
          "(" <:> Lined(vparams.map(toDoc(_)), ", ") <:> ")" <:>
          ": " <:> toDoc(retType) <:> " = {",
        Indented(toDoc(body, false)),
        "}"
      )

    case ValParamDef(name, tpe) =>
      name <:> ": " <:> toDoc(tpe)

    /* Expressions */
    case Variable(name) =>
      name
    case IntLiteral(value) =>
      highlightLiteral(value.toString)
    case BooleanLiteral(value) =>
      highlightLiteral(value.toString)
    case StringLiteral(value) =>
      highlightLiteral(s"\"$value\"")
    case UnitLiteral() =>
      highlightLiteral("()")
    case InfixCall(lhs, op, rhs) =>
      binOp(lhs, op.toString, rhs)
    case Not(e) =>
      "!(" <:> toDoc(e) <:> ")"
    case Neg(e) =>
      "-(" <:> toDoc(e) <:> ")"
    case c@Call(name, _, args) =>
      printCall(c)
    case Sequence(lhs, rhs) =>
      val main = Stacked(
        toDoc(lhs, false) <:> ";",
        toDoc(rhs, false),
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
        "val " <:> toDoc(df) <:> " =",
        Indented(toDoc(value)) <:> ";",
        toDoc(body, false) // For demonstration purposes, the scope or df is indented
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
        "(if(" <:> toDoc(cond) <:> ") {",
        Indented(toDoc(thenn)),
        "} else {",
        Indented(toDoc(elze)),
        "})"
      )
    case Match(scrut, cases) =>
      Stacked(
        toDoc(scrut) <:> " match {",
        Indented(Stacked(cases map (toDoc(_)))),
        "}"
      )
    case Error(msg) =>
      "error(" <:> toDoc(msg) <:> ")"

    /* cases and patterns */
    case MatchCase(pat, expr) =>
      Stacked(
        "case " <:> toDoc(pat) <:> " =>",
        Indented(toDoc(expr))
      )
    case WildcardPattern() =>
      "_"
    case IdPattern(name) =>
      name
    case LiteralPattern(lit) =>
      toDoc(lit)
    case CaseClassPattern(name, args) =>
      name <:> "(" <:> Lined(args map (toDoc(_)), ", ") <:> ")"

    /* Types */
    case FunctionTypeTree(args, rte) =>
      s"(${args.map(apply).mkString(", ")}) => ${apply(rte)}"
    case ClassTypeTree(name) => name
    case TTypeTree(tpe) => tpe.toString
    case TypeParamDef(name) => name
    case FunRef(_) => ""
    case  EmptyExpr() => "<empty>"
  }

  def apply(t: Tree)(implicit printUniqueIDs: Boolean = false): String = {
    toDoc(t).print
  }
}

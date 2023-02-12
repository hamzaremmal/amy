package amyc.utils.printers

import amyc.ast.SymbolicTreeModule
import amyc.core.Symbols.FunctionSymbol
import amyc.utils.{Document, Lined}
import amyc.utils.printers.SymbolicPrinter.treeModule
import amyc.utils.printers.highlight.NoHighlight

object SymbolicPrinter extends Printer(NoHighlight) :

  import amyc.ast.SymbolicTreeModule.*

  val treeModule: SymbolicTreeModule.type = SymbolicTreeModule
  import treeModule.*

  implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document = {
    if (printUniqueIds) {
      name.fullName
    } else {
      name.name
    }
  }

  inline implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document =
    printName(name)

  override def printCall(c: Call)(implicit printUniqueIDs: Boolean): Document =
    val Call(name, args) = c
    name match
      case f: FunctionSymbol if f is "infix" =>
        "(" <:> toDoc(args(0)) <:> " " <:> printName(name)(false) <:> " " <:> toDoc(args(1)) <:> ")"
      case _ =>
        printQName(name) <:> "(" <:> Lined(args map (toDoc(_)), ", ") <:> ")"

package amyc.utils.printers

import amyc.ast.SymbolicTreeModule
import amyc.utils.Document
import amyc.utils.printers.highlight.NoHighlight

object SymbolicPrinter extends Printer(NoHighlight) :

  import amyc.ast.SymbolicTreeModule.*

  val treeModule: SymbolicTreeModule.type = SymbolicTreeModule

  implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document = {
    if (printUniqueIds) {
      name.fullName
    } else {
      name.name
    }
  }

  inline implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document =
    printName(name)

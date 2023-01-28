package amyc.utils.printers

import amyc.ast.NominalTreeModule
import amyc.utils.{Document, Raw}
import amyc.utils.printers.highlight.NoHighlight


object NominalPrinter extends Printer(NoHighlight) :
  import amyc.ast.NominalTreeModule.*
  val treeModule: NominalTreeModule.type = NominalTreeModule

  implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document = Raw(name)

  implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document =
    Raw(name match {
      case QualifiedName(Some(module), name) =>
        s"$module.$name"
      case QualifiedName(None, name) =>
        name
    })

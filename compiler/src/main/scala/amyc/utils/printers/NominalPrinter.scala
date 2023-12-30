package amyc.utils.printers

import amyc.ast.NominalTreeModule
import amyc.utils.printers.NominalPrinter.treeModule
import amyc.utils.{Document, Lined, Raw}
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

  override def printCall(c: treeModule.Call)(implicit printUniqueIDs: Boolean): Document =
    val Call(name, _, args) = c
    name <:> "(" <:> Lined(args map (toDoc(_)), ", ") <:> ")"

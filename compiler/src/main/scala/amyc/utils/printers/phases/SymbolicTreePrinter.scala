package amyc.utils.printers.phases

import amyc.ast.SymbolicTreeModule as S
import amyc.core.{Context, Identifier}
import amyc.utils.printers.Printer
import amyc.utils.printers.SymbolicPrinter.printName
import amyc.utils.printers.highlight.NoHighlight
import amyc.utils.{Document, Pipeline, UniqueCounter}

class SymbolicTreePrinter extends Pipeline[S.Program, S.Program]{

  private class TestUniquePrinter extends Printer(NoHighlight) {
    private val counter = new UniqueCounter[String]
    private val map = scala.collection.mutable.Map[Identifier, Int]()

    import amyc.ast.SymbolicTreeModule.*

    val treeModule: S.type = S

    inline implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document =
      printName(name)

    override implicit def printName(name: Identifier)(implicit printUniqueIds: Boolean): Document = {
      if (printUniqueIds) {
        val id = map.getOrElseUpdate(name, counter.next(name.name))
        s"${name.name}_$id"
      } else {
        name.name
      }
    }
  }

  override val name = "SymbolicTreePrinter"

  override def run(program: S.Program)(using Context): S.Program =
    println((new TestUniquePrinter)(program)(true))
    program

}

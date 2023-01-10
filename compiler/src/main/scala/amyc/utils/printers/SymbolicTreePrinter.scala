package amyc.utils.printers

import amyc.ast.{Identifier, SymbolicTreeModule as S}
import amyc.utils.{Document, Pipeline, UniqueCounter}
import amyc.core.Context

class SymbolicTreePrinter extends Pipeline[S.Program, S.Program]{

  private class TestUniquePrinter extends SymbolicPrinter {
    private val counter = new UniqueCounter[String]
    private val map = scala.collection.mutable.Map[Identifier, Int]()

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

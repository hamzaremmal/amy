package amyc.utils.printers.phases

import amyc.ast.SymbolicTreeModule as S
import amyc.core.Symbols.FunctionSymbol
import amyc.core.{Context, Identifier}
import amyc.tools.Pipeline
import amyc.utils.printers.Printer
import amyc.utils.printers.SymbolicPrinter.{apply, printName}
import amyc.utils.printers.highlight.NoHighlight
import amyc.utils.{Document, Lined, UniqueCounter}

object SymbolicTreePrinter extends Pipeline[S.Program, S.Program]{

  private class TestUniquePrinter extends Printer(NoHighlight) {
    private val counter = new UniqueCounter[String]
    private val map = scala.collection.mutable.Map[Identifier, Int]()

    import amyc.ast.SymbolicTreeModule.*

    val treeModule: S.type = S

    inline implicit def printQName(name: QualifiedName)(implicit printUniqueIds: Boolean): Document =
      printName(name)

    override def printCall(c: TestUniquePrinter.this.treeModule.Call)(implicit printUniqueIDs: Boolean): Document =
      val Call(name, targs, vargs) = c
      name match
        case f: FunctionSymbol if f is "infix" =>
          "(" <:> toDoc(vargs(0)) <:> " " <:> printName(name)(false) <:> " " <:> toDoc(vargs(1)) <:> ")"
        case _ =>
          printQName(name) <:>
            "[" <:> Lined(targs map (toDoc(_)), ", ") <:> "]" <:>
            "(" <:> Lined(vargs map (toDoc(_)), ", ") <:> ")"

    override implicit def printName(name: Name)(implicit printUniqueIds: Boolean): Document = {
      if (printUniqueIds) {
        val id = map.getOrElseUpdate(name.id, counter.next(name.name))
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

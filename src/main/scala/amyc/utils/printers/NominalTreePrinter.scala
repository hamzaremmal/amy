package amyc.utils.printers

import amyc.utils.Pipeline
import amyc.core.Context
import amyc.ast.NominalTreeModule as N

final class NominalTreePrinter extends Pipeline[N.Program, N.Program]{

  override val name = "treePrinterN"

  override def run(prog: N.Program)(using Context): N.Program = {
    println(NominalPrinter(prog))
    prog
  }

}

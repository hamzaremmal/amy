package amyc.utils.printers.phases

import amyc.ast.NominalTreeModule as N
import amyc.core.Context
import amyc.utils.Pipeline
import amyc.utils.printers.NominalPrinter

final class NominalTreePrinter extends Pipeline[N.Program, N.Program]{

  override val name = "treePrinterN"

  override def run(prog: N.Program)(using Context): N.Program = {
    println(NominalPrinter(prog))
    prog
  }

}

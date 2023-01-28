import amyc.analyzer.SymbolTable
import amyc.core.Context
import amyc.utils.Reporter

package object amyc :

  final inline def ctx(using Context): Context = summon

  final inline def reporter(using Context): Reporter = ctx.reporter

  final inline def symbols(using Context): SymbolTable = ctx.symbols
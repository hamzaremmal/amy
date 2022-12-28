import amyc.core.Context

package object amyc {

  final inline def ctx(using ctx1: Context): Context = ctx1

  final inline def reporter(using Context) = ctx.reporter

  final inline def symbols(using Context) = ctx.symbols

}

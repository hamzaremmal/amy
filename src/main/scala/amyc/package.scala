import amyc.utils.Context

package object amyc {

  final def ctx(using ctx1: Context): Context = ctx1

  final def reporter(using Context) = ctx.reporter

}

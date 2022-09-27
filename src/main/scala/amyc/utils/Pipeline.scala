package amyc.utils

// A sequence of operations to be run by the compiler,
// with interruption at every stage if there is an error
abstract class Pipeline[-F, +T] {
  self =>

  def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F,G] {
    def run(ctx : Context)(v : F) : G = {
      val first = self.run(ctx)(v)
      ctx.reporter.terminateIfErrors()
      thenn.run(ctx)(first)
    }
  }

  def run(ctx: Context)(v: F): T
}

case class Noop[T]() extends Pipeline[T, T] {
  def run(ctx: Context)(v: T) = v
}

package amyc.utils

import amyc.{core, ctx, reporter}

// A sequence of operations to be run by the compiler,
// with interruption at every stage if there is an error
abstract class Pipeline[-F, +T] {
  self =>

  def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F,G] {

    private var c = true

    override def name = if(c) self.name else thenn.name

    def run(v : F)(using core.Context) : G = {
      ctx.atPhase(self)
      val first = self.run(v)
      reporter.terminateIfErrors()
      c = false
      ctx.atPhase(thenn)
      thenn.run(first)
    }
  }

  def run(v: F)(using core.Context): T

  def name: String

}

case class Noop[T]() extends Pipeline[T, T] {
  override def run(v: T)(using core.Context): T = v

  override val name = "Noop"

}

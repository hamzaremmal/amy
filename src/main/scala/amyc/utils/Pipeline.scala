package amyc.utils

import amyc.ctx

// A sequence of operations to be run by the compiler,
// with interruption at every stage if there is an error
abstract class Pipeline[-F, +T] {
  self =>

  def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F,G] {
    def run(v : F)(using Context) : G = {
      val first = self.run(v)
      ctx.reporter.terminateIfErrors()
      thenn.run(first)
    }
  }

  def run(v: F)(using Context): T

}

case class Noop[T]() extends Pipeline[T, T] {
  def run(v: T)(using Context): T = v
}

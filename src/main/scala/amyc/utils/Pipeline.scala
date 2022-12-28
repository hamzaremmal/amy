package amyc.utils

import amyc.ctx

// A sequence of operations to be run by the compiler,
// with interruption at every stage if there is an error
abstract class Pipeline[-F, +T] {
  self =>

  def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F,G] {

    private var c = true

    def name = if(c) this.name else thenn.name

    def run(v : F)(using Context) : G = {
      val first = self.run(v)
      ctx.reporter.terminateIfErrors()
      c = false
      thenn.run(first)
    }
  }

  def run(v: F)(using Context): T

  def name: String

}

case class Noop[T]() extends Pipeline[T, T] {
  def run(v: T)(using Context): T = v

  override val name = "Noop"

}

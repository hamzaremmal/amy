package amyc.utils

import amyc.{core, ctx, reporter}
import amyc.core.Context
import amyc.utils.Pipeline.execute

// A sequence of operations to be run by the compiler,
// with interruption at every stage if there is an error
abstract class Pipeline[-F, +T] {
  self =>

  def andThen[G](thenn: Pipeline[T, G]): Pipeline[F, G] = new Pipeline[F,G] {

    private var c = true

    override def name = if(c) self.name else thenn.name

    def run(v : F)(using Context) : G =
      val first = execute(self){
        v
      }
      c = false
      execute(thenn){
        first
      }
  }

  def run(v: F)(using Context): T

  def name: String

}

object Pipeline {

  /**
    * This method should be called when running a pipeline
    * @param pipeline
    * @param body
    * @param Context
    * @tparam A
    * @tparam B
    * @return
    */
  def execute[A, B](pipeline: Pipeline[A, B])(body: => A)(using Context) : B =
      ctx.atPhase(pipeline)
      val v = pipeline.run(body)
      reporter.terminateIfErrors()
      v
  
}

case class Noop[T]() extends Pipeline[T, T] {
  override def run(v: T)(using core.Context): T = v

  override val name = "Noop"

}

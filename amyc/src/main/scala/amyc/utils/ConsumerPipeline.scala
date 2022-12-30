package amyc.utils

import amyc.core.Context

abstract class ConsumerPipeline[-F] extends Pipeline[F, Unit]

final class UnitPipeline[-F] extends ConsumerPipeline[F]{
  override val name: String = "Unit"

  override def run(v: F)(using Context): Unit = ()

}

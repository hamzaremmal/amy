package amyc.utils

import scala.collection.mutable

// Generates unique counters for each element of a type K
class UniqueCounter[K] {
  private val elemIds = mutable.Map[K, Int]().withDefaultValue(-1)

  def next(key: K): Int = synchronized {
    elemIds(key) += 1
    elemIds(key)
  }

}

package amyc.parsing

import scala.collection.mutable.ListBuffer

/**
  * Should we add them in parsing or in core or
  * have 2 separate structures in both parsing and core ?
  */
object modifiers :

  enum Modifier:
    case infix

  // ==============================================================================================
  // =================================== Utility methods ==========================================
  // ==============================================================================================

  /* Generate a component (such as regex or parser) for all the modifiers */
  def map[A](f: Modifier => A): List[A] =
    Modifier.values.toList.map(f)
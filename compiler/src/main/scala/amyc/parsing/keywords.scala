package amyc.parsing

import scala.collection.mutable.ListBuffer

/**
  * Defines all the keywords of Amy
  * To add a new keyword, only override Keyword
  *
  * `case object <name> extends Keyword`
  */
object keywords :

  private val kw: ListBuffer[Keyword] = ListBuffer.empty

  private def register(k: Keyword) =
    kw += k

  sealed abstract class Keyword:
    register(this)

  case object `abstract` extends Keyword
  case object `case`     extends Keyword
  case object `class`    extends Keyword
  case object  fn        extends Keyword
  case object `if`       extends Keyword
  case object `else`     extends Keyword
  case object `match`    extends Keyword
  case object  module    extends Keyword
  case object `val`      extends Keyword
  case object error      extends Keyword
  case object end        extends Keyword
  // cannot use _ nor `_` as an identifier
  case object wildcard extends Keyword:
    override def toString() = "_"

  // ==============================================================================================
  // =================================== utility methods ==========================================
  // ==============================================================================================

  /* Generate a component (such as regex or parser) for all the keywords */
  def map[A](f: Keyword => A): List[A] =
    kw.toList.map(f)

  /* Generate components for all the keyword */
  def flatMap[A](f: Keyword => IterableOnce[A]): List[A] =
    kw.toList.flatMap(f)

  /* Check if a given string is a keyword */
  def isKeyword(str: String) : Boolean =
    map(_.toString).contains(str)

  /* fetch the corresponding Keyword object */
  def of(str: String) : Option[Keyword] = ???

end keywords
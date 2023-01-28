package amyc.utils

import scala.annotation.targetName
import scala.collection.mutable.ListBuffer

// A structured document to be printed with nice indentation

object Document:

  /* convert a String to a Document */
  final implicit inline def str2doc(str: String): Document = Raw(str)

  /* convert a Document to a String */
  final implicit inline def doc2str(doc : Document) : String = doc.print

  // TODO HR : Implement Interpolator
  extension (sc: StringContext)
    def doc(args: Document*): Document =
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val l = ListBuffer.empty[Document]
      l.append(strings.next())
      while strings.hasNext do
        l.append(expressions.next())
        l.append(strings.next())
      Raw(l.toList.mkDoc())

  extension (it: List[Document])
    implicit def mkDoc(sep : Document = "") : Document =
      Lined(it, sep)

sealed abstract class Document {

  @targetName("concat")
  def <:>(other: Document): Document = Lined(List(this, other))

  def print: String = {
    val sb = new StringBuffer()

    def rec(d: Document)(implicit ind: Int, first: Boolean): Unit = d match {
      case Raw(s) =>
        if (first && s.nonEmpty) sb append ("  " * ind)
        sb append s
      case Indented(doc) =>
        rec(doc)(ind + 1, first)
      case Unindented(doc) =>
        assume(ind > 0)
        rec(doc)(ind - 1, first)
      case Lined(Nil, _) => // skip
      case Lined(docs, sep) =>
        rec(docs.head)
        docs.tail foreach { doc =>
          rec(sep)(ind, false)
          rec(doc)(ind, false)
        }
      case Stacked(Nil, _) => // skip
      case Stacked(docs, emptyLines) =>
        rec(docs.head)
        docs.tail foreach { doc =>
          sb append "\n"
          if (emptyLines) sb append "\n"
          rec(doc)(ind, true)
        }
    }

    rec(this)(0, true)
    sb.toString
  }
}

case class Indented(content: Document) extends Document
case class Unindented(content: Document) extends Document
case class Stacked(docs: List[Document], emptyLines: Boolean = false) extends Document
case class Lined(docs: List[Document], separator: Document = "") extends Document
case class Raw(s: String) extends Document

object Stacked {
  def apply(docs: Document*): Stacked = Stacked(docs.toList)
}
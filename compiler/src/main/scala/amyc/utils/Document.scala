package amyc.utils

import amyc.utils

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
      val l : ListBuffer[Document] = ListBuffer(strings.next())
      while strings.hasNext do
        l.append(expressions.next())
        l.append(strings.next())
      l.toList.mkDoc()

    def iden(args: Document*) : Document =
      Indented(sc.doc(args: _*))

  extension (it: List[Document])
    implicit def mkDoc(sep : Document = "") : Document =
      if it.isEmpty then
        Raw("")
      else
        it.reduce(_ <:> sep <:> _)

sealed abstract class Document {

  @targetName("concat")
  def <:>(other: Document): Document =
    Lined(this :: other :: Nil)

  def print: String = {
    val sb = new StringBuffer()
    var ind = 0
    def rec(d: Document): Unit = d match {
      case Raw(s) =>
        sb append s
      case Indented(doc) =>
        ind += 1
        rec(doc)
        ind -= 1
      case Unindented(doc) =>
        assume(ind > 0)
        sb append "\b\u0000\b"
        ind -= 1
        rec(doc)
      case Lined(Nil, _) => // skip
      case Lined(docs, sep) =>
        rec(docs.head)
        docs.tail foreach { doc =>
          rec(sep)
          rec(doc)
        }
      case Stacked(Nil, _) => // skip
      case Stacked(docs, emptyLines) =>
        sb append ("  " * ind)
        rec(docs.head)
        docs.tail foreach { doc =>
          sb append "\n"
          if (emptyLines) sb append "\n"
          sb append ("  " * ind)
          rec(doc)
        }
    }

    rec(this)
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
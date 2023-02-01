package amyc.utils.printers.highlight

import amyc.parsing.keywords.Keyword
import amyc.utils.{Document, Lined}

object SyntaxHighlighter extends Highlighter :

  import Document.*

  private inline def color(c: String, s: String): Document =
    Lined(str2doc(c) :: str2doc(s) :: str2doc(Console.RESET) :: Nil)

  extension (sc : StringContext)
    implicit def kw(k : Keyword) : Document = color(Console.BLUE, k)
    def lit(x : Any*) : Document = ???
    def tpe(x : Any*) : Document = ???

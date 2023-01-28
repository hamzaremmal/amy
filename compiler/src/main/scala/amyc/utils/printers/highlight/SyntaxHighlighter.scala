package amyc.utils.printers.highlight

import amyc.utils.Document

object SyntaxHighlighter extends Highlighter :

  extension (sc : StringContext)
    def kw(x : Any*) : Document = ???
    def lit(x : Any*) : Document = ???
    def tpe(x : Any*) : Document = ???

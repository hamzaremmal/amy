package amyc.utils.printers.highlight

import amyc.parsing.keywords.Keyword
import amyc.utils.Document

abstract class Highlighter :

  /* implicit conversion to highlight keywords */
  final implicit def keyword(k: Keyword): Document = kw"$k"

  /* Still need to fix the type parameter */

  /* String interpolator to be used to highlight */
  extension (sc : StringContext)
    /* Highlight a keyword */
    implicit def kw(x : Keyword): Document
    /* Highlight a literal */
    def lit(x : Any*) : Document
    /* Highlight a type */
    def tpe(x : Any*) : Document


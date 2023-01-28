package amyc.utils.printers.highlight

import amyc.utils.{Document, Raw}

/**
  * Use this specific Highlighter to avoid highlighting
  */
object NoHighlight extends Highlighter :

  // Use this macro to return the corresponding Document with no hightlight
  private inline def raw(inline sc : StringContext, inline args : Any*): Document =
    Raw(sc.s(args: _*))

  extension (sc: StringContext)
    inline def kw(args: Any*): Document = raw(sc, args: _*)
    inline def lit(args : Any*): Document = raw(sc, args: _*)
    inline def tpe(args: Any*): Document = raw(sc, args: _*)


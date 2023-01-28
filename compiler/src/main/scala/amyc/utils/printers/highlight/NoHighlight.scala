package amyc.utils.printers.highlight

/**
  * Use this specific Highlighter to avoid highlighting
  */
object NoHighlight extends Highlighter :

  override inline def highlightKeyword(str: String): String = str

  override inline def highlightLiteral(str: String): String = str


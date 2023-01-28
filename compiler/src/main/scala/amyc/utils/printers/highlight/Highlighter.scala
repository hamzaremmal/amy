package amyc.utils.printers.highlight

abstract class Highlighter :

  /* Highlight a keyword */
  def highlightKeyword(str: String) : String
  
  /* Highlight literals*/
  def highlightLiteral(str: String): String

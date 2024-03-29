package amyc.utils.printers.phases

import amyc.core.Context
import amyc.parsing.Token
import amyc.utils.Pipeline


/** Extracts all tokens from input and displays them */
object DisplayTokens extends Pipeline[Iterator[Token], Unit] {

  override val name = "DisplayTokens"

  override def run(tokens: Iterator[Token])(using Context): Unit =
    for token <- tokens do println(token)
}

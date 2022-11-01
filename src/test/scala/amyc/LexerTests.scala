package amyc

import parsing._
import org.junit.Test

class LexerTests extends TestSuite {
  val pipeline = Lexer andThen DisplayTokens

  val baseDir = "amyc/lexer"

  val outputExt = "txt"

  @Test def testKeywords = shouldOutput("Keywords")

  
}

package amyc

import parsing._
import amyc.utils.printers.DisplayTokens
import org.junit.Test

class LexerTests extends TestSuite {

  override val pipeline =
    Lexer andThen
    DisplayTokens

  override val baseDir = "amyc/lexer"

  override val outputExt = "txt"

  // ==============================================================================================
  // ======================================= TESTS ================================================
  // ==============================================================================================

  @Test def testKeywords = shouldOutput("Keywords")

  @Test def testIdentifiers = shouldOutput("Identifiers")

  @Test def testOperators = shouldOutput("Operators")

  @Test def testDelimiters = shouldOutput("Delimiters")

  @Test def testCombinations = shouldOutput("Combinations")

  @Test def testComments = shouldOutput("Comments")

  @Test def testIntLiterals = shouldOutput("IntLiterals")

  @Test def testStringLiterals = shouldOutput("StringLiterals")

  @Test def testTwoFiles = shouldOutput(List("Keywords", "Operators"), "TwoFiles")

  @Test def testSingleAmp = shouldFail("SingleAmp")

  @Test def testSingleBar = shouldFail("SingleBar")

  @Test def testUnclosedComment = shouldFail("UnclosedComment")

  @Test def testUnclosedComment2 = shouldFail("UnclosedComment2")

  @Test def testUnclosedComment3 = shouldFail("UnclosedComment3")

  @Test def testCommentClosedTwice = shouldOutput("CommentClosedTwice")

  @Test def testUnclosedString1 = shouldFail("UnclosedString1")

  @Test def testUnclosedString2 = shouldFail("UnclosedString2")

  @Test def testInvalid = shouldFail("Invalid")

  @Test def testTooBigInt = shouldFail("TooBigInt")

  @Test def testWhitespace = shouldOutput("Whitespace")

}

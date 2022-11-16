package amyc

import utils._
import ast._
import parsing._
import org.junit.Test

class ParserTests extends TestSuite {

  import NominalTreeModule.{Program => NP}

  def treePrinterN(title: String): Pipeline[NP, Unit] = {
    new Pipeline[NP, Unit] {
      def run(ctx: Context)(v: NP) = {
        println(title)
        println(NominalPrinter(v))
      }
    }
  }

  val pipeline = Lexer andThen Parser andThen treePrinterN("")

  val baseDir = "amyc/parser"

  val outputExt = "amy"

  @Test def testLL1 = {
    assert(Parser.program.isLL1)
  }

  
  @Test def testEmpty = shouldOutput("Empty")

  @Test def testErrorToken1 = shouldOutput("ErrorToken1")

  @Test def testErrorToken2 = shouldOutput("ErrorToken2")

  @Test def testLiterals = shouldOutput("Literals")

  @Test def testPrecedence = shouldOutput("Precedence")

  @Test def testAssoc = shouldOutput("Assoc")

  @Test def testAssocSemicolon = shouldOutput("AssocSemicolon")

  @Test def testFunDefs = shouldOutput("FunDefs")

  @Test def testFunCalls = shouldOutput("FunCalls")

  @Test def testClassDefs = shouldOutput("ClassDefs")

  @Test def testPatterns = shouldOutput("Patterns")

  @Test def testNestedMatch = shouldOutput("NestedMatch")

  @Test def testParens = shouldOutput("Parens")

  @Test def testQualifiedNames = shouldOutput("QualifiedNames")

  @Test def testList = shouldOutput("List")

  @Test def testIfCondition = shouldOutput("IfCondition")
  @Test def testMatchScrutinee = shouldOutput("MatchScrutinee")
  @Test def testChainedMatch = shouldOutput("ChainedMatch")

  @Test def testArgsError1 = shouldFail("ArgsError1")
  @Test def testArgsError2 = shouldFail("ArgsError2")
  @Test def testClassDefError1 = shouldFail("ClassDefError1")
  @Test def testClassDefError2 = shouldFail("ClassDefError2")
  @Test def testClassDefError3 = shouldFail("ClassDefError3")
  @Test def testClassDefError4 = shouldFail("ClassDefError4")
  @Test def testCommentClosedTwice = shouldFail("CommentClosedTwice")
  @Test def testEmptyFile = shouldFail("EmptyFile")
  @Test def testFunDefError1 = shouldFail("FunDefError1")
  @Test def testFunDefError2 = shouldFail("FunDefError2")
  @Test def testFunDefError3 = shouldFail("FunDefError3")
  @Test def testFunDefError4 = shouldFail("FunDefError4")
  @Test def testFunDefError5 = shouldFail("FunDefError5")
  @Test def testFunDefError6 = shouldFail("FunDefError6")
  @Test def testIfPrecedence = shouldFail("IfPrecedence")
  @Test def testIntError1 = shouldFail("IntError1")
  @Test def testIntError2 = shouldFail("IntError2")
  @Test def testIntError3 = shouldFail("IntError3")
  @Test def testTypeWidth = shouldFail("TypeWidth")
  @Test def testMatchAsOperand = shouldFail("MatchAsOperand")
  @Test def testMissingOperand = shouldFail("MissingOperand")
  @Test def testUnaryWithinUnary = shouldFail("UnaryWithinUnary")
  @Test def testUnmatchedModule = shouldFail("UnmatchedModule")
  @Test def testUnmatchedModuleName = shouldFail("UnmatchedModuleName")
  @Test def testUnmatchedParen = shouldFail("UnmatchedParen")
  @Test def testValAsOperand = shouldFail("ValAsOperand")
  @Test def testValError = shouldFail("ValError")
  @Test def testValInVal = shouldFail("ValInVal")
  @Test def testWrongQName1 = shouldFail("WrongQName1")
  @Test def testWrongQName2 = shouldFail("WrongQName2")
  @Test def testWrongQName3 = shouldFail("WrongQName3")
  @Test def testWrongQName4 = shouldFail("WrongQName4")

}

package amyc

import parsing._
import amyc.ast.{Identifier, SymbolicPrinter}
import amyc.ast.SymbolicTreeModule.Program
import analyzer.{NameAnalyzer, TypeChecker}
import amyc.utils._
import org.junit.Test

class TyperTests extends TestSuite {
  // We need a unit pipeline
  private def unit[A]: Pipeline[A, Unit] = {
    new Pipeline[A, Unit] {
      def run(ctx: Context)(v: A) = ()
    }
  }

  val pipeline = Lexer andThen Parser andThen NameAnalyzer andThen TypeChecker andThen unit

  val baseDir = "amyc/typer"

  val outputExt = "" // No output files for typechecking

  @Test def testArithError1 = shouldFail("ArithError1")
  @Test def testOperatorError1 = shouldFail("OperatorError1")
  @Test def testConstructorError1 = shouldFail("ConstructorError1")
  @Test def testFunctionCalls1 = shouldFail("FunctionCalls1")
  @Test def testIfError1 = shouldFail("IfError1")
  @Test def testLogicalError1 = shouldFail("LogicalError1")
  @Test def testMatchError1 = shouldFail("MatchError1")
  @Test def testSeqError1 = shouldFail("SeqError1")

  @Test def testArithError2 = shouldFail("ArithError2")
  @Test def testArithError3 = shouldFail("ArithError3")
  @Test def testArithError4 = shouldFail("ArithError4")
  @Test def testArithError5 = shouldFail("ArithError5")
  @Test def testComparisonError = shouldFail("ComparisonError")
  @Test def testOperatorError2 = shouldFail("OperatorError2")
  @Test def testOperatorError3 = shouldFail("OperatorError3")
  @Test def testConstructorError2 = shouldFail("ConstructorError2")
  @Test def testConstructorError3 = shouldFail("ConstructorError3")
  @Test def testConstructorError4 = shouldFail("ConstructorError4")
  @Test def testEqualityError = shouldFail("EqualityError")
  @Test def testFunctionCalls2 = shouldFail("FunctionCalls2")
  @Test def testFunctionCalls3 = shouldFail("FunctionCalls3")
  @Test def testIfAndErrorWrong = shouldFail("IfAndErrorWrong")
  @Test def testIfError2 = shouldFail("IfError2")
  @Test def testIfError3 = shouldFail("IfError3")
  @Test def testLetError1 = shouldFail("LetError1")
  @Test def testLetError2 = shouldFail("LetError2")
  @Test def testLogicalError2 = shouldFail("LogicalError2")
  @Test def testLogicalError3 = shouldFail("LogicalError3")
  @Test def testMatchError2 = shouldFail("MatchError2")
  @Test def testMatchError3 = shouldFail("MatchError3")
  @Test def testMatchError4 = shouldFail("MatchError4")
  @Test def testMatchError5 = shouldFail("MatchError5")
  @Test def testMatchError6 = shouldFail("MatchError6")
  @Test def testSeqError2 = shouldFail("SeqError2")
  @Test def testSeqError3 = shouldFail("SeqError3")
  @Test def testConcatError = shouldFail("ConcatError")
  @Test def testErrorError = shouldFail("ErrorError")

  @Test def testArithmetic = shouldPass("Arithmetic")
  @Test def testComparison = shouldPass("Comparison")
  @Test def testConstructors = shouldPass("Constructors")
  @Test def testFunctionCalls = shouldPass("FunctionCalls")
  @Test def testIf = shouldPass("If")
  @Test def testIfWithError = shouldPass("IfWithError")
  @Test def testLogical = shouldPass("Logical")
  @Test def testMatch1 = shouldPass("Match1")
  @Test def testMatch2 = shouldPass("Match2")
  @Test def testMatch3 = shouldPass("Match3")
  @Test def testMatchWithError = shouldPass("MatchWithError")
  @Test def testMisc = shouldPass("Misc")
  @Test def testSeq = shouldPass("Seq")
  @Test def testSeqWithError = shouldPass("SeqWithError")
  @Test def testLetWithError = shouldPass("LetWithError")
  @Test def testError = shouldPass("Error")

}

package amyc

import parsing.*
import amyc.utils.UnitPipeline
import amyc.ast.SymbolicTreeModule.Program
import amyc.core.Identifier
import analyzer.NameAnalyzer
import typer.Typer
import amyc.utils.*
import amyc.utils.printers.SymbolicPrinter
import org.junit.Test

class TyperTests extends TestSuite {

  override val pipeline =
    Lexer andThen
    Parser andThen
    NameAnalyzer andThen
    Typer andThen
    new UnitPipeline

  override val baseDir = "amyc/typer"

  override val outputExt = "" // No output files for typechecking

  // ==============================================================================================
  // ======================================= TESTS ================================================
  // ==============================================================================================

  @Test def testArithError1 = shouldFail("ArithError1")
  @Test def testOperatorError1 = shouldFail("OperatorError1")
  @Test def testConstructorError1 = shouldFail("ConstructorError1")
  @Test def testFunctionCalls1 = shouldFail("FunctionCalls1")
  @Test def testIfError1 = shouldFail("IfError1")
  @Test def testLogicalError1 = shouldFail("LogicalError1")
  @Test def testMatchError1 = shouldFail("MatchError1")
  def testSeqError1 = shouldFail("SeqError1")

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
  def testLetError1 = shouldFail("LetError1")
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

  @Test def testArithmetic = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "Arithmetic"))
  @Test def testComparison = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "Comparison"))
  @Test def testConstructors = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "Constructors"))
  @Test def testFunctionCalls = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "FunctionCalls"))
  @Test def testIf = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "If"))
  def testIfWithError = shouldPass("IfWithError")
  @Test def testLogical = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "Logical"))
  @Test def testMatch1 = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "Match1"))
  @Test def testMatch2 = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "Match2"))
  @Test def testMatch3 = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "Match3"))
  @Test def testMatchWithError = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "MatchWithError"))
  @Test def testMisc = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "Misc"))
  @Test def testSeq = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "Seq"))
  def testSeqWithError = shouldPass("SeqWithError")
  def testLetWithError = shouldPass("LetWithError")
  @Test def testError = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "Error"))

  @Test def highOrderFunctionParam = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "FunctionInParameter"))

  @Test def highOrderFunctionRte = shouldPass(List("String", "Unit", "Int", "Boolean","unnamed", "FunctionInReturnType"))

}

package amyc

import org.junit.Test

abstract class ExecutionTests extends TestSuite {

  override val baseDir = "amyc/execution"

  override val outputExt = "txt"

  // ==============================================================================================
  // ======================================== TESTS ===============================================
  // ==============================================================================================

  @Test def testEmptyObject = shouldOutput(List("Std", "EmptyObject"), "EmptyObject")

  @Test def testMinimalError = shouldFail(List("Std", "MinimalError"))

  @Test def testStringInputToOutput = shouldOutput(List("Std", "StringInputToOutput.grading"), "StringInputToOutput.grading", "Hello World\nHello World again")

  @Test def testIntInputToOutput = shouldOutput(List("Std", "IntInputToOutput.grading"), "IntInputToOutput.grading", "42")

  @Test def testMixStdAndNonStd = shouldOutput(List("Std", "MixStdAndNonStd.grading"), "MixStdAndNonStd.grading", "42")

  @Test def testBasicArithmetic = shouldOutput(List("Std", "BasicArithmetic.grading"), "BasicArithmetic.grading", "")

  @Test def testDivisionByZero = shouldFail(List("Std", "Division.grading"))

  @Test def testBasicPatternMatching = shouldOutput(List("Std", "BasicPatternMatching.grading"), "BasicPatternMatching.grading", "")

  @Test def testBasicBranching = shouldOutput(List("Std", "BasicBranching.grading"), "BasicBranching.grading", "")

  @Test def testBasicConditions = shouldOutput(List("Std", "BasicConditions.grading"), "BasicConditions.grading", "")

  @Test def testBasicError = shouldFail(List("Std", "BasicError.grading"))

  @Test def testEquality = shouldOutput(List("Std", "Equality.grading"), "Equality.grading")

  @Test def testFactorial = shouldOutput(List("Std", "Factorial.grading"), "Factorial.grading", "")

  @Test def testArithmetic = shouldOutput(List("Std", "Arithmetic.grading"), "Arithmetic.grading", "")

  @Test def testLists = shouldOutput(List("Std", "Option", "List", "TestLists.grading"), "TestLists.grading", "")

  @Test def testMatchError1 = shouldFail(List("Std", "MatchError1.grading"))

  @Test def testMatchError2 = shouldFail(List("Std", "MatchError2.grading"))

  @Test def testMatchError3 = shouldFail(List("Std", "MatchError3.grading"))

  @Test def testMatchError4 = shouldFail(List("Std", "MatchError4.grading"))

  @Test def testMatch1 = shouldPass(List("Std", "Match1.grading"))

  @Test def testMatch2 = shouldPass(List("Std", "Match2.grading"))

  @Test def testMatch3 = shouldPass(List("Std", "Match3.grading"))

  @Test def testMatch4 = shouldOutput(List("Std", "Match4.grading"), "Match4.grading")

  @Test def testShortCircuit1 = shouldPass(List("Std", "ShortCircuit.grading"))

  @Test def testShortCircuit2 = shouldFail(List("Std","ShortCircuit.grading"))

  @Test def testLocals1 = shouldPass(List("Std","Locals.grading"))

  @Test def testLocals2 = shouldFail(List("Std","Locals.grading"))

  @Test def testFunCallEnv = shouldOutput(List("Std", "FunCallEnv.grading"), "FunCallEnv.grading")

  @Test def testSideEffect = shouldOutput(List("Std", "Option", "List", "SideEffect.grading"), "SideEffect.grading")

}

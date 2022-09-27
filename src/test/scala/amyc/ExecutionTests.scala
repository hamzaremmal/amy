package amyc

import org.junit.Test

abstract class ExecutionTests extends TestSuite {

  val baseDir = "amyc/execution"

  val outputExt = "txt"

  @Test def testEmptyObject = shouldOutput("EmptyObject")

  @Test def testMinimalError = shouldFail("MinimalError")

  }

package amyc

import java.io._

/** Some utilities for running tests */
trait TestUtils {
  /** Run test,
    * with input also redirected from a String,
    * and output is redirected to a local StringBuilder.
    */
  def testWithRedirectedIO[T](test: => T, input: String): String = {
    import scala.Console._
    val inputS  = new StringReader(input)
    val outputS = new ByteArrayOutputStream()
    withOut(outputS) {
      withErr(outputS) {
        withIn(inputS) {
          test
        }
      }
    }
    outputS.toString()
  }
}

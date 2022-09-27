package amyc

import amyc.utils._
import java.io.File

import org.junit.Assert.fail

abstract class CompilerTest extends TestUtils {
  private def runPipeline(pipeline: Pipeline[List[File], Unit], fileNames: List[String]) = {
    val ctx = Context(new Reporter, fileNames)
    val files = ctx.files.map(new File(_))
    for (f <- files) do
      assert(f != null && f.exists, s"Could not read test file ${f.getPath()}")
    pipeline.run(ctx)(files)
    ctx.reporter.terminateIfErrors()
  }

  private def runPipelineRedirected(
    pipeline: Pipeline[List[File], Unit],
    compiledFiles: List[String],
    input: String
  ): String = {
    testWithRedirectedIO(runPipeline(pipeline, compiledFiles), input)
  }

  private def assertEqual(output: String, expected: String) = {
    val rejectLine = (s: String) =>
      s.isEmpty ||
        s.startsWith("[ Info  ]") ||
        s.startsWith("[Warning]") ||
        s.startsWith("[ Error ]") ||
        s.startsWith("[ Fatal ]")
    def filtered(s: String) = s.linesIterator.filterNot(rejectLine).mkString("\n")
    val filteredOutput = filtered(output)
    val filteredExpected = filtered(expected)
    if (filteredOutput != filteredExpected) {
      val sb = new StringBuffer()
      sb.append("\nOutput is different:\n")
      sb.append("\nOutput: \n")
      sb.append(filteredOutput)
      sb.append("\n\nExpected output: \n")
      sb.append(filteredExpected)
      sb.append("\n")
      fail(sb.toString)
    }
  }

  protected def compareOutputs(
    pipeline: Pipeline[List[File], Unit],
    compiledFiles: List[String],
    expectedFile: String,
    input: String = ""
  ) = {
    try {
      val output = runPipelineRedirected(pipeline, compiledFiles, input)
      val expected = scala.io.Source.fromFile(new File(expectedFile)).mkString
      assertEqual(output, expected)
    } catch {
      // We only want to catch AmyFatalError gracefully, the rest can propagate
      case AmycFatalError(msg) =>
        fail(s"\n  $msg\n")
    }
  }

  protected def demandPass(
    pipeline: Pipeline[List[File], Unit],
    compiledFiles: List[String],
    input: String = ""
  ) = {
    try {
      runPipelineRedirected(pipeline, compiledFiles, input)
    } catch {
      case AmycFatalError(msg) =>
        fail(s"\n  $msg\n")
    }
  }

  protected def demandFailure(
    pipeline: Pipeline[List[File], Unit],
    compiledFiles: List[String],
    input: String = ""
  ) = {
    try {
      runPipelineRedirected(pipeline, compiledFiles, input)
      fail("Test should fail but it passed!")
    } catch {
      case AmycFatalError(_) =>
      // Ok, this is what we wanted. Other exceptions should propagate though
    }

  }


}

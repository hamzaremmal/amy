package amyc

import amyc.utils.Pipeline
import java.io.File
import java.nio.file.Files
import java.nio.file.StandardCopyOption

abstract class TestSuite extends CompilerTest {
  val pipeline: Pipeline[List[File], Unit]

  val baseDir: String

  val passing = "passing"
  val failing = "failing"
  val outputs = "outputs"

  val tmpDir = Files.createTempDirectory("amyc");

  val outputExt: String

  def getResourcePath(relativePath: String): String =
    val stream = getClass.getResourceAsStream(s"/$baseDir/$relativePath")
    assert(stream != null, s"can not read $baseDir/$relativePath")
    val targetPath = tmpDir.resolve(relativePath)
    Files.createDirectories(targetPath.getParent())
    Files.copy(stream, targetPath, StandardCopyOption.REPLACE_EXISTING)
    targetPath.toAbsolutePath().toString()

  def shouldOutput(inputFiles: List[String], outputFile: String, input: String = ""): Unit = {
    compareOutputs(
      pipeline,
      inputFiles map (f => getResourcePath(s"$passing/$f.amy")),
      getResourcePath(s"$outputs/$outputFile.$outputExt"),
      input
    )
  }

  def shouldOutput(inputFile: String): Unit = {
    shouldOutput(List(inputFile), inputFile)
  }

  def shouldFail(inputFiles: List[String], input: String = ""): Unit = {
    demandFailure(
      pipeline,
      inputFiles map (f => getResourcePath(s"$failing/$f.amy")),
      input
    )
  }

  def shouldFail(inputFile: String): Unit = {
    shouldFail(List(inputFile))
  }

  def shouldPass(inputFiles: List[String], input: String = ""): Unit = {
    demandPass(pipeline, inputFiles map (f => getResourcePath(s"$passing/$f.amy")), input)
  }

  def shouldPass(inputFile: String): Unit = {
    shouldPass(List(inputFile))
  }

}

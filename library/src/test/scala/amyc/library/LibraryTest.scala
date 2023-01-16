package amyc.library

import amyc.TestSuite
import amyc.analyzer.NameAnalyzer
import amyc.parsing.{Lexer, Parser}
import amyc.typer.Typer
import amyc.utils.UnitPipeline
import org.junit.Test

import java.nio.file.{Files, StandardCopyOption}

class LibraryTest extends TestSuite :

  override val baseDir = ""

  override val outputExt = ""

  override val pipeline =
    Lexer andThen
    Parser andThen
    NameAnalyzer andThen
    Typer andThen
    new UnitPipeline

  def checkCompilation(files : List[String]) : Unit =
    demandPass(pipeline, files map (f => getResourcePath(s"$f.amy")), "")

  def getResourcePath(relativePath: String): String =
    val stream = getClass.getResourceAsStream(s"/$relativePath")
    val path = relativePath
    if stream == null then
      assert(stream != null, s"can not read $baseDir/$relativePath")
    val targetPath = tmpDir.resolve(path)
    Files.createDirectories(targetPath.getParent)
    Files.copy(stream, targetPath, StandardCopyOption.REPLACE_EXISTING)
    targetPath.toAbsolutePath.toString


  // ==============================================================================================
  // ======================================== TESTS ===============================================
  // ==============================================================================================

  @Test def testStd = checkCompilation("Std" :: Nil)

  @Test def testListAndOption = checkCompilation("Std" :: "Option" :: "List" :: Nil)
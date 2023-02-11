package amyc

import parsing.*
import amyc.ast.SymbolicTreeModule
import amyc.ast.SymbolicTreeModule.Program
import amyc.core.Identifier
import analyzer.{NameAnalyzer, SymbolTable}
import amyc.utils.*
import amyc.utils.printers.SymbolicPrinter
import amyc.utils.printers.phases.SymbolicTreePrinter
import org.junit.Test

import scala.language.implicitConversions

class NameAnalyzerTests extends TestSuite {

  override val pipeline =
    Lexer andThen
    Parser andThen
    NameAnalyzer andThen
    new SymbolicTreePrinter andThen
    new UnitPipeline

  override val baseDir = "amyc/nameAnalyzer"

  override val outputExt = "scala"

  // ==============================================================================================
  // ====================================== TESTS =================================================
  // ==============================================================================================

  @Test def testClasses = shouldOutput(List("Classes", "Unit", "Int", "Boolean","unnamed", "String"), "Classes")
  @Test def testConstructorVsIdPattern = shouldOutput(List("ConstructorVsIdPattern", "Unit", "Int", "Boolean","unnamed", "String"), "ConstructorVsIdPattern")
  @Test def testDefOrder = shouldOutput(List("DefOrder", "Unit", "Int", "Boolean","unnamed", "String"), "DefOrder")
  @Test def testDuplicateClassParams = shouldOutput(List("DuplicateClassParams", "Unit", "Int", "Boolean","unnamed", "String"), "DuplicateClassParams")
  @Test def testNotDuplicateLocals1 = shouldOutput(List("NotDuplicateLocals1", "Unit", "Int", "Boolean","unnamed", "String"), "NotDuplicateLocals1")
  @Test def testNotDuplicateLocals2 = shouldOutput(List("NotDuplicateLocals2", "Unit", "Int", "Boolean","unnamed", "String"), "NotDuplicateLocals2")
  @Test def testParamAndLocal = shouldOutput(List("ParamAndLocal", "Unit", "Int", "Boolean","unnamed", "String"), "ParamAndLocal")
  @Test def testRecursive = shouldOutput(List("Recursive", "Unit", "Int", "Boolean","unnamed", "String"), "Recursive")
  @Test def testList = shouldOutput(List("List", "Std", "Option", "Unit", "Int", "Boolean","unnamed", "String"), "List")
  @Test def matchShadowParameter = shouldOutput(List("MatchShadowParameter", "Unit", "Int", "Boolean","unnamed", "String"), "MatchShadowParameter")

  @Test def testArgumentNumberFunction = shouldFail("ArgumentNumberFunction")
  @Test def testArgumentNumberConstructor = shouldFail("ArgumentNumberConstructor")
  @Test def testArgumentNumberPattern = shouldFail("ArgumentNumberPattern")
  @Test def testDuplicateDefs1 = shouldFail("DuplicateDefs1")
  @Test def testDuplicateDefs2 = shouldFail("DuplicateDefs2")
  @Test def testDuplicateLocals = shouldFail("DuplicateLocals")
  @Test def testDuplicatMatchLocals1 = shouldFail("DuplicateMatchLocals1")
  @Test def testDuplicatMatchLocals2 = shouldFail("DuplicateMatchLocals2")
  @Test def testDuplicateModules = shouldFail(List("DuplicateModulesA", "DuplicateModulesB"))
  @Test def testDuplicateParams = shouldFail("DuplicateParams")
  @Test def testLocalScope = shouldFail("LocalScope")
  @Test def testUndefinedConstructor1 = shouldFail("UndefinedConstructor1")
  @Test def testUndefinedConstructor2 = shouldFail("UndefinedConstructor2")
  @Test def testUndefinedFunction = shouldFail("UndefinedFunction")
  @Test def testUndefinedModule = shouldFail("UndefinedModule")
  @Test def testUndefinedTypeInModule = shouldFail(List("UndefinedTypeInModuleA", "UndefinedTypeInModuleB"))
  @Test def testUndefinedType = shouldFail("UndefinedType")
  @Test def testUndefinedVariable = shouldFail("UndefinedVariable")
  @Test def testUnknownParent = shouldFail("UnknownParent")
}

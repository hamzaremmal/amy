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

  
}

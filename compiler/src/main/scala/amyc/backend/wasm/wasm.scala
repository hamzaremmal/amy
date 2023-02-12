package amyc.backend.wasm

import amyc.utils.Preconditions.*
import Instructions.{Code, i32, id}
import amyc.backend.wasm.utils.*
import amyc.core.*
import amyc.ast.SymbolicTreeModule.FunDef
import amyc.backend.wasm.types.*
import amyc.core.Symbols.*
import amyc.symbols

import scala.annotation.constructorOnly

/**
  * https://webassembly.github.io/spec/core/syntax/modules.html
  */


// A WebAssembly module
case class Module(name: String,
                  globals: Int,
                  imports: List[String],
                  table: Option[Table],
                  functions: List[Function])

// A web assembly type (i32, i64, f32, f64)
abstract class Type

// TODO HR : replace Function with F
case class Function private(name: id, params: List[param], result: Option[result], locals: List[local], code: Code, idx: Int)

object Function {

  def forSymbol(sym: FunctionSymbol, result: Option[result])(code: LocalsHandler ?=> Code)(using ModuleHandler): Function =
    given LocalsHandler = new LocalsHandler(sym, mh, textmode = true)
    val instructions = code
    new Function(
      fullName(sym.owner, sym),
      lh.params,
      result,
      lh.locals,
      instructions,
      mh.function(sym)
    )

  def forDefinition(fd: FunDef, owner: Symbol, result: Option[result])(codeGen: LocalsHandler ?=> Code)(using ModuleHandler): Function =
    given LocalsHandler = new LocalsHandler(fd.name.asInstanceOf, mh, textmode = true)
    // Make code first, as it may increment the locals in lh
    val code = codeGen
    new Function(
      fullName(owner, fd.name),
      lh.params,
      result,
      lh.locals,
      code,
      mh.function(fd.name)
    )

}

case class Table(size: Int, elems: List[Function])(using Context):
  require(elems.size == size)

case class Import(elem: String)
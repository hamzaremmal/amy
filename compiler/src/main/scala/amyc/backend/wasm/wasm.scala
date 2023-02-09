package amyc.backend.wasm

import amyc.utils.Preconditions.*
import Instructions.{Code, i32, id}
import amyc.backend.wasm.utils.{LocalsHandler, lh}
import amyc.backend.wasm.utils.Utils.*
import amyc.core.{Context, Identifier}
import amyc.ast.SymbolicTreeModule.FunDef
import amyc.backend.wasm.types.param
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

// If isMain = false, represents a function which returns an i32 and will not be exported to js
// If isMain = true , represents a function which does not return a value, and will be exported to js
case class Function private (name: String, params: List[param], locals: Int, code: Code, idx: Int, isMain: Boolean)

object Function {

  def forSymbol(sym: FunctionSymbol, isMain: Boolean)(code: LocalsHandler ?=> Code): Function =
    given LocalsHandler = new LocalsHandler(sym.param.length, textmode = false)
    val instructions = code
    new Function(
      fullName(sym.owner, sym),
      sym.param.map(_ => param(None, i32)),
      lh.locals,
      instructions,
      sym.idx,
      isMain
    )

  def forDefinition(fd: FunDef, owner: Symbol, isMain: Boolean, idx: Int)(codeGen: LocalsHandler ?=> Code): Function =
    given LocalsHandler = new LocalsHandler(fd.params.map(_.name.id), textmode = false)
    // Make code first, as it may increment the locals in lh
    val code = codeGen
    new Function(
      fullName(owner, fd.name),
      fd.params.map(df => param(Some(id(df.name.name)), i32)),
      lh.locals,
      code,
      idx,
      isMain
    )

}

case class Table(size: Int, elems: List[Function])(using Context):
  require(elems.size == size)

case class Import(elem: String)
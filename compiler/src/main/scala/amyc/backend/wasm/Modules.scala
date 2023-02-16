package amyc.backend.wasm

import amyc.utils.Preconditions.*
import Instructions.{Code, i32}
import amyc.backend.wasm.Values.*
import amyc.backend.wasm.utils.*
import amyc.core.*
import amyc.ast.SymbolicTreeModule.{CaseClassDef, FunDef}
import amyc.backend.wasm.Types.*
import amyc.backend.wasm.handlers.{LocalsHandler, ModuleHandler}
import amyc.core.Symbols.*
import amyc.symbols

import scala.annotation.constructorOnly

/**
  * https://webassembly.github.io/spec/core/syntax/modules.html
  */
object Modules :
  // A WebAssembly module
  case class Module(name: String,
                    globals: List[Global],
                    imports: List[String],
                    table: Option[Table],
                    data: List[Data],
                    functions: List[Function])

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

    def forDefinition(fd: FunDef, result: Option[result])(codeGen: LocalsHandler ?=> Code)(using ModuleHandler): Function =
      given LocalsHandler = new LocalsHandler(fd.name.asInstanceOf, mh, textmode = true)

      // Make code first, as it may increment the locals in lh
      val code = codeGen
      new Function(
        fullName(fd.name.asInstanceOf[FunctionSymbol].owner, fd.name),
        lh.params,
        result,
        lh.locals,
        code,
        mh.function(fd.name)
      )

    def forDefinition(cd: CaseClassDef)(codeGen: LocalsHandler ?=> Code)(using ModuleHandler): Function =
      given LocalsHandler = new LocalsHandler(cd.name.asInstanceOf, mh, textmode = true)

      // Make code first, as it may increment the locals in lh
      val code = codeGen
      new Function(
        fullName(cd.name.asInstanceOf[ConstructorSymbol].owner, cd.name),
        lh.params,
        Some(result(i32)), // The result of a constructor is the address in memory -i32-
        lh.locals,
        code,
        -1
      )

  }

  case class Table(elems: List[FunctionSymbol])

  case class Data(offset: Int, str: String)

  case class Global(value: Int)

  case class Import(elem: String)

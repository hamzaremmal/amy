package amyc.backend.wasm

import amyc.*
import amyc.ast.SymbolicTreeModule.{CaseClassDef, FunDef}
import amyc.core.*
import amyc.core.Symbols.*
import amyc.backend.wasm.Indices.*
import amyc.backend.wasm.Instructions.*
import amyc.backend.wasm.Types.*
import amyc.backend.wasm.Values.*
import amyc.backend.wasm.utils.*
import amyc.backend.wasm.handlers.*
import amyc.utils.Preconditions.*

/**
  * https://webassembly.github.io/spec/core/text/modules.html
  */
object Modules :

  /**
    * A WebAssembly `import` statement
    *
    * https://webassembly.github.io/spec/core/text/modules.html#imports
    */
  case class Import(elem: String) // TODO HR : Doesn't follow the convention


  /**
    * A WebAssembly Function
    *
    * https://webassembly.github.io/spec/core/text/modules.html#functions
    */
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

  /**
    * A WebAssembly Table
    *
    * https://webassembly.github.io/spec/core/text/modules.html#tables
    */
  case class Table(elems: List[FunctionSymbol])

  /**
    * A WbeAssembly Memory
    *
    * https://webassembly.github.io/spec/core/text/modules.html#memories
    */
  case class Memory(id: Option[id]) // TODO HR: Still need to  add memtype

  /**
    * A WebAssembly Global Variable
    *
    * https://webassembly.github.io/spec/core/text/modules.html#globals
    */
  case class Global(value: Int) // TODO HR : Doesn't follow the convention

  /**
    * A WebAssembly `export` statement
    *
    * https://webassembly.github.io/spec/core/text/modules.html#exports
    */
  case object Export // TODO HR : Doesn't follow the convention

  /**
    * A WebAssembly Start Function
    *
    * https://webassembly.github.io/spec/core/text/modules.html#start-function
    */
  case class Start(idx: funcidx)

  /**
    * A WebAssembly element segment
    *
    * https://webassembly.github.io/spec/core/text/modules.html#element-segments
    */
  case object ElementSegment // TODO HR : Doesn't follow the convention

  /**
    * A WebAssembly data segment
    *
    * https://webassembly.github.io/spec/core/text/modules.html#data-segments
    */
  case class Data(offset: Int, str: String) // TODO HR : Doesn't follow the convention

  /**
    * A WebAssembly module
    *
    * https://webassembly.github.io/spec/core/text/modules.html#text-module
    */
  case class Module(name: String,
                    globals: List[Global],
                    imports: List[String],
                    table: Option[Table],
                    data: List[Data],
                    functions: List[Function])
  // TODO HR : Doesn't follow the convention
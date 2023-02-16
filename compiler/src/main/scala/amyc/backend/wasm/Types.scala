package amyc.backend.wasm

import amyc.backend.wasm.Values.id
import amyc.backend.wasm.Indices.typeidx

/**
  * https://webassembly.github.io/spec/core/syntax/types.html
  */

object Types:
  abstract class WasmType

  sealed abstract class valtype extends WasmType

  /**
    * https://webassembly.github.io/spec/core/text/types.html#number-types
    */
  abstract class numtype extends valtype
  inline def i32: Instructions.i32.type = Instructions.i32
  inline def i64: Instructions.i64.type = Instructions.i64
  inline def f32: Instructions.f32.type = Instructions.f32
  inline def f64: Instructions.f64.type = Instructions.f64

  /**
    * https://webassembly.github.io/spec/core/text/types.html#vector-types
    */
  abstract class vectype extends valtype
  inline def v128 : Instructions.v128.type = Instructions.v128

  /**
    * https://webassembly.github.io/spec/core/text/types.html#reference-types
    */
  sealed trait reftype extends valtype
  sealed trait heaptype

  case object funcref   extends reftype, heaptype
  case object externref extends reftype, heaptype

  /**
    * https://webassembly.github.io/spec/core/text/types.html#function-types
    */
  case class functype(params: List[param], results: List[result])
  case class param(id: Option[id], tpe: valtype)
  case class result(tpe: valtype)

  /**
    * TODO HR :
    * 1- Add : https://webassembly.github.io/spec/core/text/types.html#limits
    * 2- Add : https://webassembly.github.io/spec/core/text/types.html#memory-types
    * 3- Add : https://webassembly.github.io/spec/core/text/types.html#table-types
    * 4- Add : https://webassembly.github.io/spec/core/text/types.html#global-types
    * 5- Move the definitions below to their corresponding file
    */


  case class typeuse(x: typeidx)
  case class local(id: Option[id], tpe: valtype)

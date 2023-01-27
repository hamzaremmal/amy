package amyc.backend.wasm

import amyc.backend.wasm.indices.typeidx

object types :
  abstract class WasmType

  sealed abstract class valtype extends WasmType

  abstract class numtype extends valtype
  abstract class vectype extends valtype
  abstract class reftype extends valtype

  case class result(tpe: valtype)
  case class typeuse(x: typeidx)

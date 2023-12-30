package amyc
package backend
package wasm

/**
  * https://webassembly.github.io/spec/core/text/values.html#
  */
object Values :

  /**
    * https://webassembly.github.io/spec/core/text/values.html#strings
    */
  opaque type string = String
  implicit def str2string(str: String) : string = s"\"$str\""

  /**
    * https://webassembly.github.io/spec/core/text/values.html#text-id
    */
  case class name(value: List[string])

  /**
    * https://webassembly.github.io/spec/core/text/values.html#text-id
    */
  opaque type id = String
  implicit def str2id(str: String): id = s"$$$str"




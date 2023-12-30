package amyc
package backend
package wasm
package builtin
package amy

import amyc.backend.wasm.Instructions.{call, i32, local}
import amyc.backend.wasm.builtin.BuiltInModule
import amyc.backend.wasm.utils.{and, or}
import amyc.core.StdDefinitions.stdDef
import amyc.core.Symbols.*
import amyc.core.Context
import amyc.ctx

import scala.annotation.targetName

object unnamed extends BuiltInModule:

  override lazy val owner: Context ?=> ModuleSymbol = stdDef.UnnamedModule

  @targetName("add")
  lazy val + : BuiltIn =
    builtInForSymbol("+"){
      local.get(0) <:>
      local.get(1) <:>
      i32.add
    }

  @targetName("sub")
  lazy val - : BuiltIn =
    builtInForSymbol("-"){
      local.get(0) <:>
      local.get(1) <:>
      i32.sub
    }

  @targetName("mul")
  lazy val * : BuiltIn =
    builtInForSymbol("*"){
      local.get(0) <:>
      local.get(1) <:>
      i32.mul
    }

  @targetName("div")
  lazy val / : BuiltIn =
    builtInForSymbol("/"){
      local.get(0) <:>
      local.get(1) <:>
      i32.div_s
    }

  @targetName("mod")
  lazy val % : BuiltIn =
    builtInForSymbol("%"){
      local.get(0) <:>
      local.get(1) <:>
      i32.rem_s
    }

  @targetName("lt")
  lazy val < : BuiltIn =
    builtInForSymbol("<"){
      local.get(0) <:>
      local.get(1) <:>
      i32.lt_s
    }

  @targetName("leq")
  lazy val <= : BuiltIn =
    builtInForSymbol("<="){
      local.get(0) <:>
      local.get(1) <:>
      i32.le_s
    }

  // TODO HR : Cannot apply this method, we should have by-name parameters
  //@targetName("and")
  //lazy val && : BuiltIn =
  //  builtInForSymbol("&&"){
  //    and(local.get(0), local.get(1))
  //  }

  // TODO HR : Cannot apply this method, we should have by-name parameters
  //@targetName("or")
  //lazy val || : BuiltIn =
  //  builtInForSymbol("||"){
  //    or(local.get(0), local.get(1))
  //  }

  @targetName("eq")
  lazy val == : BuiltIn =
    builtInForSymbol("=="){
      local.get(0) <:>
      local.get(1) <:>
      i32.eq
    }

  @targetName("concat")
  lazy val ++ : BuiltIn =
    builtInForSymbol("++"){
      local.get(0) <:>
      local.get(1) <:>
      call(String.concat.name)
    }
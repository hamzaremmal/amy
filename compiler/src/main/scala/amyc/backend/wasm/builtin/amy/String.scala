package amyc.backend.wasm.builtin.amy

import amyc.*
import amyc.ast.SymbolicTreeModule.ClassTypeTree
import amyc.core.Symbols.*
import amyc.core.StdDefinitions.*
import amyc.core.Context
import amyc.backend.wasm.*
import amyc.backend.wasm.builtin.BuiltInModule
import amyc.backend.wasm.Values.*
import amyc.backend.wasm.Instructions.*
import amyc.backend.wasm.Indices.localidx
import amyc.backend.wasm.Types.result
import amyc.backend.wasm.utils.{ift, incr}

object String extends BuiltInModule :

  override lazy val owner: Context ?=> ModuleSymbol = stdDef.StringModule

  /*
     fn concat(str1, str2) =
       val dest = dynamic_alloc(String.length(str1) + String.length(str2) + 1)
       val dest_offset = 0
       val base = str1
       val src_offset = 0
       label1:
         if base[src_offset] != 0 then
           dest[dest_offset] = base[src_offset]
           src_offset++
           dest_offset++
           br_if label1
         else
           end
       base = str2
       src_offset = 0
       label2:
         if base[src_offset] != 0 then
           dest[dest_offset] = base[src_offset]
           src_offset++
           dest_offset++
           br_if label2
         else
           end
       dest[dest_offset] = '\0'
       dest
   */
  lazy val concat : BuiltIn =
    builtInForSymbol("concat") {
      val lhs = local.get(0)
      val rhs = local.get(1)
      val dest = lh.getFreshLocal
      val dest_offset = lh.getFreshLocal
      val base = lh.getFreshLocal
      val base_offset = lh.getFreshLocal

      def loop =
        val label = getFreshLabel()
        Loop(label, Some(result(i32))) <:>
          ift(
            {
              local.get(base) <:>
              local.get(base_offset) <:>
              i32.add <:>
              i32.load8_u <:>
              i32.const(0) <:>
              i32.ne
            },
            {
              local.get(dest) <:>
              local.get(dest_offset) <:>
              i32.add <:>
              local.get(base) <:>
              local.get(base_offset) <:>
              i32.add <:>
              i32.load8_u <:>
              i32.store8 <:>
              incr(base_offset) <:>
              incr(dest_offset) <:>
              br(label) <:>
              i32.const(0) // HR: trick to satisfy typechecking of wat2wasm
            },
            {
            i32.const(0)
            }
          ) <:> end <:>
          drop

      // Compute the size and allocate memory
      lhs <:>
      call(str2id("String_length")) <:>
      rhs <:>
      call(str2id("String_length")) <:>
      i32.add <:>
      i32.const(1) <:>
      i32.add <:>
      lh.mh.dynamic_alloc <:>
      local.set(dest) <:>
      // Set the base to the lhs
      lhs <:>
      local.set(base) <:>
      loop <:> // copy the first String
      // Set the base to rhs and reset the base_offset
      rhs <:>
      local.set(base) <:>
      i32.const(0) <:>
      local.set(base_offset) <:>
      loop <:> // copy the second String
      // Store the null character add dest[dest_offset]
      local.get(dest) <:>
      local.get(dest_offset) <:>
      i32.add <:>
      i32.const(0) <:> // '\0'
      i32.store8 <:>
      // return the base address
      local.get(dest)
    }

    /*
    fn length(str: String): Int =
      val size = 0
      val offset = 0
      label:
        if str[offset] != 0 then
          size++
          offset++
          br label
        else
          return size
      }
    */
  lazy val length: BuiltIn =
    builtInForSymbol("length"){
      val str : localidx = 0 // string is stored in the first parameter
      val size: localidx = lh.getFreshLocal // allocate space to store the size to return
      val offset : localidx = lh.getFreshLocal // allocate space to store the offset
      val label = getFreshLabel()
      Loop(label, Some(result(i32))) <:>
      ift(
        {
        local.get(str) <:>
        local.get(offset) <:>
        i32.add <:>
        i32.load8_u <:>
        i32.const(0) <:>
        i32.ne
        },
        {
          incr(size) <:>
          incr(offset) <:>
          br(label) <:>
          i32.const(0) // HR: trick to satisfy typechecking of wat2wasm
        },
        {
          local.get(size)
        }
      ) <:> end
    }
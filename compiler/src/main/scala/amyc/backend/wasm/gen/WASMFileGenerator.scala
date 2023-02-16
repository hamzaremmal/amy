package amyc.backend.wasm.gen

import amyc.*
import amyc.backend.fs.*
import CodePrinter.*
import amyc.backend.wasm.Modules.*
import amyc.core.Context
import amyc.utils.Env

import java.io.*
import scala.sys.process.*

object WASMFileGenerator :

  def apply(m: Module)(using Context): String =
    val (local, inPath) = {
      import Env.*
      os match {
        case Linux => ("./bin/wat2wasm", "wat2wasm")
        case Windows => ("./bin/wat2wasm.exe", "wat2wasm.exe")
        case Mac => ("./bin/wat2wasm", "wat2wasm")
      }
    }

    val w2wOptions = s"${pathWithExt(m, wat_ext)} -o ${pathWithExt(m, wasm_ext)}"

    try
      try
        s"$local $w2wOptions".!!
      catch
        case _: IOException =>
          s"$inPath $w2wOptions".!!
    catch
      case _: IOException =>
        reporter.fatal{
          "wat2wasm utility was not found under ./bin or in system path, " +
            "or did not have permission to execute. Make sure it is either in the system path, or in <root of the project>/bin"
        }
      case _: RuntimeException =>
        reporter.fatal(s"wat2wasm failed to translate WebAssembly text file ${pathWithExt(m, wat_ext)} to binary")
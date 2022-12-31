package amyc.backend.codegen

import amyc.backend.wasm.{Module, wrapper}
import amyc.utils.FileWriter
import amyc.*
import amyc.backend.wasm.wrapper.{HTMLWrapper, NodeJSWrapper, WATFile}
import amyc.core.Context
import amyc.utils.{Env, Pipeline}

import scala.sys.process.*
import java.io.*

// Prints all 4 different files from a wasm Module
object CodePrinter extends Pipeline[Module, Unit]{

  val outDirName = "wasmout"

  def pathWithExt(module: Module, ext: String) = s"$outDirName/${nameWithExt(module, ext)}"

  def nameWithExt(module: Module, ext: String) = s"${module.name}.$ext"

  override val name = "CodePrinter"

  override def run(m: Module)(using Context): Unit = {
    val (local, inPath) = {
      import Env._
      os match {
        case Linux   => ("./bin/wat2wasm",     "wat2wasm")
        case Windows => ("./bin/wat2wasm.exe", "wat2wasm.exe")
        case Mac     => ("./bin/wat2wasm",     "wat2wasm")
      }
    }

    val w2wOptions = s"${pathWithExt(m, "wat")} -o ${pathWithExt(m, "wasm")}"

    val outDir = new File(outDirName)
    if (!outDir.exists()) {
      outDir.mkdir()
    }

    FileWriter(pathWithExt(m, "wat")){
      WATFile(m)
    }

    try {
      try {
        s"$local $w2wOptions".!!
      } catch {
        case _: IOException =>
          s"$inPath $w2wOptions".!!
      }
    } catch {
      case _: IOException =>
        reporter.fatal(
          "wat2wasm utility was not found under ./bin or in system path, " +
          "or did not have permission to execute. Make sure it is either in the system path, or in <root of the project>/bin"
        )
      case _: RuntimeException =>
        reporter.fatal(s"wat2wasm failed to translate WebAssembly text file ${pathWithExt(m, "wat")} to binary")
    }

    // Web version needs path relative to .html
    FileWriter(pathWithExt(m, "html")){
      HTMLWrapper(nameWithExt(m, "wasm"), m)
    }

    // Node version needs path relative to project root
    FileWriter(pathWithExt(m, "js")){
      wrapper.NodeJSWrapper(pathWithExt(m, "wasm"), m)
    }

  }
}

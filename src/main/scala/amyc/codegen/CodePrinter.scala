package amyc
package codegen

import wasm.Module
import amyc.utils.{Context, Pipeline, Env}
import scala.sys.process._
import java.io._

// Prints all 4 different files from a wasm Module
object CodePrinter extends Pipeline[Module, Unit]{
  def run(ctx: Context)(m: Module) = {
    val outDirName = "wasmout"

    def pathWithExt(ext: String) = s"$outDirName/${nameWithExt(ext)}"
    def nameWithExt(ext: String) = s"${m.name}.$ext"

    val (local, inPath) = {
      import Env._
      os match {
        case Linux   => ("./bin/wat2wasm",     "wat2wasm")
        case Windows => ("./bin/wat2wasm.exe", "wat2wasm.exe")
        case Mac     => ("./bin/wat2wasm",     "wat2wasm")
      }
    }

    val w2wOptions = s"${pathWithExt("wat")} -o ${pathWithExt("wasm")}"

    val outDir = new File(outDirName)
    if (!outDir.exists()) {
      outDir.mkdir()
    }

    m.writeWasmText(pathWithExt("wat"))

    try {
      try {
        s"$local $w2wOptions".!!
      } catch {
        case _: IOException =>
          s"$inPath $w2wOptions".!!
      }
    } catch {
      case _: IOException =>
        ctx.reporter.fatal(
          "wat2wasm utility was not found under ./bin or in system path, " +
          "or did not have permission to execute. Make sure it is either in the system path, or in <root of the project>/bin"
        )
      case _: RuntimeException =>
        ctx.reporter.fatal(s"wat2wasm failed to translate WebAssembly text file ${pathWithExt("wat")} to binary")
    }

    m.writeHtmlWrapper(pathWithExt("html"), nameWithExt("wasm")) // Web version needs path relative to .html
    m.writeNodejsWrapper(pathWithExt("js"), pathWithExt("wasm")) // Node version needs path relative to project root

  }
}

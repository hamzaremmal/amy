package amyc.backend.wasm

import amyc.*
import amyc.backend.wasm.wrapper.{HTMLWrapper, NodeJSWrapper, WASMFileGenerator, WATFile}
import amyc.backend.wasm.{Module, wrapper}
import amyc.core.Context
import amyc.utils.{Env, FileWriter, Pipeline}

import java.io.File

// Prints all 4 different files from a wasm Module
object CodePrinter extends Pipeline[Module, Unit]{

  opaque type Extension = String

  val outDirName = "wasmout"

  val wat_ext  : Extension = "wat"
  val wasm_ext : Extension = "wasm"
  val html_ext : Extension = "html"
  val js_ext   : Extension = "js"

  def pathWithExt(module: Module, ext: Extension) = s"$outDirName/${nameWithExt(module, ext)}"

  def nameWithExt(module: Module, ext: Extension) = s"${module.name}.$ext"

  def mkOutputDirectory(using Context) =
    val outDir = new File(outDirName)
    if (!outDir.exists()) {
      outDir.mkdir()
    }

  override val name = "CodePrinter"

  override def run(m: Module)(using Context): Unit =
    mkOutputDirectory
    // Generate a *.wat file
    FileWriter(pathWithExt(m, wat_ext)){
      WATFile(m)
    }
    // Generate a *.wasm file based on the *.wat file
    WASMFileGenerator(m)
    // Web version needs path relative to .html
    FileWriter(pathWithExt(m, html_ext)){
      HTMLWrapper(nameWithExt(m, wasm_ext), m)
    }
    // Node version needs path relative to project root
    FileWriter(pathWithExt(m, js_ext)){
      wrapper.NodeJSWrapper(pathWithExt(m, wasm_ext), m)
    }
}

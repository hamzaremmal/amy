package amyc.backend.wasm.utils

import amyc.backend.wasm.Module
import amyc.core.Context

import java.io.File

// Interface with the file system
object fs {

  val outDirName = "wasmout"

  opaque type Extension = String

  val wat_ext: Extension = "wat"
  val wasm_ext: Extension = "wasm"
  val html_ext: Extension = "html"
  val js_ext: Extension = "js"

  def pathWithExt(module: Module, ext: Extension) = s"$outDirName/${nameWithExt(module, ext)}"

  def nameWithExt(module: Module, ext: Extension) = s"${module.name}.$ext"

  def mkOutputDirectory(using Context): Unit =
    val outDir = new File(outDirName)
    if (!outDir.exists()) {
      outDir.mkdir()
    }

}

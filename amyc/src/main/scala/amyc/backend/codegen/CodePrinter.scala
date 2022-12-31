package amyc.backend.codegen

import amyc.backend.wasm.{Module, wrapper}
import amyc.utils.FileWriter
import amyc.*
import amyc.backend.wasm.wrapper.{HTMLWrapper, NodeJSWrapper, WASMFileGenerator, WATFile}
import amyc.core.Context
import amyc.utils.{Env, Pipeline}

import java.io.File

// Prints all 4 different files from a wasm Module
object CodePrinter extends Pipeline[Module, Unit]{

  val outDirName = "wasmout"

  def pathWithExt(module: Module, ext: String) = s"$outDirName/${nameWithExt(module, ext)}"

  def nameWithExt(module: Module, ext: String) = s"${module.name}.$ext"

  def mkOutputDirectory(using Context) =
    val outDir = new File(outDirName)
    if (!outDir.exists()) {
      outDir.mkdir()
    }

  override val name = "CodePrinter"

  override def run(m: Module)(using Context): Unit =
    mkOutputDirectory
    // Generate a *.wat file
    FileWriter(pathWithExt(m, "wat")){
      WATFile(m)
    }
    // Generate a *.wasm file based on the *.wat file
    WASMFileGenerator(m)
    // Web version needs path relative to .html
    FileWriter(pathWithExt(m, "html")){
      HTMLWrapper(nameWithExt(m, "wasm"), m)
    }
    // Node version needs path relative to project root
    FileWriter(pathWithExt(m, "js")){
      wrapper.NodeJSWrapper(pathWithExt(m, "wasm"), m)
    }
}

package amyc
package backend
package wasm
package gen

import amyc.*
import amyc.backend.fs.*
import amyc.backend.wasm.gen.{HTMLWrapper, NodeJSWrapper, WASMFileGenerator, WATFile}
import amyc.backend.wasm.Modules.*
import amyc.core.Context
import amyc.tools.Pipeline
import amyc.utils.{Env, FileWriter}

import java.io.File

// Prints all 4 different files from a wasm Module
object CodePrinter extends Pipeline[Module, Unit] :

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
      NodeJSWrapper(pathWithExt(m, wasm_ext), m)
    }

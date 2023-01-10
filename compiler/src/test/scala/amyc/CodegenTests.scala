package amyc

import parsing.*
import analyzer.NameAnalyzer
import typer.Typer
import amyc.backend.js.runners.CodePrinterExecutor
import amyc.backend.wasm.WASMCodeGenerator
import amyc.utils.Pipeline

import java.io.File


class CodegenTests extends ExecutionTests:

  override val pipeline =
    Lexer andThen
    Parser andThen
    NameAnalyzer andThen
    Typer andThen
    WASMCodeGenerator andThen
    CodePrinterExecutor
package amyc

import amyc.backend.codegen.CodeGen
import parsing.*
import analyzer.NameAnalyzer
import typer.Typer
import amyc.backend.js.runners.CodePrinterExecutor
import amyc.utils.Pipeline

import java.io.File


class CodegenTests extends ExecutionTests:

  override val pipeline =
    Lexer andThen
    Parser andThen
    NameAnalyzer andThen
    Typer andThen
    CodeGen andThen
    CodePrinterExecutor
package amyc.backend.wasm

import amyc.core.Context

// A WebAssembly module
case class Module(name: String, imports: List[String], globals: Int, functions: List[Function])
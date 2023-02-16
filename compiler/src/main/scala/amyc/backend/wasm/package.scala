package amyc.backend.wasm

import amyc.core.*
import amyc.core.Symbols.*
import amyc.backend.wasm.handlers.*

/* Fetch the local handler */
inline def lh(using LocalsHandler): LocalsHandler = summon
/* Fetch the module handler */
inline def mh(using ModuleHandler): ModuleHandler = summon

/* wasm name */
// HR : In case of implementing overloading, it should be Symbol::fullName
inline def fullName(inline sym: Symbol): String = s"${sym.owner.name}_${sym.name}"

/* Generate a new fresh label, this function should be moved to Context */
inline def getFreshLabel(inline name: String = "label") = {
  Identifier.fresh(name).fullName
}
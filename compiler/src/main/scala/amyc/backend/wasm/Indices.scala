package amyc.backend.wasm

import amyc.backend.wasm.Values.id

/**
  * https://webassembly.github.io/spec/core/text/modules.html#indices
  */

object Indices :

  type typeidx   = Int | id
  type funcidx   = Int | id
  type tableidx  = Int | id
  type memidx    = Int | id
  type globalidx = Int | id
  type elemidx   = Int | id
  type dataidx   = Int | id
  type localidx  = Int | id
  type labelidx  = Int | id
